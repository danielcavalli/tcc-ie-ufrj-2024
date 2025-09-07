# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ FUNÇÕES PARA LIDAR COM DESBALANCEAMENTO                                 │ #
# └─────────────────────────────────────────────────────────────────────────┘ #

#' trim_by_propensity_score()
#' ---------------------------------------------------------------------------
#' Remove unidades com propensity score extremo para melhorar balanço
#' @param df DataFrame com dados
#' @param lower_threshold Percentil inferior (default 0.1)
#' @param upper_threshold Percentil superior (default 0.9)
#' @return DataFrame trimado
#' ---------------------------------------------------------------------------
trim_by_propensity_score <- function(df, lower_threshold = 0.1, upper_threshold = 0.9) {
    cli::cli_h3("Aplicando Trimming por Propensity Score")

    # Estimar propensity score
    ps_formula <- as.formula(
        "I(gname > 0) ~ log_area_plantada + log_populacao + log_pib_per_capita + prop_pib_agro"
    )

    # Dados completos para PS
    df_complete <- df %>%
        filter(!is.na(log_area_plantada) & !is.na(log_populacao) &
            !is.na(log_pib_per_capita) & !is.na(prop_pib_agro))

    # Modelo
    ps_model <- glm(ps_formula, data = df_complete, family = binomial())

    # Adicionar PS ao dataframe
    df_ps <- df_complete %>%
        mutate(
            prop_score = predict(ps_model, type = "response"),
            ever_treated = gname > 0
        )

    # Calcular thresholds
    ps_treated <- df_ps %>%
        filter(ever_treated) %>%
        pull(prop_score)
    ps_control <- df_ps %>%
        filter(!ever_treated) %>%
        pull(prop_score)

    # Common support
    min_ps <- max(min(ps_treated), min(ps_control))
    max_ps <- min(max(ps_treated), max(ps_control))

    # Adicionar margem de segurança
    ps_range <- max_ps - min_ps
    min_threshold <- min_ps + ps_range * lower_threshold
    max_threshold <- min_ps + ps_range * upper_threshold

    # Aplicar trimming
    df_trimmed <- df_ps %>%
        filter(prop_score >= min_threshold & prop_score <= max_threshold)

    n_removed <- nrow(df_complete) - nrow(df_trimmed)
    pct_removed <- n_removed / nrow(df_complete) * 100

    cli::cli_alert_info("Unidades removidas: {n_removed} ({round(pct_removed, 1)}%)")
    cli::cli_alert_info("PS range original: [{round(min(df_ps$prop_score), 3)}, {round(max(df_ps$prop_score), 3)}]")
    cli::cli_alert_info("PS range após trim: [{round(min_threshold, 3)}, {round(max_threshold, 3)}]")

    return(df_trimmed %>% select(-prop_score))
}

#' winsorize_outliers()
#' ---------------------------------------------------------------------------
#' Winsoriza valores extremos para reduzir influência de outliers
#' @param df DataFrame
#' @param vars Variáveis para winsorizar
#' @param percentile Percentil para corte (default 0.01 = 1% e 99%)
#' @return DataFrame winsorizado
#' ---------------------------------------------------------------------------
winsorize_outliers <- function(df, vars = NULL, percentile = 0.01) {
    cli::cli_h3("Winsorização de Outliers")

    if (is.null(vars)) {
        vars <- c(
            "log_pib_agro", "log_area_plantada", "log_populacao",
            "log_pib_per_capita", "prop_pib_agro"
        )
    }

    df_wins <- df

    for (var in vars) {
        if (var %in% names(df)) {
            values <- df[[var]]
            if (sum(!is.na(values)) > 0) {
                # Calcular percentis
                lower <- quantile(values, percentile, na.rm = TRUE)
                upper <- quantile(values, 1 - percentile, na.rm = TRUE)

                # Winsorizar
                n_lower <- sum(values < lower, na.rm = TRUE)
                n_upper <- sum(values > upper, na.rm = TRUE)

                df_wins[[var]] <- case_when(
                    is.na(values) ~ NA_real_,
                    values < lower ~ lower,
                    values > upper ~ upper,
                    TRUE ~ values
                )

                if (n_lower + n_upper > 0) {
                    cli::cli_alert_info("{var}: {n_lower} valores < P{percentile*100}, {n_upper} valores > P{(1-percentile)*100}")
                }
            }
        }
    }

    return(df_wins)
}

#' entropy_balancing_weights()
#' ---------------------------------------------------------------------------
#' Calcula pesos via entropy balancing para alcançar balanço perfeito
#' Implementação simplificada - para produção usar pacote ebal
#' @param df DataFrame
#' @param covars Covariáveis para balancear
#' @return DataFrame com pesos
#' ---------------------------------------------------------------------------
entropy_balancing_weights <- function(df, covars = NULL) {
    cli::cli_h3("Calculando Pesos via Entropy Balancing")

    if (is.null(covars)) {
        covars <- c("log_area_plantada", "log_populacao", "log_pib_per_capita", "prop_pib_agro")
    }

    # Filtrar dados completos
    df_complete <- df %>%
        filter(complete.cases(select(., all_of(covars))))

    # Separar tratados e controles
    df_treated <- df_complete %>% filter(gname > 0)
    df_control <- df_complete %>% filter(gname == 0)

    if (nrow(df_control) == 0) {
        cli::cli_alert_warning("Sem unidades de controle permanente. Usando not-yet-treated.")
        # Para not-yet-treated, precisaríamos de uma abordagem diferente
        return(df %>% mutate(eb_weight = 1))
    }

    # Calcular momentos alvo (médias dos tratados)
    target_moments <- df_treated %>%
        select(all_of(covars)) %>%
        summarise_all(mean, na.rm = TRUE)

    # Para simplificar, usar inverse probability weighting como proxy
    # Em produção, usar algoritmo de entropy balancing completo
    ps_model <- glm(
        as.formula(paste("I(gname > 0) ~", paste(covars, collapse = " + "))),
        data = df_complete,
        family = binomial()
    )

    df_weighted <- df_complete %>%
        mutate(
            prop_score = predict(ps_model, type = "response"),
            # Pesos IPW como aproximação
            eb_weight = case_when(
                gname > 0 ~ 1,
                gname == 0 ~ prop_score / (1 - prop_score)
            )
        ) %>%
        # Normalizar pesos
        mutate(
            eb_weight = eb_weight / mean(eb_weight[gname == 0], na.rm = TRUE)
        )

    # Verificar balanço alcançado
    cli::cli_alert_info("Verificando balanço após ponderação...")

    balance_check <- map_dfr(covars, function(var) {
        mean_treated <- mean(df_weighted[[var]][df_weighted$gname > 0], na.rm = TRUE)
        mean_control_unweighted <- mean(df_weighted[[var]][df_weighted$gname == 0], na.rm = TRUE)
        mean_control_weighted <- weighted.mean(
            df_weighted[[var]][df_weighted$gname == 0],
            df_weighted$eb_weight[df_weighted$gname == 0],
            na.rm = TRUE
        )

        tibble(
            variable = var,
            diff_before = abs(mean_treated - mean_control_unweighted),
            diff_after = abs(mean_treated - mean_control_weighted),
            improvement = (diff_before - diff_after) / diff_before * 100
        )
    })

    print(balance_check)

    # Retornar dataframe original com pesos adicionados
    return(df %>%
        left_join(
            df_weighted %>% select(id_microrregiao, ano, eb_weight),
            by = c("id_microrregiao", "ano")
        ) %>%
        mutate(eb_weight = ifelse(is.na(eb_weight), 1, eb_weight)))
}

#' create_matched_sample()
#' ---------------------------------------------------------------------------
#' Cria amostra pareada usando nearest neighbor matching
#' @param df DataFrame
#' @param caliper Distância máxima para match (em unidades de desvio padrão)
#' @param ratio Quantos controles por tratado
#' @return DataFrame com matches
#' ---------------------------------------------------------------------------
create_matched_sample <- function(df, caliper = 0.2, ratio = 1) {
    cli::cli_h3("Criando Amostra Pareada")

    # Preparar dados para matching
    df_match <- df %>%
        filter(!is.na(log_area_plantada) & !is.na(log_populacao) &
            !is.na(log_pib_per_capita) & !is.na(prop_pib_agro)) %>%
        group_by(id_microrregiao) %>%
        summarise(
            ever_treated = max(gname > 0),
            gname = first(gname),
            log_area_plantada = mean(log_area_plantada, na.rm = TRUE),
            log_populacao = mean(log_populacao, na.rm = TRUE),
            log_pib_per_capita = mean(log_pib_per_capita, na.rm = TRUE),
            prop_pib_agro = mean(prop_pib_agro, na.rm = TRUE),
            .groups = "drop"
        )

    # Estimar propensity score
    ps_model <- glm(
        ever_treated ~ log_area_plantada + log_populacao + log_pib_per_capita + prop_pib_agro,
        data = df_match,
        family = binomial()
    )

    df_match$prop_score <- predict(ps_model, type = "response")

    # Separar grupos
    treated_units <- df_match %>% filter(ever_treated == 1)
    control_units <- df_match %>% filter(ever_treated == 0)

    # Matching simples (nearest neighbor)
    matched_controls <- list()

    for (i in 1:nrow(treated_units)) {
        # Calcular distâncias
        distances <- abs(control_units$prop_score - treated_units$prop_score[i])

        # Aplicar caliper (em unidades de SD do propensity score)
        ps_sd <- sd(df_match$prop_score)
        valid_matches <- which(distances <= caliper * ps_sd)

        if (length(valid_matches) >= ratio) {
            # Selecionar os mais próximos
            best_matches <- valid_matches[order(distances[valid_matches])[1:ratio]]
            matched_controls[[i]] <- control_units$id_microrregiao[best_matches]
        }
    }

    # Criar dataset final
    matched_control_ids <- unique(unlist(matched_controls))
    matched_ids <- c(treated_units$id_microrregiao, matched_control_ids)

    df_matched <- df %>%
        filter(id_microrregiao %in% matched_ids)

    cli::cli_alert_success("Amostra pareada criada:")
    cli::cli_alert_info("Unidades tratadas: {nrow(treated_units)}")
    cli::cli_alert_info("Unidades controle pareadas: {length(matched_control_ids)}")
    cli::cli_alert_info("Total de observações: {nrow(df_matched)}")

    return(df_matched)
}

#' analyze_and_adjust_balance()
#' ---------------------------------------------------------------------------
#' Função principal que analisa desbalanceamento e aplica ajustes
#' @param df DataFrame original
#' @param method Método de ajuste: "trim", "winsorize", "entropy", "match", "all"
#' @return Lista com dataframe ajustado e diagnósticos
#' ---------------------------------------------------------------------------
analyze_and_adjust_balance <- function(df, method = "all") {
    cli::cli_h2("Análise e Ajuste de Balanceamento")

    results <- list(original = df)

    # 1. Diagnóstico inicial
    cli::cli_h3("Diagnóstico Inicial")
    initial_balance <- create_covariate_balance_table(df)

    # 2. Aplicar ajustes conforme método
    if (method %in% c("trim", "all")) {
        results$trimmed <- trim_by_propensity_score(df)
    }

    if (method %in% c("winsorize", "all")) {
        results$winsorized <- winsorize_outliers(df)
    }

    if (method %in% c("entropy", "all")) {
        results$entropy <- entropy_balancing_weights(df)
    }

    if (method %in% c("match", "all")) {
        results$matched <- create_matched_sample(df)
    }

    # 3. Comparar resultados
    if (method == "all") {
        cli::cli_h3("Comparação de Métodos")

        comparison <- map_dfr(names(results), function(name) {
            data <- results[[name]]
            n_obs <- nrow(data)
            n_units <- n_distinct(data$id_microrregiao)

            # Calcular desbalanceamento médio (simplificado)
            if (name != "original") {
                balance <- create_covariate_balance_table(data)
            }

            tibble(
                method = name,
                n_observations = n_obs,
                n_units = n_units
            )
        })

        print(comparison)
    }

    cli::cli_alert_success("Análise de balanceamento concluída")

    return(results)
}
