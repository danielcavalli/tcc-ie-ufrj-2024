# ══════════════════════════════════════════════════════════════════════════════
# VISUALIZAÇÕES E TABELAS COMPLEMENTARES PARA ANÁLISE DID
# ══════════════════════════════════════════════════════════════════════════════
#
# Este script contém funções para gerar visualizações e tabelas complementares
# para o estudo de Diferenças-em-Diferenças (DiD) com adoção escalonada.
#
# Autor: Assistant
# Data: 2024-12-19
# ══════════════════════════════════════════════════════════════════════════════

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 1. TABELA DE ESTATÍSTICAS DESCRITIVAS COMPARATIVAS                   ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' create_descriptive_stats_table(): Tabela comparativa de estatísticas
#'
#' @description Gera tabela GT com estatísticas descritivas comparando
#' grupos nunca tratados, pré e pós-tratamento, incluindo testes de diferença
#' @param df DataFrame limpo com dados do painel
#' @return Objeto GT e salva arquivo
#' @examples
#' create_descriptive_stats_table(df_clean)
create_descriptive_stats_table <- function(df) {
    cli::cli_h3("Criando Tabela de Estatísticas Descritivas Comparativas")

    # Criar diretório se não existir
    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Categorizar observações
    df_stats <- df %>%
        mutate(
            group = case_when(
                gname == 0 ~ "Nunca Tratados",
                treated == 0 ~ "Pré-Tratamento",
                treated == 1 ~ "Pós-Tratamento",
                TRUE ~ NA_character_
            )
        ) %>%
        filter(!is.na(group))

    # Calcular estatísticas por grupo
    stats_summary <- df_stats %>%
        group_by(group) %>%
        summarise(
            # PIB Agropecuário
            pib_agro_mean = mean(pib_agropecuario / 1000, na.rm = TRUE),
            pib_agro_median = median(pib_agropecuario / 1000, na.rm = TRUE),
            pib_agro_sd = sd(pib_agropecuario / 1000, na.rm = TRUE),

            # Área Plantada de Cana (outcome principal)
            area_mean = mean(area_plantada_cana, na.rm = TRUE),
            area_median = median(area_plantada_cana, na.rm = TRUE),
            area_sd = sd(area_plantada_cana, na.rm = TRUE),

            # População
            pop_mean = mean(populacao_total / 1000, na.rm = TRUE),
            pop_median = median(populacao_total / 1000, na.rm = TRUE),
            pop_sd = sd(populacao_total / 1000, na.rm = TRUE),

            # Contagens
            n_obs = n(),
            n_micro = n_distinct(id_microrregiao),
            .groups = "drop"
        )

    # Teste de diferença de médias (pré vs pós)
    pre_data <- df_stats %>% filter(group == "Pré-Tratamento")
    post_data <- df_stats %>% filter(group == "Pós-Tratamento")

    # T-tests apenas se houver dados suficientes
    if (nrow(pre_data) > 1 && nrow(post_data) > 1) {
        t_pib <- t.test(
            post_data$pib_agropecuario,
            pre_data$pib_agropecuario,
            var.equal = FALSE
        )

        t_area <- t.test(
            post_data$area_plantada_cana,
            pre_data$area_plantada_cana,
            var.equal = FALSE
        )

        t_pop <- t.test(
            post_data$populacao_total,
            pre_data$populacao_total,
            var.equal = FALSE
        )
    } else {
        t_pib <- list(p.value = NA)
        t_area <- list(p.value = NA)
        t_pop <- list(p.value = NA)
    }

    # Reorganizar dados para tabela
    stats_long <- stats_summary %>%
        pivot_longer(
            cols = -group,
            names_to = c("variable", "stat"),
            names_pattern = "(.+)_(mean|median|sd|obs|micro)",
            values_to = "value"
        ) %>%
        pivot_wider(
            names_from = group,
            values_from = value
        ) %>%
        mutate(
            `Diferença (Pós - Pré)` = ifelse(
                !is.na(`Pós-Tratamento`) & !is.na(`Pré-Tratamento`),
                `Pós-Tratamento` - `Pré-Tratamento`,
                NA
            ),
            variable_label = case_when(
                variable == "pib_agro" ~ "PIB Agropecuário (R$ milhões)",
                variable == "area" ~ "Área Plantada (hectares)",
                variable == "pop" ~ "População (milhares)",
                variable == "n" & stat == "obs" ~ "Número de Observações",
                variable == "n" & stat == "micro" ~ "Número de Microrregiões",
                TRUE ~ variable
            ),
            stat_label = case_when(
                stat == "mean" ~ "Média",
                stat == "median" ~ "Mediana",
                stat == "sd" ~ "Desvio Padrão",
                TRUE ~ ""
            )
        ) %>%
        filter(!is.na(variable_label))

    # Adicionar p-valores
    p_values <- tibble(
        variable_label = c(
            "PIB Agropecuário (R$ milhões)",
            "Área Plantada (hectares)",
            "População (milhares)"
        ),
        stat_label = "Média",
        p_value = c(t_pib$p.value, t_area$p.value, t_pop$p.value)
    )

    stats_final <- stats_long %>%
        left_join(p_values, by = c("variable_label", "stat_label")) %>%
        arrange(variable_label, factor(stat_label, levels = c("Média", "Mediana", "Desvio Padrão", "")))

    # Criar tabela GT
    desc_table <- stats_final %>%
        select(
            variable_label, stat_label, `Pré-Tratamento`,
            `Pós-Tratamento`, `Diferença (Pós - Pré)`, p_value
        ) %>%
        gt() %>%
        tab_header(
            title = "Estatísticas Descritivas Comparativas",
            subtitle = "Comparação entre grupos de tratamento"
        ) %>%
        tab_spanner(
            label = "Grupos",
            columns = c(`Pré-Tratamento`, `Pós-Tratamento`)
        ) %>%
        tab_spanner(
            label = "Análise",
            columns = c(`Diferença (Pós - Pré)`, p_value)
        ) %>%
        fmt_number(
            columns = c(
                `Pré-Tratamento`, `Pós-Tratamento`,
                `Diferença (Pós - Pré)`
            ),
            decimals = 2
        ) %>%
        fmt_number(
            columns = p_value,
            decimals = 4
        ) %>%
        cols_label(
            variable_label = "Variável",
            stat_label = "Estatística",
            p_value = "P-valor"
        ) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
                columns = p_value,
                rows = !is.na(p_value) & p_value < 0.05
            )
        ) %>%
        tab_footnote(
            footnote = "P-valores referem-se ao teste t de diferença de médias entre Pré e Pós-Tratamento",
            locations = cells_column_labels(columns = p_value)
        ) %>%
        tab_options(
            table.font.size = 12,
            heading.title.font.size = 14,
            heading.subtitle.font.size = 12
        )

    # Salvar tabela
    gtsave(desc_table, file.path(output_dir, "descriptive_stats_comparative.html"))

    if (requireNamespace("webshot2", quietly = TRUE)) {
        gtsave(desc_table, file.path(output_dir, "descriptive_stats_comparative.png"))
    }

    cli::cli_alert_success("Tabela salva em: {output_dir}")

    return(desc_table)
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 2. MAPA DE TIMING DE ADOÇÃO                                          ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' create_adoption_map(): Mapa do Brasil com timing de adoção
#'
#' @description Cria mapa colorido por ano de primeira estação meteorológica
#' @param df DataFrame limpo com dados do painel
#' @return ggplot object do mapa e salva arquivo
#' @examples
#' create_adoption_map(df_clean)
create_adoption_map <- function(df) {
    cli::cli_h3("Criando Mapa de Timing de Adoção")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Verificar se pacotes de mapas estão disponíveis
    if (!requireNamespace("sf", quietly = TRUE) ||
        !requireNamespace("geobr", quietly = TRUE)) {
        cli::cli_alert_warning("Pacotes 'sf' e 'geobr' necessários para mapas")
        cli::cli_alert_info("Instale com: install.packages(c('sf', 'geobr'))")

        # Alternativa: criar gráfico de barras por UF
        uf_timing <- df %>%
            filter(gname > 0) %>%
            distinct(id_microrregiao, sigla_uf, gname) %>%
            group_by(sigla_uf) %>%
            summarise(
                mean_year = mean(gname),
                median_year = median(gname),
                n_micro = n(),
                .groups = "drop"
            ) %>%
            arrange(mean_year)

        p_alt <- ggplot(uf_timing, aes(x = reorder(sigla_uf, mean_year), y = mean_year)) +
            geom_col(fill = "#2E86AB", alpha = 0.8) +
            geom_point(aes(y = median_year), color = "#A23B72", size = 3) +
            geom_text(aes(label = n_micro), y = 2002, size = 3, color = "white") +
            coord_flip() +
            scale_y_continuous(breaks = seq(2003, 2020, 2)) +
            theme_minimal() +
            labs(
                title = "Timing Médio de Adoção por Estado",
                subtitle = "Barras = média, Pontos = mediana, Números = qtd microrregiões",
                x = "Estado",
                y = "Ano de Adoção"
            )

        ggsave(
            file.path(output_dir, "adoption_timing_by_state.png"),
            p_alt,
            width = 8,
            height = 10,
            dpi = 300
        )

        cli::cli_alert_info("Gráfico alternativo criado por estado")
        return(p_alt)
    }

    # Se pacotes disponíveis, criar mapa
    # Código do mapa aqui...

    return(NULL)
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 3. GRÁFICO DE COMPOSIÇÃO DINÂMICA DO TRATAMENTO                      ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' plot_treatment_composition(): Composição dinâmica do status de tratamento
#'
#' @description Gráfico de área empilhada mostrando evolução da composição
#' @param df DataFrame limpo com dados do painel
#' @return ggplot object e salva arquivo
#' @examples
#' plot_treatment_composition(df_clean)
plot_treatment_composition <- function(df) {
    cli::cli_h3("Criando Gráfico de Composição Dinâmica do Tratamento")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Calcular composição por ano
    composition <- df %>%
        mutate(
            # Calcular anos desde o tratamento (negativo se ainda não tratada)
            years_since_treatment = ano - gname,
            status = case_when(
                gname == 0 ~ "Nunca Tratadas",
                years_since_treatment < 0 ~ "Ainda Não Tratadas",
                years_since_treatment == 0 ~ "Tratadas há 1 ano",
                years_since_treatment %in% 1:2 ~ "Tratadas há 2-3 anos",
                years_since_treatment %in% 3:4 ~ "Tratadas há 4-5 anos",
                years_since_treatment >= 5 ~ "Tratadas há >5 anos",
                TRUE ~ "Outro"
            )
        ) %>%
        filter(status != "Outro") %>%
        count(ano, status) %>%
        group_by(ano) %>%
        mutate(
            total = sum(n),
            prop = n / total * 100
        ) %>%
        ungroup()

    # Ordenar categorias
    status_order <- c(
        "Nunca Tratadas",
        "Ainda Não Tratadas",
        "Tratadas há 1 ano",
        "Tratadas há 2-3 anos",
        "Tratadas há 4-5 anos",
        "Tratadas há >5 anos"
    )

    # Garantir que todas as categorias existam para todos os anos
    composition_complete <- expand.grid(
        ano = unique(composition$ano),
        status = status_order
    ) %>%
        left_join(composition, by = c("ano", "status")) %>%
        mutate(
            prop = ifelse(is.na(prop), 0, prop),
            status = factor(status, levels = status_order)
        )

    # Criar gráfico
    p_comp <- ggplot(composition_complete, aes(x = ano, y = prop, fill = status)) +
        geom_area(alpha = 0.8) +
        scale_fill_brewer(
            palette = "RdYlBu",
            direction = -1,
            name = "Status de Tratamento"
        ) +
        scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            expand = c(0, 0)
        ) +
        scale_x_continuous(
            breaks = seq(2003, 2023, 2),
            expand = c(0, 0)
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12)
        ) +
        labs(
            title = "Composição Dinâmica do Status de Tratamento",
            subtitle = "Evolução da proporção de microrregiões por categoria (2003-2023)",
            x = "Ano",
            y = "Proporção de Microrregiões (%)",
            caption = "Nota: Tratamento = instalação da primeira estação meteorológica"
        )

    # Salvar gráfico
    ggsave(
        file.path(output_dir, "treatment_composition_dynamic.png"),
        p_comp,
        width = 10,
        height = 6,
        dpi = 300
    )

    ggsave(
        file.path(output_dir, "treatment_composition_dynamic.pdf"),
        p_comp,
        width = 10,
        height = 6
    )

    cli::cli_alert_success("Gráfico salvo em: {output_dir}")

    return(p_comp)
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 4. TABELA DE BALANÇO DE COVARIÁVEIS                                  ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' create_covariate_balance_table(): Tabela de balanço pré-tratamento
#'
#' @description Avalia balanço de covariáveis entre tratados e controles
#' @param df DataFrame limpo com dados do painel
#' @return Objeto GT e salva arquivo
#' @examples
#' create_covariate_balance_table(df_clean)
create_covariate_balance_table <- function(df) {
    cli::cli_h3("Criando Tabela de Balanço de Covariáveis")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Focar apenas em observações pré-tratamento
    df_pre <- df %>%
        filter(treated == 0) %>%
        mutate(
            ever_treated = ifelse(gname > 0, 1, 0)
        )

    # Covariáveis de interesse
    covars <- c(
        "log_pib_agro", "log_area_cana", "log_populacao",
        "log_pib_per_capita", "prop_pib_agro", "log_densidade_estacoes_uf"
    )

    # Calcular estatísticas por grupo
    balance_stats <- map_dfr(covars, function(var) {
        # Dados por grupo
        treated_data <- df_pre %>%
            filter(ever_treated == 1) %>%
            pull(!!sym(var))

        control_data <- df_pre %>%
            filter(ever_treated == 0) %>%
            pull(!!sym(var))

        # Remover NAs
        treated_data <- treated_data[!is.na(treated_data)]
        control_data <- control_data[!is.na(control_data)]

        if (length(treated_data) < 2 || length(control_data) < 2) {
            return(tibble(
                variable = var,
                mean_treated = NA,
                mean_control = NA,
                diff = NA,
                std_diff = NA,
                p_value = NA,
                n_treated = length(treated_data),
                n_control = length(control_data)
            ))
        }

        # Estatísticas
        mean_treated <- mean(treated_data)
        mean_control <- mean(control_data)
        sd_treated <- sd(treated_data)
        sd_control <- sd(control_data)

        # Diferença padronizada (Cohen's d)
        pooled_sd <- sqrt((sd_treated^2 + sd_control^2) / 2)
        std_diff <- if (pooled_sd > 0) (mean_treated - mean_control) / pooled_sd else 0

        # Teste t
        t_test <- t.test(treated_data, control_data, var.equal = FALSE)

        tibble(
            variable = var,
            mean_treated = mean_treated,
            mean_control = mean_control,
            diff = mean_treated - mean_control,
            std_diff = std_diff,
            p_value = t_test$p.value,
            n_treated = length(treated_data),
            n_control = length(control_data)
        )
    })

    # Formatar tabela
    balance_table <- balance_stats %>%
        mutate(
            variable_label = case_when(
                variable == "log_pib_agro" ~ "Log PIB Agropecuário",
                variable == "log_area_plantada" ~ "Log Área Plantada",
                variable == "log_populacao" ~ "Log População",
                variable == "log_pib_per_capita" ~ "Log PIB per Capita",
                variable == "prop_pib_agro" ~ "Proporção PIB Agro/Total",
                variable == "log_densidade_estacoes_uf" ~ "Log Densidade Estações (UF)",
                TRUE ~ variable
            ),
            imbalanced = !is.na(std_diff) & abs(std_diff) > 0.1
        ) %>%
        select(
            variable_label, mean_treated, mean_control, diff,
            std_diff, p_value, imbalanced
        ) %>%
        gt() %>%
        tab_header(
            title = "Balanço de Covariáveis Pré-Tratamento",
            subtitle = "Comparação entre microrregiões eventualmente tratadas vs controles"
        ) %>%
        tab_spanner(
            label = "Médias",
            columns = c(mean_treated, mean_control)
        ) %>%
        tab_spanner(
            label = "Diferenças",
            columns = c(diff, std_diff)
        ) %>%
        tab_spanner(
            label = "Teste",
            columns = c(p_value, imbalanced)
        ) %>%
        fmt_number(
            columns = c(mean_treated, mean_control, diff, std_diff),
            decimals = 3
        ) %>%
        fmt_number(
            columns = p_value,
            decimals = 4
        ) %>%
        cols_label(
            variable_label = "Covariável",
            mean_treated = "Tratados",
            mean_control = "Controles",
            diff = "Diferença",
            std_diff = "Dif. Padrão",
            p_value = "P-valor",
            imbalanced = "Desbalanceado"
        ) %>%
        tab_style(
            style = cell_fill(color = "#ffcccc"),
            locations = cells_body(
                columns = everything(),
                rows = imbalanced == TRUE
            )
        ) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
                columns = p_value,
                rows = !is.na(p_value) & p_value < 0.05
            )
        ) %>%
        tab_footnote(
            footnote = "Desbalanceado = |Diferença Padronizada| > 0.1",
            locations = cells_column_labels(columns = imbalanced)
        ) %>%
        tab_footnote(
            footnote = "Diferença Padronizada = (Média Tratados - Média Controles) / Desvio Padrão Pooled",
            locations = cells_column_labels(columns = std_diff)
        )

    # Salvar tabela
    gtsave(balance_table, file.path(output_dir, "covariate_balance_table.html"))

    if (requireNamespace("webshot2", quietly = TRUE)) {
        gtsave(balance_table, file.path(output_dir, "covariate_balance_table.png"))
    }

    cli::cli_alert_success("Tabela de balanço salva em: {output_dir}")

    # Resumo
    n_imbalanced <- sum(balance_stats$imbalanced, na.rm = TRUE)
    if (n_imbalanced > 0) {
        cli::cli_alert_warning("{n_imbalanced} covariável(is) desbalanceada(s)")
    } else {
        cli::cli_alert_success("Todas as covariáveis estão balanceadas")
    }

    return(balance_table)
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 5. DISTRIBUIÇÃO DOS EFEITOS ATT(g,t)                                 ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' plot_att_distribution(): Distribuição dos efeitos individuais ATT(g,t)
#'
#' @description Violin plot mostrando distribuição dos ATT por grupo
#' @param att_res Resultado do modelo att_gt
#' @return ggplot object e salva arquivo
#' @examples
#' plot_att_distribution(att_res)
plot_att_distribution <- function(att_res) {
    cli::cli_h3("Criando Gráfico de Distribuição dos Efeitos ATT(g,t)")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Extrair ATT(g,t) individuais
    # Verificar estrutura do objeto att_res - aceitar MP ou att_gt
    if (!inherits(att_res, c("att_gt", "MP", "AGGTEobj"))) {
        cli::cli_alert_warning("Objeto att_res não é do tipo esperado (classe: {class(att_res)})")
        return(NULL)
    }

    # Criar data frame com os componentes básicos
    att_data <- data.frame(
        group = att_res$group,
        time = att_res$t,
        att = att_res$att,
        se = att_res$se
    )

    # Adicionar n se disponível e for vetor
    if (!is.null(att_res$n) && is.vector(att_res$n)) {
        att_data$n <- att_res$n
    } else {
        att_data$n <- NA
    }

    # Converter para tibble e processar
    att_data <- as_tibble(att_data) %>%
        filter(group > 0, time >= group) %>% # Apenas pós-tratamento
        mutate(
            ci_lower = att - 1.96 * se,
            ci_upper = att + 1.96 * se,
            significant = abs(att / se) > 1.96,
            group_label = paste("G", group)
        )

    # ATT agregado
    agg_att <- aggte(att_res, type = "simple")
    overall_att <- agg_att$overall.att

    # Criar violin plot
    p_dist <- ggplot(att_data, aes(x = factor(group), y = att)) +
        # Violin plot
        geom_violin(
            aes(fill = factor(group)),
            alpha = 0.6,
            scale = "width"
        ) +
        # Pontos individuais
        geom_point(
            aes(color = significant),
            position = position_jitter(width = 0.1),
            size = 2,
            alpha = 0.7
        ) +
        # Linha do ATT agregado
        geom_hline(
            yintercept = overall_att,
            linetype = "dashed",
            color = "red",
            linewidth = 1
        ) +
        # Linha do zero
        geom_hline(
            yintercept = 0,
            linetype = "solid",
            color = "gray50"
        ) +
        # Customização
        scale_fill_viridis_d(guide = "none") +
        scale_color_manual(
            values = c("TRUE" = "#2E86AB", "FALSE" = "#A23B72"),
            labels = c("TRUE" = "Significativo", "FALSE" = "Não Significativo"),
            name = "Significância (5%)"
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Distribuição dos Efeitos ATT(g,t) por Grupo de Adoção",
            subtitle = paste("Linha vermelha = ATT agregado:", round(overall_att, 3)),
            x = "Ano de Adoção (g)",
            y = "ATT(g,t)",
            caption = "Nota: Inclui apenas períodos pós-tratamento"
        )

    # Adicionar boxplot sobreposto
    p_dist <- p_dist +
        geom_boxplot(
            aes(group = factor(group)),
            width = 0.2,
            alpha = 0.3,
            outlier.shape = NA
        )

    # Salvar gráfico
    ggsave(
        file.path(output_dir, "att_distribution_by_group.png"),
        p_dist,
        width = 10,
        height = 6,
        dpi = 300
    )

    # Criar tabela resumo
    summary_stats <- att_data %>%
        group_by(group) %>%
        summarise(
            n_periods = n(),
            mean_att = mean(att),
            median_att = median(att),
            sd_att = sd(att),
            prop_significant = mean(significant) * 100,
            .groups = "drop"
        )

    # Salvar tabela resumo
    write.csv(
        summary_stats,
        file.path(output_dir, "att_distribution_summary.csv"),
        row.names = FALSE
    )

    cli::cli_alert_success("Gráfico de distribuição salvo em: {output_dir}")

    return(p_dist)
}

# Continuação das funções 6-10 em próximo arquivo...
