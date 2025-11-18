# ══════════════════════════════════════════════════════════════════════════════
# VISUALIZAÇÕES E TABELAS COMPLEMENTARES - PARTE 2
# ══════════════════════════════════════════════════════════════════════════════

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 6. ANÁLISE DE SENSIBILIDADE AO PERÍODO                               ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' sensitivity_analysis_period(): Análise de sensibilidade temporal
#'
#' @description Mostra como o ATT muda ao excluir diferentes períodos
#' @param df DataFrame limpo com dados do painel
#' @return ggplot object e salva arquivo
#' @examples
#' sensitivity_analysis_period(df_clean)
sensitivity_analysis_period <- function(df) {
    cli::cli_h3("Realizando Análise de Sensibilidade ao Período")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Definir cenários
    scenarios <- list(
        "Completo (2003-2023)" = c(2003, 2023),
        "Excluindo Início (2006-2023)" = c(2006, 2023),
        "Excluindo Final (2003-2019)" = c(2003, 2019),
        "Excluindo COVID (2003-2019)" = c(2003, 2019),
        "Período Central (2010-2019)" = c(2010, 2019),
        "Pré-COVID (2003-2019)" = c(2003, 2019)
    )

    # Calcular ATT para cada cenário
    results <- map_dfr(names(scenarios), function(scenario_name) {
        years <- scenarios[[scenario_name]]

        cli::cli_alert_info("Calculando ATT para: {scenario_name}")

        # Filtrar dados
        df_scenario <- df %>%
            filter(ano >= years[1] & ano <= years[2])

        # Tentar estimar modelo
        tryCatch(
            {
                att_res <- att_gt(
                    yname = "log_pib_agro",
                    tname = "ano",
                    idname = "id_microrregiao",
                    gname = "gname",
                    data = df_scenario,
                    control_group = "notyettreated",
                    # allow_unbalanced_panel = TRUE,
                    est_method = "dr",
                    print_details = FALSE
                )

                # Agregar resultados
                agg_res <- aggte(att_res, type = "simple")

                # Criar data.frame primeiro para evitar problemas
                data.frame(
                    scenario = scenario_name,
                    att = agg_res$overall.att,
                    se = agg_res$overall.se,
                    ci_lower = agg_res$overall.att - 1.96 * agg_res$overall.se,
                    ci_upper = agg_res$overall.att + 1.96 * agg_res$overall.se,
                    n_treated = sum(df_scenario$treated),
                    years_range = paste(years[1], "-", years[2])
                )
            },
            error = function(e) {
                cli::cli_alert_warning("Erro no cenário {scenario_name}: {e$message}")
                data.frame(
                    scenario = scenario_name,
                    att = NA,
                    se = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    n_treated = NA,
                    years_range = paste(years[1], "-", years[2])
                )
            }
        )
    })

    # Filtrar resultados válidos
    results_valid <- results %>%
        filter(!is.na(att))

    if (nrow(results_valid) == 0) {
        cli::cli_alert_danger("Nenhum cenário produziu resultados válidos")
        return(NULL)
    }

    # Criar gráfico
    p_sens <- ggplot(results_valid, aes(x = reorder(scenario, att), y = att)) +
        geom_point(size = 4, color = "#2E86AB") +
        geom_errorbar(
            aes(ymin = ci_lower, ymax = ci_upper),
            width = 0.2,
            linewidth = 1,
            color = "#2E86AB"
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        coord_flip() +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.text.y = element_text(size = 10)
        ) +
        labs(
            title = "Análise de Sensibilidade: ATT por Período",
            subtitle = "Efeito do tratamento sob diferentes recortes temporais",
            x = "Cenário",
            y = "ATT (Log PIB Agropecuário)",
            caption = "Barras de erro = IC 95%"
        )

    # Adicionar labels com valores
    p_sens <- p_sens +
        geom_text(
            aes(label = round(att, 3)),
            hjust = -0.2,
            size = 3.5
        )

    # Salvar gráfico
    ggsave(
        file.path(output_dir, "sensitivity_analysis_period.png"),
        p_sens,
        width = 10,
        height = 6,
        dpi = 300
    )

    # Salvar tabela de resultados
    write.csv(
        results_valid,
        file.path(output_dir, "sensitivity_analysis_results.csv"),
        row.names = FALSE
    )

    cli::cli_alert_success("Análise de sensibilidade salva em: {output_dir}")

    return(p_sens)
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 7. TABELA DE DECOMPOSIÇÃO DOS EFEITOS                                ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' create_effect_decomposition_table(): Decomposição dos efeitos por grupo
#'
#' @description Mostra contribuição de cada grupo para o ATT total
#' @param att_res Resultado do modelo att_gt
#' @param df DataFrame com dados
#' @return Objeto GT e salva arquivo
#' @examples
#' create_effect_decomposition_table(att_res, df_clean)
create_effect_decomposition_table <- function(att_res, df) {
    cli::cli_h3("Criando Tabela de Decomposição dos Efeitos")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Verificar tipo do objeto - aceitar MP ou att_gt
    if (!inherits(att_res, c("att_gt", "MP", "AGGTEobj"))) {
        cli::cli_alert_warning("Objeto att_res não é do tipo esperado (classe: {class(att_res)})")
        return(NULL)
    }

    # Agregar por grupo
    agg_group <- tryCatch(
        {
            aggte(att_res, type = "group")
        },
        error = function(e) {
            cli::cli_alert_warning("Erro ao agregar por grupo: {e$message}")
            return(NULL)
        }
    )

    if (is.null(agg_group)) {
        return(NULL)
    }

    # Extrair informações com data.frame primeiro
    group_effects <- data.frame(
        group = agg_group$egt,
        att_group = agg_group$att.egt,
        se_group = agg_group$se.egt
    )

    # Converter para tibble e filtrar
    group_effects <- as_tibble(group_effects) %>%
        filter(group > 0)

    # Contar microrregiões por grupo
    group_counts <- df %>%
        filter(gname > 0) %>%
        distinct(id_microrregiao, gname) %>%
        count(gname, name = "n_micro")

    # ATT total
    agg_simple <- aggte(att_res, type = "simple")
    att_total <- agg_simple$overall.att

    # Calcular decomposição
    decomposition <- group_effects %>%
        left_join(group_counts, by = c("group" = "gname")) %>%
        mutate(
            # Peso do grupo (proporcional ao número de unidades)
            weight = n_micro / sum(n_micro),
            # Contribuição absoluta
            contribution = att_group * weight,
            # Contribuição percentual
            contribution_pct = (contribution / att_total) * 100,
            # Significância
            t_stat = att_group / se_group,
            p_value = 2 * (1 - pnorm(abs(t_stat))),
            significant = p_value < 0.05
        ) %>%
        arrange(group)

    # Criar tabela GT
    decomp_table <- decomposition %>%
        select(group, n_micro, att_group, weight, contribution, contribution_pct, significant) %>%
        gt() %>%
        tab_header(
            title = "Decomposição dos Efeitos do Tratamento por Grupo",
            subtitle = paste("ATT Total:", round(att_total, 3))
        ) %>%
        fmt_number(
            columns = c(att_group, weight, contribution),
            decimals = 3
        ) %>%
        fmt_number(
            columns = contribution_pct,
            decimals = 1
        ) %>%
        fmt_number(
            columns = n_micro,
            decimals = 0
        ) %>%
        cols_label(
            group = "Ano de Adoção",
            n_micro = "Nº Microrregiões",
            att_group = "ATT do Grupo",
            weight = "Peso",
            contribution = "Contribuição",
            contribution_pct = "Contribuição (%)",
            significant = "Significativo"
        ) %>%
        tab_style(
            style = cell_fill(color = "#e6f3ff"),
            locations = cells_body(
                columns = everything(),
                rows = significant == TRUE
            )
        ) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
                columns = att_group,
                rows = significant == TRUE
            )
        ) %>%
        tab_footnote(
            footnote = "Peso = proporção de microrregiões no grupo",
            locations = cells_column_labels(columns = weight)
        ) %>%
        tab_footnote(
            footnote = "Contribuição = ATT do Grupo × Peso",
            locations = cells_column_labels(columns = contribution)
        ) %>%
        grand_summary_rows(
            columns = c(n_micro, contribution, contribution_pct),
            fns = list(
                Total = ~ sum(.)
            ),
            fmt = ~ fmt_number(., decimals = 1)
        )

    # Salvar tabela
    gtsave(decomp_table, file.path(output_dir, "effect_decomposition_table.html"))

    if (requireNamespace("webshot2", quietly = TRUE)) {
        gtsave(decomp_table, file.path(output_dir, "effect_decomposition_table.png"))
    }

    # Criar gráfico complementar
    p_decomp <- ggplot(decomposition, aes(x = factor(group), y = contribution_pct)) +
        geom_col(aes(fill = significant), alpha = 0.8) +
        geom_text(aes(label = round(contribution_pct, 1)), vjust = -0.5, size = 3) +
        scale_fill_manual(
            values = c("TRUE" = "#2E86AB", "FALSE" = "#cccccc"),
            labels = c("TRUE" = "Significativo", "FALSE" = "Não Significativo"),
            name = "Significância (5%)"
        ) +
        theme_minimal() +
        labs(
            title = "Contribuição Percentual por Grupo de Adoção",
            subtitle = "Para o ATT total do modelo",
            x = "Ano de Adoção",
            y = "Contribuição (%)"
        )

    ggsave(
        file.path(output_dir, "effect_decomposition_chart.png"),
        p_decomp,
        width = 10,
        height = 6,
        dpi = 300
    )

    cli::cli_alert_success("Tabela de decomposição salva em: {output_dir}")

    return(decomp_table)
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 8. GRÁFICO DE TENDÊNCIAS POR QUARTIS DE TAMANHO                      ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' plot_trends_by_size_quartile(): Tendências por tamanho inicial
#'
#' @description Mostra evolução do PIB agro por quartis de área plantada
#' @param df DataFrame limpo com dados do painel
#' @return ggplot object e salva arquivo
#' @examples
#' plot_trends_by_size_quartile(df_clean)
plot_trends_by_size_quartile <- function(df) {
    cli::cli_h3("Criando Gráfico de Tendências por Quartis de Tamanho")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Calcular área inicial (primeiro ano disponível)
    # Usa area_plantada_cana (outcome principal) para definir quartis
    initial_area <- df %>%
        group_by(id_microrregiao) %>%
        filter(!is.na(area_plantada_cana)) %>%
        summarise(
            initial_area = first(area_plantada_cana),
            gname = first(gname),
            .groups = "drop"
        )

    # Criar quartis
    initial_area <- initial_area %>%
        mutate(
            quartile = cut(
                initial_area,
                breaks = quantile(initial_area, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                labels = c("Q1 (Menor)", "Q2", "Q3", "Q4 (Maior)"),
                include.lowest = TRUE
            )
        )

    # Juntar com dados principais
    df_quartiles <- df %>%
        inner_join(initial_area, by = c("id_microrregiao", "gname")) %>%
        filter(!is.na(quartile))

    # Calcular médias por quartil, ano e status de tratamento
    trends_quartile <- df_quartiles %>%
        mutate(
            treated_status = ifelse(treated == 1, "Tratado", "Controle"),
            # Tempo relativo ao tratamento
            time_to_treat = ifelse(gname > 0, ano - gname, NA)
        ) %>%
        filter(abs(time_to_treat) <= 5 | is.na(time_to_treat)) %>% # -5 a +5 anos
        group_by(quartile, time_to_treat, treated_status) %>%
        summarise(
            mean_pib_agro = mean(log_pib_agro, na.rm = TRUE),
            se_pib_agro = sd(log_pib_agro, na.rm = TRUE) / sqrt(n()),
            n_obs = n(),
            .groups = "drop"
        )

    # Criar gráfico
    p_quartiles <- ggplot(
        trends_quartile %>% filter(!is.na(time_to_treat)),
        aes(
            x = time_to_treat, y = mean_pib_agro,
            color = treated_status, linetype = treated_status
        )
    ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        geom_ribbon(
            aes(
                ymin = mean_pib_agro - 1.96 * se_pib_agro,
                ymax = mean_pib_agro + 1.96 * se_pib_agro,
                fill = treated_status
            ),
            alpha = 0.2
        ) +
        geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
        facet_wrap(~quartile, scales = "free_y", nrow = 2) +
        scale_color_manual(values = c("Tratado" = "#2E86AB", "Controle" = "#A23B72")) +
        scale_fill_manual(values = c("Tratado" = "#2E86AB", "Controle" = "#A23B72")) +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12)
        ) +
        labs(
            title = "Evolução do PIB Agropecuário por Quartis de Área Plantada Inicial",
            subtitle = "5 anos antes até 5 anos depois do tratamento",
            x = "Anos Relativos ao Tratamento",
            y = "Log PIB Agropecuário",
            color = "Status",
            fill = "Status",
            linetype = "Status",
            caption = "Quartis baseados na área plantada no primeiro ano observado"
        )

    # Salvar gráfico
    ggsave(
        file.path(output_dir, "trends_by_size_quartile.png"),
        p_quartiles,
        width = 12,
        height = 8,
        dpi = 300
    )

    cli::cli_alert_success("Gráfico por quartis salvo em: {output_dir}")

    return(p_quartiles)
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 9. DASHBOARD DE QUALIDADE DOS DADOS                                  ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' create_data_quality_dashboard(): Dashboard visual de qualidade
#'
#' @description Painel mostrando completude e qualidade dos dados
#' @param df DataFrame limpo com dados do painel
#' @return Lista de gráficos e salva arquivos
#' @examples
#' create_data_quality_dashboard(df_clean)
create_data_quality_dashboard <- function(df) {
    cli::cli_h3("Criando Dashboard de Qualidade dos Dados")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # 1. Heatmap de valores faltantes
    vars_check <- c(
        "log_pib_agro", "log_pib_nao_agro", "log_area_cana",
        "log_populacao", "log_pib_per_capita", "log_densidade_estacoes_uf"
    )

    missing_summary <- df %>%
        select(ano, all_of(vars_check)) %>%
        group_by(ano) %>%
        summarise(across(
            everything(),
            ~ sum(is.na(.)) / n() * 100,
            .names = "{.col}_missing"
        ), .groups = "drop") %>%
        pivot_longer(
            cols = -ano,
            names_to = "variable",
            values_to = "missing_pct"
        ) %>%
        mutate(
            variable = gsub("_missing", "", variable),
            variable_label = case_when(
                variable == "log_pib_agro" ~ "PIB Agro",
                variable == "log_pib_nao_agro" ~ "PIB Não-Agro",
                variable == "log_area_cana" ~ "Área Cana",
                variable == "log_populacao" ~ "População",
                variable == "log_pib_per_capita" ~ "PIB per Capita",
                variable == "log_densidade_estacoes_uf" ~ "Densidade Estações",
                TRUE ~ variable
            )
        )

    p_heatmap <- ggplot(
        missing_summary,
        aes(x = factor(ano), y = variable_label, fill = missing_pct)
    ) +
        geom_tile() +
        geom_text(aes(label = round(missing_pct, 1)), size = 3) +
        scale_fill_gradient2(
            low = "green",
            mid = "yellow",
            high = "red",
            midpoint = 5,
            name = "% Missing"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 14, face = "bold")
        ) +
        labs(
            title = "Heatmap de Valores Faltantes por Ano",
            x = "Ano",
            y = "Variável"
        )

    # 2. Histograma de completude por microrregião
    completeness_micro <- df %>%
        group_by(id_microrregiao) %>%
        summarise(
            n_years = n(),
            n_complete = sum(!is.na(log_pib_agro)),
            completeness = n_complete / n_years * 100,
            .groups = "drop"
        )

    p_hist <- ggplot(completeness_micro, aes(x = completeness)) +
        geom_histogram(bins = 20, fill = "#2E86AB", alpha = 0.8) +
        geom_vline(
            xintercept = mean(completeness_micro$completeness),
            linetype = "dashed", color = "red", linewidth = 1
        ) +
        theme_minimal() +
        labs(
            title = "Distribuição de Completude por Microrregião",
            subtitle = paste("Média:", round(mean(completeness_micro$completeness), 1), "%"),
            x = "Completude (%)",
            y = "Número de Microrregiões"
        )

    # 3. Atrito ao longo do tempo
    attrition <- df %>%
        group_by(ano) %>%
        summarise(
            n_micro = n_distinct(id_microrregiao),
            n_complete = sum(!is.na(log_pib_agro)),
            .groups = "drop"
        ) %>%
        mutate(
            attrition_rate = (max(n_micro) - n_micro) / max(n_micro) * 100
        )

    p_attrition <- ggplot(attrition, aes(x = ano)) +
        geom_line(aes(y = n_micro), linewidth = 1.2, color = "#2E86AB") +
        geom_point(aes(y = n_micro), size = 3, color = "#2E86AB") +
        geom_text(aes(y = n_micro, label = n_micro), vjust = -1, size = 3) +
        scale_y_continuous(limits = c(0, max(attrition$n_micro) * 1.1)) +
        theme_minimal() +
        labs(
            title = "Número de Microrregiões Observadas por Ano",
            subtitle = "Verificação de atrito no painel",
            x = "Ano",
            y = "Número de Microrregiões"
        )

    # 4. Tabela resumo
    quality_summary <- tibble(
        Métrica = c(
            "Total de Observações",
            "Microrregiões Únicas",
            "Anos Cobertos",
            "Taxa Média de Missing (PIB Agro)",
            "Microrregiões com Dados Completos",
            "Proporção de Painel Balanceado"
        ),
        Valor = c(
            nrow(df),
            n_distinct(df$id_microrregiao),
            n_distinct(df$ano),
            paste0(round(mean(is.na(df$log_pib_agro)) * 100, 2), "%"),
            sum(completeness_micro$completeness == 100),
            paste0(round(mean(completeness_micro$n_years == max(completeness_micro$n_years)) * 100, 1), "%")
        )
    )

    quality_table <- quality_summary %>%
        gt() %>%
        tab_header(
            title = "Resumo da Qualidade dos Dados"
        )

    # Salvar componentes
    ggsave(
        file.path(output_dir, "data_quality_heatmap.png"),
        p_heatmap,
        width = 10,
        height = 6,
        dpi = 300
    )

    ggsave(
        file.path(output_dir, "data_quality_histogram.png"),
        p_hist,
        width = 8,
        height = 6,
        dpi = 300
    )

    ggsave(
        file.path(output_dir, "data_quality_attrition.png"),
        p_attrition,
        width = 10,
        height = 6,
        dpi = 300
    )

    gtsave(quality_table, file.path(output_dir, "data_quality_summary.html"))

    # Criar dashboard combinado
    if (requireNamespace("patchwork", quietly = TRUE)) {
        dashboard <- (p_heatmap / (p_hist | p_attrition)) +
            plot_annotation(
                title = "Dashboard de Qualidade dos Dados",
                subtitle = "Análise de completude e consistência do painel"
            )

        ggsave(
            file.path(output_dir, "data_quality_dashboard_combined.png"),
            dashboard,
            width = 14,
            height = 10,
            dpi = 300
        )
    }

    cli::cli_alert_success("Dashboard de qualidade salvo em: {output_dir}")

    return(list(
        heatmap = p_heatmap,
        histogram = p_hist,
        attrition = p_attrition,
        summary = quality_table
    ))
}

# ╔═══════════════════════════════════════════════════════════════════════╗ #
# ║ 10. SIMULAÇÃO DE PODER ESTATÍSTICO                                   ║ #
# ╚═══════════════════════════════════════════════════════════════════════╝ #
#' power_analysis_simulation(): Análise de poder estatístico
#'
#' @description Simula poder para diferentes tamanhos de efeito
#' @param df DataFrame limpo com dados do painel
#' @param n_sims Número de simulações
#' @return ggplot object e salva arquivo
#' @examples
#' power_analysis_simulation(df_clean, n_sims = 100)
power_analysis_simulation <- function(df, n_sims = 100) {
    cli::cli_h3("Realizando Análise de Poder Estatístico")

    output_dir <- here::here("data", "outputs", "additional_figures")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Parâmetros da simulação
    effect_sizes <- seq(0, 0.15, by = 0.01) # 0% a 15%
    alpha_levels <- c(0.01, 0.05, 0.10)

    # Características dos dados reais
    n_treated <- sum(df$gname > 0 & df$treated == 1)
    n_control <- sum(df$treated == 0)
    sd_outcome <- sd(df$log_pib_agro, na.rm = TRUE)

    cli::cli_alert_info("N tratados: {n_treated}, N controles: {n_control}")
    cli::cli_alert_info("Desvio padrão do outcome: {round(sd_outcome, 3)}")

    # Função para calcular poder
    calculate_power <- function(effect_size, alpha, n_treat, n_ctrl, sd_pool) {
        # Erro padrão da diferença
        se_diff <- sd_pool * sqrt(1 / n_treat + 1 / n_ctrl)

        # Estatística t sob H1
        t_stat <- effect_size / se_diff

        # Valor crítico
        t_crit <- qt(1 - alpha / 2, df = n_treat + n_ctrl - 2)

        # Poder = P(|T| > t_crit | H1 verdadeira)
        power <- pt(t_crit - t_stat, df = n_treat + n_ctrl - 2, lower.tail = FALSE) +
            pt(-t_crit - t_stat, df = n_treat + n_ctrl - 2, lower.tail = TRUE)

        return(power)
    }

    # Calcular poder para cada combinação
    power_results <- expand.grid(
        effect_size = effect_sizes,
        alpha = alpha_levels
    ) %>%
        mutate(
            power = map2_dbl(
                effect_size, alpha,
                ~ calculate_power(.x, .y, n_treated, n_control, sd_outcome)
            ),
            alpha_label = paste("α =", alpha)
        )

    # Criar gráfico
    p_power <- ggplot(
        power_results,
        aes(x = effect_size, y = power, color = factor(alpha_label))
    ) +
        geom_line(linewidth = 1.2) +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray50") +
        geom_vline(xintercept = 0.083, linetype = "solid", color = "red", alpha = 0.7) +
        scale_x_continuous(labels = scales::percent_format()) +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_color_manual(values = c("#2E86AB", "#F18F01", "#A23B72")) +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12)
        ) +
        labs(
            title = "Análise de Poder Estatístico",
            subtitle = paste("Baseado em", n_treated, "obs. tratadas e", n_control, "controles"),
            x = "Tamanho do Efeito",
            y = "Poder Estatístico",
            color = "Nível de Significância",
            caption = "Linha vermelha = efeito observado (8.3%); Linha cinza = poder de 80%"
        ) +
        annotate(
            "text",
            x = 0.083, y = 0.1,
            label = "Efeito\nobservado",
            hjust = 0, vjust = 0,
            size = 3, color = "red"
        )

    # Salvar gráfico
    ggsave(
        file.path(output_dir, "power_analysis_simulation.png"),
        p_power,
        width = 10,
        height = 6,
        dpi = 300
    )

    # Calcular poder para o efeito observado
    observed_power <- power_results %>%
        filter(abs(effect_size - 0.083) == min(abs(effect_size - 0.083)))

    cli::cli_alert_info("Poder para efeito observado (8.3%):")
    print(observed_power)

    # Salvar resultados
    write.csv(
        power_results,
        file.path(output_dir, "power_analysis_results.csv"),
        row.names = FALSE
    )

    cli::cli_alert_success("Análise de poder salva em: {output_dir}")

    return(p_power)
}

# Fim do arquivo
