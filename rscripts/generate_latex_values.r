#!/usr/bin/env Rscript
# Script para gerar comandos LaTeX com valores atualizados dos resultados

library(readr)
library(dplyr)
library(glue)

# Função para formatar números para LaTeX
format_number <- function(x, digits = 4, percent = FALSE) {
    # Handle NA values
    if (is.na(x)) {
        return("NA")
    }

    if (percent) {
        pct_value <- x * 100
        if (pct_value < 0.1) {
            return(sprintf("%.2f\\%%", pct_value))  # Show 2 decimals for very small percentages
        } else {
            return(sprintf("%.1f\\%%", pct_value))
        }
    } else if (abs(x) < 0.01) {
        return(sprintf("%.3f", x))
    } else {
        return(sprintf("%.3f", x))
    }
}

# Função para formatar p-valor
format_pvalue <- function(p) {
    if (p < 0.001) {
        return("< 0,001")
    }
    if (p < 0.01) {
        return(sprintf("%.3f", p))
    }
    return(sprintf("%.3f", p))
}

# Ler dados do placebo
placebo_data <- read_csv("data/outputs/placebo_random_summary.csv", show_col_types = FALSE)

# Ler dados principais detalhados (para z, p, CI)
# CORRIGIDO: Agora lê do arquivo correto de valor de produção (não área)
att_summary_path <- "data/outputs/att_summary_main_valor_cana.csv"
if (file.exists(att_summary_path)) {
    att_summary <- read_csv(att_summary_path, show_col_types = FALSE)
}

# Ler dados principais (tabela completa com robustez)
main_results_path <- "data/outputs/main_results_table.csv"
if (file.exists(main_results_path)) {
    main_results <- read_csv(main_results_path, show_col_types = FALSE)
}

# Ler dados do Valor de Produção Cana em microrregiões produtoras
valor_filtered_path <- "data/outputs/att_summary_valor_cana_producers.csv"
if (file.exists(valor_filtered_path)) {
    valor_filtered <- read_csv(valor_filtered_path, show_col_types = FALSE)
}

# Criar conteúdo do arquivo LaTeX
latex_content <- c(
    "% Arquivo gerado automaticamente por generate_latex_values.r",
    paste0("% Ultima atualizacao: ", Sys.Date()),
    "",
    "% Valores do teste placebo aleatório",
    paste0("\\newcommand{\\placebotruatt}{", format_number(placebo_data$true_att[1]), "}"),
    paste0("\\newcommand{\\placebopvalue}{", format_pvalue(placebo_data$p_value_empirical[1]), "}"),
    paste0("\\newcommand{\\placebolower}{", format_number(placebo_data$placebo_ci_95_lower[1]), "}"),
    paste0("\\newcommand{\\placeboupper}{", format_number(placebo_data$placebo_ci_95_upper[1]), "}"),
    paste0("\\newcommand{\\placebonsims}{", placebo_data$n_valid_sims[1], "}"),
    paste0("\\newcommand{\\placebomean}{", format_number(placebo_data$placebo_mean[1]), "}"),
    paste0("\\newcommand{\\placebosd}{", format_number(placebo_data$placebo_sd[1]), "}"),
    "",
    "% Valores de precisão do p-valor (Monte Carlo)",
    paste0("\\newcommand{\\placebopvaluese}{", format_number(placebo_data$p_value_se[1]), "}"),
    paste0("\\newcommand{\\placebopvaluelower}{", format_number(placebo_data$p_value_ci_lower[1]), "}"),
    paste0("\\newcommand{\\placebopvalueupper}{", format_number(placebo_data$p_value_ci_upper[1]), "}"),
    paste0("\\newcommand{\\placebonextremes}{", placebo_data$n_extremes[1], "}"),
    "",
    "% Valores formatados para texto",
    paste0("\\newcommand{\\placebotruattpct}{", format_number(placebo_data$true_att[1], percent = TRUE), "}"),
    paste0(
        "\\newcommand{\\placebopvaluepct}{",
        ifelse(placebo_data$p_value_empirical[1] == 0,
            "< 1\\%",
            format_number(placebo_data$p_value_empirical[1], percent = TRUE)
        ), "}"
    ),
    ""
)

# Se existirem resultados principais detalhados, adicionar
if (exists("att_summary") && nrow(att_summary) > 0) {
    latex_content <- c(
        latex_content,
        "% Valores do modelo principal (resultado detalhado)",
        paste0("\\newcommand{\\mainatt}{", format_number(att_summary$att[1]), "}"),
        paste0("\\newcommand{\\mainse}{", format_number(att_summary$se[1]), "}"),
        paste0("\\newcommand{\\mainz}{", format_number(att_summary$z[1], digits = 2), "}"),
        paste0("\\newcommand{\\mainp}{", format_number(att_summary$p[1], digits = 4), "}"),
        paste0("\\newcommand{\\maincilower}{", format_number(att_summary$ci_low[1]), "}"),
        paste0("\\newcommand{\\mainciupper}{", format_number(att_summary$ci_high[1]), "}"),
        paste0("\\newcommand{\\mainattpct}{", format_number(att_summary$att[1], percent = TRUE), "}"),
        ""
    )
}

# Adicionar valores de outcomes alternativos
alternative_outcomes_path <- "data/outputs/alternative_outcomes_analysis.csv"
if (file.exists(alternative_outcomes_path)) {
    alt_outcomes <- read_csv(alternative_outcomes_path, show_col_types = FALSE)

    # Área Cana
    area_cana_row <- alt_outcomes[grepl("Área Cana", alt_outcomes$outcome_label), ]
    if (nrow(area_cana_row) > 0) {
        latex_content <- c(
            latex_content,
            "% Valores de área cana (outcome secundário)",
            paste0("\\newcommand{\\areacanaatt}{", format_number(area_cana_row$att[1]), "}"),
            paste0("\\newcommand{\\areacanaattpct}{", format_number(area_cana_row$att[1], percent = TRUE), "}"),
            paste0("\\newcommand{\\areacanase}{", format_number(area_cana_row$se[1]), "}"),
            paste0("\\newcommand{\\areacanap}{", format_number(area_cana_row$p_value[1], digits = 4), "}"),
            paste0("\\newcommand{\\areacanalower}{", format_number(area_cana_row$ci_lower[1]), "}"),
            paste0("\\newcommand{\\areacanaupper}{", format_number(area_cana_row$ci_upper[1]), "}"),
            ""
        )
    }

    # Área Soja
    area_soja_row <- alt_outcomes[grepl("Área Soja", alt_outcomes$outcome_label), ]
    if (nrow(area_soja_row) > 0) {
        latex_content <- c(
            latex_content,
            "% Valores de área soja",
            paste0("\\newcommand{\\areasojaat}{", format_number(area_soja_row$att[1]), "}"),
            paste0("\\newcommand{\\areasojase}{", format_number(area_soja_row$se[1]), "}"),
            paste0("\\newcommand{\\areasojap}{", format_number(area_soja_row$p_value[1], digits = 4), "}"),
            paste0("\\newcommand{\\areasojlower}{", format_number(area_soja_row$ci_lower[1]), "}"),
            paste0("\\newcommand{\\areasojaupper}{", format_number(area_soja_row$ci_upper[1]), "}"),
            ""
        )
    }

    # Área Arroz
    area_arroz_row <- alt_outcomes[grepl("Área Arroz", alt_outcomes$outcome_label), ]
    if (nrow(area_arroz_row) > 0) {
        latex_content <- c(
            latex_content,
            "% Valores de área arroz",
            paste0("\\newcommand{\\areaarrozatt}{", format_number(area_arroz_row$att[1]), "}"),
            paste0("\\newcommand{\\areaarrozse}{", format_number(area_arroz_row$se[1]), "}"),
            paste0("\\newcommand{\\areaarrozp}{", format_number(area_arroz_row$p_value[1], digits = 4), "}"),
            paste0("\\newcommand{\\areaarrozlower}{", format_number(area_arroz_row$ci_lower[1]), "}"),
            paste0("\\newcommand{\\areaarrozupper}{", format_number(area_arroz_row$ci_upper[1]), "}"),
            ""
        )
    }

    # Valor Soja
    valor_soja_row <- alt_outcomes[grepl("Valor Produção Soja", alt_outcomes$outcome_label), ]
    if (nrow(valor_soja_row) > 0) {
        latex_content <- c(
            latex_content,
            "% Valores de valor de produção soja",
            paste0("\\newcommand{\\valorsojaat}{", format_number(valor_soja_row$att[1]), "}"),
            paste0("\\newcommand{\\valorsojase}{", format_number(valor_soja_row$se[1]), "}"),
            paste0("\\newcommand{\\valorsojap}{", format_number(valor_soja_row$p_value[1], digits = 4), "}"),
            paste0("\\newcommand{\\valorsojlower}{", format_number(valor_soja_row$ci_lower[1]), "}"),
            paste0("\\newcommand{\\valorsojaupper}{", format_number(valor_soja_row$ci_upper[1]), "}"),
            ""
        )
    }

    # Valor Arroz
    valor_arroz_row <- alt_outcomes[grepl("Valor Produção Arroz", alt_outcomes$outcome_label), ]
    if (nrow(valor_arroz_row) > 0) {
        latex_content <- c(
            latex_content,
            "% Valores de valor de produção arroz",
            paste0("\\newcommand{\\valorarrozatt}{", format_number(valor_arroz_row$att[1]), "}"),
            paste0("\\newcommand{\\valorarrozse}{", format_number(valor_arroz_row$se[1]), "}"),
            paste0("\\newcommand{\\valorarrozp}{", format_number(valor_arroz_row$p_value[1], digits = 4), "}"),
            paste0("\\newcommand{\\valorarrozlower}{", format_number(valor_arroz_row$ci_lower[1]), "}"),
            paste0("\\newcommand{\\valorarrozupper}{", format_number(valor_arroz_row$ci_upper[1]), "}"),
            ""
        )
    }

    # Área Outras Lavouras
    area_outras_row <- alt_outcomes[grepl("Área Outras Lavouras", alt_outcomes$outcome_label), ]
    if (nrow(area_outras_row) > 0) {
        latex_content <- c(
            latex_content,
            "% Valores de área outras lavouras temporárias",
            paste0("\\newcommand{\\areaoutrasatt}{", format_number(area_outras_row$att[1]), "}"),
            paste0("\\newcommand{\\areaoutrasse}{", format_number(area_outras_row$se[1]), "}"),
            paste0("\\newcommand{\\areaoutrasp}{", format_number(area_outras_row$p_value[1], digits = 4), "}"),
            paste0("\\newcommand{\\areaoutraslower}{", format_number(area_outras_row$ci_lower[1]), "}"),
            paste0("\\newcommand{\\areaoutrasupper}{", format_number(area_outras_row$ci_upper[1]), "}"),
            ""
        )
    }

    # Valor Outras Lavouras
    valor_outras_row <- alt_outcomes[grepl("Valor Produção Outras", alt_outcomes$outcome_label), ]
    if (nrow(valor_outras_row) > 0) {
        latex_content <- c(
            latex_content,
            "% Valores de valor de produção outras lavouras",
            paste0("\\newcommand{\\valoroutrasatt}{", format_number(valor_outras_row$att[1]), "}"),
            paste0("\\newcommand{\\valoroutrasse}{", format_number(valor_outras_row$se[1]), "}"),
            paste0("\\newcommand{\\valoroutrasp}{", format_number(valor_outras_row$p_value[1], digits = 4), "}"),
            paste0("\\newcommand{\\valoroutraslower}{", format_number(valor_outras_row$ci_lower[1]), "}"),
            paste0("\\newcommand{\\valoroutrasupper}{", format_number(valor_outras_row$ci_upper[1]), "}"),
            ""
        )
    }

    # Se não existirem dados de outras lavouras, criar placeholders
    if (nrow(area_outras_row) == 0) {
        latex_content <- c(
            latex_content,
            "% Valores de área outras lavouras (placeholder - executar análise completa)",
            "\\newcommand{\\areaoutrasatt}{--}",
            "\\newcommand{\\areaoutrasse}{--}",
            "\\newcommand{\\areaoutrasp}{--}",
            "\\newcommand{\\areaoutraslower}{--}",
            "\\newcommand{\\areaoutrasupper}{--}",
            ""
        )
    }
    if (nrow(valor_outras_row) == 0) {
        latex_content <- c(
            latex_content,
            "% Valores de valor outras lavouras (placeholder - executar análise completa)",
            "\\newcommand{\\valoroutrasatt}{--}",
            "\\newcommand{\\valoroutrasse}{--}",
            "\\newcommand{\\valoroutrasp}{--}",
            "\\newcommand{\\valoroutraslower}{--}",
            "\\newcommand{\\valoroutrasupper}{--}",
            ""
        )
    }
}

# Se existirem resultados de robustez, adicionar
if (exists("main_results") && nrow(main_results) > 0) {
    # Helper function to parse CI string "[lower, upper]"
    parse_ci <- function(ci_str) {
        if (is.na(ci_str) || ci_str == "") return(c(NA, NA))
        # Remove brackets and split by comma
        ci_str <- gsub("\\[|\\]", "", ci_str)
        values <- as.numeric(unlist(strsplit(ci_str, ",")))
        return(values)
    }

    # Find robustness rows (they contain specific keywords)
    for (i in 1:nrow(main_results)) {
        analise <- main_results$Análise[i]

        # Parse ATT and SE
        att_val <- as.numeric(gsub("[^0-9.-]", "", main_results$ATT[i]))
        se_val <- as.numeric(gsub("[^0-9.-]", "", gsub("\\(|\\)", "", main_results$`Erro Padrão`[i])))
        ci_vals <- parse_ci(main_results$`IC 95%`[i])

        # Generate macros based on analysis type
        if (grepl("Sem Covariáveis", analise, ignore.case = TRUE)) {
            latex_content <- c(
                latex_content,
                "% Especificação: Sem Covariáveis",
                paste0("\\newcommand{\\nocovatt}{", format_number(att_val), "}"),
                paste0("\\newcommand{\\nocovse}{", format_number(se_val), "}"),
                paste0("\\newcommand{\\nocovlower}{", format_number(ci_vals[1]), "}"),
                paste0("\\newcommand{\\nocover}{", format_number(ci_vals[2]), "}"),
                ""
            )
        } else if (grepl("IPW", analise, ignore.case = TRUE)) {
            latex_content <- c(
                latex_content,
                "% Especificação: IPW",
                paste0("\\newcommand{\\ipwatt}{", format_number(att_val), "}"),
                paste0("\\newcommand{\\ipwse}{", format_number(se_val), "}"),
                paste0("\\newcommand{\\ipwlower}{", format_number(ci_vals[1]), "}"),
                paste0("\\newcommand{\\ipwupper}{", format_number(ci_vals[2]), "}"),
                ""
            )
        } else if (grepl("REG", analise, ignore.case = TRUE)) {
            latex_content <- c(
                latex_content,
                "% Especificação: Regressão de Resultado",
                paste0("\\newcommand{\\regatt}{", format_number(att_val), "}"),
                paste0("\\newcommand{\\regse}{", format_number(se_val), "}"),
                paste0("\\newcommand{\\reglower}{", format_number(ci_vals[1]), "}"),
                paste0("\\newcommand{\\regupper}{", format_number(ci_vals[2]), "}"),
                ""
            )
        } else if (grepl("Nevertreated", analise, ignore.case = TRUE)) {
            latex_content <- c(
                latex_content,
                "% Grupo de Controle: Never-treated",
                paste0("\\newcommand{\\nevertreatedatt}{", format_number(att_val), "}"),
                paste0("\\newcommand{\\nevertreatedse}{", format_number(se_val), "}"),
                paste0("\\newcommand{\\nevertreatedlower}{", format_number(ci_vals[1]), "}"),
                paste0("\\newcommand{\\nevertreatedupper}{", format_number(ci_vals[2]), "}"),
                ""
            )
        }
    }
}

# Se existirem resultados do Valor de Produção Cana filtrado, adicionar
if (exists("valor_filtered") && nrow(valor_filtered) > 0) {
    latex_content <- c(
        latex_content,
        "% Valores do Valor de Produção Cana em Microrregiões Produtoras",
        paste0("\\newcommand{\\valorfiltatt}{", format_number(valor_filtered$att[1]), "}"),
        paste0("\\newcommand{\\valorfiltse}{", format_number(valor_filtered$se[1]), "}"),
        paste0("\\newcommand{\\valorfiltcilower}{", format_number(valor_filtered$ci_low[1]), "}"),
        paste0("\\newcommand{\\valorfiltciupper}{", format_number(valor_filtered$ci_high[1]), "}"),
        paste0("\\newcommand{\\valorfiltatppct}{", format_number(valor_filtered$att[1], percent = TRUE), "}"),
        paste0("\\newcommand{\\valorfiltpvalue}{", format_pvalue(valor_filtered$p[1]), "}"),
        paste0("\\newcommand{\\valorfiltnmicro}{", valor_filtered$n_microregions[1], "}"),
        paste0("\\newcommand{\\valorfiltretention}{", format_number(valor_filtered$retention_rate[1], percent = TRUE), "}"),
        ""
    )
}

# Ler dados de sensibilidade temporal (se existir)
sensitivity_path <- "data/outputs/additional_figures/sensitivity_analysis_results.csv"
if (file.exists(sensitivity_path)) {
    sensitivity_data <- read_csv(sensitivity_path, show_col_types = FALSE)

    latex_content <- c(
        latex_content,
        "% Valores da análise de sensibilidade temporal"
    )

    # Mapear cenários exatos da tese (linha 1130-1132)
    # Use exact string matching to avoid duplicates
    scenario_names <- list(
        "Completo (2003-2023)" = "full",
        "Completo (2003-2021)" = "full",
        "Excluindo Início (2006-2023)" = "nostart",
        "Excluindo Início (2006-2021)" = "nostart",
        "Excluindo COVID (2003-2019)" = "nocovid"
    )

    # Track which commands we've already defined to avoid duplicates
    defined_commands <- character(0)

    for (i in 1:nrow(sensitivity_data)) {
        scenario <- sensitivity_data$scenario[i]
        if (!grepl("Período Central", scenario)) {
            # Find exact match in scenario_names
            cmd_name <- scenario_names[[scenario]]
            if (is.null(cmd_name)) {
                # If no exact match, create a unique name with letter prefix
                # (LaTeX can't handle \sens3att - it interprets 3 as parameter number)
                cmd_name <- paste0("sensalt", letters[i])
            }

            # Only add if not already defined
            if (!(cmd_name %in% defined_commands)) {
                defined_commands <- c(defined_commands, cmd_name)

                latex_content <- c(
                    latex_content,
                    paste0("% ", scenario),
                    paste0("\\newcommand{\\sens", cmd_name, "att}{", format_number(sensitivity_data$att[i]), "}"),
                    paste0("\\newcommand{\\sens", cmd_name, "se}{", format_number(sensitivity_data$se[i]), "}"),
                    paste0("\\newcommand{\\sens", cmd_name, "lower}{", format_number(sensitivity_data$ci_lower[i]), "}"),
                    paste0("\\newcommand{\\sens", cmd_name, "upper}{", format_number(sensitivity_data$ci_upper[i]), "}"),
                    paste0("\\newcommand{\\sens", cmd_name, "n}{", sensitivity_data$n_treated[i], "}")
                )
            }
        }
    }
} else {
    # Se arquivo de sensibilidade não existir, criar macros de fallback com valores do modelo principal
    cat("Warning: Sensitivity analysis file not found. Using main model values as fallback.\n")
    if (exists("att_summary") && nrow(att_summary) > 0) {
        latex_content <- c(
            latex_content,
            "",
            "% Valores da análise de sensibilidade temporal (fallback do modelo principal)",
            "% NOTA: Estes valores são placeholder até a análise de sensibilidade ser executada",
            paste0("\\newcommand{\\sensfullatt}{", format_number(att_summary$att[1]), "}"),
            paste0("\\newcommand{\\sensfullse}{", format_number(att_summary$se[1]), "}"),
            paste0("\\newcommand{\\sensfulllower}{", format_number(att_summary$ci_low[1]), "}"),
            paste0("\\newcommand{\\sensfullupper}{", format_number(att_summary$ci_high[1]), "}"),
            paste0("\\newcommand{\\sensnostartatt}{", format_number(att_summary$att[1]), "}"),
            paste0("\\newcommand{\\sensnostartse}{", format_number(att_summary$se[1]), "}"),
            paste0("\\newcommand{\\sensnostartlower}{", format_number(att_summary$ci_low[1]), "}"),
            paste0("\\newcommand{\\sensnostartupper}{", format_number(att_summary$ci_high[1]), "}"),
            paste0("\\newcommand{\\sensnocovidatt}{", format_number(att_summary$att[1]), "}"),
            paste0("\\newcommand{\\sensnocovidse}{", format_number(att_summary$se[1]), "}"),
            paste0("\\newcommand{\\sensnocovidlower}{", format_number(att_summary$ci_low[1]), "}"),
            paste0("\\newcommand{\\sensnocovidupper}{", format_number(att_summary$ci_high[1]), "}")
        )
    }
}

# ============================================================================
# TABELA DE EFEITOS DiD DETALHADOS (Apendice D)
# Lendo diretamente dos arquivos RDS gerados pelo did_v2.r
# ============================================================================

# Helper function: number to word for LaTeX compatibility
num_to_word <- function(num_str) {
    num_str <- gsub("0", "zero", num_str)
    num_str <- gsub("1", "one", num_str)
    num_str <- gsub("2", "two", num_str)
    num_str <- gsub("3", "three", num_str)
    num_str <- gsub("4", "four", num_str)
    num_str <- gsub("5", "five", num_str)
    num_str <- gsub("6", "six", num_str)
    num_str <- gsub("7", "seven", num_str)
    num_str <- gsub("8", "eight", num_str)
    num_str <- gsub("9", "nine", num_str)
    return(num_str)
}

# Helper function: compute p-value from z-score
compute_pvalue <- function(att, se) {
    z <- att / se
    2 * pnorm(-abs(z))
}

# Helper function: compute significance stars
get_sig_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.01) return("***")
    if (p < 0.05) return("**")
    if (p < 0.10) return("*")
    return("")
}

generate_did_macros_from_rds <- function(att_rds_path, agg_overall_rds_path, agg_event_rds_path, prefix) {
    # Check if files exist
    if (!file.exists(agg_overall_rds_path) || !file.exists(agg_event_rds_path)) {
        cat(paste0("Aviso: RDS files para ", prefix, " nao encontrados\n"))
        return(character(0))
    }

    # Read RDS files
    agg_overall <- readRDS(agg_overall_rds_path)
    agg_event <- readRDS(agg_event_rds_path)

    macros <- c()

    # -------------------------------------------------------------------------
    # 1. EFEITO GLOBAL (Overall ATT)
    # -------------------------------------------------------------------------
    overall_att <- agg_overall$overall.att
    overall_se <- agg_overall$overall.se
    crit_val <- agg_overall$crit.val.egt
    if (is.null(crit_val)) crit_val <- 1.96
    overall_lower <- overall_att - crit_val * overall_se
    overall_upper <- overall_att + crit_val * overall_se
    overall_p <- compute_pvalue(overall_att, overall_se)
    overall_sig <- get_sig_stars(overall_p)

    macros <- c(macros,
        paste0("% Efeito Global - ", prefix),
        paste0("\\newcommand{\\", prefix, "globalatt}{", format_number(overall_att), "}"),
        paste0("\\newcommand{\\", prefix, "globalse}{", format_number(overall_se), "}"),
        paste0("\\newcommand{\\", prefix, "globallower}{", format_number(overall_lower), "}"),
        paste0("\\newcommand{\\", prefix, "globalupper}{", format_number(overall_upper), "}"),
        paste0("\\newcommand{\\", prefix, "globalp}{", format_number(overall_p), "}"),
        paste0("\\newcommand{\\", prefix, "globalsig}{", overall_sig, "}"),
        ""
    )

    # -------------------------------------------------------------------------
    # 2. EFEITOS POR COORTE (Group ATT)
    # Para isso precisamos agregar por grupo usando att_results
    # att_results e um objeto de classe MP (did package), nao um data.frame
    # -------------------------------------------------------------------------
    if (file.exists(att_rds_path)) {
        att_results <- readRDS(att_rds_path)

        # Get unique cohorts (excluding 0 = never-treated)
        cohorts <- unique(att_results$group)
        cohorts <- cohorts[cohorts != 0]
        cohorts <- sort(cohorts)

        # Limit to first 8 cohorts for letters a-h
        cohorts <- head(cohorts, 8)
        letters_map <- c("a", "b", "c", "d", "e", "f", "g", "h")

        macros <- c(macros, paste0("% Efeitos por Coorte - ", prefix))

        for (i in seq_along(cohorts)) {
            cohort <- cohorts[i]
            letter <- letters_map[i]

            # Filter indices for this cohort and post-treatment periods
            # att_results$group, $t, $att, $se are vectors of the same length
            idx <- which(att_results$group == cohort & att_results$t >= cohort)

            if (length(idx) > 0) {
                cohort_att_vals <- att_results$att[idx]
                cohort_se_vals <- att_results$se[idx]

                # Compute cohort ATT as simple mean
                cohort_att <- mean(cohort_att_vals, na.rm = TRUE)
                # Use pooled SE (approximate)
                cohort_se <- sqrt(mean(cohort_se_vals^2, na.rm = TRUE))
                cohort_lower <- cohort_att - crit_val * cohort_se
                cohort_upper <- cohort_att + crit_val * cohort_se
                cohort_p <- compute_pvalue(cohort_att, cohort_se)
                cohort_sig <- get_sig_stars(cohort_p)
            } else {
                cohort_att <- NA
                cohort_se <- NA
                cohort_lower <- NA
                cohort_upper <- NA
                cohort_p <- NA
                cohort_sig <- ""
            }

            macros <- c(macros,
                paste0("\\newcommand{\\", prefix, "coorte", letter, "att}{", format_number(cohort_att), "}"),
                paste0("\\newcommand{\\", prefix, "coorte", letter, "se}{", format_number(cohort_se), "}"),
                paste0("\\newcommand{\\", prefix, "coorte", letter, "lower}{", format_number(cohort_lower), "}"),
                paste0("\\newcommand{\\", prefix, "coorte", letter, "upper}{", format_number(cohort_upper), "}"),
                paste0("\\newcommand{\\", prefix, "coorte", letter, "p}{", format_number(cohort_p), "}"),
                paste0("\\newcommand{\\", prefix, "coorte", letter, "sig}{", cohort_sig, "}"),
                paste0("\\newcommand{\\", prefix, "coorte", letter, "year}{", cohort, "}")
            )
        }
        macros <- c(macros, "")
    }

    # -------------------------------------------------------------------------
    # 3. EFEITOS DINAMICOS (Event-time effects)
    # -------------------------------------------------------------------------
    event_times <- agg_event$egt
    att_egt <- agg_event$att.egt
    se_egt <- agg_event$se.egt
    crit_val_event <- agg_event$crit.val.egt
    if (is.null(crit_val_event)) crit_val_event <- 1.96

    # Specific event times expected by thesis: -5, -3, -1, 0, 1, 2, 3, 5, 10
    target_events <- c(-5, -3, -1, 0, 1, 2, 3, 5, 10)

    macros <- c(macros, paste0("% Efeitos Dinamicos - ", prefix))

    for (t_event in target_events) {
        idx <- which(event_times == t_event)

        if (length(idx) > 0) {
            att_t <- att_egt[idx]
            se_t <- se_egt[idx]
            lower_t <- att_t - crit_val_event * se_t
            upper_t <- att_t + crit_val_event * se_t
            p_t <- compute_pvalue(att_t, se_t)
            sig_t <- get_sig_stars(p_t)
        } else {
            att_t <- NA
            se_t <- NA
            lower_t <- NA
            upper_t <- NA
            p_t <- NA
            sig_t <- ""
        }

        # Convert event time to LaTeX-compatible name
        if (t_event < 0) {
            nome <- paste0("tminus", num_to_word(as.character(abs(t_event))))
        } else {
            nome <- paste0("tplus", num_to_word(as.character(t_event)))
        }

        macros <- c(macros,
            paste0("\\newcommand{\\", prefix, nome, "att}{", format_number(att_t), "}"),
            paste0("\\newcommand{\\", prefix, nome, "se}{", format_number(se_t), "}"),
            paste0("\\newcommand{\\", prefix, nome, "lower}{", format_number(lower_t), "}"),
            paste0("\\newcommand{\\", prefix, nome, "upper}{", format_number(upper_t), "}"),
            paste0("\\newcommand{\\", prefix, nome, "p}{", format_number(p_t), "}"),
            paste0("\\newcommand{\\", prefix, nome, "sig}{", sig_t, "}")
        )
    }
    macros <- c(macros, "")

    return(macros)
}

# Generate macros for Valor de Producao de Cana
valor_macros <- generate_did_macros_from_rds(
    att_rds_path = "data/outputs/att_results_dr_valor_producao_cana.rds",
    agg_overall_rds_path = "data/outputs/agg_overall_dr_valor_producao_cana.rds",
    agg_event_rds_path = "data/outputs/agg_event_dr_valor_producao_cana.rds",
    prefix = "didvalor"
)

# Generate macros for Area Plantada de Cana
area_macros <- generate_did_macros_from_rds(
    att_rds_path = "data/outputs/att_results_dr_area_cana.rds",
    agg_overall_rds_path = "data/outputs/agg_overall_dr_area_cana.rds",
    agg_event_rds_path = "data/outputs/agg_event_dr_area_cana.rds",
    prefix = "didarea"
)

if (length(valor_macros) > 0 || length(area_macros) > 0) {
    latex_content <- c(latex_content,
        "",
        "% ============================================================================",
        "% TABELA DE EFEITOS DiD DETALHADOS (Apendice D)",
        "% ============================================================================",
        ""
    )
    if (length(valor_macros) > 0) {
        latex_content <- c(latex_content, valor_macros)
    }
    if (length(area_macros) > 0) {
        latex_content <- c(latex_content, area_macros)
    }
}

# ============================================================================
# TABELA DE MICRORREGIOES TRATADAS POR ANO (Apendice B - Tabela 6)
# ============================================================================

# Ler dados principais para gerar tabela de tratamento temporal
data_path <- "data/csv/microrregions_Cana-de-açúcar-Soja-Arroz_2003-2021_mapbiomas.csv"
if (file.exists(data_path)) {
    df_main <- read_csv(data_path, show_col_types = FALSE)

    # Calcular microrregioes tratadas por ano
    treatment_by_year <- df_main %>%
        filter(primeiro_ano_tratamento > 0) %>%
        group_by(primeiro_ano_tratamento) %>%
        summarise(
            n_microregioes = n_distinct(id_microrregiao),
            .groups = "drop"
        ) %>%
        rename(ano = primeiro_ano_tratamento) %>%
        filter(ano >= 2003, ano <= 2021) %>%
        arrange(ano)

    # Calcular N Obs (microrregioes x anos no painel)
    n_anos <- n_distinct(df_main$ano)
    treatment_by_year <- treatment_by_year %>%
        mutate(n_obs = n_microregioes * n_anos)

    # Gerar macros LaTeX para cada linha da tabela
    latex_content <- c(latex_content,
        "",
        "% ============================================================================",
        "% TABELA DE MICRORREGIOES TRATADAS POR ANO (Apendice B - Tabela 6)",
        "% ============================================================================",
        ""
    )

    # Gerar macro para numero de anos no painel
    latex_content <- c(latex_content,
        paste0("\\newcommand{\\treatmentnanos}{", n_anos, "}")
    )

    # Gerar tabela LaTeX completa como macro unico
    # Isso evita problemas com numeros em nomes de comandos
    table_rows <- paste(
        sapply(1:nrow(treatment_by_year), function(i) {
            paste0(treatment_by_year$ano[i], " & ",
                   treatment_by_year$n_microregioes[i], " & ",
                   treatment_by_year$n_obs[i], " \\\\")
        }),
        collapse = "\n"
    )

    latex_content <- c(latex_content,
        "\\newcommand{\\treatmenttablerows}{%",
        table_rows,
        "}"
    )

    # Salvar CSV com dados da tabela
    write_csv(treatment_by_year, "data/outputs/treatment_by_year.csv")
    cat("Tabela de tratamento temporal salva em data/outputs/treatment_by_year.csv\n")
}

writeLines(latex_content, "data/outputs/latex_values.tex")
writeLines(latex_content, "documents/drafts/latex_output/auto_values.tex")

cat("Arquivo latex_values.tex gerado com sucesso!\n")
cat("Macros de efeitos DiD adicionados!\n")
