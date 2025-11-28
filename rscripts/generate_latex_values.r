#!/usr/bin/env Rscript
# Script para gerar comandos LaTeX com valores atualizados dos resultados

library(readr)
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
    paste0("% Última atualização: ", Sys.Date()),
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

# Escrever arquivo para ambos os locais (para compatibilidade)
# Local principal usado pelo LaTeX
writeLines(latex_content, "data/outputs/latex_values.tex", useBytes = TRUE)
# Local secundário (histórico)
writeLines(latex_content, "documents/drafts/latex_output/auto_values.tex", useBytes = TRUE)

cat("Arquivo latex_values.tex gerado com sucesso!\n")
cat("  - data/outputs/latex_values.tex (usado pelo LaTeX)\n")
cat("  - documents/drafts/latex_output/auto_values.tex (backup)\n")
