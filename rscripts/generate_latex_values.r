#!/usr/bin/env Rscript
# Script para gerar comandos LaTeX com valores atualizados dos resultados

library(readr)
library(glue)

# Função para formatar números para LaTeX
format_number <- function(x, digits = 4, percent = FALSE) {
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

# Ler dados principais (se existir)
main_results_path <- "data/outputs/main_results_table.csv"
if (file.exists(main_results_path)) {
    main_results <- read_csv(main_results_path, show_col_types = FALSE)
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
    paste0("\\newcommand{\\placebomean}{", format_number(mean(c(placebo_data$placebo_ci_95_lower[1], placebo_data$placebo_ci_95_upper[1]))), "}"),
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

# Se existirem resultados principais, adicionar também
if (exists("main_results") && nrow(main_results) > 0) {
    # Extrair valores da primeira linha (resultado principal)
    att_main <- as.numeric(gsub("[^0-9.-]", "", main_results$ATT[1]))
    se_main <- as.numeric(gsub("[^0-9.-]", "", gsub("\\(|\\)", "", main_results$`Erro Padrão`[1])))

    latex_content <- c(
        latex_content,
        "% Valores do modelo principal",
        paste0("\\newcommand{\\mainatt}{", format_number(att_main), "}"),
        paste0("\\newcommand{\\mainse}{", format_number(se_main), "}"),
        paste0("\\newcommand{\\mainattpct}{", format_number(att_main, percent = TRUE), "}"),
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

    # Criar comandos para cada cenário (exceto Período Central)
    scenario_names <- list(
        "Completo (2003-2023)" = "full",
        "Excluindo Início (2006-2023)" = "nostart",
        "Excluindo Final (2003-2019)" = "noend",
        "Excluindo COVID (2003-2019)" = "nocovid",
        "Pré-COVID (2003-2019)" = "precovid"
    )

    for (i in 1:nrow(sensitivity_data)) {
        scenario <- sensitivity_data$scenario[i]
        if (!grepl("Período Central", scenario)) {
            # Usar nome curto predefinido
            cmd_name <- ifelse(scenario %in% names(scenario_names),
                scenario_names[[scenario]],
                paste0("sens", i)
            )

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

    # Não adicionar linha vazia no final quando há dados de sensibilidade
}

# Escrever arquivo
writeLines(latex_content, "documents/drafts/latex_output/auto_values.tex", useBytes = TRUE)

cat("Arquivo auto_values.tex gerado com sucesso!\n")
cat("Adicione \\input{auto_values} no preâmbulo do seu documento LaTeX.\n")
