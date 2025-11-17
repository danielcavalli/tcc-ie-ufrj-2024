#!/usr/bin/env Rscript
# ==============================================================================
# GENERATE LATEX VALUES - FIXED VERSION
# ==============================================================================
# Purpose: Generate LaTeX macros for thesis document with correct values
# Author: Daniel Cavalli
# Date: 2025-01-16
# ==============================================================================

library(tidyverse)
library(here)

# Set working directory
setwd(here::here())

# Function to format numbers
format_number <- function(x, digits = 3) {
  if (is.null(x) || is.na(x) || length(x) == 0) return("")
  if (abs(x) < 0.001) {
    return("0.000")
  }
  return(formatC(x, format = "f", digits = digits))
}

# Function to format percentage
format_pct <- function(x, digits = 1) {
  if (is.na(x)) return("")
  return(paste0(formatC(x * 100, format = "f", digits = digits), "\\%"))
}

# Read data files
cat("Reading data files...\n")

# Main results (valor de produção)
main_valor <- read_csv("data/outputs/att_summary_main_valor_cana.csv", show_col_types = FALSE)

# Alternative outcomes
alt_outcomes <- read_csv("data/outputs/alternative_outcomes_analysis.csv", show_col_types = FALSE)

# Main results table (for robustness checks)
main_table <- read_csv("data/outputs/main_results_table.csv", show_col_types = FALSE)

# Initialize LaTeX content
latex_content <- c(
  "% ==============================================================================",
  "% VALORES LATEX PARA TESE - GERADO AUTOMATICAMENTE",
  paste0("% Data: ", Sys.Date()),
  "% ==============================================================================",
  "",
  "% ------------------------------------------------------------------------------",
  "% RESULTADO PRINCIPAL: VALOR DE PRODUÇÃO CANA",
  "% ------------------------------------------------------------------------------"
)

# Main ATT (Valor de Produção)
if (nrow(main_valor) > 0) {
  latex_content <- c(
    latex_content,
    paste0("\\newcommand{\\mainatt}{", format_number(main_valor$att[1])),"}",
    paste0("\\newcommand{\\mainse}{", format_number(main_valor$se[1])),"}",
    paste0("\\newcommand{\\mainz}{", format_number(main_valor$z[1])),"}",
    paste0("\\newcommand{\\mainp}{", format_number(main_valor$p[1])),"}",
    paste0("\\newcommand{\\maincilower}{", format_number(main_valor$ci_low[1])),"}",
    paste0("\\newcommand{\\mainciupper}{", format_number(main_valor$ci_high[1])),"}",
    paste0("\\newcommand{\\mainattpct}{", format_pct(main_valor$att[1])),"}",
    ""
  )
}

# Area Cana (Secondary outcome for decomposition)
area_cana <- alt_outcomes %>% filter(str_detect(outcome_label, "Área Cana"))
if (nrow(area_cana) > 0) {
  latex_content <- c(
    latex_content,
    "% ------------------------------------------------------------------------------",
    "% OUTCOME SECUNDÁRIO: ÁREA PLANTADA CANA (PARA DECOMPOSIÇÃO)",
    "% ------------------------------------------------------------------------------",
    paste0("\\newcommand{\\areacanaatt}{", format_number(area_cana$att[1])),"}",
    paste0("\\newcommand{\\areacanase}{", format_number(area_cana$se[1])),"}",
    paste0("\\newcommand{\\areacanap}{", format_number(area_cana$p_value[1])),"}",
    paste0("\\newcommand{\\areacanalower}{", format_number(area_cana$ci_lower[1])),"}",
    paste0("\\newcommand{\\areacanaupper}{", format_number(area_cana$ci_upper[1])),"}",
    paste0("\\newcommand{\\areacanaattpct}{", format_pct(area_cana$att[1])),"}",
    ""
  )
}

# Alternative Outcomes - Valor
valor_soja <- alt_outcomes %>% filter(str_detect(outcome_label, "Valor.*Soja"))
if (nrow(valor_soja) > 0) {
  latex_content <- c(
    latex_content,
    "% Valor Soja",
    paste0("\\newcommand{\\valorsojaat}{", format_number(valor_soja$att[1])),"}",
    paste0("\\newcommand{\\valorsojase}{", format_number(valor_soja$se[1])),"}",
    paste0("\\newcommand{\\valorsojap}{", format_number(valor_soja$p_value[1])),"}",
    paste0("\\newcommand{\\valorsojlower}{", format_number(valor_soja$ci_lower[1])),"}",
    paste0("\\newcommand{\\valorsojaupper}{", format_number(valor_soja$ci_upper[1])),"}",
    ""
  )
}

valor_arroz <- alt_outcomes %>% filter(str_detect(outcome_label, "Valor.*Arroz"))
if (nrow(valor_arroz) > 0) {
  latex_content <- c(
    latex_content,
    "% Valor Arroz",
    paste0("\\newcommand{\\valorarrozatt}{", format_number(valor_arroz$att[1])),"}",
    paste0("\\newcommand{\\valorarrozse}{", format_number(valor_arroz$se[1])),"}",
    paste0("\\newcommand{\\valorarrozp}{", format_number(valor_arroz$p_value[1])),"}",
    paste0("\\newcommand{\\valorarrozlower}{", format_number(valor_arroz$ci_lower[1])),"}",
    paste0("\\newcommand{\\valorarrozupper}{", format_number(valor_arroz$ci_upper[1])),"}",
    ""
  )
}

# Alternative Outcomes - Área
area_soja <- alt_outcomes %>% filter(str_detect(outcome_label, "Área Soja"))
if (nrow(area_soja) > 0) {
  latex_content <- c(
    latex_content,
    "% Área Soja",
    paste0("\\newcommand{\\areasojaat}{", format_number(area_soja$att[1])),"}",
    paste0("\\newcommand{\\areasojase}{", format_number(area_soja$se[1])),"}",
    paste0("\\newcommand{\\areasojap}{", format_number(area_soja$p_value[1])),"}",
    paste0("\\newcommand{\\areasojlower}{", format_number(area_soja$ci_lower[1])),"}",
    paste0("\\newcommand{\\areasojaupper}{", format_number(area_soja$ci_upper[1])),"}",
    ""
  )
}

area_arroz <- alt_outcomes %>% filter(str_detect(outcome_label, "Área Arroz"))
if (nrow(area_arroz) > 0) {
  latex_content <- c(
    latex_content,
    "% Área Arroz",
    paste0("\\newcommand{\\areaarrozatt}{", format_number(area_arroz$att[1])),"}",
    paste0("\\newcommand{\\areaarrozse}{", format_number(area_arroz$se[1])),"}",
    paste0("\\newcommand{\\areaarrozp}{", format_number(area_arroz$p_value[1])),"}",
    paste0("\\newcommand{\\areaarrozlower}{", format_number(area_arroz$ci_lower[1])),"}",
    paste0("\\newcommand{\\areaarrozupper}{", format_number(area_arroz$ci_upper[1])),"}",
    ""
  )
}

# Robustness checks from main_results_table
latex_content <- c(
  latex_content,
  "% ------------------------------------------------------------------------------",
  "% ROBUSTNESS CHECKS",
  "% ------------------------------------------------------------------------------"
)

# Parse main_results_table for robustness values
for (i in 1:nrow(main_table)) {
  row <- main_table[i,]
  analise <- row$Análise

  # Extract numeric values from formatted strings
  att_str <- gsub("\\*", "", row$ATT)
  att_val <- as.numeric(att_str)

  se_str <- gsub("[()]", "", row$`Erro Padrão`)
  se_val <- as.numeric(se_str)

  # Parse confidence interval - handle both formats
  ci_str <- row$`IC 95%`
  ci_str <- gsub("\\[|\\]", "", ci_str)  # Remove brackets
  ci_parts <- strsplit(ci_str, ",")[[1]]

  if (length(ci_parts) == 2) {
    ci_lower <- as.numeric(trimws(ci_parts[1]))
    ci_upper <- as.numeric(trimws(ci_parts[2]))
  } else {
    ci_lower <- NA
    ci_upper <- NA
  }

  if (grepl("Nevertreated", analise, ignore.case = TRUE)) {
    latex_content <- c(
      latex_content,
      "% Never-treated control group",
      paste0("\\newcommand{\\nevertreatedatt}{", format_number(att_val)),"}",
      paste0("\\newcommand{\\nevertreatedse}{", format_number(se_val)),"}",
      paste0("\\newcommand{\\nevertreatedlower}{", format_number(ci_lower)),"}",
      paste0("\\newcommand{\\nevertreatedupper}{", format_number(ci_upper)),"}",
      ""
    )
  } else if (grepl("Sem Covariáveis", analise, ignore.case = TRUE)) {
    latex_content <- c(
      latex_content,
      "% No covariates",
      paste0("\\newcommand{\\nocovatt}{", format_number(att_val)),"}",
      paste0("\\newcommand{\\nocovse}{", format_number(se_val)),"}",
      paste0("\\newcommand{\\nocovlower}{", format_number(ci_lower)),"}",
      paste0("\\newcommand{\\nocover}{", format_number(ci_upper)),"}",
      ""
    )
  } else if (grepl("IPW", analise, ignore.case = TRUE)) {
    latex_content <- c(
      latex_content,
      "% IPW",
      paste0("\\newcommand{\\ipwatt}{", format_number(att_val)),"}",
      paste0("\\newcommand{\\ipwse}{", format_number(se_val)),"}",
      paste0("\\newcommand{\\ipwlower}{", format_number(ci_lower)),"}",
      paste0("\\newcommand{\\ipwupper}{", format_number(ci_upper)),"}",
      ""
    )
  } else if (grepl("REG", analise, ignore.case = TRUE)) {
    latex_content <- c(
      latex_content,
      "% Regression",
      paste0("\\newcommand{\\regatt}{", format_number(att_val)),"}",
      paste0("\\newcommand{\\regse}{", format_number(se_val)),"}",
      paste0("\\newcommand{\\reglower}{", format_number(ci_lower)),"}",
      paste0("\\newcommand{\\regupper}{", format_number(ci_upper)),"}",
      ""
    )
  }
}

# Write to file
output_file <- "data/outputs/latex_values.tex"
writeLines(latex_content, output_file)

cat(paste0("\n✓ LaTeX values written to: ", output_file, "\n"))
cat("\nSummary of generated values:\n")
cat(paste0("  Main ATT (Valor): ", format_number(main_valor$att[1]), " (",
           format_pct(main_valor$att[1]), ")\n"))
cat(paste0("  Area Cana ATT: ", format_number(area_cana$att[1]), " (",
           format_pct(area_cana$att[1]), ")\n"))
cat("\nTo use these values, add the following line to your LaTeX preamble:\n")
cat(paste0("  \\input{", output_file, "}\n\n"))