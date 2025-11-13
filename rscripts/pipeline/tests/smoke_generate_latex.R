#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(here)
  library(cli)
})

required_files <- c(
  here("data", "outputs", "main_results_table.csv"),
  here("data", "outputs", "placebo_random_summary.csv"),
  here("data", "outputs", "additional_figures", "sensitivity_analysis_results.csv")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  cli::cli_abort(c(
    "LaTeX smoke test exige que o pipeline tenha sido publicado.",
    "Arquivos ausentes:" = paste(missing_files, collapse = "\n")
  ))
}

cli::cli_alert_info("Executando generate_latex_values.r…")

rscript_bin <- file.path(R.home("bin"), "Rscript")
generate_script <- here("rscripts", "generate_latex_values.r")

status <- system2(rscript_bin, generate_script)

if (status != 0) {
  cli::cli_abort("Falha ao executar generate_latex_values.r")
}

output_file <- here("documents", "drafts", "latex_output", "auto_values.tex")

if (!file.exists(output_file)) {
  cli::cli_abort("generate_latex_values.r executou, mas auto_values.tex não foi encontrado.")
}

cli::cli_alert_success("Smoke test concluído com sucesso. auto_values.tex atualizado em {output_file}")
