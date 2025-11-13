# Stage module: publishing legacy-facing artifacts
source(here::here("rscripts", "pipeline", "io_utils.R"))

format_significance <- function(p_value) {
  if (is.na(p_value)) {
    ""
  } else if (p_value < 0.01) {
    "***"
  } else if (p_value < 0.05) {
    "**"
  } else if (p_value < 0.1) {
    "*"
  } else {
    ""
  }
}

format_att_value <- function(att, p_value) {
  sprintf("%.3f%s", att, format_significance(p_value))
}

format_se_value <- function(se) {
  sprintf("(%.3f)", se)
}

format_ci_value <- function(lower, upper) {
  sprintf("[%.3f, %.3f]", lower, upper)
}

safe_integer <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    ""
  } else {
    format(round(x[1]), big.mark = ".", scientific = FALSE)
  }
}

resolve_placebo_summary <- function(pipeline_res) {
  if (!is.null(pipeline_res$placebo_random) && !is.null(pipeline_res$placebo_random$summary)) {
    return(pipeline_res$placebo_random$summary)
  }

  if (!is.null(pipeline_res$manifest)) {
    path <- pipeline_res$manifest %>%
      dplyr::filter(stage == "tests", name == "placebo_random_summary") %>%
      dplyr::arrange(dplyr::desc(timestamp)) %>%
      dplyr::pull(path) %>%
      purrr::compact() %>%
      purrr::pluck(1)

    if (!is.null(path) && file.exists(path)) {
      return(readr::read_csv(path, show_col_types = FALSE))
    }
  }

  NULL
}

publish_placebo_random <- function(summary_tbl, overwrite = TRUE) {
  if (is.null(summary_tbl) || nrow(summary_tbl) == 0) {
    cli::cli_alert_warning("Nenhum resumo de placebo aleatório disponível para publicação.")
    return(NA_character_)
  }

  published_tbl <- summary_tbl %>%
    dplyr::transmute(
      true_att = true_att,
      p_value_empirical = empirical_p_value,
      p_value_se = p_value_se,
      p_value_ci_lower = p_value_ci_low,
      p_value_ci_upper = p_value_ci_high,
      n_extremes = n_extremes,
      placebo_ci_95_lower = dist_q025,
      placebo_ci_95_upper = dist_q975,
      n_valid_sims = n_valid
    )

  write_published_csv(published_tbl, "placebo_random_summary.csv", overwrite = overwrite)
}

publish_main_results <- function(main_results, overwrite = TRUE) {
  if (is.null(main_results) || is.null(main_results$table) || nrow(main_results$table) == 0) {
    stop("main_results$table ausente para publicação.")
  }

  table <- main_results$table %>%
    dplyr::mutate(
      `Análise` = analysis,
      `ATT` = purrr::map2_chr(att, p_value, format_att_value),
      `Erro Padrão` = purrr::map_chr(se, format_se_value),
      `IC 95%` = purrr::map2_chr(ci_lower, ci_upper, format_ci_value),
      `N` = purrr::map_chr(n, safe_integer)
    ) %>%
    dplyr::select(`Análise`, `ATT`, `Erro Padrão`, `IC 95%`, `N`)

  write_published_csv(table, "main_results_table.csv", overwrite = overwrite)
}

publish_sensitivity <- function(sensitivity_res, overwrite = TRUE) {
  if (is.null(sensitivity_res) || is.null(sensitivity_res$results)) {
    cli::cli_alert_warning("Sensibilidade não disponível para publicação.")
    return(list(results = NA_character_, plot = NA_character_))
  }

  results_path <- write_published_csv(
    sensitivity_res$results,
    filename = "sensitivity_analysis_results.csv",
    subdir = "additional_figures",
    overwrite = overwrite
  )

  plot_path <- publish_copy(
    sensitivity_res$plot_path,
    subdir = "additional_figures",
    filename = "sensitivity_analysis_period.png",
    overwrite = overwrite
  )

  list(results = results_path, plot = plot_path)
}

publish_visuals <- function(pipeline_res, overwrite = TRUE) {
  outputs <- list()

  get_path <- function(item, field) {
    if (is.null(item)) {
      return(NULL)
    }
    item[[field]]
  }

  outputs$event_png <- publish_copy(
    get_path(pipeline_res$visuals$event_study, "png_path"),
    filename = "event_study.png",
    overwrite = overwrite
  )
  outputs$event_pdf <- publish_copy(
    get_path(pipeline_res$visuals$event_study, "pdf_path"),
    filename = "event_study.pdf",
    overwrite = overwrite
  )
  outputs$robustness_plot <- publish_copy(
    pipeline_res$robustness$plot_path,
    filename = "robustness_plot.png",
    overwrite = overwrite
  )
  outputs$weights_plot <- publish_copy(
    pipeline_res$weights$plot_path,
    filename = "weights_distribution.png",
    overwrite = overwrite
  )

  if (!is.null(pipeline_res$placebo_random) && !is.null(pipeline_res$placebo_random$plot_path)) {
    outputs$placebo_plot <- publish_copy(
      pipeline_res$placebo_random$plot_path,
      filename = "placebo_distribution.png",
      overwrite = overwrite
    )
  }

  outputs$parallel_trends_agro <- publish_copy(
    get_path(pipeline_res$visuals$parallel_trends_agro, "png_path"),
    filename = "parallel_trends_complete_pib_agro_normalized.png",
    overwrite = overwrite
  )
  outputs$parallel_trends_nonagro <- publish_copy(
    get_path(pipeline_res$visuals$parallel_trends_nonagro, "png_path"),
    filename = "parallel_trends_complete_pib_nao_agro_normalized.png",
    overwrite = overwrite
  )
  outputs$parallel_trends_by_gname <- publish_copy(
    get_path(pipeline_res$visuals$parallel_trends_by_gname, "png_path"),
    filename = "parallel_trends_by_gname_pib_agro.png",
    overwrite = overwrite
  )

  outputs
}

finalize_outputs <- function(pipeline_res, overwrite = TRUE) {
  if (is.null(pipeline_res$main_results)) {
    stop("Pipeline result set não contém main_results; finalize_outputs deve ser chamado após run_pipeline.")
  }

  published <- list()

  published$main_results <- publish_main_results(pipeline_res$main_results, overwrite = overwrite)

  placebo_summary <- resolve_placebo_summary(pipeline_res)
  published$placebo_random <- publish_placebo_random(placebo_summary, overwrite = overwrite)

  published$sensitivity <- publish_sensitivity(pipeline_res$sensitivity, overwrite = overwrite)

  published$weights_analysis <- publish_copy(
    pipeline_res$weights$csv_path,
    filename = "weights_analysis.csv",
    overwrite = overwrite
  )

  published$robustness <- publish_copy(
    pipeline_res$robustness$results_path,
    filename = "robustness_analysis.csv",
    overwrite = overwrite
  )

  published$visuals <- publish_visuals(pipeline_res, overwrite = overwrite)

  published
}
