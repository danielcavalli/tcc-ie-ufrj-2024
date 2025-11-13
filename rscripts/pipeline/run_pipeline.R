source(here::here("rscripts", "pipeline", "prep_stage.R"))
source(here::here("rscripts", "pipeline", "estimation_stage.R"))
source(here::here("rscripts", "pipeline", "tests_stage.R"))
source(here::here("rscripts", "pipeline", "visuals_stage.R"))
source(here::here("rscripts", "pipeline", "heterogeneity_stage.R"))
source(here::here("rscripts", "pipeline", "reporting_stage.R"))
source(here::here("rscripts", "pipeline", "publish_stage.R"))

add_manifest_entry <- function(entries, stage, name, path) {
  if (is.null(path) || all(is.na(path))) {
    return(entries)
  }
  if (length(path) > 1) {
    for (i in seq_along(path)) {
      entries <- add_manifest_entry(entries, stage, paste0(name, "_", i), path[[i]])
    }
    return(entries)
  }
  entries[[length(entries) + 1]] <- list(
    stage = stage,
    name = name,
    path = path,
    timestamp = as.character(Sys.time())
  )
  entries
}

run_pipeline <- function(config = list()) {
  default_config <- list(
    data_path = here::here("data", "csv", "dataset_mapbiomas_municipio_culturas_irrigadas_2003-2021_20251008.csv"),
    label = NULL,
    placebo_year = 2015,
    random_placebo = list(enabled = TRUE, n_sims = 1000, parallel = TRUE, n_cores = NULL),
    run_visuals = TRUE,
    publish = TRUE,
    overwrite_published = TRUE
  )

  config <- utils::modifyList(default_config, config)
  manifest_entries <- list()

  cli::cli_h1("Pipeline DiD - Execução Orquestrada")

  # Stage 1: Preparation ----------------------------------------------------
  prep_res <- prep_data(config$data_path, label = config$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "prep", "microrregiao", prep_res$microrregiao_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "prep", "municipio", prep_res$municipio_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "prep", "summary", prep_res$summary_path)

  # Stage 2: Estimation -----------------------------------------------------
  att_res <- estimate_att(prep_res$microrregiao_path, method = "dr", label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "att_results", att_res$att_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "agg_overall", att_res$agg_overall_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "agg_event", att_res$agg_event_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "att_summary", att_res$summary_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "covariates", att_res$covariate_artifact)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "covariate_diagnostics", att_res$diagnostics_artifact)

  weights_res <- analyze_weights(att_res)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "weights_analysis", weights_res$csv_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "weights_distribution", weights_res$plot_path)

  robust_specs_res <- robust_specs(prep_res$microrregiao_path, label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "robust_specs", robust_specs_res$artifact_path)

  robustness_res <- robustness_analysis(prep_res$microrregiao_path, label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "estimation", "robustness_results", robustness_res$results_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "robustness_plot", robustness_res$plot_path)

  # Stage 3: Tests ----------------------------------------------------------
  placebo_fixed <- placebo_test(prep_res$microrregiao_path, placebo_year = config$placebo_year, label = prep_res$label)
  if (!is.null(placebo_fixed)) {
    manifest_entries <- add_manifest_entry(manifest_entries, "tests", "placebo_fixed", placebo_fixed$artifact_path)
  }

  placebo_non_agro <- placebo_test_non_agro(prep_res$microrregiao_path, label = prep_res$label)
  if (!is.null(placebo_non_agro)) {
    manifest_entries <- add_manifest_entry(manifest_entries, "tests", "placebo_non_agro", placebo_non_agro$artifact_path)
  }

  balance_res <- check_balance_post_dr(att_res, prep_res$microrregiao_path, label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "tests", "balance_stats", balance_res$artifact_path)

  control_res <- compare_control_groups(prep_res$microrregiao_path, label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "tests", "control_groups", control_res$artifact_path)

  na_diag <- diagnose_na_cohorts(att_res, prep_res$microrregiao_path, label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "tests", "na_cases", na_diag$na_artifact_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "tests", "cohort_sizes", na_diag$cohort_artifact_path)

  placebo_rand <- NULL
  if (isTRUE(config$random_placebo$enabled)) {
    placebo_rand <- random_placebo_test(
      prep_res$microrregiao_path,
      n_sims = config$random_placebo$n_sims,
      method = "dr",
      seed = 42,
      parallel = config$random_placebo$parallel,
      n_cores = config$random_placebo$n_cores,
      label = prep_res$label
    )
    manifest_entries <- add_manifest_entry(manifest_entries, "tests", "placebo_random_summary", placebo_rand$summary_path)
    manifest_entries <- add_manifest_entry(manifest_entries, "tests", "placebo_random_distribution", placebo_rand$distribution_path)
    manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "placebo_random_plot", placebo_rand$plot_path)
  }

  # Stage 4: Visuals --------------------------------------------------------
  visuals_res <- list()
  if (isTRUE(config$run_visuals)) {
    event_plot <- visualize_results(att_res, label = prep_res$label)
    manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "event_study_png", event_plot$png_path)
    manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "event_study_pdf", event_plot$pdf_path)
    visuals_res$event_study <- event_plot

    pt_agro <- plot_parallel_trends(prep_res$microrregiao_path, outcome = "log_pib_agro", normalize = TRUE, label = prep_res$label)
    manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "parallel_trends_agro", pt_agro$png_path)
    visuals_res$parallel_trends_agro <- pt_agro

    pt_nonagro <- plot_parallel_trends(prep_res$microrregiao_path, outcome = "log_pib_nao_agro", normalize = TRUE, label = prep_res$label)
    manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "parallel_trends_nonagro", pt_nonagro$png_path)
    visuals_res$parallel_trends_nonagro <- pt_nonagro

    gname_plot <- plot_parallel_trends_by_gname(prep_res$microrregiao_path, outcome = "log_pib_agro", label = prep_res$label)
    manifest_entries <- add_manifest_entry(manifest_entries, "visuals", "parallel_trends_by_gname_agro", gname_plot$png_path)
    visuals_res$parallel_trends_by_gname <- gname_plot
  }

  # Stage 5: Additional Analyses --------------------------------------------
  df_micro <- prep_res$df_microrregiao
  df_mun <- prep_res$df_municipio

  crop_res <- crop_heterogeneity_analysis(df_mun, method = "dr", label = prep_res$label)

  manifest_entries <- add_manifest_entry(manifest_entries, "heterogeneity", "crop_results", crop_res$artifact_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "heterogeneity", "crop_plot", crop_res$plot_path)

  # Stage 6: Reporting -------------------------------------------------------
  main_results <- prepare_main_results_table(
    att_res,
    placebo_fixed = placebo_fixed,
    placebo_non_agro = placebo_non_agro,
    control_groups = control_res,
    robustness_res = robustness_res,
    label = prep_res$label
  )
  manifest_entries <- add_manifest_entry(manifest_entries, "reporting", "main_results_table", main_results$artifact_path)

  sensitivity_res <- sensitivity_analysis_stage(df_micro, label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "reporting", "sensitivity_results", sensitivity_res$artifact_path)
  manifest_entries <- add_manifest_entry(manifest_entries, "reporting", "sensitivity_plot", sensitivity_res$plot_path)

  summary_cards <- generate_summary_cards(att_res, weights_res, heterogeneity_res, label = prep_res$label)
  manifest_entries <- add_manifest_entry(manifest_entries, "reporting", "summary_cards", summary_cards$artifact_path)

  # Stage 7: Manifest -------------------------------------------------------
  manifest_tbl <- purrr::map_dfr(manifest_entries, tibble::as_tibble)
  manifest_path <- write_artifact(
    manifest_tbl,
    "diagnostics",
    sprintf("%s_pipeline_manifest.csv", prep_res$label),
    overwrite = TRUE
  )

  cli::cli_alert_success("Pipeline finalizado. Manifesto salvo em {manifest_path}")

  pipeline_result <- list(
    label = prep_res$label,
    manifest_path = manifest_path,
    manifest = manifest_tbl,
    prep = prep_res,
    att = att_res,
    weights = weights_res,
    robustness = robustness_res,
    placebo_fixed = placebo_fixed,
    placebo_non_agro = placebo_non_agro,
    placebo_random = placebo_rand,
    balance = balance_res,
    control_groups = control_res,
    heterogeneity = NULL,
    crop = crop_res,
    uf = NULL,
    main_results = main_results,
    sensitivity = sensitivity_res,
    summary_cards = summary_cards,
    visuals = visuals_res
  )

  if (isTRUE(config$publish)) {
    pipeline_result$published <- finalize_outputs(
      pipeline_result,
      overwrite = isTRUE(config$overwrite_published)
    )
  } else {
    pipeline_result$published <- NULL
  }

  pipeline_result
}

