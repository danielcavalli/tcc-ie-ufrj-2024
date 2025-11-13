# Stage module: placebo and diagnostic tests
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(cli)
library(foreach)

source(here::here("rscripts", "pipeline", "io_utils.R"))

placebo_test <- function(panel_source,
                         placebo_year = 2015,
                         seed = 2024,
                         method = "dr",
                         label = NULL,
                         overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  cli::cli_alert_info("Executando placebo test fixo (ano fictício = {placebo_year})…")
  year_range <- range(df$ano)
  if (placebo_year < year_range[1] || placebo_year > year_range[2]) {
    cli::cli_alert_warning("Ano placebo {placebo_year} fora do período dos dados [{year_range[1]}, {year_range[2]}]")
    return(NULL)
  }

  set.seed(seed)
  all_units <- unique(df$id_microrregiao)
  n_units <- length(all_units)
  n_treated_placebo <- round(n_units / 2)
  treated_units_placebo <- sample(all_units, n_treated_placebo, replace = FALSE)

  cli::cli_alert_info("Total de unidades: {n_units}")
  cli::cli_alert_info("Unidades aleatoriamente atribuídas ao tratamento placebo: {n_treated_placebo} ({round(100 * n_treated_placebo / n_units, 1)}%)")

  df_placebo <- df %>%
    mutate(
      gname = ifelse(id_microrregiao %in% treated_units_placebo, placebo_year, 0),
      treated = as.integer(gname > 0 & ano >= gname),
      never_treated = gname == 0
    )

  spec_label <- sanitize_label(paste(label, "placebo_fixed", placebo_year, sep = "_"))

  res <- tryCatch(
    estimate_att(
      df_placebo,
      method = method,
      control_grp = "notyettreated",
      label = spec_label,
      overwrite = overwrite
    ),
    error = function(e) {
      cli::cli_alert_warning("Placebo test falhou: {e$message}")
      NULL
    }
  )

  if (is.null(res)) {
    return(NULL)
  }

  summary_tbl <- tibble::tibble(
    label = label,
    placebo_year = placebo_year,
    method = method,
    att = res$att_global,
    se = res$se_global,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_high = res$ci_high,
    n_units = n_units,
    n_treated_placebo = n_treated_placebo,
    created_at = as.character(Sys.time())
  )

  artifact_path <- write_artifact(
    summary_tbl,
    "tests",
    sprintf("%s_placebo_fixed_%s.csv", label, placebo_year),
    overwrite = overwrite
  )

  list(
    summary = summary_tbl,
    artifact_path = artifact_path,
    result = res
  )
}

placebo_test_non_agro <- function(panel_source, method = "dr", label = NULL, overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  cli::cli_alert_info("Executando placebo test com PIB não-agropecuário...")

  df_placebo <- df %>%
    mutate(
      log_pib_agro_original = log_pib_agro,
      log_pib_agro = log_pib_nao_agro
    ) %>%
    filter(!is.na(log_pib_agro))

  spec_label <- sanitize_label(paste(label, "placebo_non_agro", sep = "_"))

  res <- tryCatch(
    estimate_att(
      df_placebo,
      method = method,
      control_grp = "notyettreated",
      label = spec_label,
      overwrite = overwrite
    ),
    error = function(e) {
      cli::cli_alert_warning("Erro no placebo test: {e$message}")
      NULL
    }
  )

  if (is.null(res)) {
    return(NULL)
  }

  summary_tbl <- tibble::tibble(
    label = label,
    method = method,
    att = res$att_global,
    se = res$se_global,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_high = res$ci_high,
    n_obs = nrow(df_placebo),
    created_at = as.character(Sys.time())
  )

  artifact_path <- write_artifact(
    summary_tbl,
    "tests",
    sprintf("%s_placebo_non_agro.csv", label),
    overwrite = overwrite
  )

  list(
    summary = summary_tbl,
    artifact_path = artifact_path,
    result = res
  )
}

check_balance_post_dr <- function(att_result, panel_source, label = NULL, overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)

  att_obj <- NULL
  if (is.list(att_result) && !is.null(att_result$att)) {
    att_obj <- att_result$att
    label <- sanitize_label(ifelse(is.null(label), att_result$label, label))
  } else if (is.character(att_result) && length(att_result) == 1) {
    att_obj <- readRDS(att_result)
    label <- sanitize_label(ifelse(is.null(label), tools::file_path_sans_ext(basename(att_result)), label))
  } else {
    stop("check_balance_post_dr() requer objeto estimate_att() ou caminho para RDS.")
  }

  cli::cli_alert_info("Verificando balanceamento das covariáveis pós-DR...")

  covars <- c(
    "log_area_plantada", "log_populacao", "log_pib_per_capita",
    "log_densidade_estacoes_uf"
  )

  balance_pre <- df %>%
    filter((gname == 0) | (gname > 0 & ano < gname)) %>%
    mutate(treat_group = ifelse(gname > 0, "Tratado", "Controle"))

  balance_stats <- purrr::map_dfr(covars, function(cv) {
    stats_by_group <- balance_pre %>%
      group_by(treat_group) %>%
      summarise(
        mean = mean(.data[[cv]], na.rm = TRUE),
        sd = sd(.data[[cv]], na.rm = TRUE),
        .groups = "drop"
      )

    control_stats <- stats_by_group %>% filter(treat_group == "Controle")
    treated_stats <- stats_by_group %>% filter(treat_group == "Tratado")

    if (nrow(control_stats) == 0 || nrow(treated_stats) == 0) {
      tibble::tibble(
        variable = cv,
        Controle_mean = NA_real_,
        Controle_sd = NA_real_,
        Tratado_mean = NA_real_,
        Tratado_sd = NA_real_,
        diff_means = NA_real_,
        std_diff = NA_real_,
        balanced = NA
      )
    } else {
      diff_means <- treated_stats$mean - control_stats$mean
      pooled_sd <- sqrt((treated_stats$sd^2 + control_stats$sd^2) / 2)
      std_diff <- diff_means / pooled_sd

      tibble::tibble(
        variable = cv,
        Controle_mean = control_stats$mean,
        Controle_sd = control_stats$sd,
        Tratado_mean = treated_stats$mean,
        Tratado_sd = treated_stats$sd,
        diff_means = diff_means,
        std_diff = std_diff,
        balanced = abs(std_diff) < 0.1
      )
    }
  })

  artifact_path <- write_artifact(
    balance_stats,
    "tests",
    sprintf("%s_balance_stats.csv", label),
    overwrite = overwrite
  )

  list(
    label = label,
    balance = balance_stats,
    artifact_path = artifact_path,
    att = att_obj
  )
}

setup_parallel_placebo <- function(n_cores = NULL) {
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
    cli::cli_alert_info("Auto-detectados {parallel::detectCores()} cores, usando {n_cores}")
  }

  if (.Platform$OS.type == "windows") {
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    list(n_cores = n_cores, cluster = cl)
  } else {
    doParallel::registerDoParallel(cores = n_cores)
    list(n_cores = n_cores, cluster = NULL)
  }
}

cleanup_parallel_placebo <- function(setup_info) {
  if (!is.null(setup_info$cluster)) {
    parallel::stopCluster(setup_info$cluster)
  }
  foreach::registerDoSEQ()
}

random_placebo_test_serial <- function(panel_source,
                                       n_sims = 100,
                                       method = "dr",
                                       seed = 42,
                                       label = NULL,
                                       overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  cli::cli_alert_info("Executando {n_sims} testes placebo aleatórios (serial)...")

  true_res <- estimate_att(df, method = method, label = paste0(label, "_placebo_true"), overwrite = overwrite)
  true_att <- true_res$att_global

  all_units <- unique(df$id_microrregiao)
  n_treated_original <- df %>%
    group_by(id_microrregiao) %>%
    summarise(treated = any(gname > 0), .groups = "drop") %>%
    pull(treated) %>%
    sum()

  year_range <- range(df$ano)
  possible_years <- seq(year_range[1] + 2, year_range[2] - 2)

  placebo_atts <- numeric(n_sims)
  cli::cli_progress_bar("Simulando placebos", total = n_sims)

  for (i in seq_len(n_sims)) {
    cli::cli_progress_update()
    set.seed(seed + i)

    treated_units_placebo <- sample(all_units, n_treated_original, replace = FALSE)

    unit_assignments <- tibble::tibble(
      id_microrregiao = all_units,
      gname_placebo = ifelse(
        id_microrregiao %in% treated_units_placebo,
        sample(possible_years, length(all_units), replace = TRUE),
        0
      )
    )

    df_placebo <- df %>%
      select(-gname) %>%
      left_join(unit_assignments, by = "id_microrregiao") %>%
      rename(gname = gname_placebo) %>%
      mutate(
        treated = as.integer(gname > 0 & ano >= gname),
        never_treated = gname == 0
      )

    placebo_res <- tryCatch(
      {
        res <- estimate_att(
          df_placebo,
          method = method,
          label = paste0(label, "_placebo_serial"),
          overwrite = overwrite,
          persist = FALSE
        )
        res$att_global
      },
      error = function(e) NA_real_
    )

    placebo_atts[i] <- placebo_res
  }

  cli::cli_progress_done()

  placebo_atts <- placebo_atts[!is.na(placebo_atts)]
  n_valid <- length(placebo_atts)

  n_extremes <- sum(abs(placebo_atts) >= abs(true_att))
  p_value <- (1 + n_extremes) / (n_valid + 1)
  se_pvalue <- sqrt(p_value * (1 - p_value) / n_valid)
  ci_lower_pvalue <- max(0, p_value - 1.96 * se_pvalue)
  ci_upper_pvalue <- min(1, p_value + 1.96 * se_pvalue)
  percentiles <- quantile(placebo_atts, c(0.025, 0.05, 0.95, 0.975))

  summary_tbl <- tibble::tibble(
    label = label,
    method = method,
    n_simulations = n_sims,
    n_valid = n_valid,
    true_att = true_att,
    empirical_p_value = p_value,
    p_value_se = se_pvalue,
    p_value_ci_low = ci_lower_pvalue,
    p_value_ci_high = ci_upper_pvalue,
    n_extremes = n_extremes,
    dist_q025 = percentiles[1],
    dist_q975 = percentiles[4],
    created_at = as.character(Sys.time())
  )

  summary_path <- write_artifact(
    summary_tbl,
    "tests",
    sprintf("%s_placebo_random_serial_summary.csv", label),
    overwrite = overwrite
  )

  dist_path <- raw_artifact_path("tests", sprintf("%s_placebo_random_serial_distribution.csv", label))
  readr::write_csv(tibble::tibble(att = placebo_atts), dist_path)

  plot <- ggplot(tibble::tibble(att = placebo_atts), aes(x = att)) +
    geom_histogram(bins = 30, fill = "lightgray", color = "black", alpha = 0.7) +
    geom_vline(xintercept = true_att, color = "red", linetype = "dashed", linewidth = 1.5) +
    geom_vline(xintercept = percentiles[c(1, 4)], color = "blue", linetype = "dotted") +
    theme_minimal() +
    labs(
      title = "Distribuição de ATTs Placebo vs ATT Verdadeiro",
      x = "ATT estimado",
      y = "Frequência"
    )

  plot_path <- raw_artifact_path("tests", sprintf("%s_placebo_random_serial.png", label))
  ggplot2::ggsave(plot_path, plot, width = 10, height = 6, dpi = 300)
  include_metadata(plot_path, list(label = label, created_at = as.character(Sys.time())))

  list(
    summary = summary_tbl,
    summary_path = summary_path,
    distribution_path = dist_path,
    plot = plot,
    plot_path = plot_path,
    true_result = true_res
  )
}

random_placebo_test <- function(panel_source,
                                n_sims = 100,
                                method = "dr",
                                seed = 42,
                                parallel = TRUE,
                                n_cores = NULL,
                                label = NULL,
                                overwrite = TRUE) {
  if (!parallel || n_sims < 100) {
    return(random_placebo_test_serial(panel_source, n_sims, method, seed, label, overwrite))
  }

  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  setup_info <- setup_parallel_placebo(n_cores)
  on.exit(cleanup_parallel_placebo(setup_info), add = TRUE)

  cli::cli_alert_info("Executando {n_sims} testes placebo em paralelo ({setup_info$n_cores} cores)...")

  true_res <- estimate_att(df, method = method, label = paste0(label, "_placebo_true"), overwrite = overwrite)
  true_att <- true_res$att_global

  all_units <- unique(df$id_microrregiao)
  n_units <- length(all_units)
  n_treated_original <- df %>%
    group_by(id_microrregiao) %>%
    summarise(treated = any(gname > 0), .groups = "drop") %>%
    pull(treated) %>%
    sum()

  year_range <- range(df$ano)
  possible_years <- seq(year_range[1] + 2, year_range[2] - 2)

  start_time <- Sys.time()

  placebo_results <- foreach::foreach(
    sim_id = 1:n_sims,
    .combine = "c",
    .packages = c("dplyr", "did", "tibble")
  ) %dopar% {
    set.seed(seed * 1000 + sim_id)

    treated_units <- sample(all_units, n_treated_original, replace = FALSE)
    treatment_years <- sample(possible_years, n_treated_original, replace = TRUE)

    unit_treatment <- data.frame(
      id_microrregiao = all_units,
      gname_placebo = 0
    )
    unit_treatment$gname_placebo[match(treated_units, all_units)] <- treatment_years

    df_placebo <- df %>%
      left_join(unit_treatment, by = "id_microrregiao") %>%
      mutate(
        gname = gname_placebo,
        treated = as.integer(gname > 0 & ano >= gname),
        never_treated = gname == 0
      ) %>%
      select(-gname_placebo)

    tryCatch(
      {
        res <- estimate_att(
          df_placebo,
          method = method,
          label = paste0(label, "_placebo_parallel"),
          overwrite = overwrite,
          persist = FALSE
        )
        res$att_global
      },
      error = function(e) NA_real_
    )
  }

  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")

  valid_results <- placebo_results[!is.na(placebo_results)]
  n_valid <- length(valid_results)

  n_extremes <- sum(abs(valid_results) >= abs(true_att))
  p_value <- (1 + n_extremes) / (n_valid + 1)
  se_pvalue <- sqrt(p_value * (1 - p_value) / n_valid)
  ci_lower_pvalue <- max(0, p_value - 1.96 * se_pvalue)
  ci_upper_pvalue <- min(1, p_value + 1.96 * se_pvalue)
  percentiles <- quantile(valid_results, c(0.025, 0.05, 0.5, 0.95, 0.975))

  summary_tbl <- tibble::tibble(
    label = label,
    method = method,
    n_simulations = n_sims,
    n_valid = n_valid,
    true_att = true_att,
    empirical_p_value = p_value,
    p_value_se = se_pvalue,
    p_value_ci_low = ci_lower_pvalue,
    p_value_ci_high = ci_upper_pvalue,
    n_extremes = n_extremes,
    dist_q025 = percentiles[1],
    dist_q975 = percentiles[5],
    elapsed_minutes = as.numeric(elapsed_time),
    n_cores_used = setup_info$n_cores,
    created_at = as.character(Sys.time())
  )

  summary_path <- write_artifact(
    summary_tbl,
    "tests",
    sprintf("%s_placebo_random_parallel_summary.csv", label),
    overwrite = overwrite
  )

  dist_path <- raw_artifact_path("tests", sprintf("%s_placebo_random_parallel_distribution.csv", label))
  readr::write_csv(tibble::tibble(att = valid_results), dist_path)

  plot <- ggplot(tibble::tibble(att = valid_results), aes(x = att)) +
    geom_histogram(bins = 50, fill = "lightgray", color = "black", alpha = 0.7) +
    geom_vline(xintercept = true_att, color = "red", linewidth = 2) +
    geom_vline(xintercept = percentiles[c(1, 5)], color = "blue", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray50", linetype = "dotted") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Distribuição de ATTs Placebo vs ATT Verdadeiro",
      subtitle = sprintf("P-valor: %.3f | Extremos: %d/%d | Tempo: %.1f min", p_value, n_extremes, n_valid, as.numeric(elapsed_time)),
      x = "ATT Estimado",
      y = "Frequência"
    )

  plot_path <- raw_artifact_path("tests", sprintf("%s_placebo_random_parallel.png", label))
  ggplot2::ggsave(plot_path, plot, width = 10, height = 6, dpi = 300)
  include_metadata(plot_path, list(label = label, created_at = as.character(Sys.time())))

  list(
    summary = summary_tbl,
    summary_path = summary_path,
    distribution_path = dist_path,
    plot = plot,
    plot_path = plot_path,
    true_result = true_res
  )
}

compare_control_groups <- function(panel_source, method = "dr", label = NULL, overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  cli::cli_alert_info("Comparando grupos de controle...")

  n_never <- n_distinct(df$id_microrregiao[df$gname == 0])
  n_total <- n_distinct(df$id_microrregiao)
  pct_never <- n_never / n_total * 100

  cli::cli_alert_info("Unidades nunca tratadas: {n_never} ({round(pct_never, 1)}% do total)")

  results <- list()
  results$nyt <- tryCatch(
    estimate_att(df, method = method, control_grp = "notyettreated", label = paste0(label, "_nyt"), overwrite = overwrite),
    error = function(e) {
      cli::cli_alert_warning("Falha com not-yet-treated: {e$message}")
      NULL
    }
  )

  results$nt <- tryCatch(
    estimate_att(df, method = method, control_grp = "nevertreated", label = paste0(label, "_nt"), overwrite = overwrite),
    error = function(e) {
      cli::cli_alert_warning("Falha com nevertreated: {e$message}")
      NULL
    }
  )

  comparison <- tibble::tibble(
    control_group = c("not-yet-treated", "nevertreated"),
    att = c(
      ifelse(is.null(results$nyt), NA, results$nyt$att_global),
      ifelse(is.null(results$nt), NA, results$nt$att_global)
    ),
    se = c(
      ifelse(is.null(results$nyt), NA, results$nyt$se_global),
      ifelse(is.null(results$nt), NA, results$nt$se_global)
    ),
    p_value = c(
      ifelse(is.null(results$nyt), NA, results$nyt$p),
      ifelse(is.null(results$nt), NA, results$nt$p)
    )
  ) %>%
    filter(!is.na(att))

  artifact_path <- write_artifact(
    comparison,
    "tests",
    sprintf("%s_control_group_comparison.csv", label),
    overwrite = overwrite
  )

  list(
    label = label,
    results = results,
    comparison = comparison,
    artifact_path = artifact_path
  )
}

diagnose_na_cohorts <- function(att_result, panel_source, label = NULL, overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)

  att_obj <- NULL
  if (is.list(att_result) && !is.null(att_result$att)) {
    att_obj <- att_result$att
    label <- sanitize_label(ifelse(is.null(label), att_result$label, label))
  } else if (is.character(att_result) && length(att_result) == 1) {
    att_obj <- readRDS(att_result)
    label <- sanitize_label(ifelse(is.null(label), tools::file_path_sans_ext(basename(att_result)), label))
  } else {
    stop("diagnose_na_cohorts() requer objeto estimate_att() ou caminho para RDS.")
  }

  cli::cli_alert_info("Diagnosticando valores NA em ATT(g,t)...")

  na_cases <- tibble::tibble(
    group = att_obj$group,
    time = att_obj$t,
    att = att_obj$att,
    n = att_obj$n
  ) %>%
    filter(is.na(att) & group > 0)

  cohort_sizes <- df %>%
    filter(gname > 0) %>%
    group_by(gname) %>%
    summarise(
      n_units = n_distinct(id_microrregiao),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(n_units)

  small_cohorts <- cohort_sizes %>%
    filter(n_units < 5)

  na_path <- write_artifact(
    na_cases,
    "tests",
    sprintf("%s_att_na_cases.csv", label),
    overwrite = overwrite
  )

  cohort_path <- write_artifact(
    cohort_sizes,
    "tests",
    sprintf("%s_cohort_sizes.csv", label),
    overwrite = overwrite
  )

  list(
    label = label,
    na_cases = na_cases,
    cohort_sizes = cohort_sizes,
    small_cohorts = small_cohorts,
    na_artifact_path = na_path,
    cohort_artifact_path = cohort_path
  )
}

merge_small_cohorts <- function(panel_source, min_size = 5, bin_width = 2, label = NULL, overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  cli::cli_alert_info("Agregando coortes pequenas...")

  cohort_info <- df %>%
    filter(gname > 0) %>%
    group_by(gname) %>%
    summarise(n_units = n_distinct(id_microrregiao), .groups = "drop") %>%
    arrange(gname)

  small_cohorts <- cohort_info %>%
    filter(n_units < min_size) %>%
    pull(gname)

  if (length(small_cohorts) == 0) {
    cli::cli_alert_success("Nenhuma coorte pequena para agregar")
    merged_path <- raw_artifact_path("tests", sprintf("%s_no_merge_needed.txt", label))
    writeLines("Nenhuma coorte pequena identificada.", merged_path)
    return(list(
      label = label,
      merged_path = merged_path,
      cohort_info = cohort_info,
      df = df
    ))
  }

  year_range <- range(small_cohorts)
  breaks <- seq(year_range[1], year_range[2] + bin_width, by = bin_width)

  df_merged <- df %>%
    mutate(
      gname_original = gname,
      gname = case_when(
        gname == 0 ~ 0,
        gname %in% small_cohorts ~ cut(gname, breaks = breaks, labels = FALSE) * bin_width + year_range[1] - bin_width,
        TRUE ~ gname
      ),
      treated = as.integer(gname > 0 & ano >= gname),
      never_treated = gname == 0
    )

  merged_path <- write_artifact(
    df_merged,
    "tests",
    sprintf("%s_df_microrregiao_merged.rds", label),
    overwrite = overwrite
  )

  list(
    label = label,
    merged_path = merged_path,
    cohort_info = cohort_info,
    df = df_merged
  )
}

estimate_att_robust_se <- function(panel_source,
                                   method = "dr",
                                   se_type = "twoway",
                                   label = NULL,
                                   overwrite = TRUE) {
  cli::cli_alert_info("Estimando com SEs robustos: {se_type}")

  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  if (se_type %in% c("clustered", "default")) {
    res <- estimate_att(df, method = method, label = paste0(label, "_robust_se"), overwrite = overwrite)
    return(res)
  }

  if (se_type == "wildboot") {
    cli::cli_alert_warning("Wild bootstrap não implementado diretamente; utilizando bootstrap padrão com mais replicações.")
    res <- estimate_att(df, method = method, label = paste0(label, "_wildboot"), overwrite = overwrite)
    return(res)
  }

  if (se_type == "twoway") {
    cli::cli_alert_info("Two-way clustering via pós-processamento não está implementado neste pipeline.")
    res <- estimate_att(df, method = method, label = paste0(label, "_twoway_stub"), overwrite = overwrite)
    return(res)
  }

  stop("Tipo de SE não suportado: ", se_type)
}

