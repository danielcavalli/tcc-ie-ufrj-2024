# Stage module: descriptive analysis and reporting
source(here::here("rscripts", "pipeline", "io_utils.R"))

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) > 0 && !all(is.na(x))) {
    x
  } else {
    y
  }
}

prepare_main_results_table <- function(att_res,
                                       placebo_fixed = NULL,
                                       placebo_non_agro = NULL,
                                       control_groups = NULL,
                                       robustness_res = NULL,
                                       label = NULL,
                                       overwrite = TRUE) {
  if (is.null(att_res)) {
    stop("prepare_main_results_table() requires the main ATT results.")
  }

  base_label <- sanitize_label(ifelse(
    is.null(label),
    att_res$label %||% "main",
    label
  ))

  rows <- list()

  rows[[length(rows) + 1]] <- tibble::tibble(
    analysis = "ATT Principal (PIB Agro)",
    att = att_res$att_global,
    se = att_res$se_global,
    p_value = att_res$p,
    ci_lower = att_res$ci_low,
    ci_upper = att_res$ci_high,
    n = att_res$summary_object$n_obs[1] %||% NA_real_
  )

  if (!is.null(placebo_non_agro)) {
    rows[[length(rows) + 1]] <- tibble::tibble(
      analysis = "Placebo (PIB Não-Agro)",
      att = placebo_non_agro$summary$att[1],
      se = placebo_non_agro$summary$se[1],
      p_value = placebo_non_agro$summary$p_value[1],
      ci_lower = placebo_non_agro$summary$ci_low[1],
      ci_upper = placebo_non_agro$summary$ci_high[1],
      n = placebo_non_agro$summary$n_obs[1] %||% NA_real_
    )
  }

  if (!is.null(placebo_fixed)) {
    rows[[length(rows) + 1]] <- tibble::tibble(
      analysis = sprintf("Placebo (Ano Fictício %s)", placebo_fixed$summary$placebo_year[1]),
      att = placebo_fixed$summary$att[1],
      se = placebo_fixed$summary$se[1],
      p_value = placebo_fixed$summary$p_value[1],
      ci_lower = placebo_fixed$summary$ci_low[1],
      ci_upper = placebo_fixed$summary$ci_high[1],
      n = placebo_fixed$summary$n_units[1]
    )
  }

  if (!is.null(control_groups) && !is.null(control_groups$comparison)) {
    nevertreated_row <- control_groups$comparison %>%
      dplyr::filter(control_group == "nevertreated")

    if (nrow(nevertreated_row) == 1) {
      rows[[length(rows) + 1]] <- tibble::tibble(
        analysis = "Robustez - Nevertreated",
        att = nevertreated_row$att,
        se = nevertreated_row$se,
        p_value = nevertreated_row$p_value,
        ci_lower = nevertreated_row$att - 1.96 * nevertreated_row$se,
        ci_upper = nevertreated_row$att + 1.96 * nevertreated_row$se,
        n = NA_real_
      )
    }
  }

  if (!is.null(robustness_res) && !is.null(robustness_res$results)) {
    label_map <- c(
      "Sem covariáveis" = "Robustez - Sem Covariáveis",
      "Covariáveis básicas" = "Robustez - Covariáveis Básicas",
      "IPW" = "Robustez - IPW",
      "REG" = "Robustez - REG",
      "Excl. 2022-23" = "Robustez - Até 2021"
    )

    robust_rows <- robustness_res$results %>%
      dplyr::filter(specification %in% names(label_map)) %>%
      dplyr::mutate(
        analysis = label_map[specification]
      ) %>%
      dplyr::select(
        analysis,
        att,
        se,
        p_value,
        ci_lower,
        ci_upper,
        n = n_obs
      )

    if (nrow(robust_rows) > 0) {
      rows[[length(rows) + 1]] <- robust_rows
    }
  }

  results_tbl <- rows %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      att = as.numeric(att),
      se = as.numeric(se),
      p_value = as.numeric(p_value),
      ci_lower = as.numeric(ci_lower),
      ci_upper = as.numeric(ci_upper)
    )

  artifact_path <- write_artifact(
    results_tbl,
    "reporting",
    sprintf("%s_main_results_table.csv", base_label),
    overwrite = overwrite
  )

  list(
    label = base_label,
    table = results_tbl,
    artifact_path = artifact_path
  )
}

sensitivity_analysis_stage <- function(panel_source,
                                       label = NULL,
                                       scenarios = NULL,
                                       overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  base_label <- sanitize_label(ifelse(is.null(label),
    extract_panel_label(panel_source, fallback = "main"),
    label
  ))

  if (is.null(scenarios)) {
    scenarios <- list(
      "Completo (2003-2023)" = c(2003, 2023),
      "Excluindo Início (2006-2023)" = c(2006, 2023),
      "Excluindo Final (2003-2019)" = c(2003, 2019),
      "Excluindo COVID (2003-2019)" = c(2003, 2019),
      "Período Central (2010-2019)" = c(2010, 2019),
      "Pré-COVID (2003-2019)" = c(2003, 2019)
    )
  }

  cli::cli_h2("Análise de sensibilidade temporal ({base_label})")

  results <- purrr::map_dfr(names(scenarios), function(name) {
    window <- scenarios[[name]]
    cli::cli_alert_info("Estimando cenário: {name}")

    df_window <- df %>%
      dplyr::filter(ano >= window[1], ano <= window[2])

    valid_treated <- sum(df_window$treated == 1, na.rm = TRUE)
    valid_control <- sum(df_window$never_treated == TRUE, na.rm = TRUE)

    if (valid_treated == 0 || valid_control == 0) {
      cli::cli_alert_warning("Cenário {name} não possui tratados ou controles suficientes (tratados = {valid_treated}, controles = {valid_control}).")
      return(tibble::tibble(
        scenario = name,
        att = NA_real_,
        se = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        n_treated = valid_treated,
        years_range = sprintf("%d - %d", window[1], window[2])
      ))
    }

    tryCatch(
      {
        att_res <- did::att_gt(
          yname = "log_pib_agro",
          tname = "ano",
          idname = "id_microrregiao",
          gname = "gname",
          data = df_window,
          control_group = "notyettreated",
          est_method = "dr",
          print_details = FALSE
        )
        agg <- did::aggte(att_res, type = "simple", na.rm = TRUE)
        tibble::tibble(
          scenario = name,
          att = agg$overall.att,
          se = agg$overall.se,
          ci_lower = agg$overall.att - 1.96 * agg$overall.se,
          ci_upper = agg$overall.att + 1.96 * agg$overall.se,
          n_treated = sum(df_window$treated, na.rm = TRUE),
          years_range = sprintf("%d - %d", window[1], window[2])
        )
      },
      error = function(e) {
        cli::cli_alert_warning("Falha no cenário {name}: {e$message}")
        tibble::tibble(
          scenario = name,
          att = NA_real_,
          se = NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          n_treated = NA_real_,
          years_range = sprintf("%d - %d", window[1], window[2])
        )
      }
    )
  })

  valid_results <- results %>% dplyr::filter(!is.na(att))

  artifact_path <- write_artifact(
    valid_results,
    "reporting",
    sprintf("%s_sensitivity_results.csv", base_label),
    overwrite = overwrite
  )

  plot_path <- NA_character_
  plot_obj <- NULL

  if (nrow(valid_results) > 0) {
    plot_obj <- ggplot2::ggplot(
      valid_results,
      ggplot2::aes(x = reorder(scenario, att), y = att)
    ) +
      ggplot2::geom_point(size = 3, color = "#2E86AB") +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
        width = 0.2,
        color = "#2E86AB"
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Análise de Sensibilidade: ATT por recorte temporal",
        x = NULL,
        y = "ATT (log PIB agropecuário)"
      )

    plot_path <- raw_artifact_path(
      "reporting",
      sprintf("%s_sensitivity_plot.png", base_label)
    )
    ggplot2::ggsave(plot_path, plot_obj, width = 10, height = 6, dpi = 300)
  }

  list(
    label = base_label,
    results = valid_results,
    raw_results = results,
    artifact_path = artifact_path,
    plot = plot_obj,
    plot_path = plot_path
  )
}

generate_summary_cards <- function(att_res,
                                   weights_res,
                                   heterogeneity_res,
                                   label = NULL,
                                   overwrite = TRUE) {
  base_label <- sanitize_label(ifelse(
    is.null(label),
    att_res$label %||% "main",
    label
  ))

  summary_tbl <- tibble::tibble(
    metric = c(
      "ATT Principal",
      "Erro-Padrão",
      "P-valor",
      "Correlação Peso vs Pós-Períodos"
    ),
    value = c(
      att_res$att_global,
      att_res$se_global,
      att_res$p,
      weights_res$correlation
    )
  )

  artifact_path <- write_artifact(
    summary_tbl,
    "reporting",
    sprintf("%s_summary_cards.csv", base_label),
    overwrite = overwrite
  )

  list(
    label = base_label,
    summary = summary_tbl,
    artifact_path = artifact_path
  )
}

