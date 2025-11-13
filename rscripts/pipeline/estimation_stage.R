# Stage module: ATT estimation and related utilities
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(cli)

source(here::here("rscripts", "pipeline", "io_utils.R"))

extract_panel_label <- function(panel_source, fallback = "main") {
  if (is.list(panel_source)) {
    if (!is.null(panel_source$label)) {
      return(sanitize_label(panel_source$label))
    }
    if (!is.null(panel_source$microrregiao_path)) {
      return(sanitize_label(tools::file_path_sans_ext(basename(panel_source$microrregiao_path))))
    }
  }
  if (is.character(panel_source) && length(panel_source) == 1) {
    return(sanitize_label(tools::file_path_sans_ext(basename(panel_source))))
  }
  sanitize_label(fallback)
}

resolve_panel_data <- function(panel_source) {
  if (is.data.frame(panel_source)) {
    return(panel_source)
  }

  if (is.list(panel_source) && !is.null(panel_source$microrregiao_path)) {
    return(readRDS(panel_source$microrregiao_path))
  }

  if (is.character(panel_source) && length(panel_source) == 1) {
    if (file.exists(panel_source)) {
      return(readRDS(panel_source))
    }
    if (artifact_exists("prep", panel_source)) {
      return(read_artifact("prep", panel_source))
    }
  }

  stop("Unsupported panel_source provided to resolve_panel_data()")
}

check_covariates <- function(df, covars, label, overwrite = TRUE) {
  cli::cli_alert_info("Verificando colinearidade/variância das covariáveis…")

  df_pre <- df %>% filter((gname == 0) | (gname > 0 & ano < gname))

  var_tbl <- purrr::map_dfr(covars, function(v) {
    if (!v %in% names(df_pre)) {
      return(tibble::tibble(var = v, var_pre = NA_real_, prop_na = 1))
    }
    cv_data <- df_pre[[v]]
    tibble::tibble(
      var = v,
      var_pre = var(cv_data, na.rm = TRUE),
      prop_na = mean(is.na(cv_data))
    )
  })

  valid_covars <- var_tbl %>%
    filter(!is.na(var_pre), var_pre > 1e-10, prop_na < 0.5) %>%
    pull(var)

  pib_vars <- var_tbl %>%
    filter(
      grepl("pib|populacao", var, ignore.case = TRUE),
      !is.na(var_pre), var_pre > 1e-10, prop_na <= 0.2
    ) %>%
    pull(var)

  valid_covars <- unique(c(valid_covars, pib_vars))

  keep <- c()
  for (v in valid_covars) {
    test_set <- c(keep, v)
    df_test <- df_pre %>%
      select(all_of(test_set)) %>%
      tidyr::drop_na()

    if (nrow(df_test) < 50) {
      cli::cli_alert_warning("Poucos dados para testar '{v}' após remover NAs")
      next
    }

    X <- model.matrix(as.formula(paste("~", paste(test_set, collapse = "+"))), data = df_test)
    rk <- qr(X)$rank
    if (rk == ncol(X)) {
      keep <- test_set
    } else {
      cli::cli_alert_warning("Removendo covariável '{v}' por colinearidade no pré-tratamento.")
    }
  }

  diag_path <- write_artifact(
    var_tbl,
    "diagnostics",
    sprintf("%s_covariate_diagnostics.csv", label),
    overwrite = overwrite
  )

  covar_path <- if (length(keep) > 0) {
    write_artifact(
      list(valid_covariates = keep),
      "estimation",
      sprintf("%s_valid_covariates.json", label),
      overwrite = overwrite
    )
  } else {
    NA_character_
  }

  cli::cli_alert_success("Covariáveis elegíveis: {paste(keep, collapse = ', ')}")

  list(valid_covariates = keep, diagnostics_path = diag_path, covars_path = covar_path)
}

estimate_att <- function(panel_source,
                         method = "dr",
                         seed = 42,
                         control_grp = "notyettreated",
                         label = NULL,
                         overwrite = TRUE,
                         candidate_covariates = NULL,
                         persist = TRUE) {
  set.seed(seed)

  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(if (is.null(label)) {
    if (is.character(panel_source) && length(panel_source) == 1) {
      tools::file_path_sans_ext(basename(panel_source))
    } else {
      "main"
    }
  } else {
    label
  })

  cli::cli_h2("Estimando ATT(g,t) - método {method} | label = {label}")

  base_covars <- if (is.null(candidate_covariates)) {
    c(
      "log_area_plantada", "log_populacao", "log_pib_per_capita",
      "log_densidade_estacoes_uf"
    )
  } else {
    candidate_covariates
  }

  covar_info <- if (length(base_covars) == 0) {
    list(valid_covariates = character(0), diagnostics_path = NA_character_, covars_path = NA_character_)
  } else {
    check_covariates(df, base_covars, label, overwrite = overwrite)
  }
  covars_ok <- covar_info$valid_covariates

  xform <- if (length(covars_ok) == 0) {
    ~1
  } else {
    as.formula(paste("~", paste(covars_ok, collapse = "+")))
  }

  safe_att <- function(met, form) {
    cli::cli_alert_info("Tentando estimador {met} com fórmula: {deparse(form)}")
    did::att_gt(
      yname = "log_pib_agro",
      tname = "ano",
      idname = "id_microrregiao",
      gname = "gname",
      xformla = form,
      data = df,
      est_method = met,
      control_group = control_grp,
      bstrap = TRUE
    )
  }

  att_res <- tryCatch(
    {
      safe_att(method, xform)
    },
    error = function(e) {
      if (grepl("singular", e$message)) {
        cli::cli_alert_warning("Singularidade detectada. Reestimando sem covariáveis…")
        tryCatch(safe_att(method, ~1), error = function(e2) {
          cli::cli_alert_warning("Ainda falhou. Alternando para método 'ipw'.")
          safe_att("ipw", ~1)
        })
      } else {
        stop(e)
      }
    }
  )

  agg_overall <- did::aggte(att_res, type = "group", na.rm = TRUE)
  agg_event <- did::aggte(att_res, type = "dynamic", na.rm = TRUE)

  if (any(is.na(att_res$att))) {
    cli::cli_alert_warning("Alguns ATT(g,t) não foram estimados (NA). Eles foram removidos na agregação.")
  }

  att_global <- agg_overall$overall.att
  se_global <- agg_overall$overall.se
  z_score <- att_global / se_global
  p_value <- 2 * stats::pnorm(-abs(z_score))
  ci_low <- att_global - 1.96 * se_global
  ci_high <- att_global + 1.96 * se_global

  summary_tbl <- tibble::tibble(
    label = label,
    method = method,
    control_group = control_grp,
    att = att_global,
    se = se_global,
    z = z_score,
    p_value = p_value,
    ci_low = ci_low,
    ci_high = ci_high,
    n_obs = nrow(df),
    n_units = dplyr::n_distinct(df$id_microrregiao),
    created_at = as.character(Sys.time())
  )

  if (persist) {
    att_path <- write_artifact(att_res, "estimation", sprintf("%s_att_results_%s.rds", label, method), overwrite = overwrite)
    overall_path <- write_artifact(agg_overall, "estimation", sprintf("%s_agg_overall_%s.rds", label, method), overwrite = overwrite)
    event_path <- write_artifact(agg_event, "estimation", sprintf("%s_agg_event_%s.rds", label, method), overwrite = overwrite)
    summary_path <- write_artifact(summary_tbl, "estimation", sprintf("%s_att_summary_%s.csv", label, method), overwrite = overwrite)

    include_metadata(att_path, list(method = method, control_group = control_grp, label = label))
    include_metadata(overall_path, list(method = method, control_group = control_grp, label = label))
    include_metadata(event_path, list(method = method, control_group = control_grp, label = label))
  } else {
    att_path <- overall_path <- event_path <- summary_path <- NA_character_
  }

  cli::cli_alert_success("ATT = {round(att_global,4)}, SE = {round(se_global,4)}, z = {round(z_score,2)}, p = {signif(p_value, 3)}")

  list(
    label = label,
    method = method,
    covariates = covars_ok,
    covariate_artifact = covar_info$covars_path,
    diagnostics_artifact = covar_info$diagnostics_path,
    att_path = att_path,
    agg_overall_path = overall_path,
    agg_event_path = event_path,
    summary_path = summary_path,
    att = att_res,
    overall = agg_overall,
    event = agg_event,
    att_global = att_global,
    se_global = se_global,
    z = z_score,
    p = p_value,
    ci_low = ci_low,
    ci_high = ci_high,
    att_object = att_res,
    agg_overall_object = agg_overall,
    agg_event_object = agg_event,
    summary_object = summary_tbl
  )
}

analyze_weights <- function(att_result, label = NULL, overwrite = TRUE) {
  att_obj <- NULL
  if (is.list(att_result) && !is.null(att_result$att)) {
    att_obj <- att_result$att
    label <- sanitize_label(ifelse(is.null(label), att_result$label, label))
  } else if (is.character(att_result) && length(att_result) == 1) {
    att_obj <- readRDS(att_result)
    label <- sanitize_label(ifelse(is.null(label), tools::file_path_sans_ext(basename(att_result)), label))
  } else {
    stop("analyze_weights() requires either an estimate_att() result or path to att RDS.")
  }

  cli::cli_alert_info("Analisando distribuição de pesos por coorte (label = {label})")

  groups <- unique(att_obj$group[att_obj$group > 0])
  max_period <- max(att_obj$t)

  post_periods <- purrr::map_dfr(groups, function(g) {
    tibble::tibble(
      group = g,
      n_post_periods = max_period - g + 1
    )
  })

  weight_data <- att_obj$DIDparams$data %>%
    filter(gname > 0) %>%
    group_by(gname) %>%
    summarise(n_units = n_distinct(id_microrregiao), .groups = "drop") %>%
    mutate(weight = n_units / sum(n_units)) %>%
    rename(group = gname)

  weight_analysis <- weight_data %>%
    left_join(post_periods, by = "group") %>%
    arrange(group)

  cor_weight_periods <- cor(weight_analysis$weight, weight_analysis$n_post_periods)
  cli::cli_alert_info("Correlação entre peso e períodos pós-tratamento: {round(cor_weight_periods, 3)}")

  early_cohorts <- weight_analysis %>%
    filter(n_post_periods >= quantile(n_post_periods, 0.75))
  early_weight <- sum(early_cohorts$weight)

  if (early_weight > 0.5) {
    cli::cli_alert_warning("Coortes iniciais representam {round(early_weight * 100, 1)}% do peso total")
  }

  csv_path <- write_artifact(
    weight_analysis,
    "estimation",
    sprintf("%s_weights_analysis.csv", label),
    overwrite = overwrite
  )

  plot_path <- raw_artifact_path("estimation", sprintf("%s_weights_distribution.png", label))

  p_weights <- ggplot(weight_analysis, aes(x = factor(group), y = weight)) +
    geom_col(aes(fill = n_post_periods), alpha = 0.7) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Períodos pós") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    labs(
      title = "Distribuição de Pesos por Coorte de Adoção",
      x = "Ano de adoção",
      y = "Peso no ATT agregado"
    )

  ggplot2::ggsave(plot_path, p_weights, width = 10, height = 6, dpi = 300)
  include_metadata(plot_path, list(label = label, created_at = as.character(Sys.time())))

  list(
    label = label,
    data = weight_analysis,
    csv_path = csv_path,
    plot_path = plot_path,
    plot = p_weights,
    correlation = cor_weight_periods,
    early_weight_share = early_weight
  )
}

robust_specs <- function(panel_source, methods = c("dr", "ipw", "reg"), label = NULL, overwrite = TRUE) {
  base_label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  results <- purrr::map_dfr(methods, function(m) {
    res <- tryCatch(
      estimate_att(panel_source, method = m, label = base_label, overwrite = overwrite),
      error = function(e) {
        cli::cli_alert_warning("Método {m} falhou: {e$message}")
        NULL
      }
    )

    if (is.null(res)) {
      tibble::tibble(
        metodo = m,
        att_global = NA_real_,
        se_global = NA_real_,
        z = NA_real_,
        p = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_
      )
    } else {
      tibble::tibble(
        metodo = m,
        att_global = res$att_global,
        se_global = res$se_global,
        z = res$z,
        p = res$p,
        ci_low = res$ci_low,
        ci_high = res$ci_high
      )
    }
  })

  path <- write_artifact(
    results,
    "estimation",
    sprintf("%s_robust_specs.csv", base_label),
    overwrite = overwrite
  )

  list(
    label = base_label,
    results = results,
    artifact_path = path
  )
}

robustness_analysis <- function(panel_source, label = NULL, overwrite = TRUE) {
  cli::cli_h2("Análise de Robustez - Múltiplas Especificações")

  df_base <- resolve_panel_data(panel_source)
  base_label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  specs <- list(
    list(
      name = "Baseline (DR)",
      method = "dr",
      covars = c("log_area_plantada", "log_populacao", "log_pib_per_capita", "log_densidade_estacoes_uf"),
      filter_years = FALSE,
      suffix = "baseline"
    ),
    list(
      name = "Sem covariáveis",
      method = "dr",
      covars = character(0),
      filter_years = FALSE,
      suffix = "sem_covariaveis"
    ),
    list(
      name = "Covariáveis básicas",
      method = "dr",
      covars = c("log_area_plantada", "log_populacao"),
      filter_years = FALSE,
      suffix = "covariaveis_basicas"
    ),
    list(
      name = "IPW",
      method = "ipw",
      covars = c("log_area_plantada", "log_populacao", "log_pib_per_capita", "log_densidade_estacoes_uf"),
      filter_years = FALSE,
      suffix = "ipw"
    ),
    list(
      name = "REG",
      method = "reg",
      covars = c("log_area_plantada", "log_populacao", "log_pib_per_capita", "log_densidade_estacoes_uf"),
      filter_years = FALSE,
      suffix = "reg"
    ),
    list(
      name = "Excl. 2022-23",
      method = "dr",
      covars = c("log_area_plantada", "log_populacao", "log_pib_per_capita", "log_densidade_estacoes_uf"),
      filter_years = TRUE,
      suffix = "ate_2021"
    )
  )

  results <- purrr::map_dfr(specs, function(spec) {
    cli::cli_alert_info("Estimando: {spec$name}")

    df_spec <- if (spec$filter_years) {
      df_base %>% filter(ano <= 2021)
    } else {
      df_base
    }

    spec_label <- sanitize_label(paste(base_label, spec$suffix, sep = "_"))

    res <- tryCatch(
      estimate_att(
        df_spec,
        method = spec$method,
        label = spec_label,
        overwrite = overwrite,
        candidate_covariates = spec$covars
      ),
      error = function(e) {
        cli::cli_alert_warning("Erro em {spec$name}: {e$message}")
        NULL
      }
    )

    if (is.null(res)) {
      tibble::tibble(
        specification = spec$name,
        method = spec$method,
        att = NA_real_,
        se = NA_real_,
        p_value = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        n_obs = nrow(df_spec),
        significant = NA
      )
    } else {
      tibble::tibble(
        specification = spec$name,
        method = spec$method,
        att = res$att_global,
        se = res$se_global,
        p_value = res$p,
        ci_lower = res$ci_low,
        ci_upper = res$ci_high,
        n_obs = nrow(df_spec),
        significant = res$p < 0.05
      )
    }
  })

  results_path <- write_artifact(
    results,
    "estimation",
    sprintf("%s_robustness_analysis.csv", base_label),
    overwrite = overwrite
  )

  plot_data <- results %>% filter(!is.na(att))

  p_robust <- ggplot(plot_data, aes(x = specification, y = att)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Análise de Robustez: ATT sob Diferentes Especificações",
      x = "Especificação",
      y = "ATT Estimado"
    )

  plot_path <- raw_artifact_path("estimation", sprintf("%s_robustness_plot.png", base_label))
  ggplot2::ggsave(plot_path, p_robust, width = 10, height = 6, dpi = 300)
  include_metadata(plot_path, list(label = base_label, created_at = as.character(Sys.time())))

  list(
    label = base_label,
    results = results,
    results_path = results_path,
    plot = p_robust,
    plot_path = plot_path
  )
}

