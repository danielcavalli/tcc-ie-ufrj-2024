# Stage module: heterogeneity and cross-sectional analyses
source(here::here("rscripts", "pipeline", "io_utils.R"))
source(here::here("rscripts", "pipeline", "estimation_stage.R"))

# Helper to coerce different inputs into data frames --------------------------
resolve_panel_like <- function(source) {
  if (is.data.frame(source)) {
    return(source)
  }
  if (is.list(source) && !is.null(source$microrregiao_path)) {
    return(readRDS(source$microrregiao_path))
  }
  if (is.list(source) && !is.null(source$municipio_path)) {
    return(readRDS(source$municipio_path))
  }
  if (is.character(source) && length(source) == 1 && file.exists(source)) {
    ext <- tools::file_ext(source)
    if (tolower(ext) == "rds") {
      return(readRDS(source))
    }
    stop("Unsupported file type supplied to resolve_panel_like(): ", source)
  }
  stop("Unsupported input provided to resolve_panel_like()")
}

# Heterogeneity by macro-region ------------------------------------------------
heterogeneity_analysis <- function(panel_source,
                                   method = "dr",
                                   label = NULL,
                                   overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  if (!"sigla_uf" %in% names(df)) {
    stop("heterogeneity_analysis() requires a sigla_uf column.")
  }

  base_label <- sanitize_label(ifelse(is.null(label),
    extract_panel_label(panel_source, fallback = "main"),
    label
  ))

  region_map <- list(
    north = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
    northeast = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
    centerwest = c("DF", "GO", "MT", "MS"),
    southeast = c("ES", "MG", "RJ", "SP"),
    south = c("PR", "RS", "SC")
  )

  region_titles <- c(
    north = "Norte",
    northeast = "Nordeste",
    centerwest = "Centro-Oeste",
    southeast = "Sudeste",
    south = "Sul"
  )

  cli::cli_h2("Heterogeneity analysis by macro-region ({base_label})")

  region_results <- purrr::imap_dfr(region_map, function(ufs, region_id) {
    region_label <- region_titles[[region_id]]
    cli::cli_alert_info("Estimating ATT for region {region_label}...")

    df_region <- df %>% dplyr::filter(sigla_uf %in% ufs)

    treated_units <- sum(df_region$treated == 1, na.rm = TRUE)
    control_units <- sum(df_region$never_treated == TRUE, na.rm = TRUE)
    if (treated_units < 5 || control_units < 5) {
      cli::cli_alert_warning("Insufficient treated/control units for {region_label}; skipping.")
      return(tibble::tibble(
        region = region_label,
        att = NA_real_,
        se = NA_real_,
        p_value = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        n_obs = nrow(df_region)
      ))
    }

    res <- tryCatch(
      estimate_att(
        df_region,
        method = method,
        label = paste0(base_label, "_", sanitize_label(region_id)),
        overwrite = overwrite,
        persist = FALSE
      ),
      error = function(e) {
        cli::cli_alert_warning("ATT failed for {region_label}: {e$message}")
        NULL
      }
    )

    if (is.null(res)) {
      return(tibble::tibble(
        region = region_label,
        att = NA_real_,
        se = NA_real_,
        p_value = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        n_obs = nrow(df_region)
      ))
    }

    tibble::tibble(
      region = region_label,
      att = res$att_global,
      se = res$se_global,
      p_value = res$p,
      ci_lower = res$ci_low,
      ci_upper = res$ci_high,
      n_obs = nrow(df_region),
      n_treated = treated_units,
      n_control = control_units,
      significant = res$p < 0.05
    )
  })

  artifact_path <- write_artifact(
    region_results,
    "heterogeneity",
    sprintf("%s_heterogeneity_regional.csv", base_label),
    overwrite = overwrite
  )

  plot_data <- region_results %>% dplyr::filter(!is.na(att))
  plot_obj <- NULL
  plot_path <- NA_character_

  if (nrow(plot_data) > 0) {
    plot_obj <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = reorder(region, att), y = att * 100)
    ) +
      ggplot2::geom_col(ggplot2::aes(fill = significant), alpha = 0.8) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = ci_lower * 100,
          ymax = ci_upper * 100
        ),
        width = 0.2
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "steelblue", "FALSE" = "gray70", "NA" = "gray80"),
        na.value = "gray80",
        guide = "none"
      ) +
      ggplot2::labs(
        title = "ATT by macro-region",
        x = NULL,
        y = "ATT (%)"
      ) +
      ggplot2::theme_minimal()

    plot_path <- raw_artifact_path(
      "heterogeneity",
      sprintf("%s_heterogeneity_regional.png", base_label)
    )
    ggplot2::ggsave(plot_path, plot_obj, width = 9, height = 6, dpi = 300)
  }

  list(
    label = base_label,
    results = region_results,
    artifact_path = artifact_path,
    plot = plot_obj,
    plot_path = plot_path
  )
}

# UF-level aggregation ---------------------------------------------------------
uf_level_analysis <- function(panel_source,
                              method = "dr",
                              label = NULL,
                              overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  base_label <- sanitize_label(ifelse(is.null(label),
    extract_panel_label(panel_source, fallback = "main"),
    label
  ))

  cli::cli_h2("UF-level DiD ({base_label})")

  df_uf <- df %>%
    dplyr::group_by(sigla_uf, ano) %>%
    dplyr::summarise(
      log_pib_agro = stats::weighted.mean(log_pib_agro, w = area_plantada, na.rm = TRUE),
      log_pib_nao_agro = stats::weighted.mean(log_pib_nao_agro, w = area_plantada, na.rm = TRUE),
      log_area_plantada = log(sum(area_plantada, na.rm = TRUE) + 1),
      log_populacao = log(sum(populacao_total, na.rm = TRUE) + 1),
      log_pib_per_capita = stats::weighted.mean(log_pib_per_capita, w = populacao_total, na.rm = TRUE),
      treated_share = mean(treated, na.rm = TRUE),
      gname_candidate = suppressWarnings(min(dplyr::if_else(gname > 0, gname, Inf), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      gname_candidate = dplyr::if_else(is.infinite(gname_candidate), NA_real_, gname_candidate)
    ) %>%
    dplyr::group_by(sigla_uf) %>%
    dplyr::mutate(
      gname = dplyr::case_when(
        any(treated_share >= 0.2, na.rm = TRUE) ~ min(ano[treated_share >= 0.2], na.rm = TRUE),
        any(treated_share >= 0.1, na.rm = TRUE) ~ min(ano[treated_share >= 0.1], na.rm = TRUE),
        !is.na(gname_candidate) ~ gname_candidate,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      id_uf = as.integer(as.factor(sigla_uf)),
      treated = as.integer(gname > 0 & ano >= gname),
      never_treated = gname == 0,
      log_densidade_estacoes_uf = log1p(treated_share)
    )

  treated_groups <- dplyr::n_distinct(df_uf$gname[df_uf$gname > 0])
  if (treated_groups < 2) {
    cli::cli_alert_warning("Not enough variation in UF adoption timing. Skipping UF-level analysis.")
    return(list(
      label = base_label,
      results = tibble::tibble(),
      artifact_path = NA_character_,
      panel = df_uf
    ))
  }

  att_uf <- tryCatch(
    did::att_gt(
      yname = "log_pib_agro",
      tname = "ano",
      idname = "id_uf",
      gname = "gname",
      xformla = ~ log_area_plantada + log_populacao + log_pib_per_capita + log_densidade_estacoes_uf,
      data = df_uf,
      est_method = method,
      control_group = "notyettreated",
      bstrap = TRUE,
      clustervars = "id_uf"
    ),
    error = function(e) {
      cli::cli_alert_warning("UF-level ATT failed: {e$message}")
      NULL
    }
  )

  if (is.null(att_uf)) {
    return(list(
      label = base_label,
      results = tibble::tibble(),
      artifact_path = NA_character_,
      panel = df_uf
    ))
  }

  agg_overall <- did::aggte(att_uf, type = "overall", na.rm = TRUE)
  agg_group <- did::aggte(att_uf, type = "group", na.rm = TRUE)

  results_tbl <- tibble::tibble(
    label = base_label,
    att = agg_overall$overall.att,
    se = agg_overall$overall.se,
    p_value = 2 * stats::pnorm(-abs(att / se)),
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se
  )

  artifact_path <- write_artifact(
    results_tbl,
    "heterogeneity",
    sprintf("%s_uf_results.csv", base_label),
    overwrite = overwrite
  )

  group_tbl <- tibble::tibble(
    gname = agg_group$egt,
    att = agg_group$att.egt,
    se = agg_group$se.egt
  )

  list(
    label = base_label,
    results = results_tbl,
    group_results = group_tbl,
    att = att_uf,
    artifact_path = artifact_path,
    panel = df_uf
  )
}

# Municipality-level analysis --------------------------------------------------
municipio_level_analysis <- function(municipio_source,
                                     method = "dr",
                                     label = NULL,
                                     overwrite = TRUE) {
  df_mun <- resolve_panel_like(municipio_source)

  base_label <- sanitize_label(ifelse(is.null(label), "municipio", label))

  cli::cli_h2("Municipality-level DiD ({base_label})")

  densidade_uf <- df_mun %>%
    dplyr::group_by(sigla_uf, ano) %>%
    dplyr::summarise(
      treated_share = mean(tratado == 1, na.rm = TRUE),
      .groups = "drop"
    )

  df_mun_did <- df_mun %>%
    dplyr::left_join(densidade_uf, by = c("sigla_uf", "ano")) %>%
    dplyr::mutate(
      gname = dplyr::if_else(
        !is.na(primeiro_ano_tratamento) & primeiro_ano_tratamento > 0,
        primeiro_ano_tratamento,
        0
      ),
      treated = as.integer(gname > 0 & ano >= gname),
      never_treated = gname == 0,
      log_pib_agro = log1p(pib_agropecuario),
      log_area_plantada = log1p(area_cana),
      log_populacao = log1p(populacao_total),
      log_pib_per_capita = log1p(pib_total / pmax(populacao_total, 1)),
      log_densidade_estacoes_uf = log1p(treated_share)
    ) %>%
    dplyr::filter(!is.na(log_pib_agro))

  att_mun <- tryCatch(
    estimate_att(
      df_mun_did,
      method = method,
      label = paste0(base_label, "_municipio"),
      overwrite = overwrite,
      persist = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning("Municipality-level ATT failed: {e$message}")
      NULL
    }
  )

  if (is.null(att_mun)) {
    return(list(
      label = base_label,
      data = df_mun_did,
      summary = tibble::tibble(),
      artifact_path = NA_character_
    ))
  }

  summary_tbl <- tibble::tibble(
    label = base_label,
    att = att_mun$att_global,
    se = att_mun$se_global,
    p_value = att_mun$p,
    ci_lower = att_mun$ci_low,
    ci_upper = att_mun$ci_high,
    n_obs = nrow(df_mun_did),
    n_units = dplyr::n_distinct(df_mun_did$id_municipio)
  )

  artifact_path <- write_artifact(
    summary_tbl,
    "heterogeneity",
    sprintf("%s_municipio_results.csv", base_label),
    overwrite = overwrite
  )

  list(
    label = base_label,
    data = df_mun_did,
    summary = summary_tbl,
    artifact_path = artifact_path,
    att = att_mun
  )
}

# Crop heterogeneity -----------------------------------------------------------
crop_heterogeneity_analysis <- function(municipio_source,
                                        method = "dr",
                                        label = NULL,
                                        overwrite = TRUE) {
  df_mun <- resolve_panel_like(municipio_source)
  base_label <- sanitize_label(ifelse(is.null(label), "municipio", label))

  cli::cli_h2("Crop heterogeneity ({base_label})")

  crop_vars <- c("area_cana", "area_soja", "area_cafe", "area_citricos")
  available_crops <- crop_vars[crop_vars %in% names(df_mun)]
  if (length(available_crops) == 0) {
    cli::cli_alert_warning("No crop columns found for crop_heterogeneity_analysis().")
    return(list(
      label = base_label,
      results = tibble::tibble(),
      artifact_path = NA_character_,
      plot_path = NA_character_
    ))
  }

  densidade_uf <- df_mun %>%
    dplyr::group_by(sigla_uf, ano) %>%
    dplyr::summarise(
      treated_share = mean(tratado == 1, na.rm = TRUE),
      .groups = "drop"
    )

  df_base <- df_mun %>%
    dplyr::left_join(densidade_uf, by = c("sigla_uf", "ano")) %>%
    dplyr::mutate(
      gname = dplyr::if_else(
        !is.na(primeiro_ano_tratamento) & primeiro_ano_tratamento > 0,
        primeiro_ano_tratamento,
        0
      ),
      treated = as.integer(gname > 0 & ano >= gname),
      never_treated = gname == 0,
      log_populacao = log1p(populacao_total),
      log_pib_per_capita = log1p(pib_total / pmax(populacao_total, 1)),
      log_area_plantada = log1p(area_cana),
      log_densidade_estacoes_uf = log1p(treated_share)
    )

  results_list <- purrr::map(available_crops, function(crop_var) {
    crop_name <- gsub("^area_", "", crop_var)
    cli::cli_alert_info("Estimating ATT for crop {crop_name}...")

    df_crop <- df_base %>%
      dplyr::mutate(
        outcome = log1p(.data[[crop_var]])
      ) %>%
      dplyr::filter(!is.na(outcome))

    if (nrow(df_crop) < 500) {
      cli::cli_alert_warning("Too few observations for {crop_name}; skipping.")
      return(NULL)
    }

    att_crop <- tryCatch(
      did::att_gt(
        yname = "outcome",
        tname = "ano",
        idname = "id_municipio",
        gname = "gname",
        xformla = ~ log_populacao + log_pib_per_capita + log_densidade_estacoes_uf,
        data = df_crop,
        est_method = method,
        control_group = "notyettreated",
        bstrap = TRUE
      ),
      error = function(e) {
        cli::cli_alert_warning("Crop {crop_name} failed: {e$message}")
        NULL
      }
    )

    if (is.null(att_crop)) {
      return(NULL)
    }

    agg_crop <- did::aggte(att_crop, type = "simple", na.rm = TRUE)
    se <- agg_crop$overall.se
    att <- agg_crop$overall.att

    tibble::tibble(
      crop = crop_name,
      att = att,
      se = se,
      p_value = 2 * stats::pnorm(-abs(att / se)),
      ci_lower = att - 1.96 * se,
      ci_upper = att + 1.96 * se,
      n_obs = nrow(df_crop),
      n_units = dplyr::n_distinct(df_crop$id_municipio)
    )
  })

  results_tbl <- results_list %>%
    purrr::compact() %>%
    dplyr::bind_rows()

  if (nrow(results_tbl) == 0) {
    cli::cli_alert_warning("No valid crop-specific estimates were produced.")
    return(list(
      label = base_label,
      results = tibble::tibble(),
      artifact_path = NA_character_,
      plot_path = NA_character_
    ))
  }

  artifact_path <- write_artifact(
    results_tbl,
    "heterogeneity",
    sprintf("%s_crop_results.csv", base_label),
    overwrite = overwrite
  )

  plot_path <- raw_artifact_path(
    "heterogeneity",
    sprintf("%s_crop_results.png", base_label)
  )

  plot_obj <- ggplot2::ggplot(
    results_tbl,
    ggplot2::aes(x = reorder(crop, att), y = att * 100)
  ) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = ci_lower * 100,
        ymax = ci_upper * 100
      ),
      width = 0.2
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "ATT by crop (log area cultivated)",
      x = NULL,
      y = "ATT (%)"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(plot_path, plot_obj, width = 8, height = 5, dpi = 300)

  list(
    label = base_label,
    results = results_tbl,
    artifact_path = artifact_path,
    plot = plot_obj,
    plot_path = plot_path
  )
}

