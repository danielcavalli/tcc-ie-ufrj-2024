# Stage module: visualization helpers
source(here::here("rscripts", "pipeline", "io_utils.R"))

visualize_results <- function(att_event_source, label = NULL, overwrite = TRUE, prefix = "event_study") {
  if (is.list(att_event_source) && !is.null(att_event_source$event)) {
    att_event <- att_event_source$event
    label <- sanitize_label(ifelse(is.null(label), att_event_source$label, label))
  } else if (is.character(att_event_source) && length(att_event_source) == 1) {
    att_event <- readRDS(att_event_source)
    label <- sanitize_label(ifelse(is.null(label), tools::file_path_sans_ext(basename(att_event_source)), label))
  } else if (inherits(att_event_source, "aggte")) {
    att_event <- att_event_source
    label <- sanitize_label(ifelse(is.null(label), prefix, label))
  } else {
    stop("visualize_results() requer objeto aggte ou caminho para RDS.")
  }

  event_df <- tibble::tibble(
    time_relative = att_event$egt,
    att = att_event$att.egt,
    se = att_event$se.egt
  )

  plot <- ggplot(event_df, aes(x = time_relative, y = att)) +
    geom_ribbon(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), fill = "skyblue", alpha = 0.3) +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "navy") +
    geom_line(color = "navy", linewidth = 1) +
    geom_point(color = "navy", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Event Study: Impacto da Estação ao Longo do Tempo",
      x = "Períodos em relação ao tratamento",
      y = "Efeito estimado (log-produtividade)"
    )

  png_path <- raw_artifact_path("visuals", sprintf("%s_%s.png", label, prefix))
  pdf_path <- raw_artifact_path("visuals", sprintf("%s_%s.pdf", label, prefix))

  ggplot2::ggsave(png_path, plot, width = 12, height = 6, dpi = 300)
  ggplot2::ggsave(pdf_path, plot, width = 12, height = 6, device = cairo_pdf)

  include_metadata(png_path, list(label = label, created_at = as.character(Sys.time())))

  list(
    label = label,
    plot = plot,
    png_path = png_path,
    pdf_path = pdf_path
  )
}

plot_parallel_trends <- function(panel_source,
                                 outcome = "log_pib_agro",
                                 n_pre_periods = 5,
                                 n_post_periods = 5,
                                 normalize = TRUE,
                                 label = NULL,
                                 overwrite = TRUE,
                                 prefix = NULL) {
  df <- resolve_panel_data(panel_source)
  base_label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))
  suffix <- ifelse(is.null(prefix), paste0(outcome, ifelse(normalize, "_normalized", "_raw")), prefix)

  cli::cli_alert_info("Gerando visualização de tendências paralelas ({outcome})...")

  cohorts <- df %>%
    filter(gname > 0) %>%
    distinct(id_microrregiao, gname) %>%
    mutate(
      cohort = case_when(
        gname <= 2007 ~ "Early Adopters\n(2003-2007)",
        gname <= 2012 ~ "Mid Adopters\n(2008-2012)",
        gname <= 2017 ~ "Late Adopters\n(2013-2017)",
        TRUE ~ "Very Late Adopters\n(2018+)"
      )
    ) %>%
    filter(cohort != "Very Late Adopters\n(2018+)") %>%
    mutate(
      cohort = factor(cohort, levels = c(
        "Early Adopters\n(2003-2007)",
        "Mid Adopters\n(2008-2012)",
        "Late Adopters\n(2013-2017)"
      ))
    )

  trends_data <- df %>%
    inner_join(cohorts, by = "id_microrregiao", suffix = c("", ".y")) %>%
    mutate(
      time_to_treat = ano - gname.y,
      outcome_var = .data[[outcome]]
    ) %>%
    filter(time_to_treat >= -n_pre_periods & time_to_treat <= n_post_periods) %>%
    group_by(cohort, time_to_treat) %>%
    summarise(
      mean_outcome = mean(outcome_var, na.rm = TRUE),
      se_outcome = sd(outcome_var, na.rm = TRUE) / sqrt(n()),
      n_obs = n(),
      .groups = "drop"
    )

  if (normalize) {
    normalize_values <- trends_data %>%
      filter(time_to_treat == -1) %>%
      select(cohort, baseline = mean_outcome)

    trends_data <- trends_data %>%
      left_join(normalize_values, by = "cohort") %>%
      mutate(
        mean_outcome = mean_outcome - baseline,
        se_outcome = se_outcome
      )
    y_label <- sprintf("Mudança em %s (Relativo a t=-1)", outcome)
  } else {
    y_label <- sprintf("Log %s", outcome)
  }

  plot <- ggplot(
    trends_data,
    aes(
      x = time_to_treat, y = mean_outcome,
      color = cohort, group = cohort
    )
  ) +
    annotate("rect",
      xmin = 0, xmax = n_post_periods,
      ymin = -Inf, ymax = Inf,
      alpha = 0.1, fill = "gray"
    ) +
    geom_ribbon(aes(
      ymin = mean_outcome - 1.96 * se_outcome,
      ymax = mean_outcome + 1.96 * se_outcome,
      fill = cohort
    ), alpha = 0.2) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    geom_vline(
      xintercept = -0.5, linetype = "solid",
      color = "red", alpha = 0.7, linewidth = 1
    ) +
    {
      if (normalize) geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
    } +
    scale_color_brewer(palette = "Set1", name = "Adoption Cohort") +
    scale_fill_brewer(palette = "Set1", guide = "none") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = paste(
        "Parallel Trends and Treatment Effects:",
        ifelse(outcome == "log_pib_agro", "Agricultural GDP", "Non-Agricultural GDP")
      ),
      subtitle = ifelse(normalize,
        "Evolution by adoption cohort (normalized at t=-1)",
        "Evolution by adoption cohort"
      ),
      x = "Years Relative to Treatment",
      y = y_label,
      caption = "Gray area = post-treatment period. Red line = treatment start. Bands = 95% CI"
    )

  png_path <- raw_artifact_path("visuals", sprintf("%s_parallel_trends_%s.png", base_label, suffix))
  ggplot2::ggsave(png_path, plot, width = 12, height = 8, dpi = 300)
  include_metadata(png_path, list(label = base_label, outcome = outcome, normalize = normalize, created_at = as.character(Sys.time())))

  list(
    label = base_label,
    outcome = outcome,
    plot = plot,
    png_path = png_path,
    data = trends_data
  )
}

plot_parallel_trends_by_gname <- function(panel_source, outcome = "log_pib_agro", label = NULL, overwrite = TRUE) {
  df <- resolve_panel_data(panel_source)
  label <- sanitize_label(ifelse(is.null(label), extract_panel_label(panel_source), label))

  cli::cli_alert_info("Criando visualização de tendências por grupo de tratamento (gname)")

  trend_data <- df %>%
    filter(gname > 0) %>%
    group_by(gname, ano) %>%
    summarise(
      mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
      se_outcome = sd(.data[[outcome]], na.rm = TRUE) / sqrt(n()),
      n_units = n_distinct(id_microrregiao),
      .groups = "drop"
    ) %>%
    mutate(
      post_treatment = ano >= gname,
      group_label = paste0("Tratados em ", gname, " (n=", n_units, ")")
    )

  unique_gnames <- sort(unique(trend_data$gname))
  n_groups <- length(unique_gnames)

  color_palette <- if (n_groups <= 8) {
    RColorBrewer::brewer.pal(max(3, n_groups), "Set1")
  } else {
    viridis::viridis(n_groups)
  }

  plot <- ggplot(trend_data, aes(x = ano, y = mean_outcome, color = factor(gname))) +
    geom_ribbon(
      aes(
        ymin = mean_outcome - 1.96 * se_outcome,
        ymax = mean_outcome + 1.96 * se_outcome,
        fill = factor(gname)
      ),
      alpha = 0.1, color = NA
    ) +
    geom_line(aes(group = gname), linewidth = 1.2, alpha = 0.8) +
    geom_point(aes(shape = post_treatment), size = 2.5, alpha = 0.9) +
    geom_vline(
      data = data.frame(gname = unique_gnames),
      aes(xintercept = gname, color = factor(gname)),
      linetype = "dashed", alpha = 0.5
    ) +
    scale_color_manual(values = color_palette, name = "Ano de\nTratamento") +
    scale_fill_manual(values = color_palette, guide = "none") +
    scale_shape_manual(
      values = c("FALSE" = 16, "TRUE" = 17),
      labels = c("Pré-tratamento", "Pós-tratamento"),
      name = "Período"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11),
      legend.title = element_text(size = 10, face = "bold")
    ) +
    labs(
      title = paste(
        "Tendências por Grupo de Tratamento:",
        ifelse(outcome == "log_pib_agro", "PIB Agropecuário", "PIB Não-Agropecuário")
      ),
      subtitle = "Evolução temporal por ano de adoção das estações meteorológicas",
      x = "Ano",
      y = paste("Log", ifelse(outcome == "log_pib_agro", "PIB Agropecuário", "PIB Não-Agropecuário")),
      caption = "Nota: Linhas verticais tracejadas indicam o ano de tratamento de cada grupo.\nPontos triangulares indicam períodos pós-tratamento."
    )

  png_path <- raw_artifact_path("visuals", sprintf("%s_parallel_trends_by_gname_%s.png", label, gsub("log_", "", outcome)))
  ggplot2::ggsave(png_path, plot, width = 12, height = 8, dpi = 300)
  include_metadata(png_path, list(label = label, outcome = outcome, created_at = as.character(Sys.time())))

  list(
    label = label,
    outcome = outcome,
    plot = plot,
    png_path = png_path,
    data = trend_data
  )
}

