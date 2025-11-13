# Stage module: data preparation utilities
library(dplyr)
library(magrittr)

source(here::here("rscripts", "pipeline", "io_utils.R"))

prep_data <- function(path_csv, label = NULL, overwrite = TRUE) {
  cli::cli_h2("Preparando dados (etapa de prep)")
  cli::cli_alert_info("Lendo dados de {path_csv} …")

  label <- sanitize_label(if (is.null(label)) tools::file_path_sans_ext(basename(path_csv)) else label)

  df_mun <- readr::read_csv(path_csv, show_col_types = FALSE) %>%
    mutate(across(where(is.character), ~ trimws(.x)))

  cli::cli_alert_info("Estrutura dos dados (nível município):")
  cli::cli_alert_info("  - Período: {min(df_mun$ano)} a {max(df_mun$ano)}")
  cli::cli_alert_info("  - Municípios: {n_distinct(df_mun$id_municipio)}")
  cli::cli_alert_info("  - Microrregiões: {n_distinct(df_mun$id_microrregiao)}")

  df <- df_mun %>%
    group_by(id_microrregiao, sigla_uf, ano) %>%
    summarise(
      populacao_total = sum(populacao_total, na.rm = TRUE),
      pib_total = sum(pib_total, na.rm = TRUE),
      pib_agropecuario = sum(pib_agropecuario, na.rm = TRUE),
      area_total_ha = sum(area_total_ha, na.rm = TRUE),
      area_cana = sum(area_cana, na.rm = TRUE),
      primeiro_ano_tratamento = {
        anos_tratamento <- primeiro_ano_tratamento[primeiro_ano_tratamento > 0 & !is.na(primeiro_ano_tratamento)]
        if (length(anos_tratamento) > 0) min(anos_tratamento) else NA_real_
      },
      tratado = as.integer(any(tratado == 1)),
      .groups = "drop"
    ) %>%
    mutate(
      pib_per_capita = ifelse(populacao_total > 0, pib_total / populacao_total, NA_real_),
      area_plantada = area_cana
    )

  cli::cli_alert_info("Dados agregados para microrregião:")
  cli::cli_alert_info("  - Período: {min(df$ano)} a {max(df$ano)}")
  cli::cli_alert_info("  - Microrregiões: {n_distinct(df$id_microrregiao)}")

  pre_check <- df %>%
    filter(!is.na(primeiro_ano_tratamento)) %>%
    group_by(id_microrregiao) %>%
    summarise(
      tem_pre = any(ano < primeiro_ano_tratamento[1]),
      anos_pre = sum(ano < primeiro_ano_tratamento[1]),
      .groups = "drop"
    )

  cli::cli_alert_success("Microrregiões com dados pré-tratamento: {sum(pre_check$tem_pre)} de {nrow(pre_check)}")

  densidade_uf <- df %>%
    group_by(sigla_uf, ano) %>%
    summarise(
      n_estacoes_uf = sum(tratado == 1, na.rm = TRUE),
      n_microregioes_uf = n_distinct(id_microrregiao),
      densidade_estacoes_uf = n_estacoes_uf / n_microregioes_uf,
      .groups = "drop"
    )

  df <- df %>%
    left_join(densidade_uf, by = c("sigla_uf", "ano")) %>%
    mutate(
      gname = ifelse(is.na(primeiro_ano_tratamento), 0, primeiro_ano_tratamento),
      never_treated = is.na(primeiro_ano_tratamento) | gname == 0,
      treated = tratado,
      log_area_plantada = log1p(area_plantada),
      log_populacao = log1p(populacao_total),
      log_pib_per_capita = log1p(pib_per_capita),
      log_pib_agro = log1p(pib_agropecuario),
      pib_nao_agro = ifelse(is.na(pib_total) | is.na(pib_agropecuario), NA, pib_total - pib_agropecuario),
      log_pib_nao_agro = log1p(pib_nao_agro),
      prop_pib_agro = ifelse(is.na(pib_total) | pib_total == 0, NA, pib_agropecuario / pib_total),
      log_densidade_estacoes_uf = log1p(densidade_estacoes_uf)
    ) %>%
    filter(!is.na(pib_agropecuario), !is.na(area_plantada))

  summary_tbl <- tibble::tibble(
    label = label,
    source_csv = path_csv,
    n_obs_municipio = nrow(df_mun),
    n_obs_microrregiao = nrow(df),
    n_municipios = n_distinct(df_mun$id_municipio),
    n_microrregioes = n_distinct(df$id_microrregiao),
    periodo_inicio = min(df$ano),
    periodo_fim = max(df$ano),
    created_at = as.character(Sys.time())
  )

  micro_path <- write_artifact(df, "prep", sprintf("%s_df_microrregiao.rds", label), overwrite = overwrite)
  mun_path <- write_artifact(df_mun, "prep", sprintf("%s_df_municipio.rds", label), overwrite = overwrite)
  summary_path <- write_artifact(summary_tbl, "prep", sprintf("%s_prep_summary.csv", label), overwrite = overwrite)

  cli::cli_alert_success("Dados preparados e salvos em {dirname(micro_path)}")

  list(
    label = label,
    microrregiao_path = micro_path,
    municipio_path = mun_path,
    summary_path = summary_path,
    df_microrregiao = df,
    df_municipio = df_mun
  )
}

