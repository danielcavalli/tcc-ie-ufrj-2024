###############################################################################
# Avaliação do Impacto da Implantação de Novas Estações Meteorológicas         #
# na Produtividade da Cana-de-Açúcar no Brasil                                 #
# Autor   : Daniel Cavalli                                                     #
# Última modificação: 2024-12-08                                               #
#                                                                              #
# Objetivo:                                                                    #
#   Estimar o efeito causal da instalação de estações de monitoramento         #
#   meteorológico sobre a produtividade agrícola utilizando o framework        #
#   Difference-in-Differences (DiD) com adoção escalonada de Callaway &        #
#   Sant'Anna (2020).                                                          #
#                                                                              #
#   O script está organizado em funções para facilitar reprodutibilidade,      #
#   testes de robustez e uso modular em notebooks.                             #
###############################################################################

# --------------------------------------------------------------------------- #
# 0. Carregamento de Pacotes                                                  #
# --------------------------------------------------------------------------- #
# Justificativa: manter dependências explícitas e garantir que versões        #
# coerentes sejam registradas no renv.lock; instalação automática aumenta     #
# portabilidade do código.                                                    #

pkgs <- c("did", "dplyr", "ggplot2", "readr", "here", "cli", "purrr", "tibble")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# --------------------------------------------------------------------------- #
# 1. Funções Auxiliares                                                       #
# --------------------------------------------------------------------------- #

# 1.1 Preparação de dados ---------------------------------------------------- #
#   • Garantir que variáveis estejam no formato correto evita vieses de        #
#     medição.                                                                 #
#   • Transformação logarítmica reduz assimetria e permite interpretação       #
#     dos coeficientes como variações percentuais aproximadas.                  #
#   • Definir corretamente gname (= ano de tratamento) e identificar             #
#     unidades never-treated (gname = Inf) é crucial para o estimador de DiD   #
#     escalonado, pois o grupo de controle de referência será "never-treated". #
prep_data <- function(path_csv) {
  cli::cli_alert_info("Lendo dados de {path_csv} …")

  df <- readr::read_csv(path_csv, show_col_types = FALSE) %>%
    # Conversão explícita para evitar fatores escondidos
    mutate(across(where(is.character), ~ trimws(.x))) %>%
    mutate(across(c(
      produtividade, total_area_plantada, precipitacao_por_area_plantada,
      qtd_estacoes_ativas
    ), as.numeric)) %>%
    # Log(1+x) evita -Inf quando produtividade = 0
    mutate(log_produtividade = log1p(produtividade)) %>%
    # Definição do ano de tratamento (gname); 0 para nunca tratados (requerido pelo pacote did)
    group_by(id_microrregiao) %>%
    mutate(gname = ifelse(is.na(primeiro_ano_tratamento), 0, primeiro_ano_tratamento)) %>%
    ungroup() %>%
    # Indicador de nunca tratado (diagnóstico)
    mutate(never_treated = gname == 0) %>%
    rename(treated = tratado)

  cli::cli_alert_success("Dados carregados: {nrow(df)} observações, {n_distinct(df$id_microrregiao)} unidades.")
  return(df)
}

# 1.2 Estimação do ATT ------------------------------------------------------- #
# Econometric rationale:                                                       #
#   • Estimador Doubly Robust (DR) combina regressão e IPW → consiste em      #
#     manter consistência se ao menos um modelo estiver corretamente          #
#     especificado.                                                           #
#   • Erros-padrão clusterizados por unidade controlam correlação serial.     #
#   • Bootstrap melhora inferência para amostras moderadas.                   #
#   • Guardar influence function (IF) permite diagnósticos adicionais.        #
estimate_att <- function(df, method = "dr", seed = 42, control_grp = "notyettreated") {
  set.seed(seed)
  cli::cli_alert_info("Estimando ATT(g,t) com método {method} …")

  # Seleção de covariáveis elegíveis
  base_covars <- c("qtd_estacoes_ativas", "precipitacao_por_area_plantada")
  covars_ok <- check_covariates(df, base_covars)

  # Construção de fórmula
  xform <- if (length(covars_ok) == 0) {
    ~1
  } else {
    as.formula(paste("~", paste(covars_ok, collapse = "+")))
  }

  # Função auxiliar para chamada segura
  safe_att <- function(met, form) {
    cli::cli_alert_info("Tentando estimador {met} com fórmula: {deparse(form)}")
    did::att_gt(
      yname = "log_produtividade",
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

  # Tentativa 1: método solicitado com covariáveis filtradas
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

  # Agregação Global e Dinâmica (event study)
  # na.rm = TRUE ignora possíveis ATT(g,t) não identificados (ex.: grupos pequenos)
  agg_overall <- aggte(att_res, type = "group", na.rm = TRUE)
  agg_event <- aggte(att_res, type = "dynamic", na.rm = TRUE)

  if (any(is.na(att_res$att))) {
    cli::cli_alert_warning("Alguns ATT(g,t) não foram estimados (NA). Eles foram removidos na agregação.")
  }

  # Persistência dos objetos para reuso
  dir_out <- here::here("data", "outputs")
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  saveRDS(att_res, file = file.path(dir_out, paste0("att_results_", method, ".rds")))
  saveRDS(agg_overall, file = file.path(dir_out, paste0("agg_overall_", method, ".rds")))
  saveRDS(agg_event, file = file.path(dir_out, paste0("agg_event_", method, ".rds")))

  # Efeito médio global da versão atual do pacote did
  att_global <- agg_overall$overall.att
  se_global <- agg_overall$overall.se

  cli::cli_alert_success("Estimativa concluída. ATT médio global: {round(att_global, 4)}")
  return(list(att = att_res, overall = agg_overall, event = agg_event, att_global = att_global, se_global = se_global))
}

# 1.3 Placebo Test ----------------------------------------------------------- #
# Econometric rationale: atribuir falsamente o tratamento antes do período     #
# real avalia se há efeitos espúrios, reforçando a validade da estratégia.    #
placebo_test <- function(df, placebo_year = 2015) {
  cli::cli_alert_info("Executando placebo test fixo (ano fictício = {placebo_year})…")

  # Todas as unidades que eventualmente recebem tratamento passam a ter gname = placebo_year
  # Unidades nunca tratadas permanecem com 0
  df_placebo <- df %>% mutate(gname = ifelse(!never_treated, placebo_year, gname))

  out <- tryCatch(
    {
      res <- estimate_att(df_placebo, method = "dr", control_grp = "notyettreated")
      res$att_global
    },
    error = function(e) {
      cli::cli_alert_warning("Placebo test falhou: {e$message}")
      NA_real_
    }
  )

  return(out)
}

# 1.4 Robustez (métodos alternativos) --------------------------------------- #
robust_specs <- function(df) {
  methods <- c("dr", "ipw", "reg")
  out <- purrr::map_dfr(methods, function(m) {
    res <- tryCatch(
      estimate_att(df, method = m),
      error = function(e) {
        cli::cli_alert_warning("Método {m} falhou: {e$message}")
        NULL
      }
    )
    if (is.null(res)) {
      tibble::tibble(metodo = m, att_global = NA_real_, se_global = NA_real_)
    } else {
      tibble::tibble(metodo = m, att_global = res$att_global, se_global = res$se_global)
    }
  })
  return(out)
}

# 1.5 Visualização ----------------------------------------------------------- #
visualize_results <- function(att_event, output_prefix = "event_study") {
  cli::cli_alert_info("Gerando gráficos…")
  event_df <- tibble::tibble(
    time_relative = att_event$egt,
    att = att_event$att.egt,
    se = att_event$se.egt
  )

  p_event <- ggplot(event_df, aes(x = time_relative, y = att)) +
    geom_ribbon(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), fill = "skyblue", alpha = 0.3) +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "navy") +
    geom_line(color = "navy", size = 1) +
    geom_point(color = "navy", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Event Study: Impacto da Estação ao Longo do Tempo",
      x = "Períodos em relação ao tratamento",
      y = "Efeito estimado (log-produtividade)"
    )

  dir_out <- here::here("data", "outputs")
  ggsave(file.path(dir_out, paste0(output_prefix, ".png")), p_event, width = 12, height = 6)
  ggsave(file.path(dir_out, paste0(output_prefix, ".pdf")), p_event, width = 12, height = 6)
}

# 1.6 Diagnóstico de Covariáveis ------------------------------------------- #
#   • Verifica variância no pré-tratamento e colinearidade.                   #
#   • Retorna subconjunto de covariáveis elegíveis para DR.                   #
check_covariates <- function(df, covars) {
  cli::cli_alert_info("Verificando colinearidade/variância das covariáveis…")

  # Filtra período pré-tratamento
  df_pre <- df %>% filter((gname == 0 & TRUE) | ano < gname)

  # Variância de cada covariável
  var_tbl <- purrr::map_dfr(covars, function(v) {
    tibble::tibble(
      var = v,
      var_pre = var(df_pre[[v]], na.rm = TRUE)
    )
  })

  # Mantém apenas variância > 0
  valid_covars <- var_tbl %>%
    filter(var_pre > 0) %>%
    pull(var)

  # Colinearidade: constrói matriz incremental
  keep <- c()
  for (v in valid_covars) {
    test_set <- c(keep, v)
    X <- model.matrix(as.formula(paste("~", paste(test_set, collapse = "+"))), data = df_pre)
    rk <- qr(X)$rank
    if (rk == ncol(X)) {
      keep <- test_set
    } else {
      cli::cli_alert_warning("Removendo covariável '{v}' por colinearidade no pré-tratamento.")
    }
  }

  cli::cli_alert_success("Covariáveis elegíveis: {paste(keep, collapse = ', ')}")

  # Exporta diagnóstico
  diag_dir <- here::here("data", "outputs")
  if (!dir.exists(diag_dir)) dir.create(diag_dir, recursive = TRUE)
  diag_path <- file.path(diag_dir, "covar_diag.csv")
  readr::write_csv(var_tbl, diag_path)

  return(keep)
}

# --------------------------------------------------------------------------- #
# 2. Execução Principal                                                      #
# --------------------------------------------------------------------------- #
# Este bloco roda automaticamente quando o script é "sourced".                #
if (interactive() || sys.nframe() == 0) {
  cli::cli_h1("Avaliação de Impacto de Estações Meteorológicas")

  DATA_PATH <- here::here("data", "csv", "WightedMethod", "sugar_cane_treated.csv")
  df_clean <- prep_data(DATA_PATH)

  # Estimação principal
  res_main <- estimate_att(df_clean, method = "dr")

  # Teste de Tendências Paralelas (pré-tratamento)
  cli::cli_alert_info("Testando tendências paralelas…")
  att_df <- tibble::tibble(
    group = res_main$att$group,
    time  = res_main$att$t,
    att   = res_main$att$att
  )
  pre_df <- dplyr::filter(att_df, time < group)
  pre_mean <- mean(pre_df$att, na.rm = TRUE)
  pre_se <- sd(pre_df$att, na.rm = TRUE) / sqrt(nrow(pre_df))
  t_stat <- pre_mean / pre_se
  p_val <- 2 * stats::pt(-abs(t_stat), df = nrow(pre_df) - 1)
  cli::cli_alert_success("Teste manual de tendências paralelas: média PRE = {round(pre_mean, 4)}, p-valor = {round(p_val, 4)}")

  # Placebo test
  placebo_att <- placebo_test(df_clean, placebo_year = 2015)
  if (!is.na(placebo_att)) {
    cli::cli_alert_success("ATT placebo (esperado ≈ 0): {round(placebo_att, 4)}")
  }

  # Robustez
  robust_tbl <- robust_specs(df_clean)
  readr::write_csv(robust_tbl, here::here("data", "outputs", "robust_att.csv"))

  # Visualizações
  visualize_results(res_main$event)

  cli::cli_h2("Script concluído com sucesso.")
}
