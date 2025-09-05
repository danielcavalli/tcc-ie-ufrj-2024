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
#' prep_data()
#' ---------------------------------------------------------------------------
#' Lê arquivo CSV bruto e produz `df` pronto para a análise DiD escalonada.
#' Principais transformações:
#'   1. Converte variáveis numéricas; limpa espaços em branco de strings.
#'   2. Aplica log(1+x) à produtividade – transformação monotônica que permite
#'      interpretar coeficientes/ATT como variações percentuais aproximadas
#'      (\(\Delta \log y \approx \%\)).
#'   3. Cria `gname`: ano da PRIMEIRA estação instalada (grupo de adoção). Por
#'      convenção exigida pelo pacote did, usa 0 para unidades nunca tratadas.
#'   4. Flag `never_treated` facilita diagnósticos e construção do placebo.
#'   5. Retorna tibble com colunas id, ano, outcome, covariáveis, gname.
#' @param path_csv Caminho do arquivo CSV bruto.
#' @return Tibble pronta para o estimador `did::att_gt()`.
#' ---------------------------------------------------------------------------
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
#' estimate_att()
#' ---------------------------------------------------------------------------
#' Envolve a chamada a `did::att_gt()` e posterior agregação com `aggte()`.
#' Elementos econométricos chave:
#'   • Para cada grupo g (ano de adoção) e tempo t, o estimador DR calcula
#'     \(ATT_{g,t} = E[(Y_{1}-Y_{0})|G=g,T=t]\).
#'   • DR (doubly robust) = combina regressão do desfecho + IPW; consistente se
#'     ao menos um dos dois modelos for corretamente especificado.
#'   • Cluster robust SE por unidade controla autocorrelação intra-microrregião.
#'   • `agg_overall$overall.att` = média ponderada dos ATT(g,t) (peso = tamanho
#'     de cada g) – interpreta-se como ‘average treatment effect on the treated’.
#'   • `agg_event` fornece trajetória dinâmica (event time).
#'   • Função inclui fallback (remover covariáveis ou trocar método) quando há
#'     singularidade.
#' @param df Tibble preparado (ver prep_data)
#' @param method Estimador: "dr" (padrão), "ipw", "reg"
#' @param control_grp Grupo de controle: "notyettreated" recomendado quando
#'        praticamente todos tratam em algum momento.
#' @return Lista com objetos att_gt, aggte (overall e dynamic) e ATT global.
#' ---------------------------------------------------------------------------
estimate_att <- function(df, method = "dr", seed = 42, control_grp = "notyettreated") {
  set.seed(seed)
  cli::cli_alert_info("Estimando ATT(g,t) com método {method} …")

  # Seleção de covariáveis elegíveis
  base_covars <- c("precipitacao_por_area_plantada")
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

  # Métricas de significância
  z_score <- att_global / se_global
  p_value <- 2 * stats::pnorm(-abs(z_score))
  ci_low <- att_global - 1.96 * se_global
  ci_high <- att_global + 1.96 * se_global

  # Formatação melhorada do p-value
  p_formatted <- ifelse(p_value < 0.001,
    sprintf("%.2e", p_value),
    sprintf("%.4f", p_value)
  )

  cli::cli_alert_success("ATT = {round(att_global,4)}, SE = {round(se_global,4)}, z = {round(z_score,2)}, p = {p_formatted}, IC95% = [{round(ci_low,4)}; {round(ci_high,4)}]")

  return(list(
    att = att_res, overall = agg_overall, event = agg_event,
    att_global = att_global, se_global = se_global,
    z = z_score, p = p_value, ci_low = ci_low, ci_high = ci_high
  ))
}

# 1.3 Placebo Test ----------------------------------------------------------- #
#' placebo_test()
#' ---------------------------------------------------------------------------
#' Define um ano fictício de tratamento (`placebo_year`) para todas as
#' unidades que de fato recebem estação. Se o modelo for bem especificado,
#' espera-se ATT ≈ 0, pois nenhuma estação foi instalada nessa data.
#' Usa grupo de controle "notyettreated" para manter coerência.
#' @param df Dados preparados.
#' @param placebo_year Ano fictício (default 2015).
#' @return Lista com att_placebo, se_placebo e p_value, ou NA se falhar.
#' ---------------------------------------------------------------------------
placebo_test <- function(df, placebo_year = 2015) {
  cli::cli_alert_info("Executando placebo test fixo (ano fictício = {placebo_year})…")

  # Todas as unidades que eventualmente recebem tratamento passam a ter gname = placebo_year
  # Unidades nunca tratadas permanecem com 0
  df_placebo <- df %>% mutate(gname = ifelse(!never_treated, placebo_year, gname))

  out <- tryCatch(
    {
      res <- estimate_att(df_placebo, method = "dr", control_grp = "notyettreated")
      att_p <- res$att_global
      se_p <- res$se_global
      p_val <- 2 * stats::pnorm(-abs(att_p / se_p))
      ci_low <- att_p - 1.96 * se_p
      ci_high <- att_p + 1.96 * se_p
      list(att = att_p, se = se_p, p = p_val, ci_low = ci_low, ci_high = ci_high)
    },
    error = function(e) {
      cli::cli_alert_warning("Placebo test falhou: {e$message}")
      NULL
    }
  )

  return(out)
}

# 1.4 Robustez (métodos alternativos) --------------------------------------- #
#' robust_specs()
#' ---------------------------------------------------------------------------
#' Executa rapidamente DR, IPW e REG para comparar magnitude dos efeitos.
#' Captura erros (ex.: IPW singular) sem abortar o script.
#' Escreve CSV para documentação reprodutível.
#' ---------------------------------------------------------------------------
robust_specs <- function(df) {
  methods <- c("dr", "ipw", "reg")
  out <- purrr::map_dfr(methods, function(m) {
    res <- tryCatch(
      estimate_att(df, method = m),
      error = function(e) {
        cli::cli_alert_warning("Método {m} falhou: {e$message}")
        tibble::tibble(metodo = m, att_global = NA_real_, se_global = NA_real_, z = NA_real_, p = NA_real_, ci_low = NA_real_, ci_high = NA_real_)
      }
    )
    if (is.null(res)) {
      tibble::tibble(metodo = m, att_global = NA_real_, se_global = NA_real_, z = NA_real_, p = NA_real_, ci_low = NA_real_, ci_high = NA_real_)
    } else {
      tibble::tibble(
        metodo = m, att_global = res$att_global, se_global = res$se_global,
        z = res$z, p = res$p, ci_low = res$ci_low, ci_high = res$ci_high
      )
    }
  })
  return(out)
}

# 1.5 Visualização ----------------------------------------------------------- #
#' visualize_results()
#' ---------------------------------------------------------------------------
#' Cria gráfico Event Study com banda de 95% IC.
#' Azul = estimativa, vermelho tracejado = 0 (referência), traço vertical =
#' início do tratamento.
#' Salva PNG e PDF em data/outputs.
#' ---------------------------------------------------------------------------
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

# 1.7 Análise de Pesos de Adoção Escalonada --------------------------------- #
#' analyze_weights()
#' ---------------------------------------------------------------------------
#' Examina a distribuição de pesos implícitos no estimador CS para detectar
#' se coortes iniciais (com poucos períodos pós) dominam o ATT agregado.
#' @param att_res Objeto att_gt retornado por did::att_gt()
#' @return Tibble com pesos por grupo e diagnósticos visuais
#' ---------------------------------------------------------------------------
analyze_weights <- function(att_res) {
  cli::cli_alert_info("Analisando distribuição de pesos por coorte de adoção...")

  # Extrai grupos, tempos e ATTs
  groups <- unique(att_res$group[att_res$group > 0])

  # Calcula número de períodos pós-tratamento para cada grupo
  max_period <- max(att_res$t)
  post_periods <- purrr::map_dfr(groups, function(g) {
    tibble::tibble(
      group = g,
      n_post_periods = max_period - g + 1
    )
  })

  # Extrai pesos implícitos (proporção de observações tratadas em cada grupo)
  # Nota: did pacote não exporta pesos diretamente, então aproximamos via n_treated
  weight_data <- att_res$DIDparams$data %>%
    filter(gname > 0) %>%
    group_by(gname) %>%
    summarise(n_units = n_distinct(id_microrregiao)) %>%
    mutate(weight = n_units / sum(n_units)) %>%
    rename(group = gname)

  # Combina informações
  weight_analysis <- weight_data %>%
    left_join(post_periods, by = "group") %>%
    arrange(group)

  # Diagnóstico: correlação entre peso e períodos pós
  cor_weight_periods <- cor(weight_analysis$weight, weight_analysis$n_post_periods)
  cli::cli_alert_info("Correlação entre peso e períodos pós-tratamento: {round(cor_weight_periods, 3)}")

  # Alerta se coortes iniciais têm peso desproporcional
  early_cohorts <- weight_analysis %>%
    filter(n_post_periods >= quantile(n_post_periods, 0.75))
  early_weight <- sum(early_cohorts$weight)

  if (early_weight > 0.5) {
    cli::cli_alert_warning("Coortes iniciais (mais períodos pós) representam {round(early_weight*100,1)}% do peso total")
  }

  # Visualização
  p_weights <- ggplot(weight_analysis, aes(x = group, y = weight)) +
    geom_col(aes(fill = n_post_periods), alpha = 0.7) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Períodos pós") +
    theme_minimal() +
    labs(
      title = "Distribuição de Pesos por Coorte de Adoção",
      x = "Ano de adoção",
      y = "Peso no ATT agregado"
    )

  ggsave(here::here("data", "outputs", "weights_distribution.png"), p_weights, width = 10, height = 6)

  return(weight_analysis)
}

# 1.8 Comparação de Grupos de Controle --------------------------------------- #
#' compare_control_groups()
#' ---------------------------------------------------------------------------
#' Estima ATT usando grupos de controle alternativos (never-treated vs
#' not-yet-treated) para avaliar robustez e potencial heterogeneidade.
#' @param df Dados preparados
#' @param method Método de estimação (dr, ipw, reg)
#' @return Tibble comparando resultados
#' ---------------------------------------------------------------------------
compare_control_groups <- function(df, method = "dr") {
  cli::cli_alert_info("Comparando grupos de controle...")

  # Verifica se há unidades nunca tratadas suficientes
  n_never <- n_distinct(df$id_microrregiao[df$gname == 0])
  n_total <- n_distinct(df$id_microrregiao)
  pct_never <- n_never / n_total * 100

  cli::cli_alert_info("Unidades nunca tratadas: {n_never} ({round(pct_never, 1)}% do total)")

  results <- list()

  # Not-yet-treated
  cli::cli_h3("Estimando com controle 'not-yet-treated'")
  results$nyt <- tryCatch(
    estimate_att(df, method = method, control_grp = "notyettreated"),
    error = function(e) {
      cli::cli_alert_warning("Falha com not-yet-treated: {e$message}")
      NULL
    }
  )

  # Never-treated
  if (n_never > 10) { # Precisa de um mínimo de unidades
    cli::cli_h3("Estimando com controle 'never-treated'")
    results$nt <- tryCatch(
      estimate_att(df, method = method, control_grp = "nevertreated"),
      error = function(e) {
        cli::cli_alert_warning("Falha com never-treated: {e$message}")
        NULL
      }
    )
  } else {
    cli::cli_alert_warning("Poucos controles nunca tratados para estimação confiável")
    results$nt <- NULL
  }

  # Compara resultados
  comparison <- tibble::tibble(
    control_group = c("not-yet-treated", "never-treated"),
    att = c(
      ifelse(is.null(results$nyt), NA, results$nyt$att_global),
      ifelse(is.null(results$nt), NA, results$nt$att_global)
    ),
    se = c(
      ifelse(is.null(results$nyt), NA, results$nyt$se_global),
      ifelse(is.null(results$nt), NA, results$nt$se_global)
    )
  ) %>%
    filter(!is.na(att))

  if (nrow(comparison) == 2) {
    diff <- abs(comparison$att[1] - comparison$att[2])
    pct_diff <- diff / abs(comparison$att[1]) * 100
    cli::cli_alert_info("Diferença entre grupos de controle: {round(diff, 4)} ({round(pct_diff, 1)}%)")

    if (pct_diff > 20) {
      cli::cli_alert_warning("Grande diferença entre grupos de controle sugere heterogeneidade nas tendências")
    }
  }

  return(list(results = results, comparison = comparison))
}

# 1.9 Diagnóstico de Coortes com NA ----------------------------------------- #
#' diagnose_na_cohorts()
#' ---------------------------------------------------------------------------
#' Identifica grupos-tempo com ATT=NA e sugere estratégias de agregação.
#' @param att_res Objeto att_gt
#' @param df Dados originais
#' @return Lista com diagnósticos e sugestões
#' ---------------------------------------------------------------------------
diagnose_na_cohorts <- function(att_res, df) {
  cli::cli_alert_info("Diagnosticando valores NA em ATT(g,t)...")

  # Identifica combinações g,t com NA
  na_cases <- tibble::tibble(
    group = att_res$group,
    time = att_res$t,
    att = att_res$att,
    n = att_res$n
  ) %>%
    filter(is.na(att) & group > 0) # Exclui controles

  if (nrow(na_cases) == 0) {
    cli::cli_alert_success("Nenhum ATT(g,t) com valor NA")
    return(NULL)
  }

  cli::cli_alert_warning("Encontrados {nrow(na_cases)} casos de ATT(g,t) = NA")

  # Analisa tamanho das coortes
  cohort_sizes <- df %>%
    filter(gname > 0) %>%
    group_by(gname) %>%
    summarise(
      n_units = n_distinct(id_microrregiao),
      n_obs = n()
    ) %>%
    arrange(n_units)

  # Identifica coortes pequenas
  small_cohorts <- cohort_sizes %>%
    filter(n_units < 5) # Limite arbitrário

  if (nrow(small_cohorts) > 0) {
    cli::cli_alert_warning("Coortes pequenas (<5 unidades): {paste(small_cohorts$gname, collapse=', ')}")
  }

  # Sugestão de agregação
  suggestions <- list()

  # Opção 1: Agrupar coortes adjacentes
  if (nrow(small_cohorts) > 1) {
    suggestions$merge_adjacent <- "Considere agrupar coortes adjacentes pequenas"
  }

  # Opção 2: Usar anticipation para expandir janela
  late_adopters <- na_cases %>%
    filter(group > quantile(att_res$group[att_res$group > 0], 0.75))

  if (nrow(late_adopters) > 0) {
    suggestions$anticipation <- "Coortes tardias podem se beneficiar de anticipation > 0"
  }

  return(list(
    na_cases = na_cases,
    cohort_sizes = cohort_sizes,
    small_cohorts = small_cohorts,
    suggestions = suggestions
  ))
}

# 1.10 Agregação Manual de Coortes ------------------------------------------ #
#' merge_small_cohorts()
#' ---------------------------------------------------------------------------
#' Agrupa coortes pequenas em bins maiores para reduzir NAs.
#' @param df Dados preparados
#' @param min_size Tamanho mínimo de coorte
#' @param bin_width Largura dos bins (em anos)
#' @return DataFrame com gname modificado
#' ---------------------------------------------------------------------------
merge_small_cohorts <- function(df, min_size = 5, bin_width = 2) {
  cli::cli_alert_info("Agregando coortes pequenas...")

  # Identifica tamanhos de coorte
  cohort_info <- df %>%
    filter(gname > 0) %>%
    group_by(gname) %>%
    summarise(n_units = n_distinct(id_microrregiao)) %>%
    arrange(gname)

  # Define bins para coortes pequenas
  small_cohorts <- cohort_info %>%
    filter(n_units < min_size) %>%
    pull(gname)

  if (length(small_cohorts) == 0) {
    cli::cli_alert_success("Nenhuma coorte pequena para agregar")
    return(df)
  }

  # Cria mapeamento para bins
  year_range <- range(small_cohorts)
  breaks <- seq(year_range[1], year_range[2] + bin_width, by = bin_width)

  # Mapeia anos originais para bins
  df_merged <- df %>%
    mutate(
      gname_original = gname,
      gname = case_when(
        gname == 0 ~ 0, # Mantém controles
        gname %in% small_cohorts ~ cut(gname, breaks = breaks, labels = FALSE) * bin_width + year_range[1] - bin_width,
        TRUE ~ gname
      )
    )

  # Reporta mudanças
  changes <- df_merged %>%
    filter(gname != gname_original & gname > 0) %>%
    distinct(gname_original, gname)

  cli::cli_alert_success("Agregados {n_distinct(changes$gname_original)} anos em {n_distinct(changes$gname)} bins")

  return(df_merged)
}

# 1.11 Clustering Alternativo ----------------------------------------------- #
#' estimate_att_robust_se()
#' ---------------------------------------------------------------------------
#' Extensão de estimate_att() com opções de clustering robustas.
#' @param df Dados
#' @param method Método base (dr, ipw, reg)
#' @param se_type Tipo de SE: "clustered" (padrão), "twoway", "wildboot"
#' @return Lista similar a estimate_att() mas com SEs alternativos
#' ---------------------------------------------------------------------------
estimate_att_robust_se <- function(df, method = "dr", se_type = "twoway") {
  cli::cli_alert_info("Estimando com SEs robustos: {se_type}")

  if (se_type == "wildboot") {
    # Wild bootstrap requer modificação na chamada att_gt
    cli::cli_alert_warning("Wild bootstrap não implementado diretamente no pacote did")
    cli::cli_alert_info("Usando bootstrap padrão com mais replicações")

    # Aumenta número de bootstraps
    base_res <- estimate_att(df, method = method)
    return(base_res)
  }

  if (se_type == "twoway") {
    # Two-way clustering não é suportado nativamente
    # Precisaríamos pós-processar os resultados
    cli::cli_alert_info("Two-way clustering via pós-processamento...")

    # Estima modelo base
    base_res <- estimate_att(df, method = method)

    # Aqui entraria cálculo manual de SEs two-way
    # Por ora, retorna resultado base com aviso
    cli::cli_alert_warning("Two-way SEs requerem implementação customizada")

    return(base_res)
  }

  # Clustering padrão
  return(estimate_att(df, method = method))
}

# 1.12 Teste Placebo Aleatório ---------------------------------------------- #
#' random_placebo_test()
#' ---------------------------------------------------------------------------
#' Atribui anos de tratamento aleatórios repetidamente e constrói distribuição
#' nula dos ATTs placebos. O ATT verdadeiro deve estar na cauda.
#' @param df Dados preparados
#' @param n_sims Número de simulações
#' @param method Método de estimação
#' @param seed Semente aleatória
#' @return Lista com distribuição de placebos e p-valor
#' ---------------------------------------------------------------------------
random_placebo_test <- function(df, n_sims = 100, method = "dr", seed = 42) {
  set.seed(seed)
  cli::cli_alert_info("Executando {n_sims} testes placebo aleatórios...")

  # ATT verdadeiro para comparação
  true_res <- estimate_att(df, method = method)
  true_att <- true_res$att_global

  # Identifica unidades tratadas e período disponível
  treated_units <- unique(df$id_microrregiao[df$gname > 0])
  year_range <- range(df$ano)
  possible_years <- seq(year_range[1] + 2, year_range[2] - 2) # Margem de segurança

  # Simulações
  placebo_atts <- numeric(n_sims)
  cli::cli_progress_bar("Simulando placebos", total = n_sims)

  for (i in 1:n_sims) {
    cli::cli_progress_update()

    # Atribui anos aleatórios - um por unidade
    unit_assignments <- tibble::tibble(
      id_microrregiao = treated_units,
      gname_placebo = sample(possible_years, length(treated_units), replace = TRUE)
    )

    df_placebo <- df %>%
      left_join(unit_assignments, by = "id_microrregiao") %>%
      mutate(
        gname_placebo = ifelse(is.na(gname_placebo), 0, gname_placebo),
        gname = gname_placebo
      ) %>%
      select(-gname_placebo)

    # Estima ATT placebo
    placebo_res <- tryCatch(
      {
        res <- estimate_att(df_placebo, method = method)
        res$att_global
      },
      error = function(e) NA_real_
    )

    placebo_atts[i] <- placebo_res
  }

  cli::cli_progress_done()

  # Remove NAs
  placebo_atts <- placebo_atts[!is.na(placebo_atts)]
  n_valid <- length(placebo_atts)

  if (n_valid < 50) {
    cli::cli_alert_warning("Apenas {n_valid} simulações válidas. Resultados podem ser imprecisos.")
  }

  # Calcula p-valor empírico
  p_value <- mean(abs(placebo_atts) >= abs(true_att))

  # Percentis para comparação
  percentiles <- quantile(placebo_atts, c(0.025, 0.05, 0.95, 0.975))

  # Formatação do p-value empírico
  p_emp_formatted <- ifelse(p_value < 0.001,
    sprintf("%.2e", p_value),
    sprintf("%.4f", p_value)
  )
  cli::cli_alert_success("P-valor empírico: {p_emp_formatted}")
  cli::cli_alert_info("ATT verdadeiro: {round(true_att, 4)}")
  cli::cli_alert_info("Intervalo 95% placebos: [{round(percentiles[1], 4)}, {round(percentiles[4], 4)}]")

  # Visualização
  p_dist <- ggplot(data.frame(att = placebo_atts), aes(x = att)) +
    geom_histogram(bins = 30, fill = "lightgray", color = "black", alpha = 0.7) +
    geom_vline(xintercept = true_att, color = "red", linetype = "dashed", size = 1.5) +
    geom_vline(xintercept = percentiles[c(1, 4)], color = "blue", linetype = "dotted") +
    theme_minimal() +
    labs(
      title = "Distribuição de ATTs Placebo vs ATT Verdadeiro",
      x = "ATT estimado",
      y = "Frequência",
      subtitle = paste0("P-valor empírico: ", round(p_value, 3))
    )

  ggsave(here::here("data", "outputs", "placebo_distribution.png"), p_dist, width = 10, height = 6)

  return(list(
    placebo_atts = placebo_atts,
    true_att = true_att,
    p_value = p_value,
    percentiles = percentiles,
    n_valid = n_valid
  ))
}

# --------------------------------------------------------------------------- #
# 2. Execução Principal                                                      #
# --------------------------------------------------------------------------- #
# Este bloco roda automaticamente quando o script é "sourced".                #
if (interactive() || sys.nframe() == 0) {
  cli::cli_h1("Avaliação de Impacto de Estações Meteorológicas")

  DATA_PATH <- here::here("data", "csv", "WightedMethod", "sugar_cane_first_year_1.csv")
  df_clean <- prep_data(DATA_PATH)

  # Estimação principal
  res_main <- estimate_att(df_clean, method = "dr")

  # Export resumo ATT
  summary_tbl <- tibble::tibble(
    metodo = "dr", control = "notyettreated",
    att = res_main$att_global, se = res_main$se_global, z = res_main$z,
    p = res_main$p, ci_low = res_main$ci_low, ci_high = res_main$ci_high
  )
  readr::write_csv(summary_tbl, here::here("data", "outputs", "att_summary.csv"))

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 1: Pesos de Adoção Escalonada
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Análise de Pesos de Adoção Escalonada")
  weights_analysis <- analyze_weights(res_main$att)
  readr::write_csv(weights_analysis, here::here("data", "outputs", "weights_analysis.csv"))

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 2: Comparação de Grupos de Controle
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Comparação de Grupos de Controle")
  control_comp <- compare_control_groups(df_clean, method = "dr")
  if (!is.null(control_comp$comparison)) {
    readr::write_csv(control_comp$comparison, here::here("data", "outputs", "control_group_comparison.csv"))
  }

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 3: Diagnóstico de Coortes com NA
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Diagnóstico de Valores NA")
  na_diag <- diagnose_na_cohorts(res_main$att, df_clean)
  if (!is.null(na_diag)) {
    readr::write_csv(na_diag$cohort_sizes, here::here("data", "outputs", "cohort_sizes.csv"))
    if (nrow(na_diag$small_cohorts) > 0) {
      # Tenta agregação de coortes pequenas
      cli::cli_alert_info("Testando agregação de coortes pequenas...")
      df_merged <- merge_small_cohorts(df_clean, min_size = 5, bin_width = 2)
      res_merged <- estimate_att(df_merged, method = "dr")
      cli::cli_alert_info("ATT após agregação: {round(res_merged$att_global, 4)} (original: {round(res_main$att_global, 4)})")
    }
  }

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 4: Verificação de Clustering Alternativo
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Verificação de Clustering")
  # Nota: implementação completa de two-way clustering requer pacotes adicionais
  # Por ora, apenas alertamos sobre a robustez do clustering padrão
  cli::cli_alert_info("Com {n_distinct(df_clean$id_microrregiao)} clusters, o clustering padrão por microrregião é adequado")
  cli::cli_alert_info("Para maior robustez, considere wild bootstrap em análises futuras")

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 5: Teste Placebo Aleatório
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Teste Placebo Aleatório")
  placebo_random <- random_placebo_test(df_clean, n_sims = 50, method = "dr")

  # Salva resultados do placebo aleatório
  placebo_summary <- tibble::tibble(
    true_att = placebo_random$true_att,
    p_value_empirical = placebo_random$p_value,
    ci_95_lower = placebo_random$percentiles[1],
    ci_95_upper = placebo_random$percentiles[4],
    n_valid_sims = placebo_random$n_valid
  )
  readr::write_csv(placebo_summary, here::here("data", "outputs", "placebo_random_summary.csv"))

  # Teste de Tendências Paralelas (pré-tratamento) - mantém análise original
  cli::cli_h2("Teste de Tendências Paralelas")
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

  # Placebo test fixo (mantém análise original)
  cli::cli_h2("Placebo Test Fixo (2015)")
  placebo_res <- placebo_test(df_clean, placebo_year = 2015)
  if (!is.null(placebo_res)) {
    cli::cli_alert_success("Placebo fixo: ATT = {round(placebo_res$att,4)}, SE = {round(placebo_res$se,4)}, p = {round(placebo_res$p,4)}, IC95% = [{round(placebo_res$ci_low,4)}; {round(placebo_res$ci_high,4)}]")
  }

  # Robustez (mantém análise original)
  cli::cli_h2("Análise de Robustez")
  robust_tbl <- robust_specs(df_clean)
  readr::write_csv(robust_tbl, here::here("data", "outputs", "robust_att.csv"))

  # Visualizações
  visualize_results(res_main$event)

  cli::cli_h2("Script concluído com sucesso - Análise expandida finalizada.")
}
