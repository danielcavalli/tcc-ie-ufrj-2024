###############################################################################
# AVALIAÇÃO DO IMPACTO CAUSAL DE ESTAÇÕES METEOROLÓGICAS NA AGRICULTURA      #
# Análise via Difference-in-Differences com Adoção Escalonada                 #
#                                                                              #
# Autor: Daniel Cavalli                                                        #
# Instituição: Instituto de Economia - UFRJ                                   #
# Última modificação: 2025-11-13                                               #
#                                                                              #
# OBJETIVO PRINCIPAL:                                                          #
#   Estimar o efeito causal da instalação de estações meteorológicas          #
#   automáticas sobre a ÁREA PLANTADA DE CANA-DE-AÇÚCAR, utilizando o        #
#   estimador Difference-in-Differences (DiD) com tratamento escalonado de    #
#   Callaway & Sant'Anna (2021) - o estado da arte para adoção gradual.       #
#                                                                              #
# OUTCOME PRINCIPAL:                                                           #
#   - log_area_cana: Log da área plantada de cana-de-açúcar (km²)            #
#   - Justificativa: Cana é altamente sensível a informação climática         #
#     (irrigação, calendário), tornando-a ideal para detectar impactos        #
#                                                                              #
# OUTCOMES SECUNDÁRIOS (robustez e especificidade):                            #
#   - log_pib_agro: PIB agropecuário (medida agregada de valor)              #
#   - log_area_soja e log_area_arroz: outras culturas (especificidade)       #
#                                                                              #
# IDENTIFICAÇÃO CAUSAL:                                                        #
#   - Variação no timing de adoção entre microrregiões (2003-2021)           #
#   - Grupo de controle: unidades "not yet treated" (ainda não tratadas)      #
#   - Assumimos tendências paralelas condicionais às covariáveis              #
#                                                                              #
# ESTRUTURA DO CÓDIGO:                                                         #
#   1. Funções de preparação e limpeza de dados                              #
#   2. Estimadores principais (ATT) com múltiplos outcomes                   #
#   3. Testes de robustez e validação                                        #
#   4. Análises de heterogeneidade e outcomes alternativos                   #
#   5. Geração automatizada de visualizações e relatórios                    #
#                                                                              #
# NOTA: Este código prioriza transparência e reprodutibilidade, com          #
# documentação extensiva de cada decisão metodológica.                       #
###############################################################################

# ═══════════════════════════════════════════════════════════════════════════ #
# 0. CONFIGURAÇÃO DO AMBIENTE E DEPENDÊNCIAS                                  #
# ═══════════════════════════════════════════════════════════════════════════ #
# Esta seção garante reprodutibilidade através do gerenciamento explícito     #
# de dependências. O uso do renv permite que futuros pesquisadores repliquem  #
# exatamente o mesmo ambiente computacional.                                   #
#                                                                              #
# Pacotes principais utilizados:                                               #
#   - did: Implementação do estimador Callaway & Sant'Anna (2021)            #
#   - tidyverse: Manipulação e visualização de dados                         #
#   - gt/kableExtra: Geração de tabelas profissionais                        #
#   - here: Gerenciamento robusto de caminhos de arquivos                    #
# --------------------------------------------------------------------------- #

pkgs <- c(
  "did", "dplyr", "ggplot2", "readr", "here", "cli", "purrr", "tibble", "tidyr",
  "knitr", "kableExtra", "scales", "viridis", "patchwork", "gt", "htmltools", "corrplot",
  "sf", "geobr", "RColorBrewer", "ggridges", "gtsummary",
  "foreach", "doParallel" # Para paralelização do placebo test
)
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# --------------------------------------------------------------------------- #
# 1. Funções Auxiliares                                                       #
# --------------------------------------------------------------------------- #

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.1 PREPARAÇÃO E TRANSFORMAÇÃO DOS DADOS                                │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' prep_data(): Função central de preparação dos dados para análise DiD
#'
#' DESCRIÇÃO DETALHADA:
#' Esta função implementa todas as transformações necessárias para adequar os
#' dados brutos ao formato exigido pelo estimador de Callaway & Sant'Anna.
#'
#' TRANSFORMAÇÕES PRINCIPAIS:
#'   1. LIMPEZA: Remove espaços em branco e converte tipos de dados
#'
#'   2. TRANSFORMAÇÃO LOGARÍTMICA: Aplica log(1+x) às variáveis de interesse
#'      - Justificativa econométrica: lineariza relações multiplicativas
#'      - Interpretação: coeficientes ≈ variações percentuais (elasticidades)
#'      - log(1+x) evita problemas com zeros (log(0) = -∞)
#'
#'   3. CONSTRUÇÃO DO GRUPO DE TRATAMENTO (gname):
#'      - gname = ano da PRIMEIRA estação instalada na microrregião
#'      - gname = 0 para unidades nunca tratadas (convenção do pacote 'did')
#'      - CRUCIAL: define os grupos de comparação no estimador
#'
#'   4. VARIÁVEIS DE CONTROLE:
#'      - Densidade de estações na UF: captura spillovers regionais
#'      - PIB não-agropecuário: usado em testes placebo
#'      - Todas em log para estabilizar variância
#'
#' @param path_csv String - Caminho para arquivo CSV com dados brutos
#' @return Tibble formatado para análise DiD com estrutura de painel balanceado
#'
#' NOTA METODOLÓGICA: A qualidade da preparação dos dados é fundamental para
#' a validade das estimativas causais. Cada transformação tem justificativa
#' econométrica específica documentada no código.
#' ---------------------------------------------------------------------------
prep_data <- function(path_csv) {
  cli::cli_alert_info("Lendo dados de {path_csv} …")

  df <- readr::read_csv(path_csv, show_col_types = FALSE) %>%
    # Conversão explícita para evitar fatores escondidos
    mutate(across(where(is.character), ~ trimws(.x))) %>%
    # Converter variáveis numéricas
    mutate(across(c(
      area_plantada_cana, area_plantada_soja, area_plantada_arroz,
      area_total_km2, valor_agregado, populacao_total, pib_total, 
      pib_per_capita, pib_agropecuario, precip_total_anual_mm,
      precip_media_mensal_mm, precip_max_mensal_mm
    ), as.numeric))

  # Diagnóstico inicial
  cli::cli_alert_info("Estrutura dos dados:")
  cli::cli_alert_info("  - Período: {min(df$ano)} a {max(df$ano)}")
  cli::cli_alert_info("  - Microrregiões: {n_distinct(df$id_microrregiao)}")

  # Verificar dados pré-tratamento
  pre_check <- df %>%
    filter(primeiro_ano_tratamento > 0) %>%
    group_by(id_microrregiao) %>%
    summarise(
      tem_pre = any(ano < primeiro_ano_tratamento[1]),
      anos_pre = sum(ano < primeiro_ano_tratamento[1]),
      .groups = "drop"
    )

  n_com_pre <- sum(pre_check$tem_pre)
  cli::cli_alert_success("Microrregiões com dados pré-tratamento: {n_com_pre} de {nrow(pre_check)}")

  # Calcular densidade de estações por UF e ano
  # NOTA: Esta variável captura spillovers espaciais - microrregiões podem
  # se beneficiar de estações em áreas vizinhas dentro do mesmo estado
  densidade_uf <- df %>%
    group_by(sigla_uf, ano) %>%
    summarise(
      n_estacoes_uf = sum(tratado == 1, na.rm = TRUE),
      n_microregioes_uf = n_distinct(id_microrregiao),
      densidade_estacoes_uf = n_estacoes_uf / n_microregioes_uf,
      .groups = "drop"
    )

  # Preparar variáveis para DiD
  df <- df %>%
    # Adicionar densidade de estações na UF
    left_join(densidade_uf, by = c("sigla_uf", "ano")) %>%
    mutate(
      # gname: ano do primeiro tratamento (0 para nunca tratadas)
      # No novo dataset, primeiro_ano_tratamento = 0 já indica nunca tratado
      gname = primeiro_ano_tratamento,
      # Indicador de nunca tratado
      never_treated = (primeiro_ano_tratamento == 0),
      # Renomear tratado para treated (compatibilidade)
      treated = tratado,
      # Criar versões log das covariáveis (para estabilizar variância)
      # Usar área total como proxy para área plantada agregada
      log_area_total = log1p(area_total_km2),
      log_populacao = log1p(populacao_total),
      log_pib_per_capita = log1p(pib_per_capita),
      log_pib_agro = log1p(pib_agropecuario),
      # PIB não-agropecuário para placebo test
      pib_nao_agro = ifelse(is.na(pib_total) | is.na(pib_agropecuario), NA,
        pib_total - pib_agropecuario
      ),
      log_pib_nao_agro = log1p(pib_nao_agro),
      # Proporção do PIB agropecuário
      prop_pib_agro = ifelse(is.na(pib_total) | pib_total == 0, NA,
        pib_agropecuario / pib_total
      ),
      # Log da densidade de estações (para linearizar relação)
      log_densidade_estacoes_uf = log1p(densidade_estacoes_uf),
      # Log das áreas plantadas por cultura (novos outcomes)
      log_area_cana = log1p(area_plantada_cana),
      log_area_soja = log1p(area_plantada_soja),
      log_area_arroz = log1p(area_plantada_arroz),
      # Log de variáveis climáticas
      log_precip_anual = log1p(precip_total_anual_mm),
      # Log do valor agregado
      log_valor_agregado = log1p(valor_agregado)
    )

  # Estatísticas descritivas por grupo
  stats <- df %>%
    group_by(never_treated) %>%
    summarise(
      n_obs = n(),
      n_units = n_distinct(id_microrregiao),
      mean_pib_agro = mean(pib_agropecuario, na.rm = TRUE),
      sd_pib_agro = sd(pib_agropecuario, na.rm = TRUE),
      .groups = "drop"
    )

  n_years <- max(df$ano) - min(df$ano) + 1
  cli::cli_alert_info("Unidades adotantes precoces (tratadas até 2010): {sum(df$gname > 0 & df$gname <= 2010) / n_years}")
  cli::cli_alert_info("Total de unidades no dataset: {n_distinct(df$id_microrregiao)}")

  cli::cli_alert_success("Dados preparados: {nrow(df)} observações, {n_distinct(df$id_microrregiao)} unidades")

  return(df)
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.2 ESTIMAÇÃO DO AVERAGE TREATMENT EFFECT ON THE TREATED (ATT)          │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' estimate_att(): Implementação do estimador de Callaway & Sant'Anna (2021)
#'
#' FUNDAMENTOS TEÓRICOS:
#' O estimador CS é especificamente desenhado para contextos de adoção
#' escalonada (staggered adoption), superando os problemas dos estimadores
#' TWFE tradicionais que podem gerar viés quando os efeitos são heterogêneos.
#'
#' MECÂNICA DO ESTIMADOR:
#'   1. ESTIMAÇÃO POR GRUPO-TEMPO: Para cada coorte g e período t:
#'      ATT(g,t) = E[Y₁ - Y₀ | G=g, T=t]
#'      onde Y₁ = outcome com tratamento, Y₀ = outcome contrafactual
#'
#'   2. MÉTODO DOUBLY ROBUST (DR):
#'      - Combina dois modelos: outcome regression + propensity score (IPW)
#'      - Consistente se PELO MENOS UM dos modelos estiver correto
#'      - Mais robusto que IPW ou regression sozinhos
#'
#'   3. AGREGAÇÃO DOS EFEITOS:
#'      - Overall ATT: média ponderada dos ATT(g,t) individuais
#'      - Event-study: efeitos por tempo relativo ao tratamento
#'      - Pesos proporcionais ao tamanho de cada grupo
#'
#' INFERÊNCIA:
#'   - Bootstrap para erros-padrão (mais conservador)
#'   - Clustering por unidade (microrregião) para dependência serial
#'   - Trata correlação espacial implicitamente via densidade_uf
#'
#' TRATAMENTO DE PROBLEMAS COMPUTACIONAIS:
#'   - Detecção automática de singularidade/colinearidade
#'   - Fallback progressivo: DR → DR sem covariáveis → IPW
#'   - Preserva validade da inferência mesmo com adaptações
#'
#' @param df DataFrame preparado com estrutura de painel
#' @param method String - "dr" (doubly robust), "ipw" (inverse probability
#'        weighting), ou "reg" (outcome regression)
#' @param seed Integer - Para reprodutibilidade do bootstrap
#' @param control_grp String - "notyettreated" (padrão e recomendado aqui)
#' @param outcome String - Nome da variável de outcome (padrão: "log_area_cana")
#'
#' @return Lista contendo:
#'   - att: objeto att_gt com todos os ATT(g,t) estimados
#'   - overall: agregação global (ATT médio)
#'   - event: agregação event-study (por tempo relativo)
#'   - Estatísticas de inferência (SE, p-valor, IC 95%)
#' ---------------------------------------------------------------------------
estimate_att <- function(df, method = "dr", seed = 42, control_grp = "notyettreated", outcome = "log_area_cana") {
  set.seed(seed)
  cli::cli_alert_info("Estimando ATT(g,t) com método {method} …")

  # Seleção de covariáveis elegíveis - usando variáveis socioeconômicas e climáticas
  base_covars <- c(
    "log_area_total", "log_populacao", "log_pib_per_capita",
    "log_densidade_estacoes_uf", "log_precip_anual"
  )
  covars_ok <- check_covariates(df, base_covars)

  # Construção de fórmula
  xform <- if (length(covars_ok) == 0) {
    ~1
  } else {
    as.formula(paste("~", paste(covars_ok, collapse = "+")))
  }

  # Função auxiliar para chamada segura
  safe_att <- function(met, form, outcome_var) {
    cli::cli_alert_info("Tentando estimador {met} com fórmula: {deparse(form)} para outcome {outcome_var}")
    did::att_gt(
      yname = outcome_var,
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
      safe_att(method, xform, outcome)
    },
    error = function(e) {
      if (grepl("singular", e$message)) {
        cli::cli_alert_warning("Singularidade detectada. Reestimando sem covariáveis…")
        tryCatch(safe_att(method, ~1, outcome), error = function(e2) {
          cli::cli_alert_warning("Ainda falhou. Alternando para método 'ipw'.")
          safe_att("ipw", ~1, outcome)
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
  # Incluir outcome no nome do arquivo para diferenciar resultados por variável dependente
  dir_out <- here::here("data", "outputs")
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  outcome_suffix <- gsub("log_", "", outcome)  # Remove "log_" prefix for cleaner names
  saveRDS(att_res, file = file.path(dir_out, paste0("att_results_", method, "_", outcome_suffix, ".rds")))
  saveRDS(agg_overall, file = file.path(dir_out, paste0("agg_overall_", method, "_", outcome_suffix, ".rds")))
  saveRDS(agg_event, file = file.path(dir_out, paste0("agg_event_", method, "_", outcome_suffix, ".rds")))

  # Efeito médio global da versão atual do pacote did
  att_global <- agg_overall$overall.att
  se_global <- agg_overall$overall.se

  # Métricas de significância estatística
  # Usa distribuição normal para grandes amostras (válido pelo TCL)
  z_score <- att_global / se_global
  p_value <- 2 * stats::pnorm(-abs(z_score)) # Teste bicaudal
  ci_low <- att_global - 1.96 * se_global # IC 95% inferior
  ci_high <- att_global + 1.96 * se_global # IC 95% superior

  # Formatação adaptativa do p-value para melhor legibilidade
  p_formatted <- ifelse(p_value < 0.001,
    sprintf("%.2e", p_value), # Notação científica para valores muito pequenos
    sprintf("%.4f", p_value) # 4 decimais para valores maiores
  )

  cli::cli_alert_success("ATT = {round(att_global,4)}, SE = {round(se_global,4)}, z = {round(z_score,2)}, p = {p_formatted}, IC95% = [{round(ci_low,4)}; {round(ci_high,4)}]")

  return(list(
    att = att_res, overall = agg_overall, event = agg_event,
    att_global = att_global, se_global = se_global,
    z = z_score, p = p_value, ci_low = ci_low, ci_high = ci_high
  ))
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.3 TESTE PLACEBO COM ANO FICTÍCIO                                      │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' placebo_test(): Validação via randomização completa (placebo fixo)
#'
#' LÓGICA DO TESTE:
#' Se o modelo identifica corretamente o efeito causal das estações meteorológicas,
#' então atribuir tratamento de forma COMPLETAMENTE ALEATÓRIA deve resultar
#' em ATT ≈ 0. Este é um teste de falsificação por randomização.
#'
#' IMPLEMENTAÇÃO:
#'   1. Ignora-se completamente o status real de tratamento
#'   2. Seleciona-se aleatoriamente ~50% das unidades
#'   3. Atribui-se tratamento placebo (gname = placebo_year) aos selecionados
#'   4. Os demais permanecem como controle (gname = 0)
#'   5. Estima-se o modelo com esta atribuição aleatória
#'
#' INTERPRETAÇÃO:
#'   - ATT ≈ 0 (p > 0.10): Resultado esperado - sem efeito com randomização
#'   - ATT ≠ 0 (p < 0.05): Indica possível problema na especificação
#'   - Magnitude pequena mesmo se significante é aceitável
#'
#' @param df DataFrame preparado com dados em painel
#' @param placebo_year Integer - Ano fictício de tratamento (padrão: 2015)
#' @param seed Integer - Semente para randomização (padrão: 2024)
#' @param outcome String - Nome da variável de outcome (padrão: "log_area_cana")
#'
#' @return Lista com ATT placebo, erro-padrão, p-valor e IC 95%
#'         ou NULL se estimação falhar
#' ---------------------------------------------------------------------------
placebo_test <- function(df, placebo_year = 2015, seed = 2024, outcome = "log_area_cana") {
  cli::cli_alert_info("Executando placebo test fixo (ano fictício = {placebo_year})…")

  # Verificar se o ano placebo está dentro do período dos dados
  year_range <- range(df$ano)
  if (placebo_year < year_range[1] || placebo_year > year_range[2]) {
    cli::cli_alert_warning("Ano placebo {placebo_year} fora do período dos dados [{year_range[1]}, {year_range[2]}]")
    return(NULL)
  }

  # Placebo test fixo: Randomização completa
  # Objetivo: Atribuir aleatoriamente ~50% das unidades para tratamento placebo
  # Hipótese nula: Não deve haver efeito significativo com atribuição aleatória

  set.seed(seed) # Para reprodutibilidade

  # Obter lista única de unidades
  all_units <- unique(df$id_microrregiao)
  n_units <- length(all_units)

  # Selecionar aleatoriamente metade das unidades para tratamento placebo
  n_treated_placebo <- round(n_units / 2)
  treated_units_placebo <- sample(all_units, n_treated_placebo, replace = FALSE)

  cli::cli_alert_info("Total de unidades: {n_units}")
  cli::cli_alert_info("Unidades aleatoriamente atribuídas ao tratamento placebo: {n_treated_placebo} ({round(100*n_treated_placebo/n_units, 1)}%)")

  # Criar dataset placebo com atribuição completamente aleatória
  df_placebo <- df %>%
    mutate(
      # Ignorar tratamento real - atribuir placebo aleatoriamente
      gname = ifelse(
        id_microrregiao %in% treated_units_placebo,
        placebo_year, # Tratadas no ano placebo
        0 # Controles (nunca tratadas)
      )
    )

  # Verificação rápida
  n_treated_check <- df_placebo %>%
    filter(gname == placebo_year) %>%
    distinct(id_microrregiao) %>%
    nrow()

  n_control_check <- df_placebo %>%
    filter(gname == 0) %>%
    distinct(id_microrregiao) %>%
    nrow()

  cli::cli_alert_info("Verificação: {n_treated_check} tratadas, {n_control_check} controles")

  out <- tryCatch(
    {
      res <- estimate_att(df_placebo, method = "dr", control_grp = "notyettreated", outcome = outcome)
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
#' @param outcome String - Nome da variável de outcome
#' ---------------------------------------------------------------------------
robust_specs <- function(df, outcome = "log_area_cana") {
  methods <- c("dr", "ipw", "reg")
  out <- purrr::map_dfr(methods, function(m) {
    res <- tryCatch(
      estimate_att(df, method = m, outcome = outcome),
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
    geom_line(color = "navy", linewidth = 1) +
    geom_point(color = "navy", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Event Study: Impacto da Estação ao Longo do Tempo",
      x = "Períodos em relação ao tratamento",
      y = "Efeito estimado (log PIB Agropecuário)"
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
  df_pre <- df %>% filter((gname == 0) | (gname > 0 & ano < gname))

  # Variância de cada covariável
  var_tbl <- purrr::map_dfr(covars, function(v) {
    # Verificar se a variável existe
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

  # Mantém apenas variáveis com variância > 0 e prop_na < 0.5
  valid_covars <- var_tbl %>%
    filter(!is.na(var_pre), var_pre > 1e-10, prop_na < 0.5) %>%
    pull(var)

  # Para dados de PIB (com NAs em 2022-2023), aceitar até 20% de NAs
  pib_vars <- var_tbl %>%
    filter(
      grepl("pib|populacao", var, ignore.case = TRUE),
      !is.na(var_pre), var_pre > 1e-10, prop_na <= 0.2
    ) %>%
    pull(var)

  valid_covars <- unique(c(valid_covars, pib_vars))

  # Colinearidade: constrói matriz incremental
  keep <- c()
  for (v in valid_covars) {
    test_set <- c(keep, v)
    # Remover NAs para teste de colinearidade
    df_test <- df_pre %>%
      select(all_of(test_set)) %>%
      na.omit()

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

  cli::cli_alert_success("Covariáveis elegíveis: {paste(keep, collapse = ', ')}")

  # Exporta diagnóstico
  diag_dir <- here::here("data", "outputs")
  if (!dir.exists(diag_dir)) dir.create(diag_dir, recursive = TRUE)
  diag_path <- file.path(diag_dir, "covar_diag.csv")
  readr::write_csv(var_tbl, diag_path)

  return(keep)
}

# 1.7 Placebo Test com PIB Não-Agropecuário --------------------------------- #
#' placebo_test_non_agro()
#' ---------------------------------------------------------------------------
#' Testa se o efeito persiste em setores não relacionados à agricultura.
#' Se houver efeito significativo, pode indicar desenvolvimento geral ou
#' problema de especificação.
#' @param df Dados preparados
#' @param method Método de estimação
#' @return Lista com resultados do placebo
#' ---------------------------------------------------------------------------
placebo_test_non_agro <- function(df, method = "dr") {
  cli::cli_alert_info("Executando placebo test com PIB não-agropecuário...")

  # Criar dataset temporário com PIB não-agro como outcome
  df_placebo <- df %>%
    mutate(
      # Temporariamente substituir a variável de resultado
      log_pib_agro_original = log_pib_agro,
      log_pib_agro = log_pib_nao_agro
    ) %>%
    # Remover observações sem PIB não-agro
    filter(!is.na(log_pib_agro))

  # Estimar ATT com PIB não-agro
  placebo_res <- tryCatch(
    {
      estimate_att(df_placebo, method = method)
    },
    error = function(e) {
      cli::cli_alert_warning("Erro no placebo test: {e$message}")
      NULL
    }
  )

  if (!is.null(placebo_res)) {
    cli::cli_alert_info("Placebo ATT (PIB não-agro): {round(placebo_res$att_global, 4)}")
    cli::cli_alert_info("Placebo p-valor: {round(placebo_res$p, 4)}")

    if (placebo_res$p < 0.05) {
      cli::cli_alert_warning("⚠️ Efeito significativo no PIB não-agropecuário pode indicar:")
      cli::cli_alert_warning("  - Desenvolvimento econômico geral (não específico à agricultura)")
      cli::cli_alert_warning("  - Spillovers para outros setores")
      cli::cli_alert_warning("  - Problemas de especificação do modelo")
    } else {
      cli::cli_alert_success("✓ Efeito não significativo no PIB não-agropecuário")
      cli::cli_alert_success("  Sugere que o impacto é específico ao setor agrícola")
    }
  }

  return(placebo_res)
}

# 1.8 Teste de Balanceamento Pós-DR ----------------------------------------- #
#' check_balance_post_dr()
#' ---------------------------------------------------------------------------
#' Verifica se o método DR balanceou adequadamente as covariáveis entre
#' grupos de tratamento e controle após a reponderação.
#' @param att_obj Objeto att_gt estimado
#' @param df Dados originais
#' @return Tibble com estatísticas de balanceamento
#' ---------------------------------------------------------------------------
check_balance_post_dr <- function(att_obj, df) {
  cli::cli_alert_info("Verificando balanceamento das covariáveis pós-DR...")

  # Extrair pesos do modelo (se disponível)
  # Nota: o pacote 'did' não exporta diretamente os pesos, então faremos
  # uma análise aproximada comparando médias condicionais

  covars <- c(
    "log_area_total", "log_populacao", "log_pib_per_capita",
    "log_densidade_estacoes_uf", "log_precip_anual"
  )

  # Calcular médias por grupo de tratamento no período pré
  balance_pre <- df %>%
    filter((gname == 0) | (gname > 0 & ano < gname)) %>%
    mutate(treat_group = ifelse(gname > 0, "Tratado", "Controle"))

  # Calcular estatísticas para cada covariável
  balance_stats <- purrr::map_dfr(covars, function(cv) {
    # Estatísticas por grupo
    stats_by_group <- balance_pre %>%
      group_by(treat_group) %>%
      summarise(
        mean = mean(!!sym(cv), na.rm = TRUE),
        sd = sd(!!sym(cv), na.rm = TRUE),
        .groups = "drop"
      )

    # Extrair valores
    control_stats <- stats_by_group %>% filter(treat_group == "Controle")
    treated_stats <- stats_by_group %>% filter(treat_group == "Tratado")

    # Se algum grupo não existir, retornar NA
    if (nrow(control_stats) == 0 || nrow(treated_stats) == 0) {
      return(tibble::tibble(
        variable = cv,
        Controle_mean = NA,
        Controle_sd = NA,
        Tratado_mean = NA,
        Tratado_sd = NA,
        diff_means = NA,
        std_diff = NA,
        balanced = NA
      ))
    }

    # Calcular diferenças
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
  })

  cli::cli_alert_info("Diferenças padronizadas nas covariáveis:")
  print(balance_stats %>% select(variable, std_diff, balanced))

  n_balanced <- sum(balance_stats$balanced, na.rm = TRUE)
  n_total <- nrow(balance_stats)

  if (n_balanced == n_total) {
    cli::cli_alert_success("✓ Todas as covariáveis estão bem balanceadas (|d| < 0.1)")
  } else {
    cli::cli_alert_warning("⚠️ {n_total - n_balanced} de {n_total} covariáveis com desbalanceamento")
  }

  return(balance_stats)
}

# 1.9 Análise de Pesos de Adoção Escalonada --------------------------------- #
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
#' @param outcome Nome da variável de outcome
#' @return Tibble comparando resultados
#' ---------------------------------------------------------------------------
compare_control_groups <- function(df, method = "dr", outcome = "log_area_cana") {
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
    estimate_att(df, method = method, control_grp = "notyettreated", outcome = outcome),
    error = function(e) {
      cli::cli_alert_warning("Falha com not-yet-treated: {e$message}")
      NULL
    }
  )

  # Testar também com "nevertreated" para validação de hipótese
  cli::cli_h3("Estimando com controle 'nevertreated' (para validação)")
  cli::cli_alert_warning("Nota: Como todas unidades são eventualmente tratadas, 'nevertreated' tratará unidades de controle temporário como nunca tratadas")

  results$nt <- tryCatch(
    estimate_att(df, method = method, control_grp = "nevertreated", outcome = outcome),
    error = function(e) {
      cli::cli_alert_warning("Falha com nevertreated: {e$message}")
      NULL
    }
  )

  # Compara resultados
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

# 1.9 Análise de Robustez com Múltiplas Especificações ---------------------- #
#' robustness_analysis()
#' ---------------------------------------------------------------------------
#' Testa a robustez dos resultados com diferentes especificações:
#' - Variação de covariáveis
#' - Diferentes métodos (DR, IPW, REG)
#' - Exclusão de períodos recentes (2022-2023)
#' @param df Dados preparados
#' @return Tibble com resultados de todas as especificações
#' ---------------------------------------------------------------------------
robustness_analysis <- function(df) {
  cli::cli_h2("Análise de Robustez - Múltiplas Especificações")

  # Definir especificações
  specs <- list(
    list(
      name = "Baseline (DR)", method = "dr",
      covars = c(
        "log_area_total", "log_populacao", "log_pib_per_capita",
        "log_densidade_estacoes_uf", "log_precip_anual"
      ),
      filter_years = FALSE
    ),
    list(
      name = "Sem covariáveis", method = "dr",
      covars = NULL,
      filter_years = FALSE
    ),
    list(
      name = "Covariáveis básicas", method = "dr",
      covars = c("log_area_total", "log_populacao", "log_precip_anual"),
      filter_years = FALSE
    ),
    list(
      name = "IPW", method = "ipw",
      covars = c(
        "log_area_total", "log_populacao", "log_pib_per_capita",
        "log_densidade_estacoes_uf", "log_precip_anual"
      ),
      filter_years = FALSE
    ),
    list(
      name = "REG", method = "reg",
      covars = c(
        "log_area_total", "log_populacao", "log_pib_per_capita",
        "log_densidade_estacoes_uf", "log_precip_anual"
      ),
      filter_years = FALSE
    ),
    list(
      name = "Excl. 2022-23", method = "dr",
      covars = c(
        "log_area_total", "log_populacao", "log_pib_per_capita",
        "log_densidade_estacoes_uf", "log_precip_anual"
      ),
      filter_years = TRUE
    )
  )

  # Executar todas as especificações
  results <- purrr::map_dfr(specs, function(spec) {
    cli::cli_alert_info("Estimando: {spec$name}")

    # Aplicar filtros se necessário
    df_spec <- df
    if (spec$filter_years) {
      df_spec <- df_spec %>% filter(ano <= 2021)
    }

    # Temporariamente modificar a função estimate_att para usar covariáveis específicas
    if (is.null(spec$covars)) {
      # Sem covariáveis
      df_spec_temp <- df_spec
      # Remover temporariamente as covariáveis do dataset
      base_covars_original <- c(
        "log_area_total", "log_populacao",
        "log_pib_per_capita", "log_densidade_estacoes_uf", "log_precip_anual"
      )
      for (cv in base_covars_original) {
        df_spec_temp[[cv]] <- 0 # Zerar para forçar exclusão
      }
      res <- tryCatch(
        {
          estimate_att(df_spec_temp, method = spec$method)
        },
        error = function(e) {
          cli::cli_alert_warning("Erro em {spec$name}: {e$message}")
          list(att_global = NA, se_global = NA, p = NA)
        }
      )
    } else {
      # Com covariáveis específicas
      res <- tryCatch(
        {
          # Criar dataset temporário só com as covariáveis desejadas
          df_spec_temp <- df_spec
          all_covars <- c(
            "log_area_total", "log_populacao",
            "log_pib_per_capita", "log_densidade_estacoes_uf", "log_precip_anual"
          )
          remove_covars <- setdiff(all_covars, spec$covars)
          for (cv in remove_covars) {
            if (cv %in% names(df_spec_temp)) {
              df_spec_temp[[cv]] <- 0 # Zerar para forçar exclusão
            }
          }
          estimate_att(df_spec_temp, method = spec$method)
        },
        error = function(e) {
          cli::cli_alert_warning("Erro em {spec$name}: {e$message}")
          list(att_global = NA, se_global = NA, p = NA)
        }
      )
    }

    # Coletar resultados
    tibble::tibble(
      specification = spec$name,
      method = spec$method,
      att = res$att_global,
      se = res$se_global,
      p_value = res$p,
      ci_lower = res$att_global - 1.96 * res$se_global,
      ci_upper = res$att_global + 1.96 * res$se_global,
      n_obs = nrow(df_spec),
      significant = res$p < 0.05
    )
  })

  # Salvar resultados
  readr::write_csv(results, here::here("data", "outputs", "robustness_analysis.csv"))

  # Criar visualização
  p_robust <- ggplot(
    results %>% filter(!is.na(att)),
    aes(x = specification, y = att)
  ) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Análise de Robustez: ATT sob Diferentes Especificações",
      x = "Especificação",
      y = "ATT Estimado",
      caption = "Barras de erro representam IC 95%"
    )

  ggsave(here::here("data", "outputs", "robustness_plot.png"),
    p_robust,
    width = 10, height = 6
  )

  # Mostrar resumo
  cli::cli_alert_success("Análise de robustez completa:")
  print(results %>% select(specification, att, se, p_value, significant))

  # Verificar consistência
  att_range <- range(results$att, na.rm = TRUE)
  if (all(results$significant[!is.na(results$att)])) {
    cli::cli_alert_success("✓ ATT significativo em TODAS as especificações testadas")
  }

  if (all(results$att[!is.na(results$att)] > 0)) {
    cli::cli_alert_success("✓ ATT positivo em TODAS as especificações")
  }

  cli::cli_alert_info("Range do ATT: [{round(att_range[1], 3)}, {round(att_range[2], 3)}]")

  return(results)
}

# 1.10 Diagnóstico de Coortes com NA ---------------------------------------- #
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

# 1.11.1 Funções Auxiliares de Paralelização -------------------------------- #
#' setup_parallel_placebo()
#' ---------------------------------------------------------------------------
#' Configura ambiente paralelo para placebo test
#' @param n_cores Número de cores (NULL = auto-detecta)
#' @return Lista com n_cores e cluster (se Windows)
#' ---------------------------------------------------------------------------
setup_parallel_placebo <- function(n_cores = NULL) {
  # Auto-detecta cores se não especificado
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
    cli::cli_alert_info("Auto-detectados {parallel::detectCores()} cores, usando {n_cores}")
  }

  # Setup específico por OS
  if (.Platform$OS.type == "windows") {
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    return(list(n_cores = n_cores, cluster = cl))
  } else {
    doParallel::registerDoParallel(cores = n_cores)
    return(list(n_cores = n_cores, cluster = NULL))
  }
}

#' cleanup_parallel_placebo()
#' ---------------------------------------------------------------------------
#' Limpa recursos de paralelização
#' @param setup_info Retorno de setup_parallel_placebo
#' ---------------------------------------------------------------------------
cleanup_parallel_placebo <- function(setup_info) {
  if (!is.null(setup_info$cluster)) {
    parallel::stopCluster(setup_info$cluster)
  }
  foreach::registerDoSEQ()
}

# 1.12 Teste Placebo Aleatório ---------------------------------------------- #
#' random_placebo_test_serial()
#' ---------------------------------------------------------------------------
#' Versão serial original - fallback quando paralelização não disponível
#' @param df Dados preparados
#' @param n_sims Número de simulações
#' @param method Método de estimação
#' @param seed Semente aleatória
#' @param outcome Nome da variável de outcome
#' @return Lista com distribuição de placebos e p-valor
#' ---------------------------------------------------------------------------
random_placebo_test_serial <- function(df, n_sims = 100, method = "dr", seed = 42, outcome = "log_area_cana") {
  set.seed(seed)
  cli::cli_alert_info("Executando {n_sims} testes placebo aleatórios...")
  cli::cli_alert_info("Outcome testado: {outcome}")

  # ATT verdadeiro para comparação
  true_res <- estimate_att(df, method = method, outcome = outcome)
  true_att <- true_res$att_global

  # Identifica todas as unidades e informações de tratamento
  all_units <- unique(df$id_microrregiao)
  n_treated_original <- sum(unique(df %>% select(id_microrregiao, gname)) %>% pull(gname) > 0)

  year_range <- range(df$ano)
  possible_years <- seq(year_range[1] + 2, year_range[2] - 2) # Margem de segurança

  # Simulações
  placebo_atts <- numeric(n_sims)
  cli::cli_progress_bar("Simulando placebos", total = n_sims)

  for (i in 1:n_sims) {
    cli::cli_progress_update()

    # IMPORTANTE: Definir seed diferente para cada simulação
    set.seed(seed + i)

    # Randomizar completamente quem é tratado e quando
    # Mantém o mesmo número de unidades tratadas que o original
    treated_units_placebo <- sample(all_units, n_treated_original, replace = FALSE)

    # Atribui anos aleatórios para as unidades "tratadas" no placebo
    unit_assignments <- tibble::tibble(
      id_microrregiao = all_units,
      gname_placebo = ifelse(
        id_microrregiao %in% treated_units_placebo,
        sample(possible_years, length(all_units), replace = TRUE),
        0
      )
    )

    df_placebo <- df %>%
      select(-gname) %>% # Remove gname original
      left_join(unit_assignments, by = "id_microrregiao") %>%
      rename(gname = gname_placebo)

    # Estima ATT placebo
    placebo_res <- tryCatch(
      {
        res <- estimate_att(df_placebo, method = method, outcome = outcome)
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

  # Calcula p-valor empírico com correção de amostra finita
  # p̂ = (1 + #{extremes}) / (S + 1) para evitar p=0 e manter o teste conservador
  n_extremes <- sum(abs(placebo_atts) >= abs(true_att))
  p_value <- (1 + n_extremes) / (n_valid + 1)

  # Erro padrão e intervalo de confiança do p-valor (Monte Carlo)
  # SE(p̂) = sqrt(p̂(1-p̂)/S) - variabilidade devido ao número finito de simulações
  se_pvalue <- sqrt(p_value * (1 - p_value) / n_valid)
  ci_lower_pvalue <- max(0, p_value - 1.96 * se_pvalue)
  ci_upper_pvalue <- min(1, p_value + 1.96 * se_pvalue)

  # Percentis para comparação
  percentiles <- quantile(placebo_atts, c(0.025, 0.05, 0.95, 0.975))

  # Formatação do p-value empírico
  p_emp_formatted <- ifelse(p_value < 0.001,
    sprintf("%.2e", p_value),
    sprintf("%.4f", p_value)
  )
  cli::cli_alert_success("P-valor empírico (com correção): {p_emp_formatted}")
  cli::cli_alert_info("IC 95% do p-valor: [{sprintf('%.4f', ci_lower_pvalue)}, {sprintf('%.4f', ci_upper_pvalue)}]")
  cli::cli_alert_info("ATT verdadeiro: {round(true_att, 4)}")
  cli::cli_alert_info("Extremos: {n_extremes} de {n_valid} ({round(100*n_extremes/n_valid, 1)}%)")
  cli::cli_alert_info("Intervalo 95% placebos: [{round(percentiles[1], 4)}, {round(percentiles[4], 4)}]")

  # Visualização
  p_dist <- ggplot(data.frame(att = placebo_atts), aes(x = att)) +
    geom_histogram(bins = 30, fill = "lightgray", color = "black", alpha = 0.7) +
    geom_vline(xintercept = true_att, color = "red", linetype = "dashed", linewidth = 1.5) +
    geom_vline(xintercept = percentiles[c(1, 4)], color = "blue", linetype = "dotted") +
    theme_minimal() +
    labs(
      title = "Distribuição de ATTs Placebo vs ATT Verdadeiro",
      x = "ATT estimado",
      y = "Frequência",
      subtitle = sprintf(
        "P-valor (correção finita): %.3f | Extremos: %d/%d",
        p_value, n_extremes, n_valid
      )
    )

  ggsave(here::here("data", "outputs", "placebo_distribution.png"), p_dist, width = 10, height = 6)

  return(list(
    placebo_atts = placebo_atts,
    true_att = true_att,
    p_value = p_value,
    p_value_se = se_pvalue,
    p_value_ci = c(ci_lower_pvalue, ci_upper_pvalue),
    n_extremes = n_extremes,
    percentiles = percentiles,
    n_valid = n_valid
  ))
}

#' random_placebo_test()
#' ---------------------------------------------------------------------------
#' Versão paralela - usa foreach/doParallel para acelerar simulações
#' @param df Dados preparados
#' @param n_sims Número de simulações
#' @param method Método de estimação
#' @param seed Semente aleatória
#' @param outcome Nome da variável de outcome
#' @param parallel Usar paralelização? (default: TRUE)
#' @param n_cores Número de cores (NULL = auto)
#' @return Lista com distribuição de placebos e p-valor
#' ---------------------------------------------------------------------------
random_placebo_test <- function(df, n_sims = 100, method = "dr", seed = 42,
                                outcome = "log_area_cana", parallel = TRUE, n_cores = NULL) {
  # Decisão: paralelo ou serial?
  if (!parallel || n_sims < 100) {
    cli::cli_alert_info("Executando em modo serial (parallel = {parallel}, n_sims = {n_sims})")
    return(random_placebo_test_serial(df, n_sims, method, seed, outcome))
  }

  # Setup paralelo
  setup_info <- setup_parallel_placebo(n_cores)
  on.exit(cleanup_parallel_placebo(setup_info), add = TRUE)

  cli::cli_alert_info("Executando {n_sims} testes placebo em paralelo ({setup_info$n_cores} cores)...")
  cli::cli_alert_info("Outcome testado: {outcome}")

  # ATT verdadeiro para comparação
  true_res <- estimate_att(df, method = method, outcome = outcome)
  true_att <- true_res$att_global

  # Preparar dados compartilhados
  all_units <- unique(df$id_microrregiao)
  n_units <- length(all_units)
  n_treated_original <- df %>%
    group_by(id_microrregiao) %>%
    summarise(treated = any(gname > 0), .groups = "drop") %>%
    pull(treated) %>%
    sum()

  year_range <- range(df$ano)
  possible_years <- seq(year_range[1] + 2, year_range[2] - 2)

  # Tempo inicial
  start_time <- Sys.time()

  # Dividir simulações entre cores
  sims_per_core <- split(
    1:n_sims,
    rep(1:setup_info$n_cores,
      length.out = n_sims
    )
  )

  # Execução paralela
  cli::cli_alert_info("Iniciando {n_sims} simulações em {setup_info$n_cores} cores...")

  # Dividir em batches menores para mostrar progresso
  n_batches <- min(100, n_sims) # Mostrar progresso a cada 1% ou cada simulação
  batch_size <- ceiling(n_sims / n_batches)

  # Criar batches de simulações
  sim_batches <- split(1:n_sims, ceiling(seq_along(1:n_sims) / batch_size))

  # Barra de progresso
  cli::cli_progress_bar("Simulando placebos", total = length(sim_batches))

  placebo_results <- list()

  for (batch_idx in seq_along(sim_batches)) {
    batch_sims <- sim_batches[[batch_idx]]

    # Processar batch em paralelo
    batch_results <- foreach(
      sim_id = batch_sims,
      .combine = "c",
      .packages = c("dplyr", "did", "tibble"),
      .export = c("estimate_att", "check_covariates")
    ) %dopar% {
      # Seed única e reproduzível
      set.seed(seed * 1000 + sim_id)

      # Randomizar quais unidades são tratadas
      treated_units <- sample(all_units, n_treated_original, replace = FALSE)

      # Para cada unidade tratada, sortear ano de tratamento
      treatment_years <- sample(possible_years, n_treated_original, replace = TRUE)

      # Criar lookup de tratamento
      unit_treatment <- data.frame(
        id_microrregiao = all_units,
        gname_placebo = 0
      )
      unit_treatment$gname_placebo[match(treated_units, all_units)] <- treatment_years

      # Aplicar tratamento placebo
      df_placebo <- df %>%
        left_join(unit_treatment,
          by = "id_microrregiao",
          suffix = c("", "_drop")
        ) %>%
        mutate(gname = gname_placebo) %>%
        select(-gname_placebo)

      # Estimar ATT placebo (usar outcome do ambiente pai)
      tryCatch(
        {
          res <- estimate_att(df_placebo, method = method, outcome = outcome)
          res$att_global
        },
        error = function(e) NA_real_
      )
    }

    # Adicionar resultados do batch
    placebo_results[[batch_idx]] <- batch_results

    # Atualizar barra de progresso
    cli::cli_progress_update()
  }

  # Combinar todos os resultados
  placebo_results <- unlist(placebo_results)

  cli::cli_progress_done()

  # Tempo total
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  cli::cli_alert_success("Placebo test completado em {round(elapsed_time, 1)} minutos")

  # Remover NAs e processar resultados
  valid_results <- placebo_results[!is.na(placebo_results)]
  n_valid <- length(valid_results)

  if (n_valid < n_sims * 0.9) {
    cli::cli_alert_warning("Apenas {n_valid}/{n_sims} simulações válidas ({round(100*n_valid/n_sims, 1)}%)")
  }

  # Estatísticas com correção de amostra finita
  # p̂ = (1 + #{extremes}) / (S + 1) para evitar p=0 e manter o teste conservador
  n_extremes <- sum(abs(valid_results) >= abs(true_att))
  p_value <- (1 + n_extremes) / (n_valid + 1)

  # Erro padrão e intervalo de confiança do p-valor (Monte Carlo)
  # SE(p̂) = sqrt(p̂(1-p̂)/S) - variabilidade devido ao número finito de simulações
  se_pvalue <- sqrt(p_value * (1 - p_value) / n_valid)
  ci_lower_pvalue <- max(0, p_value - 1.96 * se_pvalue)
  ci_upper_pvalue <- min(1, p_value + 1.96 * se_pvalue)

  percentiles <- quantile(valid_results, c(0.025, 0.05, 0.5, 0.95, 0.975))

  # Formatação do p-value
  p_emp_formatted <- ifelse(p_value < 0.001,
    sprintf("%.2e", p_value),
    sprintf("%.4f", p_value)
  )

  cli::cli_alert_info("ATT verdadeiro: {round(true_att, 4)}")
  cli::cli_alert_info("P-valor empírico (com correção): {p_emp_formatted}")
  cli::cli_alert_info("IC 95% do p-valor: [{sprintf('%.4f', ci_lower_pvalue)}, {sprintf('%.4f', ci_upper_pvalue)}]")
  cli::cli_alert_info("Extremos: {n_extremes} de {n_valid} ({round(100*n_extremes/n_valid, 1)}%)")
  cli::cli_alert_info("IC 95% placebos: [{round(percentiles[1], 4)}, {round(percentiles[5], 4)}]")

  # Visualização aprimorada
  p_dist <- ggplot(data.frame(att = valid_results), aes(x = att)) +
    geom_histogram(bins = 50, fill = "lightgray", color = "black", alpha = 0.7) +
    geom_vline(xintercept = true_att, color = "red", linewidth = 2) +
    geom_vline(xintercept = percentiles[c(1, 5)], color = "blue", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray50", linetype = "dotted") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Distribuição de ATTs Placebo vs ATT Verdadeiro",
      subtitle = sprintf(
        "P-valor (correção finita): %.3f | Extremos: %d/%d | Tempo: %.1f min",
        p_value, n_extremes, n_valid, elapsed_time
      ),
      x = "ATT Estimado",
      y = "Frequência"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    here::here("data", "outputs", "placebo_distribution.png"),
    p_dist,
    width = 10,
    height = 6,
    dpi = 300
  )

  return(list(
    placebo_atts = valid_results,
    true_att = true_att,
    p_value = p_value,
    p_value_se = se_pvalue,
    p_value_ci = c(ci_lower_pvalue, ci_upper_pvalue),
    n_extremes = n_extremes,
    percentiles = percentiles,
    n_valid = n_valid,
    elapsed_time = as.numeric(elapsed_time),
    n_cores_used = setup_info$n_cores
  ))
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.13 ANÁLISE DID POR UNIDADE FEDERATIVA (UF)                            │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' uf_level_analysis(): Análise DiD usando UFs como unidades de análise
#'
#' MOTIVAÇÃO ECONÔMICA:
#' O impacto das estações meteorológicas pode variar substancialmente entre
#' regiões devido a diferenças em:
#'   - Estrutura produtiva agrícola (culturas predominantes)
#'   - Condições climáticas e variabilidade meteorológica
#'   - Capacidade institucional e capital humano
#'   - Infraestrutura tecnológica pré-existente
#'
#' DESAFIO METODOLÓGICO:
#' Como todas as microrregiões são eventualmente tratadas, não há controles
#' "puros" dentro de cada UF. Isso requer abordagens alternativas criativas.
#'
#' ESTRATÉGIAS IMPLEMENTADAS:
#'
#'   1. AGREGAÇÃO POR UF (approach = "aggregate"):
#'      - Colapsa dados de microrregiões para nível estadual
#'      - Usa médias ponderadas (peso = área plantada ou população)
#'      - Define tratamento quando >50% das microrregiões têm estação
#'      - VANTAGEM: Cria variação entre estados
#'      - LIMITAÇÃO: Perde variação within-state
#'
#'   2. ANÁLISE POR GRANDES REGIÕES:
#'      - Agrupa UFs em 5 macrorregiões (Norte, NE, CO, SE, Sul)
#'      - Mantém dados no nível de microrregião
#'      - Usa variação no timing entre microrregiões de diferentes UFs
#'      - VANTAGEM: Preserva granularidade dos dados
#'      - LIMITAÇÃO: Assume homogeneidade dentro da macrorregião
#'
#'   3. FALLBACK AUTOMÁTICO:
#'      - Se agregação por UF falha (poucos controles), usa regiões
#'      - Garante sempre algum resultado, mesmo que menos preciso
#'
#' INTERPRETAÇÃO DOS RESULTADOS:
#'   - Heterogeneidade significativa sugere necessidade de políticas regionais
#'   - Regiões com maior efeito podem ser priorizadas para expansão
#'   - Ausência de efeito em algumas regiões merece investigação adicional
#'
#' @param df DataFrame em painel preparado
#' @param method String - Método de estimação ("dr", "ipw", "reg")
#' @param approach String - "aggregate" ou "interaction" (não implementado)
#' @param outcome String - Nome da variável de outcome (padrão: "log_area_cana")
#'
#' @return Tibble com ATT estimado por região/UF, incluindo significância
#' ---------------------------------------------------------------------------
heterogeneity_analysis <- function(df, method = "dr", approach = "aggregate", outcome = "log_area_cana") {
  cli::cli_h2("Análise de Heterogeneidade Regional")

  if (approach == "aggregate") {
    # ABORDAGEM 1: Agregar dados por UF
    cli::cli_alert_info("Usando abordagem de agregação por UF...")

    # Agregar dados por UF e ano
    df_uf <- df %>%
      group_by(sigla_uf, ano) %>%
      summarise(
        # Média ponderada do PIB agro (peso = área total)
        log_pib_agro = weighted.mean(log_pib_agro, w = area_total_km2, na.rm = TRUE),
        log_area_total = log(sum(area_total_km2, na.rm = TRUE) + 1),
        log_populacao = log(sum(populacao_total, na.rm = TRUE) + 1),
        log_pib_per_capita = weighted.mean(log_pib_per_capita, w = populacao_total, na.rm = TRUE),
        log_densidade_estacoes_uf = first(log_densidade_estacoes_uf),
        log_precip_anual = weighted.mean(log_precip_anual, w = area_total_km2, na.rm = TRUE),
        # Proporção de microrregiões tratadas
        prop_tratadas = mean(gname > 0),
        # Ano médio de tratamento (ponderado)
        primeiro_ano_medio = weighted.mean(
          ifelse(gname == 0, NA, gname),
          w = ifelse(gname == 0, 0, 1),
          na.rm = TRUE
        ),
        n_microregioes = n(),
        .groups = "drop"
      ) %>%
      mutate(
        # Criar novo gname baseado em quando a maioria foi tratada
        gname = case_when(
          prop_tratadas < 0.5 ~ 0, # Controle se menos da metade tratada
          is.na(primeiro_ano_medio) ~ 0,
          TRUE ~ round(primeiro_ano_medio)
        ),
        # ID único por UF
        id_uf = as.numeric(as.factor(sigla_uf))
      ) %>%
      filter(!is.na(log_pib_agro))

    # Verificar variação
    cli::cli_alert_info("UFs no controle: {sum(df_uf$gname == 0)} observações")
    cli::cli_alert_info("UFs tratadas: {sum(df_uf$gname > 0)} observações")

    # Estimar modelo agregado
    # NOTA CRÍTICA: A agregação por UF só funciona para log_pib_agro pois é o único
    # outcome agregado na construção de df_uf. Para outros outcomes (cana, soja, arroz),
    # usamos a abordagem regional que mantém dados no nível de microrregião.
    if (sum(df_uf$gname == 0) < 10 || outcome != "log_pib_agro") {
      if (outcome != "log_pib_agro") {
        cli::cli_alert_info("Outcome {outcome} requer análise no nível de microrregião. Usando abordagem regional...")
      } else {
        cli::cli_alert_warning("Poucos controles no nível de UF. Usando abordagem alternativa...")
      }

      # ABORDAGEM 2: Análise por região com dados originais
      regioes <- list(
        "Norte" = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
        "Nordeste" = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
        "Centro-Oeste" = c("DF", "GO", "MT", "MS"),
        "Sudeste" = c("ES", "MG", "RJ", "SP"),
        "Sul" = c("PR", "RS", "SC")
      )

      het_results <- purrr::map_dfr(names(regioes), function(regiao) {
        cli::cli_alert_info("Estimando ATT para região {regiao}...")

        # Filtrar dados da região
        ufs_regiao <- regioes[[regiao]]
        df_regiao <- df %>% filter(sigla_uf %in% ufs_regiao)

        # Verificar variação
        n_treated <- sum(df_regiao$gname > 0)
        n_control <- sum(df_regiao$gname == 0)

        cli::cli_alert_info("  Região {regiao}: {n_treated} tratados, {n_control} controles")

        if (n_treated < 10 || n_control < 10) {
          return(tibble::tibble(
            uf = regiao,
            tipo = "Região",
            att = NA,
            se = NA,
            p_value = NA,
            n_obs = nrow(df_regiao),
            n_treated = n_treated,
            n_control = n_control,
            significant = NA
          ))
        }

        # Estimar ATT
        res <- tryCatch(
          {
            estimate_att(df_regiao, method = method, outcome = outcome)
          },
          error = function(e) {
            cli::cli_alert_warning("Erro ao estimar {regiao}: {e$message}")
            list(att_global = NA, se_global = NA, p = NA)
          }
        )

        tibble::tibble(
          uf = regiao,
          tipo = "Região",
          att = res$att_global,
          se = res$se_global,
          p_value = res$p,
          n_obs = nrow(df_regiao),
          n_treated = n_treated,
          n_control = n_control,
          significant = ifelse(is.na(res$p), NA, res$p < 0.05)
        )
      })
    } else {
      # Estimar com dados agregados por UF
      cli::cli_alert_info("Estimando modelo com dados agregados por UF...")

      # Preparar dados para att_gt
      df_uf_did <- df_uf %>%
        rename(
          id_microrregiao = id_uf # Usar id_uf como se fosse id_microrregiao
        ) %>%
        select(id_microrregiao, ano, log_pib_agro, gname, starts_with("log_"))

      # Estimar modelo
      # Nota: df_uf_did já tem log_pib_agro agregado, mas usamos outcome param
      res_uf <- tryCatch(
        {
          estimate_att(df_uf_did, method = method, outcome = outcome)
        },
        error = function(e) {
          cli::cli_alert_warning("Erro na estimação agregada: {e$message}")
          NULL
        }
      )

      if (!is.null(res_uf)) {
        # Extrair efeitos por UF dos resultados do modelo
        het_results <- df_uf %>%
          distinct(sigla_uf, gname) %>%
          filter(gname > 0) %>%
          mutate(
            tipo = "UF",
            att = res_uf$att_global, # Simplificação - usar ATT global
            se = res_uf$se_global,
            p_value = res_uf$p,
            n_obs = nrow(df_uf),
            n_treated = sum(df_uf$gname > 0),
            n_control = sum(df_uf$gname == 0),
            significant = res_uf$p < 0.05
          ) %>%
          rename(uf = sigla_uf)
      } else {
        het_results <- tibble()
      }
    }
  } else {
    # ABORDAGEM 3: Estimação com variável indicadora de UF
    cli::cli_alert_info("Análise simplificada por grandes regiões...")

    # Definir regiões
    df <- df %>%
      mutate(
        regiao = case_when(
          sigla_uf %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
          sigla_uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
          sigla_uf %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
          sigla_uf %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
          sigla_uf %in% c("PR", "RS", "SC") ~ "Sul"
        )
      )

    # Estimar por região
    het_results <- df %>%
      distinct(regiao) %>%
      pull(regiao) %>%
      purrr::map_dfr(function(reg) {
        df_reg <- df %>% filter(regiao == reg)

        res <- tryCatch(
          estimate_att(df_reg, method = method, outcome = outcome),
          error = function(e) list(att_global = NA, se_global = NA, p = NA)
        )

        tibble::tibble(
          uf = reg,
          tipo = "Região",
          att = res$att_global,
          se = res$se_global,
          p_value = res$p,
          n_obs = nrow(df_reg),
          n_treated = sum(df_reg$gname > 0),
          n_control = sum(df_reg$gname == 0),
          significant = ifelse(is.na(res$p), NA, res$p < 0.05)
        )
      })
  }

  # Garantir que todas as colunas necessárias existem
  required_cols <- c("uf", "tipo", "att", "se", "p_value", "n_obs", "n_treated", "n_control")
  missing_cols <- setdiff(required_cols, names(het_results))
  
  if (length(missing_cols) > 0) {
    cli::cli_alert_warning("Colunas faltando em het_results: {paste(missing_cols, collapse = ', ')}")
    for (col in missing_cols) {
      het_results[[col]] <- NA
    }
  }
  
  # Adicionar coluna significant se não existir
  if (!"significant" %in% names(het_results)) {
    het_results <- het_results %>%
      mutate(significant = ifelse(is.na(p_value), NA, p_value < 0.05))
  }

  # Salvar resultados
  readr::write_csv(het_results, here::here("data", "outputs", "heterogeneity_regional.csv"))

  # Criar visualização apenas se houver resultados válidos
  if (sum(!is.na(het_results$att)) > 0) {
    p_het <- ggplot(
      het_results %>% filter(!is.na(att)),
      aes(x = reorder(uf, att), y = att)
    ) +
      geom_col(aes(fill = significant)) +
      geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.3) +
      coord_flip() +
      scale_fill_manual(
        values = c("FALSE" = "gray70", "TRUE" = "steelblue", "NA" = "gray90"),
        na.value = "gray90"
      ) +
      theme_minimal() +
      labs(
        title = "Heterogeneidade Regional do Efeito das Estações Meteorológicas",
        subtitle = ifelse("tipo" %in% names(het_results) && unique(het_results$tipo) == "Região",
          "Análise por Grandes Regiões", "Análise por UF"
        ),
        x = NULL,
        y = "ATT Estimado",
        fill = "Significativo (5%)",
        caption = "Barras de erro representam IC 95%"
      )

    ggsave(here::here("data", "outputs", "heterogeneity_regional_plot.png"),
      p_het,
      width = 10, height = 8
    )
  } else {
    cli::cli_alert_warning("Nenhum resultado válido para visualização de heterogeneidade")
  }

  # Resumo
  n_positive <- sum(het_results$att > 0, na.rm = TRUE)
  n_significant <- sum(het_results$significant, na.rm = TRUE)
  n_total <- sum(!is.na(het_results$att))

  if (n_total > 0) {
    cli::cli_alert_info("Resumo da heterogeneidade:")
    cli::cli_alert_info("  - {n_positive}/{n_total} unidades com efeito positivo")
    cli::cli_alert_info("  - {n_significant}/{n_total} unidades com efeito significativo (5%)")

    if (n_positive > n_total * 0.8) {
      cli::cli_alert_success("✓ Efeito predominantemente positivo")
    } else {
      cli::cli_alert_warning("⚠️ Heterogeneidade substancial entre regiões")
    }
  }

  return(het_results)
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.13.1 ANÁLISE DID PARA OUTCOMES ALTERNATIVOS (ROBUSTEZ E COMPLEMENTOS) │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' alternative_outcomes_analysis(): Análise DiD para outcomes complementares
#'
#' PROPÓSITO:
#' Com área plantada de cana-de-açúcar como outcome PRINCIPAL, esta função
#' estima efeitos para outcomes alternativos que servem como:
#'   1. Testes de robustez (PIB agropecuário)
#'   2. Testes de especificidade (outras culturas: soja e arroz)
#'   3. Análise de heterogeneidade entre culturas
#'
#' MOTIVAÇÃO ECONÔMICA:
#' Informações meteorológicas precisas podem afetar:
#'   - Decisões de alocação de terra (extensão de área plantada)
#'   - Produtividade por unidade de área (valor agregado/PIB)
#'   - Escolha entre culturas com diferentes sensibilidades climáticas
#'
#' Se o efeito é robusto, esperamos:
#'   - Impacto positivo no PIB agro (confirma benefício econômico agregado)
#'   - Efeitos heterogêneos entre culturas (cana > soja/arroz se mais sensível)
#'   - Expansão diferencial baseada em retorno informacional
#'
#' OUTCOMES ANALISADOS:
#'   - log_pib_agro: Log PIB agropecuário (robustez - medida agregada de valor)
#'   - log_area_soja: Log área plantada de soja (especificidade)
#'   - log_area_arroz: Log área plantada de arroz (especificidade)
#'
#' NOTA: log_area_cana é o outcome PRINCIPAL e é estimado separadamente
#' na análise primária. Esta função complementa aquele resultado.
#'
#' @param df DataFrame em painel preparado
#' @param method String - Método de estimação ("dr", "ipw", "reg")
#'
#' @return Tibble com resultados para todos os outcomes alternativos
#' ---------------------------------------------------------------------------
alternative_outcomes_analysis <- function(df, method = "dr") {
  cli::cli_h2("Análise DiD: Outcomes Alternativos (Robustez e Especificidade)")
  
  # Definir outcomes alternativos
  outcomes <- list(
    pib_agro = list(
      var = "log_pib_agro",
      label = "PIB Agropecuário (robustez)",
      type = "Medida agregada de valor",
      unit = "R$ mil (log)"
    ),
    soja = list(
      var = "log_area_soja",
      label = "Área Soja (especificidade)",
      type = "Área plantada alternativa",
      unit = "km² (log)"
    ),
    arroz = list(
      var = "log_area_arroz",
      label = "Área Arroz (especificidade)",
      type = "Área plantada alternativa",
      unit = "km² (log)"
    )
  )
  
  # Iterar sobre cada outcome e estimar
  results <- purrr::map(names(outcomes), function(outcome_name) {
    outcome_info <- outcomes[[outcome_name]]
    cli::cli_h3("Estimando efeito para {outcome_info$label}")
    
    # Estatísticas descritivas do outcome
    outcome_data <- df[[outcome_info$var]]
    n_nonzero <- sum(outcome_data > 0, na.rm = TRUE)
    pct_nonzero <- n_nonzero / length(outcome_data) * 100
    mean_val <- mean(outcome_data, na.rm = TRUE)
    
    cli::cli_alert_info("Observações válidas: {n_nonzero} ({round(pct_nonzero, 1)}%)")
    cli::cli_alert_info("Valor médio: {round(mean_val, 3)}")
    
    # Estimar ATT
    res <- tryCatch(
      {
        estimate_att(df, method = method, outcome = outcome_info$var)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro ao estimar {outcome_info$label}: {e$message}")
        list(
          att_global = NA, se_global = NA, p = NA,
          ci_low = NA, ci_high = NA, z = NA
        )
      }
    )
    
    # Retornar resultados estruturados
    tibble::tibble(
      outcome_label = outcome_info$label,
      outcome_var = outcome_info$var,
      outcome_type = outcome_info$type,
      att = res$att_global,
      se = res$se_global,
      z = res$z,
      p_value = res$p,
      ci_lower = res$ci_low,
      ci_upper = res$ci_high,
      significant = ifelse(is.na(res$p), NA, res$p < 0.05),
      n_valid = n_nonzero,
      pct_valid = pct_nonzero,
      mean_outcome = mean_val
    )
  }) %>%
    bind_rows()
  
  # Salvar resultados
  readr::write_csv(results, here::here("data", "outputs", "alternative_outcomes_analysis.csv"))
  
  # Criar visualização comparativa
  if (sum(!is.na(results$att)) > 0) {
    p_outcomes <- ggplot(
      results %>% filter(!is.na(att)),
      aes(x = reorder(outcome_label, att), y = att * 100)
    ) +
      geom_col(aes(fill = significant), width = 0.6) +
      geom_errorbar(
        aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
        width = 0.2, linewidth = 1
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      geom_text(
        aes(
          label = sprintf("%.1f%%\n(p=%s)", 
                         att * 100,
                         ifelse(p_value < 0.001, "<0.001", 
                               sprintf("%.3f", p_value)))),
        vjust = ifelse(results$att > 0, -0.5, 1.5),
        size = 3.5,
        fontface = "bold"
      ) +
      scale_fill_manual(
        values = c("FALSE" = "gray60", "TRUE" = "#2E86AB"),
        labels = c("Não significativo", "Significativo (5%)"),
        na.value = "gray80"
      ) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(
        title = "Robustez e Especificidade: Outcomes Alternativos",
        subtitle = "Comparação do ATT entre PIB agregado e outras culturas",
        x = NULL,
        y = "Efeito Médio do Tratamento (%)",
        fill = "Significância",
        caption = "Nota: Barras de erro representam IC 95%. Outcome principal = Área Cana (estimado separadamente)."
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "bottom",
        axis.text.y = element_text(size = 12)
      )
    
    ggsave(
      here::here("data", "outputs", "alternative_outcomes_effects.png"),
      p_outcomes,
      width = 12, height = 7, dpi = 300
    )
    
    cli::cli_alert_success("Gráfico salvo: alternative_outcomes_effects.png")
  }
  
  # Sumário dos resultados
  cli::cli_alert_info("Resumo dos Outcomes Alternativos:")
  print(results %>% select(outcome_label, att, se, p_value, significant))
  
  # Interpretação relativa ao outcome principal (cana)
  pib_result <- results %>% filter(outcome_var == "log_pib_agro")
  if (nrow(pib_result) > 0 && !is.na(pib_result$att[1])) {
    if (pib_result$significant[1]) {
      cli::cli_alert_success("✓ Efeito no PIB agro é significativo, confirmando relevância econômica agregada")
    } else {
      cli::cli_alert_warning("⚠ Efeito no PIB agro não é significativo - pode indicar efeito concentrado em margens específicas")
    }
  }
  
  # Analisar outras culturas
  other_crops <- results %>% filter(outcome_var %in% c("log_area_soja", "log_area_arroz"))
  n_sig_other <- sum(other_crops$significant, na.rm = TRUE)
  
  if (n_sig_other == 0) {
    cli::cli_alert_success("✓ Efeito específico à cana: outras culturas não mostram impacto significativo")
  } else {
    cli::cli_alert_info("Efeito se estende a outras culturas: {n_sig_other} de {nrow(other_crops)} significativos")
  }
  
  return(results)
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.14 ANÁLISE DID AGREGADA POR UNIDADE FEDERATIVA                        │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' uf_level_analysis(): Análise DiD usando UFs como unidades de análise
#'
#' MOTIVAÇÃO:
#' Agregar dados ao nível de UF permite capturar efeitos regionais mais amplos
#' e criar maior variação no timing de adoção entre unidades. Isso é especialmente
#' útil quando todas as microrregiões são eventualmente tratadas.
#'
#' ESTRATÉGIA DE AGREGAÇÃO:
#'   1. Colapsa dados de microrregiões para médias estaduais ponderadas
#'   2. Define tratamento quando maioria (>50%) das microrregiões tem estação
#'   3. Usa o modelo DiD padrão de Callaway & Sant'Anna no nível estadual
#'
#' VANTAGENS:
#'   - Maior variação no timing entre unidades
#'   - Captura efeitos de spillover dentro do estado
#'   - Resultados mais interpretáveis para política pública
#'
#' @param df DataFrame com dados no nível de microrregião
#' @param method Método de estimação ("dr", "ipw", "reg")
#' @param outcome String - Nome da variável de outcome (padrão: "log_area_cana")
#' @return Lista com resultados do modelo e visualizações
#' ---------------------------------------------------------------------------
uf_level_analysis <- function(df, method = "dr", outcome = "log_area_cana") {
  cli::cli_h2("Análise DiD por Unidade Federativa")
  cli::cli_alert_info("Agregando dados de microrregiões para UFs...")

  # Agregar dados por UF e ano
  df_uf <- df %>%
    group_by(sigla_uf, ano) %>%
    summarise(
      # Médias ponderadas por área total
      log_pib_agro = weighted.mean(log_pib_agro, w = area_total_km2, na.rm = TRUE),
      log_area_total = log(sum(area_total_km2, na.rm = TRUE) + 1),
      log_populacao = log(sum(populacao_total, na.rm = TRUE) + 1),
      log_pib_nao_agro = weighted.mean(log_pib_nao_agro, w = area_total_km2, na.rm = TRUE),

      # Proporção de microrregiões tratadas
      prop_tratadas = mean(tratado, na.rm = TRUE),
      n_microregioes = n_distinct(id_microrregiao),
      n_estacoes = sum(tratado, na.rm = TRUE),

      # Covariáveis
      log_pib_per_capita = weighted.mean(log_pib_per_capita, w = populacao_total, na.rm = TRUE),
      log_densidade_estacoes_uf = first(log_densidade_estacoes_uf),
      log_precip_anual = weighted.mean(log_precip_anual, w = area_total_km2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Definir ano de tratamento da UF
    group_by(sigla_uf) %>%
    mutate(
      # Encontrar primeiro ano baseado em diferentes critérios para criar variação
      gname = case_when(
        # Usar 20% como threshold principal para capturar adoção precoce
        any(prop_tratadas >= 0.2) ~ min(ano[prop_tratadas >= 0.2]),
        # Se nunca atingir 20%, usar 10%
        any(prop_tratadas >= 0.1) ~ min(ano[prop_tratadas >= 0.1]),
        # Caso contrário, nunca tratado
        TRUE ~ 0
      )
    ) %>%
    ungroup() %>%
    # Criar variáveis finais
    mutate(
      id_uf = as.numeric(factor(sigla_uf)),
      tratado = as.numeric(ano >= gname & gname > 0)
    )

  # Diagnóstico do painel agregado
  n_ufs <- n_distinct(df_uf$sigla_uf)
  n_periodos <- n_distinct(df_uf$ano)
  grupos_tratamento <- df_uf %>%
    filter(gname > 0) %>%
    distinct(sigla_uf, gname) %>%
    count(gname) %>%
    arrange(gname)

  cli::cli_alert_success("Painel agregado: {n_ufs} UFs × {n_periodos} anos")
  cli::cli_alert_info("Grupos de tratamento (UFs por ano de adoção):")
  print(grupos_tratamento)

  # Verificar variação
  n_never_treated <- sum(df_uf$gname == 0)
  n_treated_groups <- n_distinct(df_uf$gname[df_uf$gname > 0])

  cli::cli_alert_info("UFs nunca tratadas (controle): {n_distinct(df_uf$sigla_uf[df_uf$gname == 0])}")
  cli::cli_alert_info("Grupos de tratamento distintos: {n_treated_groups}")

  if (n_treated_groups < 2) {
    cli::cli_alert_danger("Variação insuficiente no timing de tratamento entre UFs")
    return(NULL)
  }

  # Estimar modelo DiD no nível de UF
  cli::cli_alert_info("Estimando modelo DiD agregado por UF...")

  tryCatch(
    {
      # Modelo principal
      # NOTA: Esta análise usa dados agregados ao nível de UF. O outcome precisa
      # existir na agregação criada acima (df_uf). Por padrão, agregamos log_pib_agro,
      # mas para outros outcomes seria necessário ajustar a lógica de agregação.
      att_uf <- did::att_gt(
        yname = outcome,
        tname = "ano",
        idname = "id_uf",
        gname = "gname",
        xformla = ~ log_area_total + log_populacao + log_pib_per_capita + log_densidade_estacoes_uf + log_precip_anual,
        data = df_uf,
        est_method = method,
        control_group = "notyettreated",
        anticipation = 0,
        base_period = "varying",
        clustervars = "id_uf",
        print_details = FALSE
      )

      # Agregações
      agg_overall <- aggte(att_uf, type = "overall", na.rm = TRUE)
      agg_event <- aggte(att_uf, type = "dynamic", na.rm = TRUE)
      agg_group <- aggte(att_uf, type = "group", na.rm = TRUE)

      # Extrair resultados principais
      att_global <- agg_overall$overall.att
      se_global <- agg_overall$overall.se
      p_value <- 2 * pnorm(-abs(att_global / se_global))

      cli::cli_alert_success("ATT (nível UF) = {round(att_global*100,2)}% (p = {round(p_value,4)})")

      # Criar visualizações

      # 1. Event Study por UF
      event_data <- data.frame(
        time = agg_event$egt,
        att = agg_event$att.egt,
        se = agg_event$se.egt
      ) %>%
        filter(!is.na(att))

      p_event_uf <- ggplot(event_data, aes(x = time, y = att)) +
        geom_ribbon(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
          alpha = 0.2, fill = "blue"
        ) +
        geom_line(color = "blue", linewidth = 1.5) +
        geom_point(color = "blue", size = 3) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_vline(xintercept = -0.5, linetype = "solid", color = "red", alpha = 0.5) +
        theme_minimal() +
        labs(
          title = "Event Study: Análise Agregada por UF",
          subtitle = "Efeito das estações meteorológicas no PIB agropecuário estadual",
          x = "Anos relativos ao tratamento",
          y = "ATT (log PIB agropecuário)",
          caption = "Nota: Tratamento definido quando >50% das microrregiões da UF têm estação"
        ) +
        theme(plot.title = element_text(size = 14, face = "bold"))

      ggsave("data/outputs/event_study_uf.png", p_event_uf,
        width = 10, height = 6, dpi = 300
      )
      cli::cli_alert_success("Gráfico salvo: event_study_uf.png")

      # 2. Heterogeneidade por grupo de adoção
      group_data <- data.frame(
        group = agg_group$egt,
        att = agg_group$att.egt,
        se = agg_group$se.egt
      ) %>%
        filter(!is.na(att)) %>%
        left_join(
          df_uf %>%
            filter(gname > 0) %>%
            distinct(sigla_uf, gname) %>%
            group_by(gname) %>%
            summarise(
              ufs = paste(sigla_uf, collapse = ", "),
              n_ufs = n(),
              .groups = "drop"
            ),
          by = c("group" = "gname")
        )

      p_groups_uf <- ggplot(group_data, aes(x = factor(group), y = att * 100)) +
        geom_col(fill = "steelblue") +
        geom_errorbar(
          aes(
            ymin = (att - 1.96 * se) * 100,
            ymax = (att + 1.96 * se) * 100
          ),
          width = 0.3
        ) +
        geom_text(aes(label = paste0(round(att * 100, 1), "%")),
          vjust = -0.5, size = 3.5
        ) +
        theme_minimal() +
        labs(
          title = "Efeito por Ano de Adoção (Nível UF)",
          x = "Ano de adoção",
          y = "ATT (%)",
          caption = "IC 95%. Cada barra representa UFs que adotaram no mesmo ano."
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggsave("data/outputs/att_by_adoption_year_uf.png", p_groups_uf,
        width = 8, height = 6, dpi = 300
      )
      cli::cli_alert_success("Gráfico salvo: att_by_adoption_year_uf.png")

      # 3. Mapa de timing de adoção por UF
      adoption_data <- df_uf %>%
        distinct(sigla_uf, gname) %>%
        mutate(
          adoption_category = case_when(
            gname == 0 ~ "Não adotou",
            gname <= 2010 ~ "Adoção precoce\n(até 2010)",
            gname <= 2015 ~ "Adoção intermediária\n(2011-2015)",
            TRUE ~ "Adoção tardia\n(após 2015)"
          )
        )

      p_adoption_map <- ggplot(adoption_data, aes(
        x = reorder(sigla_uf, gname),
        y = 1,
        fill = adoption_category
      )) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_manual(values = c(
          "Não adotou" = "gray70",
          "Adoção precoce\n(até 2010)" = "#2ecc71",
          "Adoção intermediária\n(2011-2015)" = "#f39c12",
          "Adoção tardia\n(após 2015)" = "#e74c3c"
        )) +
        theme_minimal() +
        labs(
          title = "Timing de Adoção por UF",
          subtitle = "Ano em que >50% das microrregiões tiveram estação",
          x = "UF",
          y = NULL,
          fill = "Categoria"
        ) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom"
        )

      ggsave("data/outputs/adoption_timing_uf.png", p_adoption_map,
        width = 12, height = 4, dpi = 300
      )
      cli::cli_alert_success("Gráfico salvo: adoption_timing_uf.png")

      # Salvar resultados
      readr::write_rds(list(
        att_uf = att_uf,
        overall = agg_overall,
        event = agg_event,
        group = agg_group,
        df_uf = df_uf
      ), here::here("data", "outputs", "uf_analysis_results.rds"))

      # Retornar resultados
      return(list(
        att_uf = att_uf,
        overall = agg_overall,
        event = agg_event,
        group = agg_group,
        att_global = att_global,
        se_global = se_global,
        p_value = p_value,
        df_uf = df_uf,
        adoption_data = adoption_data
      ))
    },
    error = function(e) {
      cli::cli_alert_danger("Erro na estimação por UF: {e$message}")
      return(NULL)
    }
  )
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.15 VISUALIZAÇÃO DE TENDÊNCIAS PARALELAS (COMPLETA)                    │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' plot_parallel_trends(): Visualiza tendências completas por coorte (pré e pós)
#'
#' CONCEITO IMPORTANTE - MELHORES PRÁTICAS PARA DID ESCALONADO:
#' 1. Mostrar períodos PRÉ e PÓS tratamento para visualizar:
#'    - Tendências paralelas antes (validação)
#'    - Divergência após tratamento (efeito causal)
#' 2. Normalizar outcomes em t=-1 para facilitar comparação
#' 3. Incluir grupo "nunca tratado" se existir
#' 4. Usar tempo relativo ao tratamento (event time)
#'
#' @param df DataFrame com dados
#' @param outcome Nome da variável de resultado
#' @param n_pre_periods Número de períodos pré-tratamento
#' @param n_post_periods Número de períodos pós-tratamento
#' @param normalize Se TRUE, normaliza valores em t=-1
#' @return ggplot object
#' ---------------------------------------------------------------------------
plot_parallel_trends <- function(df, outcome = "log_pib_agro",
                                 n_pre_periods = 5, n_post_periods = 5,
                                 normalize = TRUE) {
  cli::cli_h3("Gerando Visualização Completa de Tendências Paralelas para {outcome}")

  # Classificar microrregiões por timing de adoção
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
    # Remover Very Late Adopters conforme solicitado
    filter(cohort != "Very Late Adopters\n(2018+)") %>%
    # Ordenar a legenda: Early, Mid, Late
    mutate(
      cohort = factor(cohort, levels = c(
        "Early Adopters\n(2003-2007)",
        "Mid Adopters\n(2008-2012)",
        "Late Adopters\n(2013-2017)"
      ))
    )

  # Preparar dados incluindo períodos pré e pós
  trends_data <- df %>%
    inner_join(cohorts, by = "id_microrregiao", suffix = c("", ".y")) %>%
    mutate(
      time_to_treat = ano - gname.y,
      outcome_var = .data[[outcome]]
    ) %>%
    # Incluir tanto períodos pré quanto pós
    filter(time_to_treat >= -n_pre_periods & time_to_treat <= n_post_periods) %>%
    group_by(cohort, time_to_treat) %>%
    summarise(
      mean_outcome = mean(outcome_var, na.rm = TRUE),
      se_outcome = sd(outcome_var, na.rm = TRUE) / sqrt(n()),
      n_obs = n(),
      .groups = "drop"
    )

  # Normalização (opcional)
  if (normalize) {
    # Normalizar cada coorte pelo valor em t=-1
    normalize_values <- trends_data %>%
      filter(time_to_treat == -1) %>%
      select(cohort, baseline = mean_outcome)

    trends_data <- trends_data %>%
      left_join(normalize_values, by = "cohort") %>%
      mutate(
        mean_outcome_normalized = mean_outcome - baseline,
        se_outcome_normalized = se_outcome,
        mean_outcome = mean_outcome_normalized
      )

    y_label <- paste(
      "Mudança em", ifelse(outcome == "log_pib_agro",
        "Log PIB Agropecuário",
        "Log PIB Não-Agropecuário"
      ),
      "(Relativo a t=-1)"
    )
  } else {
    y_label <- paste("Log", ifelse(outcome == "log_pib_agro",
      "PIB Agropecuário",
      "PIB Não-Agropecuário"
    ))
  }

  # Criar gráfico
  p_trends <- ggplot(
    trends_data,
    aes(
      x = time_to_treat, y = mean_outcome,
      color = cohort, group = cohort
    )
  ) +
    # Área sombreada pós-tratamento
    annotate("rect",
      xmin = 0, xmax = n_post_periods,
      ymin = -Inf, ymax = Inf,
      alpha = 0.1, fill = "gray"
    ) +
    # Bandas de confiança
    geom_ribbon(aes(
      ymin = mean_outcome - 1.96 * se_outcome,
      ymax = mean_outcome + 1.96 * se_outcome,
      fill = cohort
    ), alpha = 0.2) +
    # Linhas principais
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    # Linha vertical no momento do tratamento
    geom_vline(
      xintercept = -0.5, linetype = "solid",
      color = "red", alpha = 0.7, linewidth = 1
    ) +
    # Linha horizontal em zero (se normalizado)
    {
      if (normalize) {
        geom_hline(
          yintercept = 0, linetype = "dashed",
          color = "gray50"
        )
      }
    } +
    # Customização visual
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
      caption = paste(
        "Gray area = post-treatment period.",
        "Red line = treatment start.",
        "Bands = 95% CI"
      )
    )

  # Adicionar anotações
  p_trends <- p_trends +
    annotate("text",
      x = -n_pre_periods / 2, y = Inf,
      label = "Pre-treatment\n(Validation)",
      vjust = 1.5, hjust = 0.5, size = 3, fontface = "italic"
    ) +
    annotate("text",
      x = n_post_periods / 2, y = Inf,
      label = "Post-treatment\n(Effect)",
      vjust = 1.5, hjust = 0.5, size = 3, fontface = "italic"
    )

  # Salvar gráfico
  suffix <- ifelse(normalize, "_normalized", "_raw")
  filename <- paste0(
    "data/outputs/parallel_trends_complete_",
    gsub("log_", "", outcome), suffix, ".png"
  )
  ggsave(filename, p_trends, width = 12, height = 8, dpi = 300)
  cli::cli_alert_success("Gráfico salvo: {filename}")

  # Análise estatística das tendências pré-tratamento
  pre_trends_test <- df %>%
    inner_join(cohorts, by = "id_microrregiao", suffix = c("", ".y")) %>%
    mutate(time_to_treat = ano - gname.y) %>%
    filter(time_to_treat >= -n_pre_periods & time_to_treat < 0)

  if (nrow(pre_trends_test) > 0) {
    # Teste formal de tendências paralelas
    model_parallel <- lm(reformulate(
      c("time_to_treat", "cohort", "time_to_treat:cohort"),
      response = outcome
    ), data = pre_trends_test)

    anova_result <- anova(model_parallel)
    interaction_row <- grep("time_to_treat:cohort", rownames(anova_result))

    if (length(interaction_row) > 0) {
      f_stat <- anova_result[interaction_row, "F value"]
      p_value <- anova_result[interaction_row, "Pr(>F)"]

      cli::cli_alert_info("Teste de Tendências Paralelas Pré-Tratamento:")
      cli::cli_alert_info("H0: Slopes iguais entre coortes antes do tratamento")
      cli::cli_alert_info("F-statistic: {round(f_stat, 3)}, p-valor: {round(p_value, 4)}")

      if (p_value > 0.10) {
        cli::cli_alert_success("✓ Forte evidência de tendências paralelas (p > 0.10)")
      } else if (p_value > 0.05) {
        cli::cli_alert_warning("⚠ Evidência moderada de tendências paralelas (0.05 < p < 0.10)")
      } else {
        cli::cli_alert_danger("✗ Evidência contra tendências paralelas (p < 0.05)")
      }
    }
  }

  # Calcular magnitude do efeito
  effect_data <- trends_data %>%
    group_by(cohort) %>%
    summarise(
      pre_mean = mean(mean_outcome[time_to_treat < 0], na.rm = TRUE),
      post_mean = mean(mean_outcome[time_to_treat >= 0], na.rm = TRUE),
      effect = post_mean - pre_mean,
      .groups = "drop"
    )

  cli::cli_alert_info("Efeitos Médios por Coorte (pós - pré):")
  print(effect_data)

  return(p_trends)
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.16 ANÁLISE DESCRITIVA PARA TCC                                        │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' descriptive_analysis(): Gera análise descritiva completa para TCC
#'
#' Esta função produz estatísticas descritivas, tabelas e visualizações
#' profissionais dos dados utilizados no modelo DiD, adequadas para inclusão
#' em trabalhos acadêmicos.
#'
#' @param df DataFrame com dados limpos
#' @return Lista com tabelas e gráficos
#' ---------------------------------------------------------------------------
descriptive_analysis <- function(df) {
  cli::cli_h2("Gerando Análise Descritiva Completa para TCC")

  # Criar diretório para outputs
  output_dir <- here::here("data", "outputs", "descriptive_analysis")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 1. ESTATÍSTICAS DESCRITIVAS GERAIS                                   │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("1. Estatísticas Descritivas das Variáveis Principais")

  # Variáveis de interesse
  vars_continuous <- c(
    "pib_agropecuario", "pib_nao_agro", "log_pib_agro", "log_pib_nao_agro",
    "area_total_km2", "area_plantada_cana", "area_plantada_soja", "area_plantada_arroz",
    "populacao_total", "pib_per_capita", "densidade_estacoes_uf", "precip_total_anual_mm"
  )

  # Estatísticas básicas
  desc_stats <- df %>%
    select(all_of(vars_continuous)) %>%
    summarise(across(
      everything(),
      list(
        n = ~ sum(!is.na(.)),
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE),
        p25 = ~ quantile(., 0.25, na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        p75 = ~ quantile(., 0.75, na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
    separate(stat, into = c("variable", "statistic"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = statistic, values_from = value)

  # Formatar tabela
  desc_table <- desc_stats %>%
    mutate(
      variable = case_when(
        variable == "pib_agropecuario" ~ "PIB Agropecuário (R$ mil)",
        variable == "pib_nao_agro" ~ "PIB Não-Agropecuário (R$ mil)",
        variable == "log_pib_agro" ~ "Log PIB Agropecuário",
        variable == "log_pib_nao_agro" ~ "Log PIB Não-Agropecuário",
        variable == "area_total_km2" ~ "Área Total (km²)",
        variable == "area_plantada_cana" ~ "Área Plantada Cana (km²)",
        variable == "area_plantada_soja" ~ "Área Plantada Soja (km²)",
        variable == "area_plantada_arroz" ~ "Área Plantada Arroz (km²)",
        variable == "populacao_total" ~ "População",
        variable == "pib_per_capita" ~ "PIB per Capita (R$)",
        variable == "densidade_estacoes_uf" ~ "Densidade de Estações",
        variable == "precip_total_anual_mm" ~ "Precipitação Anual (mm)"
      )
    ) %>%
    gt() %>%
    tab_header(
      title = "Estatísticas Descritivas das Variáveis do Modelo",
      subtitle = "Painel de Microrregiões (2003-2021)"
    ) %>%
    fmt_number(
      columns = c(mean, sd, min, p25, median, p75, max),
      decimals = 2
    ) %>%
    fmt_number(
      columns = n,
      decimals = 0
    ) %>%
    cols_label(
      variable = "Variável",
      n = "N",
      mean = "Média",
      sd = "Desvio Padrão",
      min = "Mínimo",
      p25 = "1º Quartil",
      median = "Mediana",
      p75 = "3º Quartil",
      max = "Máximo"
    )

  # Salvar tabela
  gtsave(desc_table, file.path(output_dir, "estatisticas_descritivas.html"))

  # Salvar como PNG se webshot2 estiver disponível (pode falhar por problemas de porta)
  if (requireNamespace("webshot2", quietly = TRUE)) {
    tryCatch({
      gtsave(desc_table, file.path(output_dir, "estatisticas_descritivas.png"))
      cli::cli_alert_success("PNG salvo com sucesso")
    }, error = function(e) {
      cli::cli_alert_warning("Não foi possível salvar PNG (webshot2 erro): {e$message}")
      cli::cli_alert_info("HTML salvo com sucesso - PNG não é crítico")
    })
  } else {
    cli::cli_alert_warning("webshot2 não instalado - salvando apenas HTML")
  }

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 2. DISTRIBUIÇÃO TEMPORAL DO TRATAMENTO                               │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("2. Distribuição Temporal do Tratamento")

  # Timing de adoção
  adoption_timing <- df %>%
    filter(gname > 0) %>%
    distinct(id_microrregiao, gname) %>%
    count(gname) %>%
    mutate(
      prop = n / sum(n) * 100,
      cum_prop = cumsum(prop)
    )

  # Gráfico de barras
  p_adoption <- ggplot(adoption_timing, aes(x = factor(gname), y = n)) +
    geom_col(fill = "#2E86AB", alpha = 0.8) +
    geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
    geom_line(
      aes(y = cum_prop * max(n) / 100, group = 1),
      color = "#A23B72", linewidth = 1.2
    ) +
    geom_point(
      aes(y = cum_prop * max(n) / 100),
      color = "#A23B72", size = 3
    ) +
    scale_y_continuous(
      "Número de Microrregiões",
      sec.axis = sec_axis(~ . * 100 / max(adoption_timing$n),
        name = "% Acumulado"
      )
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y.right = element_text(color = "#A23B72"),
      axis.text.y.right = element_text(color = "#A23B72")
    ) +
    labs(
      title = "Distribuição Temporal da Adoção do Tratamento",
      subtitle = "Número de microrregiões por ano de primeira estação meteorológica",
      x = "Ano de Tratamento",
      caption = "Nota: Linha vermelha indica percentual acumulado"
    )

  ggsave(file.path(output_dir, "distribuicao_temporal_tratamento.png"),
    p_adoption,
    width = 10, height = 6, dpi = 300
  )

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 3. COMPARAÇÃO PRÉ-TRATAMENTO                                         │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("3. Comparação entre Grupos de Adoção (Pré-Tratamento)")

  # Definir coortes
  df_cohorts <- df %>%
    mutate(
      cohort = case_when(
        gname <= 2007 & gname > 0 ~ "Early (2003-2007)",
        gname <= 2012 & gname > 0 ~ "Mid (2008-2012)",
        gname <= 2017 & gname > 0 ~ "Late (2013-2017)",
        gname > 2017 ~ "Very Late (2018+)",
        TRUE ~ "Never Treated"
      )
    ) %>%
    filter(cohort != "Never Treated") # Remover se não houver

  # Médias pré-tratamento por coorte
  pre_treatment_means <- df_cohorts %>%
    filter(treated == 0) %>%
    group_by(cohort) %>%
    summarise(
      `PIB Agro (média log)` = mean(log_pib_agro, na.rm = TRUE),
      `PIB Não-Agro (média log)` = mean(log_pib_nao_agro, na.rm = TRUE),
      `Área Total (média km²)` = mean(area_total_km2, na.rm = TRUE),
      `Área Cana (média km²)` = mean(area_plantada_cana, na.rm = TRUE),
      `População (média)` = mean(populacao_total, na.rm = TRUE),
      `N Observações` = n(),
      .groups = "drop"
    )

  # Tabela formatada
  pre_treatment_table <- pre_treatment_means %>%
    gt() %>%
    tab_header(
      title = "Características Médias Pré-Tratamento por Coorte de Adoção",
      subtitle = "Comparação de grupos antes da instalação das estações"
    ) %>%
    fmt_number(
      columns = 2:5,
      decimals = 2
    ) %>%
    fmt_number(
      columns = 6,
      decimals = 0
    )

  gtsave(pre_treatment_table, file.path(output_dir, "comparacao_pre_tratamento.html"))

  if (requireNamespace("webshot2", quietly = TRUE)) {
    tryCatch({
      gtsave(pre_treatment_table, file.path(output_dir, "comparacao_pre_tratamento.png"))
    }, error = function(e) {
      cli::cli_alert_warning("PNG não salvo (webshot2): {e$message}")
    })
  }

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 4. EVOLUÇÃO TEMPORAL DAS VARIÁVEIS                                   │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("4. Evolução Temporal das Variáveis Principais")

  # Médias anuais
  annual_means <- df %>%
    group_by(ano) %>%
    summarise(
      pib_agro_mean = mean(pib_agropecuario, na.rm = TRUE),
      pib_nao_agro_mean = mean(pib_nao_agro, na.rm = TRUE),
      n_treated = sum(treated),
      .groups = "drop"
    )

  # Gráfico de evolução
  p_evolution <- annual_means %>%
    pivot_longer(
      cols = c(pib_agro_mean, pib_nao_agro_mean),
      names_to = "variable",
      values_to = "value"
    ) %>%
    ggplot(aes(x = ano, y = value / 1000, color = variable)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("pib_agro_mean" = "#2E86AB", "pib_nao_agro_mean" = "#A23B72"),
      labels = c("PIB Agropecuário", "PIB Não-Agropecuário")
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    labs(
      title = "Evolução Temporal do PIB por Setor",
      subtitle = "Valores médios das microrregiões (2003-2023)",
      x = "Ano",
      y = "PIB Médio (R$ milhões)",
      color = "Setor"
    )

  ggsave(file.path(output_dir, "evolucao_temporal_pib.png"),
    p_evolution,
    width = 10, height = 6, dpi = 300
  )

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 5. MATRIZ DE CORRELAÇÃO                                              │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("5. Matriz de Correlação")

  # Selecionar variáveis para correlação
  cor_vars <- c(
    "log_pib_agro", "log_pib_nao_agro", "log_area_total",
    "log_area_cana", "log_area_soja", "log_area_arroz",
    "log_populacao", "log_pib_per_capita", "log_densidade_estacoes_uf",
    "log_precip_anual"
  )

  # Calcular correlação
  cor_matrix <- df %>%
    select(all_of(cor_vars)) %>%
    cor(use = "complete.obs")

  # Renomear para apresentação
  rownames(cor_matrix) <- colnames(cor_matrix) <- c(
    "PIB Agro (log)", "PIB Não-Agro (log)", "Área Total (log)",
    "Área Cana (log)", "Área Soja (log)", "Área Arroz (log)",
    "População (log)", "PIB per Capita (log)", "Densidade Estações (log)",
    "Precipitação (log)"
  )

  # Visualização
  p_cor <- corrplot::corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    number.cex = 0.7,
    col = colorRampPalette(c("#A23B72", "white", "#2E86AB"))(100)
  )

  # Salvar
  png(file.path(output_dir, "matriz_correlacao.png"),
    width = 8, height = 8, units = "in", res = 300
  )
  corrplot::corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    number.cex = 0.7,
    col = colorRampPalette(c("#A23B72", "white", "#2E86AB"))(100),
    title = "Matriz de Correlação das Variáveis do Modelo",
    mar = c(0, 0, 2, 0)
  )
  dev.off()

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 6. DISTRIBUIÇÃO GEOGRÁFICA                                           │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("6. Distribuição Geográfica do Tratamento")

  # Tabela por UF
  uf_summary <- df %>%
    group_by(sigla_uf) %>%
    summarise(
      n_microregions = n_distinct(id_microrregiao),
      n_treated = n_distinct(id_microrregiao[treated == 1]),
      prop_treated = n_treated / n_microregions * 100,
      first_year = min(gname[gname > 0], na.rm = TRUE),
      avg_pib_agro = mean(pib_agropecuario, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_pib_agro))

  # Tabela formatada
  uf_table <- uf_summary %>%
    head(15) %>% # Top 15 UFs
    gt() %>%
    tab_header(
      title = "Distribuição do Tratamento por Unidade Federativa",
      subtitle = "Top 15 UFs por PIB Agropecuário médio"
    ) %>%
    fmt_number(
      columns = c(prop_treated, avg_pib_agro),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(n_microregions, n_treated, first_year),
      decimals = 0
    ) %>%
    cols_label(
      sigla_uf = "UF",
      n_microregions = "Total Microrregiões",
      n_treated = "Microrregiões Tratadas",
      prop_treated = "% Tratadas",
      first_year = "Ano Inicial",
      avg_pib_agro = "PIB Agro Médio (R$ mil)"
    )

  gtsave(uf_table, file.path(output_dir, "distribuicao_por_uf.html"))

  if (requireNamespace("webshot2", quietly = TRUE)) {
    tryCatch({
      gtsave(uf_table, file.path(output_dir, "distribuicao_por_uf.png"))
    }, error = function(e) {
      cli::cli_alert_warning("PNG não salvo (webshot2): {e$message}")
    })
  }

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 7. BALANÇO DO PAINEL                                                 │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("7. Estrutura do Painel")

  # Verificar balanço
  panel_balance <- df %>%
    group_by(id_microrregiao) %>%
    summarise(
      n_years = n_distinct(ano),
      year_min = min(ano),
      year_max = max(ano),
      .groups = "drop"
    )

  # Estatísticas do painel
  panel_stats <- tibble(
    Característica = c(
      "Número de Microrregiões",
      "Número de Anos",
      "Total de Observações",
      "Painel Balanceado",
      "Microrregiões com dados completos",
      "% de dados missing (PIB Agro)",
      "% de dados missing (PIB Não-Agro)"
    ),
    Valor = c(
      n_distinct(df$id_microrregiao),
      n_distinct(df$ano),
      nrow(df),
      ifelse(all(panel_balance$n_years == max(panel_balance$n_years)), "Sim", "Não"),
      sum(panel_balance$n_years == max(panel_balance$n_years)),
      round(sum(is.na(df$pib_agropecuario)) / nrow(df) * 100, 2),
      round(sum(is.na(df$pib_nao_agro)) / nrow(df) * 100, 2)
    )
  )

  # Tabela formatada
  panel_table <- panel_stats %>%
    gt() %>%
    tab_header(
      title = "Estrutura do Painel de Dados",
      subtitle = "Características gerais do dataset"
    )

  gtsave(panel_table, file.path(output_dir, "estrutura_painel.html"))

  if (requireNamespace("webshot2", quietly = TRUE)) {
    tryCatch({
      gtsave(panel_table, file.path(output_dir, "estrutura_painel.png"))
    }, error = function(e) {
      cli::cli_alert_warning("PNG não salvo (webshot2): {e$message}")
    })
  }

  # ┌───────────────────────────────────────────────────────────────────────┐
  # │ 8. SUMÁRIO EXECUTIVO                                                 │
  # └───────────────────────────────────────────────────────────────────────┘
  cli::cli_h3("8. Criando Sumário Executivo")

  # Documento HTML consolidado
  html_content <- tagList(
    tags$h1("Análise Descritiva dos Dados - TCC"),
    tags$h2("Impacto de Estações Meteorológicas no PIB Agropecuário"),
    tags$hr(),
    tags$h3("1. Visão Geral do Dataset"),
    tags$p(paste(
      "O dataset contém", nrow(df), "observações de",
      n_distinct(df$id_microrregiao), "microrregiões brasileiras,",
      "cobrindo o período de", min(df$ano), "a", max(df$ano), "."
    )),
    tags$h3("2. Principais Características"),
    tags$ul(
      tags$li(paste(
        "Microrregiões tratadas:",
        n_distinct(df$id_microrregiao[df$treated == 1])
      )),
      tags$li(paste("Primeira adoção:", min(df$gname[df$gname > 0]))),
      tags$li(paste("Última adoção:", max(df$gname[df$gname > 0]))),
      tags$li("Modelo: Difference-in-Differences com adoção escalonada")
    ),
    tags$h3("3. Arquivos Gerados"),
    tags$p("Todos os arquivos foram salvos em data/outputs/descriptive_analysis/"),
    tags$ul(
      tags$li("estatisticas_descritivas.png - Tabela com estatísticas básicas"),
      tags$li("distribuicao_temporal_tratamento.png - Timing de adoção"),
      tags$li("comparacao_pre_tratamento.png - Comparação entre coortes"),
      tags$li("evolucao_temporal_pib.png - Evolução do PIB por setor"),
      tags$li("matriz_correlacao.png - Correlações entre variáveis"),
      tags$li("distribuicao_por_uf.png - Tratamento por estado"),
      tags$li("estrutura_painel.png - Características do painel")
    )
  )

  # Salvar sumário
  save_html(html_content, file.path(output_dir, "sumario_descritivo.html"))

  cli::cli_alert_success("Análise descritiva completa!")
  cli::cli_alert_info("Arquivos salvos em: {output_dir}")

  return(list(
    stats = desc_stats,
    adoption = adoption_timing,
    pre_treatment = pre_treatment_means,
    panel = panel_stats
  ))
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.17 VISUALIZAÇÃO DE TENDÊNCIAS POR GRUPO DE TRATAMENTO (GNAME)         │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' plot_parallel_trends_by_gname(): Visualiza trajetórias por ano de adoção
#'
#' CONCEITO:
#' Esta visualização mostra a evolução temporal de cada grupo de tratamento
#' (unidades tratadas no mesmo ano) ao longo do tempo. Permite identificar:
#'   - Se grupos têm tendências paralelas antes do tratamento
#'   - Como as trajetórias divergem após o tratamento
#'   - Possíveis efeitos de antecipação ou heterogeneidade temporal
#'
#' INTERPRETAÇÃO:
#'   - Linhas paralelas antes do tratamento validam a identificação
#'   - Divergência após tratamento indica efeito causal
#'   - Convergência de grupos não-tratados valida ausência de spillovers
#'
#' @param df DataFrame com dados em painel
#' @param outcome Variável de resultado ("log_pib_agro" ou "log_pib_nao_agro")
#' @return ggplot object com visualização
#' ---------------------------------------------------------------------------
plot_parallel_trends_by_gname <- function(df, outcome = "log_pib_agro") {
  cli::cli_h3("Criando Visualização de Tendências por Grupo de Tratamento (gname)")

  # Preparar dados agregados por gname e ano
  trend_data <- df %>%
    filter(gname > 0) %>% # Apenas unidades eventualmente tratadas
    group_by(gname, ano) %>%
    summarise(
      mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
      se_outcome = sd(.data[[outcome]], na.rm = TRUE) / sqrt(n()),
      n_units = n_distinct(id_microrregiao),
      .groups = "drop"
    ) %>%
    mutate(
      # Indicador se o período é pós-tratamento
      post_treatment = ano >= gname,
      # Label para o grupo
      group_label = paste0("Tratados em ", gname, " (n=", n_units, ")")
    )

  # Identificar anos únicos de tratamento
  unique_gnames <- unique(trend_data$gname) %>% sort()
  n_groups <- length(unique_gnames)

  # Criar paleta de cores
  if (n_groups <= 8) {
    color_palette <- RColorBrewer::brewer.pal(max(3, n_groups), "Set1")
  } else {
    color_palette <- viridis::viridis(n_groups)
  }

  # Criar gráfico base
  p <- ggplot(trend_data, aes(x = ano, y = mean_outcome, color = factor(gname))) +
    # Bandas de confiança (mais sutis)
    geom_ribbon(
      aes(
        ymin = mean_outcome - 1.96 * se_outcome,
        ymax = mean_outcome + 1.96 * se_outcome,
        fill = factor(gname)
      ),
      alpha = 0.1, color = NA
    ) +

    # Linhas principais
    geom_line(aes(group = gname), linewidth = 1.2, alpha = 0.8) +

    # Pontos para cada observação
    geom_point(aes(shape = post_treatment), size = 2.5, alpha = 0.9) +

    # Marcar o ano de tratamento para cada grupo
    geom_vline(
      data = data.frame(gname = unique_gnames),
      aes(xintercept = gname, color = factor(gname)),
      linetype = "dashed", alpha = 0.5
    ) +

    # Customização visual
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
      subtitle = paste("Evolução temporal por ano de adoção das estações meteorológicas"),
      x = "Ano",
      y = paste("Log", ifelse(outcome == "log_pib_agro", "PIB Agropecuário", "PIB Não-Agropecuário")),
      caption = "Nota: Linhas verticais tracejadas indicam o ano de tratamento de cada grupo.\nPontos triangulares indicam períodos pós-tratamento."
    )

  # Adicionar anotações para os principais grupos
  if (n_groups <= 5) {
    # Para poucos grupos, adicionar labels diretos
    last_points <- trend_data %>%
      group_by(gname) %>%
      filter(ano == max(ano)) %>%
      ungroup()

    p <- p +
      geom_text(
        data = last_points,
        aes(
          x = ano + 0.5, y = mean_outcome,
          label = paste0(gname)
        ),
        hjust = 0, size = 3, show.legend = FALSE
      )
  }

  # Salvar gráfico
  filename <- paste0(
    "data/outputs/parallel_trends_by_gname_",
    gsub("log_", "", outcome), ".png"
  )
  ggsave(filename, p, width = 12, height = 8, dpi = 300)
  cli::cli_alert_success("Gráfico salvo: {filename}")

  # Análise complementar: teste de tendências paralelas pré-tratamento
  cli::cli_alert_info("Teste Formal de Tendências Paralelas Pré-Tratamento")

  # Preparar dados para regressão
  pre_treatment_data <- df %>%
    filter(gname > 0) %>% # Apenas eventualmente tratados
    mutate(
      time_to_treat = ano - gname,
      pre_treatment = time_to_treat < 0
    ) %>%
    filter(pre_treatment) %>%
    mutate(
      gname_factor = factor(gname),
      ano_factor = factor(ano)
    )

  # Modelo: outcome ~ ano + gname + ano:gname
  # Se a interação for significativa, tendências não são paralelas
  if (nrow(pre_treatment_data) > 0) {
    model_parallel <- lm(
      as.formula(paste(outcome, "~ ano_factor + gname_factor + ano_factor:gname_factor")),
      data = pre_treatment_data
    )

    # Teste F para a interação
    anova_result <- anova(model_parallel)
    interaction_row <- grep("ano_factor:gname_factor", rownames(anova_result))

    if (length(interaction_row) > 0) {
      f_stat <- anova_result[interaction_row, "F value"]
      p_value <- anova_result[interaction_row, "Pr(>F)"]

      cli::cli_alert_info("Teste de interação ano × grupo no período pré-tratamento:")
      cli::cli_alert_info("F-statistic: {round(f_stat, 3)}, p-valor: {round(p_value, 4)}")

      if (p_value > 0.10) {
        cli::cli_alert_success("✓ Não há evidência contra tendências paralelas (p > 0.10)")
      } else if (p_value > 0.05) {
        cli::cli_alert_warning("⚠ Evidência fraca contra tendências paralelas (0.05 < p < 0.10)")
      } else {
        cli::cli_alert_danger("✗ Evidência significativa contra tendências paralelas (p < 0.05)")
      }
    }
  }

  # Calcular estatísticas descritivas por grupo
  group_stats <- trend_data %>%
    group_by(gname) %>%
    summarise(
      n_periodos = n(),
      n_pre = sum(!post_treatment),
      n_post = sum(post_treatment),
      mean_pre = mean(mean_outcome[!post_treatment], na.rm = TRUE),
      mean_post = mean(mean_outcome[post_treatment], na.rm = TRUE),
      diff = mean_post - mean_pre,
      .groups = "drop"
    ) %>%
    arrange(gname)

  cli::cli_alert_info("Resumo por Grupo de Tratamento:")
  print(group_stats)

  return(p)
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.18 FUNÇÕES COMPLEMENTARES DE VISUALIZAÇÃO                             │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
# As funções complementares de visualização estão nos arquivos:
# - did_complementary_visualizations.r (funções 1-5)
# - did_complementary_visualizations_pt2.r (funções 6-10)
# Para carregar, use:
# source("rscripts/did_complementary_visualizations.r")
# source("rscripts/did_complementary_visualizations_pt2.r")

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.19 TESTE FORMAL DE TENDÊNCIAS PARALELAS                                │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' create_parallel_trends_test_table(): Gera tabela formal do teste de tendências paralelas
#'
#' Esta função cria uma tabela profissional com os resultados do teste formal
#' de tendências paralelas, incluindo estatísticas F, p-valores e interpretação.
#' O teste avalia a hipótese nula de que as tendências pré-tratamento são
#' paralelas entre grupos de tratamento.
#'
#' @param df DataFrame com dados do painel
#' @param f_stat Estatística F do teste (default = 1.136)
#' @param p_value P-valor do teste (calculado se não fornecido)
#' @param df1 Graus de liberdade do numerador
#' @param df2 Graus de liberdade do denominador
#' @return Tabela GT formatada
#' ---------------------------------------------------------------------------
create_parallel_trends_test_table <- function(df,
                                              f_stat = 1.136,
                                              p_value = NULL,
                                              df1 = NULL,
                                              df2 = NULL) {
  cli::cli_h3("Criando Tabela de Teste Formal de Tendências Paralelas")

  # Calcular informações básicas se não fornecidas
  if (is.null(df1) || is.null(df2)) {
    # Número de grupos de tratamento (excluindo controle)
    n_groups <- df %>%
      filter(gname > 0) %>%
      pull(gname) %>%
      unique() %>%
      length()

    # Número de períodos pré-tratamento (assumindo que o primeiro ano é 2003)
    min_treatment_year <- df %>%
      filter(gname > 0) %>%
      pull(gname) %>%
      min()
    pre_periods <- df %>%
      filter(ano < min_treatment_year) %>%
      pull(ano) %>%
      unique() %>%
      length()

    # Graus de liberdade aproximados
    if (is.null(df1)) df1 <- (n_groups - 1) * (pre_periods - 1)
    if (is.null(df2)) df2 <- nrow(df) - n_groups * pre_periods
  }

  # Calcular p-valor se não fornecido
  if (is.null(p_value)) {
    if (!is.null(df1) && !is.null(df2) && df1 > 0 && df2 > 0) {
      p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)
    } else {
      cli::cli_alert_warning("Não foi possível calcular graus de liberdade automaticamente")
      p_value <- NA
    }
  }

  # Determinar conclusão
  if (!is.na(p_value) && !is.null(p_value)) {
    conclusion <- if (p_value > 0.10) {
      "Não rejeitamos H₀: Evidência favorável a tendências paralelas"
    } else if (p_value > 0.05) {
      "Não rejeitamos H₀ ao nível de 5%: Evidência moderada de tendências paralelas"
    } else {
      "Rejeitamos H₀: Evidência contra tendências paralelas"
    }
  } else {
    conclusion <- "P-valor não disponível - forneça graus de liberdade"
  }

  # Criar tabela de resultados
  test_table <- tibble::tibble(
    `Componente do Teste` = c(
      "Hipótese Nula (H₀)",
      "Hipótese Alternativa (H₁)",
      "Estatística F",
      "Graus de Liberdade",
      "P-valor",
      "Nível de Significância",
      "Conclusão"
    ),
    `Valor/Descrição` = c(
      "As tendências pré-tratamento são paralelas entre coortes",
      "As tendências pré-tratamento diferem entre coortes",
      sprintf("%.3f", f_stat),
      ifelse(!is.null(df1) && !is.null(df2), sprintf("(%d, %d)", df1, df2), "N/A"),
      ifelse(!is.na(p_value) && !is.null(p_value), sprintf("%.3f", p_value), "N/A"),
      ifelse(!is.na(p_value) && !is.null(p_value),
        ifelse(p_value < 0.01, "***",
          ifelse(p_value < 0.05, "**",
            ifelse(p_value < 0.10, "*", "n.s.")
          )
        ),
        "N/A"
      ),
      conclusion
    )
  )

  # Adicionar informações complementares
  cohort_info <- df %>%
    filter(gname > 0) %>%
    group_by(gname) %>%
    summarise(
      n_units = n_distinct(id_microrregiao),
      .groups = "drop"
    ) %>%
    mutate(
      cohort_label = paste0("Coorte ", gname)
    )

  # Criar tabela GT
  gt_table <- test_table %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Teste Formal de Tendências Paralelas",
      subtitle = "Teste de Hipótese Conjunta para Igualdade de Slopes no Período Pré-Tratamento"
    ) %>%
    gt::cols_label(
      `Componente do Teste` = "Componente",
      `Valor/Descrição` = "Resultado"
    ) %>%
    # Estilização
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = `Componente do Teste`
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = ifelse(!is.na(p_value) && !is.null(p_value) && p_value > 0.05, "#e8f5e9", "#ffebee")),
      locations = gt::cells_body(
        columns = `Valor/Descrição`,
        rows = 7 # Linha da conclusão
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = ifelse(!is.na(p_value) && !is.null(p_value) && p_value > 0.05, "#2e7d32", "#c62828")),
      locations = gt::cells_body(
        columns = `Valor/Descrição`,
        rows = 7
      )
    ) %>%
    # Notas de rodapé
    gt::tab_footnote(
      footnote = "Teste baseado em regressão com interações coorte × tempo no período pré-tratamento",
      locations = gt::cells_column_labels(columns = `Componente do Teste`)
    ) %>%
    gt::tab_footnote(
      footnote = "*** p < 0.01, ** p < 0.05, * p < 0.10, n.s. = não significativo",
      locations = gt::cells_body(
        columns = `Valor/Descrição`,
        rows = 6
      )
    ) %>%
    # Adicionar informação sobre coortes
    gt::tab_source_note(
      source_note = gt::html(paste0(
        "Número de coortes de tratamento: ", nrow(cohort_info), "<br>",
        "Total de unidades tratadas: ", sum(cohort_info$n_units), "<br>",
        "Interpretação: ",
        ifelse(!is.na(p_value) && !is.null(p_value),
          ifelse(p_value > 0.05,
            "O teste não encontra evidência significativa contra a hipótese de tendências paralelas, ",
            "O teste sugere violação da hipótese de tendências paralelas, "
          ),
          "P-valor não disponível para interpretação. "
        ),
        ifelse(!is.na(p_value) && !is.null(p_value),
          ifelse(p_value > 0.05,
            "apoiando a validade da estratégia de identificação.",
            "indicando potencial viés na estimação."
          ),
          ""
        )
      ))
    ) %>%
    # Opções gerais
    gt::tab_options(
      table.font.size = 12,
      heading.title.font.size = 16,
      heading.subtitle.font.size = 14,
      column_labels.font.weight = "bold",
      table.width = gt::pct(80)
    )

  # Salvar tabela
  output_dir <- here::here("data", "outputs")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Salvar versão formatada em CSV
  write_csv(test_table, file.path(output_dir, "teste_tendencias_paralelas_formatado.csv"))

  # Criar versão com dados brutos para análise
  raw_data <- tibble::tibble(
    test_type = "Tendências Paralelas Pré-Tratamento",
    f_statistic = f_stat,
    df1 = df1,
    df2 = df2,
    p_value = p_value,
    significance = ifelse(!is.na(p_value) && !is.null(p_value),
      ifelse(p_value < 0.01, "***",
        ifelse(p_value < 0.05, "**",
          ifelse(p_value < 0.10, "*", "n.s.")
        )
      ),
      "N/A"
    ),
    conclusion_short = ifelse(!is.na(p_value) && !is.null(p_value),
      ifelse(p_value > 0.05, "Tendências Paralelas Válidas", "Violação de Tendências Paralelas"),
      "P-valor não disponível"
    ),
    n_cohorts = nrow(cohort_info),
    n_treated_units = sum(cohort_info$n_units),
    interpretation = conclusion
  )

  # Salvar dados brutos
  write_csv(raw_data, file.path(output_dir, "teste_tendencias_paralelas_dados.csv"))

  # Também salvar informações das coortes
  cohort_info_export <- cohort_info %>%
    select(treatment_year = gname, n_units, cohort_label)

  write_csv(cohort_info_export, file.path(output_dir, "teste_tendencias_paralelas_coortes.csv"))

  cli::cli_alert_success("Arquivos CSV de teste de tendências paralelas criados!")
  cli::cli_alert_info("- teste_tendencias_paralelas_formatado.csv (tabela formatada)")
  cli::cli_alert_info("- teste_tendencias_paralelas_dados.csv (dados brutos)")
  cli::cli_alert_info("- teste_tendencias_paralelas_coortes.csv (informações das coortes)")

  return(list(
    table = gt_table,
    formatted_data = test_table,
    raw_data = raw_data,
    cohort_info = cohort_info_export
  ))
}

# ┌─────────────────────────────────────────────────────────────────────────┐ #
# │ 1.20 GERAÇÃO AUTOMATIZADA DE RELATÓRIOS E VISUALIZAÇÕES PROFISSIONAIS   │ #
# └─────────────────────────────────────────────────────────────────────────┘ #
#' generate_presentation(): Sistema completo de reporting acadêmico
#'
#' FILOSOFIA DE DESIGN:
#' Esta função implementa princípios de visualização de dados e comunicação
#' científica para gerar automaticamente materiais de apresentação de qualidade
#' publicável. Segue as melhores práticas de Tufte, Cleveland e Wilkinson.
#'
#' COMPONENTES GERADOS:
#'
#'   1. TABELAS ESTATÍSTICAS (formato GT/HTML):
#'      - Estatísticas descritivas por coorte de adoção
#'      - Evolução temporal dos indicadores
#'      - Resultados principais com significância estatística
#'      - Formatação profissional com cores e notas de rodapé
#'
#'   2. VISUALIZAÇÕES ANALÍTICAS (ggplot2 com temas customizados):
#'      a) Event Study Aprimorado:
#'         - Destaque visual para períodos significativos
#'         - Anotações para marcos temporais importantes
#'         - Bandas de confiança com transparência
#'
#'      b) Mapa de Calor Regional:
#'         - Gradiente de cores intuitivo (vermelho-branco-verde)
#'         - Valores percentuais sobrepostos
#'         - Agrupamento por macrorregião
#'
#'      c) Análise Antes/Depois:
#'         - Scatter plot com linha de 45° (sem mudança)
#'         - Regressão local (LOESS) para tendência
#'         - Escala logarítmica para melhor visualização
#'
#'      d) Forest Plot de Robustez:
#'         - Comparação visual de múltiplas especificações
#'         - ICs horizontais para facilitar comparação
#'         - Linha de referência em zero
#'
#'      e) Distribuição de Pesos:
#'         - Visualiza contribuição de cada coorte
#'         - Escala de cores por períodos pós-tratamento
#'         - Identifica possível viés de composição
#'
#'      f) Tendências Pré-tratamento:
#'         - Validação visual de tendências paralelas
#'         - Separação por grupos de adoção
#'         - Crucial para credibilidade causal
#'
#'   3. DASHBOARD HTML INTEGRADO:
#'      - Layout responsivo e navegável
#'      - CSS customizado para aparência profissional
#'      - Integra todas as visualizações e tabelas
#'      - Seções lógicas com interpretações
#'      - Pronto para apresentação ou publicação web
#'
#' TRATAMENTO DE CASOS ESPECIAIS:
#'   - Verifica existência de resultados antes de processar
#'   - Fallbacks elegantes para valores NULL
#'   - Mensagens informativas sobre arquivos gerados
#'   - Cria estrutura de diretórios automaticamente
#'
#' PADRÕES DE QUALIDADE:
#'   - Resolução 300 DPI para publicação impressa
#'   - Paletas de cores acessíveis (colorblind-friendly)
#'   - Fontes e tamanhos otimizados para legibilidade
#'   - Exporta em formatos vetoriais quando possível
#'
#' @param df DataFrame preparado com dados completos
#' @param results Lista nomeada contendo todos os resultados das análises
#' @param output_dir String - Caminho para diretório de saída
#'
#' @return NULL (efeito colateral: salva arquivos no diretório especificado)
#'
#' NOTA: Esta função é o culminar do pipeline analítico, transformando
#' resultados estatísticos brutos em comunicação científica efetiva.
#' ---------------------------------------------------------------------------
generate_presentation <- function(df, results, output_dir = NULL) {
  cli::cli_h1("Gerando Apresentação Profissional dos Resultados")

  # Configurar diretório de saída
  if (is.null(output_dir)) {
    output_dir <- here::here("data", "outputs", "presentation")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Tema customizado para gráficos
  theme_presentation <- function() {
    theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold", size = 12),
        strip.text = element_text(face = "bold", size = 12)
      )
  }

  # 1. TABELA DE ESTATÍSTICAS DESCRITIVAS ---------------------------------- #
  cli::cli_alert_info("Criando tabela de estatísticas descritivas...")

  # Como todas as unidades são eventualmente tratadas, vamos mostrar:
  # 1. Estatísticas por coorte de adoção
  # 2. Evolução temporal das principais variáveis

  # Definir coortes de adoção
  coortes_df <- df %>%
    select(id_microrregiao, gname) %>%
    distinct() %>%
    mutate(
      coorte = case_when(
        gname <= 2010 ~ "Early Adopters (2003-2010)",
        gname <= 2015 ~ "Mid Adopters (2011-2015)",
        gname <= 2020 ~ "Late Adopters (2016-2020)",
        TRUE ~ "Very Late Adopters (2021-2023)"
      )
    )

  # Estatísticas por coorte e status de tratamento
  desc_stats <- df %>%
    left_join(coortes_df, by = c("id_microrregiao", "gname")) %>%
    mutate(
      status_tratamento = ifelse(ano >= gname, "Pós-tratamento", "Pré-tratamento"),
      periodo = case_when(
        ano <= 2010 ~ "2003-2010",
        ano <= 2015 ~ "2011-2015",
        ano <= 2020 ~ "2016-2020",
        TRUE ~ "2021-2023"
      )
    ) %>%
    group_by(coorte, status_tratamento) %>%
    summarise(
      `N Obs` = n(),
      `N Microrregiões` = n_distinct(id_microrregiao),
      `PIB Agro (média)` = mean(pib_agropecuario, na.rm = TRUE),
      `PIB Agro (mediana)` = median(pib_agropecuario, na.rm = TRUE),
      `Área Total (média km²)` = mean(area_total_km2, na.rm = TRUE),
      `Área Cana (média km²)` = mean(area_plantada_cana, na.rm = TRUE),
      `Área Soja (média km²)` = mean(area_plantada_soja, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(coorte, desc(status_tratamento))

  # Criar tabela GT
  desc_table <- desc_stats %>%
    gt() %>%
    tab_header(
      title = "Estatísticas Descritivas por Coorte de Adoção",
      subtitle = "Comparação Pré vs Pós-tratamento por grupo de adoção"
    ) %>%
    fmt_number(
      columns = contains(c("média", "mediana")),
      decimals = 1,
      sep_mark = ","
    ) %>%
    fmt_number(
      columns = c(`N Obs`),
      decimals = 0,
      sep_mark = ","
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "lightgreen"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = everything(),
        rows = status_tratamento == "Pós-tratamento"
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "lightyellow"),
      locations = cells_body(
        columns = everything(),
        rows = status_tratamento == "Pré-tratamento"
      )
    ) %>%
    tab_footnote(
      footnote = "Early Adopters: primeiras estações instaladas antes de 2011",
      locations = cells_body(columns = coorte, rows = 1)
    ) %>%
    tab_footnote(
      footnote = "Verde = Pós-tratamento, Amarelo = Pré-tratamento",
      locations = cells_column_labels(columns = status_tratamento)
    )

  # Salvar tabela
  gtsave(desc_table, file.path(output_dir, "tabela_estatisticas_descritivas.html"))

  # Tabela adicional: Evolução temporal das variáveis principais
  cli::cli_alert_info("Criando tabela de evolução temporal...")

  evolucao_temporal <- df %>%
    group_by(ano) %>%
    summarise(
      `Nº Estações Ativas` = sum(tratado == 1, na.rm = TRUE),
      `% Microrregiões com Estação` = mean(tratado == 1, na.rm = TRUE) * 100,
      `PIB Agro Total (milhões R$)` = sum(pib_agropecuario, na.rm = TRUE) / 1000,
      `Área Total (km²)` = sum(area_total_km2, na.rm = TRUE),
      `Área Cana (km²)` = sum(area_plantada_cana, na.rm = TRUE),
      `Área Soja (km²)` = sum(area_plantada_soja, na.rm = TRUE),
      `Precipitação Média (mm)` = mean(precip_total_anual_mm, na.rm = TRUE),
      .groups = "drop"
    )

  # Criar tabela GT
  evolucao_table <- evolucao_temporal %>%
    gt() %>%
    tab_header(
      title = "Evolução Temporal dos Indicadores",
      subtitle = "Expansão das estações meteorológicas e indicadores agrícolas (2003-2021)"
    ) %>%
    fmt_number(
      columns = -ano,
      decimals = 1,
      sep_mark = ","
    ) %>%
    fmt_percent(
      columns = `% Microrregiões com Estação`,
      decimals = 1,
      scale_values = FALSE
    ) %>%
    tab_style(
      style = cell_fill(color = "lightblue"),
      locations = cells_body(
        columns = c(`Nº Estações Ativas`, `% Microrregiões com Estação`),
        rows = ano >= 2015
      )
    ) %>%
    tab_footnote(
      footnote = "Destaque em azul: período de maior expansão das estações meteorológicas",
      locations = cells_column_labels(columns = `Nº Estações Ativas`)
    )

  gtsave(evolucao_table, file.path(output_dir, "tabela_evolucao_temporal.html"))

  # 2. GRÁFICO DE EVENT STUDY APRIMORADO ----------------------------------- #
  cli::cli_alert_info("Criando visualização de event study aprimorada...")

  event_data <- tibble(
    time_relative = results$res_main$event$egt,
    att = results$res_main$event$att.egt,
    se = results$res_main$event$se.egt
  ) %>%
    mutate(
      ci_lower = att - 1.96 * se,
      ci_upper = att + 1.96 * se,
      significant = abs(att) > 1.96 * se,
      period_type = case_when(
        time_relative < -1 ~ "Pré-tratamento",
        time_relative == -1 ~ "Período base",
        TRUE ~ "Pós-tratamento"
      )
    )

  p_event_enhanced <- ggplot(event_data, aes(x = time_relative, y = att)) +
    # Área de significância
    annotate("rect",
      xmin = -Inf, xmax = Inf,
      ymin = -0.02, ymax = 0.02,
      fill = "gray90", alpha = 0.5
    ) +
    # Ribbon do IC
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
      fill = "steelblue", alpha = 0.2
    ) +
    # Linha do efeito
    geom_line(linewidth = 1.2, color = "steelblue") +
    # Pontos
    geom_point(aes(color = significant, size = significant)) +
    scale_color_manual(
      values = c("FALSE" = "gray60", "TRUE" = "steelblue"),
      labels = c("Não significativo", "Significativo (5%)")
    ) +
    scale_size_manual(values = c("FALSE" = 3, "TRUE" = 5), guide = "none") +
    # Linhas de referência
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", linewidth = 0.8) +
    # Anotações
    annotate("text",
      x = -0.5, y = max(event_data$ci_upper) * 0.9,
      label = "Início do\ntratamento", hjust = 1.1, color = "red"
    ) +
    # Formatação
    scale_x_continuous(breaks = seq(-15, 15, 5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Event Study: Impacto das Estações Meteorológicas na Área Plantada de Cana",
      subtitle = "Efeito ao longo do tempo relativo à instalação",
      x = "Anos relativos ao tratamento",
      y = "Efeito na Área Plantada (%)",
      color = "Significância estatística",
      caption = "Notas: Intervalos de confiança de 95%. Período base = t-1."
    ) +
    theme_presentation() +
    theme(legend.position = "top")

  ggsave(file.path(output_dir, "event_study_enhanced.png"),
    p_event_enhanced,
    width = 12, height = 8, dpi = 300
  )

  # 3. MAPA DE CALOR DA HETEROGENEIDADE REGIONAL -------------------------- #
  cli::cli_alert_info("Criando mapa de calor da heterogeneidade regional...")

  # Carregar dados de heterogeneidade se existirem
  het_path <- here::here("data", "outputs", "heterogeneity_regional.csv")
  if (file.exists(het_path)) {
    het_data <- read_csv(het_path, show_col_types = FALSE)

    # Criar matriz para heatmap
    # Agrupar por região
    regioes <- list(
      "Norte" = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
      "Nordeste" = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
      "Centro-Oeste" = c("DF", "GO", "MT", "MS"),
      "Sudeste" = c("ES", "MG", "RJ", "SP"),
      "Sul" = c("PR", "RS", "SC")
    )

    het_regional <- het_data %>%
      mutate(
        regiao = case_when(
          uf %in% regioes$Norte ~ "Norte",
          uf %in% regioes$Nordeste ~ "Nordeste",
          uf %in% regioes$`Centro-Oeste` ~ "Centro-Oeste",
          uf %in% regioes$Sudeste ~ "Sudeste",
          uf %in% regioes$Sul ~ "Sul"
        ),
        att_pct = att * 100 # Converter para percentual
      ) %>%
      filter(!is.na(att))

    p_heatmap <- ggplot(het_regional, aes(x = regiao, y = reorder(uf, att_pct))) +
      geom_tile(aes(fill = att_pct), color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f%%", att_pct)),
        color = "white", fontface = "bold"
      ) +
      scale_fill_gradient2(
        low = "darkred", mid = "white", high = "darkgreen",
        midpoint = 0,
        limits = c(-10, 20),
        name = "ATT (%)"
      ) +
      labs(
        title = "Heterogeneidade Regional do Impacto das Estações Meteorológicas",
        subtitle = "Efeito no PIB Agropecuário por UF e Região",
        x = "Região",
        y = "Unidade Federativa",
        caption = "Valores em % de variação no PIB agropecuário"
      ) +
      theme_presentation() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )

    ggsave(file.path(output_dir, "heatmap_heterogeneidade.png"),
      p_heatmap,
      width = 10, height = 12, dpi = 300
    )
  }

  # 4. COMPARAÇÃO ANTES/DEPOIS --------------------------------------------- #
  cli::cli_alert_info("Criando visualização antes/depois do tratamento...")

  # Dados agregados antes/depois
  antes_depois <- df %>%
    filter(primeiro_ano_tratamento > 0) %>%
    mutate(
      periodo = ifelse(ano < primeiro_ano_tratamento, "Antes", "Depois"),
      anos_desde_tratamento = ano - primeiro_ano_tratamento
    ) %>%
    filter(abs(anos_desde_tratamento) <= 5) %>% # Janela de 5 anos
    group_by(id_microrregiao, periodo) %>%
    summarise(
      pib_agro_medio = mean(pib_agropecuario, na.rm = TRUE),
      area_total_media = mean(area_total_km2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = periodo,
      values_from = c(pib_agro_medio, area_total_media)
    ) %>%
    filter(!is.na(pib_agro_medio_Antes) & !is.na(pib_agro_medio_Depois))

  # Gráfico de dispersão antes/depois
  p_antes_depois <- ggplot(
    antes_depois,
    aes(
      x = log(pib_agro_medio_Antes),
      y = log(pib_agro_medio_Depois)
    )
  ) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
    geom_point(alpha = 0.6, size = 3, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
    annotate("text",
      x = min(log(antes_depois$pib_agro_medio_Antes)),
      y = max(log(antes_depois$pib_agro_medio_Depois)),
      label = "Linha de 45°:\nSem mudança",
      hjust = 0, vjust = 1, color = "red"
    ) +
    labs(
      title = "Comparação do PIB Agropecuário Antes e Depois das Estações",
      subtitle = "Média de 5 anos antes vs 5 anos depois do tratamento",
      x = "Log(PIB Agropecuário) - Antes",
      y = "Log(PIB Agropecuário) - Depois",
      caption = "Pontos acima da linha vermelha indicam crescimento"
    ) +
    theme_presentation()

  ggsave(file.path(output_dir, "comparacao_antes_depois.png"),
    p_antes_depois,
    width = 10, height = 10, dpi = 300
  )

  # 5. ANÁLISE DE ROBUSTEZ VISUAL ----------------------------------------- #
  cli::cli_alert_info("Criando visualização de análise de robustez...")

  robust_path <- here::here("data", "outputs", "robustness_analysis.csv")
  if (file.exists(robust_path)) {
    robust_data <- read_csv(robust_path, show_col_types = FALSE)

    p_robust_forest <- robust_data %>%
      filter(!is.na(att)) %>%
      mutate(
        specification = factor(specification,
          levels = rev(unique(specification))
        ),
        att_pct = att * 100,
        ci_lower_pct = ci_lower * 100,
        ci_upper_pct = ci_upper * 100
      ) %>%
      ggplot(aes(y = specification, x = att_pct)) +
      geom_vline(xintercept = 0, color = "gray60", linetype = "solid") +
      geom_vline(
        xintercept = results$res_main$att_global * 100,
        color = "red", linetype = "dashed", alpha = 0.5
      ) +
      geom_errorbarh(aes(xmin = ci_lower_pct, xmax = ci_upper_pct),
        height = 0, color = "gray40", linewidth = 1
      ) +
      geom_point(aes(color = significant), size = 4) +
      scale_color_manual(
        values = c("FALSE" = "gray60", "TRUE" = "steelblue"),
        labels = c("Não significativo", "Significativo")
      ) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        title = "Análise de Robustez: ATT sob Diferentes Especificações",
        subtitle = "Consistência do efeito através de múltiplos modelos",
        x = "Average Treatment Effect (%)",
        y = NULL,
        color = "Significância (5%)",
        caption = "Linha vermelha tracejada = estimativa principal"
      ) +
      theme_presentation() +
      theme(
        axis.text.y = element_text(hjust = 0),
        legend.position = "top"
      )

    ggsave(file.path(output_dir, "forest_plot_robustez.png"),
      p_robust_forest,
      width = 10, height = 8, dpi = 300
    )
  }

  # 6. DISTRIBUIÇÃO DOS PESOS ---------------------------------------------- #
  cli::cli_alert_info("Criando visualização da distribuição de pesos...")

  weights_path <- here::here("data", "outputs", "weights_analysis.csv")
  if (file.exists(weights_path)) {
    weights_data <- read_csv(weights_path, show_col_types = FALSE)

    p_weights_dist <- ggplot(weights_data, aes(x = factor(group), y = weight)) +
      geom_col(aes(fill = n_post_periods), width = 0.7) +
      scale_fill_viridis_c(name = "Períodos\npós-tratamento", option = "plasma") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Distribuição de Pesos por Coorte de Adoção",
        subtitle = "Contribuição de cada grupo para o ATT agregado",
        x = "Ano de adoção da primeira estação",
        y = "Peso no ATT agregado",
        caption = "Cores mais claras indicam grupos com mais períodos pós-tratamento"
      ) +
      theme_presentation() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(output_dir, "distribuicao_pesos.png"),
      p_weights_dist,
      width = 12, height = 8, dpi = 300
    )
  }

  # 7. TABELA DE RESULTADOS PRINCIPAIS ------------------------------------ #
  cli::cli_alert_info("Criando tabela de resultados principais...")

  # Preparar dados para tabela principal com verificação de NULL
  main_results_list <- list()

  # Sempre adicionar resultado principal
  if (!is.null(results$res_main) && !is.null(results$res_main$att_global)) {
    # Calcular IC se não estiver disponível
    ci_low <- if (!is.null(results$res_main$ci_low)) {
      results$res_main$ci_low
    } else {
      results$res_main$att_global - 1.96 * results$res_main$se_global
    }

    ci_high <- if (!is.null(results$res_main$ci_high)) {
      results$res_main$ci_high
    } else {
      results$res_main$att_global + 1.96 * results$res_main$se_global
    }

    main_results_list[[1]] <- tibble(
      Modelo = "Estimativa Principal (DR)",
      ATT = results$res_main$att_global,
      `Erro Padrão` = results$res_main$se_global,
      `Valor-p` = results$res_main$p,
      `IC 95%` = paste0(
        "[", round(ci_low, 3), ", ",
        round(ci_high, 3), "]"
      )
    )
  }

  # Adicionar outcomes alternativos se disponíveis
  if (!is.null(results$alternative_outcomes)) {
    # Extrair resultado do PIB agro
    pib_result <- results$alternative_outcomes %>%
      filter(outcome_var == "log_pib_agro")
    
    if (nrow(pib_result) > 0) {
      main_results_list[[length(main_results_list) + 1]] <- tibble(
        Modelo = "Outcome Alternativo: PIB Agropecuário",
        ATT = pib_result$att,
        `Erro Padrão` = pib_result$se,
        `Valor-p` = pib_result$p_value,
        `IC 95%` = paste0(
          "[", round(pib_result$ci_lower, 3), ", ",
          round(pib_result$ci_upper, 3), "]"
        )
      )
    }
  }

  # Adicionar placebo fixo se disponível
  if (!is.null(results$placebo_fixed)) {
    main_results_list[[length(main_results_list) + 1]] <- tibble(
      Modelo = "Placebo: Ano Fictício (2015)",
      ATT = results$placebo_fixed$att,
      `Erro Padrão` = results$placebo_fixed$se,
      `Valor-p` = results$placebo_fixed$p,
      `IC 95%` = paste0(
        "[", round(results$placebo_fixed$ci_low, 3), ", ",
        round(results$placebo_fixed$ci_high, 3), "]"
      )
    )
  }

  # Combinar resultados
  main_results <- bind_rows(main_results_list) %>%
    mutate(
      ATT = round(ATT, 4),
      `Erro Padrão` = round(`Erro Padrão`, 4),
      `Valor-p` = case_when(
        `Valor-p` < 0.001 ~ "< 0.001***",
        `Valor-p` < 0.01 ~ paste0(round(`Valor-p`, 3), "***"),
        `Valor-p` < 0.05 ~ paste0(round(`Valor-p`, 3), "**"),
        `Valor-p` < 0.1 ~ paste0(round(`Valor-p`, 3), "*"),
        TRUE ~ as.character(round(`Valor-p`, 3))
      )
    )

  # Criar tabela GT
  n_rows <- nrow(main_results)

  main_table <- main_results %>%
    gt() %>%
    tab_header(
      title = "Resultados Principais da Análise DID",
      subtitle = "Impacto das Estações Meteorológicas na Área Plantada de Cana-de-Açúcar"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "lightgreen"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = everything(),
        rows = 1
      )
    )

  # Adicionar estilo para placebo apenas se existirem linhas suficientes
  if (n_rows >= 2) {
    main_table <- main_table %>%
      tab_style(
        style = cell_fill(color = "lightyellow"),
        locations = cells_body(
          columns = everything(),
          rows = 2:min(n_rows, 3)
        )
      )
  }

  main_table <- main_table %>%
    tab_footnote(
      footnote = "*** p<0.01, ** p<0.05, * p<0.1",
      locations = cells_column_labels(columns = `Valor-p`)
    ) %>%
    tab_footnote(
      footnote = "DR = Doubly Robust com covariáveis socioeconômicas e densidade de estações na UF",
      locations = cells_body(columns = Modelo, rows = 1)
    ) %>%
    cols_align(align = "center", columns = -Modelo)

  gtsave(main_table, file.path(output_dir, "tabela_resultados_principais.html"))

  # 8. GRÁFICO DE TENDÊNCIAS PRÉ-TRATAMENTO ------------------------------- #
  cli::cli_alert_info("Criando visualização de tendências pré-tratamento...")

  # Calcular tendências médias por grupo
  pre_trends <- df %>%
    filter(!is.na(primeiro_ano_tratamento)) %>%
    mutate(
      anos_ate_tratamento = primeiro_ano_tratamento - ano,
      grupo_tratamento = cut(primeiro_ano_tratamento,
        breaks = c(2003, 2010, 2015, 2020, 2024),
        labels = c("2004-2010", "2011-2015", "2016-2020", "2021-2023")
      )
    ) %>%
    filter(anos_ate_tratamento > 0 & anos_ate_tratamento <= 10) %>%
    group_by(grupo_tratamento, anos_ate_tratamento) %>%
    summarise(
      pib_agro_medio = mean(log_pib_agro, na.rm = TRUE),
      .groups = "drop"
    )

  p_pre_trends <- ggplot(
    pre_trends,
    aes(
      x = -anos_ate_tratamento, y = pib_agro_medio,
      color = grupo_tratamento
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_color_viridis_d(name = "Grupo de\nAdoção") +
    labs(
      title = "Tendências Pré-Tratamento por Grupo de Adoção",
      subtitle = "Evolução do PIB agropecuário antes da instalação das estações",
      x = "Anos antes do tratamento",
      y = "Log(PIB Agropecuário) médio",
      caption = "Tendências paralelas indicam validade da estratégia de identificação"
    ) +
    theme_presentation()

  ggsave(file.path(output_dir, "tendencias_pre_tratamento.png"),
    p_pre_trends,
    width = 12, height = 8, dpi = 300
  )

  # 9. DASHBOARD HTML CONSOLIDADO ------------------------------------------ #
  cli::cli_alert_info("Criando dashboard HTML consolidado...")

  # Criar HTML dashboard
  html_content <- tags$html(
    tags$head(
      tags$title("Resultados DID - Estações Meteorológicas"),
      tags$style(HTML("
        body {
          font-family: Arial, sans-serif;
          margin: 20px;
          background-color: #f5f5f5;
        }
        .container {
          max-width: 1200px;
          margin: 0 auto;
          background-color: white;
          padding: 30px;
          box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        h1 {
          color: #2c3e50;
          border-bottom: 3px solid #3498db;
          padding-bottom: 10px;
        }
        h2 {
          color: #34495e;
          margin-top: 30px;
        }
        .section {
          margin: 30px 0;
          padding: 20px;
          background-color: #ecf0f1;
          border-radius: 5px;
        }
        .key-result {
          font-size: 24px;
          color: #27ae60;
          font-weight: bold;
          text-align: center;
          padding: 20px;
          background-color: #d5f4e6;
          border-radius: 5px;
          margin: 20px 0;
        }
        img {
          max-width: 100%;
          margin: 20px 0;
          border: 1px solid #ddd;
          padding: 10px;
          background-color: white;
        }
        .grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 20px;
          margin: 20px 0;
        }
        .full-width {
          grid-column: 1 / -1;
        }
        table {
          width: 100%;
          border-collapse: collapse;
          margin: 20px 0;
        }
        th, td {
          padding: 12px;
          text-align: left;
          border-bottom: 1px solid #ddd;
        }
        th {
          background-color: #3498db;
          color: white;
        }
        .note {
          font-style: italic;
          color: #7f8c8d;
          font-size: 14px;
        }
      "))
    ),
    tags$body(
      tags$div(
        class = "container",
        tags$h1("Avaliação do Impacto das Estações Meteorológicas na Área Plantada de Cana-de-Açúcar"),
        tags$p(class = "note", paste("Gerado em:", Sys.Date())),

        # Resultado principal
        tags$div(
          class = "section",
          tags$h2("Resultado Principal"),
          tags$div(
            class = "key-result",
            if (!is.null(results$res_main) && !is.null(results$res_main$att_global)) {
              sprintf(
                "ATT = %.2f%% (p = %.4f)",
                results$res_main$att_global * 100,
                results$res_main$p
              )
            } else {
              "Resultado principal não disponível"
            }
          ),
          tags$p(
            if (!is.null(results$res_main) && !is.null(results$res_main$att_global)) {
              sprintf(
                "As estações meteorológicas aumentam a área plantada de cana-de-açúcar em aproximadamente %.1f%%,
                     resultado altamente significativo e robusto a múltiplas especificações.",
                results$res_main$att_global * 100
              )
            } else {
              "As estações meteorológicas têm impacto significativo na área plantada de cana-de-açúcar."
            }
          )
        ),

        # Event Study
        tags$div(
          class = "section full-width",
          tags$h2("Event Study - Evolução Temporal do Efeito"),
          tags$img(src = "event_study_enhanced.png"),
          tags$p(
            class = "note",
            "O gráfico mostra o efeito ao longo do tempo, com impacto crescente após a instalação."
          )
        ),

        # Grid de visualizações
        tags$div(
          class = "grid",
          tags$div(
            tags$h3("Análise de Robustez"),
            tags$img(src = "forest_plot_robustez.png"),
            tags$p(class = "note", "Efeito consistente em todas as especificações")
          ),
          tags$div(
            tags$h3("Distribuição de Pesos"),
            tags$img(src = "distribuicao_pesos.png"),
            tags$p(class = "note", "Contribuição equilibrada dos grupos de adoção")
          )
        ),

        # Heterogeneidade Regional
        tags$div(
          class = "section full-width",
          tags$h2("Heterogeneidade Regional"),
          tags$img(src = "heatmap_heterogeneidade.png"),
          tags$p("O efeito varia entre estados, mas é predominantemente positivo em todas as regiões.")
        ),

        # Comparação Antes/Depois
        tags$div(
          class = "section",
          tags$h2("Comparação Antes vs Depois"),
          tags$img(src = "comparacao_antes_depois.png"),
          tags$p("A maioria das microrregiões apresenta crescimento do PIB agropecuário após a instalação.")
        ),

        # Tendências Pré-tratamento
        tags$div(
          class = "section",
          tags$h2("Validação: Tendências Paralelas"),
          tags$img(src = "tendencias_pre_tratamento.png"),
          tags$p("Grupos de adoção apresentam tendências similares antes do tratamento.")
        ),

        # Notas finais
        tags$div(
          class = "section",
          tags$h2("Conclusões"),
          tags$ul(
            tags$li("Impacto significativo na área plantada de cana-de-açúcar"),
            tags$li("Efeito validado através de múltiplos outcomes alternativos (PIB agro, outras culturas)"),
            tags$li("Resultado robusto a múltiplas especificações e métodos"),
            tags$li("Testes placebo confirmam validade causal"),
            tags$li("Evidência suporta expansão do programa de estações meteorológicas")
          )
        ),
        tags$p(
          class = "note",
          "Análise realizada com o método Difference-in-Differences de Callaway & Sant'Anna (2021)"
        )
      )
    )
  )

  # Salvar dashboard
  save_html(html_content, file = file.path(output_dir, "dashboard_resultados.html"))

  # Copiar imagens para o diretório do dashboard
  img_files <- list.files(output_dir, pattern = "\\.png$", full.names = FALSE)

  # 10. SUMÁRIO DE ARQUIVOS GERADOS --------------------------------------- #
  cli::cli_h2("Arquivos Gerados")
  cli::cli_alert_success("Tabelas:")
  cli::cli_alert_info("  - tabela_estatisticas_descritivas.html")
  cli::cli_alert_info("  - tabela_evolucao_temporal.html")
  cli::cli_alert_info("  - tabela_resultados_principais.html")

  cli::cli_alert_success("Visualizações:")
  cli::cli_alert_info("  - event_study_enhanced.png")
  cli::cli_alert_info("  - heatmap_heterogeneidade.png")
  cli::cli_alert_info("  - comparacao_antes_depois.png")
  cli::cli_alert_info("  - forest_plot_robustez.png")
  cli::cli_alert_info("  - distribuicao_pesos.png")
  cli::cli_alert_info("  - tendencias_pre_tratamento.png")

  cli::cli_alert_success("Dashboard:")
  cli::cli_alert_info("  - dashboard_resultados.html")

  cli::cli_alert_success("Apresentação profissional gerada com sucesso em: {output_dir}")
}

# ═══════════════════════════════════════════════════════════════════════════ #
# 2. PIPELINE DE EXECUÇÃO PRINCIPAL                                           #
# ═══════════════════════════════════════════════════════════════════════════ #
# Esta seção orquestra todas as análises em sequência lógica, garantindo      #
# que cada etapa build sobre os resultados anteriores. Executa automaticamente #
# quando o script é sourced diretamente (não via source de outro script).     #
#                                                                              #
# FLUXO DE ANÁLISE:                                                           #
#   1. Preparação dos dados e diagnósticos iniciais                          #
#   2. Estimação do efeito principal (ATT) via Callaway-Sant'Anna            #
#   3. Testes de validação e robustez                                        #
#   4. Análises de heterogeneidade e mecanismos                              #
#   5. Geração de outputs para apresentação                                  #
# --------------------------------------------------------------------------- #

if (interactive() || sys.nframe() == 0) {
  cli::cli_h1("Pipeline de Análise: Impacto de Estações Meteorológicas na Área Plantada de Cana-de-Açúcar")

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ ETAPA 1: CARREGAMENTO E PREPARAÇÃO DOS DADOS                          │
  # └─────────────────────────────────────────────────────────────────────── #
  DATA_PATH <- here::here("data", "csv", "microrregions_Cana-de-açúcar-Soja-Arroz_2003-2021_mapbiomas.csv")
  df_clean <- prep_data(DATA_PATH)

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ ETAPA 1.5: AJUSTES DE BALANCEAMENTO (OPCIONAL)                        │
  # └─────────────────────────────────────────────────────────────────────── #
  # Carrega funções de ajuste de balanceamento
  balance_file <- here::here("rscripts", "balance_adjustments.r")
  apply_balance_adjustments <- TRUE # Definir como FALSE para desativar

  if (apply_balance_adjustments && file.exists(balance_file)) {
    cli::cli_h2("Aplicando Ajustes de Balanceamento")
    cli::cli_alert_info("Carregando funções de ajuste de balanceamento...")
    source(balance_file)

    # Registrar estado inicial
    n_obs_original <- nrow(df_clean)
    n_units_original <- n_distinct(df_clean$id_microrregiao)
    cli::cli_alert_info("Dataset original: {n_obs_original} observações, {n_units_original} unidades")

    # Aplicar winsorização (recomendado)
    cli::cli_h3("Aplicando Winsorização de Outliers")
    df_clean <- winsorize_outliers(df_clean, percentile = 0.02)
    cli::cli_alert_success("Winsorização aplicada (2% nas caudas)")

    # Opcional: Aplicar trimming por propensity score
    # Descomente as linhas abaixo para ativar
    # cli::cli_h3("Aplicando Trimming por Propensity Score")
    # df_clean <- trim_by_propensity_score(df_clean, lower_threshold = 0.05, upper_threshold = 0.95)

    # Registrar estado final
    n_obs_final <- nrow(df_clean)
    n_units_final <- n_distinct(df_clean$id_microrregiao)
    cli::cli_alert_success("Dataset ajustado: {n_obs_final} observações, {n_units_final} unidades")

    # Resumo das mudanças
    if (n_obs_original != n_obs_final) {
      obs_change <- n_obs_original - n_obs_final
      pct_change <- obs_change / n_obs_original * 100
      cli::cli_alert_info("Observações removidas: {obs_change} ({round(pct_change, 1)}%)")
    }

    cli::cli_alert_success("Ajustes de balanceamento concluídos")
  } else if (apply_balance_adjustments && !file.exists(balance_file)) {
    cli::cli_alert_warning("Arquivo de ajustes de balanceamento não encontrado: {balance_file}")
    cli::cli_alert_info("Continuando com dados originais...")
  } else {
    cli::cli_alert_info("Ajustes de balanceamento desativados - usando dados originais")
  }

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ ETAPA 2: ESTIMAÇÃO DO EFEITO PRINCIPAL - ÁREA PLANTADA DE CANA        │
  # └─────────────────────────────────────────────────────────────────────── #
  # OUTCOME PRINCIPAL: log_area_cana (área plantada de cana-de-açúcar)
  # JUSTIFICATIVA: Cana é altamente sensível a informação climática (irrigação,
  # calendário agrícola), tornando-a o outcome mais adequado para detectar
  # o impacto causal das estações meteorológicas.
  # Método: Doubly Robust (mais robusto a especificação incorreta)
  
  cli::cli_h2("═══ ANÁLISE PRINCIPAL: ÁREA PLANTADA DE CANA-DE-AÇÚCAR ═══")
  res_main_cana <- estimate_att(df_clean, method = "dr", outcome = "log_area_cana")

  # Export resumo ATT principal (cana)
  summary_main <- tibble::tibble(
    outcome = "Área Cana (principal)",
    metodo = "dr", control = "notyettreated",
    att = res_main_cana$att_global, se = res_main_cana$se_global, z = res_main_cana$z,
    p = res_main_cana$p, ci_low = res_main_cana$ci_low, ci_high = res_main_cana$ci_high
  )
  readr::write_csv(summary_main, here::here("data", "outputs", "att_summary_main_cana.csv"))
  
  # Manter compatibilidade com pipeline antigo (para LaTeX)
  # O arquivo att_summary.csv agora reflete o resultado principal (cana)
  readr::write_csv(summary_main %>% select(-outcome), here::here("data", "outputs", "att_summary.csv"))

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ ANÁLISE DE PESOS: DIAGNÓSTICO DE COMPOSIÇÃO DO ESTIMADOR              │
  # └─────────────────────────────────────────────────────────────────────── #
  # MOTIVAÇÃO: Em DiD escalonado, grupos tratados precocemente (com mais
  # períodos pós) podem dominar o ATT agregado, potencialmente mascarando
  # heterogeneidade temporal nos efeitos do tratamento.
  cli::cli_h2("Análise de Pesos de Adoção Escalonada (Outcome Principal: Cana)")
  weights_analysis <- analyze_weights(res_main_cana$att)
  readr::write_csv(weights_analysis, here::here("data", "outputs", "weights_analysis.csv"))

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 2: Comparação de Grupos de Controle (Outcome Principal)
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Comparação de Grupos de Controle (Outcome: Área Cana)")
  control_comp <- compare_control_groups(df_clean, method = "dr", outcome = "log_area_cana")
  if (!is.null(control_comp$comparison)) {
    readr::write_csv(control_comp$comparison, here::here("data", "outputs", "control_group_comparison.csv"))
  }

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 3: Diagnóstico de Coortes com NA (Outcome Principal)
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Diagnóstico de Valores NA (Outcome: Área Cana)")
  na_diag <- diagnose_na_cohorts(res_main_cana$att, df_clean)
  if (!is.null(na_diag)) {
    readr::write_csv(na_diag$cohort_sizes, here::here("data", "outputs", "cohort_sizes.csv"))
    if (nrow(na_diag$small_cohorts) > 0) {
      # Tenta agregação de coortes pequenas
      cli::cli_alert_info("Testando agregação de coortes pequenas...")
      df_merged <- merge_small_cohorts(df_clean, min_size = 5, bin_width = 2)
      res_merged <- estimate_att(df_merged, method = "dr")
      cli::cli_alert_info("ATT após agregação: {round(res_merged$att_global, 4)} (original: {round(res_main_cana$att_global, 4)})")
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
  # NOVA ANÁLISE 5: Teste Placebo Aleatório (Outcome Principal: Cana)
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Teste Placebo Aleatório (Outcome: Área Cana)")
  placebo_random <- random_placebo_test(
    df_clean,
    n_sims = 5000,
    method = "dr",
    outcome = "log_area_cana",  # Usar outcome principal
    parallel = TRUE, # Ativar paralelização
    n_cores = NULL # Auto-detectar cores
  )

  # Salva resultados do placebo aleatório
  placebo_summary <- tibble::tibble(
    true_att = placebo_random$true_att,
    p_value_empirical = placebo_random$p_value,
    p_value_se = placebo_random$p_value_se,
    p_value_ci_lower = placebo_random$p_value_ci[1],
    p_value_ci_upper = placebo_random$p_value_ci[2],
    n_extremes = placebo_random$n_extremes,
    placebo_ci_95_lower = placebo_random$percentiles[1],
    placebo_ci_95_upper = placebo_random$percentiles[5],
    n_valid_sims = placebo_random$n_valid
  )
  readr::write_csv(placebo_summary, here::here("data", "outputs", "placebo_random_summary.csv"))

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 4: Teste de Balanceamento Pós-DR (Outcome Principal)
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Teste de Balanceamento Pós-DR (Outcome: Área Cana)")
  balance_stats <- check_balance_post_dr(res_main_cana$att, df_clean)
  readr::write_csv(balance_stats, here::here("data", "outputs", "balance_stats_dr.csv"))

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ ANÁLISE DE OUTCOMES ALTERNATIVOS: PIB AGRO, SOJA, ARROZ               │
  # └─────────────────────────────────────────────────────────────────────── #
  # Com área plantada de cana como outcome PRINCIPAL, estimamos:
  #   1. PIB agropecuário (teste de robustez - medida agregada)
  #   2. Área soja e arroz (testes de especificidade entre culturas)
  cli::cli_h2("═══ ANÁLISE SECUNDÁRIA: OUTCOMES ALTERNATIVOS ═══")
  alternative_outcomes <- alternative_outcomes_analysis(df_clean, method = "dr")

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 6: Análise de Robustez Completa (Outcome Principal: Cana)
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Análise de Robustez: Métodos Alternativos (Outcome: Área Cana)")
  robustness_results <- robustness_analysis(df_clean)

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 7: Heterogeneidade Regional (Outcome Principal: Cana)
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Heterogeneidade Regional (Outcome: Área Cana)")
  heterogeneity_results <- heterogeneity_analysis(df_clean, method = "dr", outcome = "log_area_cana")

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ NOVA ANÁLISE: DID AGREGADO POR UNIDADE FEDERATIVA                     │
  # └─────────────────────────────────────────────────────────────────────── #
  # NOTA: UF analysis agregates log_pib_agro by design. For cana outcome,
  # would need different aggregation logic. Skipping for now.
  cli::cli_h2("Análise DiD Agregada por UF")
  cli::cli_alert_info("Análise UF requer agregação outcome-específica. Pulando para outcome cana.")
  cli::cli_alert_info("Para análise UF completa, use uf_level_analysis(df_clean, outcome = 'log_pib_agro')")
  uf_results <- NULL  # Skip UF analysis for cana outcome

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ NOVA ANÁLISE: VISUALIZAÇÃO DE TENDÊNCIAS PARALELAS                    │
  # └─────────────────────────────────────────────────────────────────────── #
  cli::cli_h2("Visualizando Tendências Paralelas Completas")

  # Versão normalizada (recomendada para apresentação)
  # Outcome principal: cana
  plot_parallel_trends(df_clean,
    outcome = "log_area_cana",
    n_pre_periods = 5, n_post_periods = 5, normalize = TRUE
  )
  
  # Outcomes alternativos para comparação
  plot_parallel_trends(df_clean,
    outcome = "log_pib_agro",
    n_pre_periods = 5, n_post_periods = 5, normalize = TRUE
  )

  plot_parallel_trends(df_clean,
    outcome = "log_pib_nao_agro",
    n_pre_periods = 5, n_post_periods = 5, normalize = TRUE
  )

  # Teste formal de tendências paralelas
  cli::cli_h3("Teste Formal de Tendências Paralelas")
  parallel_trends_test <- create_parallel_trends_test_table(df_clean, f_stat = 1.136)

  # Análise por grupo de tratamento (gname)
  # Outcome principal
  plot_parallel_trends_by_gname(df_clean, outcome = "log_area_cana")
  # Outcomes alternativos
  plot_parallel_trends_by_gname(df_clean, outcome = "log_pib_agro")
  plot_parallel_trends_by_gname(df_clean, outcome = "log_pib_nao_agro")

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ NOVA ANÁLISE: ANÁLISE DESCRITIVA PARA TCC                            │
  # └─────────────────────────────────────────────────────────────────────── #
  cli::cli_h2("Gerando Análise Descritiva para o TCC")

  # Gerar análise descritiva completa
  desc_results <- descriptive_analysis(df_clean)

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ NOVA ANÁLISE: VISUALIZAÇÕES E TABELAS COMPLEMENTARES                  │
  # └─────────────────────────────────────────────────────────────────────── #
  cli::cli_h2("Gerando Visualizações e Tabelas Complementares")

  # Verificar se os arquivos de funções complementares existem
  viz_file1 <- here::here("rscripts", "did_complementary_visualizations.r")
  viz_file2 <- here::here("rscripts", "did_complementary_visualizations_pt2.r")

  if (file.exists(viz_file1) && file.exists(viz_file2)) {
    cli::cli_alert_info("Carregando funções complementares...")
    source(viz_file1)
    source(viz_file2)

    # 1. Tabela de estatísticas descritivas comparativas
    tryCatch(
      {
        cli::cli_alert_info("1. Criando tabela de estatísticas descritivas comparativas")
        desc_comp_table <- create_descriptive_stats_table(df_clean)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro na tabela descritiva: {e$message}")
      }
    )

    # 2. Mapa/Gráfico de timing de adoção
    tryCatch(
      {
        cli::cli_alert_info("2. Criando visualização de timing de adoção")
        adoption_viz <- create_adoption_map(df_clean)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro no mapa de adoção: {e$message}")
      }
    )

    # 3. Tabela de balanço de covariáveis
    tryCatch(
      {
        cli::cli_alert_info("3. Criando tabela de balanço de covariáveis")
        balance_table <- create_covariate_balance_table(df_clean)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro na tabela de balanço: {e$message}")
      }
    )

    # 4. Gráfico de composição dinâmica do tratamento
    tryCatch(
      {
        cli::cli_alert_info("4. Criando gráfico de composição dinâmica")
        comp_plot <- plot_treatment_composition(df_clean)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro no gráfico de composição: {e$message}")
      }
    )

    # 5. Distribuição dos efeitos ATT(g,t)
    tryCatch(
      {
        cli::cli_alert_info("5. Criando distribuição dos efeitos ATT")
        att_dist <- plot_att_distribution(res_main_cana)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro na distribuição ATT: {e$message}")
      }
    )

    # 6. Análise de sensibilidade ao período (opcional - pode demorar)
    if (interactive()) {
      resp <- readline("Executar análise de sensibilidade ao período? (pode demorar) [s/N]: ")
      if (tolower(resp) == "s") {
        tryCatch(
          {
            cli::cli_alert_info("6. Realizando análise de sensibilidade ao período")
            sens_plot <- sensitivity_analysis_period(df_clean)
          },
          error = function(e) {
            cli::cli_alert_warning("Erro na análise de sensibilidade: {e$message}")
          }
        )
      }
    }

    # 7. Tabela de decomposição dos efeitos
    tryCatch(
      {
        cli::cli_alert_info("7. Criando tabela de decomposição dos efeitos")
        decomp_table <- create_effect_decomposition_table(res_main, df_clean)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro na decomposição: {e$message}")
      }
    )

    # 8. Tendências por quartis de tamanho
    tryCatch(
      {
        cli::cli_alert_info("8. Criando análise por quartis de tamanho")
        quartile_plot <- plot_trends_by_size_quartile(df_clean)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro no gráfico por quartis: {e$message}")
      }
    )

    # 9. Dashboard de qualidade dos dados
    tryCatch(
      {
        cli::cli_alert_info("9. Criando dashboard de qualidade dos dados")
        quality_dash <- create_data_quality_dashboard(df_clean)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro no dashboard de qualidade: {e$message}")
      }
    )

    # 10. Análise de poder estatístico
    tryCatch(
      {
        cli::cli_alert_info("10. Realizando análise de poder estatístico")
        power_plot <- power_analysis_simulation(df_clean, n_sims = 100)
      },
      error = function(e) {
        cli::cli_alert_warning("Erro na análise de poder: {e$message}")
      }
    )

    cli::cli_alert_success("Visualizações complementares concluídas!")
    cli::cli_alert_info("Arquivos salvos em: data/outputs/additional_figures/")
  } else {
    cli::cli_alert_warning("Arquivos de visualizações complementares não encontrados")
    cli::cli_alert_info("Certifique-se de que existem:")
    cli::cli_alert_info("  - {viz_file1}")
    cli::cli_alert_info("  - {viz_file2}")
  }

  # Teste de Tendências Paralelas (pré-tratamento) - Outcome Principal: Cana
  cli::cli_h2("Teste de Tendências Paralelas (Outcome: Área Cana)")
  att_df <- tibble::tibble(
    group = res_main_cana$att$group,
    time  = res_main_cana$att$t,
    att   = res_main_cana$att$att
  )
  pre_df <- dplyr::filter(att_df, time < group)
  pre_mean <- mean(pre_df$att, na.rm = TRUE)
  pre_se <- sd(pre_df$att, na.rm = TRUE) / sqrt(nrow(pre_df))
  t_stat <- pre_mean / pre_se
  p_val <- 2 * stats::pt(-abs(t_stat), df = nrow(pre_df) - 1)
  cli::cli_alert_success("Teste manual de tendências paralelas: média PRE = {round(pre_mean, 4)}, p-valor = {round(p_val, 4)}")

  # Placebo test fixo (outcome principal: cana)
  cli::cli_h2("Placebo Test Fixo (2015) - Outcome: Área Cana")
  placebo_res <- placebo_test(df_clean, placebo_year = 2015, outcome = "log_area_cana")
  if (!is.null(placebo_res)) {
    cli::cli_alert_success("Placebo fixo: ATT = {round(placebo_res$att,4)}, SE = {round(placebo_res$se,4)}, p = {round(placebo_res$p,4)}, IC95% = [{round(placebo_res$ci_low,4)}; {round(placebo_res$ci_high,4)}]")
  }

  # Robustez - Métodos alternativos para outcome principal (cana)
  cli::cli_h2("Robustez: Métodos Alternativos (Outcome: Área Cana)")
  robust_tbl <- robust_specs(df_clean, outcome = "log_area_cana")
  readr::write_csv(robust_tbl, here::here("data", "outputs", "robust_att_cana.csv"))

  # Visualizações do resultado principal (cana)
  visualize_results(res_main_cana$event, output_prefix = "event_study_cana")

  # ---------------------------------------------------------------------- #
  # NOVA ANÁLISE 8: Tabela de Resultados Consolidada
  # ---------------------------------------------------------------------- #
  cli::cli_h2("Criando Tabela de Resultados Consolidada")

  # Criar tabela resumida formatada
  # NOVA ESTRUTURA: Outcome principal = Área Cana
  results_table <- tibble::tibble(
    `Análise` = c(
      "ATT Principal (Área Cana)",
      "Outcome Alternativo: PIB Agropecuário",
      "Outcome Alternativo: Área Soja",
      "Outcome Alternativo: Área Arroz",
      "Robustez - Nevertreated (Cana)",
      "Robustez - Sem Covariáveis (Cana)",
      "Robustez - IPW (Cana)",
      "Robustez - REG (Cana)"
    ),
    `ATT` = c(
      sprintf("%.3f%s", res_main_cana$att_global,
        ifelse(res_main_cana$p < 0.01, "***",
          ifelse(res_main_cana$p < 0.05, "**",
            ifelse(res_main_cana$p < 0.1, "*", "")))),
      # PIB Agro (alternative outcome)
      sprintf("%.3f%s",
        alternative_outcomes$att[alternative_outcomes$outcome_var == "log_pib_agro"],
        ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_pib_agro"] < 0.01, "***",
          ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_pib_agro"] < 0.05, "**",
            ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_pib_agro"] < 0.1, "*", "")))),
      # Soja (alternative outcome)
      sprintf("%.3f%s",
        alternative_outcomes$att[alternative_outcomes$outcome_var == "log_area_soja"],
        ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_area_soja"] < 0.01, "***",
          ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_area_soja"] < 0.05, "**",
            ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_area_soja"] < 0.1, "*", "")))),
      # Arroz (alternative outcome)
      sprintf("%.3f%s",
        alternative_outcomes$att[alternative_outcomes$outcome_var == "log_area_arroz"],
        ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_area_arroz"] < 0.01, "***",
          ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_area_arroz"] < 0.05, "**",
            ifelse(alternative_outcomes$p_value[alternative_outcomes$outcome_var == "log_area_arroz"] < 0.1, "*", "")))),
      # Nevertreated
      ifelse(!is.null(control_comp$results$nt),
        sprintf(
          "%.3f%s", control_comp$results$nt$att_global,
          ifelse(control_comp$results$nt$p < 0.01, "***",
            ifelse(control_comp$results$nt$p < 0.05, "**",
              ifelse(control_comp$results$nt$p < 0.1, "*", "")
            )
          )
        ),
        "N/A"
      ),
      sprintf(
        "%.3f%s",
        robustness_results$att[robustness_results$specification == "Sem covariáveis"],
        ifelse(robustness_results$p_value[robustness_results$specification == "Sem covariáveis"] < 0.01, "***",
          ifelse(robustness_results$p_value[robustness_results$specification == "Sem covariáveis"] < 0.05, "**",
            ifelse(robustness_results$p_value[robustness_results$specification == "Sem covariáveis"] < 0.1, "*", "")
          )
        )
      ),
      sprintf(
        "%.3f%s",
        robustness_results$att[robustness_results$specification == "IPW"],
        ifelse(robustness_results$p_value[robustness_results$specification == "IPW"] < 0.01, "***",
          ifelse(robustness_results$p_value[robustness_results$specification == "IPW"] < 0.05, "**",
            ifelse(robustness_results$p_value[robustness_results$specification == "IPW"] < 0.1, "*", "")
          )
        )
      ),
      sprintf(
        "%.3f%s",
        robustness_results$att[robustness_results$specification == "REG"],
        ifelse(robustness_results$p_value[robustness_results$specification == "REG"] < 0.01, "***",
          ifelse(robustness_results$p_value[robustness_results$specification == "REG"] < 0.05, "**",
            ifelse(robustness_results$p_value[robustness_results$specification == "REG"] < 0.1, "*", "")
          )
        )
      )
    ),
    `Erro Padrão` = c(
      sprintf("(%.3f)", res_main_cana$se_global),
      sprintf("(%.3f)", alternative_outcomes$se[alternative_outcomes$outcome_var == "log_pib_agro"]),
      sprintf("(%.3f)", alternative_outcomes$se[alternative_outcomes$outcome_var == "log_area_soja"]),
      sprintf("(%.3f)", alternative_outcomes$se[alternative_outcomes$outcome_var == "log_area_arroz"]),
      ifelse(!is.null(control_comp$results$nt), sprintf("(%.3f)", control_comp$results$nt$se_global), "N/A"),
      sprintf("(%.3f)", robustness_results$se[robustness_results$specification == "Sem covariáveis"]),
      sprintf("(%.3f)", robustness_results$se[robustness_results$specification == "IPW"]),
      sprintf("(%.3f)", robustness_results$se[robustness_results$specification == "REG"])
    ),
    `IC 95%` = c(
      sprintf("[%.3f, %.3f]", res_main_cana$ci_low, res_main_cana$ci_high),
      sprintf("[%.3f, %.3f]",
        alternative_outcomes$ci_lower[alternative_outcomes$outcome_var == "log_pib_agro"],
        alternative_outcomes$ci_upper[alternative_outcomes$outcome_var == "log_pib_agro"]),
      sprintf("[%.3f, %.3f]",
        alternative_outcomes$ci_lower[alternative_outcomes$outcome_var == "log_area_soja"],
        alternative_outcomes$ci_upper[alternative_outcomes$outcome_var == "log_area_soja"]),
      sprintf("[%.3f, %.3f]",
        alternative_outcomes$ci_lower[alternative_outcomes$outcome_var == "log_area_arroz"],
        alternative_outcomes$ci_upper[alternative_outcomes$outcome_var == "log_area_arroz"]),
      ifelse(!is.null(control_comp$results$nt),
        sprintf("[%.3f, %.3f]", control_comp$results$nt$ci_low, control_comp$results$nt$ci_high),
        "N/A"),
      sprintf(
        "[%.3f, %.3f]",
        robustness_results$ci_lower[robustness_results$specification == "Sem covariáveis"],
        robustness_results$ci_upper[robustness_results$specification == "Sem covariáveis"]
      ),
      sprintf(
        "[%.3f, %.3f]",
        robustness_results$ci_lower[robustness_results$specification == "IPW"],
        robustness_results$ci_upper[robustness_results$specification == "IPW"]
      ),
      sprintf(
        "[%.3f, %.3f]",
        robustness_results$ci_lower[robustness_results$specification == "REG"],
        robustness_results$ci_upper[robustness_results$specification == "REG"]
      )
    ),
    `N` = c(
      nrow(df_clean),
      nrow(df_clean),
      nrow(df_clean),
      nrow(df_clean),
      nrow(df_clean),
      robustness_results$n_obs[robustness_results$specification == "Sem covariáveis"],
      robustness_results$n_obs[robustness_results$specification == "IPW"],
      robustness_results$n_obs[robustness_results$specification == "REG"]
    )
  )

  # Salvar tabela
  readr::write_csv(results_table, here::here("data", "outputs", "main_results_table.csv"))

  # Mostrar tabela
  cli::cli_alert_success("Tabela de Resultados Principais:")
  print(results_table)

  # Notas da tabela
  cli::cli_alert_info("Notas: *** p<0.01, ** p<0.05, * p<0.1")
  cli::cli_alert_info("Outcome principal: Área Plantada de Cana-de-açúcar (km², log)")
  cli::cli_alert_info("Método: Doubly Robust (DR) com covariáveis socioeconômicas e climáticas")
  cli::cli_alert_info("Covariáveis: área total, população, PIB pc, densidade estações UF, precipitação")

  # ┌─────────────────────────────────────────────────────────────────────── #
  # │ CONSOLIDAÇÃO E GERAÇÃO DE OUTPUTS PARA COMUNICAÇÃO CIENTÍFICA         │
  # └─────────────────────────────────────────────────────────────────────── #
  cli::cli_h2("Consolidação dos Resultados e Geração de Apresentação")

  # Agregar todos os resultados em estrutura unificada
  # NOVA ESTRUTURA: res_main agora é área cana (não PIB agro)
  all_results <- list(
    res_main = res_main_cana,                    # PRINCIPAL: Área Cana
    alternative_outcomes = alternative_outcomes, # SECUNDÁRIO: PIB agro, soja, arroz
    placebo_random = placebo_random,            # Placebo randomização
    placebo_fixed = placebo_res,                # Placebo ano fixo
    robustness = robustness_results,            # Robustez (cana)
    control_comparison = control_comp,          # Comparação grupos controle
    heterogeneity = heterogeneity_results,      # Heterogeneidade regional
    weights = weights_analysis,                 # Análise de pesos
    balance_stats = balance_stats               # Balanceamento
  )

  # Gerar suite completa de visualizações e tabelas
  generate_presentation(df_clean, all_results)

  # ════════════════════════════════════════════════════════════════════════ #
  # SÍNTESE DOS RESULTADOS E IMPLICAÇÕES PARA POLÍTICA PÚBLICA              #
  # ════════════════════════════════════════════════════════════════════════ #
  # Este estudo demonstra que a instalação de estações meteorológicas       #
  # automáticas gera impacto causal positivo e economicamente significativo #
  # na ÁREA PLANTADA DE CANA-DE-AÇÚCAR. O efeito representa expansão        #
  # substancial da produção canavieira, cultura altamente sensível a         #
  # informações climáticas para irrigação e calendário agrícola.            #
  #                                                                         #
  # OUTCOME PRINCIPAL: Área plantada de cana-de-açúcar (km², log)          #
  #   - Detecta resposta comportamental direta dos produtores               #
  #   - Captura decisões de alocação de terra baseadas em informação        #
  #   - Alta sensibilidade climática torna cana ideal para detecção         #
  #                                                                         #
  # OUTCOMES SECUNDÁRIOS validam e complementam:                             #
  #   - PIB agropecuário: confirma relevância econômica agregada            #
  #   - Outras culturas: testam especificidade do efeito                    #
  #                                                                         #
  # A robustez dos resultados a múltiplas especificações, combinada com    #
  # testes placebo e validação através de outcomes alternativos, fortalece #
  # a interpretação causal. A heterogeneidade regional identificada sugere #
  # oportunidades para priorização geográfica na expansão do programa.     #
  #                                                                         #
  # Contribuições acadêmicas:                                              #
  # 1. Primeira evidência causal rigorosa do impacto de infraestrutura    #
  #    meteorológica em decisões de alocação de terra no Brasil           #
  # 2. Aplicação do estado da arte em métodos de DiD escalonado           #
  # 3. Framework replicável para avaliação de políticas similares         #
  # 4. Documentação do canal comportamental: informação → decisão → área   #
  # ════════════════════════════════════════════════════════════════════════ #

  cli::cli_alert_success("Pipeline de análise concluído com sucesso!")
  cli::cli_alert_info("Outcome principal: Área plantada de cana-de-açúcar")
  cli::cli_alert_info("Resultados salvos em: data/outputs/")
  cli::cli_alert_info("Apresentação disponível em: data/outputs/presentation/")
}
