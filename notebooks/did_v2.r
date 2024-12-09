library(did)
library(dplyr)
library(ggplot2)

# Leitura dos dados
df <- read.csv("data/csv/WightedMethod/sugar_cane_treated.csv")

# 1. Preparação dos dados ------------------------------

set.seed(42)

# Transformar a variável produtividade em log
df <- df %>%
  mutate(log_produtividade = log(produtividade))

# Ajustar a variável gname: se a unidade nunca for tratada, colocar 0 ou NA.
# Aqui assumimos que todas as unidades são tratadas em algum momento.
df <- df %>%
  group_by(id_microrregiao) %>%
  mutate(gname = primeiro_ano_tratamento) %>%
  ungroup()

df <- df %>%
  rename(treated = tratado)

head(df)

# 2. Estimação ATT(g,t) com att_gt() -------------------

# São incluídas as covariáveis total_area_plantada e precipitacao_por_area_plantada
# Pelo método Doubly Robust (est_method = "dr")
att_results <- att_gt(
  yname = "log_produtividade",
  tname = "ano",
  idname = "id_microrregiao",
  gname = "gname",
  xformla = ~ qtd_estacoes_ativas + precipitacao_por_area_plantada,
  data = df,
  est_method = "dr"
)

# 3. Agregação dos Efeitos com aggte() -----------------

# (a) Efeito Médio Global (Overall Treatment Effect)
agg_overall <- aggte(att_results, type = "group", na.rm = TRUE)

# (b) Event Study (efeito por tempo de exposição)
# Definir type = "dynamic" cria uma análise de estudo de evento
agg_event <- aggte(att_results, type = "dynamic", na.rm = TRUE)

# 4. Visualização dos Resultados -----------------------

# (a) Gráfico do Event Study
event_df <- data.frame(
  time_relative = agg_event$egt,
  att = agg_event$att.egt,
  se = agg_event$se.egt
)

ggplot(event_df, aes(x = time_relative, y = att)) +
  # Faixa de confiança (intervalo de confiança) usando geom_ribbon
  geom_ribbon(aes(ymin = att - 1.96*se, ymax = att + 1.96*se), fill = "blue", alpha = 0.2) +
  
  # Linha do efeito estimado
  geom_line(color = "blue", size = 1) +
  
  # Pontos sobre a linha
  geom_point(color = "blue", size = 2) +
  
  # Linha horizontal em y=0 para referencia
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  
  # Linha vertical em x=0 para marcar o início do tratamento
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  # Ajuste no tema
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90"),
    plot.title = element_text(face = "bold", size = 16)
  ) +
  
  # Legenda
  labs(
    title = "Event Study: Efeito do Tratamento ao Longo do Tempo de Disponibilidade da Estação",
    x = "Períodos após o tratamento",
    y = "Efeito estimado (log-produtividade)"
  )

ggsave(
  "event_study_plot.png",
  plot = last_plot(), 
  width = 12,
  height = 6 
)

# ==============================================================================
# Interpretação dos resultados 
# ==============================================================================

# 1. Resumo dos ATT(g,t)
summary(att_results)
summary(agg_overall)  # Para o efeito médio global
summary(agg_event)    # Para o event study agregado

# O objeto retornado por att_gt() possui componentes que armazenam os ATT(g,t) individuais
# Para acessá-los individualmente, os coloquei na tabela abaixo:
att_table <- data.frame(
  group = att_results$group,
  time = att_results$t,
  att = att_results$att,
  se = att_results$se,
  n = att_results$n
)

head(att_table)

# 2. Estatísticas Descritivas dos ATT
# Média, mediana, desvio padrão, minímas e máximas dos efeitos estimados
summary_att <- att_table %>%
  summarise(
    mean_att = mean(att, na.rm = TRUE),
    median_att = median(att, na.rm = TRUE),
    sd_att = sd(att, na.rm = TRUE),
    min_att = min(att, na.rm = TRUE),
    max_att = max(att, na.rm = TRUE)
  )

print(summary_att)

# 3. Validando as Tendências Pré-Tratamento
# Filtramos os períodos anteriores ao tratamento (time < group)
att_pre <- att_table %>% 
  filter(time < group)

# Calculamos a Média dos ATT pré-tratamento  que devem estar, idealmente, próximo de zero # nolint
pre_trend_mean <- mean(att_pre$att, na.rm = TRUE)
pre_trend_sd <- sd(att_pre$att, na.rm = TRUE)

cat("Média do ATT pré-tratamento:", pre_trend_mean, 
    "\nDesvio padrão do ATT pré-tratamento:", pre_trend_sd, "\n")

# Número de observações pré-tratamento
n <- nrow(att_pre)

# Cálculo da estatística t
t_stat <- pre_trend_mean / (pre_trend_sd / sqrt(n))

# Cálculo do p-valor (teste bilateral)
p_value <- 2 * pt(-abs(t_stat), df = n-1)

# Para um intervalo de confiança de 95%
error_margin <- qt(0.975, df = n-1) * (pre_trend_sd / sqrt(n))
ci_lower <- pre_trend_mean - error_margin
ci_upper <- pre_trend_mean + error_margin

cat("Estatística t pré:", t_stat, "\nP-valor pré:", p_value, 
    "\nIntervalo de Confiança (95%) pré: [", ci_lower, ",", ci_upper, "]\n")


# 4. Validando as Tendências Pós-Tratamento
# Filtramos os períodos pós tratamento (time >= group)
att_post <- att_table %>% 
  filter(time >= group)

# Calculamos a Média e Desvio Padrão dos ATT pós-tratamento
post_trend_mean <- mean(att_post$att, na.rm = TRUE)
post_trend_sd <- sd(att_post$att, na.rm = TRUE)

# Número de observações pós-tratamento
n_post <- nrow(att_post)

# Cálculo da estatística t
t_stat_post <- post_trend_mean / (post_trend_sd / sqrt(n_post))

# Cálculo do p-valor (teste bilateral)
p_value_post <- 2 * pt(-abs(t_stat_post), df = n_post-1)

# Para um intervalo de confiança de 95%
error_margin_post <- qt(0.975, df = n_post-1) * (post_trend_sd / sqrt(n_post))
ci_lower_post <- post_trend_mean - error_margin_post
ci_upper_post <- post_trend_mean + error_margin_post

cat("Estatística t pós:", t_stat_post, "\nP-valor pós:", p_value_post, 
    "\nIntervalo de Confiança (95%) pós: [", ci_lower_post, ",", ci_upper_post, "]\n")

# 5. Interpretação do Efeito Médio Global
summary(agg_overall)

# O objeto agg_overall$att.agg fornece o efeito médio global,
# e agg_overall$se.agg o erro padrão. 
# Podemos interpretar esse efeito da seguinte forma:
overall_effect <- agg_overall$att.agg
overall_se <- agg_overall$se.agg

cat("Overall Treatment Effect (ETE):", overall_effect, 
    "\nSE:", overall_se,
    "\nInterpretação: Um valor positivo significa que, em média,\n",
    "a intervenção aumentou o log da produtividade. Por exemplo,",
    "um efeito de 0.1 implica um aumento aproximado de 10,5%\n",
    "na produtividade.\n")

# 6. Interpretação do Event Study (dinâmica do efeito)
summary(agg_event)

event_df <- data.frame(
  event_time = agg_event$egt,
  att = agg_event$att.egt,
  se = agg_event$se.egt
)
# Verificamos se, em média, o efeito pós-tratamento é maior que o pré-tratamento
post_event_att <- mean(event_df$att[event_df$event_time > 0], na.rm = TRUE)
pre_event_att <- mean(event_df$att[event_df$event_time < 0], na.rm = TRUE)

cat("Média do efeito pós-tratamento:", post_event_att,
    "\nMédia do efeito pré-tratamento:", pre_event_att,
    "\nSe a média pós > média pré e há evidência estatística (intervalos não cruzam zero),",
    "o tratamento parece ter impacto positivo ao longo do tempo.\n")

ggplot(att_table, aes(x = factor(group), y = att)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribuição dos ATT(g,t) por Coorte (Group)",
    x = "Ano Inicial de Tratamento (Coorte)",
    y = "Efeito estimado (ATT)"
  )

ggsave(
  "distribution.png",
  plot = last_plot(), 
  width = 12,
  height = 6 
)

# ADICIONAL

# ==============================================================================
# Algumas visualizações dos dados
# ==============================================================================


library(dplyr)

desc_stats <- df %>%
  mutate(periodo = ifelse(pos_tratamento == 0, "Pre", "Pos"),
         grupo = ifelse(treated == 1, "Tratado", "Não Tratado")) %>%
  group_by(grupo, periodo) %>%
  summarise(
    mean_prod = mean(produtividade, na.rm = TRUE),
    median_prod = median(produtividade, na.rm = TRUE),
    mean_area = mean(total_area_plantada, na.rm = TRUE),
    median_area = median(total_area_plantada, na.rm = TRUE),
    mean_precip = mean(total_precipitacao, na.rm = TRUE),
    median_precip = median(total_precipitacao, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

desc_stats

unidades_ano <- df %>%
  group_by(ano, treated) %>%
  summarise(num_unidades = n_distinct(id_microrregiao), .groups = "drop")

print(n = 100, unidades_ano)

# Menor e maior ano na amostra
range_anos <- range(df$ano)
pre_trat <- df %>% filter(pos_tratamento == 0)
pos_trat <- df %>% filter(pos_tratamento == 1)

cat("Intervalo de anos:", range_anos, "\n")
cat("Anos pré-tratamento disponíveis:", length(unique(pre_trat$ano)), "\n")
cat("Anos pós-tratamento disponíveis:", length(unique(pos_trat$ano)), "\n")

summary_precip_area <- df %>%
  group_by(treated, pos_tratamento) %>%
  summarise(
    mean_ratio = mean(precipitacao_por_area_plantada, na.rm = TRUE),
    median_ratio = median(precipitacao_por_area_plantada, na.rm = TRUE),
    sd_ratio = sd(precipitacao_por_area_plantada, na.rm = TRUE),
    .groups = "drop"
  )

summary_precip_area

estacoes_ano <- df %>%
  group_by(ano) %>%
  summarise(
    mean_estacoes = mean(qtd_estacoes_ativas, na.rm = TRUE),
    median_estacoes = median(qtd_estacoes_ativas, na.rm = TRUE),
    .groups = "drop"
  )

print(n = 100,estacoes_ano)


prod_ano <- df %>%
  group_by(ano, treated) %>%
  summarise(mean_prod = mean(log_produtividade, na.rm = TRUE), .groups = "drop")

ggplot(prod_ano, aes(x = ano, y = mean_prod, color = factor(treated))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Evolução da Produtividade ao Longo do Tempo",
       x = "Ano",
       y = "Produtividade Média",
       color = "Tratado") +
  scale_color_discrete(labels = c("0" = "Não Tratado", "1" = "Tratado"))

df_periodo_grupo <- df %>%
  mutate(periodo = ifelse(pos_tratamento == 0, "Pré", "Pós"),
         grupo = ifelse(treated == 1, "Tratado", "Não Tratado"),
         cat = paste(grupo, periodo))

ggplot(df_periodo_grupo, aes(x = cat, y = produtividade)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribuição da Produtividade por Grupo e Período",
       x = "Grupo e Período",
       y = "Produtividade")

ggplot(estacoes_ano, aes(x = ano, y = mean_estacoes)) +
  geom_line(size = 1, color = "blue") +
  theme_minimal() +
  labs(title = "Evolução do Número Médio de Estações Ativas ao Longo do Tempo",
       x = "Ano",
       y = "Média de Estações Ativas")



