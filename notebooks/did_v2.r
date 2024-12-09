library(did)
library(dplyr)
library(ggplot2)

# Leitura dos dados
df <- read.csv("data/csv/WightedMethod/all_crops.csv")

# 1. Preparação dos dados ------------------------------

# Transformar a variável produtividade em log
df <- df %>%
  mutate(log_produtividade = log(produtividade))

# Ajustar a variável gname: se a unidade nunca for tratada, colocar 0 ou NA.
# Aqui assumimos que todas as unidades são tratadas em algum momento.
df <- df %>%
  group_by(id_microrregiao) %>%
  mutate(gname = primeiro_ano_tratamento) %>%
  ungroup()

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
# Definir type = "dynamic" criará uma análise de estudo de evento
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
  
  # Rótulos
  labs(
    title = "Event Study: Efeito do Tratamento ao Longo do Tempo de Exposição",
    x = "Períodos após o tratamento",
    y = "Efeito estimado (log-produtividade)"
  )

ggsave(
  "event_study_plot.png", # Nome do arquivo
  plot = last_plot(),     # Último gráfico gerado (ou substitua pelo objeto do gráfico)
  width = 12,             # Largura maior
  height = 6              # Altura ajustada
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
# "0" Marque o início do tratamento para cada grupo
# Portanto, filtramos os períodos anteriores ao tratamento (time < group)
att_pre <- att_table %>% 
  filter(time < group)

# Média dos ATT pré-tratamento (idealmente próximo de zero)
pre_trend_mean <- mean(att_pre$att, na.rm = TRUE)
pre_trend_sd <- sd(att_pre$att, na.rm = TRUE)

cat("Média do ATT pré-tratamento:", pre_trend_mean, 
    "\nDesvio padrão do ATT pré-tratamento:", pre_trend_sd, "\n")

# Se essa média estiver próxima de zero e não significativa,
# isso sugere tendências paralelas razoáveis.

# 4. Interpretação do Efeito Médio Global
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

# 5. Interpretação do Event Study (dinâmica do efeito)
summary(agg_event)

# O objeto agg_event fornece ATT por tempo de exposição.
# Podemos visualizar se o efeito aumenta ou diminui ao longo do tempo.
event_df <- data.frame(
  event_time = agg_event$egt,
  att = agg_event$att.egt,
  se = agg_event$se.egt
)

# Vamos verificar se, em média, o efeito pós-tratamento é maior que o pré-tratamento
post_event_att <- mean(event_df$att[event_df$event_time > 0], na.rm = TRUE)
pre_event_att <- mean(event_df$att[event_df$event_time < 0], na.rm = TRUE)

cat("Média do efeito pós-tratamento:", post_event_att,
    "\nMédia do efeito pré-tratamento:", pre_event_att,
    "\nSe a média pós > média pré e há evidência estatística (intervalos não cruzam zero),",
    "o tratamento parece ter impacto positivo ao longo do tempo.\n")

