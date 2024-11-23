install.packages("fixest")


# Load necessary packages
library(did)
library(dplyr)

# Assuming your data is in a CSV file or similar, load it
# Replace 'your_data.csv' with the actual file name or data frame
data <- read.csv("data/csv/all_crops.csv")

# Prepare data for the `did` function
# Ensure the data has the necessary columns: id_microrregiao, ano, produtividade, tratado, ano_tratamento, pos_tratamento, etc.
data_prepared <- data_prepared %>%
  group_by(id_microrregiao) %>%
  mutate(
    has_pre_treatment = any(ano < primeiro_ano_tratamento)  # Identifica se há períodos antes do tratamento
  ) %>%
  ungroup() %>%
  filter(has_pre_treatment)  # Filtrar apenas microrregiões com períodos pré-tratamento


data_cleaned <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% # Preencher NA com 0
  group_by(id_microrregiao) %>%
  filter(any(ano < primeiro_ano_tratamento) | primeiro_ano_tratamento == 0) %>% # Manter microrregiões com pré-tratamento ou nunca tratadas
  ungroup()

# Criar indicador de "not yet treated"
data_cleaned <- data_cleaned %>%
  mutate(
    not_yet_treated = ifelse(ano < primeiro_ano_tratamento | primeiro_ano_tratamento == 0, 1, 0)
  )

# Filtrar os dados para incluir apenas grupos "not yet treated" nos controles
control_data <- data_cleaned %>%
  filter(not_yet_treated == 1)

summary(data_cleaned)

# Run the DiD model using `att_gt` function
did_model <- att_gt(
  yname = "produtividade",         # Variável dependente
  tname = "ano",                  # Variável de tempo
  idname = "id_microrregiao",     # Identificador único
  gname = "primeiro_ano_tratamento",  # Ano do primeiro tratamento
  xformla = ~ total_area_plantada + total_precipitacao + precipitacao_por_area_plantada +
               qtd_estacoes_ativas,  # Covariáveis
  data = data_cleaned,
  control_group = "notyettreated"
)

# Summarize the results
summary(did_model)

ggdid(did_model)

# Plot the results
# Extraindo os resultados do did_model
plot_data <- ggdid(did_model, quiet = TRUE)$data

# Criando o gráfico personalizado com ggplot2
library(ggplot2)

# Filtrar os dados para excluir os grupos 2008 e 2016
filtered_plot_data <- plot_data %>%
  filter(group != 2008)

# Criar o gráfico com faceting (um painel por grupo)
ggplot(filtered_plot_data, aes(x = year, y = att)) +
  geom_line(color = "blue", linewidth = 1) + # Linhas por grupo
  geom_point(color = "blue", size = 2) + # Pontos nas estimativas
  geom_errorbar(aes(ymin = att - 1.96 * att.se, ymax = att + 1.96 * att.se), 
                width = 0.2, color = "darkblue") + # Barras de erro
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Linha de referência
  labs(
    title = "Efeitos Médios do Tratamento ao Longo do Tempo (ATT-GT)",
    x = "Ano",
    y = "ATT (Efeito Médio do Tratamento)"
  ) +
  facet_wrap(~ group, nrow = 3, scales = "free_x") + # Faceting por grupo
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centralizar o título
    axis.text = element_text(size = 12), # Texto dos eixos
    axis.title = element_text(size = 14), # Título dos eixos
    strip.text = element_text(size = 14, face = "bold") # Títulos dos painéis
  )





install.packages(did)
install.packages(dplyr)
install.packages("officer")
install.packages("flextable")
install.packages(ggplot2)

# Carregar pacotes necessários
library(did)
library(dplyr)
library(officer)
library(flextable)
library(ggplot2)

# Assumindo que o modelo `did_model` já foi gerado
# Extrair os resultados do modelo para uma tabela
results_table <- data.frame(
  Group = did_model$group,
  Time = did_model$t,
  ATT = did_model$att,
  `Std.Error` = did_model$se,
  Conf.Low = did_model$att - 1.96 * did_model$se,
  Conf.High = did_model$att + 1.96 * did_model$se
)

# Converter para flextable para facilitar a exportação
results_ft <- flextable(results_table) %>%
  set_header_labels(
    Group = "Group (Year of Treatment)",
    Time = "Time (Year)",
    ATT = "ATT Estimate",
    `Std. Error` = "Standard Error",
    `Conf. Low` = "95% Conf. Low",
    `Conf. High` = "95% Conf. High"
  ) %>%
  autofit()

# Criar um gráfico de coeficientes
coef_plot <- ggplot(results_table, aes(x = Time, y = ATT, group = Group)) +
  geom_point(aes(color = as.factor(Group))) +
  geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High, color = as.factor(Group)), width = 0.3) +
  labs(
    title = "Average Treatment Effects Over Time",
    x = "Time (Year)",
    y = "ATT Estimate",
    color = "Group (Year of Treatment)"
  ) +
  theme_minimal()

# Salvar o gráfico como imagem temporária
temp_graph <- tempfile(fileext = ".png")
ggsave(temp_graph, coef_plot, width = 8, height = 5)

# Criar o documento Word
doc <- read_docx() %>%
  body_add_par("Resultados do Modelo DID", style = "heading 1") %>%
  body_add_par("Tabela de Resultados", style = "heading 2") %>%
  body_add_flextable(results_ft) %>%
  body_add_par("Visualização dos Coeficientes", style = "heading 2") %>%
  body_add_img(src = temp_graph, width = 6, height = 4) %>%
  body_add_par("Observação: Os intervalos de confiança ao nível de 95% são apresentados para cada coeficiente estimado.", style = "Normal")

# Salvar o documento Word
print(doc, target = "resultados_did.docx")

