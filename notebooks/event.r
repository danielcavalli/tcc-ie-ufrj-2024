data_cleaned <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% # Preencher NA com 0
  mutate(
    ano_relativo = ano - primeiro_ano_tratamento,  # Criar ano relativo
    treated = ifelse(tratado == 1, 1, 0)          # Criar indicador de tratamento binário
  )

# Ajustar o modelo com efeitos fixos para Estudo de Eventos
event_study <- feols(
  produtividade ~ i(ano_relativo, ref = -1) + total_area_plantada + qtd_estacoes_ativas | id_microrregiao + ano,
  data = data_cleaned
)

# Resumo dos resultados
summary(event_study)
