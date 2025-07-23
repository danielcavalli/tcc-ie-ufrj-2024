WITH
  climate_aggregates AS (
    SELECT
      mun.id_microrregiao,
      EXTRACT(YEAR FROM mic.data) AS year,
      -- Precipitação
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 1 THEN mic.precipitacao_total END) AS avg_precipitacao_jan,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 2 THEN mic.precipitacao_total END) AS avg_precipitacao_feb,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 3 THEN mic.precipitacao_total END) AS avg_precipitacao_mar,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 4 THEN mic.precipitacao_total END) AS avg_precipitacao_apr,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 5 THEN mic.precipitacao_total END) AS avg_precipitacao_may,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 6 THEN mic.precipitacao_total END) AS avg_precipitacao_jun,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 7 THEN mic.precipitacao_total END) AS avg_precipitacao_jul,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 8 THEN mic.precipitacao_total END) AS avg_precipitacao_aug,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 9 THEN mic.precipitacao_total END) AS avg_precipitacao_sep,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 10 THEN mic.precipitacao_total END) AS avg_precipitacao_oct,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 11 THEN mic.precipitacao_total END) AS avg_precipitacao_nov,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 12 THEN mic.precipitacao_total END) AS avg_precipitacao_dec,
      -- Temperatura (bulbo seco)
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 1 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_jan,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 2 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_feb,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 3 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_mar,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 4 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_apr,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 5 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_may,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 6 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_jun,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 7 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_jul,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 8 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_aug,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 9 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_sep,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 10 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_oct,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 11 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_nov,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 12 THEN mic.temperatura_bulbo_hora END) AS avg_temp_bulbo_dec,
      -- Umidade Relativa
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 1 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_jan,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 2 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_feb,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 3 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_mar,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 4 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_apr,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 5 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_may,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 6 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_jun,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 7 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_jul,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 8 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_aug,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 9 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_sep,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 10 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_oct,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 11 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_nov,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 12 THEN mic.umidade_rel_hora END) AS avg_umidade_rel_dec,
      -- Temperatura máxima
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 1 THEN mic.temperatura_max END) AS avg_temp_max_jan,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 2 THEN mic.temperatura_max END) AS avg_temp_max_feb,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 3 THEN mic.temperatura_max END) AS avg_temp_max_mar,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 4 THEN mic.temperatura_max END) AS avg_temp_max_apr,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 5 THEN mic.temperatura_max END) AS avg_temp_max_may,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 6 THEN mic.temperatura_max END) AS avg_temp_max_jun,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 7 THEN mic.temperatura_max END) AS avg_temp_max_jul,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 8 THEN mic.temperatura_max END) AS avg_temp_max_aug,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 9 THEN mic.temperatura_max END) AS avg_temp_max_sep,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 10 THEN mic.temperatura_max END) AS avg_temp_max_oct,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 11 THEN mic.temperatura_max END) AS avg_temp_max_nov,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 12 THEN mic.temperatura_max END) AS avg_temp_max_dec,
      -- Vento velocidade
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 1 THEN mic.vento_velocidade END) AS avg_vento_vel_jan,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 2 THEN mic.vento_velocidade END) AS avg_vento_vel_feb,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 3 THEN mic.vento_velocidade END) AS avg_vento_vel_mar,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 4 THEN mic.vento_velocidade END) AS avg_vento_vel_apr,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 5 THEN mic.vento_velocidade END) AS avg_vento_vel_may,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 6 THEN mic.vento_velocidade END) AS avg_vento_vel_jun,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 7 THEN mic.vento_velocidade END) AS avg_vento_vel_jul,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 8 THEN mic.vento_velocidade END) AS avg_vento_vel_aug,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 9 THEN mic.vento_velocidade END) AS avg_vento_vel_sep,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 10 THEN mic.vento_velocidade END) AS avg_vento_vel_oct,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 11 THEN mic.vento_velocidade END) AS avg_vento_vel_nov,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 12 THEN mic.vento_velocidade END) AS avg_vento_vel_dec,
      -- Pressão atmosférica horária
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 1 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_jan,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 2 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_feb,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 3 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_mar,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 4 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_apr,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 5 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_may,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 6 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_jun,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 7 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_jul,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 8 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_aug,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 9 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_sep,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 10 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_oct,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 11 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_nov,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 12 THEN mic.pressao_atm_hora END) AS avg_pressao_atm_dec
    FROM
      `basedosdados.br_inmet_bdmep.microdados` AS mic
    JOIN
      `basedosdados.br_inmet_bdmep.estacao` AS est
    ON
      mic.id_estacao = est.id_estacao
    JOIN
      `basedosdados.br_bd_diretorios_brasil.municipio` AS mun
    ON
      est.id_municipio = mun.id_municipio
    WHERE
      EXTRACT(YEAR FROM mic.data) BETWEEN 2001 AND 2024
    GROUP BY
      mun.id_microrregiao,
      year
  ),
  
  production_data AS (
    SELECT
      mun.id_microrregiao,
      lav.ano,
      SUM(lav.area_colhida)/SUM(lav.area_plantada) AS total_valor_producao
    FROM
      `basedosdados.br_ibge_pam.lavoura_temporaria` AS lav
    JOIN
      `basedosdados.br_bd_diretorios_brasil.municipio` AS mun
    ON
      lav.id_municipio = mun.id_municipio
    WHERE
      lav.ano BETWEEN 2001 AND 2024
      AND lav.produto = 'Cana-de-açúcar'  -- Filtro para Cana-de-açúcar
    GROUP BY
      mun.id_microrregiao,
      lav.ano
  ),
  
  treatment_flags AS (
    SELECT
      mun.id_microrregiao,
      EXTRACT(YEAR FROM MIN(mic.data)) + 1 AS treatment_year,  -- Um ano após o primeiro dado registrado
      1 AS treated
    FROM
      `basedosdados.br_inmet_bdmep.microdados` AS mic
    JOIN
      `basedosdados.br_inmet_bdmep.estacao` AS est
    ON
      mic.id_estacao = est.id_estacao
    JOIN
      `basedosdados.br_bd_diretorios_brasil.municipio` AS mun
    ON
      est.id_municipio = mun.id_municipio
    WHERE
      EXTRACT(YEAR FROM mic.data) BETWEEN 2000 AND 2023  -- Ativações até o ano anterior ao último solicitado
    GROUP BY
      mun.id_microrregiao
  )

SELECT
  prod.id_microrregiao,
  prod.ano,
  prod.total_valor_producao,
  clm.*,
  COALESCE(treated.treated, 0) AS treatment
FROM
  production_data AS prod
JOIN
  climate_aggregates AS clm
ON
  prod.id_microrregiao = clm.id_microrregiao
  AND prod.ano = clm.year
LEFT JOIN
  treatment_flags AS treated
ON
  prod.id_microrregiao = treated.id_microrregiao
  AND prod.ano = treated.treatment_year
ORDER BY
  prod.id_microrregiao,
  prod.ano;