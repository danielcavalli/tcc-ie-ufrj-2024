WITH
  climate_aggregates AS (
    SELECT
      mun.id_microrregiao,
      EXTRACT(YEAR FROM mic.data) AS year,
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
      
      -- TODO Repeat this extraction for other climate variables
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 1 THEN mic.pressao_atm_hora END) AS avg_pressao_jan,
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 2 THEN mic.pressao_atm_hora END) AS avg_pressao_feb,
      -- Add similar columns for each climate variable and each month
      AVG(CASE WHEN EXTRACT(MONTH FROM mic.data) = 12 THEN mic.pressao_atm_hora END) AS avg_pressao_dec
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
      SUM(lav.valor_producao) AS total_valor_producao
    FROM
      `basedosdados.br_ibge_pam.lavoura_temporaria` AS lav
    JOIN
      `basedosdados.br_bd_diretorios_brasil.municipio` AS mun
    ON
      lav.id_municipio = mun.id_municipio
    WHERE
      lav.ano BETWEEN 2001 AND 2024
    GROUP BY
      mun.id_microrregiao,
      lav.ano
  ),
  
  treatment_flags AS (
    SELECT
      mun.id_microrregiao,
      EXTRACT(YEAR FROM MIN(mic.data)) + 1 AS treatment_year,  -- One year after the first recorded data point
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
      EXTRACT(YEAR FROM mic.data) BETWEEN 2000 AND 2023  -- Ensure activations up to the year before the last requested year
    GROUP BY
      mun.id_microrregiao
  )

SELECT
  prod.id_microrregiao,
  prod.ano,
  prod.total_valor_producao,
  clm.avg_precipitacao_jan,
  clm.avg_precipitacao_feb,
  clm.avg_precipitacao_mar,
  clm.avg_precipitacao_apr,
  clm.avg_precipitacao_may,
  clm.avg_precipitacao_jun,
  clm.avg_precipitacao_jul,
  clm.avg_precipitacao_aug,
  clm.avg_precipitacao_sep,
  clm.avg_precipitacao_oct,
  clm.avg_precipitacao_nov,
  clm.avg_precipitacao_dec,
  -- clm.avg_pressao_jan,
  -- clm.avg_pressao_feb,
  -- -- Add remaining columns for each climate variable and month
  -- clm.avg_pressao_dec,
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
