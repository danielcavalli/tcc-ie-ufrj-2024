WITH
  microrregiao_total_estacoes AS (
    SELECT
      mun.id_microrregiao,
      COUNT(DISTINCT est.id_estacao) AS total_estacoes
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
      mic.data IS NOT NULL
      AND EXTRACT(YEAR FROM mic.data) BETWEEN 2000 AND 2024
    GROUP BY
      mun.id_microrregiao
  ),
  estacoes_por_ano AS (
    SELECT
      mun.id_microrregiao,
      EXTRACT(YEAR FROM mic.data) AS ano,
      COUNT(DISTINCT est.id_estacao) AS qtd_estacoes_ativas
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
      mic.data IS NOT NULL
      AND EXTRACT(YEAR FROM mic.data) BETWEEN 2000 AND 2024
    GROUP BY
      mun.id_microrregiao,
      ano
  ),
  primeiro_ano_tratamento AS (
    SELECT
      id_microrregiao,
      MIN(ano) AS primeiro_ano_tratamento
    FROM
      estacoes_por_ano
    WHERE
      qtd_estacoes_ativas >= 5
    GROUP BY
      id_microrregiao
  ),
  tratamento AS (
    SELECT
      epa.id_microrregiao,
      epa.ano,
      epa.qtd_estacoes_ativas,
      CASE
        WHEN epa.qtd_estacoes_ativas >= 5 THEN 1 ELSE 0
      END AS tratado,
      COALESCE(pat.primeiro_ano_tratamento, 0) as primeiro_ano_tratamento
    FROM
      estacoes_por_ano AS epa
    LEFT JOIN
      primeiro_ano_tratamento AS pat
    ON
      epa.id_microrregiao = pat.id_microrregiao
  ),
  production_data AS (
    SELECT
      mun.id_microrregiao,
      lav.ano,
      SUM(lav.rendimento_medio_producao) AS produtividade,
      SUM(lav.area_plantada) AS total_area_plantada
    FROM
      `basedosdados.br_ibge_pam.lavoura_temporaria` AS lav
    JOIN
      `basedosdados.br_bd_diretorios_brasil.municipio` AS mun
    ON
      lav.id_municipio = mun.id_municipio
    WHERE
      lav.ano BETWEEN 2001 AND 2024
    --   AND lav.produto = 'Cana-de-açúcar'
    GROUP BY
      mun.id_microrregiao,
      lav.ano
  ),
  climate_aggregates AS (
    SELECT
      mun.id_microrregiao,
      EXTRACT(YEAR FROM mic.data) AS ano,
      SUM(mic.precipitacao_total) AS total_precipitacao
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
      mic.data IS NOT NULL
      AND EXTRACT(YEAR FROM mic.data) BETWEEN 2000 AND 2024
    GROUP BY
      mun.id_microrregiao,
      ano
  )
SELECT
  prod.id_microrregiao,
  prod.ano,
  prod.produtividade,
  prod.total_area_plantada,
  clm.total_precipitacao,
  clm.total_precipitacao / prod.total_area_plantada AS precipitacao_por_area_plantada,
  trat.qtd_estacoes_ativas,
  trat.tratado,
  trat.primeiro_ano_tratamento,
  CASE
    WHEN prod.ano >= trat.primeiro_ano_tratamento and trat.primeiro_ano_tratamento != 0 THEN 1 ELSE 0
  END AS pos_tratamento
FROM
  production_data AS prod
LEFT JOIN
  climate_aggregates AS clm
ON
  prod.id_microrregiao = clm.id_microrregiao
  AND prod.ano = clm.ano
LEFT JOIN
  tratamento AS trat
ON
  prod.id_microrregiao = trat.id_microrregiao
  AND prod.ano = trat.ano
WHERE
  prod.total_area_plantada > 0  -- Evita divisão por zero
ORDER BY
  trat.qtd_estacoes_ativas DESC,
  prod.id_microrregiao,
  prod.ano;