#!/usr/bin/env python3
"""
Dataset de Microrregiões Brasileiras para Análise

Extração com mapeamento CORRETO de microrregiões

Este script extrai dados da Base dos Dados usando o mapeamento correto 
município → microrregião, gerando um dataset consolidado para análises econométricas.

FONTES DE DADOS:
- ÁREAS: MapBiomas (área plantada + área total)
- VALOR AGRÍCOLA: PAM/IBGE (apenas valor agregado)
- POPULAÇÃO E PIB: IBGE
- TRATAMENTO: Estações INMET

Como usar:
----------
Um único produto:
    PRODUTOS_AGRICOLAS = ['Soja']

Múltiplos produtos (agregados):
    PRODUTOS_AGRICOLAS = ['Milho', 'Soja']  # Análise conjunta

Exemplos de produtos disponíveis (com MapBiomas):
- ['Cana-de-açúcar'] - Padrão
- ['Soja'] - Apenas soja
- ['Arroz'] - Arroz

NOTA: Apenas produtos com mapeamento MapBiomas são suportados.
"""

import basedosdados as bd
import pandas as pd
import os
import json
from itertools import product


# =============================================================================
# CONFIGURAÇÃO
# =============================================================================

PROJECT_ID = "bdsdasd"
ANOS = list(range(2003, 2022))

# CONFIGURAÇÃO DOS PRODUTOS AGRÍCOLAS
# Pode ser uma lista com 1 ou mais produtos!
# IMPORTANTE: Usar nomes EXATOS conforme PAM/IBGE
PRODUTOS_AGRICOLAS = ['Cana-de-açúcar', 'Soja (em grão)', 'Arroz (em casca)']

# Garantir que sempre seja uma lista
if isinstance(PRODUTOS_AGRICOLAS, str):
    PRODUTOS_AGRICOLAS = [PRODUTOS_AGRICOLAS]

# MAPEAMENTO: PAM produto → MapBiomas id_classe
MAPEAMENTO_PAM_MAPBIOMAS = {
    'Cana-de-açúcar': '20',
    'Soja (em grão)': '39',
    'Arroz (em casca)': '40'
}

# MAPEAMENTO: PAM produto → nome curto para coluna
MAPEAMENTO_NOME_CURTO = {
    'Cana-de-açúcar': 'cana',
    'Soja (em grão)': 'soja',
    'Arroz (em casca)': 'arroz'
}

# Validar que todos os produtos têm mapeamento MapBiomas
produtos_sem_mapbiomas = [p for p in PRODUTOS_AGRICOLAS if p not in MAPEAMENTO_PAM_MAPBIOMAS]
if produtos_sem_mapbiomas:
    raise ValueError(f"❌ Produtos sem mapeamento MapBiomas: {produtos_sem_mapbiomas}\n"
                     f"   Produtos disponíveis: {list(MAPEAMENTO_PAM_MAPBIOMAS.keys())}")

os.makedirs("output", exist_ok=True)

print("✅ Configuração completa!")
print(f"📅 Período: {ANOS[0]}-{ANOS[-1]}")
print(f"🌾 Produtos selecionados: {', '.join(PRODUTOS_AGRICOLAS)}")
print(f"🗺️  MapBiomas classes: {[MAPEAMENTO_PAM_MAPBIOMAS[p] for p in PRODUTOS_AGRICOLAS]}")


# =============================================================================
# 1. MAPEAMENTO MUNICÍPIO → MICRORREGIÃO
# =============================================================================

print("\n" + "="*80)
print("1. MAPEAMENTO MUNICÍPIO → MICRORREGIÃO")
print("="*80)

query_mapeamento = """
SELECT 
    id_municipio,
    id_microrregiao,
    nome AS nome_municipio,
    sigla_uf
FROM 
    `basedosdados.br_bd_diretorios_brasil.municipio`
"""

print("🗺️ Baixando mapeamento município → microrregião...")
df_municipios = bd.read_sql(query_mapeamento, billing_project_id=PROJECT_ID)

# Ensure consistent ID types (string) throughout the script
df_municipios['id_municipio'] = df_municipios['id_municipio'].astype(str)
df_municipios['id_microrregiao'] = df_municipios['id_microrregiao'].astype(str)

# Remover municípios sem mapeamento de microrregião
n_total = len(df_municipios)
df_municipios = df_municipios[df_municipios['id_microrregiao'].notna()].copy()
n_validos = len(df_municipios)

if n_total > n_validos:
    print(f"⚠️  Removidos {n_total - n_validos} municípios sem id_microrregiao")

print(f"✅ {len(df_municipios):,} municípios mapeados")
print(f"✅ {df_municipios['id_microrregiao'].nunique()} microrregiões identificadas")

# Validate that microrregiões don't cross state boundaries
ufs_por_micro = df_municipios.groupby('id_microrregiao')['sigla_uf'].nunique()
if (ufs_por_micro > 1).any():
    print("⚠️  WARNING: Some microrregiões cross state boundaries!")
    micros_problema = ufs_por_micro[ufs_por_micro > 1].index.tolist()
    print(f"   Microrregiões problemáticas: {micros_problema}")
else:
    print("✅ Validação: Todas as microrregiões estão dentro de um único estado")

print("\nExemplo do mapeamento:")
print(df_municipios.head())


# =============================================================================
# 2. ESTAÇÕES METEOROLÓGICAS (TRATAMENTO)
# =============================================================================

print("\n" + "="*80)
print("2. ESTAÇÕES METEOROLÓGICAS (TRATAMENTO)")
print("="*80)

query_estacoes = """
SELECT
    e.id_municipio,
    e.id_estacao,
    e.estacao AS nome_estacao,
    EXTRACT(YEAR FROM e.data_fundacao) AS ano_fundacao,
    e.latitude,
    e.longitude
FROM
    `basedosdados.br_inmet_bdmep.estacao` e
WHERE
    e.data_fundacao IS NOT NULL
    AND e.id_municipio IS NOT NULL
"""

print("🌡️  Baixando dados de estações...")
df_estacoes_mun = bd.read_sql(query_estacoes, billing_project_id=PROJECT_ID)

# Fazer o JOIN com o mapeamento
df_estacoes_full = df_estacoes_mun.merge(
    df_municipios[['id_municipio', 'id_microrregiao']], 
    on='id_municipio', 
    how='left'
)

# Agregar por microrregião
df_estacoes = df_estacoes_full.groupby('id_microrregiao').agg({
    'ano_fundacao': 'min',
    'id_estacao': 'count'
}).reset_index()

df_estacoes.columns = ['id_microrregiao', 'primeiro_ano_estacao', 'num_estacoes']

print(f"✅ {len(df_estacoes)} microrregiões com estações ({len(df_estacoes)/558*100:.1f}% de cobertura)")
print(f"✅ Total: {df_estacoes_full['id_estacao'].nunique()} estações")
print(df_estacoes.head())


# =============================================================================
# 3. POPULAÇÃO E PIB
# =============================================================================

print("\n" + "="*80)
print("3. POPULAÇÃO E PIB")
print("="*80)

# POPULAÇÃO
query_pop = f"""
SELECT
    ano,
    id_municipio,
    populacao
FROM
    `basedosdados.br_ibge_populacao.municipio`
WHERE
    ano BETWEEN {ANOS[0]} AND {ANOS[-1]}
"""

print("👥 Baixando dados de população...")
df_pop_mun = bd.read_sql(query_pop, billing_project_id=PROJECT_ID)
df_pop_mun['id_municipio'] = df_pop_mun['id_municipio'].astype(str)

# JOIN com mapeamento
df_pop_mapped = df_pop_mun.merge(
    df_municipios[['id_municipio', 'id_microrregiao', 'sigla_uf']],
    on='id_municipio',
    how='left'
)

# Report data loss from mapping
n_sem_mapping_pop = df_pop_mapped['id_microrregiao'].isna().sum()
if n_sem_mapping_pop > 0:
    print(f"⚠️  {n_sem_mapping_pop} registros de população sem mapeamento de microrregião (serão descartados)")
    df_pop_mapped = df_pop_mapped[df_pop_mapped['id_microrregiao'].notna()].copy()

# Agregar por microrregião
df_pop = df_pop_mapped.groupby(['ano', 'id_microrregiao', 'sigla_uf'])['populacao'].sum().reset_index()
df_pop.columns = ['ano', 'id_microrregiao', 'uf', 'populacao_total']

print(f"✅ {len(df_pop):,} registros agregados")

# PIB
query_pib = f"""
SELECT
    ano,
    id_municipio,
    pib,
    va_agropecuaria
FROM
    `basedosdados.br_ibge_pib.municipio`
WHERE
    ano BETWEEN {ANOS[0]} AND {ANOS[-1]}
"""

print("\n💰 Baixando dados de PIB...")
df_pib_mun = bd.read_sql(query_pib, billing_project_id=PROJECT_ID)
df_pib_mun['id_municipio'] = df_pib_mun['id_municipio'].astype(str)

# JOIN com mapeamento
df_pib_mapped = df_pib_mun.merge(
    df_municipios[['id_municipio', 'id_microrregiao']],
    on='id_municipio',
    how='left'
)

# Report data loss from mapping
n_sem_mapping_pib = df_pib_mapped['id_microrregiao'].isna().sum()
if n_sem_mapping_pib > 0:
    print(f"⚠️  {n_sem_mapping_pib} registros de PIB sem mapeamento de microrregião (serão descartados)")
    df_pib_mapped = df_pib_mapped[df_pib_mapped['id_microrregiao'].notna()].copy()

# Agregar
df_pib = df_pib_mapped.groupby(['ano', 'id_microrregiao']).agg({
    'pib': 'sum',
    'va_agropecuaria': 'sum'
}).reset_index()

df_pib.columns = ['ano', 'id_microrregiao', 'pib_total', 'pib_agropecuario']

print(f"✅ {len(df_pib):,} registros agregados")


# =============================================================================
# 4. CONSOLIDAÇÃO DO DATASET
# =============================================================================

print("\n" + "="*80)
print("4. CONSOLIDAÇÃO DO DATASET")
print("="*80)

# Obter lista de todas as microrregiões
all_micros = sorted(df_municipios['id_microrregiao'].unique())
print(f"📊 Total de microrregiões: {len(all_micros)}")

# Criar painel balanceado
painel = pd.DataFrame(
    list(product(all_micros, ANOS)), 
    columns=['id_microrregiao', 'ano']
)

# Adicionar informações básicas das microrregiões
micro_info = df_municipios.groupby('id_microrregiao')['sigla_uf'].first().reset_index()
painel = painel.merge(micro_info, on='id_microrregiao', how='left')

# Adicionar tratamento (estações)
painel = painel.merge(
    df_estacoes[['id_microrregiao', 'primeiro_ano_estacao']], 
    on='id_microrregiao', 
    how='left'
)

painel['primeiro_ano_tratamento'] = painel['primeiro_ano_estacao'].fillna(0).astype(int)
painel['tratado'] = (painel['primeiro_ano_tratamento'] != 0).astype(int)
painel['pos_tratamento'] = (
    (painel['ano'] >= painel['primeiro_ano_tratamento']) & 
    (painel['tratado'] == 1)
).astype(int)

# Adicionar população
painel = painel.merge(
    df_pop[['ano', 'id_microrregiao', 'populacao_total']], 
    on=['ano', 'id_microrregiao'], 
    how='left'
)

# Adicionar PIB
painel = painel.merge(
    df_pib, 
    on=['ano', 'id_microrregiao'], 
    how='left'
)

# PIB per capita
painel['pib_per_capita'] = (painel['pib_total'] / painel['populacao_total']).round(2)

print(f"\n✅ Painel criado: {len(painel):,} observações")
print(f"📊 Microrregiões tratadas: {painel[painel['tratado'] == 1]['id_microrregiao'].nunique()}")
print(f"📊 Microrregiões controle: {painel[painel['tratado'] == 0]['id_microrregiao'].nunique()}")


# =============================================================================
# 5. DADOS DE VALOR AGRÍCOLA (PAM) - APENAS VALOR
# =============================================================================

print("\n" + "="*80)
print("5. DADOS DE VALOR AGRÍCOLA (PAM) - APENAS VALOR")
print("="*80)

# Converter lista de produtos para formato SQL
produtos_sql = ', '.join([f"'{p}'" for p in PRODUTOS_AGRICOLAS])

query_pam = f"""
SELECT
    lav.id_municipio,
    lav.ano,
    lav.produto,
    lav.valor_producao
FROM
    `basedosdados.br_ibge_pam.lavoura_temporaria` AS lav
WHERE
    lav.ano BETWEEN {ANOS[0]} AND {ANOS[-1]}
    AND lav.produto IN ({produtos_sql})
"""

print(f"💰 Baixando dados de valor de produção de: {', '.join(PRODUTOS_AGRICOLAS)}...")
df_pam_mun = bd.read_sql(query_pam, billing_project_id=PROJECT_ID)
df_pam_mun['id_municipio'] = df_pam_mun['id_municipio'].astype(str)
print(f"✅ {len(df_pam_mun):,} registros de valor municipal")

# JOIN com mapeamento para obter microrregião
df_pam_mapped = df_pam_mun.merge(
    df_municipios[['id_municipio', 'id_microrregiao']],
    on='id_municipio',
    how='left'
)

# Report data loss from mapping
n_sem_mapping_pam = df_pam_mapped['id_microrregiao'].isna().sum()
if n_sem_mapping_pam > 0:
    print(f"⚠️  {n_sem_mapping_pam} registros de PAM sem mapeamento de microrregião (serão descartados)")
    df_pam_mapped = df_pam_mapped[df_pam_mapped['id_microrregiao'].notna()].copy()

# Agregar por microrregião E produto (manter produtos separados)
df_valor_por_produto = df_pam_mapped.groupby(['ano', 'id_microrregiao', 'produto']).agg({
    'valor_producao': 'sum'
}).reset_index()

# Mapear produto para nome curto
df_valor_por_produto['produto_curto'] = df_valor_por_produto['produto'].map(MAPEAMENTO_NOME_CURTO)

# Check which products have data
produtos_com_dados = df_valor_por_produto['produto'].unique()
produtos_sem_dados = [p for p in PRODUTOS_AGRICOLAS if p not in produtos_com_dados]
if produtos_sem_dados:
    print(f"⚠️  Produtos sem dados de valor de produção (PAM): {produtos_sem_dados}")

# Pivotar: cada produto vira uma coluna valor_producao_<produto>
df_valor = df_valor_por_produto.pivot_table(
    index=['ano', 'id_microrregiao'],
    columns='produto_curto',
    values='valor_producao',
    fill_value=0
).reset_index()

# Renomear colunas para valor_producao_<produto>
df_valor.columns.name = None  # Remover nome do índice de colunas
novos_nomes = {col: f'valor_producao_{col}' for col in df_valor.columns if col not in ['ano', 'id_microrregiao']}
df_valor = df_valor.rename(columns=novos_nomes)

# Ensure all expected columns exist (add missing ones with zeros)
for produto in PRODUTOS_AGRICOLAS:
    nome_curto = MAPEAMENTO_NOME_CURTO[produto]
    col_name = f'valor_producao_{nome_curto}'
    if col_name not in df_valor.columns:
        print(f"⚠️  Criando coluna vazia: {col_name}")
        df_valor[col_name] = 0

print(f"✅ {len(df_valor):,} registros de valor agregados por microrregião")
print(f"✅ Colunas criadas: {sorted([col for col in df_valor.columns if col.startswith('valor_producao_')])}")
for col in sorted([col for col in df_valor.columns if col.startswith('valor_producao_')]):
    n_nonzero = (df_valor[col] > 0).sum()
    if n_nonzero > 0:
        media = df_valor[df_valor[col] > 0][col].mean()
        print(f"   📊 {col}: {n_nonzero} obs com valor > 0, média = R$ {media:,.0f}")
    else:
        print(f"   📊 {col}: 0 obs com valor > 0 (sem dados PAM)")


# =============================================================================
# 6. DADOS DE USO DO SOLO (MAPBIOMAS)
# =============================================================================

print("\n" + "="*80)
print("6. DADOS DE USO DO SOLO (MAPBIOMAS)")
print("="*80)

# Obter IDs das classes MapBiomas para os produtos selecionados
ids_classes = [MAPEAMENTO_PAM_MAPBIOMAS[p] for p in PRODUTOS_AGRICOLAS]
# id_classe é STRING no BigQuery, então precisa de aspas
ids_str = ', '.join([f"'{id_classe}'" for id_classe in ids_classes])

# Criar mapeamento reverso: id_classe → nome_curto
id_classe_para_nome = {MAPEAMENTO_PAM_MAPBIOMAS[p]: MAPEAMENTO_NOME_CURTO[p] for p in PRODUTOS_AGRICOLAS}

print(f"🗺️  Classes MapBiomas: {ids_str}")
print(f"🗺️  Mapeamento: {id_classe_para_nome}")

# Query 1: Área plantada do(s) produto(s)
query_mapbiomas_plantada = f"""
SELECT
    ano,
    id_municipio,
    id_classe,
    area
FROM 
    `basedosdados.br_mapbiomas_estatisticas.cobertura_municipio_classe`
WHERE 
    ano BETWEEN {ANOS[0]} AND {ANOS[-1]}
    AND id_classe IN ({ids_str})
"""

print("🌾 Baixando área plantada (MapBiomas)...")
df_mapbiomas_mun = bd.read_sql(query_mapbiomas_plantada, billing_project_id=PROJECT_ID)
df_mapbiomas_mun['id_municipio'] = df_mapbiomas_mun['id_municipio'].astype(str)
print(f"✅ {len(df_mapbiomas_mun):,} registros municipais de área plantada")

# JOIN com mapeamento
df_mapbiomas_mapped = df_mapbiomas_mun.merge(
    df_municipios[['id_municipio', 'id_microrregiao']],
    on='id_municipio',
    how='left'
)

# Report data loss from mapping
n_sem_mapping_mapbiomas = df_mapbiomas_mapped['id_microrregiao'].isna().sum()
if n_sem_mapping_mapbiomas > 0:
    print(f"⚠️  {n_sem_mapping_mapbiomas} registros de MapBiomas sem mapeamento de microrregião (serão descartados)")
    df_mapbiomas_mapped = df_mapbiomas_mapped[df_mapbiomas_mapped['id_microrregiao'].notna()].copy()

# Agregar por microrregião E id_classe (manter produtos separados)
df_area_por_classe = df_mapbiomas_mapped.groupby(['ano', 'id_microrregiao', 'id_classe']).agg({
    'area': 'sum'
}).reset_index()

# Mapear id_classe para nome do produto
df_area_por_classe['produto'] = df_area_por_classe['id_classe'].astype(str).map(id_classe_para_nome)

# Pivotar: cada produto vira uma coluna area_plantada_<produto>
df_area_plantada = df_area_por_classe.pivot_table(
    index=['ano', 'id_microrregiao'],
    columns='produto',
    values='area',
    fill_value=0
).reset_index()

# Renomear colunas para area_plantada_<produto>
df_area_plantada.columns.name = None  # Remover nome do índice de colunas
novos_nomes = {col: f'area_plantada_{col}' for col in df_area_plantada.columns if col not in ['ano', 'id_microrregiao']}
df_area_plantada = df_area_plantada.rename(columns=novos_nomes)

print(f"✅ {len(df_area_plantada):,} registros de área plantada por microrregião")
print(f"✅ Colunas criadas: {[col for col in df_area_plantada.columns if col.startswith('area_plantada_')]}")
for col in df_area_plantada.columns:
    if col.startswith('area_plantada_'):
        media = df_area_plantada[df_area_plantada[col] > 0][col].mean()
        print(f"   📊 {col}: média = {media:,.1f} km²")

# Query 2: Área total por município
query_area_total = f"""
SELECT
    ano,
    id_municipio,
    SUM(area) as area_total_km2
FROM 
    `basedosdados.br_mapbiomas_estatisticas.cobertura_municipio_classe`
WHERE 
    ano BETWEEN {ANOS[0]} AND {ANOS[-1]}
GROUP BY 
    ano, id_municipio
"""

print("\n🌍 Baixando área total (MapBiomas)...")
df_area_total_mun = bd.read_sql(query_area_total, billing_project_id=PROJECT_ID)
df_area_total_mun['id_municipio'] = df_area_total_mun['id_municipio'].astype(str)
print(f"✅ {len(df_area_total_mun):,} registros municipais de área total")

# JOIN com mapeamento
df_area_total_mapped = df_area_total_mun.merge(
    df_municipios[['id_municipio', 'id_microrregiao']],
    on='id_municipio',
    how='left'
)

# Report data loss from mapping
n_sem_mapping_area_total = df_area_total_mapped['id_microrregiao'].isna().sum()
if n_sem_mapping_area_total > 0:
    print(f"⚠️  {n_sem_mapping_area_total} registros de área total sem mapeamento de microrregião (serão descartados)")
    df_area_total_mapped = df_area_total_mapped[df_area_total_mapped['id_microrregiao'].notna()].copy()

# Agregar por microrregião
df_area_total = df_area_total_mapped.groupby(['ano', 'id_microrregiao']).agg({
    'area_total_km2': 'sum'
}).reset_index()

print(f"✅ {len(df_area_total):,} registros de área total por microrregião")
print(f"📊 Área total média: {df_area_total['area_total_km2'].sum():,.1f} km²")


# =============================================================================
# 7. DADOS DE PRECIPITAÇÃO (CDS/ERA5 local)
# =============================================================================

print("\n" + "="*80)
print("7. DADOS DE PRECIPITAÇÃO (CDS/ERA5 local)")
print("="*80)

# Ler CSV local de precipitação
precip_path = 'CDS/precipitacao_municipal_anual_com_centroides.csv'
print(f"🌧️  Carregando dados de precipitação: {precip_path}")
df_precip_mun = pd.read_csv(precip_path)

# Renomear CD_MUN para id_municipio para compatibilidade
df_precip_mun = df_precip_mun.rename(columns={'CD_MUN': 'id_municipio'})

# Garantir que id_municipio é do mesmo tipo que no mapeamento
df_precip_mun['id_municipio'] = df_precip_mun['id_municipio'].astype(str)

# Selecionar apenas as colunas necessárias (ano, id_municipio, e as três variáveis "_mm")
colunas_precip = ['id_municipio', 'ano', 'precip_total_anual_mm', 'precip_media_mensal_mm', 'precip_max_mensal_mm']
df_precip_clean = df_precip_mun[colunas_precip].copy()

print(f"✅ {len(df_precip_clean):,} registros municipais de precipitação")
print(f"📊 Anos disponíveis: {sorted(df_precip_clean['ano'].unique())}")

# JOIN com mapeamento município → microrregião
df_precip_mapped = df_precip_clean.merge(
    df_municipios[['id_municipio', 'id_microrregiao']], 
    on='id_municipio', 
    how='left'
)

# Verificar cobertura do mapeamento
n_sem_micro = df_precip_mapped['id_microrregiao'].isna().sum()
if n_sem_micro > 0:
    print(f"⚠️  {n_sem_micro} registros de precipitação sem mapeamento de microrregião (serão descartados)")
    df_precip_mapped = df_precip_mapped[df_precip_mapped['id_microrregiao'].notna()].copy()

# Agregar precipitação por microrregião-ano
# Regra de agregação:
#   - precip_total_anual_mm: soma (total acumulado da microrregião)
#   - precip_media_mensal_mm: média (média das médias municipais)
#   - precip_max_mensal_mm: média (média dos máximos municipais)
df_precip = df_precip_mapped.groupby(['ano', 'id_microrregiao']).agg({
    'precip_total_anual_mm': 'sum',
    'precip_media_mensal_mm': 'mean',
    'precip_max_mensal_mm': 'mean'
}).reset_index()

print(f"✅ {len(df_precip):,} registros de precipitação agregados por microrregião")
print(f"📊 Precipitação total anual média: {df_precip['precip_total_anual_mm'].mean():,.1f} mm")
print(f"📊 Precipitação média mensal média: {df_precip['precip_media_mensal_mm'].mean():,.1f} mm")
print(f"📊 Precipitação máxima mensal média: {df_precip['precip_max_mensal_mm'].mean():,.1f} mm")


# =============================================================================
# 8. CONSOLIDAÇÃO FINAL DO PAINEL
# =============================================================================

print("\n" + "="*80)
print("8. CONSOLIDAÇÃO FINAL DO PAINEL")
print("="*80)

# Começar com o painel base
painel_final = painel.copy()

# Remover coluna auxiliar se existir
if 'primeiro_ano_estacao' in painel_final.columns:
    painel_final = painel_final.drop('primeiro_ano_estacao', axis=1)

# Adicionar valor agregado (PAM)
painel_final = painel_final.merge(
    df_valor, 
    on=['ano', 'id_microrregiao'], 
    how='left'
)

# Adicionar área plantada (MapBiomas)
painel_final = painel_final.merge(
    df_area_plantada, 
    on=['ano', 'id_microrregiao'], 
    how='left'
)

# Adicionar área total (MapBiomas)
painel_final = painel_final.merge(
    df_area_total, 
    on=['ano', 'id_microrregiao'], 
    how='left'
)

# Adicionar precipitação (CDS/ERA5)
painel_final = painel_final.merge(
    df_precip, 
    on=['ano', 'id_microrregiao'], 
    how='left'
)

# Preencher zeros onde não há dados
colunas_area_plantada = [col for col in painel_final.columns if col.startswith('area_plantada_')]
colunas_valor_producao = [col for col in painel_final.columns if col.startswith('valor_producao_')]

painel_final['area_total_km2'] = painel_final['area_total_km2'].fillna(0)

# Preencher zeros para todas as colunas de área plantada por produto
for col in colunas_area_plantada:
    painel_final[col] = painel_final[col].fillna(0)

# Preencher zeros para todas as colunas de valor de produção por produto
for col in colunas_valor_producao:
    painel_final[col] = painel_final[col].fillna(0)

# Fill precipitation with 0 as well for consistency
painel_final['precip_total_anual_mm'] = painel_final['precip_total_anual_mm'].fillna(0)
painel_final['precip_media_mensal_mm'] = painel_final['precip_media_mensal_mm'].fillna(0)
painel_final['precip_max_mensal_mm'] = painel_final['precip_max_mensal_mm'].fillna(0)

# Validate balanced panel
n_esperado = len(all_micros) * len(ANOS)
n_observado = len(painel_final)
if n_esperado != n_observado:
    print(f"\n⚠️  WARNING: Panel is not balanced!")
    print(f"   Esperado: {n_esperado:,} observações ({len(all_micros)} micros × {len(ANOS)} anos)")
    print(f"   Observado: {n_observado:,} observações")
    print(f"   Diferença: {n_esperado - n_observado:,} observações faltando")
else:
    print(f"\n✅ Validação: Painel balanceado ({n_observado:,} observações)")

# Filter out microrregiões with ZERO agricultural activity across ALL years
# Keep microrregiões that have EITHER MapBiomas area data OR PAM production value data
print("\n" + "="*80)
print("FILTRO: Removendo microrregiões sem produção agrícola")
print("="*80)
print(f"📊 Tamanho antes do filtro: {len(painel_final):,} observações")
print(f"📊 Microrregiões antes: {painel_final['id_microrregiao'].nunique()}")

# Calculate total planted area per microrregião across all years and crops (MapBiomas)
colunas_area_plantada = [col for col in painel_final.columns if col.startswith('area_plantada_')]
painel_final['area_total_produtos'] = painel_final[colunas_area_plantada].sum(axis=1)

# Calculate total production value per microrregião across all years and crops (PAM)
colunas_valor_producao = [col for col in painel_final.columns if col.startswith('valor_producao_')]
painel_final['valor_total_produtos'] = painel_final[colunas_valor_producao].sum(axis=1)

# Identify microrregiões with at least SOME area OR value in at least ONE year
micros_com_area = painel_final.groupby('id_microrregiao')['area_total_produtos'].sum()
micros_com_area = set(micros_com_area[micros_com_area > 0].index)

micros_com_valor = painel_final.groupby('id_microrregiao')['valor_total_produtos'].sum()
micros_com_valor = set(micros_com_valor[micros_com_valor > 0].index)

# Union: keep microrregiões with EITHER area OR value
micros_com_producao = micros_com_area.union(micros_com_valor)

print(f"   - Microrregiões com MapBiomas área > 0: {len(micros_com_area)}")
print(f"   - Microrregiões com PAM valor > 0: {len(micros_com_valor)}")
print(f"   - Total com área OU valor: {len(micros_com_producao)}")

# Filter the panel
n_micros_antes = painel_final['id_microrregiao'].nunique()
painel_final = painel_final[painel_final['id_microrregiao'].isin(micros_com_producao)].copy()
n_micros_depois = painel_final['id_microrregiao'].nunique()

# Drop the auxiliary columns
painel_final = painel_final.drop(['area_total_produtos', 'valor_total_produtos'], axis=1)

print(f"✅ Tamanho após filtro: {len(painel_final):,} observações")
print(f"✅ Microrregiões após filtro: {n_micros_depois}")
print(f"✅ Microrregiões removidas: {n_micros_antes - n_micros_depois} ({(n_micros_antes - n_micros_depois)/n_micros_antes*100:.1f}%)")
print(f"✅ Microrregiões tratadas após filtro: {painel_final[painel_final['tratado'] == 1]['id_microrregiao'].nunique()}")
print(f"✅ Microrregiões controle após filtro: {painel_final[painel_final['tratado'] == 0]['id_microrregiao'].nunique()}")

# Estatísticas do painel final
print("\n" + "="*80)
print("ESTATÍSTICAS DO PAINEL FINAL")
print("="*80)
print(f"📊 Total de observações: {len(painel_final):,}")
print(f"📊 Área total média: {painel_final['area_total_km2'].mean():,.1f} km²")
print("\n📊 Áreas plantadas por produto:")
for col in colunas_area_plantada:
    n_obs = (painel_final[col] > 0).sum()
    pct = n_obs / len(painel_final) * 100
    n_micros = painel_final[painel_final[col] > 0]['id_microrregiao'].nunique()
    media = painel_final[painel_final[col] > 0][col].mean() if n_obs > 0 else 0
    print(f"   - {col}: {n_obs} obs ({pct:.1f}%), {n_micros} microrregiões, média = {media:,.1f} km²")
print("\n📊 Valor de produção por produto:")
for col in colunas_valor_producao:
    n_obs = (painel_final[col] > 0).sum()
    pct = n_obs / len(painel_final) * 100
    n_micros = painel_final[painel_final[col] > 0]['id_microrregiao'].nunique()
    media = painel_final[painel_final[col] > 0][col].mean() if n_obs > 0 else 0
    print(f"   - {col}: {n_obs} obs ({pct:.1f}%), {n_micros} microrregiões, média = R$ {media:,.0f}")


# =============================================================================
# 9. EXPORTAR DATASET FINAL
# =============================================================================

print("\n" + "="*80)
print("9. EXPORTAR DATASET FINAL")
print("="*80)

# Ordenar colunas (apenas variáveis do novo pipeline)
# Primeiro as colunas fixas
cols_order = [
    'ano', 'id_microrregiao', 'sigla_uf',
    'primeiro_ano_tratamento', 'tratado', 'pos_tratamento'
]

# Adicionar todas as colunas area_plantada_<produto> dinamicamente
colunas_area_plantada = sorted([col for col in painel_final.columns if col.startswith('area_plantada_')])
cols_order.extend(colunas_area_plantada)

# Adicionar todas as colunas valor_producao_<produto> dinamicamente
colunas_valor_producao = sorted([col for col in painel_final.columns if col.startswith('valor_producao_')])
cols_order.extend(colunas_valor_producao)

# Adicionar restante das colunas
cols_order.extend([
    'area_total_km2',
    'populacao_total', 'pib_total', 'pib_per_capita', 'pib_agropecuario',
    'precip_total_anual_mm', 'precip_media_mensal_mm', 'precip_max_mensal_mm'
])

# Selecionar colunas existentes
cols_final = [c for c in cols_order if c in painel_final.columns]
df_final = painel_final[cols_final].sort_values(['id_microrregiao', 'ano'])

# Criar nome do arquivo com produtos separados por hífen
# Remover sufixos "(em grão)" e "(em casca)" dos nomes para o arquivo
produtos_filename_list = []
for p in PRODUTOS_AGRICOLAS:
    # Remove "(em grão)" e "(em casca)" para manter compatibilidade com nomes antigos
    nome_limpo = p.replace(' (em grão)', '').replace(' (em casca)', '')
    produtos_filename_list.append(nome_limpo)
produtos_filename = '-'.join(produtos_filename_list)
output_file = f'data/microrregions_{produtos_filename}_{min(ANOS)}-{max(ANOS)}_mapbiomas.csv'

# Criar diretório se não existir
os.makedirs('data', exist_ok=True)

# Salvar com nome descritivo
df_final.to_csv(output_file, index=False)

print(f"✅ Dataset exportado: {output_file}")
print(f"📏 Tamanho: {os.path.getsize(output_file) / 1024 / 1024:.1f} MB")
print(f"📊 Dimensões: {len(df_final):,} observações × {len(df_final.columns)} variáveis")


# =============================================================================
# 10. GERAR DICIONÁRIO DE VARIÁVEIS (DATA DICTIONARY)
# =============================================================================

print("\n" + "="*80)
print("10. GERAR DICIONÁRIO DE VARIÁVEIS")
print("="*80)

def infer_variable_type(series):
    """
    Infer a coarse data type from a pandas Series.
    Returns: 'integer', 'float', 'boolean', 'category', or 'string'
    """
    dtype = series.dtype
    
    if pd.api.types.is_integer_dtype(dtype):
        return 'integer'
    elif pd.api.types.is_float_dtype(dtype):
        return 'float'
    elif pd.api.types.is_bool_dtype(dtype):
        return 'boolean'
    elif pd.api.types.is_categorical_dtype(dtype):
        return 'category'
    elif pd.api.types.is_object_dtype(dtype):
        # Could be string or mixed
        return 'string'
    else:
        return 'string'


def compute_stats(series, var_type):
    """
    Compute univariate statistics for a variable based on its type.
    """
    stats = {
        'n': int(series.notna().sum()),
        'n_missing': int(series.isna().sum())
    }
    
    if var_type in ['integer', 'float']:
        # Numeric statistics
        if stats['n'] > 0:
            stats['mean'] = float(series.mean()) if pd.notna(series.mean()) else None
            stats['std'] = float(series.std()) if pd.notna(series.std()) else None
            stats['min'] = float(series.min()) if pd.notna(series.min()) else None
            stats['max'] = float(series.max()) if pd.notna(series.max()) else None
            stats['median'] = float(series.median()) if pd.notna(series.median()) else None
            stats['q25'] = float(series.quantile(0.25)) if pd.notna(series.quantile(0.25)) else None
            stats['q75'] = float(series.quantile(0.75)) if pd.notna(series.quantile(0.75)) else None
        else:
            stats['mean'] = None
            stats['std'] = None
            stats['min'] = None
            stats['max'] = None
            stats['median'] = None
            stats['q25'] = None
            stats['q75'] = None
    else:
        # Non-numeric statistics
        stats['n_unique'] = int(series.nunique())
        
        # Top categories (most frequent values)
        if stats['n'] > 0:
            value_counts = series.value_counts().head(10)
            stats['top_values'] = [
                {'value': str(val), 'count': int(count)}
                for val, count in value_counts.items()
            ]
        else:
            stats['top_values'] = []
    
    return stats


# Metadata mapping for known variables
# This provides human-readable labels, sources, and units
VARIABLE_METADATA = {
    # Identifiers
    'ano': {
        'label': 'Calendar year',
        'source': 'constructed',
        'units': 'year',
        'notes': 'Panel time dimension'
    },
    'id_microrregiao': {
        'label': 'IBGE microrregião code',
        'source': 'IBGE Diretórios Brasil',
        'units': None,
        'notes': 'Panel cross-sectional unit identifier'
    },
    'sigla_uf': {
        'label': 'State abbreviation (UF)',
        'source': 'IBGE Diretórios Brasil',
        'units': None,
        'notes': 'Two-letter state code'
    },
    
    # Treatment variables
    'primeiro_ano_tratamento': {
        'label': 'Year of first INMET station installation',
        'source': 'INMET',
        'units': 'year',
        'notes': 'Zero if no station installed; minimum foundation year across all stations in the microrregião'
    },
    'tratado': {
        'label': 'Treatment group indicator',
        'source': 'constructed',
        'units': None,
        'notes': '1 if microrregião ever receives a meteorological station, 0 otherwise'
    },
    'pos_tratamento': {
        'label': 'Post-treatment indicator',
        'source': 'constructed',
        'units': None,
        'notes': '1 if year >= first treatment year and microrregião is treated, 0 otherwise'
    },
    
    # Area variables (MapBiomas)
    'area_total_km2': {
        'label': 'Total land area',
        'source': 'MapBiomas cobertura_municipio_classe',
        'units': 'km²',
        'notes': 'Total area across all land cover classes'
    },
    
    # Population and economic variables
    'populacao_total': {
        'label': 'Total population',
        'source': 'IBGE População',
        'units': 'persons',
        'notes': 'Aggregated municipal population'
    },
    'pib_total': {
        'label': 'Total GDP',
        'source': 'IBGE PIB',
        'units': 'BRL (nominal, 1000s)',
        'notes': 'Gross domestic product at current prices'
    },
    'pib_per_capita': {
        'label': 'GDP per capita',
        'source': 'constructed',
        'units': 'BRL per person (nominal)',
        'notes': 'Computed as pib_total / populacao_total'
    },
    'pib_agropecuario': {
        'label': 'Agricultural GDP',
        'source': 'IBGE PIB',
        'units': 'BRL (nominal, 1000s)',
        'notes': 'Value added from agricultural and livestock activities'
    },
    
    # Precipitation variables
    'precip_total_anual_mm': {
        'label': 'Total annual precipitation',
        'source': 'CDS/ERA5 local CSV',
        'units': 'mm',
        'notes': 'Sum of municipal precipitation totals within microrregião'
    },
    'precip_media_mensal_mm': {
        'label': 'Mean monthly precipitation',
        'source': 'CDS/ERA5 local CSV',
        'units': 'mm',
        'notes': 'Average of municipal mean monthly precipitation'
    },
    'precip_max_mensal_mm': {
        'label': 'Maximum monthly precipitation',
        'source': 'CDS/ERA5 local CSV',
        'units': 'mm',
        'notes': 'Average of municipal maximum monthly precipitation'
    }
}

# Add metadata for dynamic area_plantada_<produto> columns
for produto in PRODUTOS_AGRICOLAS:
    nome_curto = MAPEAMENTO_NOME_CURTO[produto]
    col_name = f'area_plantada_{nome_curto}'
    id_classe = MAPEAMENTO_PAM_MAPBIOMAS[produto]

    VARIABLE_METADATA[col_name] = {
        'label': f'Planted area of {produto}',
        'source': 'MapBiomas cobertura_municipio_classe',
        'units': 'km²',
        'notes': f'MapBiomas class {id_classe}; aggregated from municipal to microrregião level'
    }

# Add metadata for dynamic valor_producao_<produto> columns
for produto in PRODUTOS_AGRICOLAS:
    nome_curto = MAPEAMENTO_NOME_CURTO[produto]
    col_name = f'valor_producao_{nome_curto}'

    VARIABLE_METADATA[col_name] = {
        'label': f'Production value of {produto}',
        'source': 'PAM/IBGE lavoura_temporaria',
        'units': 'BRL (nominal)',
        'notes': f'Sum of production value for {produto}; aggregated from municipal to microrregião level'
    }


# Build the data dictionary
data_dictionary = {}

print(f"🔍 Processando {len(df_final.columns)} variáveis...")

for col in df_final.columns:
    # Infer type
    var_type = infer_variable_type(df_final[col])
    
    # Compute statistics
    stats = compute_stats(df_final[col], var_type)
    
    # Get metadata (use mapping or generate generic metadata)
    if col in VARIABLE_METADATA:
        meta = VARIABLE_METADATA[col].copy()
        label = meta.pop('label')
        source = meta.pop('source')
        units = meta.pop('units')
        notes = meta.pop('notes', None)
    else:
        # Fallback for unmapped variables
        label = f"Variable: {col}"
        source = "unknown"
        units = None
        notes = "Metadata not explicitly defined"
    
    # Build entry
    entry = {
        'label': label,
        'type': var_type,
        'source': source,
        'stats': stats
    }
    
    if units is not None:
        entry['units'] = units
    
    if notes is not None:
        entry['notes'] = notes
    
    data_dictionary[col] = entry

# Generate dictionary filename matching the CSV
dict_file = output_file.replace('.csv', '_dict.json')

# Save to JSON
with open(dict_file, 'w', encoding='utf-8') as f:
    json.dump(data_dictionary, f, indent=2, ensure_ascii=False)

print(f"✅ Dicionário de variáveis exportado: {dict_file}")
print(f"📏 Tamanho: {os.path.getsize(dict_file) / 1024:.1f} KB")
print(f"📊 Variáveis documentadas: {len(data_dictionary)}")

# Print summary by source
print("\n📋 Distribuição das variáveis por fonte:")
sources = {}
for var, meta in data_dictionary.items():
    src = meta['source']
    sources[src] = sources.get(src, 0) + 1

for src, count in sorted(sources.items(), key=lambda x: x[1], reverse=True):
    print(f"   - {src}: {count} variáveis")

# Estatísticas finais
print("\n📊 Resumo do dataset final:")
print(f"  - Microrregiões: {df_final['id_microrregiao'].nunique()}")
print(f"  - Estados: {df_final['sigla_uf'].nunique()}")
print(f"  - Anos: {ANOS[0]}-{ANOS[-1]}")
print(f"  - Microrregiões tratadas: {df_final[df_final['tratado'] == 1]['id_microrregiao'].nunique()}")
print(f"  - Microrregiões controle: {df_final[df_final['tratado'] == 0]['id_microrregiao'].nunique()}")
print("\n📊 Cobertura de dados:")
print(f"  - Área total média: {df_final['area_total_km2'].mean():,.1f} km²")
print("\n📊 Áreas plantadas por produto (dataset final):")
colunas_area_final = [col for col in df_final.columns if col.startswith('area_plantada_')]
for col in colunas_area_final:
    n_obs = (df_final[col] > 0).sum()
    pct = n_obs / len(df_final) * 100
    media = df_final[df_final[col] > 0][col].mean() if n_obs > 0 else 0
    print(f"  - {col}: {n_obs} obs ({pct:.1f}%), média = {media:,.1f} km²")
print("\n📊 Valor de produção por produto (dataset final):")
colunas_valor_final = [col for col in df_final.columns if col.startswith('valor_producao_')]
for col in colunas_valor_final:
    n_obs = (df_final[col] > 0).sum()
    pct = n_obs / len(df_final) * 100
    media = df_final[df_final[col] > 0][col].mean() if n_obs > 0 else 0
    print(f"  - {col}: {n_obs} obs ({pct:.1f}%), média = R$ {media:,.0f}")
print("\n📊 Missing values:")
print(f"  - População: {df_final['populacao_total'].isnull().sum()} ({df_final['populacao_total'].isnull().sum()/len(df_final)*100:.1f}%)")
print(f"  - PIB: {df_final['pib_total'].isnull().sum()} ({df_final['pib_total'].isnull().sum()/len(df_final)*100:.1f}%)")
print(f"  - Precipitação total anual: {df_final['precip_total_anual_mm'].isnull().sum()} ({df_final['precip_total_anual_mm'].isnull().sum()/len(df_final)*100:.1f}%)")
print("\n📊 Estatísticas de precipitação:")
if df_final['precip_total_anual_mm'].notna().sum() > 0:
    print(f"  - Precipitação total anual média: {df_final['precip_total_anual_mm'].mean():.1f} mm")
    print(f"  - Precipitação média mensal média: {df_final['precip_media_mensal_mm'].mean():.1f} mm")
    print(f"  - Precipitação máxima mensal média: {df_final['precip_max_mensal_mm'].mean():.1f} mm")

# Salvar mapeamento município-microrregião
df_municipios.to_csv('output/mapeamento_municipio_microrregiao.csv', index=False)
print("\n✅ Mapeamento salvo: output/mapeamento_municipio_microrregiao.csv")

# Mostrar amostra final
print("\n📋 Amostra do dataset final:")
print(df_final.head(10))

print("\n" + "="*80)
print("✨ PROCESSAMENTO CONCLUÍDO COM SUCESSO!")
print("📦 Dados: MapBiomas (área) + PAM (valor) + IBGE (PIB/pop) + INMET (tratamento) + CDS/ERA5 (precipitação)")
print(f"📖 Dicionário de variáveis salvo em: {dict_file}")
print("="*80)