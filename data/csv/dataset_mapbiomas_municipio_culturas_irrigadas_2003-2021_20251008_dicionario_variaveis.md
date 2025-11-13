# Dicionário de Variáveis - Dataset MapBiomas com Culturas Irrigadas

**Dataset**: dataset_mapbiomas_municipio_culturas_irrigadas_2003-2021_20251008.csv  
**Período**: 2003-2021  
**Granularidade**: Municipal (com id_microrregiao para agregação flexível)  
**Culturas irrigadas**: Cana-de-açúcar, Arroz (em casca), Soja (em grão), Milho (em grão), Feijão (em grão)  
**Data de geração**: 08/10/2025

## Descrição das Variáveis

### 1. Variáveis de Identificação

- **ano**: Ano da observação (2003-2021)
- **id_municipio**: Código IBGE do município
- **id_microrregiao**: Código IBGE da microrregião (para agregação flexível)
- **sigla_uf**: Sigla da Unidade Federativa (estado)

### 2. Variáveis de Tratamento (Estações Meteorológicas)

- **primeiro_ano_tratamento**: Ano de instalação da primeira estação meteorológica automática no município. Valor 0 indica ausência de estação.
- **tratado**: Variável binária indicando se o município possui estação meteorológica (1 = sim, 0 = não)
- **pos_tratamento**: Variável binária indicando se a observação ocorre após a instalação da estação (1 = sim, 0 = não)

### 3. Variáveis Socioeconômicas

- **populacao_total**: População total do município (habitantes)
- **pib_total**: PIB total em reais correntes
- **pib_per_capita**: PIB per capita em reais/habitante
- **pib_agropecuario**: Valor adicionado bruto da agropecuária em reais

### 4. Variáveis de Urbanização

- **area_total_ha**: Área total do município em hectares
- **pct_area_agropecuaria**: Percentual da área do município dedicado à agropecuária (%)
- **pct_area_urbana**: Percentual da área do município considerado urbano (%)
- **razao_agropecuaria_urbana**: Razão entre área agropecuária e área urbana
- **municipio_urbano**: Classificação binária do município (1 = urbano, 0 = rural)
  - Município é considerado urbano se: % área urbana > 40% OU razão agropecuária/urbana < 1

### 5. Variáveis de Produção Agrícola (PAM) - Culturas Irrigadas

Para cada uma das 5 culturas com maior uso de irrigação no Brasil, são incluídas as seguintes variáveis:


#### Cana-de-açúcar
- **area_plantada_cana**: Área plantada em hectares
- **area_colhida_cana**: Área colhida em hectares
- **quantidade_cana**: Quantidade produzida em toneladas
- **produtividade_cana**: Produtividade média em kg/hectare (quantidade/área colhida)
- **valor_cana**: Valor da produção em mil reais

#### Arroz (em casca)
- **area_plantada_arroz**: Área plantada em hectares
- **area_colhida_arroz**: Área colhida em hectares
- **quantidade_arroz**: Quantidade produzida em toneladas
- **produtividade_arroz**: Produtividade média em kg/hectare (quantidade/área colhida)
- **valor_arroz**: Valor da produção em mil reais

#### Soja (em grão)
- **area_plantada_soja**: Área plantada em hectares
- **area_colhida_soja**: Área colhida em hectares
- **quantidade_soja**: Quantidade produzida em toneladas
- **produtividade_soja**: Produtividade média em kg/hectare (quantidade/área colhida)
- **valor_soja**: Valor da produção em mil reais

#### Milho (em grão)
- **area_plantada_milho**: Área plantada em hectares
- **area_colhida_milho**: Área colhida em hectares
- **quantidade_milho**: Quantidade produzida em toneladas
- **produtividade_milho**: Produtividade média em kg/hectare (quantidade/área colhida)
- **valor_milho**: Valor da produção em mil reais

#### Feijão (em grão)
- **area_plantada_feijao**: Área plantada em hectares
- **area_colhida_feijao**: Área colhida em hectares
- **quantidade_feijao**: Quantidade produzida em toneladas
- **produtividade_feijao**: Produtividade média em kg/hectare (quantidade/área colhida)
- **valor_feijao**: Valor da produção em mil reais

### 6. Variáveis de Uso da Terra (MapBiomas - Dados de Satélite)

- **area_agropecuaria**: Área total dedicada à agropecuária (agricultura + pecuária) (hectares)
- **area_agricultura**: Área dedicada somente à agricultura (hectares)
- **area_lavoura_temporaria**: Área dedicada a lavouras temporárias (hectares)
- **area_soja**: Área específica de cultivo de soja (hectares)
- **area_cana**: Área específica de cultivo de cana-de-açúcar (hectares)
- **area_outras_lavouras_temporarias**: Área de outras lavouras temporárias excluindo soja e cana (hectares)
- **area_cafe**: Área específica de cultivo de café (hectares)
- **area_citricos**: Área específica de cultivo de cítricos (hectares)
- **area_area_urbana**: Área urbana do município (hectares)

## Notas Metodológicas

1. **Granularidade Municipal**: O dataset mantém o id_municipio como menor unidade de identificação, permitindo análises detalhadas em nível municipal. O id_microrregiao é incluído para permitir agregações flexíveis.

2. **Culturas Irrigadas**: As 5 culturas selecionadas representam as maiores áreas irrigadas no Brasil:
   - Cana-de-açúcar: 1,7 milhão de hectares irrigados
   - Arroz: 1,1 milhão de hectares irrigados
   - Soja: 624 mil hectares irrigados
   - Milho: 559 mil hectares irrigados
   - Feijão: 195 mil hectares irrigados

3. **MapBiomas vs PAM**: Os dados do MapBiomas são baseados em imagens de satélite e tendem a ser mais precisos que os dados autodeclarados da PAM. Note que pode haver discrepâncias entre áreas reportadas pelo MapBiomas e pelo PAM.

4. **Produtividade**: A produtividade é calculada como quantidade produzida dividida pela área colhida. Valores zero indicam ausência de produção ou área colhida.

5. **Valores faltantes**: 
   - Zeros indicam ausência real do fenômeno (ex: sem produção)
   - NAs foram removidos apenas para população e PIB

6. **Tratamento**: O tratamento refere-se à presença de estações meteorológicas automáticas do INMET, que fornecem dados climáticos mais precisos e frequentes.

7. **Classificação de Urbanização**: A classificação de municípios como urbanos ou rurais é baseada em dois critérios:
   - Área urbana superior a 40% da área total do município, OU
   - Razão entre área agropecuária e área urbana menor que 1 (área urbana > área agropecuária)
   - Esta classificação usa dados de uso da terra do MapBiomas, que são baseados em imagens de satélite

## Fontes dos Dados

- **IBGE**: População, PIB Municipal, Produção Agrícola Municipal (PAM)
- **INMET**: Estações meteorológicas
- **MapBiomas**: Cobertura e uso da terra
- **Base dos Dados**: Plataforma de acesso integrado aos dados

---
*Documento gerado automaticamente pelo notebook de extração de dados*
