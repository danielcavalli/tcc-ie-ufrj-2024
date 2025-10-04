# Impacto de Estações Meteorológicas na Produtividade Agrícola: Uma Aplicação de Diferenças em Diferenças com Tratamento Escalonado

> Trabalho de Conclusão de Curso – Instituto de Economia, UFRJ (2025)

Este repositório contém os códigos, dados e documentação do meu TCC no Instituto de Economia da UFRJ. O estudo investiga o impacto causal da instalação de estações meteorológicas automáticas sobre o PIB agropecuário no Brasil, utilizando o estimador de Diferenças em Diferenças (DiD) com adoção escalonada de Callaway e Sant'Anna (2021).

## 📋 Sumário

1. [Resumo da Pesquisa](#resumo-da-pesquisa)
2. [Resultados Principais](#resultados-principais)
3. [Estrutura do Repositório](#estrutura-do-repositório)
4. [Reprodução dos Resultados](#reprodução-dos-resultados)
5. [Metodologia](#metodologia)
6. [Dados](#dados)
7. [Citação](#citação)

## 📊 Resumo da Pesquisa

**Pergunta:** A instalação de estações meteorológicas automáticas aumenta a produtividade agrícola local?

**Hipótese:** O acesso a informações meteorológicas precisas e localizadas melhora a tomada de decisão no setor agrícola, resultando em ganhos de produtividade.

**Metodologia:** Diferenças em Diferenças com adoção escalonada, adequado para contextos onde todas as unidades são eventualmente tratadas.

**Período:** 2003-2023 (21 anos)

**Unidades:** 490 microrregiões produtoras de cana-de-açúcar

## 🎯 Resultados Principais

### Efeito Principal
- **ATT Agregado:** +8,2% no PIB agropecuário (IC 95%: [1,9%; 14,5%], p = 0,0103)
- **Interpretação:** Cada estação gera, em média, R$ 8,20 adicionais para cada R$ 100 de PIB agropecuário

### Validação e Robustez
- ✅ **Tendências Paralelas:** Confirmadas (múltiplos testes, p > 0,10)
- ✅ **Placebo PIB Não-Agrícola:** Efeito nulo, confirmando especificidade
- ✅ **Placebo Monte Carlo:** 5.000 simulações confirmam significância (p < 0,01)
- ✅ **Especificações Alternativas:** Resultados consistentes (DR, IPW, REG)

### Dinâmica Temporal
- **Event Study:** Efeitos crescentes ao longo do tempo
- **Lag:** Impactos significativos após 2-3 anos
- **Persistência:** Benefícios mantidos no longo prazo

## Metodologia

Empregamos o modelo de Diferenças-em-Diferenças (DiD) com adoção escalonada de Callaway & Sant'Anna (2021). Este framework é especialmente adequado para nosso contexto onde:
- Múltiplas microrregiões recebem estações em diferentes anos (2003-2023)
- Todas as unidades são eventualmente tratadas (não há controles "puros")
- Os efeitos do tratamento podem ser heterogêneos entre grupos e ao longo do tempo

### Principais características:

* **Especificação do modelo**: Estimamos os Efeitos Médios de Tratamento por Grupo e Tempo, ATT(g,t), que medem o efeito médio para unidades tratadas no ano g, observadas no tempo t.
* **Grupo de controle**: Utilizamos exclusivamente unidades "not yet treated" (ainda não tratadas), adequado quando todas as unidades são eventualmente tratadas.
* **Estimador**: Doubly Robust (DR), que combina:
  - Regressão do outcome condicional às covariáveis
  - Ponderação por propensity score (IPW)
  - Robusto a má especificação de um dos dois modelos
* **Inferência**: Bootstrap com clustering por microrregião para capturar dependência serial.
* **Agregação**: 
  - Overall ATT: efeito médio ponderado entre todos os grupos
  - Event-study: trajetória dinâmica dos efeitos ao longo do tempo

### Validação das hipóteses:

* **Tendências paralelas**: Múltiplos testes confirmam tendências paralelas pré-tratamento (p > 0.10)
* **Sem antecipação**: Assumimos que unidades não antecipam o tratamento futuro
* **SUTVA**: Controlamos spillovers parcialmente via densidade estadual de estações

## Dados e variáveis

### Variáveis principais:

| Tipo | Variável | Descrição |
|------|----------|-----------|
| **Outcome principal** | `log_pib_agro` | Log do PIB agropecuário municipal (R$ constantes) |
| **Outcome placebo** | `log_pib_nao_agro` | Log do PIB não-agropecuário (teste de especificidade) |
| **Tratamento** | `gname` | Ano da primeira estação meteorológica na microrregião |
| **Identificação** | `id_microrregiao` | Código único da microrregião |
| **Tempo** | `ano` | Ano da observação (2003-2023) |

### Covariáveis:

| Variável | Descrição |
|----------|-----------|
| `log_area_plantada` | Log da área plantada total (hectares) |
| `log_populacao` | Log da população municipal |
| `log_pib_per_capita` | Log do PIB per capita |
| `log_densidade_estacoes_uf` | Log da densidade de estações na UF (spillovers) |

### Estrutura dos dados:
- **Unidade de análise**: Microrregião
- **Período**: 2003-2023 (21 anos)
- **Painel**: Balanceado com 490 microrregiões × 21 anos = 10.290 observações
- **Grupo de controle**: "Not yet treated" (dinâmico ao longo do tempo)

## Principais resultados

### Efeito principal:
* **ATT agregado**: Aumento de **8,3%** no PIB agropecuário (p < 0,001)
* **Intervalo de confiança 95%**: [4,8%; 11,8%]
* **Interpretação**: Cada estação meteorológica gera, em média, R$ 8,30 adicionais para cada R$ 100 de PIB agropecuário

### Validação e robustez:
* **Tendências paralelas**: Confirmadas por múltiplos testes (p > 0,50)
* **Placebo PIB não-agrícola**: Efeito nulo (p > 0,10), confirmando especificidade
* **Placebo temporal**: ATT não significativo para anos fictícios
* **Robustez**: Resultados consistentes entre diferentes especificações (DR, IPW, Reg)

### Dinâmica temporal:
* **Efeitos crescentes**: Impacto se intensifica ao longo do tempo
* **Lag inicial**: Efeitos tornam-se significativos após 2-3 anos
* **Persistência**: Benefícios mantidos no longo prazo (10+ anos)

### Heterogeneidade:
* **Por coorte**: Early adopters (2003-2007) apresentam maiores ganhos
* **Regional**: Variação substancial entre regiões (análise em desenvolvimento)
* **Mecanismos**: Evidências sugerem melhor alocação de insumos e timing de plantio

## Detalhes de implementação do código

### Stack tecnológico:
- **R 4.5+**: Análise estatística principal
- **Pacote `did`**: Implementação do estimador Callaway & Sant'Anna
- **tidyverse**: Manipulação de dados e visualizações
- **gt/kableExtra**: Tabelas profissionais para apresentação
- **Python 3.9+**: ETL e preparação de dados via BigQuery

### Pipeline de análise (`rscripts/did_v2.r`):

1. **Preparação de dados** (`prep_data()`):
   - Transformações logarítmicas
   - Construção da variável de tratamento (gname)
   - Criação de covariáveis e controles de spillover

2. **Estimação principal** (`estimate_att()`):
   - ATT(g,t) via método Doubly Robust
   - Agregações: overall, event-study, por grupo
   - Tratamento de singularidade com fallback automático

3. **Testes de robustez**:
   - **Placebo temporal**: Anos fictícios de tratamento
   - **Placebo de outcome**: PIB não-agropecuário
   - **Placebo aleatório**: Tratamento randomizado (50 simulações)
   - **Especificações alternativas**: DR vs IPW vs Reg

4. **Análises complementares**:
   - **Heterogeneidade regional**: Por UF e grandes regiões
   - **Tendências paralelas**: Visualização por coorte e gname
   - **Análise de pesos**: Contribuição de cada grupo ao ATT
   - **Event-study estendido**: -10 a +10 períodos

5. **Geração de apresentação** (`generate_presentation()`):
   - Dashboard HTML interativo
   - Tabelas formatadas (gt)
   - Visualizações publicáveis (300 DPI)
   - Documentação automática de resultados

## Funcionalidades principais do código

### Análises disponíveis:
1. **Estimação DiD principal**: ATT com método doubly robust
2. **Testes placebo**: 
   - Temporal (anos fictícios)
   - Outcome alternativo (PIB não-agrícola)
   - Aleatório (50 simulações)
3. **Análise de tendências paralelas**:
   - Por coorte de adoção
   - Por grupo de tratamento (gname)
   - Testes formais de interação
4. **Heterogeneidade regional**:
   - Análise por UF (agregada)
   - Análise por grandes regiões
5. **Visualizações avançadas**:
   - Event-study dinâmico
   - Mapas de calor regionais
   - Forest plots de robustez
   - Dashboard HTML interativo

## Estrutura do repositório
```
├── data/                 # Dados brutos e tratados
│   ├── csv/              # Arquivos CSV intermediários
│   ├── outputs/          # Resultados das análises
│   │   └── presentation/ # Dashboard e visualizações
│   ├── parquet/          # Arquivos Parquet
│   └── sql_query/        # Consultas SQL usadas no BigQuery
├── documents/            # Rascunhos e versão final do TCC
│   └── drafts/           # Versões LaTeX do documento
├── rscripts/             # Scripts em R (análises estatísticas)
│   └── did_v2.r          # Script principal do modelo
├── scripts/              # Scripts em Python (ETL, coleta de dados)
├── renv/ & renv.lock     # Ambiente R reproduzível (renv)
├── requirements.txt      # Dependências Python
└── Makefile              # Comandos automatizados
```

## Guia Detalhado dos Diretórios

### 📁 `rscripts/` - Scripts de Análise em R

Este diretório contém todos os scripts R utilizados para análise estatística e econométrica do projeto.

#### Arquivos principais:

| Arquivo | Descrição | Funcionalidades |
|---------|-----------|-----------------|
| **`did_v2.r`** | Script principal do modelo DiD | • Estimação do ATT via Callaway & Sant'Anna (2021)<br>• Testes de robustez e placebo<br>• Análise de tendências paralelas<br>• Geração de todas as visualizações<br>• Dashboard HTML automático |
| **`balance_adjustments.r`** | Análise de balanceamento | • Diagnóstico de covariáveis<br>• Ajustes de propensity score<br>• Verificação de overlap |
| **`did_complementary_visualizations.r`** | Visualizações adicionais | • Gráficos de tendências por grupo<br>• Mapas de heterogeneidade regional<br>• Análises de sensibilidade visual |
| **`did_complementary_visualizations_pt2.r`** | Visualizações avançadas | • Event-study estendido<br>• Análise de composição dinâmica<br>• Gráficos de qualidade dos dados |
| **`generate_latex_values.r`** | Integração com LaTeX | • Extração automatizada de valores<br>• Geração de tabelas formatadas<br>• Criação de `auto_values.tex` |

#### Como executar:
```r
# Análise completa
source("rscripts/did_v2.r")

# Apenas visualizações
source("rscripts/did_complementary_visualizations.r")
```

### 📁 `data_extraction/` - Extração e Preparação de Dados

Contém notebooks e scripts para extração de dados do BigQuery e preparação inicial.

#### Arquivos:

| Arquivo | Descrição | Outputs |
|---------|-----------|---------|
| **`analise_did_microrregions.ipynb`** | Notebook principal de ETL | • Extração de dados do BigQuery<br>• Mapeamento município → microrregião<br>• Agregação de variáveis por produto<br>• Geração do CSV final |

#### Funcionalidades do notebook:
- **Flexibilidade de produtos**: Permite análise por produto único ou agregado
- **Mapeamento geográfico**: Usa tabelas oficiais IBGE para agregação correta
- **Queries otimizadas**: SQL eficiente para grandes volumes de dados
- **Validação**: Checks de qualidade e consistência dos dados

#### Exemplo de uso:
```python
# Para mudar o produto analisado, edite:
PRODUTOS_AGRICOLAS = ['Cana-de-açúcar']  # Padrão
# Ou
PRODUTOS_AGRICOLAS = ['Soja', 'Milho']   # Agregado
```

### 📁 `documents/drafts/latex_output/` - Documentos LaTeX

Diretório com o texto do TCC em LaTeX e arquivos relacionados.

#### Estrutura:

| Arquivo/Tipo | Descrição |
|--------------|-----------|
| **`TCC_DanielCavalli_ABNT2.tex`** | Documento principal do TCC em formato ABNT |
| **`referencias.bib`** | Bibliografia em formato BibTeX |
| **`auto_values.tex`** | Valores extraídos automaticamente dos resultados |
| **`Makefile`** | Comandos para compilação do PDF |
| **`*.png`** | Figuras incorporadas no documento |
| **Auxiliares** | `.aux`, `.bbl`, `.log`, etc. (gerados na compilação) |

#### Compilação:
```bash
cd documents/drafts/latex_output/
make            # Compila o PDF
make clean      # Remove arquivos auxiliares
make preview    # Gera preview HTML
```

### 📁 `data/outputs/` - Resultados das Análises

Diretório com todos os outputs gerados pelos scripts de análise.

#### Organização:

##### 📂 Raiz (`data/outputs/`)
| Tipo de arquivo | Conteúdo | Exemplos |
|-----------------|----------|----------|
| **`.rds`** | Objetos R serializados | `att_results_*.rds` - Resultados do modelo<br>`agg_*.rds` - Agregações |
| **`.csv`** | Tabelas de resultados | `att_summary.csv` - ATT principal<br>`robust_att.csv` - Testes de robustez |
| **`.png`/`.pdf`** | Visualizações principais | `event_study.png` - Gráfico principal<br>`robustness_plot.png` - Forest plot |

##### 📂 `descriptive_analysis/`
Análises descritivas e exploratórias:
- **HTML**: Dashboards interativos com estatísticas
- **PNG**: Gráficos de distribuição e evolução temporal

##### 📂 `additional_figures/`
Análises complementares e sensibilidade:
- Diagnósticos de qualidade dos dados
- Análise de poder estatístico
- Testes de sensibilidade temporal
- Tendências por quartil de tamanho

##### 📂 `presentation/`
Material para apresentação final:
- **`dashboard_resultados.html`**: Dashboard interativo completo
- **Tabelas HTML**: Formatadas para apresentação
- **Gráficos aprimorados**: Versões em alta resolução

### 📁 `data/csv/` - Dados Processados

Contém o dataset principal utilizado nas análises.

#### Arquivo principal:

**`microrregions_Cana-de-açúcar_2003-2023.csv`**

##### Estrutura do dataset:

| Coluna | Tipo | Descrição |
|--------|------|-----------|
| `ano` | int | Ano da observação (2003-2023) |
| `id_microrregiao` | int | Código IBGE da microrregião |
| `sigla_uf` | str | Sigla do estado |
| `primeiro_ano_tratamento` | int | Ano da primeira estação (gname) |
| `tratado` | int | Indicador binário de tratamento |
| `pos_tratamento` | int | Indicador pós-tratamento |
| `area_plantada` | float | Área plantada em hectares |
| `producao` | float | Produção em toneladas |
| `valor_producao` | float | Valor da produção em R$ mil |
| `populacao_total` | int | População municipal |
| `pib_total` | float | PIB total em R$ |
| `pib_agropecuario` | float | PIB agropecuário em R$ |

##### Características:
- **Unidade**: Microrregião-ano
- **Período**: 2003-2023 (21 anos)
- **Observações**: 10.290 (490 microrregiões × 21 anos)
- **Formato**: CSV com encoding UTF-8

#### Uso nos scripts:
```r
# Em R
df <- read_csv("data/csv/microrregions_Cana-de-açúcar_2003-2023.csv")

# Em Python
df = pd.read_csv("data/csv/microrregions_Cana-de-açúcar_2003-2023.csv")
```

## Pré-requisitos

| Tecnologia | Versão recomendada |
|------------|--------------------|
| Python     | ≥ 3.9              |
| R          | ≥ 4.5              |
| Renv       | ≥ 1.0              |
| GNU Make   | Opcional, para facilitar comandos |

Além disso é necessário acesso à [Google Cloud BigQuery](https://cloud.google.com/bigquery) para executar as consultas SQL presentes em `data/sql_query/`.

## Instalação do ambiente
1. **Clone o repositório:**
   ```bash
   git clone https://github.com/<usuario>/tcc-ie-ufrj-2024.git
   cd tcc-ie-ufrj-2024
   ```
2. **Python:** crie um ambiente virtual e instale as dependências.
   ```bash
   python -m venv .venv
   source .venv/bin/activate
   pip install -r requirements.txt
   ```
3. **R:** restaure o ambiente renv.
   ```R
   # No console R
   renv::restore()
   ```

Opcionalmente, utilize o **Makefile** para automatizar passos comuns:
```bash
make setup            # Instala dependências Python e R
make reproduce-data    # Executa pipeline de extração e tratamento
make analysis          # Roda os scripts de análise estatística
```

## Reprodução dos resultados

### 1. Preparação do ambiente:
```bash
# Clone o repositório
git clone https://github.com/[usuario]/tcc-ie-ufrj-2024.git
cd tcc-ie-ufrj-2024

# Python: criar ambiente virtual
python -m venv .venv
source .venv/bin/activate  # No Windows: .venv\Scripts\activate
pip install -r requirements.txt

# R: restaurar pacotes
R -e "renv::restore()"
```

### 2. Obtenção dos dados:
Os dados já processados estão disponíveis em:
- `data/csv/microrregions_Cana-de-açúcar_2003-2023.csv` (arquivo principal)
- Alternativamente, execute as queries SQL em `data/sql_query/` no BigQuery

### 3. Execução da análise completa:
```bash
# Rodar o pipeline completo de análise DiD
Rscript rscripts/did_v2.r
```

Isso irá gerar automaticamente:
- Estimativas do modelo em `data/outputs/`
- Visualizações em `data/outputs/` (PNG, 300 DPI)
- Dashboard HTML em `data/outputs/presentation/`
- Tabelas de resultados em formatos CSV e HTML

### 4. Visualizações específicas:
```r
# Para gerar apenas visualizações de tendências paralelas
source("rscripts/did_v2.r")
plot_parallel_trends(df_clean, "log_pib_agro")
plot_parallel_trends_by_gname(df_clean, "log_pib_agro")
```

## Fontes de dados

### Dados principais:
* **PAM/IBGE**: Produção Agrícola Municipal
  - PIB agropecuário municipal
  - Área plantada, produção e produtividade por cultura
  - Período: 2003-2023

* **INMET**: Instituto Nacional de Meteorologia
  - Localização e data de instalação das estações automáticas
  - Dados meteorológicos (temperatura, precipitação)
  
* **IBGE - Contas Municipais**:
  - PIB total e setorial
  - População municipal
  - Indicadores socioeconômicos

### Acesso aos dados:
* Dados processados disponíveis no repositório
* Queries originais em `data/sql_query/` para replicação via BigQuery
* Base de dados pública: [basedosdados.org](https://basedosdados.org/)

## Licença

Este projeto é licenciado sob os termos da [GNU General Public License v3.0](LICENSE). Isso significa que você pode:
- ✅ Usar o código para qualquer propósito
- ✅ Modificar o código fonte
- ✅ Distribuir o código original ou modificado
- ✅ Usar o código em projetos comerciais

Com as seguintes condições:
- 📋 Deve manter a mesma licença GPL v3.0
- 📋 Deve disponibilizar o código fonte
- 📋 Deve documentar as modificações realizadas
- 📋 Deve incluir a licença e avisos de copyright

Para mais detalhes, consulte o arquivo [LICENSE](LICENSE) ou visite [GNU GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.html).

## Autor e citação

| Autor | Contato |
|-------|---------|
| Daniel Cavalli | <daniel.cavalli@ufrj.br> |

Se você utilizar este código ou dados em seu trabalho, por favor cite da seguinte forma:

> Cavalli, D. (2025). *Impacto de Estações Meteorológicas no PIB Agropecuário: Uma Aplicação de Diferenças em Diferenças com Tratamento Escalonado*. Trabalho de Conclusão de Curso (Graduação em Ciências Econômicas), Instituto de Economia, Universidade Federal do Rio de Janeiro.

---

**Observação:** Este repositório segue as recomendações de reprodutibilidade do [Turing Way](https://the-turing-way.netlify.app/).
