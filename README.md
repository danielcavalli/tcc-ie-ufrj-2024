# Impacto de Estações Meteorológicas na Cultura de Cana-de-Açúcar: Uma Aplicação de Diferenças em Diferenças com Tratamento Escalonado

> Trabalho de Conclusão de Curso – Instituto de Economia, UFRJ (2025)

Este repositório contém os códigos, dados e documentação do TCC desenvolvido no Instituto de Economia da UFRJ. O estudo investiga o impacto causal da instalação de estações meteorológicas automáticas sobre a produção de cana-de-açúcar no Brasil, utilizando o estimador de Diferenças em Diferenças (DiD) com adoção escalonada de Callaway e Sant'Anna (2021).

## Características da Implementação

- **Metodologia**: Aplicação do estimador Callaway & Sant'Anna (2021) para DiD com adoção escalonada.
- **Pipeline de Análise**: Automação do fluxo de dados, estimação e geração de resultados via Makefile.
- **Integração R/LaTeX**: Exportação automática de estimativas e estatísticas para o documento final.
- **Reprodutibilidade**: Controle de ambiente com `renv` e documentação de dependências.
- **Validação**: Inclui testes de placebo (Monte Carlo), tendências paralelas e análises de sensibilidade.
- **Dados**: Scripts de extração e processamento para fontes públicas (IBGE, INMET).

## Sumário

1. [Resumo da Pesquisa](#resumo-da-pesquisa)
2. [Resultados Principais](#resultados-principais)
3. [Estrutura de Análise e Pipeline](#estrutura-de-análise-e-pipeline)
4. [Metodologia](#metodologia)
5. [Dados e Variáveis](#dados-e-variáveis)
6. [Estrutura do Repositório](#estrutura-do-repositório)
7. [Instalação do Ambiente](#instalação-do-ambiente)
8. [Reprodução Completa dos Resultados](#reprodução-completa-dos-resultados)
9. [Citação](#citação)

## Resumo da Pesquisa

**Pergunta:** A instalação de estações meteorológicas automáticas aumenta a produção de cana-de-açúcar local?

**Hipótese:** O acesso a informações meteorológicas precisas e localizadas melhora a tomada de decisão no setor agrícola (irrigação, calendário de plantio), resultando em expansão da área plantada e aumento do valor de produção de cana-de-açúcar.

**Metodologia:** Diferenças em Diferenças com adoção escalonada (Callaway & Sant'Anna, 2021), adequado para contextos onde todas as unidades são eventualmente tratadas.

**Período:** 2003-2021 (19 anos)

**Unidades:** 225 microrregiões produtoras de cana-de-açúcar

**Outcomes Analisados:**
- Área plantada de cana-de-açúcar (outcome principal)
- Valor de produção de cana-de-açúcar
- PIB agropecuário (medida agregada)
- Culturas alternativas: soja e arroz (testes de especificidade)

## Resultados Principais

Os resultados reportados abaixo são obtidos através do pipeline automatizado implementado em [rscripts/did_v2.r](rscripts/did_v2.r). Todos os valores numéricos são extraídos automaticamente e incorporados no documento LaTeX através do script [rscripts/generate_latex_values.r](rscripts/generate_latex_values.r).

### Efeito Principal (Cana-de-Açúcar)
- **Área Plantada:** ATT agregado significativo na expansão da área de cana-de-açúcar
- **Valor de Produção:** Aumento substancial no valor de produção da cultura
- **Interpretação:** As estações meteorológicas facilitam decisões sobre expansão de área e práticas de manejo

### Validação e Robustez
- **Tendências Paralelas:** Confirmadas através de múltiplos testes formais e inspeção visual por coorte
- **Placebo com Outras Culturas:** Soja e arroz apresentam efeitos nulos ou distintos, confirmando especificidade do impacto em cana
- **Placebo Monte Carlo:** Simulações confirmam que resultados não são artefatos estatísticos
- **Especificações Alternativas:** Consistência entre estimadores Doubly Robust (DR), IPW e Regressão

### Dinâmica Temporal
- **Event Study:** Trajetória dinâmica dos efeitos desde 10 anos antes até 10 anos após tratamento
- **Lag Inicial:** Efeitos tornam-se mais pronunciados após 2-3 anos (tempo para ajustes de plantio)
- **Persistência:** Benefícios se mantêm ou se ampliam no longo prazo

## Metodologia

Empregamos o modelo de Diferenças em Diferenças (DiD) com adoção escalonada de Callaway & Sant'Anna (2021). Este framework é especialmente adequado para nosso contexto onde:
- Múltiplas microrregiões recebem estações em diferentes anos (2003-2021)
- Todas as unidades são eventualmente tratadas (não há controles permanentes)
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

## Dados e Variáveis

### Variáveis Principais:

| Tipo | Variável | Descrição |
|------|----------|-----------|
| **Outcome principal** | `log_area_cana` | Log da área plantada de cana-de-açúcar (hectares) |
| **Outcomes secundários** | `log_valor_producao_cana` | Log do valor de produção de cana (R$ mil, valores constantes) |
|  | `log_pib_agro` | Log do PIB agropecuário (R$ constantes) |
| **Outcomes placebo** | `log_area_soja`, `log_area_arroz` | Log da área plantada de soja e arroz (testes de especificidade) |
| **Tratamento** | `gname` | Ano da primeira estação meteorológica na microrregião |
| **Identificação** | `id_microrregiao` | Código IBGE da microrregião |
| **Tempo** | `ano` | Ano da observação (2003-2021) |

### Covariáveis:

| Variável | Descrição |
|----------|-----------|
| `log_area_plantada_total` | Log da área plantada agregada (hectares) |
| `log_populacao` | Log da população municipal |
| `log_pib_per_capita` | Log do PIB per capita |
| `log_densidade_estacoes_uf` | Log da densidade de estações na UF (controle de spillovers) |

### Estrutura dos Dados:
- **Unidade de análise**: Microrregião (agregação de municípios segundo IBGE)
- **Período**: 2003-2021 (19 anos)
- **Painel**: Balanceado com 225 microrregiões produtoras de cana × 19 anos = 9.310 observações
- **Grupo de controle**: "Not yet treated" (dinâmico, muda ao longo do tempo)
- **Filtro de amostra**: Microrregiões com produção de cana-de-açúcar em pelo menos um ano

## Estrutura de Análise e Pipeline

### Pipeline Completo:

O projeto utiliza um pipeline automatizado para sincronizar a análise estatística com o documento final:

1. **Análise Econométrica** ([rscripts/did_v2.r](rscripts/did_v2.r)):
   - Estimação dos efeitos via Callaway & Sant'Anna (2021)
   - Testes de robustez e validação
   - Geração de todas as figuras e tabelas
   - Exportação de resultados em formatos `.rds` e `.csv`

2. **Extração de Valores** ([rscripts/generate_latex_values.r](rscripts/generate_latex_values.r)):
   - Leitura automática dos outputs gerados por `did_v2.r`
   - Extração de valores numéricos (ATTs, p-valores, ICs)
   - Geração de macros LaTeX em `data/outputs/latex_values.tex`
   - Criação de pequenas tabelas formatadas para inclusão no documento

3. **Compilação do Documento** (via Makefile):
   - Inclusão automática de `latex_values.tex` no documento principal
   - Compilação com pdflatex + bibtex
   - Geração do PDF final da tese

### Comandos Principais via Makefile:

```bash
make analysis          # Executa análise DiD completa e atualiza valores LaTeX
make analysis NSIMS=50 # Análise rápida com 50 simulações Monte Carlo (vs 5000)
make latex-values      # Apenas atualiza valores LaTeX a partir dos outputs existentes
make thesis            # Compila o documento LaTeX (TCC)
make docs              # Compila tese e apresentação
```

## Detalhes de Implementação

### Stack Tecnológico:
- **R 4.5+**: Análise estatística principal
- **Pacote `did`**: Implementação do estimador Callaway & Sant'Anna (2021)
- **tidyverse**: Manipulação de dados e visualizações (dplyr, ggplot2, readr, tidyr)
- **gt/kableExtra**: Tabelas formatadas para LaTeX e HTML
- **Python 3.9+**: ETL e preparação de dados via BigQuery
- **LaTeX (abntex2)**: Documento final em conformidade com normas ABNT
- **GNU Make**: Automação do pipeline completo

### Arquitetura do Código:

#### Script Principal: [rscripts/did_v2.r](rscripts/did_v2.r)

1. **Preparação de Dados**:
   - Filtro de microrregiões produtoras de cana (via `filter_cana_producers()`)
   - Transformações logarítmicas e criação de variáveis
   - Construção da variável de tratamento (`gname`)
   - Criação de covariáveis e controles de spillover

2. **Estimação Principal**:
   - ATT(g,t) via método Doubly Robust (combina regressão + IPW)
   - Agregações: overall ATT, event-study, por grupo/coorte
   - Fallback automático para estimadores alternativos em caso de singularidade

3. **Testes de Robustez**:
   - Placebo com culturas alternativas (soja, arroz)
   - Placebo aleatório com tratamento randomizado (Monte Carlo: 5.000 simulações)
   - Especificações alternativas (DR, IPW, Regressão)
   - Análise de sensibilidade temporal

4. **Análises Complementares**:
   - Heterogeneidade regional (por UF e grandes regiões)
   - Visualização de tendências paralelas (por coorte e gname)
   - Análise de pesos e composição do ATT
   - Event-study estendido (10 períodos antes/depois)

5. **Outputs Gerados**:
   - Objetos R serializados (`.rds`) com resultados completos
   - Tabelas em CSV para análise externa
   - Visualizações em alta resolução (PNG 300 DPI, PDF vetorial)
   - Dashboard HTML interativo

#### Script de Extração: [rscripts/generate_latex_values.r](rscripts/generate_latex_values.r)

Este script implementa a integração automática entre R e LaTeX:

1. **Leitura de Outputs**:
   - Carrega resultados `.rds` gerados por `did_v2.r`
   - Extrai ATTs agregados, ICs, p-valores
   - Processa tabelas de robustez e heterogeneidade

2. **Geração de Macros LaTeX**:
   - Cria comandos `\newcommand` para cada valor numérico
   - Formata números com precisão adequada (casas decimais, porcentagens)
   - Gera tabelas pequenas em formato LaTeX

3. **Sincronização**:
   - Garante que todos os valores no documento LaTeX são atualizados automaticamente
   - Elimina necessidade de copiar/colar valores manualmente
   - Reduz erros de transcrição

4. **Output**:
   - Arquivo `data/outputs/latex_values.tex` com todas as macros
   - Incluído automaticamente no documento principal via `\input{}`

## Estrutura do Repositório

```
.
├── data/
│   ├── csv/                        # Dados processados em formato CSV
│   │   └── microrregions_Cana-de-açúcar-Soja-Arroz_2003-2021_mapbiomas.csv
│   ├── outputs/                    # Resultados das análises
│   │   ├── *.rds                   # Objetos R serializados (ATT results)
│   │   ├── *.csv                   # Tabelas de resultados
│   │   ├── *.png, *.pdf            # Visualizações
│   │   ├── latex_values.tex        # Macros LaTeX auto-gerados
│   │   ├── descriptive_analysis/   # Análises descritivas
│   │   ├── additional_figures/     # Figuras complementares
│   │   └── presentation/           # Dashboard HTML e material para apresentação
│   └── sql_query/                  # Consultas SQL do BigQuery
│
├── documents/
│   └── drafts/
│       └── latex_output/
│           ├── TCC_DanielCavalli_ABNT2.tex    # Documento principal da tese
│           ├── TCC_DanielCavalli_ABNT2.pdf    # PDF compilado
│           └── referencias.bib                 # Bibliografia
│
├── rscripts/                       # Scripts de análise em R
│   ├── did_v2.r                   # Pipeline principal de análise DiD
│   ├── generate_latex_values.r    # Extração de valores para LaTeX
│   ├── data_prep_crop_filters.R   # Funções de filtro por cultura
│   └── *.r                        # Scripts complementares
│
├── data_extraction/                # Notebooks de extração de dados
│   └── analise_did_microrregions.py
│
├── Makefile                        # Automação do pipeline completo
├── renv.lock                       # Lock de dependências R (renv)
├── requirements.txt                # Dependências Python
└── README.md                       # Este arquivo
```

## Guia Detalhado dos Diretórios

### `rscripts/` - Scripts de Análise em R

Este diretório contém todos os scripts R utilizados para análise estatística e econométrica do projeto.

#### Arquivos principais:

| Arquivo | Descrição | Funcionalidades |
|---------|-----------|-----------------|
| **`did_v2.r`** | Script principal do modelo DiD | • Estimação do ATT via Callaway & Sant'Anna (2021)<br>• Testes de robustez e placebo<br>• Análise de tendências paralelas<br>• Geração de todas as visualizações<br>• Suporte a múltiplos outcomes (cana, soja, arroz, PIB) |
| **`generate_latex_values.r`** | Integração com LaTeX | • Extração automatizada de valores dos outputs<br>• Geração de macros LaTeX (`\newcommand`)<br>• Criação de `data/outputs/latex_values.tex` |
| **`data_prep_crop_filters.R`** | Funções de filtro | • `filter_cana_producers()`: filtra microrregiões produtoras de cana<br>• Funções análogas para soja e arroz<br>• Reutilizável em todo o pipeline |
| **`balance_adjustments.r`** | Análise de balanceamento | • Diagnóstico de covariáveis<br>• Verificação de overlap no propensity score |
| **`did_complementary_visualizations.r`** | Visualizações complementares | • Gráficos de tendências por grupo<br>• Mapas de heterogeneidade regional |
| **`did_complementary_visualizations_pt2.r`** | Visualizações avançadas | • Event-study estendido<br>• Análise de composição dinâmica |

#### Como executar:

**Recomendado: Use o Makefile**
```bash
make analysis          # Executa análise completa (5000 simulações MC)
make analysis NSIMS=50 # Teste rápido (50 simulações)
```

**Direto via Rscript:**
```bash
Rscript rscripts/did_v2.r           # Análise completa
Rscript rscripts/did_v2.r --nsims 50  # Com 50 simulações
```

**No console R:**
```r
source("rscripts/did_v2.r")  # Análise completa
```

### `data_extraction/` - Extração e Preparação de Dados

Scripts Python para extração e preparação dos dados brutos via BigQuery.

#### Arquivo principal:

**`analise_did_microrregions.py`**
- Extração de dados do BigQuery (PAM/IBGE, INMET, Contas Municipais)
- Mapeamento de municípios para microrregiões (classificação IBGE)
- Agregação de variáveis por cultura (cana, soja, arroz)
- Construção da variável de tratamento (ano da primeira estação)
- Geração do dataset final em CSV

**Fontes de dados:**
- Base dos Dados (basedosdados.org): PIB municipal, área plantada, produção agrícola
- INMET: localização e data de instalação das estações meteorológicas
- IBGE: mapeamento geográfico município-microrregião

### `documents/drafts/latex_output/` - Documento Final da Tese

Contém o documento LaTeX da tese em conformidade com normas ABNT.

#### Arquivos principais:

| Arquivo | Descrição |
|---------|-----------|
| **`TCC_DanielCavalli_ABNT2.tex`** | Documento principal (classe abntex2) |
| **`TCC_DanielCavalli_ABNT2.pdf`** | PDF compilado da tese |
| **`referencias.bib`** | Bibliografia em formato BibTeX |

**Integração automática com R:**
```latex
% No preâmbulo do documento:
\input{../../../data/outputs/latex_values.tex}

% Uso no texto:
O ATT agregado foi de \attCanaArea\% ...
```

#### Compilação:

**Via Makefile (na raiz do projeto):**
```bash
make thesis     # Compila apenas a tese
make docs       # Compila tese + apresentação
```

**Manualmente:**
```bash
cd documents/drafts/latex_output/
pdflatex TCC_DanielCavalli_ABNT2.tex
bibtex TCC_DanielCavalli_ABNT2
pdflatex TCC_DanielCavalli_ABNT2.tex
pdflatex TCC_DanielCavalli_ABNT2.tex
```

### `data/outputs/` - Resultados das Análises

Todos os outputs gerados automaticamente pelo pipeline de análise.

#### Organização:

#####  Raiz (`data/outputs/`)

**Objetos R (`.rds`):**
- `att_results_dr_area_cana.rds`: Resultado completo da estimação (área de cana)
- `att_results_dr_valor_producao_cana.rds`: Resultado para valor de produção
- `agg_event_dr_*.rds`, `agg_overall_dr_*.rds`: Agregações event-study e overall

**Tabelas (`.csv`):**
- `att_summary_main_cana.csv`: Resumo dos ATTs principais
- `robust_att_cana.csv`: Testes de robustez
- `att_summary.csv`: Resultados agregados de todos os outcomes

**Visualizações (`.png`, `.pdf`):**
- `event_study_cana.png`: Gráfico event-study principal
- `parallel_trends_complete_*.png`: Tendências paralelas por outcome
- `robustness_plot.png`: Forest plot com testes de robustez

**Integração LaTeX:**
- `latex_values.tex`: Macros LaTeX com todos os valores numéricos

#####  Subdiretórios

- **`descriptive_analysis/`**: Análises exploratórias e descritivas
- **`additional_figures/`**: Figuras complementares (poder estatístico, sensibilidade)
- **`presentation/`**: Material para apresentação (dashboard HTML, figuras aprimoradas)

### `data/csv/` - Dados Processados

Dataset principal utilizado nas análises.

#### Arquivo principal:

**`microrregions_Cana-de-açúcar-Soja-Arroz_2003-2021_mapbiomas.csv`**

Dataset em painel balanceado com múltiplas culturas (cana, soja, arroz) agregadas ao nível de microrregião.

##### Variáveis principais:

| Coluna | Tipo | Descrição |
|--------|------|-----------|
| `ano` | int | Ano da observação (2003-2021) |
| `id_microrregiao` | int | Código IBGE da microrregião |
| `sigla_uf` | str | Sigla do estado |
| `primeiro_ano_tratamento` | int | Ano da primeira estação (variável de tratamento) |
| `area_cana`, `area_soja`, `area_arroz` | float | Área plantada por cultura (hectares) |
| `valor_producao_cana`, `_soja`, `_arroz` | float | Valor de produção por cultura (R$ mil, valores constantes) |
| `pib_agropecuario` | float | PIB agropecuário (R$ constantes) |
| `populacao_total` | int | População municipal agregada |
| `densidade_estacoes_uf` | float | Densidade de estações por km² no estado |

##### Características:
- **Unidade**: Microrregião-ano
- **Período**: 2003-2021 (19 anos)
- **Observações**: ~9.310 (225 microrregiões produtoras × 19 anos)
- **Formato**: CSV, UTF-8, separador vírgula
- **Filtro**: Microrregiões com pelo menos um ano de produção da cultura relevante

## Pré-requisitos

| Tecnologia | Versão | Necessário para |
|------------|--------|-----------------|
| **R** | ≥ 4.5 | Análise estatística |
| **Python** | ≥ 3.9 | Extração de dados (opcional) |
| **LaTeX** | texlive-full | Compilação da tese |
| **GNU Make** | Qualquer versão | Automação (recomendado) |

**Nota sobre extração de dados:** Os dados já processados estão incluídos no repositório. A extração via BigQuery só é necessária para replicar completamente desde a origem ou modificar a amostra.

## Como Reproduzir

### 1. Instalação

Clone o repositório e instale as dependências do R:

```bash
git clone https://github.com/danielcavalli/tcc-ie-ufrj-2024.git
cd tcc-ie-ufrj-2024
make renv-restore
```

### 2. Execução da Análise

Para rodar a análise completa e gerar o PDF da tese:

```bash
make analysis    # Executa scripts R
make thesis      # Compila documento LaTeX
```

Os resultados (gráficos e tabelas) estarão em `data/outputs/`. O PDF final estará em `documents/drafts/latex_output/`.

## Fontes de Dados

Este projeto utiliza dados públicos agregados de múltiplas fontes oficiais:

### 1. Produção Agrícola Municipal (PAM/IBGE)
- **Fonte**: IBGE via Base dos Dados
- **Conteúdo**: Área plantada, produção e valor de produção por cultura e município
- **Culturas**: Cana-de-açúcar, soja, arroz
- **Período**: 2003-2021
- **Acesso**: [basedosdados.org](https://basedosdados.org/)

### 2. Estações Meteorológicas (INMET)
- **Fonte**: Instituto Nacional de Meteorologia
- **Conteúdo**: Localização geográfica e ano de instalação das estações automáticas
- **Uso**: Construção da variável de tratamento (primeiro ano com estação na microrregião)
- **Acesso**: Portal INMET / Base dos Dados

### 3. PIB Municipal (IBGE)
- **Fonte**: IBGE Contas Regionais via Base dos Dados
- **Conteúdo**: PIB total, PIB agropecuário, PIB per capita
- **Período**: 2003-2021
- **Uso**: Outcomes agregados e covariáveis

### 4. População (IBGE)
- **Fonte**: IBGE Estimativas Populacionais
- **Uso**: Covariável de controle

### 5. Classificação Geográfica (IBGE)
- **Fonte**: Divisões Territoriais IBGE
- **Uso**: Mapeamento município → microrregião

**Replicação:** Todas as queries SQL utilizadas estão documentadas em `data/sql_query/`. O dataset processado já está incluído no repositório.

## Licença

Este projeto é licenciado sob os termos da [GNU General Public License v3.0](LICENSE). Isso significa que você pode:
- Usar o código para qualquer propósito
- Modificar o código fonte
- Distribuir o código original ou modificado
- Usar o código em projetos comerciais

Com as seguintes condições:
- Deve manter a mesma licença GPL v3.0
- Deve disponibilizar o código fonte
- Deve documentar as modificações realizadas
- Deve incluir a licença e avisos de copyright

Para mais detalhes, consulte o arquivo [LICENSE](LICENSE) ou visite [GNU GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.html).

## Citação

Se você utilizar este código, dados ou metodologia em seu trabalho, por favor cite:

```bibtex
@mastersthesis{cavalli2025,
  author  = {Cavalli, Daniel},
  title   = {Impacto de Estações Meteorológicas na Cultura de Cana-de-Açúcar: Uma Aplicação de Diferenças em Diferenças com Tratamento Escalonado},
  school  = {Instituto de Economia, Universidade Federal do Rio de Janeiro},
  year    = {2025},
  type    = {Trabalho de Conclusão de Curso},
  address = {Rio de Janeiro, Brasil}
}
```

## Contato

**Daniel Cavalli**
Instituto de Economia, UFRJ
Email: daniel.cavalli@ufrj.br
GitHub: [@danielcavalli](https://github.com/danielcavalli)

---

## Notas Adicionais

- Os valores numéricos reportados na tese são gerados pelo pipeline de análise, assegurando consistência entre o código e o texto.
- O código segue práticas de organização para facilitar a reprodutibilidade dos resultados.
- Questões e sugestões são bem-vindas via [GitHub Issues](https://github.com/danielcavalli/tcc-ie-ufrj-2024/issues)