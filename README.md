# Análise de Produtividade da Cana-de-Açúcar no Brasil

> Trabalho de Conclusão de Curso – Instituto de Economia, UFRJ (2024)

Este repositório contém os códigos, dados e documentação utilizados no meu Trabalho de Conclusão de Curso (TCC) no Instituto de Economia da Universidade Federal do Rio de Janeiro. O estudo investiga os determinantes climáticos e econômicos da produtividade da cana-de-açúcar no Brasil, empregando métodos econométricos em painel e diferenças-em-diferenças.

## Índice
1. [Objetivos do projeto](#objetivos-do-projeto)
2. [Estrutura do repositório](#estrutura-do-repositório)
3. [Pré-requisitos](#pré-requisitos)
4. [Instalação do ambiente](#instalação-do-ambiente)
5. [Reprodução dos resultados](#reprodução-dos-resultados)
6. [Fontes de dados](#fontes-de-dados)
7. [Licença](#licença)
8. [Autor e citação](#autor-e-citação)

## Objetivos do projeto
1. Reunir e tratar bases de dados agropecuários (PAM/IBGE), meteorológicos (INMET) e de mercado.
2. Construir métricas mensais de clima e produtividade agrícola.
3. Estimar o impacto de choques climáticos sobre a produtividade da cana-de-açúcar utilizando modelos em painel e a estratégia de diferenças-em-diferenças (DiD).
4. Documentar todo o fluxo de extração, transformação, análise e visualização de dados para garantir reprodutibilidade científica.

## Visão geral da pesquisa

Esta pesquisa investiga o impacto causal da instalação de novas estações meteorológicas sobre a produtividade agrícola em áreas rurais brasileiras. A hipótese central é que informações climáticas mais precisas e localizadas melhoram a tomada de decisão no setor agrícola. O foco empírico recai sobre a produtividade da cana-de-açúcar, cultura altamente sensível à precipitação e à umidade do solo.

## Metodologia

Empregamos um modelo de Diferenças-em-Diferenças (DiD) com adoção escalonada, conforme desenvolvido por Callaway & Sant'Anna (2020). Este arcabouço é adequado para cenários em que múltiplos grupos tratados recebem a intervenção em anos distintos ao longo de 22 anos de análise.

* **Especificação do modelo**: estimamos os Efeitos Médios de Tratamento por Grupo e Tempo, \(ATT(g,t)\), que medem o efeito médio para o grupo \(g\) no tempo \(t\). Esses efeitos individuais são posteriormente agregados para inferências mais amplas.
* **Estimador**: utilizamos o estimador *doubly robust* (DR), que combina regressão do desfecho (*Outcome Regression*) e ponderação por probabilidade inversa (*Inverse Probability Weighting*), provendo robustez a violações de especificação.
* **Suposição chave**: pressupõe-se a tendência paralela entre grupos tratados e de controle na ausência de tratamento. Testes estatísticos nos períodos pré-tratamento não rejeitam essa hipótese.

## Dados e variáveis

| Tipo | Variável | Descrição |
|------|----------|-----------|
| Desfecho | `log_produtividade` | Logaritmo da produtividade da cana-de-açúcar |
| Tratamento | `gname` | Ano em que a microrregião recebe a primeira estação nova |
| Covariáveis | `qtd_estacoes_ativas` | Número de estações ativas |
|            | `precipitacao_por_area_plantada` | Precipitação por área plantada |
| Grupo de controle | "never-treated" | Microrregiões que nunca receberam tratamento |

## Principais resultados

* **Impacto médio**: a instalação de novas estações meteorológicas resultou em aumento estatisticamente significativo da produtividade agrícola, entre **9,4 %** e **17,7 %**. O ATT agregado do *event study* é **0,1409**, com intervalo de confiança de 95 % \([0,0225; 0,2592]\).
* **Dinâmica temporal**: os efeitos não são imediatos; tornam-se mais pronunciados a partir do terceiro ano pós-tratamento.
* **Período pré-tratamento**: efeitos médios próximos de zero (0,011) reforçam a validade da hipótese de tendências paralelas.
* **Heterogeneidade**: há variação substancial entre coortes; por exemplo, a coorte de 2017 apresenta efeitos positivos consistentes, enquanto a de 2018 exibe maior variabilidade.

## Detalhes de implementação do código

A análise empírica foi conduzida no R com o pacote `did`.

Passos principais:

1. **Preparação de dados** – carregamento e transformação (incluindo transformação logarítmica da produtividade).
2. **Estimativa de \(ATT(g,t)\)** – função `att_gt()` com método "dr".
3. **Agregação de efeitos** – função `aggte()` para resultados globais e dinâmicos (*event-study*).
4. **Visualização** – gráficos gerados com `ggplot2`.
5. **Testes formais** – avaliação de efeitos pré- e pós-tratamento.

O script correspondente encontra-se em `rscripts/did_v2.r`.

## Estrutura do repositório
```
├── data/                 # Dados brutos e tratados
│   ├── csv/              # Arquivos CSV intermediários
│   ├── parquet/          # Arquivos Parquet
│   └── sql_query/        # Consultas SQL usadas no BigQuery
├── documents/            # Rascunhos e versão final do TCC
├── rscripts/             # Scripts em R (análises estatísticas)
├── scripts/              # Scripts em Python (ETL, coleta de dados)
├── renv/ & renv.lock     # Ambiente R reproduzível (renv)
├── requirements.txt      # Dependências Python
└── Makefile              # Comandos automatizados
```

## Pré-requisitos

| Tecnologia | Versão recomendada |
|------------|--------------------|
| Python     | ≥ 3.9              |
| R          | ≥ 4.2              |
| Renv       | ≥ 0.17             |
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
1. **Extração & tratamento de dados (Python):**
   ```bash
   python scripts/fetch_sugar_cane_data.py        # Baixa e processa dados no BigQuery
   ```
   Arquivos tratados serão salvos em `data/csv/` e `data/parquet/`.

2. **Análises econométricas (R):**
   ```bash
   Rscript rscripts/did_v2.r                      # Estima modelos DiD
   ```
   Saídas (tabelas, gráficos) são exportadas para `documents/`.

3. **Compilação do relatório:** edite o arquivo LaTeX/Markdown (caso aplicável) ou consulte a versão em PDF em `documents/drafts/`.

## Fontes de dados
* **PAM/IBGE** – Produção Agrícola Municipal (acesso via BigQuery).
* **INMET** – Estações meteorológicas (temperatura, precipitação).
* **Outros** – Séries históricas de preços de açúcar/etanol.

As consultas SQL utilizadas para extração dos dados encontram-se em `data/sql_query/`.

## Licença

Este projeto é licenciado sob os termos da [MIT License](LICENSE). Sinta-se livre para utilizar e adaptar o código, desde que cite a fonte.

## Autor e citação

| Autor | Contato |
|-------|---------|
| Daniel Cavalli | <daniel.cavalli@ufrj.br> |

Se você utilizar este código ou dados em seu trabalho, por favor cite da seguinte forma:

> Cavalli, D. (2024). *Análise de Produtividade da Cana-de-Açúcar no Brasil*. Trabalho de Conclusão de Curso (Graduação em Ciências Econômicas), Instituto de Economia, Universidade Federal do Rio de Janeiro.

---

**Observação:** Este repositório segue as recomendações de reprodutibilidade do [Turing Way](https://the-turing-way.netlify.app/).