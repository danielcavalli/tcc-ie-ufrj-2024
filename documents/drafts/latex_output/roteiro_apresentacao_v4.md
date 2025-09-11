# Roteiro de Apresentação V4 - Foco em Metodologia e Causalidade
## Daniel Cavalli - IE/UFRJ - 2025

**ARQUIVO**: `apresentacao_defesa_v4.tex`

---

## SLIDE 1: TÍTULO (30 segundos)

"Bom dia, professores. Meu nome é Daniel Cavalli e apresento hoje meu trabalho sobre o impacto causal de estações meteorológicas na produtividade agrícola brasileira. Este estudo foi orientado pelo Professor Romero Rocha."

---

## SLIDE 2: ROTEIRO (20 segundos)

"A apresentação está organizada em seis seções. Começarei contextualizando o problema, depois apresento a estratégia empírica e a solução metodológica. Em seguida, mostro os resultados principais e os testes de robustez, finalizando com as implicações econômicas."

---

## SLIDE 3: CONTEXTO (1 minuto)

"A agricultura brasileira enfrenta crescente variabilidade climática num contexto onde precisamos aumentar a produtividade. A informação meteorológica emerge como insumo produtivo crítico.

A literatura existente é predominantemente descritiva. Não há evidências causais sobre o impacto econômico da expansão da infraestrutura meteorológica. 

Felizmente, a instalação escalonada de estações entre 2000 e 2019 criou um experimento natural que permite identificação causal.

Nossa questão central: qual o impacto causal da instalação de estações meteorológicas sobre o PIB agropecuário?"

---

## SLIDE 4: MECANISMO CAUSAL (45 segundos)

"O mecanismo é direto: estações geram dados climáticos precisos, que permitem decisões otimizadas, resultando em aumento do PIB agropecuário.

A literatura, especialmente Mavi e Tupper 2004, identifica três dimensões: planejamento estratégico, decisões táticas e construção de resiliência. 

Há também um canal indireto importante: os dados alimentam modelos de simulação como o DSSAT-CANEGRO, permitindo otimização antes da implementação no campo."

---

## SLIDE 5: DADOS E DEFINIÇÕES (1 minuto)

"Analisamos 490 microrregiões brasileiras ao longo de 21 anos, totalizando 10.290 observações. 

O critério de seleção foram microrregiões produtoras de cana-de-açúcar - pela sensibilidade climática e qualidade dos dados. Mas medimos o impacto no PIB agropecuário TOTAL.

O tratamento é a instalação da primeira estação meteorológica. Das 490 microrregiões, 351 foram tratadas, com concentração em 2006-2008 devido a programas federais."

---

## SLIDE 6: PROBLEMA DO TRATAMENTO ESCALONADO (1 minuto)

"O método tradicional de diferenças em diferenças - o Two-Way Fixed Effects - falha quando o tratamento é escalonado.

Por quê? Ele usa unidades já tratadas como controle, pode gerar pesos negativos, e produz viés quando há efeitos heterogêneos.

A figura mostra o problema: comparações entre unidades tratadas em momentos diferentes. Isso foi demonstrado por Goodman-Bacon e Sun & Abraham em 2021."

---

## SLIDE 7: SOLUÇÃO METODOLÓGICA (1 minuto e 15 segundos)

"Callaway e Sant'Anna desenvolveram uma solução elegante em três etapas:

Primeiro, estimação desagregada - calculamos ATT(g,t) para cada grupo e período, comparando apenas com unidades ainda não tratadas.

Segundo, agregação apropriada - combinamos os efeitos individuais para obter o efeito médio geral e o event study.

Terceiro, inferência robusta - usamos bootstrap multiplicativo com mil replicações e clustering por microrregião.

Utilizamos o estimador Doubly Robust, que permanece consistente mesmo se especificarmos incorretamente o modelo de resultado OU o propensity score."

---

## SLIDE 8: VALIDAÇÃO DOS PRESSUPOSTOS (1 minuto)

"O pressuposto crucial é tendências paralelas. Validamos de duas formas: visualmente no event study e formalmente com teste F por coorte. O resultado - F de 1,136 com p-valor 0,322 - confirma fortemente o pressuposto.

Também validamos no anticipation - efeitos apenas após instalação física. Se violado, nossas estimativas seriam conservadoras.

O tratamento é irreversível - estações permanecem ativas. E há overlap nas características entre tratados e controles.

Todos os pressupostos foram testados e validados empiricamente."

---

## SLIDE 9: RESULTADO PRINCIPAL (1 minuto)

"O resultado principal: as estações meteorológicas causam um aumento de 8,2% no PIB agropecuário, com intervalo de confiança entre 1,9% e 14,5%.

Para contextualizar: o setor cresce tipicamente 3-4% ao ano. Nosso efeito equivale a mais de dois anos de crescimento - é um salto estrutural.

A robustez é notável: usando diferentes estimadores, o efeito varia apenas entre 6,6% e 9,4%, sempre altamente significativo."

---

## SLIDE 10: EVENT STUDY (1 minuto)

"O event study conta a história completa. 

No pré-tratamento, vemos ausência de tendências diferenciadas - validação crucial da estratégia de identificação.

Após o tratamento, os efeitos emergem gradualmente e se estabilizam em torno de 10% após 5 anos. Isso é consistente com um processo de aprendizado e adaptação tecnológica."

---

## SLIDE 11: TESTE DE RANDOMIZAÇÃO (1 minuto e 30 segundos)

"Para garantir que o resultado não é coincidência, implementamos um teste rigoroso de randomização de Monte Carlo.

Realizamos 5 mil simulações, randomizando completamente quais unidades recebem tratamento e quando. Para cada simulação, estimamos o ATT com o mesmo método.

O histograma mostra a distribuição dos ATTs placebo centrada em zero. Nosso ATT verdadeiro está na cauda extrema. O p-valor empírico menor que 0,001 significa que menos de 0,1% das simulações produziram efeito similar.

É praticamente impossível que este resultado seja fruto do acaso."

---

## SLIDE 12: TESTES COMPLEMENTARES (1 minuto)

"Realizamos múltiplos testes complementares.

Primeiro, aplicamos o modelo ao PIB não-agropecuário. Resultado: efeito não significativo, confirmando especificidade setorial.

Segundo, atribuição completamente aleatória produz efeito zero.

Terceiro, o efeito é robusto a diferentes períodos - excluindo COVID ou anos iniciais.

Quarto, a escolha do grupo de controle tem impacto mínimo.

Conclusão inequívoca: o efeito é causal, robusto e específico à agricultura."

---

## SLIDE 13: SÍNTESE DA ROBUSTEZ (30 segundos)

"Este gráfico sintetiza todos os testes. O efeito permanece consistente entre 6 e 13% em todas as especificações. 

Essa estabilidade é notável e reforça a confiabilidade de nossa estimativa principal de 8,2%."

---

## SLIDE 14: MAGNITUDE ECONÔMICA (1 minuto)

"O efeito de 8,2% representa um salto estrutural na produtividade. Das 490 microrregiões, 351 já foram beneficiadas, mas 139 - quase 30% - ainda não têm cobertura.

Em dezembro de 2024, o MAPA anunciou 49 milhões para 220 novas estações. Nossa evidência mostra que os benefícios superam amplamente os custos de implementação.

A expansão da rede é justificada como estratégia de desenvolvimento e adaptação climática."

---

## SLIDE 15: CONCLUSÕES (1 minuto)

"Este trabalho oferece quatro contribuições principais:

Primeira, evidência causal pioneira - a primeira quantificação rigorosa do impacto de estações meteorológicas.

Segunda, avanço metodológico - demonstração prática de DiD com tratamento escalonado para avaliação de políticas.

Terceira, caracterização da dinâmica - documentamos o processo gradual de difusão com efeitos persistentes.

Quarta, subsídios para políticas - o retorno social supera os custos, justificando expansão urgente.

A mensagem central: informação meteorológica é investimento com retorno econômico comprovado."

---

## SLIDE 16: AGRADECIMENTO (20 segundos)

"Agradeço a atenção. O código e os dados estão disponíveis publicamente no GitHub. Estou à disposição para questões."

---

## TEMPO TOTAL: 14-15 minutos

## PONTOS DE ÊNFASE:
1. **Causalidade** - sempre enfatizar que é o primeiro estudo causal
2. **Metodologia** - destacar a inovação em usar Callaway & Sant'Anna
3. **Robustez** - múltiplos testes convergem para o mesmo resultado
4. **Relevância** - impacto econômico significativo justifica políticas

## SLIDES DE BACKUP DISPONÍVEIS:
- Especificação detalhada do modelo
- Distribuição regional (dados do Apêndice B)
- Análise de poder estatístico (92,1% para α=0,05)
