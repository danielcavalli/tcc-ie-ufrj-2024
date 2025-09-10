# Documentação LaTeX - TCC Daniel Cavalli

Este diretório contém os arquivos LaTeX para o Trabalho de Conclusão de Curso e apresentação de defesa.

## Arquivos Principais

- `TCC_DanielCavalli_ABNT2.tex` - Documento principal do TCC (formatação ABNT)
- `apresentacao_defesa.tex` - Apresentação em Beamer para defesa
- `referencias.bib` - Bibliografia completa
- `auto_values.tex` - Valores gerados automaticamente a partir dos resultados R

## Compilação

### Usando o Makefile (recomendado)

A partir da raiz do projeto:

```bash
# Gerar valores automáticos do R
make latex-values

# Compilar apenas o TCC
make thesis

# Compilar apenas a apresentação
make presentation

# Compilar ambos os documentos
make docs

# Limpar arquivos auxiliares
make latex-clean
```

### Compilação Manual

Se preferir compilar manualmente:

#### TCC:
```bash
pdflatex TCC_DanielCavalli_ABNT2.tex
bibtex TCC_DanielCavalli_ABNT2
pdflatex TCC_DanielCavalli_ABNT2.tex
pdflatex TCC_DanielCavalli_ABNT2.tex
```

#### Apresentação:
```bash
pdflatex apresentacao_defesa.tex
pdflatex apresentacao_defesa.tex
```

## Requisitos

- LaTeX completo (TeX Live ou MiKTeX)
- Pacotes ABNT (abntex2) para o TCC
- Beamer para a apresentação
- BibTeX para referências

## Notas Importantes

1. **auto_values.tex**: Este arquivo é gerado automaticamente pelo script R `generate_latex_values.r`. Não edite manualmente.

2. **Figuras**: As figuras estão localizadas em `data/outputs/`. Certifique-se de que a análise R foi executada antes de compilar.

3. **Bibliografia**: O arquivo `referencias.bib` contém todas as referências. Para adicionar novas referências, edite este arquivo.

## Estrutura da Apresentação

A apresentação está organizada em:
- Introdução e motivação
- Revisão da literatura
- Metodologia (DiD escalonado)
- Dados e estratégia empírica
- Resultados principais
- Testes de robustez
- Conclusões e implicações

## Customização

### Cores e Tema
A apresentação usa o tema Madrid com cores customizadas da UFRJ. Para alterar:
- Edite as definições de cor no preâmbulo
- Mude o tema com `\usetheme{NomeTema}`

### Tempo de Apresentação
A apresentação está dimensionada para aproximadamente 15-20 minutos, adequada para defesa de TCC.

