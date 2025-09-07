# Makefile para compilar o TCC em formato ABNT2

# Nome do arquivo principal (sem extensão)
MAIN = TCC_DanielCavalli_ABNT2

# Compilador LaTeX
LATEX = pdflatex
BIBTEX = bibtex

# Flags do compilador
LATEXFLAGS = -interaction=nonstopmode -halt-on-error

# Arquivos auxiliares
AUX_FILES = *.aux *.log *.toc *.bbl *.blg *.out *.lof *.lot *.ilg *.ind *.idx *.brf

# Regra padrão
all: $(MAIN).pdf

# Compilação completa
$(MAIN).pdf: $(MAIN).tex referencias.bib
	@echo "Primeira compilação..."
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex
	@echo "Processando bibliografia..."
	$(BIBTEX) $(MAIN)
	@echo "Segunda compilação..."
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex
	@echo "Terceira compilação (para resolver referências)..."
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex
	@echo "Compilação concluída!"

# Compilação rápida (apenas uma passada)
quick: $(MAIN).tex
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex

# Forçar recompilação completa
force:
	@echo "Forçando recompilação completa..."
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex
	$(BIBTEX) $(MAIN)
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex
	@echo "Compilação concluída!"

# Limpar arquivos auxiliares
clean:
	@echo "Removendo arquivos auxiliares..."
	rm -f $(AUX_FILES)

# Limpar tudo (incluindo PDF)
distclean: clean
	@echo "Removendo PDF..."
	rm -f $(MAIN).pdf

# Visualizar o PDF
view: $(MAIN).pdf
	@echo "Abrindo PDF..."
	@if command -v open >/dev/null 2>&1; then \
		open $(MAIN).pdf; \
	elif command -v xdg-open >/dev/null 2>&1; then \
		xdg-open $(MAIN).pdf; \
	else \
		echo "Não foi possível abrir o PDF automaticamente."; \
	fi

# Help
help:
	@echo "Uso: make [target]"
	@echo ""
	@echo "Targets disponíveis:"
	@echo "  all       - Compilação completa (padrão)"
	@echo "  quick     - Compilação rápida (apenas uma passada)"
	@echo "  force     - Força recompilação completa (ignora timestamps)"
	@echo "  clean     - Remove arquivos auxiliares"
	@echo "  distclean - Remove arquivos auxiliares e PDF"
	@echo "  view      - Compila e abre o PDF"
	@echo "  help      - Mostra esta ajuda"
	@echo ""
	@echo "Dica: Se 'make' diz 'Nothing to be done', use 'make force' ou 'make clean all'"

.PHONY: all quick force clean distclean view help