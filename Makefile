.PHONY: help env lock install update-lock clean shell fetch renv-init renv-restore renv-snapshot renv-clean

# -------- Configuration ------------------------------------------------------
# Virtual-env directory
VENV := .venv

# Select Python executable (override e.g. `make PY=python3.11 lock`)
PY ?= python3

PYTHON := $(VENV)/bin/python
PIP := $(VENV)/bin/pip

# -----------------------------------------------------------------------------
help:  ## Show this help
	@echo "Usage: make <target> [PY=pythonX.Y]"
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?##' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-14s %s\n", $$1, $$2}' | sort

# -----------------------------------------------------------------------------
# Environment setup & dependency locking
# -----------------------------------------------------------------------------

env:  ## Create local virtual environment ($(VENV))
	$(PY) -m venv $(VENV)
	$(PIP) install --upgrade pip

# requirements.in holds unpinned deps; lock pins them into requirements.txt
lock: clean env  ## Install deps from requirements.in and freeze versions
	@if [ ! -f requirements.in ]; then echo "requirements.in missing" && exit 1; fi
	$(PIP) install -r requirements.in
	$(PIP) freeze | sort > requirements.txt
	@echo "Locked dependencies written to requirements.txt"

install: env  ## Install exact versions from requirements.txt
	@if [ ! -f requirements.txt ]; then echo "requirements.txt missing — run 'make lock' first" && exit 1; fi
	$(PIP) install -r requirements.txt

update-lock: install  ## Refresh requirements.txt after adding packages
	$(PIP) freeze | sort > requirements.txt
	@echo "requirements.txt updated"

# -----------------------------------------------------------------------------
# Convenience targets
# -----------------------------------------------------------------------------

shell: env  ## Drop into a shell with venv activated
	@echo "Activating virtual environment; exit shell to return."
	/bin/bash -c "source $(VENV)/bin/activate && exec bash --norc --noprofile -i"

fetch: install  ## Run data-extraction (needs PROJECT_ID=...)
	@if [ -z "$(PROJECT_ID)" ]; then echo "Need PROJECT_ID=your-gcp-project" && exit 1; fi
	$(PYTHON) scripts/fetch_sugar_cane_data.py --project_id $(PROJECT_ID)

clean:  ## Remove venv and artefacts
	rm -rf $(VENV)
	rm -f *.pyc
	@echo "Cleaned Python virtual environment" 

# -----------------------------------------------------------------------------
# R environment setup & dependency locking (renv)
# -----------------------------------------------------------------------------

# Directory where renv stores project libraries
RENVDIR := renv

R := Rscript

# Initialise renv (creates renv/ and renv.lock if absent, or restores if lock exists)
renv-init:  ## Create or restore renv-managed R environment
	@if command -v $(R) >/dev/null 2>&1; then :; else echo "Rscript not found – install R first" && exit 1; fi
	@$(R) -e "if (!require('renv')) install.packages('renv', repos = 'https://cloud.r-project.org')"
	@if [ -f renv.lock ]; then \
	  $(R) -e "renv::restore(prompt = FALSE)"; \
	else \
	  $(R) -e "renv::init(bare = TRUE, force = TRUE)"; \
	fi
	@echo "renv environment ready"

# Install packages exactly as recorded in renv.lock
renv-restore:  ## Install R packages from renv.lock
	@$(R) -e "if (!require('renv')) install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(prompt = FALSE)"

# Snapshot current package versions into renv.lock
renv-snapshot:  ## Update renv.lock with current package state
	@$(R) -e "if (!require('renv')) install.packages('renv', repos = 'https://cloud.r-project.org'); renv::snapshot(prompt = FALSE)"
	@echo "renv.lock updated"

# Remove the local renv library cache (does NOT delete lock file)
renv-clean:  ## Delete renv library directory
	rm -rf $(RENVDIR)/library
	@echo "Removed renv library directory"

# -----------------------------------------------------------------------------
# LaTeX value generation
# -----------------------------------------------------------------------------

latex-values:  ## Generate LaTeX values from R outputs
	@$(R) rscripts/generate_latex_values.r
	@echo "LaTeX values updated in documents/drafts/latex_output/auto_values.tex"

# Run the complete DiD analysis and update LaTeX values
analysis: renv-restore  ## Run complete DiD analysis and update LaTeX values
	@$(R) rscripts/did_v2.r
	@$(R) rscripts/generate_latex_values.r
	@echo "Analysis complete and LaTeX values updated"

# -----------------------------------------------------------------------------
# LaTeX document compilation
# -----------------------------------------------------------------------------

# Compile the thesis document
thesis: latex-values  ## Compile the thesis PDF (TCC)
	@echo "📄 Compiling thesis document..."
	@cd documents/drafts/latex_output && \
	pdflatex -interaction=nonstopmode TCC_DanielCavalli_ABNT2.tex && \
	bibtex TCC_DanielCavalli_ABNT2 && \
	pdflatex -interaction=nonstopmode TCC_DanielCavalli_ABNT2.tex && \
	pdflatex -interaction=nonstopmode TCC_DanielCavalli_ABNT2.tex
	@echo "✅ Thesis compiled: documents/drafts/latex_output/TCC_DanielCavalli_ABNT2.pdf"

# Compile the defense presentation
presentation: latex-values  ## Compile the defense presentation PDF
	@echo "📊 Compiling defense presentation..."
	@cd documents/drafts/latex_output && \
	pdflatex -interaction=nonstopmode apresentacao_defesa.tex && \
	pdflatex -interaction=nonstopmode apresentacao_defesa.tex
	@echo "✅ Presentation compiled: documents/drafts/latex_output/apresentacao_defesa.pdf"

# Compile all LaTeX documents
docs: thesis presentation  ## Compile both thesis and presentation

# Clean LaTeX auxiliary files
latex-clean:  ## Remove LaTeX auxiliary files
	@echo "🧹 Cleaning LaTeX auxiliary files..."
	@cd documents/drafts/latex_output && \
	rm -f *.aux *.log *.out *.toc *.lof *.lot *.bbl *.blg *.idx *.ilg *.ind *.synctex.gz *.fdb_latexmk *.fls *.nav *.snm *.vrb
	@echo "✅ LaTeX cleanup complete" 