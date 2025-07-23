.PHONY: help env lock install update-lock clean shell fetch

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