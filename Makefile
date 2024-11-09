# Makefile

# Default values (can be overridden when calling 'make')
name ?= myenv
py ?= 3.8

.PHONY: env compile check-pip-tools

# Command to create a conda environment with a specified name and Python version
env:
	conda create -y -n $(name) python=$(py)

# Check if pip-tools is installed; if not, install it
check-pip-tools:
	@if ! pip show pip-tools > /dev/null 2>&1; then \
		echo "pip-tools not found. Installing pip-tools..."; \
		pip install pip-tools; \
	fi

# Command to compile requirements.in into requirements.txt, ensuring pip-tools is available
compile: check-pip-tools
	pip-compile requirements.in -o requirements.txt

# Example usage:
# make env name=myenv py=3.9
# make compile