# Makefile local para compilação dos documentos LaTeX
# Este Makefile permite compilar os documentos diretamente deste diretório

.PHONY: help presentation presentation-clean presentation-v2 presentation-v3 presentation-refinada thesis all clean docx docx-from-pdf rtf docx-preserveformat docx-libreoffice docx-tex4ht docx-python docx-best

help:
	@echo "Comandos disponíveis:"
	@echo "  make presentation           - Compila apresentação original (Madrid+Whale)"
	@echo "  make presentation-revisada  - Compila apresentação Madrid refinada"
	@echo "  make thesis                 - Compila o TCC completo"
	@echo "  make docx-best              - ⭐ MELHOR OPÇÃO: Guia para conversão perfeita"
	@echo "  make docx                   - Converte o TCC para Word (.docx) via LaTeX"
	@echo "  make docx-python            - DOCX via Python pdf2docx (requer instalação)"
	@echo "  make docx-preserveformat    - DOCX com formatação preservada (pandoc)"
	@echo "  make docx-libreoffice       - DOCX via LibreOffice (preserva layout)"
	@echo "  make docx-tex4ht            - DOCX via tex4ht (melhor para equações)"
	@echo "  make docx-from-pdf          - Converte o TCC para Word (.docx) via PDF"
	@echo "  make rtf                    - Converte o TCC para RTF (alternativa ao .docx)"
	@echo "  make all                    - Compila TCC + apresentação original"
	@echo "  make clean                  - Remove arquivos auxiliares"

presentation:
	@echo "📊 Compilando apresentação..."
	@pdflatex -interaction=nonstopmode apresentacao_defesa.tex
	@pdflatex -interaction=nonstopmode apresentacao_defesa.tex
	@echo "✅ Apresentação compilada: apresentacao_defesa.pdf"

presentation-revisada:
	@echo "✨ Compilando apresentação Madrid refinada..."
	@pdflatex -interaction=nonstopmode apresentacao_defesa_v4.tex
	@pdflatex -interaction=nonstopmode apresentacao_defesa_v4.tex
	@echo "✅ Apresentação refinada compilada: apresentacao_defesa_revisada.pdf"

thesis:
	@echo "📄 Compilando TCC..."
	@pdflatex -interaction=nonstopmode TCC_DanielCavalli_ABNT2.tex
	@bibtex TCC_DanielCavalli_ABNT2
	@pdflatex -interaction=nonstopmode TCC_DanielCavalli_ABNT2.tex
	@pdflatex -interaction=nonstopmode TCC_DanielCavalli_ABNT2.tex
	@echo "✅ TCC compilado: TCC_DanielCavalli_ABNT2.pdf"

all: presentation thesis

docx:
	@echo "📝 Convertendo TCC para Word (.docx) via LaTeX..."
	@if command -v pandoc >/dev/null 2>&1; then \
		echo "🔄 Tentando conversão com bibliografia..."; \
		pandoc TCC_DanielCavalli_ABNT2.tex \
			--from=latex \
			--to=docx \
			--output=TCC_DanielCavalli_ABNT2.docx \
			--bibliography=referencias.bib \
			--metadata lang=pt-BR \
			--metadata title="Impacto de Estações Meteorológicas na Produtividade Agrícola" \
			--metadata author="Daniel Cavalli" \
			--variable documentclass=article \
			--variable fontsize=12pt \
			-V geometry:margin=1in \
			2>/dev/null || { \
			echo "⚠️  Conversão com bibliografia falhou. Tentando conversão simplificada..."; \
			pandoc TCC_DanielCavalli_ABNT2.tex \
				--from=latex \
				--to=docx \
				--output=TCC_DanielCavalli_ABNT2.docx \
				--metadata lang=pt-BR; \
		}; \
		echo "✅ Conversão concluída: TCC_DanielCavalli_ABNT2.docx"; \
		echo "📌 Nota: A conversão pode não preservar toda a formatação ABNT."; \
		echo "    Revise cuidadosamente o documento Word gerado."; \
	else \
		echo "❌ Erro: pandoc não está instalado. Instale com:"; \
		echo "   macOS: brew install pandoc"; \
		echo "   Ubuntu/Debian: sudo apt-get install pandoc"; \
		echo "   Fedora: sudo dnf install pandoc"; \
	fi

docx-from-pdf:
	@echo "📝 Convertendo TCC para Word (.docx) via PDF..."
	@if ! [ -f "TCC_DanielCavalli_ABNT2.pdf" ]; then \
		echo "⚠️  PDF não encontrado. Compilando primeiro..."; \
		$(MAKE) thesis; \
	fi
	@if command -v pandoc >/dev/null 2>&1; then \
		pandoc TCC_DanielCavalli_ABNT2.pdf \
			--from=pdf \
			--to=docx \
			--output=TCC_DanielCavalli_ABNT2_from_pdf.docx \
			--extract-media=media \
			2>/dev/null || \
		echo "⚠️  Conversão via PDF pode não preservar toda formatação."; \
		echo "✅ Conversão concluída: TCC_DanielCavalli_ABNT2_from_pdf.docx"; \
	else \
		echo "❌ Erro: pandoc não está instalado. Instale com:"; \
		echo "   macOS: brew install pandoc"; \
		echo "   Ubuntu/Debian: sudo apt-get install pandoc"; \
		echo "   Fedora: sudo dnf install pandoc"; \
	fi

rtf:
	@echo "📝 Convertendo TCC para RTF..."
	@if command -v pandoc >/dev/null 2>&1; then \
		pandoc TCC_DanielCavalli_ABNT2.tex \
			--from=latex \
			--to=rtf \
			--output=TCC_DanielCavalli_ABNT2.rtf \
			--standalone \
			--bibliography=referencias.bib \
			--metadata lang=pt-BR \
			2>/dev/null || { \
			echo "⚠️  Conversão com bibliografia falhou. Tentando conversão simplificada..."; \
			pandoc TCC_DanielCavalli_ABNT2.tex \
				--from=latex \
				--to=rtf \
				--output=TCC_DanielCavalli_ABNT2.rtf \
				--standalone; \
		}; \
		echo "✅ Conversão concluída: TCC_DanielCavalli_ABNT2.rtf"; \
		echo "📌 O formato RTF pode ser aberto no Word e geralmente preserva melhor a formatação."; \
	else \
		echo "❌ Erro: pandoc não está instalado. Instale com:"; \
		echo "   macOS: brew install pandoc"; \
		echo "   Ubuntu/Debian: sudo apt-get install pandoc"; \
		echo "   Fedora: sudo dnf install pandoc"; \
	fi

docx-preserveformat:
	@echo "📝 Convertendo TCC para DOCX com formatação preservada..."
	@echo "🔧 Passo 1: Compilando PDF se necessário..."
	@if ! [ -f "TCC_DanielCavalli_ABNT2.pdf" ]; then \
		$(MAKE) thesis; \
	fi
	@echo "🔧 Passo 2: Criando ODT intermediário..."
	@if command -v pandoc >/dev/null 2>&1; then \
		pandoc TCC_DanielCavalli_ABNT2.tex \
			--from=latex \
			--to=odt \
			--output=TCC_DanielCavalli_ABNT2_temp.odt \
			--reference-doc=default \
			--pdf-engine=xelatex \
			--variable mainfont="Times New Roman" \
			--variable fontsize=12pt \
			--variable linestretch=1.5 \
			--variable geometry:a4paper \
			--variable geometry:margin=3cm \
			--variable geometry:left=3cm \
			--variable geometry:right=2cm \
			--variable geometry:top=3cm \
			--variable geometry:bottom=2cm \
			--bibliography=referencias.bib \
			--citeproc \
			--metadata lang=pt-BR \
			2>/dev/null || echo "⚠️  Aviso: Algumas opções podem não ter sido aplicadas."; \
		echo "🔧 Passo 3: Convertendo ODT para DOCX..."; \
		pandoc TCC_DanielCavalli_ABNT2_temp.odt \
			--from=odt \
			--to=docx \
			--output=TCC_DanielCavalli_ABNT2_formatted.docx \
			--reference-doc=default \
			2>/dev/null; \
		rm -f TCC_DanielCavalli_ABNT2_temp.odt; \
		echo "✅ Conversão concluída: TCC_DanielCavalli_ABNT2_formatted.docx"; \
		echo "📌 Esta versão preserva melhor a formatação ABNT."; \
	else \
		echo "❌ Erro: pandoc não está instalado."; \
	fi

docx-libreoffice:
	@echo "📝 Convertendo PDF para DOCX via LibreOffice..."
	@if ! [ -f "TCC_DanielCavalli_ABNT2.pdf" ]; then \
		echo "⚠️  PDF não encontrado. Compilando primeiro..."; \
		$(MAKE) thesis; \
	fi
	@if command -v soffice >/dev/null 2>&1; then \
		echo "🔄 Convertendo com LibreOffice..."; \
		soffice --headless --convert-to docx:"MS Word 2007 XML" \
			--outdir . TCC_DanielCavalli_ABNT2.pdf; \
		mv -f TCC_DanielCavalli_ABNT2.docx TCC_DanielCavalli_ABNT2_libreoffice.docx 2>/dev/null || true; \
		echo "✅ Conversão concluída: TCC_DanielCavalli_ABNT2_libreoffice.docx"; \
		echo "📌 LibreOffice geralmente preserva melhor o layout visual do PDF."; \
	elif command -v /Applications/LibreOffice.app/Contents/MacOS/soffice >/dev/null 2>&1; then \
		echo "🔄 Convertendo com LibreOffice (macOS)..."; \
		/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to docx:"MS Word 2007 XML" \
			--outdir . TCC_DanielCavalli_ABNT2.pdf; \
		mv -f TCC_DanielCavalli_ABNT2.docx TCC_DanielCavalli_ABNT2_libreoffice.docx 2>/dev/null || true; \
		echo "✅ Conversão concluída: TCC_DanielCavalli_ABNT2_libreoffice.docx"; \
		echo "📌 LibreOffice geralmente preserva melhor o layout visual do PDF."; \
	else \
		echo "❌ Erro: LibreOffice não está instalado."; \
		echo "   macOS: brew install --cask libreoffice"; \
		echo "   Ubuntu/Debian: sudo apt-get install libreoffice"; \
		echo "   Fedora: sudo dnf install libreoffice"; \
	fi

docx-tex4ht:
	@echo "📝 Convertendo LaTeX para DOCX via tex4ht..."
	@if command -v make4ht >/dev/null 2>&1; then \
		echo "🔄 Convertendo com make4ht..."; \
		make4ht -f odt TCC_DanielCavalli_ABNT2.tex 2>/dev/null || { \
			echo "⚠️  make4ht falhou. Tentando htlatex..."; \
			htlatex TCC_DanielCavalli_ABNT2.tex "xhtml,ooffice" "ooffice/! -cmozhtf" "-coo" 2>/dev/null || \
			echo "❌ Conversão falhou."; \
		}; \
		if [ -f "TCC_DanielCavalli_ABNT2.odt" ]; then \
			echo "🔧 Convertendo ODT para DOCX..."; \
			if command -v pandoc >/dev/null 2>&1; then \
				pandoc TCC_DanielCavalli_ABNT2.odt -o TCC_DanielCavalli_ABNT2_tex4ht.docx; \
				rm -f TCC_DanielCavalli_ABNT2.odt TCC_DanielCavalli_ABNT2.html TCC_DanielCavalli_ABNT2*.png; \
				echo "✅ Conversão concluída: TCC_DanielCavalli_ABNT2_tex4ht.docx"; \
			else \
				mv TCC_DanielCavalli_ABNT2.odt TCC_DanielCavalli_ABNT2_tex4ht.odt; \
				echo "✅ Arquivo ODT gerado: TCC_DanielCavalli_ABNT2_tex4ht.odt"; \
				echo "📌 Use o LibreOffice para converter ODT para DOCX."; \
			fi; \
		fi; \
	else \
		echo "❌ Erro: tex4ht/make4ht não está instalado."; \
		echo "   macOS: brew install --cask mactex (inclui tex4ht)"; \
		echo "   Ubuntu/Debian: sudo apt-get install tex4ht"; \
		echo "   Fedora: sudo dnf install tex4ht"; \
	fi

docx-python:
	@echo "📝 Convertendo PDF para DOCX via Python (melhor preservação de formatação)..."
	@if ! [ -f "TCC_DanielCavalli_ABNT2.pdf" ]; then \
		echo "⚠️  PDF não encontrado. Compilando primeiro..."; \
		$(MAKE) thesis; \
	fi
	@if command -v python3 >/dev/null 2>&1; then \
		echo "🔧 Verificando dependências Python..."; \
		python3 -c "import pdf2docx, PyPDF2" 2>/dev/null || { \
			echo "📦 Instalando bibliotecas necessárias..."; \
			pip3 install pdf2docx PyPDF2 --user --quiet || pip install pdf2docx PyPDF2 --user --quiet; \
		}; \
		echo "🔄 Convertendo com pdf2docx..."; \
		python3 pdf_to_docx.py TCC_DanielCavalli_ABNT2.pdf TCC_DanielCavalli_ABNT2_python.docx; \
	elif command -v python >/dev/null 2>&1; then \
		echo "🔧 Verificando dependências Python..."; \
		python -c "import pdf2docx, PyPDF2" 2>/dev/null || { \
			echo "📦 Instalando bibliotecas necessárias..."; \
			pip install pdf2docx PyPDF2 --user --quiet; \
		}; \
		echo "🔄 Convertendo com pdf2docx..."; \
		python pdf_to_docx.py TCC_DanielCavalli_ABNT2.pdf TCC_DanielCavalli_ABNT2_python.docx; \
	else \
		echo "❌ Erro: Python não está instalado."; \
		echo "   macOS: brew install python3"; \
		echo "   Ubuntu/Debian: sudo apt-get install python3 python3-pip"; \
		echo "   Fedora: sudo dnf install python3 python3-pip"; \
	fi

docx-best:
	@echo "⭐ Iniciando melhor opção de conversão PDF→DOCX..."
	@if ! [ -f "TCC_DanielCavalli_ABNT2.pdf" ]; then \
		echo "⚠️  PDF não encontrado. Compilando primeiro..."; \
		$(MAKE) thesis; \
	fi
	@./convert_pdf_docx_alternative.sh

clean:
	@echo "🧹 Limpando arquivos auxiliares..."
	@rm -f *.aux *.log *.out *.toc *.lof *.lot *.bbl *.blg *.idx *.ilg *.ind
	@rm -f *.synctex.gz *.fdb_latexmk *.fls *.nav *.snm *.vrb
	@rm -f *.4ct *.4tc *.dvi *.idv *.lg *.tmp *.xref  # arquivos do tex4ht
	@rm -f TCC_DanielCavalli_ABNT2_temp.odt  # arquivo temporário
	@echo "✅ Limpeza concluída"