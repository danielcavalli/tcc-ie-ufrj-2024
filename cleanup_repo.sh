#!/bin/bash
# Script para limpar arquivos desnecessários do repositório

echo "🧹 Iniciando limpeza do repositório..."

# Remover arquivos auxiliares do LaTeX
echo "📄 Removendo arquivos auxiliares do LaTeX..."
rm -f documents/drafts/latex_output/*.aux
rm -f documents/drafts/latex_output/*.log
rm -f documents/drafts/latex_output/*.toc
rm -f documents/drafts/latex_output/*.lof
rm -f documents/drafts/latex_output/*.lot
rm -f documents/drafts/latex_output/*.bbl
rm -f documents/drafts/latex_output/*.blg
rm -f documents/drafts/latex_output/*.idx
rm -f documents/drafts/latex_output/*.brf
rm -f documents/drafts/latex_output/*.out
rm -f documents/drafts/latex_output/*.nav
rm -f documents/drafts/latex_output/*.snm

# Remover arquivos HTML temporários
echo "🌐 Removendo arquivos HTML temporários..."
rm -f data/outputs/additional_figures/*.html
rm -f data/outputs/descriptive_analysis/*.html
rm -f data/outputs/presentation/*.html

# Remover arquivos .md temporários em data/outputs
echo "📝 Removendo arquivos .md temporários..."
rm -f data/outputs/*.md
rm -f data/outputs/presentation/*.md
rm -f data/outputs/additional_figures/*.md
rm -f data/outputs/descriptive_analysis/*.md
rm -f data/documents/drafts/latex_output/*.md

# Remover Rplots.pdf se existir
echo "📊 Removendo Rplots.pdf..."
rm -f Rplots.pdf

# Remover arquivos docx temporários
echo "📄 Removendo arquivos .docx temporários..."
rm -f documents/drafts/latex_output/*.docx

# Remover diretórios vazios
echo "📁 Removendo diretórios vazios..."
find . -type d -empty -delete 2>/dev/null

echo "✅ Limpeza concluída!"
echo ""
echo "📊 Estatísticas da limpeza:"
echo "- Arquivos LaTeX auxiliares removidos"
echo "- Arquivos HTML temporários removidos"
echo "- Arquivos .md temporários removidos"
echo "- Rplots.pdf removido (se existia)"
echo "- Arquivos .docx temporários removidos"
echo ""
echo "💡 Dica: Atualize o .gitignore para evitar que esses arquivos sejam commitados novamente."
