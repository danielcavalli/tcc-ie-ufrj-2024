#!/bin/bash
# Script alternativo para conversão PDF para DOCX com formatação preservada

echo "📝 Guia de conversão PDF para DOCX com formatação preservada"
echo "============================================================"
echo ""
echo "Como o pip está com problemas no sistema, aqui estão alternativas:"
echo ""

# Verifica se o LibreOffice está instalado
if command -v soffice >/dev/null 2>&1 || command -v /Applications/LibreOffice.app/Contents/MacOS/soffice >/dev/null 2>&1; then
    echo "✅ LibreOffice detectado! Use:"
    echo "   make docx-libreoffice"
    echo ""
fi

echo "🌐 OPÇÕES ONLINE (Preservam melhor a formatação):"
echo ""
echo "1. Adobe Acrobat Online (MELHOR OPÇÃO):"
echo "   • URL: https://www.adobe.com/acrobat/online/pdf-to-word.html"
echo "   • Gratuito para arquivos até 100MB"
echo "   • Preserva formatação ABNT, margens, fontes e estilos"
echo ""
echo "2. SmallPDF:"
echo "   • URL: https://smallpdf.com/pdf-to-word"
echo "   • 2 conversões gratuitas por dia"
echo "   • Boa preservação de layout"
echo ""
echo "3. ILovePDF:"
echo "   • URL: https://www.ilovepdf.com/pdf_to_word"
echo "   • Gratuito com limitações"
echo "   • Mantém formatação básica"
echo ""
echo "4. Microsoft Word Online:"
echo "   • Abra o Word Online (Office 365)"
echo "   • Use: Arquivo > Abrir > Procurar > Selecione o PDF"
echo "   • Word converte automaticamente mantendo formatação"
echo ""

echo "🖥️  OPÇÕES DESKTOP:"
echo ""

# Verifica se é macOS
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "1. Preview + Pages (macOS):"
    echo "   • Abra o PDF no Preview"
    echo "   • Selecione todo o texto (Cmd+A)"
    echo "   • Copie (Cmd+C)"
    echo "   • Abra o Pages e cole"
    echo "   • Salve como DOCX"
    echo ""
fi

echo "2. Google Docs:"
echo "   • Faça upload do PDF para o Google Drive"
echo "   • Clique com botão direito > Abrir com > Google Docs"
echo "   • Arquivo > Download > Microsoft Word (.docx)"
echo ""

echo "📌 RECOMENDAÇÃO:"
echo "Para melhor resultado com formatação ABNT preservada:"
echo "1. Use Adobe Acrobat Online (link acima)"
echo "2. Ou instale o LibreOffice e use: make docx-libreoffice"
echo ""

# Se quiser abrir automaticamente o navegador na página do Adobe
read -p "Deseja abrir o Adobe Acrobat Online agora? (s/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Ss]$ ]]; then
    if command -v open >/dev/null 2>&1; then
        open "https://www.adobe.com/acrobat/online/pdf-to-word.html"
    elif command -v xdg-open >/dev/null 2>&1; then
        xdg-open "https://www.adobe.com/acrobat/online/pdf-to-word.html"
    else
        echo "Abra manualmente: https://www.adobe.com/acrobat/online/pdf-to-word.html"
    fi
fi
