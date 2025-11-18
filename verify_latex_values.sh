#!/bin/bash
# Script para verificar se auto_values.tex está sincronizado com os CSVs

echo "=== Verificação de Sincronização de Valores LaTeX ==="
echo ""
echo "1. Timestamp dos arquivos:"
echo "   CSV placebo:    $(stat -f "%Sm" -t "%Y-%m-%d %H:%M:%S" data/outputs/placebo_random_summary.csv)"
echo "   CSV main:       $(stat -f "%Sm" -t "%Y-%m-%d %H:%M:%S" data/outputs/att_summary_main_valor_cana.csv)"
echo "   latex_values:   $(stat -f "%Sm" -t "%Y-%m-%d %H:%M:%S" data/outputs/latex_values.tex)"
echo "   auto_values:    $(stat -f "%Sm" -t "%Y-%m-%d %H:%M:%S" documents/drafts/latex_output/auto_values.tex)"
echo ""

echo "2. Valores do Placebo:"
echo "   CSV:"
PLACEBO_CSV=$(tail -1 data/outputs/placebo_random_summary.csv | cut -d',' -f1,2,11)
echo "      ATT: $(echo $PLACEBO_CSV | cut -d',' -f1)"
echo "      p-value: $(echo $PLACEBO_CSV | cut -d',' -f2)"
echo "      n_sims: $(echo $PLACEBO_CSV | cut -d',' -f3)"
echo ""
echo "   LaTeX (data/outputs/latex_values.tex usado pelo documento):"
grep "placebotruatt" data/outputs/latex_values.tex | head -1
grep "placebopvalue" data/outputs/latex_values.tex | head -1
grep "placebonsims" data/outputs/latex_values.tex

echo ""
echo "3. Valores Principais:"
echo "   CSV:"
MAIN_CSV=$(tail -1 data/outputs/att_summary_main_valor_cana.csv | cut -d',' -f4,5,7)
echo "      ATT: $(echo $MAIN_CSV | cut -d',' -f1)"
echo "      SE: $(echo $MAIN_CSV | cut -d',' -f2)"
echo "      p-value: $(echo $MAIN_CSV | cut -d',' -f3)"
echo ""
echo "   LaTeX (data/outputs/latex_values.tex usado pelo documento):"
grep "mainatt" data/outputs/latex_values.tex | head -1
grep "mainse" data/outputs/latex_values.tex | head -1
grep "mainp" data/outputs/latex_values.tex | head -1
grep "mainattpct" data/outputs/latex_values.tex | head -1

echo ""
echo "=== Verificação Concluída ==="
