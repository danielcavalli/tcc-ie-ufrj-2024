#!/usr/bin/env python3
"""
Script para converter PDF para DOCX preservando formatação
Usa pdf2docx que mantém layout, fontes e estilos
"""

import sys
import os
from pathlib import Path

try:
    from pdf2docx import Converter
    from PyPDF2 import PdfReader
except ImportError:
    print("❌ Erro: Bibliotecas necessárias não instaladas.")
    print("   Execute: pip install pdf2docx PyPDF2")
    print("   ou: pip3 install pdf2docx PyPDF2")
    sys.exit(1)


def convert_pdf_to_docx(pdf_path, docx_path=None):
    """
    Converte PDF para DOCX preservando formatação
    """
    pdf_file = Path(pdf_path)

    if not pdf_file.exists():
        print(f"❌ Erro: Arquivo PDF não encontrado: {pdf_path}")
        return False

    if docx_path is None:
        docx_path = pdf_file.with_suffix(".docx")

    print(f"📄 Convertendo: {pdf_file.name}")
    print(f"📝 Para: {Path(docx_path).name}")

    try:
        # Verifica número de páginas
        with open(pdf_path, "rb") as file:
            pdf = PdfReader(file)
            num_pages = len(pdf.pages)
            print(f"📊 Total de páginas: {num_pages}")

        # Cria o conversor
        cv = Converter(str(pdf_path))

        # Converte com opções para preservar formatação
        print("🔄 Convertendo... (isso pode levar alguns minutos)")
        cv.convert(str(docx_path), start=0, end=None)
        cv.close()

        # Verifica se o arquivo foi criado
        if Path(docx_path).exists():
            size_mb = Path(docx_path).stat().st_size / (1024 * 1024)
            print(f"✅ Conversão concluída!")
            print(f"📁 Arquivo gerado: {Path(docx_path).name} ({size_mb:.1f} MB)")
            print(f"📌 Este método preserva:")
            print("   • Layout e quebras de página")
            print("   • Fontes e tamanhos")
            print("   • Espaçamentos e margens")
            print("   • Tabelas e listas")
            print("   • Imagens e gráficos")
            return True
        else:
            print("❌ Erro: Arquivo DOCX não foi criado")
            return False

    except Exception as e:
        print(f"❌ Erro durante conversão: {str(e)}")
        return False


def main():
    """Função principal"""
    if len(sys.argv) < 2:
        pdf_path = "TCC_DanielCavalli_ABNT2.pdf"
        docx_path = "TCC_DanielCavalli_ABNT2_preserved.docx"
    else:
        pdf_path = sys.argv[1]
        docx_path = sys.argv[2] if len(sys.argv) > 2 else None

    success = convert_pdf_to_docx(pdf_path, docx_path)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
