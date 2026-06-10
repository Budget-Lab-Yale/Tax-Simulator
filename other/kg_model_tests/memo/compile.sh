#!/usr/bin/env bash
# Build memo.pdf from memo.tex. Run twice for cross-refs.
# Requires the Yale-cluster texlive module.
set -euo pipefail
cd "$(dirname "$0")"
if ! command -v pdflatex >/dev/null 2>&1; then
  for init in /apps/custom/lmod/*/init/bash; do source "$init" && break; done
  module load texlive/20240312-GCC-13.3.0
fi
pdflatex -interaction=nonstopmode -halt-on-error memo.tex
pdflatex -interaction=nonstopmode -halt-on-error memo.tex
echo "wrote $(pwd)/memo.pdf"
