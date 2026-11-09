#!/bin/bash
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:20:00
#SBATCH --mem=8G
#SBATCH --job-name=memo-pdf
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/mechanical_pass/logs/memo_pdf_%j.log
module load texlive/20240312-GCC-13.3.0
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax
echo "START"
pdflatex -interaction=nonstopmode -halt-on-error methodology_memo.tex >/dev/null 2>&1
echo "PASS1=$?"
pdflatex -interaction=nonstopmode -halt-on-error methodology_memo.tex >/dev/null 2>&1
echo "PASS2=$?"
grep -c "^!" methodology_memo.log 2>/dev/null || true
grep -A3 "^!" methodology_memo.log 2>/dev/null | head -20
ls -l methodology_memo.pdf
echo "DONE"
