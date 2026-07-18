#!/bin/bash
#SBATCH --job-name=acc_var
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=12G
#SBATCH --time=0:30:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/report_prep/accrual_variance_%j.out

set -euo pipefail
python3 /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/report_prep/accrual_variance_check.py
