#!/bin/bash
#SBATCH --job-name=table1_v5
#SBATCH --partition=day
#SBATCH --time=1:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/report_prep/table1_v5_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/report_prep
echo "=============== hundred_pct_ceiling.py (100% ceiling framing) ==============="
python3 hundred_pct_ceiling.py
echo
echo "=============== mtr_table_data.py (rate-needed framing) ==============="
python3 mtr_table_data.py
