#!/bin/bash
#SBATCH --job-name=dist_etrs_other
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/dist_etrs_other_%j.out
#SBATCH --time=02:00:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=2

# Regenerate the corp_perm distribution_etrs.csv on the NEW Tax-Data vintage
# (default now 2026070814) so it carries the standalone `other` tier, for the
# tax_type_etr_cash_vs_accrual.html chart. Same config as dist_etrs_smoke
# (baseline + corp_perm, corp_test OME, dist_years=2028), fresh output vintage.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript src/main.R tests/dist_etrs_other_smoke NULL user_test 1 dist_etrs_other_v1 1 0 NULL 0 none
echo "=== main.R exit: $? ==="
