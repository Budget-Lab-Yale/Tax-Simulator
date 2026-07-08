#!/bin/bash
#-----------------------------------------------------------------------
# check_hidden_ledger.sbatch
#
# SLURM wrapper for other/top_tax/tests/check_hidden_ledger.R: post-hoc
# checks (a)-(e) on a hidden_ledger_smoke run pair (CHI-central vs CHI=0).
#
# Submit from repo root, forwarding the five positional args to the R script:
#   sbatch other/top_tax/tests/check_hidden_ledger.sbatch \
#     <root_central> <root_chi0> <scenario_id> <first_year> <last_year>
#-----------------------------------------------------------------------
#SBATCH --job-name=hl_check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=0:20:00
#SBATCH --output=other/top_tax/tests/check_hidden_ledger_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/top_tax/tests/check_hidden_ledger.R "$@"
