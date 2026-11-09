#!/bin/bash
#SBATCH --job-name=ot-benefit-agi
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/ot_analysis_%j.log
#SBATCH --time=00:15:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=2

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/ot_deduction/ot_benefit_by_agi.R
