#!/bin/bash
#SBATCH --job-name=ot-cost-behav
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/ot_cost_behav_%j.log
#SBATCH --time=00:15:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=2

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/ot_deduction/ot_cost_behavioral_2026.R
