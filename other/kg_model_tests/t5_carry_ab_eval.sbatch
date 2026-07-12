#!/bin/bash
#SBATCH --job-name=t5_eval
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=48G
#SBATCH --time=1:00:00
#SBATCH --output=other/kg_model_tests/t5_carry_ab_eval_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/t5_carry_ab_eval.R
