#!/bin/bash
#SBATCH --job-name=t6_smear
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --output=other/kg_model_tests/t6_smear_benchmark_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/t6_smear_benchmark.R
