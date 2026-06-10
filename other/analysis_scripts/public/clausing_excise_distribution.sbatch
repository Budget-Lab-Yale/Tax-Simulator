#!/bin/bash
#SBATCH --job-name=clausing_excise_dist
#SBATCH --output=other/analysis_scripts/public/clausing_excise_distribution_%j.out
#SBATCH --time=01:30:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=1

module load R/4.4.1-foss-2022b 2>/dev/null || module load R 2>/dev/null || true

Rscript other/analysis_scripts/public/clausing_excise_distribution.R
