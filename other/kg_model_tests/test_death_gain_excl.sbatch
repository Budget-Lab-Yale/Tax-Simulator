#!/bin/bash
#SBATCH --job-name=test_death_gain_excl
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=0:30:00
#SBATCH --output=other/kg_model_tests/test_death_gain_excl_%j.out
set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/test_death_gain_excl.R
