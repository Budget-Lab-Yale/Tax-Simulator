#!/bin/bash
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=0:05:00
#SBATCH --output=other/kg_model_tests/check_meff_%j.out

module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/check_meff.R
