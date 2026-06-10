#!/bin/bash
#SBATCH --time=0:05:00
#SBATCH --mem=4G
#SBATCH --job-name=kg-test
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/test_planned_timing.R
