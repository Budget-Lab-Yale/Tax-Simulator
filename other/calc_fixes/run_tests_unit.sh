#!/bin/bash
#SBATCH --job-name=calcfix_unit
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/calc_fixes/logs/unit_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/calc_fixes/tests_unit.R
echo "UNIT_TEST_EXIT=$?"
