#!/bin/bash
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --job-name=mech-tests
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/mechanical_pass/logs/tests_%j.log
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "START"
Rscript -e '
suppressMessages({
  library(tidyverse); library(magrittr); library(yaml); library(data.table)
})
TAX_LAW_ROOT = file.path("config", "scenarios", "tax_law")
source("src/data/tax_law.R")
source("src/sim/payroll.R")
source("other/mechanical_pass/test_er_payroll_predicate.R")
' 2>&1
echo "EXIT=$?"
