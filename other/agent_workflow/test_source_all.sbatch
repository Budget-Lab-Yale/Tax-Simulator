#!/bin/bash
#SBATCH -J src_test
#SBATCH -p day
#SBATCH -c 1
#SBATCH --mem=8G
#SBATCH -t 0:20:00
#SBATCH -o /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/agent_workflow/test_source_all.log

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/agent_workflow/test_source_all.R
