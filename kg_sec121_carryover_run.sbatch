#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=kg-s121-carry
#SBATCH --output=kg_sec121_carryover_%j.log
# Phase 0 setup + array-chain submission for the §121-under-carryover test.
# Runs baseline + all-carryover (§121 on), all-carryover (§121 off / cap=0),
# home-carryover-others-deemed, and all-deemed. Full sample per kg_dynamics
# regression convention. Output vintage fixed so estimates are easy to locate.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh tests/kg_dyn_sec121_carryover NULL user_test 1 sec121_carryover 1 1 NULL 0
