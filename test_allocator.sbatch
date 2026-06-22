#!/bin/bash
#SBATCH --job-name=test_allocator
#SBATCH --partition=day
#SBATCH --time=00:10:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
#SBATCH --output=logs/test_allocator_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

# Syntax-check the two modified/new source files, then run unit tests
Rscript -e 'invisible(parse("src/data/post_processing/estate_allocator.R")); invisible(parse("src/data/post_processing/distribution.R")); cat("parse OK\n")'
TAXSIM_ROOT=$(pwd) Rscript other/estate_tax/test_allocator.R
