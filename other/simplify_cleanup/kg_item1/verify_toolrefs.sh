#!/bin/bash
#SBATCH --job-name=kg_toolrefs
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/kg_item1/logs/toolrefs_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

Rscript -e '
  # 1. Both edited scripts must parse
  for (f in c("other/kg_model_tests/calibrate.R",
              "other/kg_model_tests/test_terminal_charity.R")) {
    parse(f); cat("PARSE_OK:", f, "\n")
  }

  # 2. Sourcing the three src files (as the edited scripts now do) must make the
  #    previously-dangling symbols resolve.
  suppressPackageStartupMessages({library(tidyverse); library(data.table); library(magrittr)})
  source("./src/calc/functions/tax/estate.R")
  source("./src/sim/cohort_bathtub.R")
  for (f in sort(list.files("./src/sim/kg", full.names = TRUE))) source(f)

  stopifnot(exists("ESTATE_ASSET_COLS"), exists("build_aging_matrix"))
  cat("SYMBOLS_RESOLVE: ESTATE_ASSET_COLS (n=", length(ESTATE_ASSET_COLS),
      "), build_aging_matrix\n", sep = "")

  # 3. build_aging_matrix must actually run on the kg grid the tool uses
  A = build_aging_matrix(KG_DYN_AGE_MIN:KG_DYN_AGE_MAX)
  stopifnot(is.matrix(A), nrow(A) == length(KG_DYN_AGE_MIN:KG_DYN_AGE_MAX),
            A[nrow(A), ncol(A)] == 1)
  cat("AGING_MATRIX_OK: dim", nrow(A), "x", ncol(A), "\n")

  cat("ALL_TOOLREF_CHECKS_PASS\n")
'
echo "TOOLREF_EXIT=$?"
