#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 03_gq_backfill.R  (group C, stage C5 -- run BEFORE the calibration: the
#                    anchor includes group quarters and the ASEC frame does
#                    not, so C4's accounting identity needs these units first)
#
# Extract group-quarters persons from the ACS, classify them with the
# production classify_gq() (B1), apply the filing test, and score filing under
# the STATED ASSUMPTION below -- Mok's coefficients are estimated on a
# household frame and may not be scored on GQ records (D-A7).
#
# The stated assumption, in full:
#   * dorm students (GQ 4, in school, 18-24) BELOW the threshold: claimed
#     dependents on parents' returns -- they are the DORM NETTING, never pool
#     units. ABOVE the threshold they are filers heading their own units,
#     exactly B1's rule ("unless income makes them filers") -- the committed
#     anchor netting (residual_anchors' dorm_dependents) counts only the
#     below-threshold students, and 04 asserts the two sides agree.
#   * institutional (GQ 3), below threshold: do not file (p_file = 0).
#     Prisons and nursing homes; the design memo calls this population's
#     filing essentially nil and F6 sized it.
#   * other noninstitutional (GQ 4 non-student, incl. military barracks),
#     below threshold: file at the ASEC household pool's SCORED rate for
#     unmarried no-dependent units of the same age side (u65/65p) -- a RATE
#     transplant, not a covariate transplant, which is what keeps Mok's
#     equations off the GQ frame.
#   * above threshold (any class): filers (p_file = 1). The Pub 5785 hazard
#     stays on the household side for v1.
# The income test mirrors the household side: gross proxy = INCTOT - INCSS
# against the single threshold (65+ variant), plus the $400 SE rule (INCBUS00).
#
# Writes: results/gq_persons_{tax_year}.csv.gz (record level, C7's input) and
#         results/gq_backfill_summary_{tax_year}.csv (class x band, C4's input)
#
# NEEDS SBATCH (the ACS read OOMs the login node ~5G cap). Submit from the
# repo root:
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/nonfiler_pool/run_gq_backfill.sbatch [tax_year]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml); library(readr)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')
source('src/data/filing_model.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES   <- 'research/state_weights/nonfiler_pool/results'

ACS_COLS <- c('YEAR', 'STATEFIP', 'PERWT', 'AGE', 'SEX', 'GQ', 'SCHOOL',
              'INCTOT', 'INCWAGE', 'INCBUS00', 'INCSS', 'INCINVST',
              'INCRETIR', 'INCWELFR', 'INCSUPP', 'INCOTHER')

#' Blank IPUMS USA income sentinels to 0 using each variable's field width
#' from variables.csv: NIU = 10^w - 1, missing = 10^w - 2 (the convention
#' read_acs_extract() already applies to INCTOT). Asserted, never assumed:
#' any blanked value must equal exactly one of the two codes.
clean_acs_income <- function(a, acs_year, vars) {
  v <- fread(file.path(acs_extract_dir(acs_year), 'variables.csv'))
  for (nm in vars) {
    w <- v[var_name == nm, end - start + 1]
    stopifnot(length(w) == 1)
    cut <- 10^w - 2
    z <- a[[nm]]
    bad <- !is.na(z) & z >= cut
    stopifnot(all(z[bad] %in% c(10^w - 1, 10^w - 2)))
    z[bad] <- 0
    data.table::set(a, j = nm, value = z)
  }
  a
}

for (yr in YEARS) {
  message('=== TY', yr, ' (ACS ', yr, ')')

  a <- read_acs_extract(yr, cols = ACS_COLS)
  a <- clean_acs_income(a, yr, setdiff(ACS_COLS, c('YEAR', 'STATEFIP', 'PERWT',
                                                   'AGE', 'SEX', 'GQ', 'SCHOOL',
                                                   'INCTOT')))
  a[is.na(INCTOT), INCTOT := 0]   # read_acs_extract blanks INCTOT's sentinel to NA

  a[, gq_class := classify_gq(GQ, SCHOOL, AGE)]
  gq <- a[gq_class != 'household']
  rm(a); gc()

  gq[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  stopifnot(!anyNA(gq$state))
  gq[, band := fifelse(AGE >= 18, as.character(age_band(AGE)), 'u18')]

  # Filing test, mirroring the household side's concepts
  fp <- filing_requirement_params(yr)
  gq[, gross_proxy := INCTOT - INCSS]
  gq[, must_file := gross_proxy >= fifelse(AGE >= 65, fp$single_65, fp$single) |
                    abs(INCBUS00) >= SE_FILING_FLOOR]

  # The rate transplant for other-noninstitutional: the ASEC pool's scored
  # below-threshold filing rate, unmarried no-dependent units by age side
  sc <- readRDS(file.path(RES, sprintf('scored_units_%d.rds', yr)))
  rate <- sc[unit_type == 'nondependent' & filing_status != 'joint' &
             n_dep == 0 & must_file == FALSE,
             .(p = sum(weight * p_file_mok) / sum(weight)),
             by = .(side = fifelse(age_head >= 65, '65p', 'u65'))]
  p_u65 <- rate[side == 'u65', p]; p_65p <- rate[side == '65p', p]
  message(sprintf('  rate transplant (below-threshold unmarried dep0): u65 %.3f, 65p %.3f',
                  p_u65, p_65p))

  gq[, p_file := fcase(
    gq_class == 'dorm_student' & must_file == FALSE, NA_real_,  # claimed dependents
    must_file == TRUE, 1,                       # incl. above-threshold students
    gq_class == 'institutional', 0,
    AGE >= 65, p_65p,
    default = p_u65)]
  gq[, is_dorm_dependent := gq_class == 'dorm_student' & must_file == FALSE]

  fwrite(gq, file.path(RES, sprintf('gq_persons_%d.csv.gz', yr)))

  summ <- gq[AGE >= 18, .(
    persons = sum(PERWT),
    nonfiling_adults = sum(PERWT * fifelse(is.na(p_file), 0, 1 - p_file)),
    filers = sum(PERWT * fifelse(is.na(p_file), 0, p_file)),
    dorm_dependents = sum(PERWT * is_dorm_dependent)
  ), keyby = .(gq_class, band)]
  fwrite(summ, file.path(RES, sprintf('gq_backfill_summary_%d.csv', yr)))

  u18 <- gq[AGE < 18, sum(PERWT)]
  message(sprintf(paste('  GQ adults: %.2fM persons -> %.2fM non-filing units,',
                        '%.2fM filers, %.2fM dorm dependents | %.2fM under-18',
                        'reported, outside the 18+ anchor'),
                  summ[, sum(persons)] / 1e6,
                  summ[, sum(nonfiling_adults)] / 1e6,
                  summ[, sum(filers)] / 1e6,
                  summ[, sum(dorm_dependents)] / 1e6,
                  u18 / 1e6))
  message('  wrote gq_persons_', yr, '.csv.gz, gq_backfill_summary_', yr, '.csv')
}
