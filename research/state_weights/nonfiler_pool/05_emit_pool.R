#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 05_emit_pool.R  (group C, stage C7 -- plan: research/state_weights/plan.md §3
#                  step 6: "emit in the PUF schema")
#
# Emit the calibrated non-filer pool as the file Tax-Data's impute_nonfilers.R
# will read in place of the DINA append (C9). Two parts, one output:
#
#   * HOUSEHOLD units (calibrated_units, 04): every nondependent unit at its
#     EXPECTED WEIGHT, weight x (1 - p_file_cal). Deliberately not a random
#     draw: expected weights are deterministic, preserve the calibrated band
#     totals exactly, and remove the D4 re-randomization hazard from selection
#     entirely (run.R binds precomputed random numbers positionally, so a
#     draw-based builder would re-randomize downstream every time it changed).
#   * GQ units (gq_persons, 03): single-person units at PERWT x (1 - p_file).
#     Dorm dependents (p_file NA) are claimed on parents' returns and are
#     never emitted.
#
# SCHEMA CONTRACT. The file carries every column impute_nonfilers.R constructs
# today (same names, same conventions -- txbl_int "assume all taxable",
# qual_div "assume all qualified", capped 3 dependent slots) plus what the
# replacement OBSERVES rather than imputes: GENDER from ASEC/ACS sex (1 male,
# 2 female -- the PUF coding), male1/male2, real ages, wages2, sole_prop2,
# rent sign-split, other_inc, and dependent ages mapped to the PUF AGEDP
# coding. Zero-filling the REMAINING mid-pipeline columns stays in the READER
# (C9), exactly as impute_nonfilers.R does today -- only Tax-Data knows its
# live mid-pipeline column set, so the writer must not guess it. `filer = 0`
# is explicit here where the DINA append relied on the zero-fill to supply it.
#
# Capital gains: kg_lt = INCCAPG for TY2022; TY2017 has NO survey capital-
# gains item (S16) and emits zero -- the same value DINA's all-zero fikgi
# produced, so the consumed vintage loses nothing while the asymmetry stays
# recorded.
#
# Writes: results/nonfiler_pool_{year}.csv.gz, and publishes both years plus a
# README to the shared model_data store under ASEC-Nonfilers/v1/{vintage}
# (pass --publish; without it the store is untouched).
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/05_emit_pool.R [--publish] [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')
source('src/data/filing_model.R')

args    <- commandArgs(trailingOnly = TRUE)
PUBLISH <- '--publish' %in% args
YEARS   <- suppressWarnings(as.integer(setdiff(args, '--publish')))
if (!length(YEARS)) YEARS <- c(2017L, 2022L)
RES     <- 'research/state_weights/nonfiler_pool/results'
VINTAGE <- format(Sys.time(), '%Y%m%d01')

# PUF AGEDP dependent-age coding (PUF codebook; process_puf.R renames
# AGEDP1-3 -> dep_age_group1-3): 1 under 5, 2 = 5-12, 3 = 13-16, 4 = 17-18,
# 5 = 19-23, 6 = 24+. impute_nonfilers.R drew groups 1-4 from hard-coded
# probabilities; here they come from the constructed dependents' real ages.
dep_age_group <- function(age) {
  fcase(age < 5, 1L, age < 13, 2L, age < 17, 3L, age < 19, 4L,
        age < 24, 5L, default = 6L)
}
CTC_AGE_LIMIT  <- 17L   # qualifying child for the CTC: under 17
EITC_AGE_LIMIT <- 19L   # EITC qualifying child: under 19, or under 24 full-time

emit_totals <- list()

for (yr in YEARS) {
  message('=== TY', yr)

  u   <- readRDS(file.path(RES, sprintf('calibrated_units_%d.rds', yr)))
  st  <- readRDS(file.path(RES, sprintf('units_%d.rds', yr)))
  gqp <- fread(file.path(RES, sprintf('gq_persons_%d.csv.gz', yr)))
  cal <- fread(file.path(RES, sprintf('calibration_%d.csv', yr)))
  gqs <- fread(file.path(RES, sprintf('gq_backfill_summary_%d.csv', yr)))

  #---------------------------------------------------------------------------
  # Household side
  #---------------------------------------------------------------------------
  hh <- u[unit_type == 'nondependent' & age_head >= 18]
  hh[, weight_out := weight * (1 - p_file_cal)]
  hh <- hh[weight_out > 0]

  # Primary/spouse wage and self-employment splits from the person table
  ps <- st$persons[role %in% c('primary', 'spouse'),
                   .(unit_id, role, INCWAGE, se_income)]
  splits <- dcast(ps, unit_id ~ role,
                  value.var = c('INCWAGE', 'se_income'), fill = 0)
  hh <- merge(hh, splits, by = 'unit_id', all.x = TRUE, sort = FALSE)

  # Dependent detail from the constructed dependents: real ages (three PUF
  # slots, youngest first, mirroring the AGEDP convention), CTC/EITC counts
  # from age and full-time enrollment -- replacing impute_nonfilers.R's
  # "assume all age-qualifying dependents qualify" draw with observation
  deps <- st$persons[is_dependent == TRUE,
                     .(unit_id, AGE, ft = SCHLCOLL %in% SCHLCOLL_FULLTIME)]
  dep_info <- deps[order(unit_id, AGE),
                   .(dep_ages   = list(AGE),
                     n_dep_ctc  = sum(AGE < CTC_AGE_LIMIT),
                     n_dep_eitc = sum(AGE < EITC_AGE_LIMIT |
                                      (AGE < QC_STUDENT_AGE_LIMIT & ft))),
                   by = unit_id]
  hh <- merge(hh, dep_info, by = 'unit_id', all.x = TRUE, sort = FALSE)
  hh[is.na(n_dep_ctc),  n_dep_ctc := 0L]
  hh[is.na(n_dep_eitc), n_dep_eitc := 0L]

  slot <- function(ages, k) vapply(ages, function(a) {
    if (length(a) >= k) dep_age_group(a[k]) else NA_integer_ }, integer(1))

  hh_out <- hh[, .(
    weight        = weight_out,
    filer         = 0L,
    dep_status    = 0L,
    filing_status = fcase(filing_status == 'joint', 2L,
                          filing_status == 'hoh', 4L, default = 1L),
    # demographics: OBSERVED, not imputed (S14's filer=0 cells disappear here)
    GENDER    = as.integer(sex_head),           # PUF coding: 1 male, 2 female
    male1     = as.integer(sex_head == 1),
    male2     = fifelse(is.na(sex_spouse), NA_integer_,
                        as.integer(sex_spouse == 1)),
    age1      = as.integer(age_head),
    age2      = as.integer(age_spouse),
    age_group = fcase(age_head < 26, 1L, age_head < 35, 2L, age_head < 45, 3L,
                      age_head < 55, 4L, age_head < 65, 5L, default = 6L),
    EARNSPLIT = NA_integer_,
    # dependents: capped at the PUF's three slots, as impute_nonfilers.R does
    n_dep          = pmin(3L, n_dep),
    dep_age_group1 = slot(dep_ages, 1),
    dep_age_group2 = slot(dep_ages, 2),
    dep_age_group3 = slot(dep_ages, 3),
    n_dep_ctc      = pmin(3L, n_dep_ctc),
    n_dep_eitc     = pmin(3L, n_dep_eitc),
    # incomes, impute_nonfilers.R's names and conventions
    wages    = INCWAGE,
    wages1   = INCWAGE_primary,
    wages2   = INCWAGE_spouse,
    txbl_int = INCINT,                          # assume all taxable
    qual_div = INCDIVID,                        # assume all qualified
    sole_prop  = se_income,
    sole_prop1 = se_income_primary,
    sole_prop2 = se_income_spouse,
    kg_lt = fifelse(rep(yr >= 2018, .N),
                    pmax(gross_income_inc_kg - gross_income, 0), 0),
    gross_pens_dist = retirement_income,
    txbl_pens_dist  = retirement_income,
    rent      = pmax(INCRENT, 0),
    rent_loss = pmin(INCRENT, 0),
    ui        = INCUNEMP,
    gross_ss  = INCSS,
    other_inc = gross_income - INCWAGE - se_income - INCINT - INCDIVID -
                INCRENT - INCUNEMP - retirement_income,
    source    = 'asec_household'
  )]

  #---------------------------------------------------------------------------
  # GQ side
  #---------------------------------------------------------------------------
  gq <- gqp[!is.na(p_file) & p_file < 1 & AGE >= 18]
  gq_out <- gq[, .(
    weight        = PERWT * (1 - p_file),
    filer         = 0L,
    dep_status    = 0L,
    filing_status = 1L,
    GENDER    = as.integer(SEX),
    male1     = as.integer(SEX == 1),
    male2     = NA_integer_,
    age1      = as.integer(AGE),
    age2      = NA_integer_,
    age_group = fcase(AGE < 26, 1L, AGE < 35, 2L, AGE < 45, 3L,
                      AGE < 55, 4L, AGE < 65, 5L, default = 6L),
    EARNSPLIT = NA_integer_,
    n_dep = 0L, dep_age_group1 = NA_integer_, dep_age_group2 = NA_integer_,
    dep_age_group3 = NA_integer_, n_dep_ctc = 0L, n_dep_eitc = 0L,
    wages = INCWAGE, wages1 = INCWAGE, wages2 = 0,
    txbl_int = INCINVST,   # ACS fuses interest/dividends/rent; assigned whole
    qual_div = 0,          # to interest -- documented coarseness of the ACS side
    sole_prop = INCBUS00, sole_prop1 = INCBUS00, sole_prop2 = 0,
    kg_lt = 0,
    gross_pens_dist = INCRETIR, txbl_pens_dist = INCRETIR,
    rent = 0, rent_loss = 0,
    ui = 0,                # no separate ACS UI item; it sits inside INCOTHER
    gross_ss = INCSS,
    other_inc = INCOTHER,
    source = paste0('acs_', gq_class)
  )]
  gq_out <- gq_out[weight > 0]

  pool <- rbindlist(list(hh_out, gq_out), use.names = TRUE)
  pool[, id := 1e6 + .I]
  pool[, tax_year := yr]
  setcolorder(pool, c('id', 'tax_year', 'weight', 'filer'))

  #---------------------------------------------------------------------------
  # Gates: the emitted file must reproduce the calibration exactly
  #---------------------------------------------------------------------------
  emitted_hh_adults <- pool[source == 'asec_household',
                            sum(weight * (1 + (filing_status == 2)))]
  emitted_gq_adults <- pool[source != 'asec_household', sum(weight)]
  cal_total <- cal[, sum(achieved)]
  gq_total  <- gqs[band != 'u18', sum(nonfiling_adults)]
  stopifnot(abs(emitted_hh_adults - (cal_total - gq_total)) < 1,
            abs(emitted_gq_adults - gq_total) < 1)

  # No NA outside the deliberately-NA slots (spouse fields on non-joint,
  # dependent slots beyond n_dep, EARNSPLIT which the PUF imputes)
  na_ok <- c('age2', 'male2', 'dep_age_group1', 'dep_age_group2',
             'dep_age_group3', 'EARNSPLIT')
  for (v in setdiff(names(pool), na_ok)) {
    if (anyNA(pool[[v]])) stop('unexpected NA in ', v)
  }
  stopifnot(all(pool$weight > 0), all(pool$GENDER %in% 1:2),
            all(pool$age1 >= 18), uniqueN(pool$id) == nrow(pool))

  message(sprintf(paste('  emitted %s rows: %.2fM household non-filing adults',
                        '+ %.2fM GQ = %.2fM (calibration %.2fM, exact)'),
                  format(nrow(pool), big.mark = ','),
                  emitted_hh_adults / 1e6, emitted_gq_adults / 1e6,
                  (emitted_hh_adults + emitted_gq_adults) / 1e6,
                  cal_total / 1e6))
  message(sprintf(paste('  income mass: wages $%.1fB | interest $%.1fB |',
                        'dividends $%.1fB | pensions $%.1fB | SS $%.1fB',
                        '(DINA: $116.2B / 0 / 0 / small / large)'),
                  pool[, sum(weight * wages)] / 1e9,
                  pool[, sum(weight * txbl_int)] / 1e9,
                  pool[, sum(weight * qual_div)] / 1e9,
                  pool[, sum(weight * txbl_pens_dist)] / 1e9,
                  pool[, sum(weight * gross_ss)] / 1e9))

  f <- file.path(RES, sprintf('nonfiler_pool_%d.csv.gz', yr))
  fwrite(pool, f)
  message('  wrote ', f)
  emit_totals[[as.character(yr)]] <- pool[, .(rows = .N, adults =
    sum(weight * (1 + (filing_status == 2))))]
}

#-----------------------------------------------------------------------------
# Publish to the shared model_data store (opt-in)
#-----------------------------------------------------------------------------
if (PUBLISH) {
  root <- read_yaml('./config/interfaces/output_roots.yaml')$production
  dest <- file.path(root, 'model_data/ASEC-Nonfilers/v1', VINTAGE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  for (yr in YEARS) {
    stopifnot(file.copy(file.path(RES, sprintf('nonfiler_pool_%d.csv.gz', yr)),
                        file.path(dest, sprintf('nonfiler_pool_%d.csv.gz', yr)),
                        overwrite = TRUE))
  }
  readme <- file.path(dest, 'README.md')
  writeLines(c(
    '# ASEC-Nonfilers -- the constructed non-filer pool',
    '',
    sprintf('Vintage %s, built from Tax-Simulator branch state-tax by', VINTAGE),
    'research/state_weights/nonfiler_pool/ scripts 01-05. Replaces the DINA',
    'non-filer append (decision S13). TY2017 is the vintage Tax-Data consumes',
    '(the append happens at the 2017 base year); TY2022 is the validation',
    'artifact against the second anchor year.',
    '',
    'Method of record: research/state_weights/plan.md section 3 and',
    'research/state_weights/nonfiler_residual/10_asec_tax_unit_design.md.',
    'Records carry EXPECTED weights (weight x P(nonfile) under the calibrated',
    'filing model) -- deterministic, no random draw. Column conventions mirror',
    'Tax-Data src/impute_nonfilers.R; the reader zero-fills remaining',
    'mid-pipeline columns exactly as the DINA append does today. filer = 0 is',
    'explicit. GENDER is OBSERVED (ASEC/ACS sex), not imputed.',
    '',
    'Known asymmetry (S16): TY2017 kg_lt is zero -- the ASEC has no survey',
    'capital-gains item before income year 2018. The anchor identity:',
    'pool adults + claimed-dependent netting = residual anchor, exactly, by',
    'construction (48.470M TY2017 / 47.803M TY2022, S15 corrected basis).'),
    readme)
  message('published vintage ', VINTAGE, ' to ', dest)
}
