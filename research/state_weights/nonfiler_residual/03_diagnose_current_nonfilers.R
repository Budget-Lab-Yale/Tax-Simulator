#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 03_diagnose_current_nonfilers.R  (Stage D, research/state_weights/nonfiler_residual_design.md §4.3)
#
# Two modes:
#
#   --acs [year]   HEAVY (run under sbatch: run_acs_tabulation.sbatch; the
#                  extract read OOMs the login node). Reads the IPUMS extract
#                  once and writes the ACS-side inputs to results/:
#                    acs_margins_v0_{year}.csv       v0 non-filer margins (as
#                                                    production, GQ untreated)
#                    acs_margins_gqexcl_{year}.csv   GQ in {3,4} excluded first
#                    acs_filer_units_{year}.csv      v0 filer units by state,
#                                                    both variants
#                    acs_gq_composition_{year}.csv   T7: GQ persons by type x
#                                                    state x age band
#                    acs_irs_person_compare_{year}.csv  person-level ACS vs IRS
#
#   --tables       (default; login node OK) Assembles the T1-T7 diagnostic
#                  tables from the anchors (script 02), the ACS outputs (--acs
#                  mode), and the production Tax-Data non-filer records.
#                  Degrades gracefully: tables whose inputs are missing (SSA
#                  stores, --acs not yet run) are written with NA columns or
#                  skipped with a message.
#
# Run from the repo root.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr)
  library(stringr); library(yaml)
})
source('src/data/state_weights.R')

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) && args[1] == '--acs') 'acs' else 'tables'
res_dir <- 'research/state_weights/nonfiler_residual/results'
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

# Anchor age bands (Pub 1304 Table 1.6; must match 02_build_residual_anchors.R)
# a16_band()/TARGET_AGE_BANDS now live in src/data/state_weights.R (sourced
# above) -- one definition, shared with age_band()/target_age_band() so the
# anchor bands and the fit's cell bands cannot drift apart again. Verified
# identical to the local copy this replaces (2026-08-19).
A16_BANDS <- TARGET_AGE_BANDS

# Production Tax-Data vintage (same resolution as research/state_weights/scripts/build_state_weights.R)
tax_data_path <- function(year) {
  file.path(read_yaml('./config/interfaces/output_roots.yaml')$production,
            'model_data/Tax-Data/v1',
            read_yaml('./config/interfaces/interface_versions.yaml')$`Tax-Data`$default_vintage,
            'baseline', sprintf('tax_units_%d.csv', year))
}

#==============================================================================
# --acs mode
#==============================================================================

if (mode == 'acs') {

  year <- if (length(args) >= 2) as.integer(args[2]) else 2022L
  message('ACS tabulation for TY', year)

  acs <- read_acs_extract(min(year, 2022),
                          cols = c('YEAR','STATEFIP','PERWT','PERNUM','SERIAL','SAMPLE',
                                   'AGE','MARST','SPLOC','MOMLOC','POPLOC','INCTOT',
                                   'GQ','SCHOOL','EMPSTAT','SEX','INCWAGE'))
  message('  extract rows: ', nrow(acs))

  # v0 margins exactly as production (GQ untreated), then the exclusion variant
  m_v0 <- build_acs_margins(acs, year)
  m_gq <- build_acs_margins(acs[!(GQ %in% c(3, 4))], year)

  fwrite(m_v0$nonfiler_margins, file.path(res_dir, sprintf('acs_margins_v0_%d.csv', year)))
  fwrite(m_gq$nonfiler_margins, file.path(res_dir, sprintf('acs_margins_gqexcl_%d.csv', year)))
  fwrite(rbind(m_v0$filer_units[, variant := 'v0'],
               m_gq$filer_units[, variant := 'gq_excluded']),
         file.path(res_dir, sprintf('acs_filer_units_%d.csv', year)))

  # T7 input: GQ persons by type x state x age band. Dorm students are the
  # dependents-claimed-elsewhere population (design memo §3.2); institutional
  # residents are genuine own-state non-filers; GQ==4 non-students bundle
  # military barracks with other non-institutional GQ (extract lacks a
  # military-quarters flag; EMPSTAT cannot separate it cleanly).
  gq <- as.data.table(acs)[GQ %in% c(3, 4)]
  gq <- gq[YEAR == max(YEAR)]
  gq[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  gq <- gq[!is.na(state)]
  gq[, gq_type := fifelse(GQ == 3, 'institutional',
                  fifelse(SCHOOL == 2 & AGE %in% 18:24, 'dorm_student',
                          'other_noninstitutional'))]
  gq[, band := fifelse(AGE < 18, 'u18', as.character(a16_band(AGE)))]
  gq_comp <- gq[, .(persons = sum(PERWT)), by = .(state, gq_type, band)]
  fwrite(gq_comp[order(state, gq_type, band)],
         file.path(res_dir, sprintf('acs_gq_composition_%d.csv', year)))
  message(sprintf('  GQ persons: %.2fM (institutional %.2fM, dorm students %.2fM, other %.2fM)',
                  gq_comp[, sum(persons)] / 1e6,
                  gq_comp[gq_type == 'institutional', sum(persons)] / 1e6,
                  gq_comp[gq_type == 'dorm_student', sum(persons)] / 1e6,
                  gq_comp[gq_type == 'other_noninstitutional', sum(persons)] / 1e6))

  # Person-level ACS vs IRS reconciliation (model-free)
  ht2 <- read_ht2(ht2_path(year), year)
  fwrite(compare_individuals_acs_irs(ht2, acs[YEAR == max(YEAR)]),
         file.path(res_dir, sprintf('acs_irs_person_compare_%d.csv', year)))

  message('Done (--acs).')
  quit(save = 'no')
}

#==============================================================================
# --tables mode
#==============================================================================

ANCHOR_YEARS <- c(2017L, 2022L)
maybe_read <- function(f) if (file.exists(f)) fread(f) else NULL

nat  <- lapply(setNames(ANCHOR_YEARS, ANCHOR_YEARS), function(y)
  maybe_read(file.path(res_dir, sprintf('national_anchor_%d.csv', y))))
stanch <- lapply(setNames(ANCHOR_YEARS, ANCHOR_YEARS), function(y)
  maybe_read(file.path(res_dir, sprintf('residual_anchors_%d.csv', y))))
stopifnot(!is.null(nat$`2022`), !is.null(stanch$`2022`))

# Production non-filer slices (13,204 records/year: cheap once filtered)
nf_cols <- c('weight', 'filer', 'filing_status', 'age1', 'age2', 'wages',
             'txbl_int', 'exempt_int', 'div_ord', 'div_pref', 'kg_st', 'kg_lt',
             'txbl_pens_dist', 'txbl_ira_dist', 'gross_ss', 'sole_prop', 'ui')
nf <- lapply(setNames(ANCHOR_YEARS, ANCHOR_YEARS), function(y) {
  fread(tax_data_path(y), select = nf_cols, showProgress = FALSE)[filer == 0]
})

#---------------------------------------
# T1: national level
#---------------------------------------

t1 <- rbindlist(lapply(ANCHOR_YEARS, function(y) {
  x <- nf[[as.character(y)]]; a <- nat[[as.character(y)]]
  if (is.null(a)) return(NULL)
  ht2_dep <- ht2_filing_persons(read_ht2(ht2_path(y), y))[, sum(dependents)]
  pep_u18 <- {  # adult-dependent wedge arithmetic: claimed dependents vs child pop
    pep_f <- file.path(raw_data_root(), 'Census-PEP',
                       if (y <= 2020) 'sc-est2020int-alldata6.csv' else 'sc-est2024-alldata6.csv')
    p <- fread(pep_f, select = c('SEX','ORIGIN','AGE', sprintf('POPESTIMATE%d', y)))
    p[SEX == 0 & ORIGIN == 0 & AGE < 18, sum(get(sprintf('POPESTIMATE%d', y)))]
  }
  data.table(
    year                        = y,
    taxdata_nonfiler_units      = x[, sum(weight)],
    taxdata_nonfiler_adults     = x[, sum(weight * (1 + (filing_status == 2)))],
    residual_nonfiling_adults_18p = a[band == 'total_18p', residual_nonfiling_adults],
    pep_adults_18p              = a[band == 'total_18p', pep_adults],
    filing_adults_18p           = a[band == 'total_18p', filing_adults],
    pub5785_above_threshold     = 10.6e6,     # TY2014 level, IRS Pub 5785
    dependent_filer_returns     = NA_real_,   # filled from anchors log; see T1 notes
    ht2_dependents_claimed      = ht2_dep,
    pep_under18                 = pep_u18,
    adult_dependent_wedge_lb    = ht2_dep - pep_u18)  # lower bound: never-claimed kids offset
}))
fwrite(t1, file.path(res_dir, 'T1_national_level.csv'))
message(sprintf('T1: Tax-Data non-filer adults %.1fM (2022) vs residual anchor %.1fM -- ratio %.2f',
                t1[year == 2022, taxdata_nonfiler_adults] / 1e6,
                t1[year == 2022, residual_nonfiling_adults_18p] / 1e6,
                t1[year == 2022, taxdata_nonfiler_adults / residual_nonfiling_adults_18p]))

#---------------------------------------
# T2: age composition (production ages are 2017-frozen; the 2017 comparison is
# the clean one, 2022 is carried for reference)
#---------------------------------------

t2 <- rbindlist(lapply(ANCHOR_YEARS, function(y) {
  x <- copy(nf[[as.character(y)]]); a <- nat[[as.character(y)]]
  if (is.null(a)) return(NULL)
  # adults: primary at age1's band; MFJ spouse at age2's band (age2 == 0 -> age1)
  prim <- x[, .(adults = sum(weight)), by = .(band = as.character(a16_band(pmax(age1, 18))))]
  spou <- x[filing_status == 2,
            .(adults = sum(weight)),
            by = .(band = as.character(a16_band(pmax(fifelse(age2 > 0, age2, age1), 18))))]
  puf <- rbind(prim, spou)[, .(taxdata_adults = sum(adults)), by = band]
  m <- merge(a[band != 'total_18p', .(band, residual_nonfiling_adults)], puf, by = 'band', all.x = TRUE)
  m[, `:=`(year = y,
           taxdata_share  = taxdata_adults / sum(taxdata_adults, na.rm = TRUE),
           residual_share = residual_nonfiling_adults / sum(residual_nonfiling_adults))]
  m[match(A16_BANDS, band)]
}))
fwrite(t2, file.path(res_dir, 'T2_age_composition.csv'))
message('T2: 65+ share of non-filer adults -- Tax-Data ',
        sprintf('%.1f%%', 100 * t2[year == 2017 & band == '65p', taxdata_share]),
        ' vs residual ',
        sprintf('%.1f%%', 100 * t2[year == 2017 & band == '65p', residual_share]), ' (2017)')

#---------------------------------------
# T3: income composition of the non-filer records
#---------------------------------------

t3 <- rbindlist(lapply(ANCHOR_YEARS, function(y) {
  x <- nf[[as.character(y)]]
  W <- x[, sum(weight)]
  has <- function(v) x[, sum(weight * (v > 0))] / W
  data.table(
    year = y,
    share_wages    = has(x$wages),
    share_interest = has(x$txbl_int + x$exempt_int),
    share_dividends= has(x$div_ord + x$div_pref),
    share_kg       = has(x$kg_st + x$kg_lt),
    share_ss       = has(x$gross_ss),
    share_pension  = has(x$txbl_pens_dist + x$txbl_ira_dist),
    share_soleprop = x[, sum(weight * (sole_prop != 0))] / W,
    share_ui       = has(x$ui))
}))
fwrite(t3, file.path(res_dir, 'T3_income_composition.csv'))

# income_tier placement under the current zeros
t3b <- rbindlist(lapply(ANCHOR_YEARS, function(y) {
  x <- nf[[as.character(y)]]
  x[, .(year = y, units = sum(weight)), by = .(tier = income_tier(puf_gross_income(x)))]
}))
fwrite(t3b[order(year, tier)], file.path(res_dir, 'T3b_income_tier_placement.csv'))
message('T3: shares with interest/dividends/kg (2022): ',
        paste(sprintf('%.1f%%', 100 * unlist(t3[year == 2022,
              .(share_interest, share_dividends, share_kg)])), collapse = ' / '))

#---------------------------------------
# T4: aging path of the filer/non-filer split, production weight ledger
#---------------------------------------

t4 <- rbindlist(lapply(2017:2035, function(y) {
  p <- tax_data_path(y)
  if (!file.exists(p)) return(NULL)
  x <- fread(p, select = c('weight', 'filer', 'filing_status'), showProgress = FALSE)
  x[, .(year = y,
        filer_units    = sum(weight[filer == 1]),
        nonfiler_units = sum(weight[filer == 0]),
        nonfiler_adults = sum((weight * (1 + (filing_status == 2)))[filer == 0]))]
}))
# anchor-implied path where PEP actuals exist
pep24 <- fread(file.path(raw_data_root(), 'Census-PEP', 'sc-est2024-alldata6.csv'))
pep_adults_by_year <- sapply(2020:2024, function(y)
  pep24[SEX == 0 & ORIGIN == 0 & AGE >= 18 & AGE != 999, sum(get(sprintf('POPESTIMATE%d', y)))])
t4[, pep_adults_18p := c(rep(NA_real_, sum(t4$year < 2020)), pep_adults_by_year,
                         rep(NA_real_, sum(t4$year > 2024)))[seq_len(.N)]]
fwrite(t4, file.path(res_dir, 'T4_aging_path.csv'))
message(sprintf('T4: non-filer units %.1fM (2017) -> %.1fM (2035); share of all units %.1f%% -> %.1f%%',
                t4[year == 2017, nonfiler_units] / 1e6, t4[year == 2035, nonfiler_units] / 1e6,
                100 * t4[year == 2017, nonfiler_units / (filer_units + nonfiler_units)],
                100 * t4[year == 2035, nonfiler_units / (filer_units + nonfiler_units)]))

#---------------------------------------
# T5: state margins -- v0 ACS vs residual anchors (needs --acs outputs)
#---------------------------------------

# IRS EITC participation rates by state, TY2022 (copied constant from
# research/state_weights/scripts/sweep_state_weights.R:67 -- irs.gov EITC Central, ACS-Census linkage)
EITC_TAKEUP <- c(
  AL=79.2, AK=75.1, AZ=78.6, AR=80.5, CA=77.4, CO=77.3, CT=82.0, DE=82.4,
  DC=73.6, FL=82.0, GA=82.1, HI=83.3, ID=80.7, IL=81.0, IN=83.3, IA=80.7,
  KS=78.3, KY=82.5, LA=81.6, ME=85.2, MD=82.2, MA=81.7, MI=82.8, MN=81.9,
  MS=80.2, MO=79.3, MT=75.1, NE=83.2, NV=82.2, NH=80.0, NJ=81.1, NM=83.6,
  NY=83.7, NC=79.7, ND=79.6, OH=82.6, OK=79.1, OR=78.4, PA=82.8, RI=82.4,
  SC=80.7, SD=82.3, TN=82.0, TX=80.6, UT=75.4, VT=80.0, VA=82.2, WA=77.4,
  WV=85.1, WI=78.9, WY=78.6)

y <- 2022L
m_v0 <- maybe_read(file.path(res_dir, sprintf('acs_margins_v0_%d.csv', y)))
m_gq <- maybe_read(file.path(res_dir, sprintf('acs_margins_gqexcl_%d.csv', y)))
f_ac <- maybe_read(file.path(res_dir, sprintf('acs_filer_units_%d.csv', y)))
if (!is.null(m_v0)) {
  ht2_ret <- dcast(read_ht2(ht2_path(y), y)[variable == 'n_returns' & !(state %in% NONTAX_BUCKETS),
                                            .(value = sum(value)), by = state],
                   state ~ ., value.var = 'value')
  setnames(ht2_ret, '.', 'ht2_returns')
  t5 <- Reduce(function(a, b) merge(a, b, by = 'state', all.x = TRUE), list(
    stanch$`2022`[, .(state, pep_adults_18p, filing_adults, residual_nonfiling_adults)],
    m_v0[, .(acs_v0_nonfiler_units = sum(n_units)), by = state],
    m_gq[, .(acs_gqexcl_nonfiler_units = sum(n_units)), by = state],
    f_ac[variant == 'v0', .(state, acs_v0_filer_units = n_units)],
    ht2_ret))
  t5[, `:=`(v0_filer_vs_ht2      = acs_v0_filer_units / ht2_returns,
            v0_nonfiler_vs_resid = acs_v0_nonfiler_units / residual_nonfiling_adults,
            gqexcl_nonfiler_vs_resid = acs_gqexcl_nonfiler_units / residual_nonfiling_adults)]
  fwrite(t5[order(state)], file.path(res_dir, 'T5_state_margins.csv'))
  tk <- data.table(state = names(EITC_TAKEUP), takeup = EITC_TAKEUP)
  t5c <- merge(t5, tk, by = 'state')
  message(sprintf('T5: v0 filer units vs HT2 returns: national %.3f (range %.2f-%.2f); corr(v0 nonfiler gap, EITC take-up) = %.2f',
                  t5c[, sum(acs_v0_filer_units) / sum(ht2_returns)],
                  min(t5c$v0_filer_vs_ht2), max(t5c$v0_filer_vs_ht2),
                  t5c[, cor(v0_nonfiler_vs_resid, takeup, use = 'complete.obs')]))
} else {
  message('T5: skipped (run --acs mode under sbatch first)')
}

#---------------------------------------
# T6: anchor cell support
#---------------------------------------

t6 <- stanch$`2022`[, .(state, residual_nonfiling_adults)][order(residual_nonfiling_adults)]
message(sprintf('T6: smallest state residuals: %s',
                paste(sprintf('%s %.0fk', t6$state[1:5], t6$residual_nonfiling_adults[1:5] / 1e3),
                      collapse = ', ')))
fwrite(t6, file.path(res_dir, 'T6_cell_support.csv'))

#---------------------------------------
# T7: GQ composition (needs --acs output)
#---------------------------------------

gq <- maybe_read(file.path(res_dir, sprintf('acs_gq_composition_%d.csv', y)))
if (!is.null(gq)) {
  t7 <- dcast(gq[band != 'u18', .(persons = sum(persons)), by = .(state, gq_type)],
              state ~ gq_type, value.var = 'persons', fill = 0)
  t7 <- merge(t7, stanch$`2022`[, .(state, residual_nonfiling_adults)], by = 'state')
  t7[, gq_adults_pct_of_residual := 100 * (institutional + dorm_student + other_noninstitutional) /
                                    residual_nonfiling_adults]
  setorder(t7, -gq_adults_pct_of_residual)
  fwrite(t7, file.path(res_dir, 'T7_gq_composition.csv'))
  message(sprintf('T7: GQ adults nationally %.2fM = %.1f%% of the residual; top state shares: %s',
                  t7[, sum(institutional + dorm_student + other_noninstitutional)] / 1e6,
                  100 * t7[, sum(institutional + dorm_student + other_noninstitutional)] /
                        t7[, sum(residual_nonfiling_adults)],
                  paste(t7$state[1:3], collapse = ', ')))
} else {
  message('T7: skipped (run --acs mode under sbatch first)')
}

message('Done (--tables).')
