#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 02_build_residual_anchors.R  (Stage D, research/state_weights/nonfiler_residual_design.md §4.2)
#
# Build the residual non-filer anchors for one tax year:
#
#   national_anchor_{year}.csv     age band x {pep_adults, filing_adults,
#                                  residual_nonfiling_adults} -- filing side
#                                  from Pub 1304 Table 1.6 (returns by marital
#                                  status x age), the age dimension HT2 lacks
#   residual_anchors_{year}.csv    state x {pep_adults_18p, married/single
#                                  filing adults (HT2 identities via
#                                  ht2_filing_persons()), residual}
#   nonfiler_wage_margin_{year}.csv state wage margin: HT2 returns-with-wages
#                                  and wage dollars against two covered-worker
#                                  frames -- QCEW (jobs and payroll) and SSA
#                                  EEDATA Table 4 (persons and taxable
#                                  earnings, HI/Medicare coverage)
#   ssa_age_margin_{year}.csv      state x SSA age band covered-worker counts
#                                  (EEDATA Table 5), the input to the state x
#                                  age allocation of the residual (D6)
#
# Universe (design memo §3.0): PEP RESIDENT population, no group-quarters
# subtraction -- the PUF/DINA universe includes GQ residents. Age bands follow
# Table 1.6 (18-25, 26-34, 35-44, 45-54, 55-64, 65+), not age_band().
#
# Universe tags travel with the SSA margins (memo §7.3): EEDATA is
# `covered_worker_hi`, OASDI is `beneficiary`. Neither is `resident` -- both
# are administrative person-level universes -- so they enter as SHAPES over
# the PEP-based residual, never as levels.
#
# Known wedges carried, not resolved (memo §3.1/fn.8): return-state vs
# residence; MFS/QSS residual in the identities; adult dependents claimed on
# returns sit inside the residual (national wedge quantified in T1, script 03);
# MFJ spouses are assigned the primary taxpayer's age band.
#
# Usage (repo root, login node OK):
#   Rscript research/state_weights/nonfiler_residual/02_build_residual_anchors.R [year]
# Default runs both anchor years (2017, 2022).
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr)
  library(stringr); library(yaml); library(readxl)
})
source('src/data/state_weights.R')

args <- commandArgs(trailingOnly = TRUE)
years <- if (length(args)) as.integer(args) else c(2017L, 2022L)
res_dir <- 'research/state_weights/nonfiler_residual/results'
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

# Anchor age bands = Pub 1304 Table 1.6 bands (adults 18+)
# a16_band()/TARGET_AGE_BANDS now live in src/data/state_weights.R (sourced
# above) -- one definition, shared with age_band()/target_age_band() so the
# anchor bands and the fit's cell bands cannot drift apart again. Verified
# identical to the local copy this replaces (2026-08-19).
A16_BANDS <- TARGET_AGE_BANDS

#---------------------------------------------------------------
# Pub 1304 Table 1.6: returns by filing status block x age band
#---------------------------------------------------------------

# Row-label -> (block | age band) parse of the published sheet. Blocks carry
# 1 or 2 adults per return; MFJ ages are the PRIMARY taxpayer's (documented
# approximation). "Under 26" rows in status blocks are treated as 18-25 --
# under-18 married/HoH filers are negligible; the single block's "Under 18"
# row (dependent-age filers) is excluded from the adult bands and reported.
read_pub1304_t16 <- function(year) {
  f <- file.path(raw_data_root(), 'IRS-Ind/national/by_size',
                 sprintf('returns_marital_age_%d.xls', year))
  x <- suppressMessages(read_excel(f, sheet = 1, col_names = FALSE))
  lab <- str_squish(as.character(x[[1]]))
  n   <- suppressWarnings(as.numeric(gsub('[^0-9.-]', '', as.character(x[[2]]))))

  block_map <- c('^All returns, total'                        = 'all',
                 'married persons filing jointly'             = 'mfj',
                 'married persons filing separately'          = 'mfs',
                 'heads of household'                         = 'hoh',
                 'surviving spouse'                           = 'qss',
                 'single persons'                             = 'single')
  age_map <- c('^Under 18$' = 'u18', '^Under 26$' = '18_25',
               '^18 under 26$' = '18_25', '^26 under 35$' = '26_34',
               '^35 under 45$' = '35_44', '^45 under 55$' = '45_54',
               '^55 under 65$' = '55_64', '^65 and over$'  = '65p')

  cur_block <- NA_character_
  out <- list()
  for (i in seq_along(lab)) {
    if (is.na(lab[i]) || lab[i] == '') next
    b <- block_map[str_detect(lab[i], regex(names(block_map), ignore_case = TRUE))]
    b <- b[!is.na(b)]
    if (length(b)) { cur_block <- b[1]; next }
    a <- age_map[str_detect(lab[i], regex(names(age_map), ignore_case = TRUE))]
    a <- a[!is.na(a)]
    if (length(a) && !is.na(cur_block) && !is.na(n[i])) {
      out[[length(out) + 1]] <- data.table(block = cur_block, band = a[1],
                                           n_returns = n[i])
    }
  }
  t16 <- rbindlist(out)
  # Duplicate (block, band) rows would mean the label parse broke on a vintage
  stopifnot(nrow(t16) == uniqueN(t16[, .(block, band)]))
  t16
}

read_pub1304_t17_total <- function(year) {
  f <- file.path(raw_data_root(), 'IRS-Ind/national/by_size',
                 sprintf('dependent_returns_%d.xls', year))
  x <- suppressMessages(read_excel(f, sheet = 1, col_names = FALSE))
  i <- which(str_squish(as.character(x[[1]])) == 'All returns')[1]
  as.numeric(gsub('[^0-9.-]', '', as.character(x[[2]][i])))
}

#---------------------------------------------------------------
# Census PEP: resident population by state x single year of age
#---------------------------------------------------------------

read_pep <- function(year) {
  f <- file.path(raw_data_root(), 'Census-PEP',
                 if (year <= 2020) 'sc-est2020int-alldata6.csv' else
                                   'sc-est2024-alldata6.csv')
  pep <- fread(f, showProgress = FALSE)
  pop_col <- sprintf('POPESTIMATE%d', year)
  stopifnot(pop_col %in% names(pep), all(pep$AGE <= 85 | pep$AGE == 999))
  # Totals: SEX 0 and ORIGIN 0 are marginal rows; RACE 1-6 partition (no total)
  pep <- pep[SEX == 0 & ORIGIN == 0 & AGE != 999,
             .(pop = sum(get(pop_col))), by = .(STATE, AGE)]
  pep[, state := FIPS_TO_STATE[as.character(STATE)]]
  stopifnot(!anyNA(pep$state), uniqueN(pep$state) == 51)
  pep[]
}

#---------------------------------------------------------------
# Build per year
#---------------------------------------------------------------

for (yr in years) {
  message('=== TY', yr)

  pep <- read_pep(yr)
  message(sprintf('  PEP resident pop: %.2fM total, %.2fM adults 18+',
                  pep[, sum(pop)] / 1e6, pep[AGE >= 18, sum(pop)] / 1e6))

  t16 <- read_pub1304_t16(yr)
  t17_dep_returns <- read_pub1304_t17_total(yr)

  # Cross-check the label parse against the published all-returns total
  t16_total <- t16[block == 'all', sum(n_returns)]
  blocks_total <- t16[block != 'all', sum(n_returns)]
  message(sprintf('  T1.6 all-returns %.1fM | status blocks sum %.1fM (gap %.2f%%) | T1.7 dependent returns %.2fM',
                  t16_total / 1e6, blocks_total / 1e6,
                  100 * (blocks_total / t16_total - 1), t17_dep_returns / 1e6))

  # Filing adults by band: 2 per MFJ return (primary's band), 1 otherwise.
  # The status blocks carry no "Under 18" row -- under-18 filers (published
  # only in the all-returns block) sit inside the single block's "Under 26",
  # so they are subtracted from the 18_25 band here.
  fa <- t16[block != 'all' & band != 'u18',
            .(filing_adults = sum(n_returns * fifelse(block == 'mfj', 2, 1))),
            by = band]
  u18_filers <- t16[block == 'all' & band == 'u18', sum(n_returns)]
  fa[band == '18_25', filing_adults := filing_adults - u18_filers]

  # National anchor: PEP adults by band minus filing adults by band
  pep_nat <- pep[AGE >= 18, .(pep_adults = sum(pop)), by = .(band = as.character(a16_band(AGE)))]
  nat <- merge(pep_nat, fa, by = 'band', all = TRUE)
  nat <- nat[match(A16_BANDS, band)]
  nat[, residual_nonfiling_adults := pep_adults - filing_adults]
  nat <- rbind(nat, data.table(band = 'total_18p', t(colSums(nat[, -1]))))

  # HT2 identities by state; national consistency check vs the T1.6 build
  ht2 <- read_ht2(ht2_path(yr), yr)
  fp  <- ht2_filing_persons(ht2)
  ht2_filing_adults <- fp[, sum(married_filing_adults + single_filing_adults)]
  message(sprintf('  filing adults 18+: T1.6 %.1fM (excl. %.2fM under-18 filers) vs HT2 identities %.1fM (gap %.2f%%)',
                  nat[band == 'total_18p', filing_adults] / 1e6, u18_filers / 1e6,
                  ht2_filing_adults / 1e6,
                  100 * (nat[band == 'total_18p', filing_adults] / ht2_filing_adults - 1)))
  message(sprintf('  national residual non-filing adults 18+: %.1fM (%.1f%% of PEP adults)',
                  nat[band == 'total_18p', residual_nonfiling_adults] / 1e6,
                  100 * nat[band == 'total_18p', residual_nonfiling_adults / pep_adults]))

  # State anchor: PEP 18+ minus HT2-identity filing adults (state shares of the
  # T1.6-consistent national level come later, with the OASDI age allocation)
  st <- merge(pep[AGE >= 18, .(pep_adults_18p = sum(pop)), by = state],
              fp, by = 'state')
  st[, filing_adults := married_filing_adults + single_filing_adults]
  st[, residual_nonfiling_adults := pep_adults_18p - filing_adults]
  st[, residual_share_of_adults := residual_nonfiling_adults / pep_adults_18p]

  # SSA OASDI beneficiaries aged 65+ (December stock) -- the state x age input
  # for the elderly end of D6, where Table 1.6 stops at a single 65+ band. A
  # SHAPE, not a level: it is a beneficiary universe and a point-in-time stock
  # against the residual's annual resident flow.
  st <- merge(st, read_ssa_oasdi_65p(yr)[, .(state, beneficiaries_65p)],
              by = 'state')
  pep65 <- pep[AGE >= 65, .(pep_65p = sum(pop)), by = state]
  st <- merge(st, pep65, by = 'state')
  st[, ssa_65p_coverage := beneficiaries_65p / pep_65p]
  message(sprintf('  OASDI 65+ beneficiaries: %.1fM = %.1f%% of PEP 65+ (state range %.1f%%-%.1f%%)',
                  st[, sum(beneficiaries_65p)] / 1e6,
                  100 * st[, sum(beneficiaries_65p) / sum(pep_65p)],
                  100 * min(st$ssa_65p_coverage), 100 * max(st$ssa_65p_coverage)))
  message(sprintf('  state residual shares: %.1f%% (min %s) to %.1f%% (max %s)',
                  100 * min(st$residual_share_of_adults), st[which.min(residual_share_of_adults), state],
                  100 * max(st$residual_share_of_adults), st[which.max(residual_share_of_adults), state]))

  # Wage margin: HT2 returns-with-wages / wage dollars vs QCEW; SSA
  # persons-with-wages pending (store blocked; see 01_fetch README)
  ht2_w <- dcast(ht2[variable %in% c('n_wages', 'wages_amt') & !(state %in% NONTAX_BUCKETS),
                     .(value = sum(value)), by = .(state, variable)],
                 state ~ variable, value.var = 'value')
  qcew <- fread(file.path(raw_data_root(), 'BLS-QCEW',
                          sprintf('qcew_state_totals_%d.csv', yr)))[state != 'US']
  wm <- merge(ht2_w, qcew[, .(state, qcew_avg_emplvl = annual_avg_emplvl,
                              qcew_wages = total_annual_wages)], by = 'state')
  # SSA EEDATA Table 4: persons with HI-covered wage-and-salary earnings, and
  # those earnings. Persons, unlike QCEW's average monthly employment level,
  # are the right denominator for HT2's returns-with-wages -- the remaining
  # wedge is returns vs persons (joint returns, multiple earners), not jobs vs
  # persons. Earnings are uncapped under HI, so the dollar ratio is meaningful.
  ee <- read_ssa_eedata_hi(yr)
  wm <- merge(wm, ee$persons[, .(state,
                                 ssa_covered_persons = hi_persons_wage_salary,
                                 ssa_covered_wages   = hi_wage_salary_earnings)],
              by = 'state')
  wm[, `:=`(ht2_returns_per_ssa_person = n_wages / ssa_covered_persons,
            ht2_wages_per_ssa_wages    = wages_amt / ssa_covered_wages,
            ht2_wages_per_qcew_wages   = wages_amt / qcew_wages)]
  message(sprintf('  SSA HI-covered wage earners: %.1fM, $%.3fT | HT2 returns-with-wages per covered person %.3f (range %.3f-%.3f)',
                  wm[, sum(ssa_covered_persons)] / 1e6,
                  wm[, sum(ssa_covered_wages)] / 1e12,
                  wm[, sum(n_wages) / sum(ssa_covered_persons)],
                  wm[, min(ht2_returns_per_ssa_person)],
                  wm[, max(ht2_returns_per_ssa_person)]))

  fwrite(nat, file.path(res_dir, sprintf('national_anchor_%d.csv', yr)))
  fwrite(st[order(state)], file.path(res_dir, sprintf('residual_anchors_%d.csv', yr)))
  fwrite(wm[order(state)], file.path(res_dir, sprintf('nonfiler_wage_margin_%d.csv', yr)))
  fwrite(ee$age, file.path(res_dir, sprintf('ssa_age_margin_%d.csv', yr)))
  message('  wrote national_anchor / residual_anchors / nonfiler_wage_margin / ssa_age_margin CSVs')
}
