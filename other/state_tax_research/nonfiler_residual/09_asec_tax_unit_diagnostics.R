#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 09_asec_tax_unit_diagnostics.R  (Stage D, research pass A / todo A2)
#
# Audit what the CPS ASEC already carries before building a tax-unit builder on
# top of it. The design memo's research pass A says to "start from what the
# extract already carries" -- IPUMS ships the Census Bureau's own tax-model
# recodes (FILESTAT, DEPSTAT, ADJGINC, TAXINC), and the question is whether a
# builder would be duplicating them, extending them, or contradicting them.
#
# Everything here is a measurement, not an assumption. The companion design note
# 10_asec_tax_unit_design.md interprets these tables and states the convention
# they lead to; every number it quotes comes from a CSV written here.
#
# Tables written to results/:
#   asec_A1_filestat_series.csv       TY x {population, filers, non-filers,
#                                     returns, dependents} -- the continuity
#                                     check the IPUMS DEPSTAT caveat asks for
#   asec_A1b_income_continuity.csv    the same check for the income items
#   asec_A2_filing_status_{year}.csv  filing-status mix vs SOI Pub 1304 T1.6
#   asec_A3_depstat_{year}.csv        dependency-pointer coherence
#   asec_A4_income_{year}.csv         ASEC income aggregates vs SOI HT2 and SSA
#   asec_A5_arrangements_{year}.csv   living arrangements that make unit
#                                     construction ambiguous
#   asec_A6_unit_structure_{year}.csv whether the recodes are unit- or
#                                     person-level
#
# Login-node safe: reads 11 slim year files plus 2 full anchor-year files.
#   Rscript other/state_tax_research/nonfiler_residual/09_asec_tax_unit_diagnostics.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(ipumsr); library(readxl)
  library(stringr); library(dplyr); library(readr); library(yaml)
})
source('src/data/state_weights.R')

ANCHOR_YEARS <- c(2017L, 2022L)
RESULTS <- 'other/state_tax_research/nonfiler_residual/results'
dir.create(RESULTS, recursive = TRUE, showWarnings = FALSE)

asec_dir <- function() file.path(raw_data_root(), 'CPS-ASEC/cps_asec_common')

# ASEC year Y asks about income in calendar year Y-1.
asec_sample_dir <- function(tax_year) {
  file.path(asec_dir(), sprintf('cps%d_03s', tax_year + 1L))
}

# IPUMS strips value labels onto haven attributes; drop them so data.table
# arithmetic and comparisons behave like plain numerics.
strip_labels <- function(dt) dt[, lapply(.SD, function(v) { attributes(v) <- NULL; v })]

read_asec <- function(tax_year, cols = NULL) {
  dd  <- asec_sample_dir(tax_year)
  ddi <- read_ipums_ddi(list.files(dd, '\\.xml$', full.names = TRUE))
  x   <- as.data.table(read_ipums_micro(ddi, verbose = FALSE))
  if (!is.null(cols)) x <- x[, ..cols]
  x <- strip_labels(x)
  x[, tax_year := tax_year]
  setattr(x, 'ddi', ddi)
  x[]
}

# NIU codes differ per variable (INCSS 999999, INCINT 9999999, INCWAGE
# 99999999, EITCRED 9999, ...). Read them off the DDI rather than hardcoding,
# and assert the code is the variable's maximum -- if it is not, the label and
# the data disagree and every aggregate below would be silently wrong.
niu_code <- function(ddi, var, x) {
  vl <- ipums_val_labels(ddi, all_of(var))
  hit <- vl$val[str_detect(vl$lbl, regex('N\\.?I\\.?U', ignore_case = TRUE))]
  stopifnot(length(hit) == 1, max(x[[var]]) == hit)
  hit
}

#-----------------------------------------------------------------------------
# A1. Continuity: do the Census recodes behave the same way in every year?
#
# IPUMS warns that DEPSTAT shows "dramatic shifts across time in the proportion
# of persons who are dependent, which may indicate inaccuracies in the data" and
# asks users to check. The extract spans TY2014-2024, so the check is cheap.
#-----------------------------------------------------------------------------

SERIES_COLS <- c('YEAR','SERIAL','PERNUM','ASECWT','GQ','AGE','MARST','SPLOC',
                 'MOMLOC','POPLOC','FAMUNIT','RELATE','FILESTAT','DEPSTAT')
SERIES_YEARS <- 2014:2024

message('=== A1. Census tax-model recodes, TY', min(SERIES_YEARS), '-', max(SERIES_YEARS))
panel <- rbindlist(lapply(SERIES_YEARS, function(ty) read_asec(ty, SERIES_COLS)))

FILER_CODES <- 1:5     # FILESTAT 1-3 joint, 4 head of household, 5 single
NONFILER    <- 6L
JOINT_CODES <- 1:3

a1 <- panel[, .(
  population_M       = sum(ASECWT) / 1e6,
  adults_18p_M       = sum(ASECWT * (AGE >= 18)) / 1e6,
  filing_adults_M    = sum(ASECWT * (AGE >= 18 & FILESTAT %in% FILER_CODES)) / 1e6,
  nonfiling_adults_M = sum(ASECWT * (AGE >= 18 & FILESTAT == NONFILER)) / 1e6,
  returns_M          = sum(ASECWT * (FILESTAT %in% JOINT_CODES)) / 2e6 +
                       sum(ASECWT * (FILESTAT %in% 4:5)) / 1e6,
  dependents_M       = sum(ASECWT * (DEPSTAT > 0)) / 1e6,
  adult_dependents_M = sum(ASECWT * (DEPSTAT > 0 & AGE >= 18)) / 1e6,
  gq_M               = sum(ASECWT * (GQ == 2)) / 1e6
), by = tax_year][order(tax_year)]
fwrite(a1, file.path(RESULTS, 'asec_A1_filestat_series.csv'))
print(a1[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])

message('  FILESTAT non-filing adults collapse in TY2020-2021 (',
        a1[tax_year == 2020, round(nonfiling_adults_M, 1)], 'M / ',
        a1[tax_year == 2021, round(nonfiling_adults_M, 1)], 'M against ',
        a1[tax_year == 2019, round(nonfiling_adults_M, 1)], 'M in TY2019) -- ',
        'the pandemic-era model assigned Economic Impact Payments through the ',
        'filing units, so those two years are unusable as a filing benchmark.')
message('  DEPSTAT adult dependents step from ',
        a1[tax_year == 2014, round(adult_dependents_M, 1)], 'M (TY2014) to ',
        a1[tax_year == 2015, round(adult_dependents_M, 1)], 'M (TY2015) -- ',
        'this is the level break IPUMS warns about; TY2014 is not comparable.')

#-----------------------------------------------------------------------------
# A1b. The same continuity question for the income items the filing model reads.
#
# A recode break and an income-question break are equally fatal to a pooled
# estimation, and the ASEC has had both. INCTOT is carried alongside as the
# control: if a component breaks while the total does not, the mass is still in
# the survey and only our extract's decomposition of it is wrong.
#-----------------------------------------------------------------------------

CONTINUITY_VARS <- c('INCTOT', 'INCWAGE', 'INCRETIR', 'INCSS', 'INCINT',
                     'INCDIVID', 'INCBUS', 'INCRENT')

message('\n=== A1b. Income-item continuity, TY', min(SERIES_YEARS), '-', max(SERIES_YEARS))
a1b <- rbindlist(lapply(SERIES_YEARS, function(ty) {
  x   <- read_asec(ty, c('ASECWT', CONTINUITY_VARS))
  ddi <- attr(x, 'ddi')
  rbindlist(lapply(CONTINUITY_VARS, function(v) {
    niu <- niu_code(ddi, v, x)
    data.table(tax_year = ty, variable = v,
               amount_B    = x[get(v) != niu, sum(as.numeric(get(v)) * ASECWT)] / 1e9,
               recipients_M = x[get(v) != niu & get(v) > 0, sum(ASECWT)] / 1e6)
  }))
}))
fwrite(a1b, file.path(RESULTS, 'asec_A1b_income_continuity.csv'))
print(dcast(a1b, tax_year ~ variable, value.var = 'amount_B')[
  , lapply(.SD, function(v) if (is.numeric(v)) round(v) else v)])

# INCRETIR is not comparable across 2019: from ASEC 2019 (TY2018) it covers only
# retirement-ACCOUNT income for respondents aged 58+, with pensions and
# annuities moved into INCPEN1/INCPEN2/INCRANN -- none of which this extract
# pulls. Assert the break rather than leaving a reader to notice the level jump.
retir <- dcast(a1b[variable == 'INCRETIR'], tax_year ~ ., value.var = 'amount_B')
setnames(retir, '.', 'amount_B')
drop <- 1 - retir[tax_year == 2018, amount_B] / retir[tax_year == 2017, amount_B]
stopifnot(drop > 0.5)
message(sprintf('  INCRETIR falls %.0f%% between TY2017 and TY2018 ($%.0fB -> $%.0fB): the ASEC 2019 income-question redesign. Pensions and annuities moved to INCPEN1/INCPEN2/INCRANN, which this extract does NOT carry, so the component is missing from TY2018 on. INCTOT is unaffected ($%.1fT -> $%.1fT), so the mass is in the survey and only our decomposition of it is short.',
                100 * drop, retir[tax_year == 2017, amount_B], retir[tax_year == 2018, amount_B],
                a1b[variable == 'INCTOT' & tax_year == 2017, amount_B] / 1000,
                a1b[variable == 'INCTOT' & tax_year == 2018, amount_B] / 1000))

#-----------------------------------------------------------------------------
# A2. Filing-status mix against SOI Pub 1304 Table 1.6.
#
# The Census tax model calibrates the COUNT of filers to administrative totals
# (O'Hara 2004 introduced a $2,000 income floor for exactly that purpose), so
# agreement on the total is by construction. The MIX is not calibrated, and it
# is where the model's structural assumptions show.
#-----------------------------------------------------------------------------

# Pub 1304 T1.6 block totals. The published "married filing jointly" block also
# carries surviving spouses; the four blocks partition all returns exactly.
soi_filing_status <- function(year) {
  f <- file.path(raw_data_root(), 'IRS-Ind/national/by_size',
                 sprintf('returns_marital_age_%d.xls', year))
  x <- suppressMessages(read_excel(f, sheet = 1, col_names = FALSE))
  lab <- str_squish(as.character(x[[1]]))
  n   <- suppressWarnings(as.numeric(gsub('[^0-9.-]', '', as.character(x[[2]]))))
  pick <- function(pat) n[which(str_detect(lab, regex(pat, ignore_case = TRUE)))[1]]
  out <- data.table(
    filing_status = c('joint_and_qss', 'married_separate', 'head_of_household', 'single'),
    soi_returns   = c(pick('married persons filing jointly'),
                      pick('married persons filing separately'),
                      pick('heads of household'),
                      pick('single persons')))
  total <- pick('^All returns, total')
  stopifnot(abs(sum(out$soi_returns) - total) < 1000)   # the blocks must partition
  out
}

for (ty in ANCHOR_YEARS) {
  message('\n=== A2. Filing-status mix, TY', ty)
  a <- panel[tax_year == ty]
  asec <- data.table(
    filing_status = c('joint_and_qss', 'married_separate', 'head_of_household', 'single'),
    asec_returns  = c(a[FILESTAT %in% JOINT_CODES, sum(ASECWT)] / 2,
                      0,                       # the Census model does not produce MFS
                      a[FILESTAT == 4, sum(ASECWT)],
                      a[FILESTAT == 5, sum(ASECWT)]))
  a2 <- merge(soi_filing_status(ty), asec, by = 'filing_status', sort = FALSE)
  a2[, `:=`(diff_M = (asec_returns - soi_returns) / 1e6,
            ratio  = asec_returns / soi_returns)]
  a2 <- rbind(a2, data.table(filing_status = 'total',
                             soi_returns = a2[, sum(soi_returns)],
                             asec_returns = a2[, sum(asec_returns)],
                             diff_M = a2[, sum(asec_returns - soi_returns)] / 1e6,
                             ratio = a2[, sum(asec_returns) / sum(soi_returns)]))
  fwrite(a2, file.path(RESULTS, sprintf('asec_A2_filing_status_%d.csv', ty)))
  print(a2[, .(filing_status, soi_M = round(soi_returns / 1e6, 2),
               asec_M = round(asec_returns / 1e6, 2),
               diff_M = round(diff_M, 2), ratio = round(ratio, 3))])
}

#-----------------------------------------------------------------------------
# A3. Does the dependency pointer cohere with the filing-status recode?
#
# DEPSTAT names the person who claimed the respondent, by CENSUS LINE NUMBER
# (LINENO). LINENO is not in the extract, so PERNUM stands in; the first row
# below sizes how often that substitution fails, which is also the case for
# adding LINENO to the extract.
#-----------------------------------------------------------------------------

for (ty in ANCHOR_YEARS) {
  message('\n=== A3. Dependency pointer coherence, TY', ty)
  a <- panel[tax_year == ty]
  claimers <- a[, .(SERIAL, DEPSTAT = PERNUM, claimer_age = AGE,
                    claimer_filestat = FILESTAT, claimer_relate = RELATE)]
  dep <- merge(a[DEPSTAT > 0], claimers, by = c('SERIAL', 'DEPSTAT'), all.x = TRUE)
  hh  <- a[, .(lineno_gap = max(DEPSTAT) > max(PERNUM)), by = SERIAL]
  dep <- merge(dep, hh, by = 'SERIAL')

  a3 <- data.table(
    measure = c('dependents (DEPSTAT > 0)',
                'adult dependents (age 18+)',
                'pointer resolves to no PERNUM',
                '  ...in a household with a LINENO gap',
                'pointer lands on a modelled NON-FILER',
                'pointer lands on a person under 18'),
    persons_M = c(dep[, sum(ASECWT)],
                  dep[AGE >= 18, sum(ASECWT)],
                  dep[is.na(claimer_age), sum(ASECWT)],
                  dep[is.na(claimer_age) & lineno_gap, sum(ASECWT)],
                  dep[claimer_filestat == NONFILER, sum(ASECWT)],
                  dep[claimer_age < 18, sum(ASECWT)]) / 1e6)
  a3[, pct_of_dependents := 100 * persons_M / persons_M[1]]
  fwrite(a3, file.path(RESULTS, sprintf('asec_A3_depstat_%d.csv', ty)))
  print(a3[, .(measure, persons_M = round(persons_M, 2),
               pct = round(pct_of_dependents, 1))])
}

#-----------------------------------------------------------------------------
# A4. Income aggregates against the administrative frames.
#
# Which ASEC income items can carry a tax concept, and which cannot. The
# comparison is deliberately three-way: SOI HT2 is income ON FILED RETURNS, SSA
# EEDATA is wages of ALL COVERED WORKERS, and the ASEC covers the whole
# civilian noninstitutional population -- so ASEC > HT2 is expected for wages
# and tells us nothing on its own, while ASEC vs SSA is a like-for-like read.
#-----------------------------------------------------------------------------

INCOME_VARS <- c('ADJGINC', 'TAXINC', 'INCTOT', 'INCWAGE', 'INCINT', 'INCDIVID',
                 'INCRENT', 'INCBUS', 'INCFARM', 'INCSS', 'INCRETIR', 'EITCRED',
                 'FEDTAX')

for (ty in ANCHOR_YEARS) {
  message('\n=== A4. Income aggregates, TY', ty)
  full <- read_asec(ty)
  ddi  <- attr(full, 'ddi')
  ht2  <- as.data.table(read_ht2(ht2_path(ty), ty))[
    !(state %in% NONTAX_BUCKETS), .(v = sum(value)), by = variable]
  setkey(ht2, variable)
  ssa <- read_ssa_eedata_hi(ty)$persons

  agg <- rbindlist(lapply(INCOME_VARS, function(v) {
    niu <- niu_code(ddi, v, full)
    data.table(variable = v,
               asec_amount   = full[get(v) != niu, sum(as.numeric(get(v)) * ASECWT)],
               asec_positive_M = full[get(v) != niu & get(v) > 0, sum(ASECWT)] / 1e6)
  }))
  # Only some ASEC items have a return-level SOI analogue; the rest stay NA
  # rather than being compared to something they are not.
  soi_map <- c(ADJGINC = 'agi_amt', INCWAGE = 'wages_amt', INCINT = 'int_amt',
               INCDIVID = 'div_amt', EITCRED = 'eitc_amt')
  agg[, soi_variable := soi_map[variable]]
  agg <- merge(agg, ht2[, .(soi_variable = variable, soi_amount = v)],
               by = 'soi_variable', all.x = TRUE, sort = FALSE)
  agg[, asec_over_soi := asec_amount / soi_amount]
  # SSA HI covered wage-and-salary earnings: the only truly like-for-like
  # comparator, because it too covers workers regardless of filing.
  agg[variable == 'INCWAGE',
      asec_over_ssa_wages := asec_amount / ssa[, sum(hi_wage_salary_earnings)]]
  fwrite(agg, file.path(RESULTS, sprintf('asec_A4_income_%d.csv', ty)))
  setcolorder(agg, c('variable', 'soi_variable'))
  print(agg[, .(variable, asec_B = round(asec_amount / 1e9),
                asec_pos_M = round(asec_positive_M, 1),
                soi_B = round(soi_amount / 1e9),
                vs_soi = round(asec_over_soi, 3),
                vs_ssa = round(asec_over_ssa_wages, 3))])

  #---------------------------------------------------------------------------
  # A5. Living arrangements that make unit construction ambiguous. These are the
  # cases every surveyed approach handles differently (design note §3), so their
  # size bounds how much the choice of convention can matter.
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  # A6. Do the Census recodes implicitly define a UNIT, or only a person-level
  # status? If the tax variables sit on exactly one spouse, the recodes carry a
  # unit structure that could in principle be reconstructed (design note §2.1).
  #---------------------------------------------------------------------------
  message('\n=== A6. Is the Census recode unit-level or person-level? TY', ty)
  jt <- full[FILESTAT %in% JOINT_CODES & SPLOC > 0,
             .(SERIAL, PERNUM, SPLOC, ADJGINC, FEDTAX, EITCRED, ASECWT)]
  spouse <- jt[, .(SERIAL, SPLOC = PERNUM, sp_adjginc = ADJGINC,
                   sp_fedtax = FEDTAX, sp_eitcred = EITCRED)]
  jt <- merge(jt, spouse, by = c('SERIAL', 'SPLOC'))
  carried <- function(v, sp, niu) {
    both <- jt[get(v) > 0 & get(sp) > 0 & get(v) != niu & get(sp) != niu, sum(ASECWT)]
    one  <- jt[xor(get(v) > 0 & get(v) != niu, get(sp) > 0 & get(sp) != niu), sum(ASECWT)]
    c(both, one) / 1e6
  }
  a6 <- rbindlist(list(
    data.table(variable = 'ADJGINC', t(carried('ADJGINC', 'sp_adjginc', niu_code(ddi, 'ADJGINC', full)))),
    data.table(variable = 'FEDTAX',  t(carried('FEDTAX',  'sp_fedtax',  niu_code(ddi, 'FEDTAX',  full)))),
    data.table(variable = 'EITCRED', t(carried('EITCRED', 'sp_eitcred', niu_code(ddi, 'EITCRED', full))))))
  setnames(a6, c('V1', 'V2'), c('both_spouses_M', 'exactly_one_spouse_M'))
  a6[, persons_in_joint_units_M := jt[, sum(ASECWT)] / 1e6]
  fwrite(a6, file.path(RESULTS, sprintf('asec_A6_unit_structure_%d.csv', ty)))
  print(a6[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])
  # A value carried by both spouses would mean the recodes are person-level and
  # no unit could be read off them. That case is not quite empty -- TY2017 has
  # ~0.01M on ADJGINC -- but it is far below any level that would matter.
  stopifnot(a6[, max(both_spouses_M / persons_in_joint_units_M)] < 0.001)

  message('\n=== A5. Ambiguous living arrangements, TY', ty)
  a <- panel[tax_year == ty]
  PARTNER_CODES <- c(1113, 1114, 1116, 1117)   # partner/roommate, unmarried partner
  partner_hh    <- a[RELATE %in% PARTNER_CODES, unique(SERIAL)]
  multifam_hh   <- a[FAMUNIT > 1, unique(SERIAL)]
  # The categories overlap, so the union is what actually bounds how much the
  # choice of convention can matter -- quote that, not the sum of the rows.
  ambiguous <- a[SERIAL %in% partner_hh | SERIAL %in% multifam_hh |
                 RELATE %in% c(501, 701, 901, 1001, 1241, 1260), unique(SERIAL)]
  a5 <- data.table(
    arrangement = c('total population',
                    'unmarried partners (RELATE 1113/1114/1116/1117)',
                    'persons in households containing an unmarried partner',
                    'children under 18 in those households',
                    'persons in multi-family households (any FAMUNIT > 1)',
                    'other relatives of the householder (RELATE 501/701/901/1001)',
                    'non-relatives other than partners (RELATE 1241/1260)',
                    'group-quarters residents',
                    'UNION: persons in any household above (excl. GQ)'),
    persons_M = c(a[, sum(ASECWT)],
                  a[RELATE %in% PARTNER_CODES, sum(ASECWT)],
                  a[SERIAL %in% partner_hh, sum(ASECWT)],
                  a[SERIAL %in% partner_hh & AGE < 18, sum(ASECWT)],
                  a[SERIAL %in% multifam_hh, sum(ASECWT)],
                  a[RELATE %in% c(501, 701, 901, 1001), sum(ASECWT)],
                  a[RELATE %in% c(1241, 1260), sum(ASECWT)],
                  a[GQ == 2, sum(ASECWT)],
                  a[SERIAL %in% ambiguous, sum(ASECWT)]) / 1e6)
  a5[, pct_of_population := 100 * persons_M / persons_M[1]]
  fwrite(a5, file.path(RESULTS, sprintf('asec_A5_arrangements_%d.csv', ty)))
  print(a5[, .(arrangement, persons_M = round(persons_M, 2),
               pct = round(pct_of_population, 2))])
  rm(full); invisible(gc())
}

message('\nWrote asec_A1/A1b/A2..A6 tables to ', RESULTS)
