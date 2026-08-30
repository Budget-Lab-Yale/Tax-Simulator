#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 12_aging_check.R  (group D stage D, proposal 3)
#
# Tax-Data consumes ONE base-year non-filer file and ages it forward to 2097.
# Nobody has ever checked what that aging produces against a file built
# directly in the target year, because until this week we could not build one.
# Now we can build any year 2014-2022, so the check is available and this is it.
#
# For each test year: age the TY2017 pool forward exactly as Tax-Data would,
# then compare against the pool built directly in that year.
#
# THE AGING PATH, replicated from Tax-Data src/project_puf.R:
#   weights  compute_weights_for_year()'s NON-FILER branch -- pure demography.
#            2018-19: population factors by marital status only, vs 2017.
#            2020+  : start from the 2019 weights, then married x age vs 2019,
#                     averaged over the persons in the unit, with a 0.99
#                     factor on under-18s.
#   values   factor_ledger, which is cumulative from 2017 per variable. It has
#            NO filer dimension, so non-filer income grows at filer-derived
#            rates -- one of the defects this comparison is meant to size.
#
# WHY THESE YEARS. Each breaks a different assumption aging makes.
#   2018  TCJA nearly doubled the standard deduction, moving the filing
#         threshold itself. No growth factor derived from 2017 relationships
#         can represent a change in the RULE; a direct build can, because the
#         threshold is computed from law.
#   2021  Stimulus payments and advance child credits drove millions of
#         habitual non-filers to file. That is a change in the POPULATION, not
#         in its incomes, and no income factor represents it either.
#   2019, 2020  Ordinary years, included as controls: if built and aged also
#         disagree here, the disagreement is not about TCJA or the pandemic.
#   2022  The longest span, and already built.
#
# Disagreement IS the finding. Its size per year is what says how often this
# file has to be rebuilt rather than aged.
#
# Writes: results/aging_check_{year}.csv, results/aging_check_summary.csv
#
#   Rscript research/state_weights/nonfiler_pool/12_aging_check.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(readr); library(yaml)
})
source('src/data/state_weights.R')

args      <- commandArgs(trailingOnly = TRUE)
TEST_YEARS <- if (length(args)) as.integer(args) else c(2018L, 2019L, 2020L, 2021L, 2022L)
RES       <- 'research/state_weights/nonfiler_pool/results'
BASE_YEAR <- 2017L
MAX_AGE   <- 85L        # project_puf caps the demographic join here

# Vintages pinned on purpose -- the drift measured here is against THIS
# Tax-Data build's ledgers. Paths resolve through output_roots.yaml.
TAXDATA_VINTAGE <- c(model = 'Tax-Data',          version = 'v1', vintage = '2026081216')
MACRO_VINTAGE   <- c(model = 'Macro-Projections', version = 'v3', vintage = '2026071916')
TAXDATA <- model_data_path(TAXDATA_VINTAGE[['model']], TAXDATA_VINTAGE[['version']],
                           TAXDATA_VINTAGE[['vintage']], 'baseline')
MACRO   <- model_data_path(MACRO_VINTAGE[['model']], MACRO_VINTAGE[['version']],
                           MACRO_VINTAGE[['vintage']], 'baseline')

#-------------------------------------------------------------------------------
# Demographic population factors, rebuilt as project_puf.R builds them
#-------------------------------------------------------------------------------
macro <- rbindlist(list(
  as.data.table(read_csv(file.path(MACRO, 'historical.csv'), show_col_types = FALSE)),
  as.data.table(read_csv(file.path(MACRO, 'projections.csv'), show_col_types = FALSE))
), fill = TRUE)

demog <- melt(macro[, c('year', grep('^(un)?married_', names(macro), value = TRUE)),
                    with = FALSE],
              id.vars = 'year', variable.name = 'k', value.name = 'n')
demog[, `:=`(married = as.integer(!grepl('^unmarried', k)),
             age     = pmin(MAX_AGE, as.integer(sub('.*_', '', as.character(k)))))]
demog <- demog[, .(n = sum(n)), by = .(year, married, age)]

# 2018-19: marital status only, relative to 2017
pf_1819 <- demog[year %in% 2017:2019, .(n = sum(n)), by = .(year, married)]
pf_1819[, population_factor := n / n[year == 2017L], by = married]

# 2020+: married x age, relative to 2019
pf_2020 <- demog[year >= 2019]
pf_2020[, population_factor := fifelse(n > 0, n / n[year == 2019L], 1),
        by = .(married, age)]

#-------------------------------------------------------------------------------
# Value growth: Tax-Data's own factor ledger, cumulative from 2017
#-------------------------------------------------------------------------------
fl <- as.data.table(readRDS(file.path(TAXDATA, 'factor_ledger.rds')))

#' Age one variable's values from 2017 to `year`. Variables the ledger does not
#' carry are left UNGROWN and reported, never silently grown at some default.
age_factor <- function(var_name, to_year) {
  # distinct argument names: `variable` and `year` are also COLUMNS of fl, and
  # data.table resolves the column first
  v <- fl[year == to_year & variable == var_name, factor]
  if (length(v) == 1L && is.finite(v)) v else NA_real_
}

#-------------------------------------------------------------------------------
# Age the base-year pool
#-------------------------------------------------------------------------------
pool_path <- function(y) file.path(RES, sprintf('nonfiler_pool_%d.csv.gz', y))
base <- fread(pool_path(BASE_YEAR))

age_pool <- function(target_year) {
  p <- copy(base)
  p[, married := as.integer(filing_status == 2)]

  # --- weights, the non-filer branch of compute_weights_for_year -------------
  if (target_year <= 2019L) {
    f <- pf_1819[year == target_year, .(married, population_factor)]
    p <- merge(p, f, by = 'married', all.x = TRUE)
    p[, w_aged := weight * population_factor][, population_factor := NULL]
  } else {
    f19 <- pf_1819[year == 2019L, .(married, pf19 = population_factor)]
    p   <- merge(p, f19, by = 'married', all.x = TRUE)
    p[, w2019 := weight * pf19]
    # married x age, averaged over the unit's persons. Tax-Data selects
    #   select(id, married_flag, age1, age2, starts_with('dep_age'))
    # and takes mean(weight) over ALL of those slots, so the DEPENDENT slots
    # are in the average too and the divisor is up to five, not two. Leaving
    # them out changes w_aged for every unit that has a dependent -- 16.1% of
    # the emitted weight -- which is why they are here.
    #
    # REPLICATED QUIRK, deliberately. `dep_age_group1-3` hold GROUP CODES 1-4,
    # not ages: impute_nonfilers.R samples them from c(1,2,3,4). Tax-Data
    # pivots them into the same `age` column it then joins to
    # population_factors_2020plus on, so a dependent is matched to the
    # population factor for age 1, 2, 3 or 4 and picks up the under-18 0.99
    # factor. That is what production does, and this script exists to measure
    # production, so it is copied rather than corrected. Flagged for the
    # Tax-Data branch review; see NONFILER_BRANCH_NOTES.md.
    fy <- pf_2020[year == target_year, .(married, age, population_factor)]
    dep_cols <- grep('^dep_age', names(p), value = TRUE)
    stopifnot(length(dep_cols) == 3L)
    ppl <- melt(p[, c('id', 'married', 'w2019', 'age1', 'age2', dep_cols),
                  with = FALSE],
                id.vars = c('id', 'married', 'w2019'),
                measure.vars = c('age1', 'age2', dep_cols),
                variable.name = 'slot', value.name = 'age', na.rm = TRUE)
    ppl <- ppl[age > 0]
    # heads and spouses carry the unit's marital status; dependants sit in the
    # unmarried cells, exactly as project_puf assigns them
    ppl[, married := fifelse(slot %in% c('age1', 'age2'), married, 0L)]
    ppl[, age := pmin(MAX_AGE, as.integer(age))]
    ppl <- merge(ppl, fy, by = c('married', 'age'), all.x = TRUE)
    ppl[is.na(population_factor), population_factor := 1]
    ppl[, w := w2019 * population_factor * fifelse(age < 18, 0.99, 1)]
    agg <- ppl[, .(w_aged = mean(w)), by = id]
    p <- merge(p, agg, by = 'id', all.x = TRUE)
    p[is.na(w_aged), w_aged := w2019]
  }

  # --- values ----------------------------------------------------------------
  money <- c('wages', 'txbl_int', 'qual_div', 'txbl_pens_dist', 'gross_ss',
             'sole_prop', 'kg_lt', 'ui')
  money <- intersect(money, names(p))
  ungrown <- character(0)
  for (v in money) {
    fac <- age_factor(v, target_year)
    if (is.na(fac)) { ungrown <- c(ungrown, v); next }
    set(p, j = v, value = p[[v]] * fac)
  }
  attr(p, 'ungrown') <- ungrown
  p
}

#-------------------------------------------------------------------------------
# Compare
#-------------------------------------------------------------------------------
summarise_pool <- function(p, wcol) {
  w  <- p[[wcol]]
  ad <- w * (1 + (p$filing_status == 2))          # adults, the target's unit
  data.table(
    adults_M     = sum(ad) / 1e6,
    units_M      = sum(w) / 1e6,
    wages_B      = sum(w * p$wages) / 1e9,
    ss_B         = sum(w * p$gross_ss) / 1e9,
    pens_B       = sum(w * p$txbl_pens_dist) / 1e9,
    int_B        = sum(w * p$txbl_int) / 1e9,
    pct_wages    = 100 * sum(w * (p$wages != 0)) / sum(w),
    pct_int      = 100 * sum(w * (p$txbl_int != 0)) / sum(w),
    pct_ss       = 100 * sum(w * (p$gross_ss != 0)) / sum(w),
    mean_age     = sum(w * p$age1) / sum(w),
    pct_65p      = 100 * sum(w * (p$age1 >= 65)) / sum(w))
}

summary_rows <- list()
for (y in TEST_YEARS) {
  message('=== TY', y)
  bp <- pool_path(y)
  if (!file.exists(bp)) {
    message(sprintf('  no directly-built pool for %d -- run 01->05 for that year', y))
    next
  }
  built <- fread(bp)
  aged  <- age_pool(y)
  ung   <- attr(aged, 'ungrown')
  if (length(ung)) {
    message('  NOT grown (absent from factor_ledger): ', paste(ung, collapse = ', '))
  }

  a <- summarise_pool(aged,  'w_aged')
  b <- summarise_pool(built, 'weight')
  cmp <- data.table(metric = names(a), aged = unlist(a), built = unlist(b))
  cmp[, `:=`(diff = built - aged, pct = 100 * (built / aged - 1))]
  fwrite(cmp, file.path(RES, sprintf('aging_check_%d.csv', y)))

  for (i in seq_len(nrow(cmp))) {
    message(sprintf('  %-11s aged %10.2f | built %10.2f | %+8.1f%%',
                    cmp$metric[i], cmp$aged[i], cmp$built[i], cmp$pct[i]))
  }
  summary_rows[[as.character(y)]] <- cbind(tax_year = y, cmp)
}

if (length(summary_rows)) {
  s <- rbindlist(summary_rows)
  fwrite(s, file.path(RES, 'aging_check_summary.csv'))
  message('\n=== how far aging drifts, by year (built vs aged)')
  w <- dcast(s[metric %in% c('adults_M', 'wages_B', 'pct_65p')],
             tax_year ~ metric, value.var = 'pct')
  for (i in seq_len(nrow(w))) {
    message(sprintf('  TY%d  adults %+6.1f%%  wages %+7.1f%%  share 65+ %+6.1f%%',
                    w$tax_year[i], w$adults_M[i], w$wages_B[i], w$pct_65p[i]))
  }
  message('  wrote aging_check_summary.csv')
}
