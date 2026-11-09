#-------------------------------------------------------------------------------
# check_ded_pilot.R
#
# Acceptance checks for the deemed-tax-deductible-against-estate pilot
# (niskanen_estate_ded_pilot vintage; see niskanen_estate_ded_pilot.sbatch).
#
#   1. co_prim (carryover: no tax at death => no deduction): estate totals
#      exactly equal baseline's; detail estate columns match baseline
#      record-by-record; no estate_income_tax_ded column in detail.
#   2. dr_prim (deemed realization): estate totals fall vs baseline every
#      year; the fall operates only through records with a positive
#      conditional deemed tax (estate_income_tax_ded > 0); liability never
#      rises at the record level.
#   3. Receipts: dr_prim revenues_estate_tax delta vs baseline is ~0 in the
#      first FY and negative thereafter (death-year CY t books FY t+1).
#
# Estate columns are RNG-free (pure wealth + law), so record-level joins to
# the baseline copied from the niskanen_housing vintage are exact despite
# the pilot's different sample_ids ordering. Exits nonzero on any failure.
#
# Usage (from repo root, via sbatch): Rscript other/estate_tax/check_ded_pilot.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

root  = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/niskanen_estate_ded_pilot'
years = 2026:2029

# Reform effective 1/1/2027; 2026 is the law-identical lead-in year (no
# deemed realization => no deduction => dr_prim estate == baseline exactly)
effective = 2027

fails = 0
check = function(label, cond) {
  status = if (isTRUE(cond)) 'PASS' else 'FAIL'
  if (!isTRUE(cond)) fails <<- fails + 1
  cat(sprintf('[%s] %s\n', status, label))
}

read_totals = function(scenario, run_type) {
  fread(file.path(root, scenario, run_type, 'totals', 'estate.csv')) %>%
    as_tibble() %>%
    filter(year %in% years)
}

bl = read_totals('baseline', 'static')

#---------------------------
# 1. Totals: co_prim invariance, dr_prim decline
#---------------------------

for (rt in c('static', 'conventional')) {
  co = read_totals('co_prim', rt)
  dr = read_totals('dr_prim', rt)
  check(paste0('co_prim ', rt, ' est_tax_exp == baseline (exact)'),
        max(abs(co$est_tax_exp - bl$est_tax_exp)) == 0)
  check(paste0('dr_prim ', rt, ' est_tax_exp == baseline pre-effective'),
        all((dr$est_tax_exp == bl$est_tax_exp)[dr$year < effective]))
  check(paste0('dr_prim ', rt, ' est_tax_exp < baseline from ', effective),
        all((dr$est_tax_exp < bl$est_tax_exp)[dr$year >= effective]))
  cat(sprintf('  dr_prim %s deltas ($B): %s\n', rt,
              paste(sprintf('%d: %+.3f', dr$year,
                            dr$est_tax_exp - bl$est_tax_exp), collapse = ', ')))
}

#---------------------------
# 2. Record-level detail (static, all pilot years)
#---------------------------

for (y in years) {
  bl_d = fread(file.path(root, 'baseline/static/detail', paste0(y, '.csv')),
               select = c('id', 'liab_estate_nodsue', 'liab_estate_dsue',
                          'estate_p_dsue', 'estate_distributable')) %>%
    as_tibble()

  co_d = fread(file.path(root, 'co_prim/static/detail', paste0(y, '.csv'))) %>%
    as_tibble()
  check(paste0(y, ' co_prim detail has no estate_income_tax_ded column'),
        !('estate_income_tax_ded' %in% names(co_d)))
  j = co_d %>%
    select(id, liab_estate_nodsue, liab_estate_dsue, estate_distributable) %>%
    inner_join(bl_d, by = 'id', suffix = c('', '.bl'))
  check(paste0(y, ' co_prim estate columns == baseline record-by-record'),
        nrow(j) == nrow(co_d) &&
          max(abs(j$liab_estate_nodsue - j$liab_estate_nodsue.bl)) == 0 &&
          max(abs(j$liab_estate_dsue   - j$liab_estate_dsue.bl))   == 0)

  dr_d = fread(file.path(root, 'dr_prim/static/detail', paste0(y, '.csv'))) %>%
    as_tibble()
  if (y < effective) {
    # Lead-in year: no deemed realization anywhere => dead-leg block skipped
    check(paste0(y, ' dr_prim detail has no estate_income_tax_ded (lead-in)'),
          !('estate_income_tax_ded' %in% names(dr_d)))
    next
  }
  check(paste0(y, ' dr_prim detail has estate_income_tax_ded column'),
        'estate_income_tax_ded' %in% names(dr_d))
  if (!('estate_income_tax_ded' %in% names(dr_d))) next

  j = dr_d %>%
    select(id, weight, estate_m, estate_income_tax_ded,
           liab_estate_nodsue, liab_estate_dsue, estate_distributable) %>%
    inner_join(bl_d, by = 'id', suffix = c('', '.bl')) %>%
    mutate(drop_nodsue = liab_estate_nodsue.bl - liab_estate_nodsue,
           drop_dsue   = liab_estate_dsue.bl   - liab_estate_dsue)

  check(paste0(y, ' dr_prim estate_distributable unchanged (fixed-wealth)'),
        max(abs(j$estate_distributable - j$estate_distributable.bl)) == 0)
  check(paste0(y, ' dr_prim liability never rises'),
        min(j$drop_nodsue) >= 0 && min(j$drop_dsue) >= 0)
  check(paste0(y, ' dr_prim liability falls ONLY where ded > 0'),
        all(j$drop_nodsue[j$estate_income_tax_ded == 0] == 0) &&
          all(j$drop_dsue[j$estate_income_tax_ded == 0] == 0))
  check(paste0(y, ' some deduction-driven drops exist'),
        any(j$drop_nodsue > 0))

  aff = j %>% filter(estate_income_tax_ded > 0, liab_estate_nodsue.bl > 0)
  if (nrow(aff) > 0) {
    cat(sprintf(paste0('  %d: %s records with ded>0 (%s taxable); E[ded] = ',
                       '$%.2fB; drop/ded on taxable: p25 %.3f / med %.3f / ',
                       'p75 %.3f / max %.3f\n'),
                y,
                format(sum(j$estate_income_tax_ded > 0), big.mark = ','),
                format(nrow(aff), big.mark = ','),
                sum(j$weight * j$estate_m * j$estate_income_tax_ded) / 1e9,
                quantile(aff$drop_nodsue / aff$estate_income_tax_ded, .25),
                median(aff$drop_nodsue / aff$estate_income_tax_ded),
                quantile(aff$drop_nodsue / aff$estate_income_tax_ded, .75),
                max(aff$drop_nodsue / aff$estate_income_tax_ded)))
  }
}

#---------------------------
# 3. Receipts: estate delta lagged one FY
#---------------------------

bl_r = fread(file.path(root, 'baseline/static/totals/receipts.csv'),
             select = c('year', 'revenues_estate_tax')) %>% as_tibble()
for (rt in c('static', 'conventional')) {
  dr_r = fread(file.path(root, 'dr_prim', rt, 'totals/receipts.csv'),
               select = c('year', 'revenues_estate_tax')) %>%
    as_tibble() %>%
    inner_join(bl_r, by = 'year', suffix = c('', '.bl')) %>%
    mutate(delta = revenues_estate_tax - revenues_estate_tax.bl)
  cat(sprintf('  dr_prim %s estate receipts delta ($B): %s\n', rt,
              paste(sprintf('FY%d: %+.3f', dr_r$year, dr_r$delta),
                    collapse = ', ')))
  check(paste0('dr_prim ', rt, ' FY', min(dr_r$year),
               ' estate receipts delta ~ 0 (lag)'),
        abs(dr_r$delta[which.min(dr_r$year)]) < 1e-6)
  later = dr_r %>% filter(year > min(year), year <= max(years) + 1)
  check(paste0('dr_prim ', rt, ' estate receipts delta < 0 from FY',
               min(dr_r$year) + 1),
        all(later$delta < 0))
}

cat(sprintf('\n%s: %d failure(s)\n', if (fails == 0) 'ALL PASS' else 'FAILED',
            fails))
quit(status = as.integer(fails > 0))
