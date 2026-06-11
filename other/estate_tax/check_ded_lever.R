#-------------------------------------------------------------------------------
# check_ded_lever.R
#
# With/without comparison for the estate.income_tax_ded law lever
# (estate_ded_lever vintage: dr_prim = lever on at the baseline default,
# dr_prim_noded = lever overridden to 0).
#
#   1. Lever default is a pure refactor: dr_prim estate totals reproduce the
#      niskanen_estate_ded_pilot dr_prim leg exactly.
#   2. Lever off restores pre-deduction behavior: dr_prim_noded estate
#      columns match baseline record-by-record; totals match exactly.
#   3. The lever touches the estate side only: dr_prim and dr_prim_noded
#      have identical 1040 totals and identical estate_income_tax_ded
#      stamps; their estate difference is the deduction effect, reported
#      per year (totals + receipts).
#
# Usage (from repo root, via sbatch): Rscript other/estate_tax/check_ded_lever.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

root  = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/estate_ded_lever'
pilot = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/niskanen_estate_ded_pilot'
years = 2026:2029

fails = 0
check = function(label, cond) {
  status = if (isTRUE(cond)) 'PASS' else 'FAIL'
  if (!isTRUE(cond)) fails <<- fails + 1
  cat(sprintf('[%s] %s\n', status, label))
}

est = function(base, scenario, rt) {
  fread(file.path(base, scenario, rt, 'totals', 'estate.csv')) %>%
    as_tibble() %>%
    filter(year %in% years)
}

#---------------------------
# 1. Lever default reproduces the pilot leg
#---------------------------

for (rt in c('static', 'conventional')) {
  check(paste0('dr_prim ', rt, ' estate totals == pilot leg (lever default)'),
        identical(est(root, 'dr_prim', rt)$est_tax_exp,
                  est(pilot, 'dr_prim', rt)$est_tax_exp))
}

#---------------------------
# 2. Lever off restores baseline estate results
#---------------------------

bl = est(root, 'baseline', 'static')
for (rt in c('static', 'conventional')) {
  nd = est(root, 'dr_prim_noded', rt)
  check(paste0('dr_prim_noded ', rt, ' estate totals == baseline (exact)'),
        max(abs(nd$est_tax_exp - bl$est_tax_exp)) == 0 &&
          max(abs(nd$est_returns - bl$est_returns)) == 0)
}

for (y in years) {
  bl_d = fread(file.path(root, 'baseline/static/detail', paste0(y, '.csv')),
               select = c('id', 'liab_estate_nodsue', 'liab_estate_dsue',
                          'estate_distributable')) %>% as_tibble()
  nd_d = fread(file.path(root, 'dr_prim_noded/static/detail', paste0(y, '.csv')),
               select = c('id', 'liab_estate_nodsue', 'liab_estate_dsue',
                          'estate_distributable')) %>% as_tibble()
  j = inner_join(nd_d, bl_d, by = 'id', suffix = c('', '.bl'))
  check(paste0(y, ' dr_prim_noded estate columns == baseline record-by-record'),
        nrow(j) == nrow(nd_d) &&
          max(abs(j$liab_estate_nodsue - j$liab_estate_nodsue.bl)) == 0 &&
          max(abs(j$liab_estate_dsue   - j$liab_estate_dsue.bl))   == 0)
}

#---------------------------
# 3. Income side identical between legs; report the deduction effect
#---------------------------

for (rt in c('static', 'conventional')) {
  a = fread(file.path(root, 'dr_prim',       rt, 'totals', '1040.csv'))
  b = fread(file.path(root, 'dr_prim_noded', rt, 'totals', '1040.csv'))
  check(paste0('dr_prim vs dr_prim_noded ', rt, ' 1040 totals identical'),
        identical(a, b))
}

for (y in setdiff(years, min(years))) {
  a = fread(file.path(root, 'dr_prim/static/detail', paste0(y, '.csv')),
            select = c('id', 'estate_income_tax_ded')) %>% as_tibble()
  b = fread(file.path(root, 'dr_prim_noded/static/detail', paste0(y, '.csv')),
            select = c('id', 'estate_income_tax_ded')) %>% as_tibble()
  check(paste0(y, ' estate_income_tax_ded stamps identical across legs'),
        identical(a, b))
}

cat('\n--- Deduction effect (lever on vs off), estate totals ($B) ---\n')
for (rt in c('static', 'conventional')) {
  on  = est(root, 'dr_prim', rt)
  off = est(root, 'dr_prim_noded', rt)
  cat(sprintf('  %s: %s\n', rt,
              paste(sprintf('%d: %+.3f', on$year,
                            on$est_tax_exp - off$est_tax_exp), collapse = ', ')))
}

cat('--- Deduction effect, estate receipts by FY ($B) ---\n')
for (rt in c('static', 'conventional')) {
  on  = fread(file.path(root, 'dr_prim',       rt, 'totals/receipts.csv'),
              select = c('year', 'revenues_estate_tax'))
  off = fread(file.path(root, 'dr_prim_noded', rt, 'totals/receipts.csv'),
              select = c('year', 'revenues_estate_tax'))
  d = inner_join(as_tibble(on), as_tibble(off), by = 'year',
                 suffix = c('.on', '.off')) %>%
    mutate(delta = revenues_estate_tax.on - revenues_estate_tax.off)
  cat(sprintf('  %s: %s\n', rt,
              paste(sprintf('FY%d: %+.3f', d$year, d$delta), collapse = ', ')))
}

cat(sprintf('\n%s: %d failure(s)\n', if (fails == 0) 'ALL PASS' else 'FAILED',
            fails))
quit(status = as.integer(fails > 0))
