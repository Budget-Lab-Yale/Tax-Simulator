#-------------------------------------------------------------------------------
# ded_lever_10yr_summary.R
#
# Ten-year (FY2027-2036) budget comparison for the estate.income_tax_ded
# lever on the Niskanen dr_prim scenario (estate_ded_lever_10yr vintage):
# deduction ON (dr_prim, baseline-law default) vs OFF (dr_prim_noded).
#
# For each leg and run type, deltas vs baseline by fiscal year and component:
#   individual income tax  = revenues_income_tax - outlays_tax_credits
#   estate tax             = revenues_estate_tax
#   other                  = payroll + corp + vat + other
#   total budget effect    = sum of the above (positive = deficit-reducing)
#
# Prints per-FY tables and 10-year totals; writes tidy CSV to
# other/estate_tax/output/ded_lever_10yr_summary.csv.
#
# Usage (from repo root, via sbatch): Rscript other/estate_tax/ded_lever_10yr_summary.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

root   = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/estate_ded_lever_10yr'
window = 2027:2036

read_receipts = function(scenario, rt) {
  # Baseline has no behavior: conventional == static, and the vintage copy
  # carries receipts under static/totals only
  if (scenario == 'baseline') rt = 'static'
  fread(file.path(root, scenario, rt, 'totals', 'receipts.csv')) %>%
    as_tibble() %>%
    filter(year %in% window) %>%
    transmute(
      year,
      iit    = revenues_income_tax - outlays_tax_credits,
      estate = revenues_estate_tax,
      other  = revenues_payroll_tax + revenues_corp_tax + revenues_vat +
               revenues_other,
      total  = iit + estate + other
    )
}

out = list()
for (rt in c('static', 'conventional')) {
  bl = read_receipts('baseline', rt)
  for (sc in c('dr_prim', 'dr_prim_noded')) {
    d = read_receipts(sc, rt) %>%
      inner_join(bl, by = 'year', suffix = c('', '.bl')) %>%
      transmute(run_type = rt,
                scenario = sc,
                year,
                delta_iit    = iit    - iit.bl,
                delta_estate = estate - estate.bl,
                delta_other  = other  - other.bl,
                delta_total  = total  - total.bl)
    out[[paste(rt, sc)]] = d

    cat(sprintf('\n=== %s / %s: deltas vs baseline ($B, FY) ===\n', sc, rt))
    d %>%
      mutate(across(starts_with('delta'), ~ sprintf('%+8.3f', .))) %>%
      select(-run_type, -scenario) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
    cat(sprintf(paste0('  10-yr (FY%d-%d): iit %+.3f | estate %+.3f | ',
                       'other %+.3f | TOTAL %+.3f\n'),
                min(window), max(window),
                sum(d$delta_iit), sum(d$delta_estate),
                sum(d$delta_other), sum(d$delta_total)))
  }
}

# Head-to-head: the deduction's own budget effect (on minus off)
cat('\n=== Deduction effect itself (dr_prim minus dr_prim_noded, $B) ===\n')
for (rt in c('static', 'conventional')) {
  on  = out[[paste(rt, 'dr_prim')]]
  off = out[[paste(rt, 'dr_prim_noded')]]
  cat(sprintf('  %s 10-yr: iit %+.3f | estate %+.3f | total %+.3f\n', rt,
              sum(on$delta_iit)    - sum(off$delta_iit),
              sum(on$delta_estate) - sum(off$delta_estate),
              sum(on$delta_total)  - sum(off$delta_total)))
}

dir.create('other/estate_tax/output', showWarnings = FALSE)
bind_rows(out) %>%
  write_csv('other/estate_tax/output/ded_lever_10yr_summary.csv')
cat('\nWrote other/estate_tax/output/ded_lever_10yr_summary.csv\n')
