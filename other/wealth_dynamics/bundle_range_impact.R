#-------------------------------------------------------------------------------
# bundle_range_impact.R
#
# Contains a measurement of how much dropping out-of-range capital-income bundle
# rates moves the wealth bathtub's cell rates
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

ROOT = paste0('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/',
              'warren_nd_30yr/nickeldime/conventional_no_wealth/detail')

out = map_dfr(c(2026, 2030, 2035, 2040, 2045), function(t) {
  f = file.path(ROOT, paste0(t, '.csv'))
  if (!file.exists(f)) return(NULL)

  # Cut cells the way wealth_dyn_assign_cells does: age cohort by within-age net
  # worth percentile, dropping records at or below zero net worth.
  d = fread(f, select = c('weight', 'filing_status', 'age1', 'age2',
                          'net_worth_raw', 'cap_bundle_F', 'mtr_cap_bundle'),
            showProgress = FALSE) %>%
    as_tibble() %>%
    filter(net_worth_raw > 0) %>%
    mutate(age = if_else(filing_status == 2 & !is.na(age2), pmax(age1, age2), age1),
           age_cohort = pmin(pmax(age, 18), 80),
           F_pos   = pmax(cap_bundle_F, 0),
           is_rate = !is.na(mtr_cap_bundle) &
                     mtr_cap_bundle >= 0 & mtr_cap_bundle <= 1) %>%
    group_by(age_cohort) %>%
    mutate(bin = ntile(net_worth_raw, 100)) %>%
    ungroup()

  d %>%
    group_by(age_cohort, bin) %>%
    summarise(tau_all  = sum(weight * F_pos * mtr_cap_bundle, na.rm = TRUE) /
                         pmax(sum(weight * F_pos, na.rm = TRUE), 1e-9),
              tau_kept = sum(weight * F_pos * mtr_cap_bundle * is_rate, na.rm = TRUE) /
                         pmax(sum(weight * F_pos * is_rate, na.rm = TRUE), 1e-9),
              n_drop   = sum(!is_rate),
              n_cell   = n(),
              .groups  = 'drop') %>%
    mutate(year = t, d_tau = tau_kept - tau_all)
})

cat('\n=== cells by movement in the capital-income rate ===\n')
out %>% arrange(desc(abs(d_tau))) %>% head(20) %>% as.data.frame() %>% print()

cat('\n=== how many cells move, by year ===\n')
out %>%
  group_by(year) %>%
  summarise(n_cells       = n(),
            over_1pt      = sum(abs(d_tau) > 0.01),
            over_5pt      = sum(abs(d_tau) > 0.05),
            over_20pt     = sum(abs(d_tau) > 0.20),
            median_abs_d  = median(abs(d_tau)),
            max_abs_d     = max(abs(d_tau)),
            tau_all_max   = max(tau_all)) %>%
  as.data.frame() %>% print()
