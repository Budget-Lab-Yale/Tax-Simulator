#-------------------------------------------------------------------------------
# mtr_range_impact.R
#
# Contains a measurement of how much dropping out-of-range marginal rates on
# gains moves the gains model's cell rates
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/kg_v5_revmax'
AGE_MIN = 18
AGE_MAX = 80

out = map_dfr(2027:2055, function(t) {
  f = file.path(ROOT, 'baseline', 'static', 'detail', paste0(t, '.csv'))
  if (!file.exists(f)) return(NULL)
  d = fread(f, select = c('weight', 'filing_status', 'age1', 'age2', 'kg_lt',
                          'mtr_kg_lt'), showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(age = if_else(filing_status == 2 & !is.na(age2), pmax(age1, age2), age1),
           age_cohort = pmin(pmax(age, AGE_MIN), AGE_MAX),
           kg_pos  = pmax(kg_lt, 0),
           is_rate = !is.na(mtr_kg_lt) & mtr_kg_lt >= 0 & mtr_kg_lt <= 1)

  d %>%
    group_by(age_cohort) %>%
    summarise(tau_all  = sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE) /
                         pmax(sum(weight * kg_pos, na.rm = TRUE), 1e-9),
              tau_kept = sum(weight * kg_pos * mtr_kg_lt * is_rate, na.rm = TRUE) /
                         pmax(sum(weight * kg_pos * is_rate, na.rm = TRUE), 1e-9),
              n_drop   = sum(!is_rate),
              n_gt1    = sum(!is.na(mtr_kg_lt) & mtr_kg_lt > 1),
              max_mtr  = max(mtr_kg_lt, na.rm = TRUE),
              drop_share = sum(weight * kg_pos * !is_rate, na.rm = TRUE) /
                           pmax(sum(weight * kg_pos, na.rm = TRUE), 1e-9),
              .groups = 'drop') %>%
    mutate(year = t, d_tau = tau_kept - tau_all)
})

cat('\n=== worst cells, by movement in the cell rate ===\n')
out %>% arrange(desc(abs(d_tau))) %>% head(15) %>% as.data.frame() %>% print()

cat('\n=== by year: gains-weighted rate over all ages ===\n')
out %>%
  group_by(year) %>%
  summarise(max_abs_d = max(abs(d_tau)),
            n_cells_over_1bp = sum(abs(d_tau) > 1e-4),
            max_mtr = max(max_mtr),
            tot_drop_share = sum(drop_share * (tau_all > 0)) / n()) %>%
  as.data.frame() %>% print()
