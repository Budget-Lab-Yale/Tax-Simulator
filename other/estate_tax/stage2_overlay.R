#-------------------------------------------------------------------------------
# stage2_overlay.R
#
# Stage-2 validation overlay: compares heir-level estate tax profiles
# (tax/inheritance by inheritance size) across three sources:
#   1. OLD upstream Estate-Tax-Distribution liability (vintage 2025092512,
#      pre-OBBBA exemption assumptions)
#   2. Model-BASELINE λ from the rank-matching allocator (OBBBA $15M, 2026)
#   3. Model-SUNSET λ (tests/estate_sunset, $7.2M 2026 — closest in spirit to
#      the old file's pre-OBBBA assumption, so levels should be roughly
#      comparable there; vs baseline only SHAPE should agree)
#
# Run via sbatch after the estate_stage2 pipeline completes.
#-------------------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(data.table)

setwd(Sys.getenv('TAXSIM_ROOT', unset = '.'))
source('./src/data/post_processing/estate_allocator.R')

yr       = 2026
old_path = file.path('/nfs/roberts/project/pi_nrs36/shared/model_data',
                     'Estate-Tax-Distribution/v1/2025092512/baseline',
                     paste0('estate_tax_detail_', yr, '.csv'))
out_root = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/estate_stage2'

old = fread(old_path) %>% tibble()
baseline_detail = fread(file.path(out_root, 'baseline/static/detail',
                                  paste0(yr, '.csv'))) %>% tibble()
sunset_file = fread(file.path(out_root, 'estate_sunset/static/supplemental',
                              paste0('estate_tax_detail_', yr, '.csv'))) %>% tibble()

heir_px = old %>% select(id, p_inheritance, inheritance)
alloc_baseline = allocate_estate_to_heirs(baseline_detail, heir_px, yr, 'baseline')

profiles = old %>%
  select(id, p_inheritance, inheritance, liab_old = estate_tax_liability) %>%
  left_join(alloc_baseline$heirs %>% rename(liab_model_baseline = estate_tax_liability),
            by = 'id') %>%
  left_join(sunset_file %>% select(id, liab_model_sunset = estate_tax_liability),
            by = 'id') %>%
  left_join(baseline_detail %>% select(id, weight), by = 'id') %>%
  filter(!is.na(weight), inheritance > 0)

# Heir-level profile by inheritance size bin (weighted by w*p = expected heirs)
cat('\n=== tax/inheritance profile by inheritance bin (means weighted by w*p) ===\n')
profiles %>%
  mutate(
    bin = cut(inheritance, c(0, 1e6, 2.5e6, 5e6, 7.5e6, 10e6, 25e6, 50e6, Inf),
              labels = c('<1M', '1-2.5M', '2.5-5M', '5-7.5M', '7.5-10M',
                         '10-25M', '25-50M', '50M+'), right = FALSE),
    wp = weight * p_inheritance
  ) %>%
  group_by(bin) %>%
  summarise(
    exp_heirs    = sum(wp),
    rate_old     = weighted.mean(liab_old / inheritance, wp),
    rate_base    = weighted.mean(liab_model_baseline / inheritance, wp),
    rate_sunset  = weighted.mean(liab_model_sunset / inheritance, wp),
    tax_old_b    = sum(wp * liab_old) / 1e9,
    tax_base_b   = sum(wp * liab_model_baseline) / 1e9,
    tax_sunset_b = sum(wp * liab_model_sunset) / 1e9
  ) %>%
  print(n = Inf, width = Inf)

cat('\n=== summary ===\n')
profiles %>%
  summarise(
    across(starts_with('liab'), list(
      taxed_ids = ~ sum(. > 0),
      cutoff    = ~ min(inheritance[. > 0]),
      max_ratio = ~ max(. / inheritance),
      total_b   = ~ sum(weight * p_inheritance * .) / 1e9
    ))
  ) %>%
  pivot_longer(everything()) %>%
  print(n = Inf)
