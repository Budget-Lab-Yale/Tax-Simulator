#-- Diagnose the rev_corp base the on-model rate module used vs baseline receipts
suppressMessages({library(dplyr); library(tidyr); library(readr); library(purrr); library(stringr); library(magrittr)})

staging = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/corp_rate_smoke/_slurm_staging'
source('src/slurm/common.R')
reconstitute_environment(staging)

macro_root = get_scenario_info('corp_rate_28')$interface_paths$`Macro-Projections`
cat('macro_root =', macro_root, '\n')

mac = read_macro_spliced(macro_root)
cat('\n-- duplicate years in spliced macro? --\n')
dups = mac %>% count(year) %>% filter(n > 1)
print(dups)

cat('\n-- rev_corp by year (all rows, 2026-2034) --\n')
print(mac %>% filter(year >= 2026, year <= 2034) %>% select(year, rev_corp))

cat('\n-- revenues_other rev_corp as calc_receipts builds it --\n')
ro = mac %>% select(year, revenues_corp_tax = rev_corp)
print(ro %>% filter(year >= 2026, year <= 2034))

cat('\n-- corp_rate_delta STATIC as the module computes it --\n')
rs = corp_rate_read_series(file.path(
  '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/corp_rate_smoke',
  'corp_rate_28', 'static/supplemental/tax_law.csv'))
cat('rate_series rows:', nrow(rs), ' (dup years:', anyDuplicated(rs$year) > 0, ')\n')
d = corp_rate_delta(rs, ro %>% select(year, rev_corp = revenues_corp_tax), static = TRUE)
print(d %>% filter(year >= 2026, year <= 2034))
cat('\nnrow(delta) =', nrow(d), ' distinct years =', n_distinct(d$year), '\n')
