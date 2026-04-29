#----------------------------------------------------------------------
# Compares 1040 line totals between baseline (current Tax-Data vintage,
# 2026030513) and new_baseline (new vintage 2026042815) for the
# tax_data_interface_regression test. The new vintage retargets record
# weights and data on age x marital-status cells, so item-level changes
# are expected but should be modest.
#
# Usage:
#   Rscript other/analysis_scripts/tax_data_interface_regression.R [vintage]
#
# Default vintage: interface_regression_2026042815
#----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

args = commandArgs(trailingOnly = TRUE)
vintage = if (length(args) >= 1) args[1] else 'interface_regression_2026042815'

output_root = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
base_path = file.path(output_root, vintage, 'baseline/static/totals/1040.csv')
new_path  = file.path(output_root, vintage, 'new_baseline/static/totals/1040.csv')

stopifnot(file.exists(base_path), file.exists(new_path))

base = read_csv(base_path, show_col_types = FALSE) %>% mutate(scenario = 'old_vintage')
new  = read_csv(new_path,  show_col_types = FALSE) %>% mutate(scenario = 'new_vintage')

long = bind_rows(base, new) %>%
  pivot_longer(-c(year, scenario), names_to = 'item', values_to = 'value') %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    diff     = new_vintage - old_vintage,
    pct_diff = if_else(abs(old_vintage) > 1e-6,
                       100 * (new_vintage - old_vintage) / old_vintage,
                       NA_real_)
  )

# Strip count variables (n_*) from the headline view -- keep dollar lines
dollar_diffs = long %>%
  filter(!grepl('^n_', item))

cat('\n========== TOP 20 ABSOLUTE DOLLAR DIFFS BY YEAR ==========\n')
dollar_diffs %>%
  group_by(year) %>%
  slice_max(abs(diff), n = 20) %>%
  arrange(year, desc(abs(diff))) %>%
  mutate(across(c(old_vintage, new_vintage, diff), ~ round(., 1)),
         pct_diff = round(pct_diff, 2)) %>%
  print(n = Inf)

cat('\n========== TOP 20 ABSOLUTE PCT DIFFS BY YEAR (|old| >= $1B) ==========\n')
dollar_diffs %>%
  filter(abs(old_vintage) >= 1000) %>%
  group_by(year) %>%
  slice_max(abs(pct_diff), n = 20) %>%
  arrange(year, desc(abs(pct_diff))) %>%
  mutate(across(c(old_vintage, new_vintage, diff), ~ round(., 1)),
         pct_diff = round(pct_diff, 2)) %>%
  print(n = Inf)

cat('\n========== HEADLINE 1040 ITEMS ==========\n')
headline_items = c('n_returns', 'wages', 'agi', 'txbl_inc', 'std_ded',
                   'item_ded', 'liab_iit', 'liab_iit_net', 'eitc',
                   'ctc_ref', 'ctc_nonref', 'liab_amt', 'liab_niit')
long %>%
  filter(item %in% headline_items) %>%
  mutate(across(c(old_vintage, new_vintage, diff), ~ round(., 1)),
         pct_diff = round(pct_diff, 2)) %>%
  arrange(year, item) %>%
  print(n = Inf)

cat('\nDone. Full results in long-format object `long`.\n')
