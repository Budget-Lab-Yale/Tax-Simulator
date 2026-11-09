#!/usr/bin/env Rscript
# Taxable estate-tax returns: SOI history vs model projection (current law/OBBBA).
# Sources the calibration script for its functions (entrypoint guard prevents
# main() from running), rebuilds the fitted inputs, applies the best-fit
# candidate, and counts death-weighted taxable returns by receipt year.
suppressPackageStartupMessages({library(data.table); library(tidyverse)})

ROOT     = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
TAX_DATA = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline'
MACRO    = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'
SOI      = file.path(ROOT, 'other/estate_tax/estate_tax_filed_2019_2023.csv')
PARAMS   = '/nfs/roberts/scratch/pi_nrs36/jar335/estate_tax_calibration/estate_calibration_parameters.csv'

Sys.setenv(ESTATE_CALIBRATE_NO_MAIN = '1')   # source for functions, don't run main()
source(file.path(ROOT, 'other/estate_tax/calibrate_estate_tax.R'))

# --- best-fit candidate ---
pars = fread(PARAMS) %>% as_tibble() %>% arrange(total_objective)
best = pars[1, ]
rf = best$reporting_form; tf = best$taxable_form
kv = strsplit(best$parameters, ';')[[1]]
par = setNames(as.numeric(sub('.*=', '', kv)), sub('=.*', '', kv))
cat(sprintf('Best candidate: %s / %s\n  params: %s\n', rf, tf, best$parameters))

# --- fitted inputs ---
policy_index = load_policy_index(2017:2035, MACRO)
soi_targets  = load_soi_targets(SOI, policy_index)
taxable_fits = fit_taxable_fraction_models(soi_targets)
dsue_table   = build_dsue_table(soi_targets)

# --- model projection: current law (OBBBA, $15M indexed), per record ---
death_years = 2021:2034
cells = load_tax_data_cells(TAX_DATA, death_years, wealth_cells = 0)
modeled = apply_candidate(cells, rf, par, tf, taxable_fits, dsue_table,
                          'obbba', policy_index)
proj = modeled %>%
  filter(taxable) %>%
  group_by(receipt_year) %>%
  summarise(taxable_returns = sum(expected_weight, na.rm = TRUE), .groups = 'drop')

# --- history: SOI actual taxable returns (size_bin == 'all') ---
soi = fread(SOI) %>% as_tibble()
hist = soi %>%
  filter(tax_status == 'taxable', size_bin == 'all') %>%
  transmute(death_year = as.integer(year) - 1L,
            taxable_returns = as.numeric(gross_estate_for_tax_purposes_n))

cat('\n===== SOI HISTORY (actual taxable returns) =====\n')
hist %>% arrange(death_year) %>%
  mutate(line = sprintf('  death %d: %s', death_year, format(round(taxable_returns), big.mark=','))) %>%
  pull(line) %>% cat(sep='\n'); cat('\n')

cat('\n===== MODEL PROJECTION (current law / OBBBA $15M indexed) =====\n')
proj %>% arrange(receipt_year) %>%
  mutate(line = sprintf('  receipt FY%d (deaths %d): %s', receipt_year, receipt_year-1,
                        format(round(taxable_returns), big.mark=','))) %>%
  pull(line) %>% cat(sep='\n'); cat('\n')
