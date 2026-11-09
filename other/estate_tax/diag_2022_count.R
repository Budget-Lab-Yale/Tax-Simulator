#!/usr/bin/env Rscript
# Diagnostic: model vs SOI for death year 2022 at the actual 2022 exemption.
# Count AND average tax per return, by bin -- to see whether a count undershoot
# is paired with a tax-per-return overshoot (revenue concentrated in too few
# estates).
suppressPackageStartupMessages({library(data.table); library(tidyverse)})

ROOT     = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
TAX_DATA = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline'
MACRO    = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'
SOI      = file.path(ROOT, 'other/estate_tax/estate_tax_filed_2019_2023.csv')
PARAMS   = '/nfs/roberts/scratch/pi_nrs36/jar335/estate_tax_calibration/estate_calibration_parameters.csv'

Sys.setenv(ESTATE_CALIBRATE_NO_MAIN = '1')
source(file.path(ROOT, 'other/estate_tax/calibrate_estate_tax.R'))

pars = fread(PARAMS) %>% as_tibble() %>% arrange(total_objective)
best = pars[1, ]; rf = best$reporting_form; tf = best$taxable_form
kv = strsplit(best$parameters, ';')[[1]]
par = setNames(as.numeric(sub('.*=', '', kv)), sub('=.*', '', kv))

policy_index = load_policy_index(2017:2035, MACRO)
soi_targets  = load_soi_targets(SOI, policy_index)
taxable_fits = fit_taxable_fraction_models(soi_targets)
dsue_table   = build_dsue_table(soi_targets)
gift_gamma   = gift_addback_gamma(soi_targets)
cat(sprintf('gift add-back gamma = %.4f\n', gift_gamma))

cells = load_tax_data_cells(TAX_DATA, 2022, wealth_cells = 0)
m = apply_candidate(cells, rf, par, tf, taxable_fits, dsue_table,
                    'historical', policy_index, gift_gamma) %>% filter(taxable)

model = m %>% group_by(size_bin) %>%
  summarise(count = sum(expected_weight),
            tax   = sum(expected_weight * liability), .groups='drop') %>%
  mutate(avg_tax_M = tax / count / 1e6)

soi = fread(SOI) %>% as_tibble() %>%
  filter(tax_status=='taxable', year==2023, size_bin!='all') %>%
  transmute(size_bin,
            soi_count = as.numeric(gross_estate_for_tax_purposes_n),
            soi_tax   = as.numeric(net_estate_tax_amt),
            soi_avg_tax_M = soi_tax/soi_count/1e6)

cmp = full_join(model, soi, by='size_bin') %>%
  mutate(across(everything(), ~replace_na(., 0)))

cat('\n=== Death year 2022, $12.06M exemption: MODEL vs SOI ===\n\n')
cat(sprintf('%-10s %10s %10s | %12s %12s | %10s %10s\n',
            'bin','mdl_count','soi_count','mdl_tax$B','soi_tax$B','mdl_avgM','soi_avgM'))
for (b in c('under_10m','10m_20m','20m_50m','50m_plus')) {
  r = cmp %>% filter(size_bin==b)
  if (nrow(r)==0) next
  cat(sprintf('%-10s %10.0f %10.0f | %12.1f %12.1f | %10.1f %10.1f\n',
              b, r$count, r$soi_count, r$tax/1e9, r$soi_tax/1e9, r$avg_tax_M, r$soi_avg_tax_M))
}
cat(sprintf('%-10s %10.0f %10.0f | %12.1f %12.1f |\n','TOTAL',
            sum(cmp$count), sum(cmp$soi_count), sum(cmp$tax)/1e9, sum(cmp$soi_tax)/1e9))
