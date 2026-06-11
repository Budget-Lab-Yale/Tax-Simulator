#-------------------------------------------------------------------------------
# test_onmodel_parity.R
#
# Port-parity check: the on-model estate path (calc_estate +
# calc_estate_mortality + frozen config/estate params) must reproduce the
# canonical standalone module (estate_module.R with runtime soi_inputs) on the
# same records, exemption, and valuation parameters. Death year 2022 at the
# actual $12.06M exemption, Tax-Data 2026060918.
#
# Expected: agreement to rounding (the frozen yaml stores SOI-estimated inputs
# at 6 decimals). Tolerance 0.1% on expected tax and taxable-return totals.
#
# Usage (from repo root, via sbatch): Rscript other/estate_tax/test_onmodel_parity.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(data.table)
  library(yaml)
})

source('src/calc/utils.R')                # parse_calc_fn_input, integrate_rates_brackets
source('src/calc/functions/tax/estate.R')
source('src/sim/estate.R')
source('other/estate_tax/estate_module.R')

TAX_DATA_CSV = file.path('/nfs/roberts/project/pi_nrs36/shared/model_data',
                         'Tax-Data/v1/2026060918/baseline/tax_units_2022.csv')
EXEMPTION = 12.06e6

#---------------------------
# Leg 1: on-model pipeline
#---------------------------

params = get_estate_params()
cat(sprintf('Frozen params: r = %.3f, rho_pt = %.3f, gamma = %.6f, cap = %.0f\n',
            params$r, params$rho_pt, params$gamma,
            params$cluster_death_weight_cap))

td = fread(TAX_DATA_CSV, showProgress = FALSE) %>% as_tibble()

# Attach death-year-2022 estate law the way the tax law join would
law = estate_policy_current_law()
for (i in seq_along(law$brackets)) {
  td[[paste0('estate.brackets', i)]] = law$brackets[i]
  td[[paste0('estate.rates', i)]]    = law$rates[i]
}
td$`estate.exemption`      = EXEMPTION
td$`estate.portability`    = 1
td$`estate.income_tax_ded` = 1  # no-op: no estate_income_tax_ded column here

onmodel = td %>%
  bind_cols(calc_estate(., params)) %>%
  mutate(estate_m = calc_estate_mortality(., params$cluster_death_weight_cap))

# Recompute reported gross for bin assignment (diagnostic only)
assets = as.matrix(onmodel[, ESTATE_ASSET_COLS]); assets[is.na(assets)] = 0
gross  = rowSums(assets)
s_pt   = ifelse(gross > 0, assets[, 'value.pass_throughs'] / gross, 0)
onmodel$size_bin = assign_size_bin(
  gross * params$r * (1 + (params$rho_pt - 1) * s_pt),
  params$bins
)

onmodel_bins = onmodel %>%
  group_by(size_bin) %>%
  summarise(
    tax = sum(weight * estate_m *
                (estate_p_dsue * liab_estate_dsue +
                 (1 - estate_p_dsue) * liab_estate_nodsue)),
    returns = sum(weight * estate_m *
                    (estate_p_dsue * (liab_estate_dsue > 0) +
                     (1 - estate_p_dsue) * (liab_estate_nodsue > 0))),
    .groups = 'drop'
  )

#-------------------------------------
# Leg 2: canonical standalone module
#-------------------------------------

recs = load_estate_records(TAX_DATA_CSV) %>%
  apply_cluster_abscap_mortality(cap = params$cluster_death_weight_cap,
                                 verbose = TRUE)
soi    = load_soi_estate_table('other/estate_tax/estate_tax_filed_2016_2023.csv')
soi22  = soi_inputs(soi, 2022, exemption = EXEMPTION)
ref = compute_estate_liability(
  recs, exemption = EXEMPTION, soi_in = soi22,
  valuation = list(r = params$r, rho_pt = params$rho_pt),
  gift_addback = TRUE, count_mode = 'expected'
)

ref_bins = ref %>%
  group_by(size_bin) %>%
  summarise(tax     = sum(exp_tax),
            returns = sum(weight * m * count_flag),
            .groups = 'drop')

#-------------
# Comparison
#-------------

cmp = full_join(onmodel_bins, ref_bins, by = 'size_bin',
                suffix = c('_onmodel', '_module')) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         lo = map_dbl(size_bin, ~ ESTATE_BIN_BOUNDS[[.x]][1])) %>%
  arrange(lo) %>%
  select(-lo)

cat('\nBy bin:\n')
cat(sprintf('%10s | %12s %12s %8s | %10s %10s %8s\n',
            'bin', 'tax_onmdl$B', 'tax_mod$B', 'diff', 'cnt_onmdl',
            'cnt_mod', 'diff'))
for (i in seq_len(nrow(cmp))) {
  b = cmp[i, ]
  cat(sprintf('%10s | %12.4f %12.4f %+7.3f%% | %10.1f %10.1f %+7.3f%%\n',
              b$size_bin, b$tax_onmodel / 1e9, b$tax_module / 1e9,
              if (b$tax_module > 0) 100 * (b$tax_onmodel / b$tax_module - 1) else 0,
              b$returns_onmodel, b$returns_module,
              if (b$returns_module > 0) 100 * (b$returns_onmodel / b$returns_module - 1) else 0))
}

tax_err = sum(cmp$tax_onmodel) / sum(cmp$tax_module) - 1
cnt_err = sum(cmp$returns_onmodel) / sum(cmp$returns_module) - 1
cat(sprintf('\nTOTALS: tax $%.4fB vs $%.4fB (%+.4f%%) | returns %.1f vs %.1f (%+.4f%%)\n',
            sum(cmp$tax_onmodel) / 1e9, sum(cmp$tax_module) / 1e9, 100 * tax_err,
            sum(cmp$returns_onmodel), sum(cmp$returns_module), 100 * cnt_err))

if (abs(tax_err) < 0.001 && abs(cnt_err) < 0.001) {
  cat('\nPARITY: PASS\n')
} else {
  cat('\nPARITY: FAIL (tolerance 0.1%)\n')
  quit(status = 1)
}
