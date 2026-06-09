#------------------------------------------------------------------------------
# clausing_excise_distribution.R
#
# Off-model distribution of the Clausing-Sarin excise package. The excise
# measures are scored off-model, so this script does NOT touch the tax
# calculator. It allocates each measure's (single-year) revenue across income
# groups and reports the average tax change and the percent change in after-tax
# income per group -- in the same group definitions the model's distribution.csv
# uses, so the results can be appended directly onto that table.
#
# Division of labor:
#   - ON-MODEL records (Tax-Sim baseline detail + per-tax-unit c_* consumption
#     joined from the Tax-Data interface) supply the income buckets, weights,
#     expanded income, after-tax income, and the *parent* consumption level for
#     each measure. These have a reliable top tail (PUF-based).
#   - CEX supplies only a crude WITHIN-category carve ratio by income quintile
#     (taxed good / parent category), applied to the on-model parent c_*. The
#     top-tail dispersion still comes from the on-model c_* levels, NOT from CEX
#     (CEX covers the top poorly). Because every Top-X bucket is a subset of
#     Quintile 5, top-bucket records automatically inherit the Q5 carve ratio.
#
# Mappings (see config below):
#   carbon   -> total consumption (sum of all c_*); no carve. Gasoline is not
#               special-cased: the carbon tax is on production and flows through
#               to prices broadly, so total consumption is the incidence base.
#   alcohol  -> c_food_off_premises,      carve = ALCBEVCQ / (FDHOME+ALCBEV)
#   tobacco  -> c_other_nondurables,      carve = TOBACCCQ / (TOBACC+PERSCA+READ+PREDRG)
#   gambling -> c_other_services_health,  carve = FEEADMCQ / (service-health CQs)   [entertainment]
#   guns     -> c_other_services_health,  carve = OTHENTCQ / (service-health CQs)   [recreation]
#
# Bucketing and after-tax income exactly mirror
# src/data/post_processing/distribution.R (Income dimension, iit_pr inclusion).
#------------------------------------------------------------------------------

library(tidyverse)
library(data.table)


#==============================================================================
# CONFIG -- edit these
#==============================================================================

YEAR = 2030

# Single-year revenue by measure, in $ billions (positive = revenue raised)
REVENUE_B = c(
  carbon   = 101.6,
  gambling = 11.4,
  guns     = 1.0,
  alcohol  = 19.4,
  tobacco  = 0.3
)

# Tax-Simulator output vintage + the baseline scenario used for grouping/ATI.
# (The model builds its distribution table off baseline microdata, so we do too.)
TS_ROOT       = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
TS_VINTAGE    = 'clausing_2026_policy'
BASELINE_SCEN = 'baseline'

# Tax-Data interface vintage carrying the per-tax-unit c_* consumption columns
TAXDATA_FILE = file.path(
  '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1',
  '2026052823/baseline',
  paste0('tax_units_', YEAR, '.csv')
)

# Raw CEX FMLI summary files used for the within-category carve ratios. Pooled
# as a cross-section; ratios are scale-invariant so quarter weighting / annual
# annualization is immaterial here.
CEX_FMLI_GLOB = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CEX/2023/fmli*.csv'

OUT_FILE = file.path(
  '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator',
  'other/analysis_scripts/public',
  paste0('clausing_excise_distribution_', YEAR, '.csv')
)

# Each measure's on-model parent consumption column. 'TOTAL' = sum of all c_*.
PARENT = c(
  carbon   = 'carbon_flat',   # overridden below to 'carbon_weighted' if CARBON_WEIGHTED
  alcohol  = 'c_food_off_premises',
  tobacco  = 'c_other_nondurables',
  gambling = 'c_other_services_health',
  guns     = 'c_other_services_health'
)

# CEX carve = (numerator CQ vars) / (denominator CQ vars), summed within each
# income quintile. carbon has no carve (ratio = 1).
CARVE_NUM = list(
  alcohol  = 'ALCBEVCQ',
  tobacco  = 'TOBACCCQ',
  gambling = 'FEEADMCQ',
  guns     = 'OTHENTCQ'
)
CARVE_DEN = list(
  alcohol  = c('FDHOMECQ', 'ALCBEVCQ'),
  tobacco  = c('TOBACCCQ', 'PERSCACQ', 'READCQ', 'PREDRGCQ'),
  # service-health basket = exact CQ inputs to c_other_services_health
  gambling = c('TELEPHCQ','HOUSOPCQ','MISCCQ','OTHENTCQ','MAINRPCQ','VRNTLOCQ',
               'PUBTRACQ','FEEADMCQ','FDAWAYCQ','OTHLODCQ','HLTHINCQ','MEDSRVCQ',
               'EDUCACQ','LIFINSCQ','VEHINSCQ','VEHFINCQ'),
  guns     = c('TELEPHCQ','HOUSOPCQ','MISCCQ','OTHENTCQ','MAINRPCQ','VRNTLOCQ',
               'PUBTRACQ','FEEADMCQ','FDAWAYCQ','OTHLODCQ','HLTHINCQ','MEDSRVCQ',
               'EDUCACQ','LIFINSCQ','VEHINSCQ','VEHFINCQ')
)

C_COLS = c('c_clothing','c_motor_vehicles','c_durables','c_other_nondurables',
           'c_food_off_premises','c_gasoline','c_housing_utilities',
           'c_other_services_health')

# CO2e intensity per dollar by category, used for the carbon base. Real values
# from EPA USEEIO v1.3 supply-chain (production-embodied) GHG factors, blended
# to the 8 c_* categories by build_carbon_intensities.R. Only relative values
# matter (distribution uses shares). Set CARBON_WEIGHTED = FALSE to fall back to
# flat total consumption.
CARBON_WEIGHTED = TRUE
# CARBON_CARVEOUT = TRUE uses the variant where retail gasoline and household
# utilities are rebated (carved out of the carbon base).
CARBON_CARVEOUT = TRUE
INTENSITY = read_csv(file.path(dirname(OUT_FILE), 'resources',
                               if (CARBON_CARVEOUT) 'carbon_intensities_carveout.csv'
                               else 'carbon_intensities.csv'),
                     show_col_types = FALSE) %>%
  { setNames(.$intensity, .$category) } %>%
  .[C_COLS]
if (CARBON_WEIGHTED) PARENT['carbon'] = 'carbon_weighted'


#==============================================================================
# 1. CEX within-category carve ratios, by income quintile
#==============================================================================

cex = list.files(dirname(CEX_FMLI_GLOB),
                 pattern    = glob2rx(basename(CEX_FMLI_GLOB)),
                 full.names = TRUE) %>%
  map(~ fread(.x) %>% tibble()) %>%
  bind_rows()

# Weighted income quintiles (rank on before-tax income FINCBTXM; weight FINLWT21)
cex_q = cex %>%
  filter(FINCBTXM >= 0, FINLWT21 > 0) %>%
  arrange(FINCBTXM) %>%
  mutate(
    pctile   = cumsum(FINLWT21) / sum(FINLWT21),
    quintile = ceiling(pmin(pctile, 1) * 5),
    quintile = paste0('Quintile ', pmax(1, quintile))
  )

# carve ratio per measure per quintile
carve_by_q = imap_dfr(CARVE_NUM, function(num, measure) {
  den = CARVE_DEN[[measure]]
  cex_q %>%
    group_by(quintile) %>%
    summarise(
      num = sum(FINLWT21 * rowSums(across(all_of(num)),  na.rm = TRUE)),
      den = sum(FINLWT21 * rowSums(across(all_of(den)),  na.rm = TRUE)),
      .groups = 'drop'
    ) %>%
    transmute(measure = measure, quintile, carve = num / den)
})

cat('\nCEX carve ratios by quintile:\n')
carve_by_q %>% pivot_wider(names_from = measure, values_from = carve) %>% print()


#==============================================================================
# 2. On-model records: bucketing + after-tax income + parent consumption
#==============================================================================

detail = file.path(TS_ROOT, TS_VINTAGE, BASELINE_SCEN, 'static/detail',
                   paste0(YEAR, '.csv')) %>%
  fread() %>% tibble() %>%
  filter(dep_status == 0) %>%
  transmute(
    id,
    weight,
    income = expanded_inc,
    ati    = expanded_inc - (liab_iit_net + liab_pr)   # iit_pr ATI, per distribution.R
  )

# Join per-tax-unit consumption from the Tax-Data interface
consumption = fread(TAXDATA_FILE, select = c('id', C_COLS)) %>% tibble()

micro = detail %>%
  left_join(consumption, by = 'id') %>%
  mutate(across(all_of(C_COLS), ~ replace_na(., 0)),
         TOTAL          = rowSums(across(all_of(C_COLS))),
         carbon_flat    = TOTAL,
         carbon_weighted= as.numeric(as.matrix(across(all_of(C_COLS))) %*% INTENSITY))

# Income percentile / quintile / top cuts -- identical construction to
# distribution.R (weighted cumulative share among income >= 0)
micro = micro %>%
  arrange(income) %>%
  mutate(
    income_pctile = cumsum(weight * (income >= 0)) / sum(weight * (income >= 0)),
    income_pctile = if_else(income < 0, NA_real_, income_pctile),
    quintile = case_when(
      income < 0           ~ 'Negative income',
      income_pctile <= 0.2 ~ 'Quintile 1',
      income_pctile <= 0.4 ~ 'Quintile 2',
      income_pctile <= 0.6 ~ 'Quintile 3',
      income_pctile <= 0.8 ~ 'Quintile 4',
      TRUE                 ~ 'Quintile 5'
    )
  )

# Attach the CEX carve ratio for each measure by the record's quintile.
# Top-X records are a subset of Quintile 5, so they inherit the Q5 carve ratio.
carve_wide = carve_by_q %>%
  bind_rows(  # negative-income records: use Quintile 1 carve as a stand-in
    carve_by_q %>% filter(quintile == 'Quintile 1') %>%
      mutate(quintile = 'Negative income')
  ) %>%
  pivot_wider(names_from = measure, values_from = carve,
              names_prefix = 'carve_')

micro = micro %>% left_join(carve_wide, by = 'quintile')


#==============================================================================
# 3. Allocate revenue and compute per-group metrics
#==============================================================================

measures = names(REVENUE_B)

# taxed base per record per measure = parent c_* level * carve ratio
for (m in measures) {
  parent = PARENT[[m]]
  carve  = if (m == 'carbon') 1 else micro[[paste0('carve_', m)]]
  micro[[paste0('base_', m)]] = micro[[parent]] * carve
}

# helper: metrics for one subset of records, one measure
group_metrics = function(df, m) {
  rev   = REVENUE_B[[m]] * 1e9
  base  = df[[paste0('base_', m)]]
  share = sum(df$weight * base) / sum(micro$weight * micro[[paste0('base_', m)]])
  dollars = rev * share
  tibble(
    measure      = m,
    dollars_B    = dollars / 1e9,
    avg          = dollars / sum(df$weight),               # avg tax change ($/unit)
    pct_chg_ati  = -dollars / sum(df$weight * df$ati)      # excise lowers ATI
  )
}

# group definitions matching distribution.R's Income dimension
group_defs = list(
  'Quintile 1'      = quote(quintile == 'Quintile 1'),
  'Quintile 2'      = quote(quintile == 'Quintile 2'),
  'Quintile 3'      = quote(quintile == 'Quintile 3'),
  'Quintile 4'      = quote(quintile == 'Quintile 4'),
  'Quintile 5'      = quote(quintile == 'Quintile 5'),
  'Top 10%'         = quote(income_pctile > 0.90),
  'Top 5%'          = quote(income_pctile > 0.95),
  'Top 1%'          = quote(income_pctile > 0.99),
  'Top 0.1%'        = quote(income_pctile > 0.999),
  'Negative income' = quote(quintile == 'Negative income'),
  'Overall'         = quote(rep(TRUE, n()))
)

results = imap_dfr(group_defs, function(cond, grp) {
  sub = micro %>% filter(!!cond)
  if (nrow(sub) == 0) return(NULL)
  map_dfr(measures, ~ group_metrics(sub, .x)) %>%
    mutate(group = grp,
           income_cutoff     = round(min(sub$income) / 5) * 5,
           share_income      = sum(sub$weight * sub$income) / sum(micro$weight * micro$income),
           share_consumption = sum(sub$weight * sub$TOTAL)  / sum(micro$weight * micro$TOTAL),
           .before = 1)
})

# total across all five measures (sum of avg dollars; ATI effects additive)
totals = results %>%
  group_by(group, income_cutoff, share_income, share_consumption) %>%
  summarise(measure = 'all_excises',
            dollars_B   = sum(dollars_B),
            avg         = sum(avg),
            pct_chg_ati = sum(pct_chg_ati),
            .groups = 'drop')

out = bind_rows(results, totals) %>%
  mutate(year = YEAR, group_dimension = 'Income', .before = 1) %>%
  arrange(factor(group, levels = names(group_defs)),
          factor(measure, levels = c(measures, 'all_excises')))

write_csv(out, OUT_FILE)
cat('\nWrote', OUT_FILE, '\n\n')
out %>% filter(measure == 'all_excises') %>%
  select(group, income_cutoff, share_income, share_consumption, avg, pct_chg_ati) %>%
  print(n = Inf)

#==============================================================================
# Carbon: flat (total consumption) vs intensity-weighted, side by side
#==============================================================================

carbon_cmp = function(base_col) {
  rev = REVENUE_B[['carbon']] * 1e9
  tot = sum(micro$weight * micro[[base_col]])
  imap_dfr(group_defs, function(cond, grp) {
    sub = micro %>% filter(!!cond)
    if (nrow(sub) == 0) return(NULL)
    dollars = rev * sum(sub$weight * sub[[base_col]]) / tot
    tibble(group = grp,
           avg = dollars / sum(sub$weight),
           pct = -dollars / sum(sub$weight * sub$ati))
  })
}

cat('\n\nCARBON ONLY: flat total-consumption base vs intensity-weighted base\n\n')
carbon_cmp('carbon_flat') %>%
  rename(avg_flat = avg, pct_flat = pct) %>%
  left_join(carbon_cmp('carbon_weighted') %>% rename(avg_wt = avg, pct_wt = pct),
            by = 'group') %>%
  mutate(avg_flat = round(avg_flat), avg_wt = round(avg_wt),
         pct_flat = round(pct_flat * 100, 2), pct_wt = round(pct_wt * 100, 2)) %>%
  print(n = Inf)
