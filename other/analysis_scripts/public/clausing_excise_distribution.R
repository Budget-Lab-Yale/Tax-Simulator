#------------------------------------------------------------------------------
# clausing_excise_distribution.R
#
# Off-model distribution of the Clausing-Sarin excise package, plus a handful
# of off-model INCOME TAX measures (carried interest repeal, QSBS reform, OZ
# repeal). All are scored off-model, so this script does NOT touch the tax
# calculator. For EACH year of the budget window it allocates the measure's
# revenue across income groups (groups defined within-year, mirroring the
# model's distribution.csv), then reports the 10-YEAR AVERAGE annual effect:
# pct_chg_ati averaged directly, dollar metrics deflated to 2026 dollars
# (chained CPI, Macro-Projections) before averaging.
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
#     Carve ratios are a 2023 cross-section, held constant across years.
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
# Income tax measures use baseline detail-file income columns as the base
# (no consumption, no CEX carve):
#   carried_interest -> LT capital gains among records with pass-through
#                       (partnership/S-corp) income: carry is reported as LTCG
#                       flowing through a K-1, so kg_lt alone would spread it
#                       over all stockholders and part_scorp alone would catch
#                       operating businesses with no carry
#   qsbs             -> LT capital gains (Sec 1202 exclusion accrues to
#                       founders/early investors; top-concentrated gains)
#   oz               -> LT capital gains (OZ deferral/exclusion is elected
#                       against realized gains)
#
# Bucketing and after-tax income exactly mirror
# src/data/post_processing/distribution.R (Income dimension, iit_pr inclusion).
#------------------------------------------------------------------------------

library(tidyverse)
library(data.table)


#==============================================================================
# CONFIG -- edit these
#==============================================================================

YEARS = 2030:2039

# Revenue by measure and year, in $ billions (positive = revenue raised).
# Each vector spans YEARS. Source: Clausing-Sarin off-model scores (user-
# supplied, 2026-06-10).
REVENUE_B = list(
  carbon           = c(101.6, 106.1, 111.1, 116.5, 121.8, 127.3, 132.8, 138.1, 143.1, 148.0),
  gambling         = c(11.4, 11.88, 12.38, 12.9, 13.45, 14.01, 14.58, 15.16, 15.77, 16.41),
  guns             = c(0.96, 1.01, 1.05, 1.09, 1.14, 1.19, 1.24, 1.29, 1.34, 1.40),
  alcohol          = c(19.4, 20.4, 21.5, 22.5, 23.6, 24.8, 26.0, 27.2, 28.5, 29.9),
  tobacco          = c(0.34, 0.36, 0.37, 0.39, 0.40, 0.41, 0.43, 0.44, 0.46, 0.47),
  carried_interest = c(7.0, 8.1, 9.2, 10.5, 11.8, 13.3, 14.7, 16.3, 17.9, 19.6),
  qsbs             = c(9.5, 10.0, 10.5, 10.9, 11.4, 11.7, 12.2, 12.7, 13.3, 13.8),
  oz               = c(7.0, 7.1, 5.1, 1.7, 3.7, 5.7, 5.9, 6.1, 6.3, 6.5)
)
stopifnot(all(lengths(REVENUE_B) == length(YEARS)))

# Measures distributed by detail-file income columns rather than consumption.
# Each entry is an expression evaluated per record on the joined micro data.
INCOME_BASE = list(
  carried_interest = quote(pmax(kg_lt, 0) * (part_scorp != 0)),
  qsbs             = quote(pmax(kg_lt, 0)),
  oz               = quote(pmax(kg_lt, 0))
)

# Tax-Simulator output vintage + the baseline scenario used for grouping/ATI.
# (The model builds its distribution table off baseline microdata, so we do too.)
TS_ROOT       = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
TS_VINTAGE    = 'clausing_estate'
BASELINE_SCEN = 'baseline'

# Tax-Data interface vintage carrying the per-tax-unit c_* consumption columns.
# Must match the vintage the TS_VINTAGE run simulated on (the join is by id,
# and ids change across Tax-Data vintages)
TAXDATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline'

# Chained CPI (base 2026 = 1) for expressing dollar metrics in 2026 dollars.
# Default Macro-Projections vintage, same as the model run used.
CCPIU = fread(file.path('/nfs/roberts/project/pi_nrs36/shared/model_data',
                        'Macro-Projections/v3/2026022522/baseline/projections.csv'),
              select = c('year', 'ccpiu')) %>%
  filter(year %in% YEARS) %>%
  { setNames(.$ccpiu, .$year) }

# Raw CEX FMLI summary files used for the within-category carve ratios. Pooled
# as a cross-section; ratios are scale-invariant so quarter weighting / annual
# annualization is immaterial here.
CEX_FMLI_GLOB = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CEX/2023/fmli*.csv'

OUT_DIR = file.path(
  '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator',
  'other/analysis_scripts/public'
)
OUT_FILE_YEARLY = file.path(OUT_DIR, 'clausing_excise_distribution_by_year.csv')
OUT_FILE_AVG    = file.path(OUT_DIR, 'clausing_excise_distribution_avg_2030_2039.csv')

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
INTENSITY = read_csv(file.path(OUT_DIR, 'resources',
                               if (CARBON_CARVEOUT) 'carbon_intensities_carveout.csv'
                               else 'carbon_intensities.csv'),
                     show_col_types = FALSE) %>%
  { setNames(.$intensity, .$category) } %>%
  .[C_COLS]
if (CARBON_WEIGHTED) PARENT['carbon'] = 'carbon_weighted'


#==============================================================================
# 1. CEX within-category carve ratios, by income quintile (year-invariant)
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

carve_wide = carve_by_q %>%
  bind_rows(  # negative-income records: use Quintile 1 carve as a stand-in
    carve_by_q %>% filter(quintile == 'Quintile 1') %>%
      mutate(quintile = 'Negative income')
  ) %>%
  pivot_wider(names_from = measure, values_from = carve,
              names_prefix = 'carve_')


#==============================================================================
# 2. Per-year micro build + allocation
#==============================================================================

measures = names(REVENUE_B)

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

run_year = function(yr) {

  detail = file.path(TS_ROOT, TS_VINTAGE, BASELINE_SCEN, 'static/detail',
                     paste0(yr, '.csv')) %>%
    fread() %>% tibble() %>%
    filter(dep_status == 0) %>%
    transmute(
      id,
      weight,
      income = expanded_inc,
      ati    = expanded_inc - (liab_iit_net + liab_pr),  # iit_pr ATI, per distribution.R
      kg_lt,
      part_scorp
    )

  # Join per-tax-unit consumption from the Tax-Data interface
  consumption = fread(file.path(TAXDATA_ROOT, paste0('tax_units_', yr, '.csv')),
                      select = c('id', C_COLS)) %>% tibble()

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
    ) %>%
    left_join(carve_wide, by = 'quintile')

  # taxed base per record per measure:
  #   excises:        parent c_* level * CEX carve ratio
  #   income measures: expression over detail-file income columns
  for (m in measures) {
    if (m %in% names(INCOME_BASE)) {
      micro[[paste0('base_', m)]] = eval(INCOME_BASE[[m]], micro)
    } else {
      parent = PARENT[[m]]
      carve  = if (m == 'carbon') 1 else micro[[paste0('carve_', m)]]
      micro[[paste0('base_', m)]] = micro[[parent]] * carve
    }
  }

  # helper: metrics for one subset of records, one measure
  group_metrics = function(df, m) {
    rev   = REVENUE_B[[m]][match(yr, YEARS)] * 1e9
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

  imap_dfr(group_defs, function(cond, grp) {
    sub = micro %>% filter(!!cond)
    if (nrow(sub) == 0) return(NULL)
    map_dfr(measures, ~ group_metrics(sub, .x)) %>%
      mutate(group = grp,
             income_cutoff     = round(min(sub$income) / 5) * 5,
             share_income      = sum(sub$weight * sub$income) / sum(micro$weight * micro$income),
             share_consumption = sum(sub$weight * sub$TOTAL)  / sum(micro$weight * micro$TOTAL),
             .before = 1) %>%
      mutate(year = yr, .before = 1)
  })
}

results_yearly = map_dfr(YEARS, function(yr) {
  cat('Year', yr, '...\n')
  run_year(yr)
})


#==============================================================================
# 3. Subtotals, 10-year average (2026 dollars), output
#==============================================================================

income_measures = names(INCOME_BASE)
total_defs = list(
  all_excises    = setdiff(measures, income_measures),
  all_income_tax = intersect(measures, income_measures),
  all_measures   = measures
)

# subtotals by type and grand total within each year (additive metrics)
totals_yearly = imap_dfr(total_defs, function(members, label) {
  results_yearly %>%
    filter(measure %in% members) %>%
    group_by(year, group, income_cutoff, share_income, share_consumption) %>%
    summarise(measure = label,
              dollars_B   = sum(dollars_B),
              avg         = sum(avg),
              pct_chg_ati = sum(pct_chg_ati),
              .groups = 'drop')
})

yearly = bind_rows(results_yearly, totals_yearly) %>%
  mutate(group_dimension = 'Income', .before = 1) %>%
  arrange(year,
          factor(group, levels = names(group_defs)),
          factor(measure, levels = c(measures, names(total_defs))))

write_csv(yearly, OUT_FILE_YEARLY)
cat('\nWrote', OUT_FILE_YEARLY, '\n')

# 10-year average annual effect: pct_chg_ati averaged directly (ratio);
# dollar metrics deflated to 2026 dollars (chained CPI) before averaging
avg = yearly %>%
  mutate(deflator = CCPIU[as.character(year)]) %>%
  group_by(group_dimension, group, measure) %>%
  summarise(
    dollars_B   = mean(dollars_B / deflator),
    avg         = mean(avg / deflator),
    pct_chg_ati = mean(pct_chg_ati),
    .groups = 'drop'
  ) %>%
  mutate(period = 'avg_2030_2039', dollars = '2026', .before = 1) %>%
  arrange(factor(group, levels = names(group_defs)),
          factor(measure, levels = c(measures, names(total_defs))))

write_csv(avg, OUT_FILE_AVG)
cat('Wrote', OUT_FILE_AVG, '\n\n')

for (label in names(total_defs)) {
  cat('\n', label, ' (10-yr avg, 2026$):\n', sep = '')
  avg %>% filter(measure == label) %>%
    select(group, avg, pct_chg_ati) %>%
    print(n = Inf)
}
