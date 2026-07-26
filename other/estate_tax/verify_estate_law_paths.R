#-------------------------------------------------------------------------------
# verify_estate_law_paths.R
#
# Parses the estate.yaml configs through the REAL tax-law machinery
# (load_tax_law_input + parse_param) and prints the generated exemption /
# schedule paths, so rules-form yamls can be checked against published
# Rev. Proc. values and policy intent without running a simulation:
#   - baseline: regime anchors 2014/2018/2026, generated intra-regime values
#     should land within one $10k rounding step of published BEA
#   - tests/estate_sunset: validation fixture, flat $7.2M from 2026
#   - public/estate_2009: $3.5M @ 2009 base indexed forward (expect ~$5.09M
#     in 2026) + the 14-bracket 2009 schedule appearing in 2026
#
# Usage (from repo root, via sbatch): Rscript other/estate_tax/verify_estate_law_paths.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(data.table)
  library(yaml)
})

source('src/misc/utils.R')
source('src/data/tax_law.R')
source('src/data/economy.R')

MACRO_ROOT = file.path('/nfs/roberts/project/pi_nrs36/shared/model_data',
                       'Macro-Projections/v3/2026022522/baseline')
YEARS = 2014:2035

# Published basic exclusion amounts (Rev. Proc. values), death years 2014-2025
BEA_PUBLISHED = c(
  '2014' = 5.34e6,  '2015' = 5.43e6,  '2016' = 5.45e6,  '2017' = 5.49e6,
  '2018' = 11.18e6, '2019' = 11.40e6, '2020' = 11.58e6, '2021' = 11.70e6,
  '2022' = 12.06e6, '2023' = 12.92e6, '2024' = 13.61e6, '2025' = 13.99e6
)

# Neutral offsets (no VAT, no excess growth)
neutral_vat    = tibble(year = 1900:2100, cpi_factor = 1, gdp_deflator_factor = 1)
indexes = generate_indexes(MACRO_ROOT, neutral_vat)

parse_estate = function(tax_law_id) {
  baseline = load_tax_law_input('./config/scenarios/tax_law/baseline')
  raw = baseline$estate
  if (tax_law_id != 'baseline') {
    changes = load_tax_law_input(file.path('./config/scenarios/tax_law', tax_law_id))
    for (subparam in names(changes$estate)) {
      raw[[subparam]] = changes$estate[[subparam]]
    }
  }
  parse_param(raw, 'estate', YEARS, indexes) %>%
    filter(filing_status == 1)
}

show_config = function(tax_law_id) {
  cat(sprintf('\n===== %s =====\n', tax_law_id))
  parsed = parse_estate(tax_law_id)

  exemption = parsed %>%
    filter(subparameter == 'exemption') %>%
    select(year, value) %>%
    mutate(published = BEA_PUBLISHED[as.character(year)],
           diff_10k_steps = (value - published) / 1e4)
  cat('Exemption path:\n')
  print(as.data.frame(exemption), row.names = FALSE)

  schedule = parsed %>%
    filter(subparameter %in% c('rates', 'brackets'), !is.na(value)) %>%
    group_by(year, subparameter) %>%
    summarise(n = n(), top = max(value), .groups = 'drop') %>%
    pivot_wider(names_from = subparameter, values_from = c(n, top))
  cat('Schedule (element count and top value by year):\n')
  print(as.data.frame(schedule), row.names = FALSE)
}

show_config('baseline')
show_config('tests/estate_sunset')
show_config('public/estate_2009')

cat('\nDone.\n')
