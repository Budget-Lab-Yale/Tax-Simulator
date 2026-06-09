#------------------------------------------------------------------------------
# build_carbon_intensities.R
#
# Builds CO2e intensity (kg per $, purchaser price) for each of the 8 on-model
# c_* consumption categories, for use as the carbon-tax incidence base in
# clausing_excise_distribution.R.
#
# Method:
#   1. Each CEX FMLI consumption subcomponent (CQ var) is mapped to a
#      representative 2017 NAICS-6 commodity.
#   2. Its intensity = EPA USEEIO v1.3 "Supply Chain Emission Factors WITH
#      margins" for that NAICS (kg CO2e / 2022 USD, purchaser price). These are
#      PRODUCTION/supply-chain embodied factors -- they capture emissions from
#      producing the good, NOT the consumer's own combustion. That matches an
#      upstream carbon tax on production processes that passes through to prices.
#   3. The 8 c_* intensities are spend-weighted blends of their subcomponents,
#      with national expenditure weights taken from pooled CEX FMLI.
#
# Electricity (NAICS 2211) is ABSENT from EPA's NAICS-6 file, so ELCTRCCQ uses
# a documented eGRID-derived factor (see ELEC_INTENSITY below).
#
# Source: EPA Supply Chain GHG Emission Factors v1.3 by NAICS-6 (2022 data),
#   resources/useeio_ghg_naics_v1.3.csv
#------------------------------------------------------------------------------

library(tidyverse)
library(data.table)

DIR  = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/analysis_scripts/public'
EPA  = file.path(DIR, 'resources/useeio_ghg_naics_v1.3.csv')
CEX  = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CEX/2023/fmli*.csv'
OUT  = file.path(DIR, 'resources/carbon_intensities.csv')

# Electricity factor: eGRID 2022 US avg 0.371 kg CO2e/kWh / ~$0.155/kWh
# residential price 2022 ~= 2.4 kg/$. Direct generation emission = the
# production emission an upstream carbon tax targets. Not in EPA NAICS file.
ELEC_INTENSITY = 2.40

# Policy carve-out: retail gasoline and household utilities are rebated to
# consumers, so they carry no net carbon burden. Zero these CQ subcomponents'
# intensity in the carve-out version. (Telephone is not an energy utility and
# is left in.) The script writes BOTH a no-carve and a carve-out intensity file.
CARVE_OUT_CQ = c('GASMOCQ',                          # retail gasoline
                 'ELCTRCCQ','NTLGASCQ','ALLFULCQ','WATRPSCQ')  # household utilities

# CQ subcomponent -> representative 2017 NAICS-6 commodity, grouped by parent c_*
XWALK = tribble(
  ~category,                  ~cq,         ~naics,
  'c_clothing',               'APPARCQ',   '315990',   # apparel
  'c_motor_vehicles',         'CARTKNCQ',  '336111',   # new autos
  'c_motor_vehicles',         'CARTKUCQ',  '336111',   # used autos
  'c_motor_vehicles',         'OTHVEHCQ',  '336991',   # motorcycles/bikes
  'c_durables',               'HOUSEQCQ',  '337121',   # furniture/appliances
  'c_durables',               'PETTOYCQ',  '339930',   # toys/hobbies
  'c_durables',               'TVRDIOCQ',  '334310',   # audio/video
  'c_durables',               'MEDSUPCQ',  '339112',   # medical equipment
  'c_other_nondurables',      'TOBACCCQ',  '312230',   # tobacco
  'c_other_nondurables',      'PERSCACQ',  '325620',   # personal care
  'c_other_nondurables',      'READCQ',    '511120',   # reading
  'c_other_nondurables',      'PREDRGCQ',  '325412',   # prescription drugs
  'c_food_off_premises',      'FDHOMECQ',  '311999',   # groceries (food mfg)
  'c_food_off_premises',      'ALCBEVCQ',  '312120',   # alcohol
  'c_gasoline',               'GASMOCQ',   '324110',   # gasoline (refining)
  'c_housing_utilities',      'OWNDWECQ',  '531110',   # owned dwelling
  'c_housing_utilities',      'RENDWECQ',  '531110',   # rented dwelling
  'c_housing_utilities',      'NTLGASCQ',  '221210',   # natural gas
  'c_housing_utilities',      'ELCTRCCQ',  'ELEC',      # electricity (override)
  'c_housing_utilities',      'ALLFULCQ',  '324110',   # heating oil/other fuels
  'c_housing_utilities',      'WATRPSCQ',  '221310',   # water/sewage
  'c_other_services_health',  'TELEPHCQ',  '517311',   # telephone
  'c_other_services_health',  'HOUSOPCQ',  '812990',   # household operations
  'c_other_services_health',  'MISCCQ',    '812990',   # misc
  'c_other_services_health',  'OTHENTCQ',  '339920',   # sporting goods
  'c_other_services_health',  'MAINRPCQ',  '811118',   # vehicle maintenance
  'c_other_services_health',  'VRNTLOCQ',  '532111',   # vehicle rental
  'c_other_services_health',  'PUBTRACQ',  '485111',   # public transit
  'c_other_services_health',  'FEEADMCQ',  '713110',   # fees & admissions
  'c_other_services_health',  'FDAWAYCQ',  '722511',   # restaurants
  'c_other_services_health',  'OTHLODCQ',  '721110',   # lodging
  'c_other_services_health',  'HLTHINCQ',  '524114',   # health insurance
  'c_other_services_health',  'MEDSRVCQ',  '621111',   # medical services
  'c_other_services_health',  'EDUCACQ',   '611310',   # education
  'c_other_services_health',  'LIFINSCQ',  '524113',   # life insurance
  'c_other_services_health',  'VEHINSCQ',  '524126',   # vehicle insurance
  'c_other_services_health',  'VEHFINCQ',  '522220'    # vehicle finance
)

#-----------------------------------------------------------------------------
# EPA factors (with margins) by NAICS
#-----------------------------------------------------------------------------

epa = fread(EPA) %>% tibble() %>%
  transmute(naics  = as.character(`2017 NAICS Code`),
            factor = `Supply Chain Emission Factors with Margins`)

xwalk = XWALK %>%
  left_join(epa, by = 'naics') %>%
  mutate(factor = if_else(naics == 'ELEC', ELEC_INTENSITY, factor))

stopifnot(!any(is.na(xwalk$factor)))   # every CQ must resolve to a factor

#-----------------------------------------------------------------------------
# National expenditure weights per CQ, from pooled CEX FMLI
#-----------------------------------------------------------------------------

cex = list.files(dirname(CEX), pattern = glob2rx(basename(CEX)), full.names = TRUE) %>%
  map(~ fread(.x) %>% tibble()) %>% bind_rows() %>%
  filter(FINLWT21 > 0)

spend = xwalk$cq %>%
  set_names() %>%
  map_dbl(~ sum(cex$FINLWT21 * replace_na(cex[[.x]], 0))) %>%
  enframe(name = 'cq', value = 'spend')

#-----------------------------------------------------------------------------
# Spend-weighted blended intensity per c_* category
#-----------------------------------------------------------------------------

C_ORDER = c('c_clothing','c_motor_vehicles','c_durables','c_other_nondurables',
            'c_food_off_premises','c_gasoline','c_housing_utilities',
            'c_other_services_health')

blend = function(factor_col) {
  xwalk %>%
    left_join(spend, by = 'cq') %>%
    mutate(f = .data[[factor_col]]) %>%
    group_by(category) %>%
    summarise(intensity = sum(f * spend) / sum(spend), .groups = 'drop') %>%
    arrange(factor(category, levels = C_ORDER))
}

# no-carve: all subcomponents; carve-out: rebated CQs get 0 intensity
xwalk = xwalk %>%
  mutate(factor_carve = if_else(cq %in% CARVE_OUT_CQ, 0, factor))

intensities         = blend('factor')
intensities_carve   = blend('factor_carve')

write_csv(intensities,       OUT)
write_csv(intensities_carve, sub('\\.csv$', '_carveout.csv', OUT))
cat('\nWrote', OUT, 'and _carveout variant\n\n')
cat('No-carve vs carve-out (retail gas + household utilities rebated):\n')
intensities %>% rename(intensity_nocarve = intensity) %>%
  left_join(intensities_carve %>% rename(intensity_carveout = intensity), by = 'category') %>%
  print()

cat('\nSubcomponent detail (factor | national spend share within category):\n\n')
xwalk %>%
  left_join(spend, by = 'cq') %>%
  group_by(category) %>%
  mutate(wt = spend / sum(spend)) %>%
  ungroup() %>%
  transmute(category, cq, naics, factor, share_in_cat = round(wt, 3)) %>%
  arrange(factor(category, levels = C_ORDER), desc(share_in_cat)) %>%
  print(n = Inf)
