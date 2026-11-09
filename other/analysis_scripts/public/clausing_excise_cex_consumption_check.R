#------------------------------------------------------------------------------
# clausing_excise_cex_consumption_check.R
#
# Validation: compare the ON-MODEL consumption shares by income group (from
# clausing_excise_distribution_2030.csv, built off Tax-Data c_*) against
# consumption shares computed DIRECTLY from raw CEX, on the identical basket
# (the exact CQ inputs that define the 8 c_* categories).
#
# CEX covers the top poorly, so Top 1% is shown but noisy and Top 0.1% is
# omitted. Quintile / Top 10 / Top 5 are the meaningful comparison.
#------------------------------------------------------------------------------

library(tidyverse)
library(data.table)

CEX_FMLI_GLOB = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CEX/2023/fmli*.csv'
ONMODEL_CSV   = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/analysis_scripts/public/clausing_excise_distribution_2030.csv'

# The exact CQ basket that builds the 8 c_* categories (from cex_pce_mapping.md)
CQ_BASKET = c(
  'APPARCQ',                                              # clothing
  'CARTKNCQ','CARTKUCQ','OTHVEHCQ',                       # motor vehicles
  'HOUSEQCQ','PETTOYCQ','TVRDIOCQ','MEDSUPCQ',            # durables
  'TOBACCCQ','PERSCACQ','READCQ','PREDRGCQ',              # other nondurables
  'FDHOMECQ','ALCBEVCQ',                                  # food off-premises
  'GASMOCQ',                                              # gasoline
  'OWNDWECQ','RENDWECQ','NTLGASCQ','ELCTRCCQ','ALLFULCQ','WATRPSCQ',  # housing/util
  'TELEPHCQ','HOUSOPCQ','MISCCQ','OTHENTCQ','MAINRPCQ','VRNTLOCQ',    # svc + health
  'PUBTRACQ','FEEADMCQ','FDAWAYCQ','OTHLODCQ','HLTHINCQ','MEDSRVCQ',
  'EDUCACQ','LIFINSCQ','VEHINSCQ','VEHFINCQ'
)

#-----------------------------------------------------------------------------
# CEX: total consumption shares by income group
#-----------------------------------------------------------------------------

cex = list.files(dirname(CEX_FMLI_GLOB),
                 pattern = glob2rx(basename(CEX_FMLI_GLOB)), full.names = TRUE) %>%
  map(~ fread(.x) %>% tibble()) %>% bind_rows() %>%
  filter(FINCBTXM >= 0, FINLWT21 > 0) %>%
  mutate(cons = rowSums(across(all_of(CQ_BASKET)), na.rm = TRUE)) %>%
  arrange(FINCBTXM) %>%
  mutate(pctile = cumsum(FINLWT21) / sum(FINLWT21))

tot = sum(cex$FINLWT21 * cex$cons)

grp_share = function(cond) sum(cex$FINLWT21[cond] * cex$cons[cond]) / tot

cex_shares = tibble(
  group = c('Quintile 1','Quintile 2','Quintile 3','Quintile 4','Quintile 5',
            'Top 10%','Top 5%','Top 1%'),
  cex_share = c(
    grp_share(cex$pctile <= 0.2),
    grp_share(cex$pctile > 0.2 & cex$pctile <= 0.4),
    grp_share(cex$pctile > 0.4 & cex$pctile <= 0.6),
    grp_share(cex$pctile > 0.6 & cex$pctile <= 0.8),
    grp_share(cex$pctile > 0.8),
    grp_share(cex$pctile > 0.90),
    grp_share(cex$pctile > 0.95),
    grp_share(cex$pctile > 0.99)
  )
)

#-----------------------------------------------------------------------------
# On-model shares + side-by-side
#-----------------------------------------------------------------------------

onmodel = read_csv(ONMODEL_CSV, show_col_types = FALSE) %>%
  filter(measure == 'all_excises') %>%
  distinct(group, onmodel_share = share_consumption)

cmp = cex_shares %>%
  left_join(onmodel, by = 'group') %>%
  mutate(diff_pp = (onmodel_share - cex_share) * 100)

cat('\nConsumption share by income group: on-model (Tax-Data c_*) vs CEX\n\n')
cmp %>%
  mutate(across(c(cex_share, onmodel_share), ~ round(. * 100, 1)),
         diff_pp = round(diff_pp, 1)) %>%
  print(n = Inf)
