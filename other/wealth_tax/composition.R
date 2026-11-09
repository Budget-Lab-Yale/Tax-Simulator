#-------------------------------------------------------------------------------
# composition.R — 2026 asset composition by net-worth threshold: public
# (marketable, avoidance e=-7) vs private (closely-held, e=-17), Tax-Data vs WTS.
# Tests whether the conventional-score residual is an asset-composition story:
# if WTS's top tail is more PRIVATE, it avoids more, so its conventional level is
# lower (and TS's relatively higher).
#
# Classes (same economic content both models):
#   PUBLIC/marketable  = cash, equities, bonds, retirement(dc+db), life_ins,
#                        annuities, trusts, other_fin
#   PRIVATE/closely-held = pass_throughs, primary_home, other_home, re_fund,
#                          other_nonfin
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

OUT_DIR    = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_tax'
thresholds = c(10e6, 50e6, 100e6, 250e6, 500e6, 1e9, 5e9)
labels     = c('>= $10M', '>= $50M', '>= $100M', '>= $250M', '>= $500M', '>= $1B', '>= $5B')

# weighted public/private dollars + private share, by threshold (df has
# weight, net_worth, public, private)
comp_rows = function(df, src) {
  tot = tibble(source = src, threshold = 'Total',
               public_T = sum(df$weight * df$public)  / 1e12,
               private_T = sum(df$weight * df$private) / 1e12) %>%
    mutate(private_share = private_T / (public_T + private_T))
  bands = map2_dfr(thresholds, labels, function(t, lab) {
    s = df %>% filter(net_worth >= t)
    pub = sum(s$weight * s$public) / 1e12
    prv = sum(s$weight * s$private) / 1e12
    tibble(source = src, threshold = lab, public_T = pub, private_T = prv,
           private_share = prv / (pub + prv))
  })
  bind_rows(tot, bands)
}

# ---- Tax-Data (raw value.* columns) -----------------------------------------
MKT = c('value.cash','value.equities','value.bonds','value.dc','value.db',
        'value.life_ins','value.annuities','value.trusts','value.other_fin')
CHD = c('value.pass_throughs','value.primary_home','value.other_home',
        'value.re_fund','value.other_nonfin')
DBT = c('value.primary_mortgage','value.other_mortgage','value.credit_lines',
        'value.credit_cards','value.installment_debt','value.other_debt')

td = fread('/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline/tax_units_2026.csv',
           select = c('weight','dep_status', MKT, CHD, DBT)) %>%
  as_tibble() %>%
  filter(dep_status == 0) %>%
  mutate(public  = rowSums(across(all_of(MKT))),
         private = rowSums(across(all_of(CHD))),
         net_worth = public + private - rowSums(across(all_of(DBT)))) %>%
  select(weight, net_worth, public, private)

td_comp = comp_rows(td, 'TaxData')

# ---- WTS (replay the aging pipeline to 2026) --------------------------------
setwd('/nfs/roberts/project/pi_nrs36/jar335/Repositories/Wealth-Tax-Simulator')
suppressPackageStartupMessages(library(Hmisc))
scenario_ids    = c('baseline'); years = 2025:2026
output_root     = '/nfs/roberts/scratch/pi_nrs36/jar335/wts_compose'
write_microdata = FALSE
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
source('./src/config.R'); source('./src/data.R'); source('./src/calc.R'); source('./src/sim.R')

mp  = read_macro_projections('baseline')
aug = process_scf('baseline') %>% age_scf_historical(mp) %>% add_forbes_data()
static = aug
for (yr in 2025:2026) {
  cur = static %>% select(all_of(colnames(aug)))
  dc  = tibble(deccumulation_rate = rep(0, nrow(cur)))
  static = age_scf(cur, dc, mp, yr)
}
wts = static %>%
  mutate(public  = cash + equities + bonds + retirement + life_ins + annuities + other_fin + trusts,
         private = pass_throughs + primary_home + other_home + re_fund + other_nonfin,
         net_worth = public + private -
                     (primary_mortgage + other_mortgage + credit_lines + credit_cards +
                      installment_debt + other_debt)) %>%
  select(weight, net_worth, public, private)

wts_comp = comp_rows(wts, 'WTS')

# sanity: count >= $50M should be ~304k (matches earlier net-worth stats)
cat('WTS sanity: units >= $50M =',
    round(sum(wts$weight[wts$net_worth >= 50e6])), '(earlier stat: 304,231)\n\n')

# ---- combined table ---------------------------------------------------------
all = bind_rows(td_comp, wts_comp) %>%
  mutate(threshold = factor(threshold, levels = c('Total', labels))) %>%
  arrange(threshold, source)

tab = all %>%
  pivot_wider(names_from = source, values_from = c(public_T, private_T, private_share)) %>%
  arrange(threshold) %>%
  transmute(threshold,
            `TD pub $T`  = round(public_T_TaxData, 2),
            `TD priv $T` = round(private_T_TaxData, 2),
            `TD priv %`  = round(100 * private_share_TaxData, 1),
            `WTS pub $T`  = round(public_T_WTS, 2),
            `WTS priv $T` = round(private_T_WTS, 2),
            `WTS priv %`  = round(100 * private_share_WTS, 1))

cat('==================================================================\n')
cat('2026 ASSET COMPOSITION above net-worth threshold: PUBLIC vs PRIVATE\n')
cat('  private % = closely-held / gross assets (the e=-17 avoidance share)\n')
cat('==================================================================\n')
print(tab, n = 30)

write_csv(all, file.path(OUT_DIR, 'composition_2026.csv'))
cat('\nwrote', file.path(OUT_DIR, 'composition_2026.csv'), '\n')
