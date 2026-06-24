#-------------------------------------------------------------------------------
# networth_stats.R — 2026 net-worth statistics: Tax-Data vs Wealth-Tax-Simulator.
# Weighted counts of tax units / households and total net worth above a set of
# thresholds, overall and split married vs nonmarried.
#
# Tax-Data:  sim baseline detail (net_worth = Sigma economic assets - Sigma debts,
#            materialized), non-dependent tax units; married = MFJ (filing_status 2).
# WTS:       standalone baseline detail (net_worth.static, the aged-SCF+Forbes
#            economic net worth); married = its own SCF/Forbes indicator. NB WTS
#            forces every Forbes billionaire to married = 1 (data.R:279).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

TD_PATH  = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/wealth_full/baseline/static/detail/2026.csv'
WTS_PATH = '/nfs/roberts/scratch/pi_nrs36/jar335/wts_compare/2026062323/baseline/detail/2026.csv'
OUT_DIR  = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_tax'

thresholds = c(10e6, 50e6, 100e6, 500e6, 1e9, 5e9)
labels     = c('>= $10M', '>= $50M', '>= $100M', '>= $500M', '>= $1B', '>= $5B')

# ---- load + normalize to (weight, net_worth, married) -----------------------
td = fread(TD_PATH, select = c('weight', 'dep_status', 'filing_status', 'net_worth')) %>%
  as_tibble() %>%
  filter(dep_status == 0) %>%                       # non-dependent tax units
  transmute(weight, net_worth, married = filing_status == 2)

wts = fread(WTS_PATH, select = c('weight', 'married', 'net_worth.static')) %>%
  as_tibble() %>%
  transmute(weight, net_worth = `net_worth.static`, married = married == 1)

# ---- helper: counts + wealth above a threshold, by group --------------------
stat_rows = function(df, src) {
  total = tibble(source = src, threshold = 'Total (all units)',
                 count = sum(df$weight),
                 wealth_T = sum(df$weight * df$net_worth) / 1e12,
                 count_married = sum(df$weight[df$married]),
                 count_nonmarried = sum(df$weight[!df$married]))
  bands = map2_dfr(thresholds, labels, function(t, lab) {
    s = df %>% filter(net_worth >= t)
    tibble(source = src, threshold = lab,
           count = sum(s$weight),
           wealth_T = sum(s$weight * s$net_worth) / 1e12,
           count_married = sum(s$weight[s$married]),
           count_nonmarried = sum(s$weight[!s$married]))
  })
  bind_rows(total, bands)
}

all = bind_rows(stat_rows(td, 'TaxData'), stat_rows(wts, 'WTS'))

# ---- Table 1: counts + total net worth above threshold ----------------------
t1 = all %>%
  select(threshold, source, count, wealth_T) %>%
  pivot_wider(names_from = source, values_from = c(count, wealth_T)) %>%
  mutate(threshold = factor(threshold, levels = c('Total (all units)', labels))) %>%
  arrange(threshold) %>%
  select(threshold,
         `TD units` = count_TaxData, `TD wealth $T` = wealth_T_TaxData,
         `WTS units` = count_WTS,    `WTS wealth $T` = wealth_T_WTS)

cat('\n==================================================================\n')
cat('2026 NET WORTH: counts of units and total net worth ABOVE threshold\n')
cat('  Tax-Data = non-dependent tax units; WTS = households. net worth = economic\n')
cat('==================================================================\n')
print(t1 %>% mutate(`TD units` = round(`TD units`), `WTS units` = round(`WTS units`),
                    across(ends_with('$T'), ~ round(., 3))), n = 50)

# ---- Table 2: married vs nonmarried counts ----------------------------------
t2 = all %>%
  select(threshold, source, count_married, count_nonmarried) %>%
  pivot_wider(names_from = source, values_from = c(count_married, count_nonmarried)) %>%
  mutate(threshold = factor(threshold, levels = c('Total (all units)', labels))) %>%
  arrange(threshold) %>%
  select(threshold,
         `TD married` = count_married_TaxData, `TD nonmarried` = count_nonmarried_TaxData,
         `WTS married` = count_married_WTS,     `WTS nonmarried` = count_nonmarried_WTS)

cat('\n==================================================================\n')
cat('2026 NET WORTH: MARRIED vs NONMARRIED unit counts ABOVE threshold\n')
cat('  (Tax-Data married = MFJ; WTS forces all Forbes billionaires married)\n')
cat('==================================================================\n')
print(t2 %>% mutate(across(-threshold, ~ round(.))), n = 50)

# ---- write tidy CSV ---------------------------------------------------------
all %>%
  mutate(threshold = factor(threshold, levels = c('Total (all units)', labels))) %>%
  arrange(source, threshold) %>%
  write_csv(file.path(OUT_DIR, 'networth_stats_2026.csv'))
cat('\nwrote', file.path(OUT_DIR, 'networth_stats_2026.csv'), '\n')
