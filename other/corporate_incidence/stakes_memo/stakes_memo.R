#-------------------------------------------------------------------------------
# stakes_memo.R
#
# Phase 0a decision-gate analysis for the on-model corporate incidence channel
# (other/corporate_incidence/CONSIDERATIONS.md §10, implementation plan Phase 0).
# NO model changes: a static reallocation/materiality exercise on existing
# baseline detail files + Tax-Data value.* columns.
#
# Canonical shock: corporate rate 21% -> 28%, priced as permanent.
#   flow factor  phi = -(0.28-0.21)/(1-0.21) = -8.86%   (naive, pre-theta)
#   markdown     mu  = +8.86%                            (P1 ceiling)
#
# Computes:
#   (i)   naive-markdown capitalization hit to household equity by net-worth
#         percentile (+ the value.db slice, sized ONLY for the D10 residual)
#   (ii)  taxable-flow offset ceiling (div + kg at effective rates) vs the
#         receipts wedge -- the cents-per-dollar conventional offset
#   (iii) estate and wealth-tax base/revenue deltas over the 30-year window
#         (estate: expected marginal 40% on taxable estates; wealth: warren
#         scenario mtr_net_worth); deemed-realization leg noted, not computed
#   (iv)  holdings-based vs current-smear allocation of a fixed $100B corporate
#         burden (the standalone-publishable defect finding, review item 8d)
#   (v)   estate portfolio composition among taxable estates (D15 kappa
#         direction: corporate-equity-heavy vs pt/interest-heavy top tail)
#
# Data (pinned):
#   - Tax-Simulator baseline: scratch vintage warren_nd_30yr (full sample,
#     2026-2055, branch `wealth`, Tax-Data 2026060918). Caveat: predates the
#     2026-07 calc fixes (~-$8.4B/yr IIT levels) -- immaterial for this gate.
#   - Tax-Data 2026060918 for value.* (rejoin by id, house pattern)
#   - Macro-Projections 2026022522 for rev_corp/gdp_corp ($B)
#
# PLACEHOLDER EXPOSURES (Phase 0c measurement replaces; flagged in output):
#   equity shares: value.equities 1.0, value.dc 0.55, value.trusts 0.50,
#   value.re_fund 0.30; omega_div 0.85; omega_kg 0.50; effective rates
#   tau_div 0.18, tau_kg 0.20.
#
# Output: CSVs under other/corporate_incidence/stakes_memo/out/
# Run: sbatch other/corporate_incidence/stakes_memo/stakes_memo.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

SIM_ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/warren_nd_30yr'
TD_ROOT  = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline'
MACRO    = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline/projections.csv'
OUT_DIR  = './other/corporate_incidence/stakes_memo/out'
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Canonical shock
TAU0 = 0.21; TAU1 = 0.28
MU  = (TAU1 - TAU0) / (1 - TAU0)   # 0.0886 markdown ceiling
PHI = -MU                          # flow factor

# Placeholder exposures (FLAGGED; Phase 0c replaces)
W_EQ = 1.00; W_DC = 0.55; W_TR = 0.50; W_REF = 0.30
W_DIV = 0.85; W_KG = 0.50
TAU_DIV_EFF = 0.18; TAU_KG_EFF = 0.20
EST_MARGINAL_RATE = 0.40

detail_years = list.files(file.path(SIM_ROOT, 'baseline/static/detail')) %>%
  str_remove('\\.csv$') %>% as.integer() %>% sort()
td_years = list.files(TD_ROOT, pattern = '^tax_units_\\d{4}\\.csv$') %>%
  str_extract('\\d{4}') %>% as.integer()
years = intersect(detail_years, td_years)
cat('Years:', min(years), '-', max(years), '(', length(years), ')\n')

read_detail = function(t, cols) {
  fread(file.path(SIM_ROOT, 'baseline/static/detail', paste0(t, '.csv')),
        select = cols) %>% tibble()
}
read_td = function(t, cols) {
  fread(file.path(TD_ROOT, paste0('tax_units_', t, '.csv')),
        select = cols) %>% tibble()
}

VAL_COLS = c('value.equities', 'value.dc', 'value.trusts', 'value.re_fund',
             'value.db', 'value.bonds', 'value.cash', 'value.pass_throughs')

exposure_of = function(td) {
  W_EQ  * td$value.equities + W_DC * td$value.dc +
  W_TR  * td$value.trusts   + W_REF * td$value.re_fund
}

# Weighted NW percentile bins (dep_status == 0 records; NA -> untouched)
nw_bin = function(nw, w) {
  ord = order(nw)
  cw  = cumsum(w[ord]) / sum(w)
  p   = numeric(length(nw)); p[ord] = cw
  cut(p, breaks = c(0, .5, .9, .99, .999, 1), include.lowest = TRUE,
      labels = c('bottom50', 'p50_90', 'p90_99', 'p99_99.9', 'top0.1'))
}

#-------------------------------------------------------------------------------
# 30-year loop: estate + wealth legs, aggregate exposure series
#-------------------------------------------------------------------------------

series = list()
for (t in years) {
  det = read_detail(t, c('id', 'weight', 'dep_status', 'net_worth', 'estate_m',
                         'estate_p_dsue', 'liab_estate_nodsue',
                         'liab_estate_dsue', 'estate_distributable'))
  td  = read_td(t, c('id', VAL_COLS))
  d   = det %>% left_join(td, by = 'id')

  expo = exposure_of(d)
  eq_share = pmin(pmax(if_else(d$net_worth > 0, expo / d$net_worth, 0), 0), 1)

  # p_dsue is NA for joint records (no DSUE branch): fall back to the nodsue leg
  p_dsue = coalesce(d$estate_p_dsue, 0)
  liab_est_exp = p_dsue * coalesce(d$liab_estate_dsue, 0) +
                 (1 - p_dsue) * coalesce(d$liab_estate_nodsue, 0)
  taxable = liab_est_exp > 0

  series[[as.character(t)]] = tibble(
    year               = t,
    hh_equity_exposure = sum(d$weight * expo, na.rm = TRUE) / 1e9,
    markdown_hit       = MU * hh_equity_exposure,
    db_slice           = MU * sum(d$weight * d$value.db, na.rm = TRUE) / 1e9,
    est_base           = sum(d$weight * d$estate_m * d$estate_distributable,
                             na.rm = TRUE) / 1e9,
    est_base_delta     = MU * sum(d$weight * d$estate_m * eq_share *
                                  d$estate_distributable, na.rm = TRUE) / 1e9,
    est_tax_delta      = MU * EST_MARGINAL_RATE *
                         sum((d$weight * d$estate_m * eq_share *
                              d$estate_distributable)[taxable], na.rm = TRUE) / 1e9)
}
series = bind_rows(series)

# Wealth-tax leg on the warren scenario (static detail carries mtr_net_worth
# under the wealth-tax law); skipped gracefully if absent
warren_detail = file.path(SIM_ROOT, 'warren/static/detail')
if (dir.exists(warren_detail)) {
  wt = map_dfr(years, function(t) {
    f = file.path(warren_detail, paste0(t, '.csv'))
    if (!file.exists(f)) return(NULL)
    dw = fread(f, select = c('id', 'weight', 'mtr_net_worth')) %>% tibble()
    td = read_td(t, c('id', VAL_COLS))
    d  = dw %>% left_join(td, by = 'id')
    tibble(year = t,
           wealth_tax_delta = MU * sum(d$weight * d$mtr_net_worth *
                                       exposure_of(d), na.rm = TRUE) / 1e9)
  })
  series = series %>% left_join(wt, by = 'year')
}
write_csv(series, file.path(OUT_DIR, 'series_30yr.csv'))
cat('Wrote series_30yr.csv\n')

#-------------------------------------------------------------------------------
# Enactment-year (first year) cross-sections
#-------------------------------------------------------------------------------

t0  = min(years)
det = read_detail(t0, c('id', 'weight', 'dep_status', 'net_worth', 'estate_m',
                        'estate_p_dsue', 'liab_estate_nodsue', 'liab_estate_dsue',
                        'estate_distributable', 'expanded_inc',
                        'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
                        'kg_st', 'kg_lt', 'sole_prop', 'part_scorp', 'farm',
                        'wages'))
td  = read_td(t0, c('id', VAL_COLS))
d   = det %>% left_join(td, by = 'id') %>% filter(dep_status == 0)

d$expo = exposure_of(d)
d$bin  = nw_bin(d$net_worth, d$weight)

# (i) markdown hit by NW percentile
hit_tbl = d %>%
  group_by(bin) %>%
  summarise(
    n_units       = sum(weight) / 1e6,
    equity_expo_B = sum(weight * expo, na.rm = TRUE) / 1e9,
    markdown_B    = MU * equity_expo_B,
    mean_hit      = MU * sum(weight * expo, na.rm = TRUE) / sum(weight),
    .groups = 'drop') %>%
  mutate(share = markdown_B / sum(markdown_B))
write_csv(hit_tbl, file.path(OUT_DIR, 'markdown_by_nw_bin.csv'))

# (ii) flow-offset ceiling vs receipts wedge
macro = read_csv(MACRO, show_col_types = FALSE) %>% select(year, rev_corp, gdp_corp)
offset = d %>%
  summarise(
    d_div_B    = abs(PHI) * W_DIV * sum(weight * (div_ord + div_pref)) / 1e9,
    d_kg_B     = abs(PHI) * W_KG  * sum(weight * (kg_st + kg_lt)) / 1e9,
    offset_B   = d_div_B * TAU_DIV_EFF + d_kg_B * TAU_KG_EFF) %>%
  mutate(year          = t0,
         wedge_B       = (TAU1 - TAU0) / TAU0 *
                         macro$rev_corp[macro$year == t0],
         cents_per_dlr = offset_B / wedge_B * 100)
write_csv(offset, file.path(OUT_DIR, 'flow_offset_ceiling.csv'))

# (iv) holdings-based vs smear allocation of $100B (capital legs compared)
BURDEN_B = 100
d = d %>%
  mutate(
    capital_smear = pmax(0, (sole_prop + part_scorp + farm) * 0.2 + txbl_int +
                            exempt_int + div_ord + div_pref + kg_st + kg_lt),
    alloc_smear    = BURDEN_B * capital_smear / sum(capital_smear * weight),
    alloc_holdings = BURDEN_B * pmax(expo, 0) / sum(pmax(expo, 0) * weight))

cmp_nw = d %>%
  group_by(bin) %>%
  summarise(smear_B    = sum(weight * alloc_smear),
            holdings_B = sum(weight * alloc_holdings), .groups = 'drop')
write_csv(cmp_nw, file.path(OUT_DIR, 'smear_vs_holdings_by_nw.csv'))

cmp_inc = d %>%
  mutate(inc_decile = nw_bin(expanded_inc, weight)) %>%   # same cuts, on income
  group_by(inc_decile) %>%
  summarise(smear_B    = sum(weight * alloc_smear),
            holdings_B = sum(weight * alloc_holdings), .groups = 'drop')
write_csv(cmp_inc, file.path(OUT_DIR, 'smear_vs_holdings_by_income.csv'))

# The headline defect case: wealth-rich, flow-poor (top 1% NW, bottom-half
# capital flows) and the converse
flow_med = with(d, {o = order(capital_smear); cw = cumsum(weight[o])/sum(weight)
                    capital_smear[o][which.min(abs(cw - 0.5))]})
defect = d %>%
  mutate(grp = case_when(
    bin %in% c('p99_99.9', 'top0.1') & capital_smear <= flow_med ~ 'top1pct_NW_flowpoor',
    bin %in% c('p99_99.9', 'top0.1')                             ~ 'top1pct_NW_flowrich',
    TRUE                                                         ~ 'other')) %>%
  group_by(grp) %>%
  summarise(n_units    = sum(weight),
            smear_B    = sum(weight * alloc_smear),
            holdings_B = sum(weight * alloc_holdings), .groups = 'drop')
write_csv(defect, file.path(OUT_DIR, 'smear_defect_cases.csv'))

# (v) estate portfolio composition among taxable estates (D15 direction)
d$liab_est_exp = coalesce(d$estate_p_dsue, 0) * coalesce(d$liab_estate_dsue, 0) +
                 (1 - coalesce(d$estate_p_dsue, 0)) * coalesce(d$liab_estate_nodsue, 0)
comp = d %>%
  filter(liab_est_exp > 0) %>%
  summarise(
    n_taxable    = sum(weight * estate_m),
    equities     = sum(weight * estate_m * expo, na.rm = TRUE),
    pass_through = sum(weight * estate_m * value.pass_throughs, na.rm = TRUE),
    interest     = sum(weight * estate_m * (value.bonds + value.cash), na.rm = TRUE),
    net_worth    = sum(weight * estate_m * net_worth, na.rm = TRUE)) %>%
  mutate(across(c(equities, pass_through, interest), ~ . / net_worth,
                .names = 'share_{.col}'))
write_csv(comp, file.path(OUT_DIR, 'taxable_estate_composition.csv'))

#-------------------------------------------------------------------------------
# Assumptions ledger (everything flagged)
#-------------------------------------------------------------------------------
tibble(
  param = c('mu (markdown ceiling)', 'phi (flow factor)', 'w_eq', 'w_dc',
            'w_trusts', 'w_re_fund', 'omega_div', 'omega_kg', 'tau_div_eff',
            'tau_kg_eff', 'estate marginal rate', 'sim vintage', 'tax-data',
            'macro'),
  value = c(MU, PHI, W_EQ, W_DC, W_TR, W_REF, W_DIV, W_KG, TAU_DIV_EFF,
            TAU_KG_EFF, EST_MARGINAL_RATE, NA, NA, NA),
  note  = c('naive P1 ceiling, permanent 21->28', 'proportional payout (D8)',
            'first-class column', 'PLACEHOLDER (SCF/ICI, Phase 0c)',
            'PLACEHOLDER (Phase 0c)', 'PLACEHOLDER (Phase 0c)',
            'PLACEHOLDER excl. REIT/bond funds (Phase 0c)',
            'PLACEHOLDER (SOI SOCA, Phase 0c)', 'ASSUMED qualified-div effective',
            'ASSUMED LTCG+NIIT effective', 'flat above-exemption approximation',
            'warren_nd_30yr (pre-2026-07 calc fixes)', '2026060918', '2026022522')
) %>% write_csv(file.path(OUT_DIR, 'assumptions.csv'))

cat('DONE. Outputs in', normalizePath(OUT_DIR), '\n')
