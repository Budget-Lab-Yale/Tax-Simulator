################################################################################
# Rampell zero-liability calculation
# For baseline / CVH (full bill) / Booker (full bill), compute the share of
# nondependent tax units with liab_iit_net <= 0 (i.e. no income tax owed
# net of refundable credits like EITC/CTC), sliced two ways:
#   (A) by AGI cutoff (cumulative: Under $25K, $50K, $75K, $100K, All)
#   (B) by AGI percentile (cumulative: Bottom Quintile, Bottom Half, All)
# Each slice is shown for Overall / Parents / Non-parents.
# Parent = any dep_age{1,2,3} < 18 (matches booker_kypa_stacked_distribution.R).
################################################################################

library(tidyverse)
library(data.table)

# --- Configuration ----------------------------------------------------------
vintage  = '202604301231'
out_root = file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1', vintage)
year     = 2026

scenarios = c(baseline = 'baseline', cvh = 'cvh', booker = 'booker')

# --- Read detail ------------------------------------------------------------
read_detail = function(scn) {
  path = file.path(out_root, scn, 'static/detail', paste0(year, '.csv'))
  fread(path, select = c('id', 'weight', 'dep_status', 'agi',
                         'liab_iit_net', 'dep_age1', 'dep_age2', 'dep_age3'))
}

dat = imap_dfr(scenarios, function(scn, label) {
  read_detail(scn) %>%
    filter(dep_status == 0) %>%
    mutate(
      scenario = label,
      zero    = as.integer(liab_iit_net <= 0),
      parent  = if_else(
        (!is.na(dep_age1) & dep_age1 < 18) |
        (!is.na(dep_age2) & dep_age2 < 18) |
        (!is.na(dep_age3) & dep_age3 < 18),
        'parent', 'nonparent'
      )
    )
})

# --- Helper: weighted share of zero-liability units in a subset -------------
share_zero = function(df) {
  if (nrow(df) == 0 || sum(df$weight) == 0) return(NA_real_)
  weighted.mean(df$zero, df$weight)
}

# Compute share for one parent-status filter (NULL = overall) across scenarios
# in a subset defined by an AGI predicate or pctile predicate column.
shares_by_scn = function(df, parent_filter = NULL) {
  if (!is.null(parent_filter)) df = df %>% filter(parent == parent_filter)
  df %>%
    group_by(scenario) %>%
    summarise(share = share_zero(cur_data()), .groups = 'drop') %>%
    pivot_wider(names_from = scenario, values_from = share)
}

# --- Table A: AGI cutoffs ---------------------------------------------------
agi_cuts = list(
  'Under $25K'  = 25e3,
  'Under $50K'  = 50e3,
  'Under $75K'  = 75e3,
  'Under $100K' = 100e3,
  'All'         = Inf
)

table_a = imap_dfr(agi_cuts, function(cut, label) {
  sub = dat %>% filter(agi < cut)
  bind_cols(
    tibble(group = label),
    shares_by_scn(sub, NULL)         %>% rename_with(~ paste0(.x, '_overall')),
    shares_by_scn(sub, 'parent')     %>% rename_with(~ paste0(.x, '_parent')),
    shares_by_scn(sub, 'nonparent')  %>% rename_with(~ paste0(.x, '_nonparent'))
  )
})

# --- Table B: AGI percentile cutoffs ----------------------------------------
# Rank by AGI within each parent group (separately for overall, parents,
# nonparents) using baseline AGI. AGI is identical across scenarios in static
# mode, so percentile assignment is propagated via id.
build_pctiles = function(parent_filter = NULL) {
  d = dat %>% filter(scenario == 'baseline')
  if (!is.null(parent_filter)) d = d %>% filter(parent == parent_filter)
  d %>%
    arrange(agi) %>%
    mutate(pctile = cumsum(weight) / sum(weight)) %>%
    select(id, pctile)
}

pct_overall   = build_pctiles(NULL)
pct_parent    = build_pctiles('parent')
pct_nonparent = build_pctiles('nonparent')

pct_cuts = list(
  'Bottom Quintile' = 0.20,
  'Bottom Half'     = 0.50,
  'All'             = 1.00
)

shares_for_pct = function(pct_df, parent_filter, cut) {
  d = dat
  if (!is.null(parent_filter)) d = d %>% filter(parent == parent_filter)
  d %>%
    inner_join(pct_df, by = 'id') %>%
    filter(pctile <= cut) %>%
    group_by(scenario) %>%
    summarise(share = share_zero(cur_data()), .groups = 'drop') %>%
    pivot_wider(names_from = scenario, values_from = share)
}

table_b = imap_dfr(pct_cuts, function(cut, label) {
  bind_cols(
    tibble(group = label),
    shares_for_pct(pct_overall,   NULL,        cut) %>% rename_with(~ paste0(.x, '_overall')),
    shares_for_pct(pct_parent,    'parent',    cut) %>% rename_with(~ paste0(.x, '_parent')),
    shares_for_pct(pct_nonparent, 'nonparent', cut) %>% rename_with(~ paste0(.x, '_nonparent'))
  )
})

# --- Format & print ---------------------------------------------------------
col_order = c('group',
              'baseline_overall',   'cvh_overall',   'booker_overall',
              'baseline_parent',    'cvh_parent',    'booker_parent',
              'baseline_nonparent', 'cvh_nonparent', 'booker_nonparent')

fmt = function(df) {
  df %>%
    select(all_of(col_order)) %>%
    mutate(across(-group, ~ round(.x * 100, 1)))
}

cat('\n===== Share of nondependent tax units with liab_iit_net <= 0 (', year, ') =====\n')
cat('Columns: <scenario>_<group> for group in {overall, parent, nonparent}\n\n')

cat('--- Table A: by AGI cutoff ---\n')
print(fmt(table_a), n = Inf, width = Inf)

cat('\n--- Table B: by AGI percentile (ranked within group) ---\n')
print(fmt(table_b), n = Inf, width = Inf)

# --- Save CSV ---------------------------------------------------------------
out_csv = file.path(out_root, 'rampell_zero_liability.csv')
bind_rows(
  fmt(table_a) %>% mutate(slice = 'AGI cutoff', .before = 1),
  fmt(table_b) %>% mutate(slice = 'AGI percentile', .before = 1)
) %>%
  write_csv(out_csv)

cat('\nResults saved to:', out_csv, '\n')
