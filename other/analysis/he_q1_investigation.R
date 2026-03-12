################################################################################
# Investigation: what drives within-cell IQR blowup in Q1 from EITC/CTC?
# Cells are income_percentile × married × n_dep (matching horizontal.R)
################################################################################

library(tidyverse)

root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202603101323'
yr   = 2026

# Read detail files
read_det = function(scn) {
  read_csv(file.path(root, scn, 'static/detail', paste0(yr, '.csv')),
           show_col_types = FALSE,
           col_select = c(id, weight, filing_status, n_dep, dep_status,
                          expanded_inc, liab_iit_net, eitc, ctc_nonref, ctc_ref,
                          wages, wages1, wages2, agi, age1)) %>%
    mutate(scn = scn)
}

all = map_dfr(c('baseline', 'std', 'std_eitc', 'std_eitc_ctc'), read_det)

# Filter same as horizontal.R: non-dependents, positive income
all = all %>% filter(dep_status == 0, expanded_inc > 0)

# Use baseline income for percentile ranking
base_inc = all %>%
  filter(scn == 'baseline') %>%
  arrange(expanded_inc) %>%
  mutate(
    inc_pctile = cut(
      x      = expanded_inc,
      breaks = c(-Inf, Hmisc::wtd.quantile(expanded_inc, weight, seq(0.01, 0.99, 0.01)), Inf),
      labels = 1:100,
      include.lowest = TRUE
    ) %>% as.character() %>% as.numeric()
  ) %>%
  select(id, inc_pctile, base_inc = expanded_inc)

all = all %>% left_join(base_inc, by = 'id')

# Compute ETR and married flag (matching horizontal.R)
all = all %>%
  mutate(
    married = as.integer(filing_status == 2),
    etr     = pmax(-1, pmin(1, liab_iit_net / base_inc))
  )

# Quintile assignment
all = all %>% mutate(quintile = floor((inc_pctile - 1) / 20) + 1)

# ---- Cell-level IQR computation (matching horizontal.R) ----
cell_iqr = all %>%
  group_by(scn, inc_pctile, married, n_dep) %>%
  summarise(
    n         = sum(weight),
    n_records = n(),
    group_iqr = if_else(
      n() > 2,
      Hmisc::wtd.quantile(etr, weight, 0.75) - Hmisc::wtd.quantile(etr, weight, 0.25),
      0
    ),
    mean_etr  = weighted.mean(etr, weight),
    .groups   = 'drop'
  ) %>%
  mutate(quintile = floor((inc_pctile - 1) / 20) + 1)

# ---- Q1 cell-level IQR: which cells have biggest IQR and biggest changes? ----
cat('\n===== Q1: Top 20 cells by IQR under std_eitc =====\n\n')
cell_iqr %>%
  filter(quintile == 1, scn == 'std_eitc') %>%
  arrange(desc(group_iqr * n)) %>%
  head(20) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = 20, width = Inf)

# ---- Compare cell IQR: baseline vs std_eitc for Q1 ----
cat('\n\n===== Q1: Biggest IQR *increases* from baseline → std_eitc =====\n\n')
iqr_compare = cell_iqr %>%
  filter(quintile == 1) %>%
  select(scn, inc_pctile, married, n_dep, group_iqr, n) %>%
  pivot_wider(names_from = scn, values_from = c(group_iqr, n)) %>%
  mutate(
    iqr_change = group_iqr_std_eitc - group_iqr_baseline,
    weighted_change = iqr_change * n_baseline
  ) %>%
  arrange(desc(weighted_change))

iqr_compare %>%
  head(20) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = 20, width = Inf)

# ---- Same for baseline → std_eitc_ctc ----
cat('\n\n===== Q1: Biggest IQR *increases* from baseline → std_eitc_ctc =====\n\n')
iqr_compare2 = cell_iqr %>%
  filter(quintile == 1) %>%
  select(scn, inc_pctile, married, n_dep, group_iqr, n) %>%
  pivot_wider(names_from = scn, values_from = c(group_iqr, n)) %>%
  mutate(
    iqr_change = group_iqr_std_eitc_ctc - group_iqr_baseline,
    weighted_change = iqr_change * n_baseline
  ) %>%
  arrange(desc(weighted_change))

iqr_compare2 %>%
  head(20) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = 20, width = Inf)

# ---- Weighted avg IQR by quintile and scenario (replicating horizontal.R) ----
cat('\n\n===== Weighted avg within-cell IQR by quintile × scenario =====\n\n')
cell_iqr %>%
  group_by(scn, quintile) %>%
  summarise(avg_iqr = weighted.mean(group_iqr, n) * 100, .groups = 'drop') %>%
  pivot_wider(names_from = scn, values_from = avg_iqr) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

# ---- Deep dive: what varies WITHIN a single large cell? ----
# Pick the biggest-impact cell and look at what drives ETR dispersion
cat('\n\n===== Deep dive: Single 0-dep cells in Q1 under std_eitc =====\n')
cat('(These are the dominant cells by population)\n\n')

# For single, 0 dep, percentile 1-20: what creates ETR variation?
single_0dep = all %>%
  filter(quintile == 1, married == 0, n_dep == 0, scn == 'std_eitc')

cat('N records:', nrow(single_0dep), ' N weighted:', round(sum(single_0dep$weight)/1e6, 2), 'M\n\n')

single_0dep %>%
  mutate(
    has_wages   = wages > 0,
    has_eitc    = eitc > 0,
    age_group   = case_when(age1 < 25 ~ '<25', age1 < 65 ~ '25-64', TRUE ~ '65+'),
    etr_bucket  = cut(etr * 100, breaks = c(-Inf, -20, -10, -5, -1, 0, 1, 5, Inf))
  ) %>%
  group_by(has_wages, has_eitc, age_group) %>%
  summarise(
    n          = sum(weight),
    mean_etr   = weighted.mean(etr, weight) * 100,
    mean_wages = weighted.mean(wages, weight),
    mean_eitc  = weighted.mean(eitc, weight),
    .groups    = 'drop'
  ) %>%
  arrange(desc(n)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

# ETR distribution for single, 0 dep, Q1 -- baseline vs std_eitc
cat('\n\nETR percentiles for Single, 0 dep, Q1:\n\n')
for (s in c('baseline', 'std_eitc')) {
  tmp = all %>% filter(quintile == 1, married == 0, n_dep == 0, scn == s)
  pctiles = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
  vals = Hmisc::wtd.quantile(tmp$etr, tmp$weight, pctiles) * 100
  cat(sprintf("  %s: p5=%.1f p10=%.1f p25=%.1f p50=%.1f p75=%.1f p90=%.1f p95=%.1f\n",
      s, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]))
}

# But this is pooled across percentiles. Let's look at a single percentile cell
cat('\n\nETR percentiles for Single, 0 dep, percentile=5 (mid-Q1):\n\n')
for (s in c('baseline', 'std_eitc')) {
  tmp = all %>% filter(inc_pctile == 5, married == 0, n_dep == 0, scn == s)
  pctiles = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
  vals = Hmisc::wtd.quantile(tmp$etr, tmp$weight, pctiles) * 100
  cat(sprintf("  %s: p5=%.1f p10=%.1f p25=%.1f p50=%.1f p75=%.1f p90=%.1f p95=%.1f  (n=%d)\n",
      s, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7], nrow(tmp)))
}

cat('\n\nETR percentiles for Single, 0 dep, percentile=10:\n\n')
for (s in c('baseline', 'std_eitc')) {
  tmp = all %>% filter(inc_pctile == 10, married == 0, n_dep == 0, scn == s)
  pctiles = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
  vals = Hmisc::wtd.quantile(tmp$etr, tmp$weight, pctiles) * 100
  cat(sprintf("  %s: p5=%.1f p10=%.1f p25=%.1f p50=%.1f p75=%.1f p90=%.1f p95=%.1f  (n=%d)\n",
      s, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7], nrow(tmp)))
}

# What distinguishes people within percentile=5, single, 0dep who get EITC vs not?
cat('\n\nPercentile=5, Single, 0 dep: EITC vs no EITC under std_eitc:\n\n')
all %>%
  filter(inc_pctile == 5, married == 0, n_dep == 0, scn == 'std_eitc') %>%
  mutate(has_eitc = eitc > 0) %>%
  group_by(has_eitc) %>%
  summarise(
    n          = sum(weight),
    mean_etr   = weighted.mean(etr, weight) * 100,
    mean_wages = weighted.mean(wages, weight),
    mean_eitc  = weighted.mean(eitc, weight),
    mean_inc   = weighted.mean(base_inc, weight),
    .groups    = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(width = Inf)
