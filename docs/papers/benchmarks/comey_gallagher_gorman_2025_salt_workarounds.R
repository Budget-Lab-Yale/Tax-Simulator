#------------------------------------------------------------------------------
# Benchmarks the model's PTET (SALT cap workaround) treatment against
# Comey, Gallagher & Gorman (2025), Table 3-4 and p.17. Reads the 2024 static
# detail files from a run of config/runscripts/tests/ptet_benchmark.csv and
# prints the model's numbers beside the paper's.
#
# Usage (repo root):
#   Rscript docs/papers/benchmarks/comey_gallagher_gorman_2025_salt_workarounds.R <vintage_root>
# where <vintage_root> holds the baseline/ and no_workaround/ scenario folders,
# e.g. $TAXSIM_LOCAL_ROOT/model_data/Tax-Simulator/v1/<vintage>.
#
# The detail files carry no workaround columns. The workaround amount is
# recovered as the no_workaround-minus-baseline change in part_scorp, which is
# exact as long as the moved SALT stays inside active passthrough losses.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
})

PAPER_ID       = 'comey_gallagher_gorman_2025_salt_workarounds'
BENCHMARK_YEAR = 2024
COUNTERFACTUAL = 'no_workaround'

vintage_root = commandArgs(trailingOnly = T)[1]
if (is.na(vintage_root) || !dir.exists(vintage_root)) {
  stop('Supply the vintage root holding baseline/ and ', COUNTERFACTUAL, '/')
}

read_detail = function(scenario) {
  path = file.path(vintage_root, scenario, 'static/detail', paste0(BENCHMARK_YEAR, '.csv'))
  if (!file.exists(path)) stop('Missing detail file: ', path)
  fread(path) %>% tibble()
}

baseline = read_detail('baseline')
no_workaround = read_detail(COUNTERFACTUAL) %>%
  select(id, part_scorp_cf = part_scorp, liab_iit_net_cf = liab_iit_net,
         txbl_inc_cf = txbl_inc, qbi_ded_cf = qbi_ded, salt_item_ded_cf = salt_item_ded)

if (!'mtr_part_active' %in% names(baseline)) {
  stop('Baseline detail lacks mtr_part_active; run the benchmark runscript as written')
}

units = baseline %>%
  inner_join(no_workaround, by = 'id') %>%
  mutate(
    moved      = part_scorp_cf - part_scorp,
    cost       = liab_iit_net_cf - liab_iit_net,
    d_txbl_inc = txbl_inc_cf - txbl_inc,
    d_qbi      = qbi_ded_cf - qbi_ded,
    d_salt     = salt_item_ded_cf - salt_item_ded
  )
stopifnot(nrow(units) == nrow(baseline))

# Ranking matches src/data/post_processing/distribution.R: non-dependent units,
# baseline expanded income, weighted percentile with negative income excluded
ranked = units %>%
  filter(dep_status == 0, weight > 0) %>%
  arrange(expanded_inc) %>%
  mutate(
    income_pctile = cumsum(weight * (expanded_inc >= 0)) / sum(weight * (expanded_inc >= 0)),
    income_pctile = if_else(expanded_inc < 0, NA_real_, income_pctile)
  )

total_cost  = sum(ranked$weight * ranked$cost)
cost_share  = function(cutoff) {
  with(ranked, sum((weight * cost)[!is.na(income_pctile) & income_pctile > cutoff])) / total_cost
}
moved_total = sum(units$weight * units$moved)

model = tribble(
  ~param_id,                  ~model,
  'ptet_ded_indiv_2024',      moved_total / 1e9,
  'fiscal_cost_2024',         sum(units$weight * units$cost) / 1e9,
  'txbl_inc_increase_2024',   sum(units$weight * units$d_txbl_inc) / 1e9,
  'qbi_share_marginal_pt',    sum(units$weight * units$d_qbi) / moved_total,
  'salt_share_deductible',    sum(units$weight * units$d_salt) / moved_total,
  'mtr_pt_avg',               with(units, sum(weight * moved * mtr_part_active) / moved_total),
  'cost_share_top1',          cost_share(0.99),
  'cost_share_top5',          cost_share(0.95),
  'cost_share_top01',         cost_share(0.999)
) %>%
  mutate(param_id = paste0(PAPER_ID, '.', param_id))

paper = read_csv('docs/papers/parameters.csv', show_col_types = F) %>%
  select(param_id, quantity, paper = value, unit)

comparison = model %>%
  left_join(paper, by = 'param_id') %>%
  mutate(paper = as.numeric(paper), ratio = model / paper) %>%
  select(param_id, quantity, unit, paper, model, ratio)

cat('Vintage root:', vintage_root, '\n')
cat('Tax units:', nrow(units), ' moved > 0:', sum(units$moved > 0), '\n\n')
comparison %>%
  mutate(param_id = sub(paste0(PAPER_ID, '.'), '', param_id, fixed = T)) %>%
  select(-quantity) %>%
  as.data.frame() %>%
  print(digits = 3, row.names = F)
