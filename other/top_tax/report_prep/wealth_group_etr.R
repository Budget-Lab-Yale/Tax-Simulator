#------------------------------------------------------------------------------
# wealth_group_etr.R
#
# Effective tax rates for wealth-ranked top groups under current law. Rebuilds
# the distribution_etrs per-record microdata for the top_tax_dials_30y_v6
# vintage (baseline law, 2027, static leg) and composes ETR rows for the top 1,
# 0.1 and 0.01 percent of net worth plus every tax unit with net worth of at
# least $1 billion, using the same etr_group_sums()/compose_etr_rows()
# machinery as the shipped cube.
#
# Groups are cumulative top groups over records sorted by baseline (static,
# un-haircut) net worth descending, weighted by tax units. The billionaire
# group is a level cut, not a percentile.
#
# Output:
#   other/top_tax/report_prep/wealth_group_etr_2027.csv  (distribution_etrs schema)
#   other/top_tax/report_prep/wealth_group_etr_diag.csv  (group diagnostics)
#
# Run via sbatch (never on the login node):
#   sbatch -p day -c 1 --mem=32G -t 2:00:00 \
#     --wrap "cd <repo> && module load R/4.4.1-foss-2022b && \
#             Rscript other/top_tax/report_prep/wealth_group_etr.R"
#------------------------------------------------------------------------------

REPO    = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
STAGING = paste0('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/',
                 'Tax-Simulator/v1/top_tax_dials_30y_v6/_slurm_staging')
OUT_DIR = file.path(REPO, 'other/top_tax/report_prep')

setwd(REPO)
source('src/slurm/common.R')
invisible(reconstitute_environment(STAGING))

cat('output_root:', globals$output_root, '\n')

id          = 'pc_ordr50_cgr30'   # any scenario with kept detail; only the
baseline_id = 'baseline'          # baseline-leg columns are consumed
yr          = 2027

# The distribution and corporate-allocation reads are economy-leg values, so the
# scenario has to be activated as the post-processing phase does it.
config = readRDS(file.path(STAGING, id, 'config.rds'))
config_activate(economy  = config$scenario_info$resolved_economy,
                behavior = config$scenario_info$resolved_behavior)

other_taxes = get_other_taxes(id, baseline_id)
rev_corp = read_macro_spliced(interface_root('Macro-Projections', baseline_id)) %>%
  select(year, rev_corp_level = rev_corp)

md = process_for_etrs(id, baseline_id, yr, other_taxes, rev_corp,
                      reform_leg = 'static')
cat('microdata rows:', nrow(md), '\n')

# Sort by net worth descending and take cumulative top groups by weight. The
# record straddling a cutoff is included, matching the top-400 convention.
ord   = order(-md$net_worth)
cw    = cumsum(md$weight[ord])
W     = sum(md$weight)

pct_groups = c('Top 1%' = 0.01, 'Top 0.1%' = 0.001, 'Top 0.01%' = 0.0001)

members = pct_groups %>%
  map(~ ord[seq_len(sum(dplyr::lag(cw, default = 0) < W * .x))])
members[['Billionaires']] = which(md$net_worth >= 1e9)

rows = imap(members, function(idx, label) {
  grp = rep(NA_character_, nrow(md))
  grp[idx] = label
  sums = etr_group_sums(md, grp) %>% filter(!is.na(group))
  ETR_INCOME_DEFS %>%
    map(~ compose_etr_rows(sums, .x, ranking = 'wealth',
                           group_dimension = 'Net worth',
                           cutoff_col = 'cutoff_nw',
                           include_other = isTRUE(attr(md, 'has_other')))) %>%
    bind_rows()
}) %>%
  bind_rows() %>%
  mutate(year = yr, reform_leg = 'static', .before = everything())

write_csv(rows, file.path(OUT_DIR, 'wealth_group_etr_2027.csv'))

# Diagnostics: how each group was cut
diag = imap(members, ~ tibble(
  group          = .y,
  n_records      = length(.x),
  weight_sum     = sum(md$weight[.x]),
  nw_cutoff      = min(md$net_worth[.x]),
  nw_total       = sum((md$net_worth * md$weight)[.x]),
  inc_expanded   = sum((md$inc_exp_core * md$weight)[.x]),
  inc_hs         = sum((md$inc_hs_core  * md$weight)[.x])
)) %>%
  bind_rows() %>%
  mutate(year = yr, total_weight = W, .before = everything())

write_csv(diag, file.path(OUT_DIR, 'wealth_group_etr_diag.csv'))
print(as.data.frame(diag))

# The figure's two lines: all-in taxes, capital-income corporate allocation
show = rows %>%
  filter(taxes_included == 'wealth_cit_vat', corp_convention == 'capital_income',
         income_definition %in% c('expanded', 'hs')) %>%
  select(group, income_definition, income_cutoff, n_tax_units, income_baseline,
         tax_baseline, etr_baseline)
print(as.data.frame(show))
cat('done\n')
