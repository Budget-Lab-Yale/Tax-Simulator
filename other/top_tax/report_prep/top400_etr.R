#------------------------------------------------------------------------------
# top400_etr.R
#
# Zucman-style "Top 400" ETR breakout under current law. Rebuilds the
# distribution_etrs per-record microdata for the top_tax_dials_30y_v3 vintage
# (baseline law, 2027, static leg) and composes ETR rows for the ~400 highest-
# net-worth tax units in the model — the model's Forbes-400 analog — using the
# same etr_group_sums()/compose_etr_rows() machinery as the shipped cube, so
# the numbers are definitionally identical to distribution_etrs.csv rows.
#
# Group construction: records sorted by baseline (static, un-haircut) net
# worth descending; take records until cumulative weight reaches 400 tax
# units. Ranked by WEALTH, not income — deliberately, that is the genre.
#
# Output:
#   other/top_tax/report_prep/top400_etr_2027.csv  (distribution_etrs schema)
#   other/top_tax/report_prep/top400_etr_diag.csv  (group diagnostics)
#
# Run via sbatch (never on the login node):
#   sbatch -p day -c 1 --mem=32G -t 2:00:00 \
#     --wrap "cd <repo> && module load R/4.4.1-foss-2022b && \
#             Rscript other/top_tax/report_prep/top400_etr.R"
#------------------------------------------------------------------------------

REPO    = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
STAGING = paste0('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/',
                 'Tax-Simulator/v1/top_tax_dials_30y_v3/_slurm_staging')
OUT_DIR = file.path(REPO, 'other/top_tax/report_prep')

setwd(REPO)
source('src/slurm/common.R')
invisible(reconstitute_environment(STAGING))

cat('output_root:', globals$output_root, '\n')

id          = 'pc_ordr50_cgr30'   # any scenario with kept detail; only the
baseline_id = 'baseline'          # baseline-leg columns are consumed
yr          = 2027

other_taxes = get_other_taxes(id, baseline_id)
rev_corp = read_macro_spliced(interface_root('Macro-Projections', baseline_id)) %>%
  select(year, rev_corp_level = rev_corp)

md = process_for_etrs(id, baseline_id, yr, other_taxes, rev_corp,
                      reform_leg = 'static')
cat('microdata rows:', nrow(md), '\n')

# Top-400 flag: cumulative weight over descending net worth. lag(cw) < 400
# includes the record that straddles the 400th tax unit.
ord    = order(-md$net_worth)
cw     = cumsum(md$weight[ord])
n_take = sum(dplyr::lag(cw, default = 0) < 400)
grp    = rep(NA_character_, nrow(md))
grp[ord[seq_len(n_take)]] = 'Top 400'

sums = etr_group_sums(md, grp) %>% filter(!is.na(group))

rows = ETR_INCOME_DEFS %>%
  map(~ compose_etr_rows(sums, .x, ranking = 'wealth',
                         group_dimension = 'Net worth',
                         cutoff_col = 'cutoff_nw',
                         include_other = isTRUE(attr(md, 'has_other')))) %>%
  bind_rows() %>%
  mutate(year = yr, reform_leg = 'static', .before = everything())

write_csv(rows, file.path(OUT_DIR, 'top400_etr_2027.csv'))

# Diagnostics: how literal is "top 400" in the model
take = ord[seq_len(n_take)]
diag = tibble(
  year               = yr,
  n_records          = n_take,
  weight_sum         = sum(md$weight[take]),
  nw_cutoff          = min(md$net_worth[take]),
  nw_top             = max(md$net_worth[take]),
  nw_total_group     = sum((md$net_worth * md$weight)[take]),
  billionaire_weight = sum(md$weight[md$net_worth >= 1e9])
)
write_csv(diag, file.path(OUT_DIR, 'top400_etr_diag.csv'))
print(as.data.frame(diag))
cat('done\n')
