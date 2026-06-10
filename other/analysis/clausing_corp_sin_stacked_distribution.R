################################################################################
# Stacked distribution analysis for the Clausing-Sarin package.
#
# Mirrors the layout of booker_kypa_stacked_distribution.R, but stacks THREE
# blocks of contribution to the change in after-tax income (pp), by income
# group, as the AVERAGE ANNUAL EFFECT over the 10-year window (2030-2039):
# pct-of-ATI pieces are computed per year (income groups defined within-year)
# and averaged; dollar averages are deflated to 2026 dollars (chained CPI)
# before averaging. The window average is used instead of a single year
# because several provisions ramp (carryover basis starts at zero in 2030 —
# its carryover stock enters heir realizations with a lag — and estate and
# carbon trend in opposite directions):
#
#   1. Individual and estate provisions: cumulative scenarios 01-07 (07 is
#      the on-model estate layer, $5M@2030/45%, distributed to heirs via the
#      rank-matching allocator), plus three off-model income measures
#      (carried interest repeal, QSBS reform, OZ repeal) appended from the
#      off-model distribution file
#   2. Corporate tax              (scenario 08_corporate, off-model stream)
#   3. Excise / "sin" taxes       (off-model: carbon, alcohol, gambling,
#                                  guns, tobacco)
#
# The chart shows the three blocks; the underlying data file breaks every
# component out separately in stacking order (01-07 on-model individual and
# estate, 08-10 off-model individual, 11 corporate, 12-16 for the five
# excises).
#
# The only summary metric printed beneath each group is the AVERAGE tax change
# (total of all three blocks) -- the share-of-net-change and winner/loser
# shares used in single-tax charts lose meaning once corporate and excise
# burdens (imputed at the group level, not per tax unit) are mixed in.
#
# Note on additivity: individual + estate + corporate come from the model's
# 'iit_pr_death_cit_vat' distribution variant (the death-inclusive variant in
# which estate tax on heirs and the off-model corporate burden, liab_corp,
# enter liab_reform). The excise block is appended from the off-model excise
# distribution, whose per-group pct_chg_ati / avg are computed against the
# same income-group definitions, so the pieces add in pp space.
################################################################################

library(tidyverse)
library(scales)

# --- Configuration -----------------------------------------------------------
vintage     = 'clausing_estate'
out_root    = file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1', vintage)
years_avg   = 2030:2039
period_lab  = 'avg_2030_2039'
repo_root   = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
excise_file = file.path(repo_root, 'other/analysis_scripts/public',
                        'clausing_excise_distribution_avg_2030_2039.csv')

# Chained CPI (base 2026 = 1) for expressing dollar averages in 2026 dollars.
# Default Macro-Projections vintage, same as the model run used.
ccpiu = readr::read_csv(file.path('/nfs/roberts/project/pi_nrs36/shared/model_data',
                                  'Macro-Projections/v3/2026022522/baseline/projections.csv'),
                        show_col_types = FALSE) %>%
  filter(year %in% years_avg) %>%
  select(year, ccpiu)

# Tax-inclusion variant in which the estate and corporate burdens appear
TAX_VARIANT = 'iit_pr_death_cit_vat'

# Individual and estate provisions, in cumulative stacking order, then the
# corporate layer
ind_scenarios = c('01_clinton_rates', '02_restore_bottom_rates', '03_199a',
                  '04_carryover_basis', '05_pref_rates', '06_niit_reform',
                  '07_estate')
corp_scenario = '08_corporate'
model_order   = c(ind_scenarios, corp_scenario)

ind_labels = c(
  '01_clinton_rates'        = 'Clinton-era rates',
  '02_restore_bottom_rates' = 'Restore bottom rates',
  '03_199a'                 = 'Repeal 199A (QBI)',
  '04_carryover_basis'      = 'Carryover basis',
  '05_pref_rates'           = 'Preferential rates',
  '06_niit_reform'          = 'NIIT reform',
  '07_estate'               = 'Estate tax ($5M / 45%)'
)

# Off-model individual income tax measures from the excise distribution file,
# in stacking order (appended to the individual block after scenarios 01-07)
inc_measures = c('carried_interest', 'qsbs', 'oz')
inc_labels   = c(carried_interest = 'Carried interest repeal',
                 qsbs             = 'QSBS reform',
                 oz               = 'OZ repeal')

# Excise measures, in stacking order (largest 2030 revenue first)
sin_measures = c('carbon', 'alcohol', 'gambling', 'guns', 'tobacco')
sin_labels   = c(carbon = 'Carbon', alcohol = 'Alcohol', gambling = 'Gambling',
                 guns = 'Guns', tobacco = 'Tobacco')

# Three-block grouping for the chart
BLOCK_IND  = 'Individual and estate'
BLOCK_CORP = 'Corporate tax'
BLOCK_SIN  = 'Excise / sin taxes'
block_levels = c(BLOCK_IND, BLOCK_CORP, BLOCK_SIN)
block_colors = c(
  setNames('#2166AC', BLOCK_IND),
  setNames('#B2182B', BLOCK_CORP),
  setNames('#F1A340', BLOCK_SIN)
)

# --- Read model distribution tables ------------------------------------------
read_dist = function(scenario) {
  path = file.path(out_root, scenario, 'static/supplemental/distribution.csv')
  read_csv(path, show_col_types = FALSE) %>%
    filter(year %in% years_avg, taxes_included == TAX_VARIANT,
           group_dimension == 'Income') %>%
    transmute(scenario = scenario, year, group, pct_chg_ati, avg)
}

model_dist = map_dfr(model_order, read_dist)

# Marginal (incremental) contribution of each scenario, within each group and
# year, in cumulative order: piece_k = cumulative_k - cumulative_{k-1}
# (k=1 -> vs baseline). Then average across years: pct directly, dollars in
# 2026 dollars
model_marg = model_dist %>%
  mutate(scenario = factor(scenario, levels = model_order)) %>%
  arrange(group, year, scenario) %>%
  group_by(group, year) %>%
  mutate(
    pct_piece = pct_chg_ati - lag(pct_chg_ati, default = 0),
    avg_piece = avg         - lag(avg,         default = 0)
  ) %>%
  ungroup() %>%
  left_join(ccpiu, by = 'year') %>%
  group_by(scenario, group) %>%
  summarise(
    pct_piece = mean(pct_piece),
    avg_piece = mean(avg_piece / ccpiu),
    .groups = 'drop'
  ) %>%
  mutate(
    scenario    = as.character(scenario),
    # 01-07 individual and estate; corporate moves to 11 so the off-model
    # individual measures (8-10) stack inside the individual block
    piece_order = if_else(scenario == corp_scenario, 11L,
                          match(scenario, model_order)),
    piece_id    = scenario,
    piece_label = if_else(scenario == corp_scenario, 'Corporate (off-model)',
                          ind_labels[scenario]),
    block       = if_else(scenario == corp_scenario, BLOCK_CORP, BLOCK_IND)
  ) %>%
  select(group, piece_order, piece_id, piece_label, block, pct_piece, avg_piece)

# --- Read off-model income tax and excise distributions ----------------------
# Already 10-year averages with dollar metrics in 2026$ (produced by
# clausing_excise_distribution.R)
offmodel = read_csv(excise_file, show_col_types = FALSE) %>%
  filter(group_dimension == 'Income')

inc_offmodel = offmodel %>%
  filter(measure %in% inc_measures) %>%
  transmute(
    group,
    piece_order = 7 + match(measure, inc_measures),     # 8..10
    piece_id    = paste0(formatC(piece_order, width = 2, flag = '0'), '_', measure),
    piece_label = inc_labels[measure],
    block       = BLOCK_IND,
    pct_piece   = pct_chg_ati,                           # already a standalone burden
    avg_piece   = avg
  )

excise = offmodel %>%
  filter(measure %in% sin_measures) %>%
  transmute(
    group,
    piece_order = 11 + match(measure, sin_measures),    # 12..16
    piece_id    = paste0(formatC(piece_order, width = 2, flag = '0'), '_', measure),
    piece_label = sin_labels[measure],
    block       = BLOCK_SIN,
    pct_piece   = pct_chg_ati,                           # already a standalone burden
    avg_piece   = avg
  )

# --- Combined broken-out data file -------------------------------------------
income_groups = c('Quintile 1', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5',
                  'Top 10%', 'Top 5%', 'Top 1%', 'Top 0.1%')

pieces = bind_rows(model_marg, inc_offmodel, excise) %>%
  filter(group %in% income_groups) %>%
  mutate(
    group        = factor(group, levels = income_groups),
    block        = factor(block, levels = block_levels),
    pct_chg_ati_pp = pct_piece * 100
  ) %>%
  arrange(group, piece_order) %>%
  select(group, piece_order, piece_id, piece_label, block,
         pct_chg_ati = pct_piece, pct_chg_ati_pp, avg = avg_piece)

data_out = file.path(out_root, paste0('clausing_corp_sin_stacked_data_', period_lab, '.csv'))
write_csv(pieces, data_out)
cat('\nBroken-out stacking data written to:', data_out, '\n\n')

cat('===== Broken-out pieces (pct chg ATI, pp) =====\n')
pieces %>%
  select(group, piece_order, piece_label, block, pct_chg_ati_pp, avg) %>%
  mutate(pct_chg_ati_pp = round(pct_chg_ati_pp, 3), avg = round(avg)) %>%
  print(n = Inf, width = Inf)

# --- Collapse to three blocks for the chart ----------------------------------
block_data = pieces %>%
  group_by(group, block) %>%
  summarise(pct_chg = sum(pct_chg_ati_pp), .groups = 'drop')

# Net change (sum of all blocks) and total average tax change per group
group_totals = pieces %>%
  group_by(group) %>%
  summarise(net = sum(pct_chg_ati_pp), avg_total = sum(avg), .groups = 'drop')

cat('\n===== Block contributions and net (pp) =====\n')
block_data %>%
  pivot_wider(names_from = block, values_from = pct_chg) %>%
  left_join(group_totals, by = 'group') %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# --- Chart layout ------------------------------------------------------------
income_xpos = c(1, 2, 3, 4, 5, 6.5, 7.5, 8.5, 9.5)
names(income_xpos) = income_groups
income_xlabs = c('Q1\n(Bottom)', 'Q2', 'Q3', 'Q4', 'Q5',
                 'Top\n10%', 'Top\n5%', 'Top\n1%', 'Top\n0.1%')
names(income_xlabs) = income_groups

dist_footnote = str_wrap(paste0(
  "Source: The Budget Lab calculations. Stacked contribution to the change in ",
  "after-tax income: average annual effect over 2030-2039, with dollar averages ",
  "in 2026 dollars (chained CPI); income groups defined within each year. ",
  "Individual, estate, and corporate components from ",
  "the Tax-Simulator distribution (estate tax borne by heirs via rank-matched ",
  "inheritances; corporate burden allocated 80% capital / 20% labor); ",
  "carried interest, QSBS, and OZ components imputed off-model from capital gains; ",
  "excise components imputed off-model from consumption. Avg. tax change is the ",
  "total across all three blocks. Universe is nondependent tax units including nonfilers."),
  width = 130)

plot_data = block_data %>%
  mutate(
    block = factor(block, levels = block_levels),
    group = factor(group, levels = income_groups),
    xpos  = income_xpos[as.character(group)]
  )

net_dots = group_totals %>%
  mutate(
    group = factor(group, levels = income_groups),
    xpos  = income_xpos[as.character(group)],
    net_label = paste0(ifelse(net >= 0, '+', ''), formatC(round(net, 1), format = 'f', digits = 1))
  )

# Average tax change annotation (total of all three blocks)
avg_ann = group_totals %>%
  mutate(
    xpos = income_xpos[as.character(group)],
    avg_label = {
      sign_chr = if_else(avg_total > 0, '+', if_else(avg_total < 0, '-', ''))
      amt = abs(avg_total)
      num_str = case_when(
        amt == 0      ~ '$0',
        amt < 1000    ~ paste0('$', formatC(amt, format = 'f', digits = 0)),
        amt < 1e6     ~ paste0('$', formatC(amt / 1000, format = 'f', digits = 1), 'K'),
        TRUE          ~ paste0('$', formatC(amt / 1e6, format = 'f', digits = 1), 'M')
      )
      paste0(sign_chr, num_str)
    }
  )

# y-range and fixed-inch layout below the x-axis (one annotation row only)
plot_height = 9
bar_totals = plot_data %>%
  group_by(group) %>%
  summarise(pos = sum(pct_chg[pct_chg > 0]), neg = sum(pct_chg[pct_chg < 0]), .groups = 'drop')
y_range = c(min(bar_totals$neg), max(bar_totals$pos))
y_lower = floor(y_range[1] * 2) / 2
y_upper = y_range[2] + diff(y_range) * 0.20
vis_range = y_upper - y_lower

plot_area_inches = plot_height - 2.5
dpi_du = vis_range / plot_area_inches      # data units per inch

xlab_gap     = dpi_du * 1.35               # gap for the bracketed x-axis labels
row_step     = dpi_du * 0.25
footnote_gap = dpi_du * 0.45

avg_y  = y_lower - xlab_gap
foot_y = avg_y - row_step - footnote_gap
label_x = min(income_xpos) - 0.5

brackets = list(
  list(x1 = 1,   x2 = 5,   label = 'Quintiles'),
  list(x1 = 6.5, x2 = 9.5, label = 'Top Decile Breakout')
)

p = ggplot(plot_data, aes(x = xpos, y = pct_chg, fill = block)) +
  geom_col(position = position_stack(), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 5.75, linetype = 'dashed', color = 'grey60', linewidth = 0.3) +
  geom_point(data = net_dots, aes(x = xpos, y = net),
             inherit.aes = FALSE, shape = 21, size = 10,
             fill = 'white', color = 'black', stroke = 0.8) +
  geom_text(data = net_dots, aes(x = xpos, y = net, label = net_label),
            inherit.aes = FALSE, size = 2.8, fontface = 'bold') +
  # Single summary row: average tax change (total of all blocks)
  annotate('text', x = label_x, y = avg_y, label = 'Avg. tax change:',
           fontface = 'bold', size = 3, hjust = 1, color = 'grey30') +
  geom_text(data = avg_ann, aes(x = xpos, y = avg_y, label = avg_label),
            inherit.aes = FALSE, size = 3, color = 'grey30') +
  annotate('text', x = label_x, y = foot_y, label = dist_footnote,
           hjust = 0, vjust = 1, size = 2.8, color = 'grey40', lineheight = 1.1)

# Top brackets
bracket_y = y_range[2] + diff(y_range) * 0.10
label_y_b = y_range[2] + diff(y_range) * 0.16
for (b in brackets) {
  p = p +
    annotate('segment', x = b$x1, xend = b$x2, y = bracket_y, yend = bracket_y, color = 'grey40') +
    annotate('segment', x = b$x1, xend = b$x1, y = bracket_y, yend = bracket_y - diff(y_range) * 0.02, color = 'grey40') +
    annotate('segment', x = b$x2, xend = b$x2, y = bracket_y, yend = bracket_y - diff(y_range) * 0.02, color = 'grey40') +
    annotate('text', x = (b$x1 + b$x2) / 2, y = label_y_b, label = b$label,
             fontface = 'bold', size = 4, color = 'grey30')
}

total_below = xlab_gap + row_step + footnote_gap + dpi_du * 0.45
bottom_margin_pt = total_below / vis_range * plot_area_inches * 72

p = p +
  scale_x_continuous(breaks = income_xpos, labels = income_xlabs) +
  scale_fill_manual(values = block_colors, breaks = block_levels) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  coord_cartesian(ylim = c(y_lower, y_upper), clip = 'off') +
  labs(
    title = 'Contribution to Change in After-Tax Income by Income Group (2030-2039 average)',
    subtitle = 'Clausing-Sarin package: individual and estate, corporate, and excise components; dollar averages in 2026 dollars',
    x = NULL, y = 'Change in After-Tax Income (pp)', fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(size = 13, face = 'bold'),
    legend.position    = 'top',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title         = element_text(face = 'bold', size = 14),
    plot.subtitle      = element_text(size = 11, color = 'grey30'),
    plot.margin        = margin(30, 10, bottom_margin_pt, 40)
  )

chart_out = file.path(out_root, paste0('clausing_corp_sin_stacked_distribution_', period_lab, '.png'))
ggsave(chart_out, plot = p, width = 11, height = plot_height, dpi = 200, bg = 'white')
cat('\nChart saved to:', chart_out, '\n')
