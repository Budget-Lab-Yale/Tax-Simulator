################################################################################
# build_decomp_panel.R
#
# Reads kg_dynamics_revenue_decomp.csv from six reform scenarios in a vintage
# and builds a 2x3 panel chart for the memo (figures/decomp_panel.pdf).
#
# Panels (column-major):
#   row 1: rate_up_2pp                | rate_up_2pp_carryover
#   row 2: carryover                  | rate_up_2pp_deemed
#   row 3: deemed                     | delayed
#
# Assumes kg_dyn_revenue_decomp.R has already been run on each scenario, so
# .../<scenario>/conventional/supplemental/kg_dynamics_revenue_decomp.csv exists.
#
# Usage:
#   Rscript other/kg_model_tests/memo/build_decomp_panel.R <vintage>
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

args     = commandArgs(trailingOnly = TRUE)
vintage  = if (length(args) >= 1) args[1] else
  stop('Usage: Rscript build_decomp_panel.R <vintage>')

out_root = file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data',
                     'Tax-Simulator/v1', vintage)

# Panel order (left-to-right, top-to-bottom in 2 cols x 3 rows).
scenarios = tribble(
  ~scenario,                 ~label,
  'rate_up_2pp',             '+2pp rate hike',
  'rate_up_2pp_carryover',   '+2pp x carryover',
  'carryover',               'Carryover at death',
  'rate_up_2pp_deemed',      '+2pp x deemed',
  'deemed',                  'Deemed at death',
  'delayed',                 '+2pp delayed (eff. 2027)'
) %>%
  mutate(label = factor(label, levels = label))

read_one = function(scn) {
  p = file.path(out_root, scn, 'conventional/supplemental',
                'kg_dynamics_revenue_decomp.csv')
  if (!file.exists(p)) {
    stop('Missing decomp CSV for scenario ', scn, ': ', p,
         '\nRun: Rscript other/analysis_scripts/kg_dyn_revenue_decomp.R ',
         vintage, ' ', scn)
  }
  read_csv(p, show_col_types = FALSE) %>% mutate(scenario = scn)
}

dat = bind_rows(lapply(scenarios$scenario, read_one)) %>%
  inner_join(scenarios, by = 'scenario')

plot_df = dat %>%
  select(year, label, mechanical, ch_lockin_unlock, ch_stock, ch_deemed) %>%
  pivot_longer(c(mechanical, ch_lockin_unlock, ch_stock, ch_deemed),
               names_to = 'channel', values_to = 'rev') %>%
  mutate(channel = factor(channel,
    levels = c('mechanical', 'ch_lockin_unlock', 'ch_stock', 'ch_deemed'),
    labels = c('Mechanical rate',
               'Realization response',
               'Policy-induced stock',
               'Deemed at death')))

engine_df = dat %>% select(year, label, conv_rev)

palette = c('Mechanical rate'        = '#1f77b4',
            'Realization response'   = '#d62728',
            'Policy-induced stock'   = '#2ca02c',
            'Deemed at death'        = '#9467bd')

p = ggplot(plot_df, aes(x = year, y = rev, fill = channel)) +
  geom_col(position = 'stack', width = 0.8) +
  geom_line(data = engine_df, aes(x = year, y = conv_rev),
            inherit.aes = FALSE, linewidth = 0.6, color = 'black') +
  geom_point(data = engine_df, aes(x = year, y = conv_rev),
             inherit.aes = FALSE, size = 0.9, color = 'black') +
  facet_wrap(~ label, ncol = 2, scales = 'free_y') +
  scale_fill_manual(values = palette, name = NULL) +
  scale_y_continuous(labels = scales::dollar_format(suffix = 'B')) +
  scale_x_continuous(breaks = c(2026, 2030, 2035)) +
  labs(x = NULL, y = 'Revenue change vs. baseline ($B)') +
  theme_minimal(base_size = 10) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 9),
        strip.text = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 8),
        panel.spacing.x = unit(1.5, 'lines'),
        panel.spacing.y = unit(1.0, 'lines'),
        plot.margin = margin(8, 12, 8, 8))

memo_dir = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/kg_model_tests/memo'
out_path = file.path(memo_dir, 'figures/decomp_panel.pdf')

ggsave(out_path, p, width = 8.0, height = 9.0, units = 'in')
cat('Wrote ', out_path, '\n', sep = '')
