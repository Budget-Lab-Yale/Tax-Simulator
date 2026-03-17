#------------------------------------------------------------------------
# Waterfall chart for time burden impacts of OBBBA
# Mirrors structure of obbba_he.R
#------------------------------------------------------------------------

library(tidyverse)

#-----------
# Read data
#-----------

output_root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202603161308'

# Read time burden CSVs -- each has baseline and reform columns
tcja_ext_tb = read_csv(file.path(output_root, 'tcja_ext/static/supplemental/time_burden.csv'), show_col_types = FALSE)
obbba_tb    = read_csv(file.path(output_root, 'obbba/static/supplemental/time_burden.csv'), show_col_types = FALSE)

# Extract mean_burden by quintile for each scenario
# Baseline is the same in both files
burden = bind_rows(
  tcja_ext_tb %>%
    filter(metric == 'mean_burden', str_detect(group, 'Quintile|Overall|Top')) %>%
    select(group, baseline) %>%
    mutate(scenario = 'baseline') %>%
    rename(value = baseline),
  tcja_ext_tb %>%
    filter(metric == 'mean_burden', str_detect(group, 'Quintile|Overall|Top')) %>%
    select(group, reform) %>%
    mutate(scenario = 'tcja_ext') %>%
    rename(value = reform),
  obbba_tb %>%
    filter(metric == 'mean_burden', str_detect(group, 'Quintile|Overall|Top')) %>%
    select(group, reform) %>%
    mutate(scenario = 'obbba') %>%
    rename(value = reform)
) %>%
  mutate(
    inc_quintile = case_when(
      group == 'Quintile 1' ~ 1,
      group == 'Quintile 2' ~ 2,
      group == 'Quintile 3' ~ 3,
      group == 'Quintile 4' ~ 4,
      group == 'Quintile 5' ~ 5,
      group == 'Overall'    ~ 6,
      group == 'Top 10%'    ~ 7,
      group == 'Top 5%'     ~ 8,
      group == 'Top 1%'     ~ 9,
      group == 'Top 0.1%'   ~ 10
    )
  ) %>%
  select(scenario, inc_quintile, group, value) %>%
  pivot_wider(names_from = scenario, values_from = value)


#-------
# Plots
#-------

theme_clean <- function() {
  theme_minimal() +
    theme(
      text = element_text(color = "#2c3e50"),
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 11, color = "#7f8c8d", margin = margin(b = 12)),
      plot.caption = element_text(size = 9, color = "#95a5a6", margin = margin(t = 12)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
}

# Full data for CSV (includes top quintile breakouts)
burden_full = burden %>%
  mutate(
    tcja_change = tcja_ext - baseline,
    obbba_change = obbba - tcja_ext
  )

# Build waterfall data (quintiles + overall only)
waterfall_data = burden_full %>%
  filter(inc_quintile <= 6)

waterfall_plot_data = waterfall_data %>%
  rowwise() %>%
  do({
    q = .$inc_quintile
    data.frame(
      inc_quintile = q,
      step = c("Baseline", "TCJA\nImprovement", "OBBBA\nChanges", "Final"),
      step_num = 1:4,
      value_type = c("level", "change", "change", "level"),
      display_value = c(.$baseline, .$tcja_change, .$obbba_change, .$obbba),
      y_bottom = c(0, .$baseline, .$tcja_ext, 0),
      y_top = c(.$baseline, .$tcja_ext, .$obbba, .$obbba),
      stringsAsFactors = FALSE
    )
  }) %>%
  ungroup() %>%
  mutate(
    step = factor(step, levels = c("Baseline", "TCJA\nImprovement", "OBBBA\nChanges", "Final")),
    bar_height = y_top - y_bottom,
    label = paste0(if_else(round(bar_height, 1) > 0 & value_type == 'change', '+', ''), as.character(round(bar_height, 1))),
    color_group = case_when(
      step_num == 1 ~ "baseline",
      value_type == 'change' & display_value <= 0 ~ "improvement",
      value_type == 'change' & display_value > 0  ~ "regression",
      step_num == 4 ~ "final"
    ),
    facet_label = if_else(inc_quintile <= 5, paste("Quintile", inc_quintile), "Overall")
  )

p_waterfall = waterfall_plot_data %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_rect(aes(xmin = step_num - 0.3, xmax = step_num + 0.3,
                ymin = y_bottom, ymax = y_top, fill = color_group),
            alpha = 0.8, color = "black", linewidth = 0.2) +
  geom_segment(data = . %>% filter(step_num < 4),
               aes(x = step_num + 0.3, xend = step_num + 0.7,
                   y = y_top, yend = y_top),
               color = 'black', linewidth = 0.5,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_text(aes(x = step_num,
                y = pmax(y_top, y_bottom) + 0.7,
                label = label),
            size = 3, fontface = "bold", color = "black") +
  facet_wrap(~factor(facet_label, levels = c(paste("Quintile", 1:5), "Overall")), ncol = 6) +
  scale_fill_manual(
    name = "",
    values = c(
      "baseline" = "#95a5a6",
      "improvement" = "#27ae60",
      "regression" = "#cc1616",
      "final" = "#34495e"
    )
  ) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Baseline\n(Pre-TCJA)", "TCJA\nChanges", "OBBBA\nChanges", "OBBBA")) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 7),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(
    title = "Estimated Impact of OBBBA on Time Burden of Filing a Tax Return, 2026",
    subtitle = "Average Hours Spent Filing (lower values indicate less time burden)\nBaseline -> TCJA Extension -> OBBBA",
    x = element_blank(),
    y = "Hours",
    caption = str_wrap('Source: The Budget Lab calculations. Time burden estimates based on IRS Taxpayer Burden Model methodology. Income quintiles defined by expanded income.', width = 180)
  )

ggsave('other/analysis_scripts/public/obbba_tb_waterfall.pdf', p_waterfall, width = 16, height = 5)
ggsave('other/analysis_scripts/public/obbba_tb_waterfall.png', p_waterfall, width = 16, height = 5, dpi = 300)

# Print and write CSV (full data with top quintile breakouts)
burden_full %>%
  select(group, baseline, tcja_ext, obbba, tcja_change, obbba_change) %>%
  print(n = 20)

burden_full %>%
  select(group, baseline, tcja_ext, obbba, tcja_change, obbba_change) %>%
  write.csv('other/analysis_scripts/public/obbba_tb_data.csv', row.names = FALSE)

# Share prefilled CSV
prefilled = bind_rows(
  tcja_ext_tb %>%
    filter(metric == 'share_prefilled', str_detect(group, 'Quintile|Overall|Top')) %>%
    select(group, baseline) %>%
    mutate(scenario = 'baseline') %>%
    rename(value = baseline),
  tcja_ext_tb %>%
    filter(metric == 'share_prefilled', str_detect(group, 'Quintile|Overall|Top')) %>%
    select(group, reform) %>%
    mutate(scenario = 'tcja_ext') %>%
    rename(value = reform),
  obbba_tb %>%
    filter(metric == 'share_prefilled', str_detect(group, 'Quintile|Overall|Top')) %>%
    select(group, reform) %>%
    mutate(scenario = 'obbba') %>%
    rename(value = reform)
) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    tcja_change = tcja_ext - baseline,
    obbba_change = obbba - tcja_ext
  )

prefilled %>% print(n = 20)
prefilled %>% write.csv('other/analysis_scripts/public/obbba_prefilled_data.csv', row.names = FALSE)
