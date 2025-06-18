#------------------
# TODO
#------------------


library(tidyverse)



# Set parameters
output_root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/'

params = list(
  house = list(
    vintage = '202506172113', 
    scenarios = c(
      '01-tcja', 
      '02-ctc', 
      '03-no_tax',
      '04-qbi', 
      '05-salt', 
      '06-other'
    )
  ), 
  senate = list(
    vintage = '202506172122', 
    scenarios = c(
      '01-tcja', 
      '02-ctc', 
      '03-no_tax',
      '04-qbi', 
      '05-salt', 
      '06-other'
    )
  ), 
  delta = list(
    vintage = '202506172125', 
    scenarios = c(
      '01-house', 
      '02-salt', 
      '03-qbi',
      '04-no_tax', 
      '05-ctc', 
      '06-other'
    )
  )
)



# Read and process data 
dist = names(params) %>% 
  map(
    .f = function(x) {
      params[[x]]$scenario %>% 
        map(
          .f = function(y) {
            output_root %>% 
              file.path(params[[x]]$vintage, y, './static/supplemental/distribution.csv') %>% 
              read_csv(show_col_types = F) %>% 
              mutate(scenario = y, .before = everything()) 
          }
        ) %>% 
        bind_rows() %>% 
        mutate(exercise = x, .before = everything())
    }
  ) %>% 
  bind_rows() %>% 
  filter(
    year            == 2026, 
    taxes_included  == 'iit_pr',   # TODO change if distributing estate
    group_dimension == 'Income', 
    group           != 'Negative income'
  ) %>% 
  mutate(top_breakout = str_sub(group, 1, 1) != 'Q', 
         pct_chg_ati  = pct_chg_ati * 100) %>% 
  select(exercise, scenario, group, top_breakout, avg, pct_chg_ati) %>% 
  pivot_longer(
    cols     = c(avg, pct_chg_ati), 
    names_to = 'variable'  
  ) %>% 
  
  # Calculate marginal impacts
  group_by(exercise, group, variable) %>% 
  mutate(
    contribution = value - lag(value, default = 0)
  ) %>% 
  ungroup()
  

# Create better scenario labels
scenario_labels = c(
  '01-tcja'   = '1) TCJA extension',
  '02-ctc'    = '2) Further CTC expansion',
  '03-no_tax' = "'3) No tax on...' provisions",
  '04-qbi'    = '4) Further QBI deduction expansion',
  '05-salt'   = '5) Higher SALT cap',
  '06-other'  = '6) Other provisions'
)


dist %>% 
  filter(
    exercise == 'house',
    variable == 'pct_chg_ati' 
  ) %>% 
  group_by(group) %>%
  mutate(total = value[scenario == '06-other']) %>%
  ungroup() %>% 
  # Create better labels for panels
  mutate(
    panel_label    = ifelse(top_breakout, "Within top quintile", "Quintiles"),
    scenario_label = scenario_labels[scenario],
    group          = factor(group, levels = unique(group[order(top_breakout, group)]))
  ) %>%
  ggplot(aes(x = group, y = contribution, fill = scenario_label)) + 
  geom_col() +
  geom_point(aes(y = total), size = 2, shape = 21, fill = "black") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) + 
  facet_wrap(~panel_label, scales = "free_x") +
  
  # Add titles and labels
  labs(
    title    = "Provision-Level Decomposition of Percent Change in After-Tax Income, 2026",
    subtitle = "House-Passed OBBBA",
    x        = "Income Group",
    y        = "Contribution to % Change in After-Tax Income (pp)",
    fill     = "Provision",
    caption  = "Source: The Budget Lab calculations"
  ) +
  
  # Improve theme and formatting
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  
  # Guide for better legend formatting
  guides(
    fill = guide_legend(
      nrow = 2, 
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0.5
    )
  )






dist %>% 
  filter(
    exercise == 'delta',
    variable == 'pct_chg_ati' 
  ) %>% 
  group_by(group) %>%
  mutate(total = value[scenario == '06-other'] - value[scenario == '01-house']) %>%
  ungroup() %>%
  filter(scenario != '01-house') %>% 
  ggplot(aes(x = group, y = contribution, fill = scenario)) + 
  geom_col() +
  geom_point(aes(y = total)) +
  geom_hline(yintercept = 0) + 
  facet_wrap(~top_breakout)



