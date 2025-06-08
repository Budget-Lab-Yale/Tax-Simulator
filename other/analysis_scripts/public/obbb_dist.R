library(tidyverse)

scenarios = c('01_tcja_ind', '02_ctc', '04_salt_item', '05_no_tax', '06_other', '07_cost_recovery', '08_tariffs')


output_root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202506080913'


dist = scenarios %>% 
  map(
    .f = ~ {
      output_root %>% 
        file.path(.x, '/static/supplemental/distribution.csv') %>% 
        read_csv(show_col_types = F) %>% 
        mutate(scenario = .x)
    }
  ) %>% 
  bind_rows()

plot_data = dist %>% 
  filter(
    year == 2027, 
    taxes_included == 'indirect_iit_pr_estate_cit', 
    str_sub(group, 1, 1) == 'Q' 
  ) %>% 
  select(indirect_tax_assumption, group, scenario, pct_chg_ati) %>%
  mutate(pct_chg_ati = pct_chg_ati * 100) %>% 
  arrange(indirect_tax_assumption, group, scenario) %>% 
  group_by(indirect_tax_assumption, group) %>% 
  mutate(contribution = if_else(scenario == '01_tcja_ind', pct_chg_ati, pct_chg_ati - lag(pct_chg_ati)), 
         `With tariffs` = pct_chg_ati[scenario == '08_tariffs'], 
         `Without tariffs` = pct_chg_ati[scenario == '07_cost_recovery']) %>% 
  ungroup() %>% 
  mutate(
    indirect_tax_assumption = case_when(
      indirect_tax_assumption == 'consumption' ~ 'Tariffs distributed by consumption', 
      indirect_tax_assumption == 'income'      ~ 'Tariffs distributed by income'
    ), 
    scenario = case_when(
      scenario == '01_tcja_ind'      ~ '1) TCJA extension',
      scenario == '02_ctc'           ~ '2) CTC expansion', 
      scenario == '04_salt_item'     ~ '3) QBI deduction, SALT cap, itemizer limits', 
      scenario == '05_no_tax'        ~ "4) 'No tax on...' provisions" , 
      scenario == '06_other'         ~ '5) Other provisions', 
      scenario == '07_cost_recovery' ~ '6) Cost recovery provisions', 
      scenario == '08_tariffs'       ~ '7) Tariffs'
    )
  ) %>% 
  group_by(indirect_tax_assumption, group) %>% 
  arrange(scenario) %>%
  mutate(
    
    # Calculate cumulative sum for positioning
    cumsum_contribution = cumsum(contribution),
    
    # Calculate position for label (middle of each segment)
    label_y = cumsum_contribution - contribution/2,
    
    # Only create labels for contributions above threshold
    label_text = if_else(
      abs(contribution) >= 0.15, 
      as.character(round(contribution, 1)), 
      NA_character_
    )
  ) %>%
  ungroup()



stacking_order <- c(
  '7) Tariffs',
  '2) CTC expansion',
  '3) QBI deduction, SALT cap, itemizer limits', 
  '4) \'No tax on...\' provisions',
  '5) Other provisions',
  '6) Cost recovery provisions',
  '1) TCJA extension'
)

# Define the legend order (this will be preserved)
legend_order <- c(
  '1) TCJA extension',
  '2) CTC expansion', 
  '3) QBI deduction, SALT cap, itemizer limits', 
  '4) \'No tax on...\' provisions',
  '5) Other provisions',
  '6) Cost recovery provisions',
  '7) Tariffs'
)


scenario_colors <- c(
  '1) TCJA extension' = '#4A96BB',      # Lighter blue
  '2) CTC expansion' = '#db707b',       # Lighter deep rose
  '3) QBI deduction, SALT cap, itemizer limits' = '#F3A333',  # Lighter orange
  '4) \'No tax on...\' provisions' = '#D55A47',  # Lighter red-orange
  '5) Other provisions' = '#6B3F3F',     # Lighter dark brown
  '6) Cost recovery provisions' = '#3DAD9B',  # Lighter teal
  '7) Tariffs' = '#A059BD'              # Lighter purple
)

# Prepare the plot data with reordered factors
plot_data_reordered <- plot_data %>%
  mutate(
    
    # Create factor for stacking order (this controls visual stacking)
    scenario_stacking = factor(scenario, levels = stacking_order),
    
    # Create factor for legend order (this will be used in scale_fill_manual)
    scenario_legend = factor(scenario, levels = legend_order)
  )


plot_data_reordered %>% 
  ggplot(aes(x = group, y = contribution, fill = scenario_stacking)) + 
  geom_col() +
  geom_hline(yintercept = 0) + 
  geom_text(
    aes(label = label_text), size = 3, position = position_stack(vjust = 0.5), colour = "white"
  ) +  
  geom_point(aes(y = `With tariffs`), size = 9, show.legend = F) + 
  geom_point(aes(y = `With tariffs`), size = 9, shape = 1, colour = 'white', show.legend = F) + 
  geom_text(
    aes(y = `With tariffs`, 
        label = paste0(round(`With tariffs`, 1))),
    hjust = 0.5,   # Center horizontally
    size = 3,
    color = "white",
  ) +
  facet_wrap(~indirect_tax_assumption) +
  theme_bw() +
  scale_fill_manual(
    name = "Provision",
    values = scenario_colors,
    breaks = legend_order,
    labels = legend_order 
  ) + 
  labs(
    y = "Contribution",
    x = "Income group",
    fill = "Provision", 
    subtitle = "● = Net effect with tariffs"
  ) + 
  ggtitle('Percentage point change in after-tax income under OBBBA plus tariffs, 2027')



# Comparison with Ernie's tariff distribution
plot_data %>% 
  filter(scenario == '7) Tariffs') %>% 
  select(source = indirect_tax_assumption, group, value = contribution) %>% 
  mutate(source = if_else(source == 'Tariffs distributed by consumption', 
                          'Tax-Simulator results, by consumption', 
                          'Tax-Simulator results, by income')) %>% 
  bind_rows(
    tibble(
      group = unique(plot_data$group), 
      value = c(-2.5, -2, -1.7, -1.5, -1.2)
    ) %>% 
    mutate(
      source = 'Current Budget Lab results (by consumption)'
    )
  ) %>% 
  ggplot(aes(x = group, y = value, label = round(value, 1))) + 
  geom_col(position = position_dodge(width = 0.9)) + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, colour = 'white') + 
  theme_bw() +
  facet_wrap(~source) + 
  labs(
    fill = 'Method', 
    x = 'Income group', 
    y = 'Percentage points'
  ) + 
  ggtitle(
    'Comparison of tariff distribution approachs, 2027 (for tariffs as of June 1)', 
    subtitle = 'Percentage point change in after-tax income'
  )
  


          