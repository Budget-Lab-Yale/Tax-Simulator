#---------------------------------------------------------------------------
# Script to produce charts for TBL tweets about OBBBA developments on 06-28
#---------------------------------------------------------------------------

library(tidyverse)

#----------------
# Set parameters
#----------------

output_root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202506281214'

scenarios = c(
  'tcja',
  'house', 
  'senate_initial', 
  'senate_update'
)

#-----------------------
# Read and process data 
#-----------------------

dist = scenarios %>% 
  map(
    .f = function(s) {
      output_root %>% 
        file.path(s, '/static/supplemental/distribution.csv') %>% 
        read_csv(show_col_types = F) %>% 
        mutate(scenario = s, .before = everything()) 
    }
  ) %>% 
  bind_rows() %>% 
  
  # Clean up data
  filter(
    taxes_included  == 'iit_pr_estate_cit_vat', 
    group_dimension == 'Income',
    group           != 'Negative income', 
    group           != 'Overall', 
  ) %>%
  mutate(
    top_breakout     = str_sub(group, 1, 1) != 'Q' & group != 'Overall', 
    pct_chg_ati      = pct_chg_ati * 100,
    share_net_change = share_net_change * 100
    
  ) %>% 
  select(year, scenario, group, top_breakout, income_cutoff, pct_chg_ati, share_net_change) %>%
  pivot_longer(
    cols     = c(pct_chg_ati, share_net_change), 
    names_to = 'metric'
  )

#--------------
# Create plots
#--------------

# Define proper order for groups
group_order = c(
  "Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5",
  "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"
)

# Define scenario order and labels
scenario_order = c('tcja', 'house', 'senate_initial', 'senate_update')
scenario_labels = c(
  'tcja' = 'TCJA Extension',
  'house' = 'House-Passed Bill', 
  'senate_initial' = 'Senate Bill (Initial)',
  'senate_update' = 'Senate Bill (Updated)'
)

# Define better color scheme (using the new labels as keys)
scenario_colors = c(
  'TCJA Extension' = '#2E86AB',           # Blue
  'House-Passed Bill' = '#A23B72',       # Magenta  
  'Senate Bill (Initial)' = '#F18F01',   # Orange
  'Senate Bill (Updated)' = '#C73E1D'    # Red
)

# Prepare data for the net effect plot
plot_data = dist %>%
  filter(metric == 'pct_chg_ati') %>%
  mutate(
    # Factorize 'group' for correct ordering on the x-axis
    group = factor(group, levels = group_order),
    
    # Factorize 'scenario' for correct ordering and labeling
    scenario = factor(scenario, levels = scenario_order, labels = scenario_labels),
    
    # Add panel_label for faceting, consistent with the prior chart
    panel_label = ifelse(str_sub(group, 1, 1) != 'Q', "Within Top Quintile", "Across Quintiles")
  )

# Create the plot
p = plot_data %>% 
  ggplot(aes(x = group, y = value, fill = scenario)) +
  geom_col(width = 0.8, position = 'dodge') + 
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  facet_wrap(~panel_label, scales = "free_x") +
  
  # Add data labels on top of each bar
  geom_text(
    aes(label = round(value, 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,  # Position labels above bars
    size = 2.5, 
    colour = 'black'
  ) +
  
  # Apply custom colors and labels
  scale_fill_manual(
    values = scenario_colors,
    name = ""
  ) +
  
  # Clean up
  labs(
    title    = "Estimated Distributional Impact of Major Tax Changes Across Versions of Reconciliation Bill, 2026",
    subtitle = "Percent Change in After-Tax Income Relative to Current Law (TCJA Expiration)",
    x        = "Income Group",
    y        = "Percentage Points",
    caption  = str_wrap("Source: The Budget Lab calculations. Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. 'Income' is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, and inheritances. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. Income group cutoffs are approximately: Quintile 2 ($21,170), Quintile 3 ($43,845), Quintile 4 ($75,995), Quintile 5 ($137,565), Top 10% ($213,480), Top 5% ($319,275), Top 1% ($845,270), and Top 0.1% ($3,973,435).
                        Includes changes to ordinary rates, the standard deduction, itemized deductions, personal exemptions, the QBI deduction, the CTC, the CDCTC, the AMT, no tax on tips/OT/car loan interest, the senior deduction, estate tax, bonus depreciation, and R&D expensing.", width = 160)
  ) + 
  theme_minimal() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title         = element_text(size = 12),
    plot.title         = element_text(size = 14),
    plot.subtitle      = element_text(size = 12), 
    plot.caption       = element_text(size = 9, color = "gray50", hjust = 0),
    legend.title       = element_text(size = 12),
    legend.text        = element_text(size = 10),
    legend.position    = "top",
    legend.box         = "horizontal",
    strip.text         = element_text(size = 12, vjust = -1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) 

p
