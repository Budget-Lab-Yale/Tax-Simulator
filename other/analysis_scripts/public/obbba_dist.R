#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Analysis script for calculations in "Standalone Distributional Effects of Major Tax Provisions in the Reconciliation Bill: Comparing House and Senate Versions"
# https://budgetlab.yale.edu/research/standalone-distributional-effects-major-tax-provisions-reconciliation-bill-comparing-house-and 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------


library(tidyverse)

#----------------
# Set parameters
#----------------

output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Simulator/v1'

params = list(
  standalone = list(
    vintage = '202506191523', 
    scenarios = c(
      'tcja',
      'amt-house',
      'amt-senate',
      'auto-house',
      'auto-senate',
      'brackets-house',
      'brackets-senate',
      'cdctc-house',
      'cdctc-senate',
      'char-house',
      'char-senate',
      'ctc-house',
      'ctc-senate',
      'item-house',
      'item-senate',
      'ot-house',
      'ot-senate',
      'qbi-house',
      'qbi-senate',
      'salt-house',
      'salt-senate',
      'senior-house',
      'senior-senate',
      'std-house',
      'std-senate',
      'tips-house',
      'tips-senate',
      'estate-house', 
      'estate-senate', 
      'depreciation-house', 
      'depreciation-senate'
    )
  )
)


#-----------------------
# Read and process data 
#-----------------------

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
  
  # Clean up data
  filter(
    taxes_included  == 'iit_pr_estate_cit_vat', 
    (group_dimension == 'Income' | group_dimension == 'Overall'), 
    group           != 'Negative income'
  ) %>%
  mutate(
    top_breakout = str_sub(group, 1, 1) != 'Q' & group != 'Overall', 
    pct_chg_ati = pct_chg_ati * 100, 
    version = case_when(
      scenario == 'tcja' ~ NA,
      T ~ if_else(str_sub(scenario, start = -6) == '-house', 'House', 'Senate')
    ),
    provision = str_extract(scenario, "^[^-]*")
  ) %>% 
  select(exercise, provision, version, group, top_breakout, income_cutoff, n_tax_units, avg, pct_chg_ati) %>% 
  pivot_longer(
    cols     = c(avg, pct_chg_ati), 
    names_to = 'variable'  
  ) %>% 
  
  # Calculate marginal impacts against current policy
  group_by(exercise, group, variable) %>% 
  mutate(value = if_else(provision == 'tcja', value, value - value[provision == 'tcja'])) %>% 
  ungroup() %>% 
  pivot_wider(names_from = variable) %>% 
  
  # Express average tax change as change in income
  mutate(
    avg_formatted = case_when(
      abs(avg) <= 5 & abs(pct_chg_ati) < 0.005 ~ "$0",
      abs(avg) < 10 ~ if_else(avg <= 0, '<$10', '>-$10'),
      abs(avg) < 100 ~ paste0(if_else(avg <= 0, '$', '-$'), as.character(round(abs(avg) / 10) * 10)),
      abs(avg) < 1000 ~ paste0(if_else(avg <= 0, '$', '-$'), as.character(round(abs(avg) / 25) * 25)),
      abs(avg) <= 10000 ~ paste0(if_else(avg <= 0, '$', '-$'), as.character(round(abs(avg) / 100) * 100)),
      T ~ paste0(if_else(avg <= 0, '$', '-$'), round(abs(avg) / 1000), 'K')
    ), 
    
    # Round tiny ATI items to 0
    pct_chg_ati = if_else(abs(pct_chg_ati) < 0.005, 0, pct_chg_ati)
  )


#-------
# Plots
#-------


# Add this function to create titles
get_provision_title = function(prov) {
  title_map = c(
    'depreciation' = 'Figure 2: Cost Recovery Policy',
    'salt'         = 'Figure 3: State and Local Tax Deduction', 
    'std'          = 'Figure 4: Standard Deduction',
    'ctc'          = 'Figure 5: Child Tax Credit',
    'senior'       = 'Figure 6: Senior Deduction',
    'estate'       = 'Figure 7: Estate Tax',
    'ot'           = 'Figure 8: No Tax on Overtime',
    'brackets'     = 'Figure 9: Tax Brackets',
    'qbi'          = 'Figure 10: Qualified Business Income Deduction',
    'auto'         = 'Figure 11: No Tax on Car Loan Interest',
    'tips'         = 'Figure 12: No Tax on Tips',
    'amt'          = 'Figure 13: Alternative Minimum Tax',
    'item'         = 'Figure 14: Limit on Itemized Deductions',
    'cdctc'        = 'Figure 15: Child and Dependent Care Tax Credit',
    'char'         = 'Figure 16: Itemized Charitable Deduction Floor'
  )
  return(title_map[prov])
}

build_net_effect_plot = function(prov, save_path = NULL) {
  
  # Define proper order for groups
  group_order = c(
    "Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5",
    "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"
  )
  
  # Prepare data for the net effect plot
  plot_data = dist %>%
    filter(
      exercise  == 'standalone',
      group     != 'Overall',
      provision == prov
    ) %>%
    mutate(
      
      # Factorize 'group' for correct ordering on the x-axis
      group = factor(group, levels = group_order),
      
      # Add panel_label for faceting, consistent with the prior chart
      panel_label = ifelse(str_sub(group, 1, 1) != 'Q', "Within Top Quintile", "Across Quintiles")
    )
  
  # Calculate dynamic y position for labels
  y_min = min(plot_data$pct_chg_ati)
  y_max = max(plot_data$pct_chg_ati)
  y_range = y_max - y_min
  label_y = min(y_min, 0) + y_range * 0.03  # 3% of range above the lower bound
  
  # Create the plot
  p = plot_data %>% 
    ggplot(aes(x = group, y = pct_chg_ati, fill = version)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.8) + 
    geom_hline(yintercept = 0, color = "black", linewidth = 1) +
    facet_wrap(~panel_label, scales = "free_x") +
    
    # Add data labels on top of each bar
    geom_text(
      aes(
        label = avg_formatted,
        y = y_range * 0.015,
        vjust = 0
      ),
      position = position_dodge(width = 0.8),
      size     = 2.5, 
      colour   = 'black'
    ) +

    # Clean up
    scale_fill_manual(
      values = c(
        'House'  = '#b3c2ff',
        'Senate' = '#fcdd88'
      ),
    ) +
    labs(
      title    = get_provision_title(prov),
      subtitle = "Percent Change in After-Tax Income Relative to Individual TCJA Extension, 2026",
      x        = "Income Group",
      y        = "Percentage Points",
      fill     = element_blank(),
      caption  = str_wrap("Source: The Budget Lab calculations. Note: Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. 'Income' is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, and inheritances. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. Income group cutoffs are approximately: Quintile 2 ($21,170), Quintile 3 ($43,845), Quintile 4 ($75,995), Quintile 5 ($137,565), Top 10% ($213,480), Top 5% ($319,275), Top 1% ($845,270), and Top 0.1% ($3,973,435).", width = 120)
    ) + 
    theme_minimal() +
    theme(
      axis.text.y        = element_text(size = 12),
      axis.title         = element_text(size = 12),
      plot.title         = element_text(size = 14),
      plot.subtitle      = element_text(size = 12), 
      plot.caption       = element_text(size = 9, color = "gray50", hjust = 0),
      legend.title       = element_text(size = 12),
      legend.text        = element_text(size = 9),
      legend.position    = "top",
      legend.box         = "horizontal",
      strip.text         = element_text(size = 12, vjust = -1), # Restored original vjust
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    ) 
  
  # Save plot if path is provided
  if (!is.null(save_path)) {
    ggsave(filename = file.path(save_path, paste0(prov, '.png')), 
           p, 
           width = 9, height = 5, dpi = 300)
  } 
  
  return(p)
}


params$standalone$scenarios %>%
  str_extract("^[^-]*") %>%
  unique() %>% 
  walk(.f = ~ build_net_effect_plot(.x, 'C:/Users/jar335/Documents/Interfaces') )


#------------
# TCJA chart
#------------

# Define proper order for groups
group_order = c(
  "Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5",
  "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"
)

# Prepare data for the net effect plot
plot_data = dist %>%
  filter(
    exercise  == 'standalone',
    group     != 'Overall',
    provision == 'tcja'
  ) %>%
  mutate(
    
    # Factorize 'group' for correct ordering on the x-axis
    group = factor(group, levels = group_order),
    
    # Add panel_label for faceting, consistent with the prior chart
    panel_label = ifelse(str_sub(group, 1, 1) != 'Q', "Within Top Quintile", "Across Quintiles")
  )

# Calculate dynamic y position for labels
y_min = min(plot_data$pct_chg_ati)
y_max = max(plot_data$pct_chg_ati)
y_range = y_max - y_min
label_y = min(y_min, 0) + y_range * 0.03  # 3% of range above the lower bound

# Create the plot
p = plot_data %>% 
  ggplot(aes(x = group, y = pct_chg_ati)) +
  geom_col(width = 0.8) + 
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  facet_wrap(~panel_label, scales = "free_x") +
  
  # Add data labels on top of each bar
  geom_text(
    aes(
      label = avg_formatted,
      y = y_range * 0.015,
      vjust = 0
    ),
    position = position_dodge(width = 0.8),
    size     = 2.5, 
    colour   = 'white'
  ) +
  
  # Clean up
  labs(
    title    = "Figure 1: Extension of TCJA's Individual Income Tax Provisions",
    subtitle = "Percent Change in After-Tax Income Relative to Current Law, 2026",
    x        = "Income Group",
    y        = "Percentage Points",
    fill     = element_blank(),
    caption  = str_wrap("Source: The Budget Lab calculations. Note: Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. 'Income' is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, and inheritances. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. Income group cutoffs are approximately: Quintile 2 ($21,170), Quintile 3 ($43,845), Quintile 4 ($75,995), Quintile 5 ($137,565), Top 10% ($213,480), Top 5% ($319,275), Top 1% ($845,270), and Top 0.1% ($3,973,435).", width = 120)
  ) + 
  theme_minimal() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title         = element_text(size = 12),
    plot.title         = element_text(size = 14),
    plot.subtitle      = element_text(size = 12), 
    plot.caption       = element_text(size = 9, color = "gray50", hjust = 0),
    legend.title       = element_text(size = 12),
    legend.text        = element_text(size = 9),
    legend.position    = "top",
    legend.box         = "horizontal",
    strip.text         = element_text(size = 12, vjust = -1), # Restored original vjust
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) 

ggsave(filename = 'C:/Users/jar335/Documents/Interfaces/tcja.png', 
       p, 
       width = 9, height = 5, dpi = 300)
