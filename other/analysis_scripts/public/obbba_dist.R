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
    taxes_included  == 'iit_pr_estate',   # TODO change if distributing estate
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
  


#-------
# Plots
#-------


build_net_effect_plot = function() {
  
  # Define colors for the 'exercise' (House vs. Senate)
  exercise_colors = c(
    'house'  = '#1f77b4', # A standard blue
    'senate' = '#ff7f0e'  # A standard orange
  )
  
  # Define proper order for groups (assuming it's the same as before)
  group_order = c(
    "Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5",
    "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"
  )
  
  # Prepare data for the net effect plot
  # The 'net effect' is typically the last scenario, '06-other'
  plot_data = dist %>%
    filter(
      variable == 'pct_chg_ati',
      scenario == '06-other', # Filter for the net effect
      exercise != 'delta'     # Exclude 'delta' exercise
    ) %>%
    mutate(
      # Factor 'group' for correct ordering on the x-axis
      group = factor(group, levels = group_order),
      # Factor 'exercise' for consistent legend order and dodged bars
      exercise = factor(exercise, levels = c('house', 'senate')),
      # Add panel_label for faceting, consistent with the prior chart
      panel_label = ifelse(str_sub(group, 1, 1) != 'Q', "Within Top Quintile", "Quintiles")
    )
  
  # Create the plot
  ggplot(plot_data, aes(x = group, y = value, fill = exercise)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) + # Dodge bars
    geom_hline(yintercept = 0, color = "black", linewidth = 1) +
    
    # Add data labels on top of each bar
    geom_text(
      aes(
        label = sprintf("%.1f", value), # Format label to one decimal place
        # Adjust vjust based on whether the bar is positive or negative
        vjust = ifelse(value >= 0, -0.5, 1.5) # Above for positive, below for negative
      ),
      position = position_dodge(width = 0.8), # Ensure labels dodge with bars
      size     = 3,
      color    = "black" # Use black for labels as they are outside the bars
    ) +
    
    # Facet by panel_label, consistent with the prior chart
    facet_wrap(~panel_label, scales = "free_x") +
    
    scale_fill_manual(
      values = exercise_colors,
      labels = c('house' = 'House Version', 'senate' = 'Senate Version') # Nicer legend labels
    ) +
    labs(
      title    = "Comparison of Net Effect: House vs. Senate Versions (2026)",
      subtitle = "Percent Change in After-Tax Income by Income Group",
      x        = "Income Group",
      y        = "Percentage Points",
      fill     = element_blank(),
      caption  = "Source: The Budget Lab calculations"
    ) +
    theme_minimal() +
    theme(
      axis.text.y        = element_text(size = 12),
      axis.title         = element_text(size = 12),
      plot.title         = element_text(size = 14), # Removed hjust for left justification
      plot.subtitle      = element_text(size = 12), # Removed hjust for left justification
      plot.caption       = element_text(size = 9, color = "gray50", hjust = 0),
      legend.title       = element_text(size = 12),
      legend.text        = element_text(size = 9),
      legend.position    = "top",
      legend.box         = "horizontal",
      strip.text         = element_text(size = 12, vjust = -1), # Restored original vjust
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    ) + 
    scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3))
}


build_net_effect_plot()




build_contribution_plot = function(version) {
  
  # Build title of plot
  title_start = "Selected Tax Provisions of the House-Passed OBBBA: "
  if (version == 'senate') {
    title_start = "Selected Tax Provisions of the Senate Finance Committee OBBBA: "
  }
  
  # Create better scenario labels
  scenario_labels = c(
    '01-tcja'   = '1) TCJA extension',
    '02-ctc'    = '2) Further CTC expansion',
    '03-no_tax' = "3) 'No tax on...' provisions",
    '04-qbi'    = '4) Further QBI deduction expansion',
    '05-salt'   = '5) Higher SALT cap',
    '06-other'  = '6) Other provisions'
  )
  
  # Define darker colors to maintain consistent legend order
  provision_colors = c(
    '1) TCJA extension' = '#2d5a3d',
    '2) Further CTC expansion' = '#b8461f',
    '3) \'No tax on...\' provisions' = '#4a5a87',
    '4) Further QBI deduction expansion' = '#a5497a',
    '5) Higher SALT cap' = '#6b8f2a',
    '6) Other provisions' = '#cc9900'
  )
  
  # Define proper order for groups
  group_order = c(
    "Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5",
    "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"
  )
  
  # Calculate cumulative sum and label positions for both positive and negative stacks
  dist_for_plot = dist %>%
    filter(
      exercise == version,
      variable == 'pct_chg_ati'
    ) %>%
    group_by(group) %>%
    # Important: Separate positive and negative contributions for correct stacking logic
    # Arrange for correct cumulative sum calculation
    arrange(scenario) %>%
    mutate(
      total = value[scenario == '06-other'], # Your existing total calculation
      
      # Calculate cumulative sums for positive contributions
      positive_contribution = ifelse(contribution >= 0, contribution, 0),
      pos_y_bottom = lag(cumsum(positive_contribution), default = 0),
      pos_y_label_pos = pos_y_bottom + (positive_contribution / 2),
      
      # Calculate cumulative sums for negative contributions (stacking downwards from 0)
      negative_contribution = ifelse(contribution < 0, contribution, 0),
      # Need to accumulate in reverse order for negative contributions to stack correctly
      # Or, more simply, think about the current position relative to the sum of preceding negative values
      neg_y_top = lag(cumsum(negative_contribution), default = 0), # Top of the negative bar segment
      neg_y_label_pos = neg_y_top + (negative_contribution / 2),
      
      # Decide which y_label_pos to use based on the sign of the contribution
      y_label_pos = ifelse(contribution >= 0, pos_y_label_pos, neg_y_label_pos)
    ) %>%
    ungroup() %>%
    mutate(
      panel_label  = ifelse(top_breakout, "Within Top Quintile", "Quintiles"),
      scenario_label = scenario_labels[scenario],
      # Reverse factor levels so TCJA is closest to axis, but keep original order for legend
      scenario_label = factor(scenario_label, levels = rev(names(provision_colors))),
      # Fix group ordering using predefined order
      group = factor(group, levels = group_order)
    )
  
  # Now use dist_for_plot in your ggplot call
  dist_for_plot %>%
    ggplot(aes(x = group, y = contribution, fill = scenario_label)) +
    geom_col() +
    
    # Add data labels for contributions > 0.2pp
    geom_text(
      data = . %>% filter(abs(contribution) > 0.2),
      aes(label = sprintf("%.1f", contribution), y = y_label_pos),
      size      = 3,
      color     = "white",
    ) +
    
    # Add dots for total effect - using separate data to avoid inheritance issues
    geom_point(
      data = dist_for_plot %>% distinct(group, total, panel_label),
      aes(x = group, y = total),
      size        = 8,
      shape       = 21,
      fill        = "black",
      inherit.aes = FALSE
    ) +
    
    # Add data labels for dots (total effect) - positioned inside the dots
    geom_text(
      data = dist_for_plot %>% distinct(group, total, panel_label),
      aes(x = group, y = total, label = sprintf("%.1f", total)),
      vjust       = 0.5,
      hjust       = 0.5,
      size        = 3,
      color       = "white",
      inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "black", linewidth = 1) +
    facet_wrap(~panel_label, scales = "free_x") +
    
    # Use manual scale to control both colors and legend order
    scale_fill_manual(
      values = provision_colors,
      breaks = names(provision_colors)
    ) +
    
    labs(
      title    = paste0(title_start, "Provision-Level \nDecomposition of Percent Change in After-Tax Income, 2026"),
      subtitle = "● Net Effect of All Provisions",
      x        = "Income Group",
      y        = "Percentage Points",
      fill     = "Provision",
      caption  = "Source: The Budget Lab calculations"
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
      legend.box         = "horizontal",
      strip.text         = element_text(size = 12, vjust = -1),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    scale_y_continuous(breaks = seq(0, 3, 1), limits = c(-0.5, 3.1))
  
}

build_contribution_plot('house')
build_contribution_plot('senate')





