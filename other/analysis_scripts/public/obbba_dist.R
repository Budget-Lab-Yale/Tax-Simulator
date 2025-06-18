#------------------
# TODO
#------------------


library(tidyverse)


# Set parameters
output_root = '/vast/palmer/scratch/sarin/jmk263/model_data/Tax-Simulator/v1/'

params = list(
  house = list(
    vintage = '202506181247', 
    scenarios = c(
      '01-tcja', 
      '02-ctc', 
      '03-no_tax',
      '04-qbi', 
      '05-salt', 
      '06-other', 
      '07-estate',
      '08-depreciation'
    )
  ), 
  senate = list(
    vintage = '202506181249', 
    scenarios = c(
      '01-tcja', 
      '02-ctc', 
      '03-no_tax',
      '04-qbi', 
      '05-salt', 
      '06-other', 
      '07-estate',
      '08-depreciation'
    )
  ), 
  delta = list(
    vintage = '202506181250', 
    scenarios = c(
      '01-house', 
      '02-salt', 
      '03-qbi',
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
    taxes_included  == 'iit_pr_estate_cit_vat', 
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
  
dist %>%
  filter(variable!='avg') %>%
  select(!c(contribution, top_breakout)) %>%
  pivot_wider(names_from = 'group', values_from = 'value') %>%
  write_csv(., 'obbba_dist_tables.csv')

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
      scenario == '08-depreciation', # Filter for the net effect
      exercise != 'delta'     # Exclude 'delta' exercise
    ) %>%
    mutate(
      # Factor 'group' for correct ordering on the x-axis
      group = factor(group, levels = group_order),
      # Factor 'exercise' for consistent legend order and dodged bars
      exercise = factor(exercise, levels = c('house', 'senate')),
      # Add panel_label for faceting, consistent with the prior chart
      panel_label = ifelse(str_sub(group, 1, 1) != 'Q', "Within Top Quintile", "Across Quintiles")
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
      title    = "Figure 1. Selected Tax Provisions in the House and Senate Versions of the OBBBA, 2026",
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
    scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3.5))
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
    '06-other'  = '6) Other individual tax provisions',
    '07-estate' = '7) Estate tax cut',
    '08-depreciation' = '8) Faster cost recovery for businesses'
  )
  
  # Define darker colors to maintain consistent legend order
  provision_colors = c(
    '1) TCJA extension' = '#2d5a3d',
    '2) Further CTC expansion' = '#b8461f',
    '3) \'No tax on...\' provisions' = '#4a5a87',
    '4) Further QBI deduction expansion' = '#a5497a',
    '5) Higher SALT cap' = '#6b8f2a',
    '6) Other individual tax provisions' = '#cc9900', 
    '7) Estate tax cut' = '#7f7f7f',
    '8) Faster cost recovery for businesses' = '#996633'
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
      total = value[scenario == '08-depreciation'], 
      
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
      panel_label  = ifelse(top_breakout, "Within Top Quintile", "Across Quintiles"),
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
    scale_y_continuous(breaks = seq(0, 3, 1), limits = c(-0.5, 3.4))
  
}


build_contribution_plot('house')
build_contribution_plot('senate')



build_delta_plot = function() {
  
  # Create better scenario labels
  scenario_labels = c(
    '02-salt'   = 'Lower SALT cap',
    '03-qbi'    = 'Smaller QBI deduction expansion',
    '04-no_tax' = "Difference restrictions for 'No tax on...' provisions",
    '05-ctc'    = 'Smaller CTC expansion',
    '06-other'  = 'Other differences'
  )
  
  # Define darker colors to maintain consistent legend order
  provision_colors = c(
    'Lower SALT cap' = '#6b8f2a',
    'Smaller QBI deduction expansion' = '#a5497a',
    "Difference restrictions for 'No tax on...' provisions" = '#4a5a87',
    'Smaller CTC expansion' = '#b8461f',
    'Other differences' = '#cc9900'
  )
  
  # Define proper order for groups
  group_order = c(
    "Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5",
    "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"
  )
  
  # Calculate cumulative sum and label positions for both positive and negative stacks
  dist_for_plot = dist %>%
    filter(
      exercise == 'delta',
      variable == 'pct_chg_ati',
      scenario != '01-house' # Exclude '01-house' scenario for delta plot
    ) %>%
    group_by(group) %>%
    # Sort by scenario to ensure consistent stacking order
    arrange(scenario) %>%
    mutate(
      # The 'total' effect for the delta plot should be the sum of the filtered contributions
      total = sum(contribution),
      
      # Separate positive and negative contributions
      pos_contribution = pmax(contribution, 0),
      neg_contribution = pmin(contribution, 0),
      
      # Calculate positions for positive contributions (stacking upward from 0)
      pos_cumsum_end = cumsum(pos_contribution),
      pos_cumsum_start = lag(pos_cumsum_end, default = 0),
      
      # Calculate positions for negative contributions (stacking downward from 0)
      neg_cumsum_end = cumsum(neg_contribution),
      neg_cumsum_start = lag(neg_cumsum_end, default = 0),
      
      # Calculate label positions (center of each segment)
      y_label_pos = case_when(
        contribution > 0 ~ (pos_cumsum_start + pos_cumsum_end) / 2,
        contribution < 0 ~ (neg_cumsum_start + neg_cumsum_end) / 2,
        TRUE ~ 0
      )
    ) %>%
    ungroup() %>%
    mutate(
      panel_label = ifelse(str_sub(group, 1, 1) != 'Q', "Within Top Quintile", "Across Quintiles"),
      scenario_label = scenario_labels[scenario],
      # Reverse factor levels so the first scenario is closest to axis, but keep original order for legend
      scenario_label = factor(scenario_label, levels = rev(names(provision_colors))),
      # Fix group ordering using predefined order
      group = factor(group, levels = group_order)
    )
  
  # Create the plot
  ggplot(dist_for_plot, aes(x = group, y = contribution, fill = scenario_label)) +
    geom_col() +
    
    # Add data labels for contributions > 0.05
    geom_text(
      data = . %>% filter(abs(contribution) > 0.05),
      aes(label = sprintf("%.2f", contribution), y = y_label_pos),
      size = 3,
      color = "white"
    ) +
    
    # Add dots for total effect
    geom_point(
      data = dist_for_plot %>% distinct(group, total, panel_label),
      aes(x = group, y = total),
      size = 8,
      shape = 21,
      fill = "black",
      inherit.aes = FALSE
    ) +
    
    # Add data labels for dots (total effect)
    geom_text(
      data = dist_for_plot %>% distinct(group, total, panel_label),
      aes(x = group, y = total, label = sprintf("%.1f", total)),
      vjust = 0.5,
      hjust = 0.5,
      size = 3,
      color = "white",
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
      title = "Decomposition of Differences Between House and Senate:\nPercent Change in After-Tax Income, 2026",
      subtitle = "● Net Effect of All Provisions",
      x = "Income Group",
      y = "Percentage Points",
      fill = "Provision",
      caption = "Source: The Budget Lab calculations"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 9),
      legend.box = "horizontal",
      strip.text = element_text(size = 12, vjust = -1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5)
    )
}


build_delta_plot()



build_delta_plot = function() {
  
  # Create better scenario labels
  scenario_labels = c(
    '02-salt'   = 'Lower SALT cap',
    '03-qbi'    = 'Smaller QBI deduction expansion',
    '06-other'  = 'Other differences'
  )
  
  # Define darker colors to maintain consistent legend order
  provision_colors = c(
    'Lower SALT cap' = '#6b8f2a',
    'Smaller QBI deduction expansion' = '#a5497a',
    'Other differences' = '#cc9900'
  )
  
  # Define proper order for groups
  group_order = c(
    "Top 0.1%", "Top 1%", "Top 5%", "Top 10%",
    "Quintile 5", "Quintile 4", "Quintile 3", "Quintile 2", "Quintile 1"
  )
  
  # Calculate cumulative sum and label positions for both positive and negative stacks
  dist_for_plot = dist %>%
    filter(
      exercise == 'delta',
      variable == 'pct_chg_ati',
      scenario != '01-house' # Exclude '01-house' scenario for delta plot
    ) %>%
    group_by(group) %>%
    # Sort by scenario to ensure consistent stacking order
    arrange(scenario) %>%
    mutate(
      # The 'total' effect for the delta plot should be the sum of the filtered contributions
      total = sum(contribution),
      
      # Separate positive and negative contributions
      pos_contribution = pmax(contribution, 0),
      neg_contribution = pmin(contribution, 0),
      
      # Calculate positions for positive contributions (stacking upward from 0)
      pos_cumsum_end = cumsum(pos_contribution),
      pos_cumsum_start = lag(pos_cumsum_end, default = 0),
      
      # Calculate positions for negative contributions (stacking downward from 0)
      neg_cumsum_end = cumsum(neg_contribution),
      neg_cumsum_start = lag(neg_cumsum_end, default = 0),
      
      # Calculate label positions (center of each segment)
      y_label_pos = case_when( # This will be x_label_pos after flipping
        contribution > 0 ~ (pos_cumsum_start + pos_cumsum_end) / 2,
        contribution < 0 ~ (neg_cumsum_start + neg_cumsum_end) / 2,
        TRUE ~ 0
      )
    ) %>%
    ungroup() %>%
    mutate(
      panel_label = ifelse(str_sub(group, 1, 1) != 'Q', "Within Top Quintile", "Across Quintiles"),
      scenario_label = scenario_labels[scenario],
      # Reverse factor levels so the first scenario is closest to axis, but keep original order for legend
      scenario_label = factor(scenario_label, levels = rev(names(provision_colors))),
      # Fix group ordering using predefined order
      group = factor(group, levels = group_order)
    )
  
  # Determine overall y-axis limits to place annotations consistently
  # For horizontal bars, y-axis is discrete categories, so we'll pick a y-position
  # that is slightly below the actual plot area.
  # A good approach is to use a negative value for discrete axes or relative to the panel.
  # Let's try a y-position relative to the plot height in "npc" (normalized parent coordinates)
  # using the 'grid' package, but for simplicity, we'll aim for just below the x-axis labels.
  
  # Calculate a suitable y-coordinate for the annotations.
  # Assuming the bottom of the plot area is at y=0, and the first discrete category is at y=1.
  # We need to place annotations below the axis labels.
  # The actual values for y for annotate will depend on the exact plotting space.
  # Let's use `min(as.numeric(dist_for_plot$group))` for a numerical reference,
  # and then subtract a small value, or use 'Inf' and 'vjust' for precise placement.
  
  # A more robust way to place annotations outside the plot area is using `coord_cartesian(clip = "off")`
  # and then specifying 'y' values that fall outside the normal range of `group` levels.
  # For discrete y-axis, the levels are typically treated as 1, 2, 3...
  # So, a y-value of 0 or slightly negative would be below the first category.
  
  # Create the plot with flipped axes
  p <- ggplot(dist_for_plot, aes(y = group, x = contribution, fill = scenario_label)) +
    geom_col() +
    
    # Add data labels for contributions > 0.05
    geom_text(
      data = . %>% filter(abs(contribution) > 0.05),
      aes(label = sprintf("%.2f", contribution), x = y_label_pos),
      size = 3,
      color = "white",
      hjust = 0.5
    ) +
    
    # Add dots for total effect
    geom_point(
      data = dist_for_plot %>% distinct(group, total, panel_label),
      aes(y = group, x = total),
      size = 8,
      shape = 21,
      fill = "black",
      inherit.aes = FALSE
    ) +
    
    # Add data labels for dots (total effect)
    geom_text(
      data = dist_for_plot %>% distinct(group, total, panel_label),
      aes(y = group, x = total, label = sprintf("%.1f", total)),
      vjust = 0.5,
      hjust = 0.5,
      size = 3,
      color = "white",
      inherit.aes = FALSE
    ) +
    
    geom_vline(xintercept = 0, color = "black", linewidth = 1) +
    facet_wrap(~panel_label, scales = "free_y", ncol = 1) +
    
    # Use manual scale to control both colors and legend order
    scale_fill_manual(
      values = provision_colors,
      breaks = names(provision_colors)
    ) +
    
    labs(
      title = "Figure 2. Effects of Senate Finance Committee Changes to Selected Tax Provisions:\nDifference in Percent Change in After-Tax Income, 2026",
      subtitle = "● Net Difference",
      y = "Income Group",
      x = "Percentage Point Difference from House-Passed Version", # Keep concise
      fill = element_blank(),#"Provision",
      caption = "Source: The Budget Lab calculations",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 9),
      legend.box = "horizontal",
      strip.text = element_text(size = 12, vjust = -1),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 40, l = 10) # Increase bottom margin for annotations
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 5)
    )
  
  # Add custom annotations for the x-axis
  # Determine the y-position for annotations, slightly below the lowest income group.
  # The lowest discrete y-position is Quintile 1 (index 9 in group_order).
  # We need to place text in the negative y-coordinate space relative to the plot panel.
  # Using `Inf` with `vjust` and `hjust` can anchor text relative to the plot edges or panels.
  
  # To place annotations precisely, we'll need to convert discrete y-axis positions
  # to numeric if we want to use specific numeric y values for annotations that apply across panels.
  # A simpler approach is to add them using `annotate` with values slightly outside the range.
  
  # Approximate y-coordinate for annotations (below the lowest income group, which is Quintile 1).
  # For discrete y-axis, the categories are plotted at integer positions (1 to N).
  # If 'Quintile 1' is the Nth position (e.g., 9th if it's the last in `group_order`),
  # we want to place text at a y-value slightly below 1.
  # The `y` value of 0 would correspond to the "bottom" of the discrete y-axis scale if it were continuous.
  
  # We'll use relative y coordinates within the plot (0 to 1) for precise placement,
  # often done with `ggtext` or `annotation_custom` from `grid`.
  # For `annotate`, we'll pick a y-coordinate that is consistently outside the plot area.
  # Let's try using `Inf` and `vjust` to place them at the very bottom.
  
  # Calculate x-axis limits for arrow positioning
  x_limits <- layer_scales(p)$x$range.max[1] # Use the actual plotted range
  x_min_val <- min(dist_for_plot$contribution) # Example: -0.5
  x_max_val <- max(dist_for_plot$contribution) # Example: 0.1
  
  # Y-position for the annotations (below the axis)
  # For a flipped chart (y=group, x=contribution), the x-axis labels are at the bottom.
  # We need to place annotations below the x-axis title.
  # 'y' for annotate can be `min(as.numeric(factor(dist_for_plot$group)))` or similar
  # if we want it aligned with the very bottom of the data.
  # For text *outside* the plotting area, `coord_cartesian(clip="off")` is essential.
  # And then use y values that are outside the range of your data's y-aesthetic (income groups).
  # If income groups are mapped as factor levels, their numeric representation goes from 1 to 9.
  # So y = 0 or y = -0.5 would be below the lowest group.
  
  # Let's try a y-position that's clearly below the lowest income group label, e.g., 0.5
  # for the center of the labels, assuming 1 is the bottom of the lowest bar.
  
  # Adjust y-position based on the actual plot dimensions and `group_order`
  # The groups are factors, so their numerical representation will be 1 through length(group_order).
  # Let's target a y-position just below 1 (which is the bottom-most bar for "Quintile 1" if it were reversed,
  # or "Top 0.1%" as per the new order).
  # We need a fixed y-coordinate that works across all panels.
  
  # A more reliable way is to use a normalized coordinate system for annotations
  # but for `annotate` we can set `y` slightly below the lowest `group` factor value.
  # Lowest factor level is 1. So, let's try `y = 0.2` or `y = 0.5`
  
  # Define y-position for annotations (relative to the discrete y-axis)
  # This might require some trial and error to get the exact vertical placement.
  # Let's use a small constant for now that places it below the lowest bar.
  
  # The y-axis ranges from 1 to `length(group_order)`. Let's pick a y for annotation
  # slightly below 1.
  anno_y_pos = 0.3 # Adjust as needed. This places it below the last bar in each facet.
  
  # Text annotations
  p <- p +
    annotate("text",
             x = -0.1, # Position left text on the negative side
             y = anno_y_pos, # Y-position for text
             label = "Senate is less generous than the House",
             hjust = .5, vjust = 0.5, size = 3) # +
  # annotate("text",
   #          x = 0.1, # Position right text on the positive side
    #         y = anno_y_pos, # Y-position for text
     #        label = "Senate is more generous than the House",
      #       hjust = .5, vjust = 0.5, size = 3)
  
  # Arrows (adjust x, xend, y, yend, and arrow parameters as needed)
  # For the left arrow: from a point far left, ending near x=0
  # For the right arrow: from a point near x=0, ending far right
  arrow_len_cm = 0.3 # Length of the arrow head
  
  p <- p +
    annotate("segment",
             x = -0.05, xend = -0.15, # Start far left, end slightly before 0
             y = anno_y_pos - 0.5, yend = anno_y_pos - 0.5, # Y-position for arrow, slightly below text
             arrow = arrow(length = unit(arrow_len_cm, "cm"), type = "closed", ends = "last"),
             linewidth = 0.6, color = "black") #+
   # annotate("segment",
   #          x = 0.05, xend = 0.15, # Start slightly after 0, end far right
  #           y = anno_y_pos - 0.3, yend = anno_y_pos - 0.3, # Y-position for arrow
  #           arrow = arrow(length = unit(arrow_len_cm, "cm"), type = "closed", ends = "last"),
    #         linewidth = 0.6, color = "black")
  
  # Ensure nothing is clipped
  p <- p + coord_cartesian(clip = "off")
  
  return(p)
}


build_delta_plot()
