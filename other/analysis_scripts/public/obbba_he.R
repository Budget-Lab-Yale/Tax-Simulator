#------------------------------------------------------------------------
# Calculations for early-July blog on horizontal equity impacts of OBBBA
#------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)

#-----------
# Read data
#-----------

output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Simulator/v1/202506301505'

get_data = function(scenario, year) {
  output_root %>% 
    file.path(scenario, 'static/detail/', paste0(year, '.csv')) %>% 
    fread() %>% 
    tibble() %>%
    filter(dep_status == 0) %>% 
    mutate(scenario = scenario, year = year) %>% 
    return()
} 

microdata = bind_rows(
  get_data('baseline', 2026), 
  get_data('tcja',  2026),
  get_data('house', 2026), 
  get_data('senate', 2026)
) %>% 
  filter(expanded_inc > 0) %>% 
  left_join(
    (.) %>% 
      filter(scenario == 'baseline') %>% 
      select(id, inc = expanded_inc), 
    by = 'id'
  ) %>% 
  mutate(
    married = as.integer(filing_status == 2),
    etr_inc_net = pmax(-1, pmin(1, liab_iit_net / inc))
  ) %>% 
  group_by(year, scenario) %>% 
  mutate(
    inc_pctile = cut(
      x = inc,
      breaks = c(-Inf, wtd.quantile(inc, weight, seq(0.01, 0.99, 0.01)), Inf),
      labels = 1:100,
      include.lowest = TRUE
    ) %>% as.character() %>% as.numeric()
  ) %>% 
  ungroup() 


#-----------------------------
# Calculate within-group IQRs
#-----------------------------

# IQRs
within_group_dispersion = microdata %>%
  group_by(scenario, inc_pctile, married, n_dep) %>%
  summarise(
    n = sum(weight),
    group_iqr = if_else(n() > 2, 
                        wtd.quantile(etr_inc_net, weight, 0.75) - wtd.quantile(etr_inc_net, weight, 0.25), 
                        0),
    .groups = "drop"
  )

# Average IQR by quintile
dispersion = within_group_dispersion %>%
  group_by(
    scenario,
    inc_quintile = floor((inc_pctile - 1) / 20) + 1
  ) %>%
  summarise(
    avg_within_group_iqr = weighted.mean(group_iqr, n, na.rm = TRUE),
    .groups = "drop"
  )

# Average IQRs
dispersion_total = within_group_dispersion %>%
  group_by(scenario) %>%
  summarise(
    avg_within_group_iqr = weighted.mean(group_iqr, n, na.rm = TRUE),
    .groups = "drop"
  )


#-------
# Plots
#-------

# Simple, clean theme
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

create_waterfall_plot <- function(bill = "senate") {
  # Validate input
  if (!bill %in% c("house", "senate")) {
    stop("bill argument must be either 'house' or 'senate'")
  }
  
  # Create bill-specific labels
  bill_label <- stringr::str_to_title(bill)
  bill_step_label <- paste0(bill_label, "\nChanges")
  bill_final_label <- paste0(bill_label, "\nBill")
  
  # Prepare waterfall data with the specified bill
  waterfall_data = dispersion %>%
    filter(scenario %in% c('baseline', 'tcja', bill)) %>%
    select(scenario, inc_quintile, avg_within_group_iqr) %>%
    pivot_wider(names_from = scenario, values_from = avg_within_group_iqr) %>%
    mutate(
      baseline_pp = baseline * 100,
      tcja_pp = tcja * 100,
      bill_pp = .data[[bill]] * 100,  # Dynamic column reference
      tcja_change = tcja_pp - baseline_pp, 
      bill_change = bill_pp - tcja_pp   
    ) %>%
    select(inc_quintile, baseline_pp, tcja_change, bill_change, tcja_pp, bill_pp)
  
  # Create proper waterfall structure
  waterfall_plot_data = waterfall_data %>%
    rowwise() %>%
    do({
      quintile = .$inc_quintile
      data.frame(
        inc_quintile = quintile,
        step = c("Baseline", "TCJA\nImprovement", bill_step_label, "Final"),
        step_num = 1:4,
        value_type = c("level", "change", "change", "level"),
        display_value = c(.$baseline_pp, .$tcja_change, .$bill_change, .$bill_pp),
        
        # Correct positioning for waterfall
        y_bottom = c(0, .$baseline_pp, .$tcja_pp, 0),
        y_top = c(.$baseline_pp, .$tcja_pp, .$bill_pp, .$bill_pp),
        
        stringsAsFactors = FALSE
      )
    }) %>%
    ungroup() %>%
    mutate(
      step = factor(step, levels = c("Baseline", "TCJA\nImprovement", bill_step_label, "Final")),
      bar_height = y_top - y_bottom, 
      label = paste0(if_else(round(bar_height, 1) > 0 & value_type == 'change', '+', ''), as.character(round(bar_height, 1))),
      color_group = case_when(
        step_num == 1 ~ "baseline", 
        value_type == 'change' & display_value <= 0 ~ "improvement", 
        value_type == 'change' & display_value > 0  ~ "regression", 
        step_num == 4 ~ "final"
      )
    )
  
  # Create the plot
  p_waterfall = waterfall_plot_data %>%
    ggplot() +
    geom_hline(yintercept = 0) +
    # All bars using geom_rect for precise positioning
    geom_rect(aes(xmin = step_num - 0.3, xmax = step_num + 0.3,
                  ymin = y_bottom, ymax = y_top, fill = color_group),
              alpha = 0.8, color = "black", linewidth = 0.2) +
    # Connecting arrows to show flow
    geom_segment(data = . %>% filter(step_num < 4),
                 aes(x = step_num + 0.3, xend = step_num + 0.7,
                     y = y_top, yend = y_top),
                 color = 'black', linewidth = 0.5,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    # Value labels on bars with conditional coloring
    geom_text(aes(x = step_num, 
                  y = (y_bottom + y_top) / 2 + ifelse(abs(bar_height) < 0.25, 0.2, 0),  
                  label = label,
                  color = ifelse(abs(bar_height) < 0.25, "black", "white")),
              size = 3, fontface = "bold") +
    scale_color_identity() +
    facet_wrap(~paste("Quintile", inc_quintile), ncol = 5) +
    scale_fill_manual(
      name = "",
      values = c(
        "baseline" = "#95a5a6",      # Light gray for starting point
        "improvement" = "#27ae60",   # Green for improvement  
        "regression" = "#cc1616",    # Red for regression
        "final" = "#34495e"          # Dark gray for final
      )
    ) +
    scale_x_continuous(breaks = 1:4, 
                       labels = c("Baseline\n(Pre-TCJA)", "TCJA\nChanges", bill_step_label, bill_final_label)) +
    theme_clean() +
    theme(
      axis.text.x = element_text(size = 7),
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.major.x = element_blank(),
      plot.caption = element_text(hjust = 0), 
      legend.position = "none"
    ) +
    labs(
      title = if_else(
        bill == 'house', 
        "Figure 1. Estimated Impact of House-Passed Reconciliation Bill on Horizontal Equity, 2026",
        "Figure 2. Estimated Impact of Senate-Amended Reconciliation Bill on Horizontal Equity, 2026"
      ),
      subtitle = paste0("Average Within-Group Interquartile Range of Effective Tax Rate (higher values indicate more tax rate dispersion)\nBaseline → TCJA Extension → ", bill_label, " Bill"),
      x = element_blank(),
      y = "Percentage Points",
      caption = str_wrap('Source: The Budget Lab calculations. "Group" in "within-group" refers to combinations of income percentile, marital status, and number of dependents. Effective tax rate is net income tax liability (including refundable credits) divided by expanded income (AGI plus nontaxable interest/pensions/OASDI benefits, nondeductible capital losses, above-the-line deductions, and employer-side payroll taxes.', width = 180)
    ) 
  
  return(
    list(
      plot_data = waterfall_plot_data, 
      plot = p_waterfall
    )
  )
}

house = create_waterfall_plot("house")
senate = create_waterfall_plot("senate")

house$plot_data %>% select(inc_quintile, name = step, value = display_value) %>% pivot_wider() %>% write.csv()
senate$plot_data %>% select(inc_quintile, name = step, value = display_value) %>% pivot_wider() %>% write.csv()

