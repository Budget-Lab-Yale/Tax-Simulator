#----------------------------------------------
# Cleaned up horizontal equity visualizations
# Simple but polished
#----------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)


output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Simulator/v1/202506231509'

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
  get_data('senate',  2026)
) %>% 
  filter(expanded_inc > 0, dep_status == 0) %>% 
  rename(inc = expanded_inc) %>% 
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

within_group_dispersion = microdata %>%
  group_by(scenario, inc_pctile, married, n_dep) %>%
  summarise(
    n = sum(weight),
    group_iqr = if_else(n() > 2, 
                        wtd.quantile(etr_inc_net, weight, 0.75) - wtd.quantile(etr_inc_net, weight, 0.25), 
                        0),
    .groups = "drop"
  )

dispersion = within_group_dispersion %>%
  group_by(
    scenario,
    inc_quintile = floor((inc_pctile - 1) / 20) + 1
  ) %>%
  summarise(
    avg_within_group_iqr = weighted.mean(group_iqr, n, na.rm = TRUE),
    .groups = "drop"
  )

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


# Prepare waterfall data
waterfall_data = dispersion %>%
  filter(scenario %in% c('baseline', 'tcja', 'house')) %>%
  select(scenario, inc_quintile, avg_within_group_iqr) %>%
  pivot_wider(names_from = scenario, values_from = avg_within_group_iqr) %>%
  mutate(
    baseline_pp = baseline * 100,
    tcja_pp = tcja * 100,
    house_pp = house * 100,
    tcja_change = tcja_pp - baseline_pp,  # Negative = improvement
    house_change = house_pp - tcja_pp     # Positive = regression
  ) %>%
  select(inc_quintile, baseline_pp, tcja_change, house_change, tcja_pp, house_pp)

# Create proper waterfall structure (YOUR DESIGN!)
waterfall_plot_data = waterfall_data %>%
  rowwise() %>%
  do({
    quintile = .$inc_quintile
    data.frame(
      inc_quintile = quintile,
      step = c("Baseline", "TCJA\nImprovement", "House\nRegression", "Final"),
      step_num = 1:4,
      value_type = c("level", "change", "change", "level"),
      display_value = c(.$baseline_pp, .$tcja_change, .$house_change, .$house_pp),
      
      # Correct positioning for waterfall
      y_bottom = c(0, .$baseline_pp, .$tcja_pp, 0),
      y_top = c(.$baseline_pp, .$tcja_pp, .$house_pp, .$house_pp),
    
      stringsAsFactors = FALSE
    )
  }) %>%
  ungroup() %>%
  mutate(
    step = factor(step, levels = c("Baseline", "TCJA\nImprovement", "House\nRegression", "Final")),
    bar_height = y_top - y_bottom, 
    label = paste0(if_else(round(bar_height, 1) > 0 & value_type == 'change', '+', ''), as.character(round(bar_height, 1))),
    color_group = case_when(
      step_num == 1 ~ "baseline", 
      value_type == 'change' & display_value <= 0 ~ "improvement", 
      value_type == 'change' & display_value > 0  ~ "regression", 
      step_num == 4 ~ "final"
    )
  )


p_waterfall = waterfall_plot_data %>%
  ggplot() +
  # All bars using geom_rect for precise positioning
  geom_rect(aes(xmin = step_num - 0.3, xmax = step_num + 0.3,
                ymin = y_bottom, ymax = y_top, fill = color_group),
            alpha = 0.8, color = "black", size = 0.2) +
  # Connecting arrows to show flow
  geom_segment(data = . %>% filter(step_num < 4),
               aes(x = step_num + 0.3, xend = step_num + 0.7,
                   y = y_top, yend = y_top),
               color = 'black', size = 0.5,
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
      "improvement" = "#27ae60",   # Green for TCJA improvement  
      "regression" = "#cc1616",    # Red for House regression
      "final" = "#34495e"          # Dark gray for final
    ),
    labels = c("baseline" = "Baseline", "improvement" = "TCJA Improvement", 
               "regression" = "House Regression", "final" = "Final Level")
  ) +
  scale_x_continuous(breaks = 1:4, 
                     labels = c("Baseline\n(Pre-TCJA)", "TCJA\nChanges", "House\nChanges", "House\nBill")) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 7),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0), 
    legend.position = "none"
  ) +
  labs(
    title = "Figure 1. Estimated Impact of Proposed Tax Changes on Horizontal Equity, 2026",
    subtitle = "Average Within-Group Interquartile Range of Effective Tax Rate (higher values indicate more tax rate dispersion)\nBaseline → TCJA Extension → House Bill",
    x = element_blank(),
    y = "Percentage Points",
    caption = str_wrap('Source: The Budget Lab calculations. "Group" in "within-group" refers to combinations of income percentile, marital status, and number of dependents. Effective tax rate is net income tax liability (including refundable credits) divided by expanded income (AGI plus nontaxable interest/pensions/OASDI benefits plus nondeductible capital losses plus above-the-line deductions plus employer-side payroll taxes.', width = 160)
  ) 

print(p_waterfall)


