#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# TODO
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------


library(tidyverse)

#----------------
# Set parameters
#----------------

output_root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1'

params = list(
  current_policy = list(
    vintage = '202507240950', 
    scenarios = c(
      'senate-ind'
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
    group_dimension %in% c('Income', 'Overall'), 
    group           != 'Negative income'
  ) %>%
  mutate(
    panel = case_when(
      group == 'Overall' ~ 'Overall',
      str_sub(group, 1, 1) != 'Q' ~ 'Within Top Quintile', 
      T ~ 'By Quintile'
    ),
    `No cut or tax increase` = 1 - share_cut.5,
    `< $100`         = share_cut.5 - share_cut.100, 
    `$100 - $500`    = share_cut.100 - share_cut.500,
    `$500 - $1,000`  = share_cut.500 - share_cut.1000,
    `$1,000 - $5,000` = share_cut.1000 - share_cut.5000,
    `> $5,000`       = share_cut.5000,
  ) %>% 
  select(year, exercise, scenario, group, panel, income_cutoff, contains(' ')) %>% 
  pivot_longer(
    cols = contains(' '),
    names_to = 'tax_change'
  )


#------------
# Chart Setup
#------------

# Define proper order for groups
panel_order = c("Overall", "By Quintile", "Within Top Quintile")

group_order = c(
  "Top 0.1%", "Top 1%", "Top 5%", "Top 10%", "Quintile 5", "Quintile 4", 
  "Quintile 3", "Quintile 2", "Quintile 1", "Overall"
)

tax_change_order = c(
  "> $5,000",
  "$1,000 - $5,000",
  "$500 - $1,000",
  "$100 - $500",
  "< $100",
  "No cut or tax increase"
)

# Define colors to match the example (adjusted for your categories)
tax_change_colors = c(
  "> $5,000"               = "#059669",  # Dark green
  "$1,000 - $5,000"        = "#34d399",  # Medium green  
  "$500 - $1,000"          = "#22d3ee",  # Cyan
  "$100 - $500"            = "#60a5fa",  # Light blue
  "< $100"                 = "#fbbf24",  # Yellow/amber
  "No cut or tax increase" = "#dc2626"   # Red
)

# Common theme function
chart_theme <- function() {
  theme_minimal() +
    theme(
      axis.text.y        = element_text(size = 12),
      axis.title         = element_text(size = 12),
      plot.title         = element_text(size = 18),
      plot.subtitle      = element_text(size = 12), 
      plot.caption       = element_text(size = 9, color = "gray50", hjust = 0),
      
      # Improved legend styling
      legend.title       = element_blank(),
      legend.text        = element_text(size = 10),
      legend.position    = "top",
      legend.box         = "horizontal",
      legend.margin      = margin(0, 0, 10, 0),
      legend.key.size    = unit(0.4, "cm"),
      legend.key.width   = unit(3.2, "cm"),
      legend.spacing.x   = unit(0.2, "cm"),
      
      strip.text         = element_text(size = 12, vjust = -1),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

#------------------
# Chart 1: Overall
#------------------

chart1_overall <- dist %>%
  filter(year == 2026, panel == "Overall") %>%
  mutate(
    group = factor(group, levels = group_order),
    tax_change = factor(tax_change, levels = tax_change_order),
  ) %>% 
  ggplot(aes(x = group, y = value, fill = tax_change)) +
  geom_col(width = 0.9) + 
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Add data labels for segments > 5% to avoid clutter
  geom_text(
    data = . %>% filter(value > 0.05),
    aes(label = paste0(round(value * 100), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    color = "white",
    fontface = "bold"
  ) +
  
  # Flip coordinates for horizontal bars
  coord_flip() +
  
  # Apply custom colors
  scale_fill_manual(values = tax_change_colors) +
  
  # Labels and styling
  labs(
    title    = "Figure 1. Overall Distribution of Tax Cuts Against Current Policy, 2026",
    subtitle = '',
    x        = element_blank(),
    y        = element_blank(),
    fill     = element_blank(),
    caption  = str_wrap("Source: The Budget Lab calculations. Note: Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. 'Income' is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, and inheritances.", width = 160)
  ) + 
  chart_theme() +
  scale_y_continuous(labels = scales::percent_format()) +
  
  # Make legend more compact
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      reverse = TRUE, 
      label.position = "bottom"
    )
  )

#-------------------------------
# Chart 2: By Income Groups
#-------------------------------

chart2_by_income <- dist %>%
  filter(year == 2026, panel %in% c("By Quintile", "Within Top Quintile")) %>%
  mutate(
    panel = factor(panel, levels = panel_order),
    group = factor(group, levels = group_order),
    tax_change = factor(tax_change, levels = tax_change_order),
  ) %>% 
  ggplot(aes(x = group, y = value, fill = tax_change)) +
  geom_col(width = 0.9) + 
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Add data labels for segments > 5% to avoid clutter
  geom_text(
    data = . %>% filter(value > 0.05),
    aes(label = paste0(round(value * 100), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white",
    fontface = "bold"
  ) +
  
  # Flip coordinates for horizontal bars
  coord_flip() +
  
  facet_wrap(~panel, scales = "free_y", ncol = 1) +
  
  # Apply custom colors
  scale_fill_manual(values = tax_change_colors) +
  
  # Labels and styling
  labs(
    title    = "Figure 2. Distribution of Tax Cuts Against Current Policy by Income Group, 2026",
    subtitle = '',
    x        = element_blank(),
    y        = element_blank(),
    fill     = element_blank(),
    caption  = str_wrap("Source: The Budget Lab calculations. Note: Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. Income group cutoffs are approximately: Quintile 2 ($21,170), Quintile 3 ($43,845), Quintile 4 ($75,995), Quintile 5 ($137,565), Top 10% ($213,480), Top 5% ($319,275), Top 1% ($845,270), and Top 0.1% ($3,973,435).", width = 160)
  ) + 
  chart_theme() +
  scale_y_continuous(labels = scales::percent_format()) +
  
  # Make legend more compact
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      reverse = TRUE, 
      label.position = "bottom"
    )
  )

# Display the charts
print(chart1_overall)
print(chart2_by_income)

# Export data as before
dist %>%
  filter(year == 2026) %>% 
  select(group, name = tax_change, value) %>% 
  pivot_wider() %>% 
  write.csv()