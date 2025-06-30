#------------------------------------------------------------------------------
# Calculations for the Budget Lab's late-June report on the net distributional 
# impact of spending cuts and tax cuts in the House-passed and Senate-amended
# OBBBA.
#------------------------------------------------------------------------------

library(tidyverse)


#----------------
# Set parameters
#----------------

tax_root      = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202506291832'
spending_root = '/gpfs/gibbs/project/sarin/shared/model_data/Safety-Net/v1/202506300952'
macro_root    = '/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2025040115/baseline'

scenarios = c(
  'house_20250522',
  'senate_20250627', 
  'senate_20250627_scott_amendment'
)


#-----------------------
# Read and process data 
#-----------------------

# Read and clean up tax data
tax = scenarios %>% 
  map(
    .f = ~ {
      tax_root %>% 
        file.path(.x, 'static/supplemental/distribution.csv') %>%
        read_csv(show_col_types = F) %>% 
        mutate(scenario = .x)
    }
  ) %>%
  bind_rows() %>% 
  filter(
    taxes_included  == 'iit_pr_estate_cit_vat', 
    group_dimension == 'AGI',
    group           != 'Overall',
  ) %>%
  group_by(scenario, year) %>%  
  mutate(
    top_breakout  = str_sub(group, 1, 1) != 'Q', 
    share_labor   = labor / sum(labor[!top_breakout]), 
    share_capital = capital / sum(capital[!top_breakout]), 
    tax_change    = avg * n_tax_units / 1e9
  ) %>% 
  ungroup() %>% 
  filter(group != 'Negative income') %>% 
  select(scenario, year, group, top_breakout, agi_cutoff, n_tax_units, 
         share_labor, share_capital, ati_baseline, tax_change)


# Read spending data. First, provider changes
providers = scenarios %>% 
  map(
    .f = ~ {
      spending_root %>% 
        file.path(.x, 'provider/medicaid_provider.csv') %>%
        read_csv(show_col_types = F) %>% 
        mutate(
          scenario = .x,
          medicaid_providers_labor   = medicaid_changes_provider_lab / 1e3, 
          medicaid_providers_capital = medicaid_changes_provider_cap / 1e3
        ) %>% 
        select(scenario, year, medicaid_providers_labor, medicaid_providers_capital)
    }
  ) %>% 
  bind_rows()


# Next, benefit changes
benefits = scenarios %>% 
  map(
    .f = ~ {
      2025:2034 %>% 
        map(
          .f = function(y) {
            spending_root %>% 
              file.path(.x, 'beneficiary', paste0(y, '.csv')) %>% 
              read_csv(show_col_types = F) %>% 
              mutate(scenario = .x, year = y, .before = everything()) 
          }
        ) %>% 
        bind_rows()
    } 
  ) %>% 
  bind_rows() %>% 
  mutate(
    group = case_when(
      agi_group == 1 ~ "Quintile 1",  # Bottom quintile
      agi_group == 2 ~ "Quintile 2",  # Second quintile
      agi_group == 3 ~ "Quintile 3",  # Middle quintile
      agi_group == 4 ~ "Quintile 4",  # Fourth quintile
      agi_group == 5 ~ "Quintile 5",  # Top quintile
      TRUE ~ agi_group_label        
    ), 
    across(
      .cols = c(starts_with('snap'), starts_with('medicaid')), 
      .fns  = ~ . / 1e3
    )
  ) %>% 
  select(scenario, year, group, 
         medicaid_benefits_baseline = medicaid_baseline, 
         medicaid_benefits_scenario = medicaid_scenario, 
         starts_with('snap'))


# Read macro projections (for inflation)
macro = macro_root %>% 
  file.path('projections.csv') %>% 
  read_csv(show_col_types = F)


#-----------------
# Do calculations
#-----------------

totals = tax %>% 
  
  # Merge datasets
  left_join(providers, by = c('scenario', 'year')) %>% 
  left_join(benefits, by = c('scenario', 'year', 'group')) %>% 
  
  mutate(
    
    # Apply provider aggregates to factor income shares
    medicaid_providers_labor   = medicaid_providers_labor * share_labor,
    medicaid_providers_capital = medicaid_providers_capital * share_capital,
    
    # Calculate after-tax-and-transfer income
    atti_baseline = ati_baseline + medicaid_benefits_baseline + snap_baseline,
    atti_scenario = ati_baseline - tax_change + medicaid_benefits_scenario + snap_scenario,

    # Deltas 
    tax_change      = -tax_change,
    medicaid_change = medicaid_providers_labor + medicaid_providers_capital + (medicaid_benefits_scenario - medicaid_benefits_baseline),
    snap_change     = snap_scenario - snap_baseline,
    spending_change = medicaid_change + snap_change,
    atti_change     = atti_scenario - atti_baseline,
    
    # Per-tax unit average deltas
    across(
      .cols = ends_with('_change'), 
      .fns  = ~ (. * 1e9) / n_tax_units, 
      .names = '{col}.avg'
    ), 
    
    # Percent-of-after-tax-and-transfer income deltas
    across(
      .cols  = c(ends_with('_change')), 
      .fns   = ~ . / atti_baseline, 
      .names = '{col}.atti'
    )
  ) 


# Averages over 9 years
avg = totals %>% 
  group_by(scenario, group) %>% 
  mutate(
    agi_cutoff_2025 = agi_cutoff[year == 2025],
  ) %>% 
  select(scenario, group, top_breakout, agi_cutoff_2025, year, ends_with('.atti'), ends_with('.avg')) %>% 
  ungroup() %>% 
  filter(year > 2025) %>% 
  
  # Deflate dollar-amounts to 2025
  left_join(
    macro %>% 
      select(year, cpiu), 
    by = 'year'
  ) %>% 
  mutate(
    across(
      .cols = c(ends_with('.avg')), 
      .fns  = ~ . / cpiu
    )
  ) %>% 
  group_by(scenario, group, top_breakout, agi_cutoff_2025) %>% 
  summarise(
    across(
      .cols = c(ends_with('.atti'), ends_with('.avg')), 
      .fns  = mean
    ), 
    .groups = 'drop'
  )


#---------
# Tables
#--------

# T1, T2, AT1
avg %>% 
  select(scenario, group, agi_cutoff_2025, atti_change.avg, atti_change.atti) %>% 
  arrange(scenario, group = factor(group, levels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", 
                                                     "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"))) %>% 
  mutate(atti_change.avg = round(atti_change.avg / 5) * 5) %>%
  write.csv()
 
# F1, F2, AF1
avg %>% 
  select(scenario, group, medicaid_change.atti, snap_change.atti, tax_change.atti, atti_change.atti) %>% 
  arrange(scenario, group = factor(group, levels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", 
                                                     "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"))) %>% 
  write.csv()

#-------
# Plots
#-------


avg %>% 
  filter(scenario == 'house_20250522') %>% 
  select(group, top_breakout, tax_change.atti, medicaid_change.atti, snap_change.atti, atti_change.atti) %>%
  pivot_longer(
    cols = c(tax_change.atti, medicaid_change.atti, snap_change.atti)
  ) %>% 
  
  # Create cleaner labels for the provisions
  mutate(
    name = case_when(
      name == "tax_change.atti" ~ "Changes to Taxes",
      name == "medicaid_change.atti" ~ "Changes to Medicaid",
      name == "snap_change.atti" ~ "Changes to SNAP",
      TRUE ~ name
    ),
    
    # Create faceting variable
    facet_group = ifelse(top_breakout, "Within Top Quintile", "Across Quintiles"),
    
    # Order the income groups properly
    group = factor(group, levels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", 
                                     "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"))
  ) %>%
  ggplot(aes(x = group, y = value, fill = name)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 1) + 
  geom_point(aes(y = atti_change.atti), size = 9, shape = 21, fill = "white", color = "black", stroke = 1, show.legend = FALSE) + 
  
  geom_text(aes(y = atti_change.atti, label = sprintf("%.1f", atti_change.atti * 100)), 
            size = 2.8, color = "black") +
  facet_wrap(~facet_group, scales = "free_x", ncol = 2) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 11), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    legend.position = "top",
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray50"),
    strip.text = element_text(size = 12, face = "bold")
  ) + 
  labs(
    x = 'Adjusted Gross Income (AGI) Group', 
    y = 'Percentage Points', 
    fill = element_blank(),
    title = "Figure 1. Estimated Effects of Major Spending and Tax Provisions of the House-Passed Reconciliation Bill, 2026-2034",
    subtitle = "Average Annual Percent Change in After-Tax-And-Transfer Income",
    caption = str_wrap("Source: The Budget Lab and Congressional Budget Office calculations. Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only. After-tax-and-transfer income is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, inheritances, Medicaid benefits, and SNAP benefits. AGI percentile thresholds are calculated with respect to positive income only. AGI thresholds for 2025 are approximately: Quintile 2 ($13,350), Quintile 3 ($36,475), Quintile 4 ($64,995), Quintile 5 ($120,390), Top 10% ($184,600), Top 5% ($264,445), Top 1% ($649,005), and Top 0.1% ($3,305,375)", width = 200)
  ) + 
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x * 100),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_fill_manual(values = c(
    "Changes to Taxes" = "#2E86AB",
    "Changes to Medicaid" = "#C73E1D", 
    "Changes to SNAP" = "#FF9800"
  )) +
  # Ensure legend shows in logical order
  guides(fill = guide_legend(override.aes = list(size = 3)))





avg %>% 
  filter(scenario == 'senate_20250627_scott_amendment') %>% 
  select(group, top_breakout, tax_change.atti, medicaid_change.atti, snap_change.atti, atti_change.atti) %>%
  pivot_longer(
    cols = c(tax_change.atti, medicaid_change.atti, snap_change.atti)
  ) %>% 
  
  # Create cleaner labels for the provisions
  mutate(
    name = case_when(
      name == "tax_change.atti" ~ "Changes to Taxes",
      name == "medicaid_change.atti" ~ "Changes to Medicaid",
      name == "snap_change.atti" ~ "Changes to SNAP",
      TRUE ~ name
    ),
    
    # Create faceting variable
    facet_group = ifelse(top_breakout, "Within Top Quintile", "Across Quintiles"),
    
    # Order the income groups properly
    group = factor(group, levels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", 
                                     "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"))
  ) %>%
  ggplot(aes(x = group, y = value, fill = name)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 1) + 
  geom_point(aes(y = atti_change.atti), size = 9, shape = 21, fill = "white", color = "black", stroke = 1, show.legend = FALSE) + 
  
  geom_text(aes(y = atti_change.atti, label = sprintf("%.1f", atti_change.atti * 100)), 
            size = 2.8, color = "black") +
  facet_wrap(~facet_group, scales = "free_x", ncol = 2) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 11), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    legend.position = "top",
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray50"),
    strip.text = element_text(size = 12, face = "bold")
  ) + 
  labs(
    x = 'Adjusted Gross Income (AGI) Group', 
    y = element_blank(),
    fill = element_blank(),
    title = "Figure 2. Estimated Effects of Major Spending and Tax Provisions of the Senate Reconciliation Bill (with Scott Proposal), 2026-2034",
    subtitle = "Average Annual Percent Change in After-Tax-And-Transfer Income",
    caption = str_wrap("Source: The Budget Lab and Congressional Budget Office calculations. Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. After-tax-and-transfer income is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, inheritances, Medicaid benefits, and SNAP benefits. AGI percentile thresholds are calculated with respect to positive income only. AGI thresholds for 2025 are approximately: Quintile 2 ($13,350), Quintile 3 ($36,475), Quintile 4 ($64,995), Quintile 5 ($120,390), Top 10% ($184,600), Top 5% ($264,445), Top 1% ($649,005), and Top 0.1% ($3,305,375) Quintile 2 ($13,350), Quintile 3 ($36,475), Quintile 4 ($64,995), Quintile 5 ($120,390), Top 10% ($184,600), Top 5% ($264,445), Top 1% ($649,005), and Top 0.1% ($3,305,375)", width = 200)
  ) + 
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x * 100),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_fill_manual(values = c(
    "Changes to Taxes" = "#2E86AB",
    "Changes to Medicaid" = "#C73E1D", 
    "Changes to SNAP" = "#FF9800"
  )) +
  # Ensure legend shows in logical order
  guides(fill = guide_legend(override.aes = list(size = 3)))






avg %>% 
  filter(scenario == 'senate_20250627') %>% 
  select(group, top_breakout, tax_change.atti, medicaid_change.atti, snap_change.atti, atti_change.atti) %>%
  pivot_longer(
    cols = c(tax_change.atti, medicaid_change.atti, snap_change.atti)
  ) %>% 
  
  # Create cleaner labels for the provisions
  mutate(
    name = case_when(
      name == "tax_change.atti" ~ "Changes to Taxes",
      name == "medicaid_change.atti" ~ "Changes to Medicaid",
      name == "snap_change.atti" ~ "Changes to SNAP",
      TRUE ~ name
    ),
    
    # Create faceting variable
    facet_group = ifelse(top_breakout, "Within Top Quintile", "Across Quintiles"),
    
    # Order the income groups properly
    group = factor(group, levels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", 
                                     "Top 10%", "Top 5%", "Top 1%", "Top 0.1%"))
  ) %>%
  ggplot(aes(x = group, y = value, fill = name)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 1) + 
  geom_point(aes(y = atti_change.atti), size = 9, shape = 21, fill = "white", color = "black", stroke = 1, show.legend = FALSE) + 
  
  geom_text(aes(y = atti_change.atti, label = sprintf("%.1f", atti_change.atti * 100)), 
            size = 2.8, color = "black") +
  facet_wrap(~facet_group, scales = "free_x", ncol = 2) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 11), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    legend.position = "top",
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray50"),
    strip.text = element_text(size = 12, face = "bold")
  ) + 
  labs(
    x = 'Adjusted Gross Income (AGI) Group', 
    y = element_blank(),
    fill = element_blank(),
    title = "Appendix Figure 1. Estimated Effects of Major Spending and Tax Provisions of the Senate Reconciliation Bill, 2026-2034",
    subtitle = "Average Annual Percent Change in After-Tax-And-Transfer Income",
    caption = str_wrap("Source: The Budget Lab and Congressional Budget Office calculations. Estimate universe is nondependent tax units, including nonfilers. Income percentile thresholds are calculated with respect to positive income only and are adult-weighted. After-tax-and-transfer income is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, inheritances, Medicaid benefits, and SNAP benefits. AGI percentile thresholds are calculated with respect to positive income only. AGI thresholds for 2025 are approximately: Quintile 2 ($13,350), Quintile 3 ($36,475), Quintile 4 ($64,995), Quintile 5 ($120,390), Top 10% ($184,600), Top 5% ($264,445), Top 1% ($649,005), and Top 0.1% ($3,305,375) Quintile 2 ($13,350), Quintile 3 ($36,475), Quintile 4 ($64,995), Quintile 5 ($120,390), Top 10% ($184,600), Top 5% ($264,445), Top 1% ($649,005), and Top 0.1% ($3,305,375)", width = 200)
  ) + 
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x * 100),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_fill_manual(values = c(
    "Changes to Taxes" = "#2E86AB",
    "Changes to Medicaid" = "#C73E1D", 
    "Changes to SNAP" = "#FF9800"
  )) +
  # Ensure legend shows in logical order
  guides(fill = guide_legend(override.aes = list(size = 3)))






