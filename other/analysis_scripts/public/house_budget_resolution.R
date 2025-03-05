#------------------------------------------------------------------------------
# Calculates figures for February 2025 blog "Illustrative Distributional 
# Effects of Policies Consistent with the House Concurrent Budget Resolution 
# for Fiscal Year 2025"
#------------------------------------------------------------------------------


library(tidyverse)
library(Hmisc)



# Read spending estimates
spending_estimates = read_csv('./resources/analysis_script_inputs/snap_medicaid.csv')


# Read baseline and reform data
baseline = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202502191709/baseline/static/detail/2026.csv')
reform   = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202502191709/tcja/static/detail/2026.csv')


# Process microdata for distribution table
microdata = baseline %>%
  
  # Remove dependent returns
  filter(dep_status == 0) %>%

  # Join reform data and calculate delta
  mutate(
    liab_baseline = liab_iit_net + liab_pr, 
    ati_baseline  = expanded_inc - liab_baseline
  ) %>%
  left_join(
    reform %>%
      mutate(
        liab = liab_iit_net + liab_pr, 
        ati  = expanded_inc - liab
      ) %>% 
      select(id, liab, ati),
    by = 'id'
  ) %>%
  mutate(
    delta_liab = liab - liab_baseline, 
    delta_ati  = ati  - ati_baseline
  )
  

# Calculate AGI-based income group thresholds
# Only consider non-negative AGI for threshold calculation
income_groups = wtd.quantile(
  x = microdata %>%
    filter(agi >= 0) %>%
    pull(agi),
  probs = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 0.999),
  weights = microdata %>%
    filter(agi >= 0) %>%
    pull(weight)
)

# Add income group classifications
microdata = microdata %>%
  mutate(
    agi_group = cut(
      x = agi,
      breaks = c(-Inf, 0, income_groups, Inf),
      labels = c(
        'Negative income', 'Bottom quintile', 'Second quintile',
        'Middle quintile', 'Fourth quintile', '80% - 90%',
        '90% - 99%', '99% - 99.9%', 'Top 0.1%'
      ),
      right = FALSE,
      include.lowest = TRUE
    )
  )

# Calculate distribution table metrics
dist_table = microdata %>%
  group_by(agi_group) %>%
  summarise(
    agi_cutoff       = round(min(agi) / 5) * 5,
    avg_change_tax   = round(weighted.mean(delta_liab, weight) / 5) * 5,
    ati_baseline_tax = sum(ati_baseline * weight) / 1e9, 
    ati_reform_tax   = sum(ati * weight) / 1e9,
    total_chg_tax    = ati_reform_tax - ati_baseline_tax 
  ) %>% 
  left_join(spending_estimates, by = 'agi_group') %>% 
  mutate(
    ati_baseline_all   = ati_baseline_tax + (medicaid_spending_2026_base    + snap_spending_2026_base)    / 1e3, 
    ati_reform_all     = ati_reform_tax   + (medicaid_spending_2026_housebr + snap_spending_2026_housebr) / 1e3, 
    total_chg_medicaid = (medicaid_spending_2026_housebr - medicaid_spending_2026_base) / 1e3,
    total_chg_snap     = (snap_spending_2026_housebr     - snap_spending_2026_base) / 1e3,
    
    pct_chg_ati_tax      = total_chg_tax / ati_baseline_all, 
    pct_chg_ati_medicaid = total_chg_medicaid / ati_baseline_all,
    pct_chg_ati_snap     = total_chg_snap / ati_baseline_all,
    pct_chg_ati_all      = ati_reform_all / ati_baseline_all - 1
  )

dist_table %>% write.csv()






income_group_order = c(
  'Negative income', 'Bottom quintile', 'Second quintile',
  'Middle quintile', 'Fourth quintile', '80% - 90%',
  '90% - 99%', '99% - 99.9%', 'Top 0.1%'
)

plot_data = dist_table %>% 
  mutate(agi_group = factor(agi_group, levels = income_group_order)) %>% 
  filter(agi_group != 'Negative income')

plot_data %>%
  select(agi_group, total = pct_chg_ati_all, 
         `Tax cuts` = pct_chg_ati_tax, 
         `Medicaid cuts` = pct_chg_ati_medicaid,
         `SNAP cuts` = pct_chg_ati_snap) %>% 
  pivot_longer(cols = contains(' ')) %>% 
  ggplot(aes(x = agi_group, y = value, fill = name)) +
  geom_col() +
  geom_point(aes(y = total), size = 4) + 
  theme_bw() + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'AGI group', y = element_blank(), fill = element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(-0.05, 0.03, 0.01)) +
  ggtitle('Percent change in disposable income, 2026')
  

