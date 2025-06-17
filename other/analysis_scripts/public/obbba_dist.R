#------------------------------------------------------------------------------
# Calculates figures for February 2025 blog "Illustrative Distributional 
# Effects of Policies Consistent with the House Concurrent Budget Resolution 
# for Fiscal Year 2025"
#------------------------------------------------------------------------------

library(tidyverse)

scenarios = c('08_cost_recovery', '09_tariffs')
output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Simulator/v1/202506101301'

tax = scenarios %>% 
  map(
    .f = ~ {
      output_root %>% 
        file.path(.x, '/static/supplemental/distribution.csv') %>% 
        read_csv(show_col_types = F) %>% 
        mutate(tax_scenario = .x)
    }
  ) %>% 
  bind_rows() %>% 
  filter(
    year == 2028, 
    taxes_included == 'indirect_iit_pr_estate_cit', 
    indirect_tax_assumption == 'consumption',
    group_dimension == 'AGI', 
    str_sub(group, 1, 1) == 'Q' 
  ) %>% 
  select(group, tax_scenario, starts_with('ati_'), avg_chg = avg) %>% 
  mutate(
    avg_chg = -avg_chg,
    total_chg = ati_reform - ati_baseline, 
    group = case_when(
      group == 'Quintile 1' ~ 'Bottom quintile',
      group == 'Quintile 2' ~ 'Second quintile', 
      group == 'Quintile 3' ~ 'Middle quintile', 
      group == 'Quintile 4' ~ 'Fourth quintile', 
      group == 'Quintile 5' ~ 'Top quintile'
    ) 
  ) %>% 
  pivot_longer(cols = -c(group, tax_scenario)) %>% 
  pivot_wider(names_from = tax_scenario) %>% 
  mutate(obbba_tax = `08_cost_recovery`, tariffs = `09_tariffs` - obbba_tax) %>% 
  select(-`08_cost_recovery`, -`09_tariffs`) %>% 
  pivot_wider(values_from = c(obbba_tax, tariffs), names_sep = '.') %>% 
  select(group, ati_baseline = obbba_tax.ati_baseline, obbba_tax.total_chg, obbba_tax.avg_chg, 
         tariffs.total_chg, tariffs.avg_chg)
  

spending = c('cbo', 'low', 'hi') %>% 
  map(
    .f = ~ paste0('./resources/analysis_script_inputs/202506/snap_medicaid_cy2028_', .x, '.csv') %>% 
      read_csv() %>% 
      mutate(assumption = .x)
  ) %>%
  bind_rows() %>% 
  filter(agi_group %in% 1:5) %>% 
  mutate(
    snap_baseline = snap_spending_2028_base / 1e3, 
    medicaid_baseline = medicaid_spending_2028_base / 1e3,
    obbba_spending.total_chg = (snap_spending_2028_housebr     - snap_spending_2028_base) / 1e3 + 
                               (medicaid_spending_2028_housebr - medicaid_spending_2028_housebr) / 1e3, 
    obbba_spending.avg_chg = avg_chg_snap + avg_chg_medicaid
  ) %>% 
  select(assumption, group = agi_group_label, 
         snap_baseline, medicaid_baseline, 
         obbba_spending.total_chg, obbba_spending.avg_chg)
  
  
plot_data = spending %>% 
  left_join(tax, by = 'group') %>% 
  mutate(atti_baseline = ati_baseline + snap_baseline + medicaid_baseline) %>% 
  select(assumption, group, atti_baseline, contains('.')) %>% 
  mutate(
    obbba_tax.pct_chg_ati = obbba_tax.total_chg / atti_baseline, 
    tariffs.pct_chg_ati = tariffs.total_chg / atti_baseline, 
    obbba_spending.pct_chg_ati = obbba_spending.total_chg / atti_baseline, 
  ) %>% 
  select(-contains('total'), -atti_baseline) %>% 
  pivot_longer(
    cols = contains('.'), 
    names_sep = '[.]', 
    names_to = c('provision', 'metric')
  ) %>% 
  mutate(
    provision = case_when(
      provision == 'obbba_spending' ~ '1) Certain Medicaid and SNAP provisions in the OBBBA', 
      provision == 'obbba_tax'      ~ '2) Certain individual, business, and estate tax provisions in the OBBBA', 
      provision == 'tariffs'        ~ '3) Tariffs as of June 1st, 2025', 
    )
  ) %>% 
  group_by(assumption, group, metric) %>% 
  mutate(total = sum(value)) %>% 
  ungroup()


income_group_order = c(
  'Bottom quintile', 'Second quintile', 'Middle quintile', 'Fourth quintile', 'Top quintile'
)


plot_data %>% 
  filter(metric == 'pct_chg_ati', assumption == 'cbo') %>% 
  ggplot(aes(x = factor(group, levels = income_group_order), y = value, fill = provision)) +
  geom_col() +
  geom_hline(yintercept = 0) + 
  geom_point(aes(y = total), size = 5, show.legend = F) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 10),  
    legend.position = "top",
    legend.key.size = unit(0.5, "cm"),  
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)  
  ) + 
  labs(
    x = 'AGI Quintile', 
    y = element_blank(), 
    fill = element_blank(),
    caption = "Source: The Budget Lab calculations"
  ) + 
  scale_y_continuous(
    labels = scales::percent_format(), 
    breaks = seq(-0.05, 0.03, 0.01)
  ) +
  scale_fill_brewer(palette = 'Set1') + 
  guides(fill = guide_legend(ncol = 1)) + 
  ggtitle(
    'Figure 1. Effects of Selected Provisions of the OBBBA and Tariffs, 2028', 
    subtitle = 'Percent Change in After-Tax-And-Transfer Income'
  )
          