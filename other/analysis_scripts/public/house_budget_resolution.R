#------------------------------------------------------------------------------
# Calculates figures for February 2025 blog "Illustrative Distributional 
# Effects of Policies Consistent with the House Concurrent Budget Resolution 
# for Fiscal Year 2025"
#------------------------------------------------------------------------------

library(tidyverse)
library(Hmisc)


#-----------
# Read data
#-----------

# Read spending estimates from Safety-Net
spending_estimates = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Safety-Net/snap_medicaid_cy2026.csv')

# Read baseline and reform data
baseline = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202502191709/baseline/static/detail/2026.csv')
reform   = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202502191709/tcja/static/detail/2026.csv')

# Read estate tax microdata 
estate_tax_baseline = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Estate-Tax-Distribution/v1/2025031317/baseline/estate_tax_detail_2026.csv')
estate_tax_reform   = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Estate-Tax-Distribution/v1/2025031317/tcja_ext/estate_tax_detail_2026.csv')

# Read cost recovery ratios
cost_recovery_baseline = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Cost-Recovery-Simulator/v1/2025031013/baseline/totals/recovery_ratios_form.csv')
cost_recovery_reform   = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Cost-Recovery-Simulator/v1/2025031013/r&d/totals/recovery_ratios_form.csv')

# Net interest estimate in billions
# Source: https://taxfoundation.org/research/all/federal/tax-cuts-and-jobs-act-tcja-permanent-analysis/
net_interest_delta = -4.6


#-----------------
# Do calculations
#-----------------

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
      mutate(liab_reform = liab_iit_net + liab_pr) %>% 
      select(id, liab_reform),
    by = 'id'
  ) %>%
  mutate(delta_iit = liab_reform - liab_baseline) %>% 
  
  # Join estate tax data
  left_join(
    estate_tax_baseline %>% 
      rename(estate_tax_liability_baseline = estate_tax_liability), 
    by = 'id'
  ) %>% 
    left_join(
    estate_tax_reform %>% 
      rename(estate_tax_liability_reform = estate_tax_liability) %>% 
      select(id, estate_tax_liability_reform), 
    by = 'id'
  ) %>% 
  mutate(delta_estate = estate_tax_liability_reform - estate_tax_liability_baseline)


# Calculate NPV of cost recovery changes
cost_recovery_delta = cost_recovery_baseline %>% 
  filter(year == 2026) %>% 
  select(form, investment, pv_baseline = pv) %>%
  left_join(
    cost_recovery_reform %>%
      filter(year == 2026) %>%
      select(form, pv_reform = pv), 
    by = 'form'
  ) %>% 
  mutate(cost_recovery_delta = -1 * investment * (pv_reform - pv_baseline) * if_else(form == 'ccorp', 0.21, 0.25)) %>% 
  summarise(cost_recovery_delta = sum(cost_recovery_delta)) %>% 
  pull(cost_recovery_delta)


# Distribute business tax changes
microdata = microdata %>%
  mutate(
    
    # Calculate labor and capital income
    labor   = pmax(0, wages + (sole_prop + part_scorp + farm) * 0.8),
    capital = pmax(0, (sole_prop + part_scorp + farm) * 0.2 + txbl_int +
                       exempt_int + div_ord + div_pref + kg_st + kg_lt),
    
    # Distribute net interest delta using retroactive labor share assumptions
    corp_tax_labor   = net_interest_delta * 1e9 * 0 * (labor / sum(labor * weight)),
    corp_tax_capital = net_interest_delta * 1e9 * 1 * (capital / sum(capital * weight)),
    
    # Distribute cost recovery delta using prospective labor share assumptions
    cost_recovery_labor   = cost_recovery_delta * 1e9 * 0.5 * (labor / sum(labor * weight)),
    cost_recovery_capital = cost_recovery_delta * 1e9 * 0.5 * (capital / sum(capital * weight)), 
    
    delta_corp = corp_tax_labor + corp_tax_capital + cost_recovery_labor + cost_recovery_capital
  )



#--------------
# Build tables
#--------------

# Calculate AGI-based income group thresholds
# Only consider non-negative AGI for threshold calculation
income_groups = wtd.quantile(
  x = microdata %>%
    filter(agi >= 0) %>%
    pull(agi),
  probs = c(0.2, 0.4, 0.6, 0.8),
  weights = microdata %>%
    filter(agi >= 0) %>%
    pull(weight)
)

top_thresholds = wtd.quantile(
  x = microdata %>%
    filter(agi >= 0) %>%
    pull(agi),
  probs = c(0.9, 0.95, 0.99, 0.999),
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
        'Middle quintile', 'Fourth quintile', 'Top quintile'
      ),
      right = FALSE,
      include.lowest = TRUE
    )
  ) %>% 
  mutate(
    top10 = if_else(agi >= top_thresholds[1], 'Top 10%', NA), 
    top5  = if_else(agi >= top_thresholds[2], 'Top 5%', NA),
    top1  = if_else(agi >= top_thresholds[3], 'Top 1%', NA),
    top01 = if_else(agi >= top_thresholds[4], 'Top 0.1%', NA)
  )


# Calculate distribution table metrics
calc_dist = function(group_var) {
  if (group_var != 'agi_group') {
    microdata$agi_group = NULL
  }
  microdata %>%
    rename_with(~ 'agi_group', all_of(group_var)) %>%
    group_by(agi_group) %>%
    summarise(
      agi_cutoff         = round(min(agi) / 5) * 5,
      avg_delta_iit      = -weighted.mean(delta_iit, weight),
      avg_delta_estate   = -weighted.mean(delta_estate * p_inheritance, weight), 
      avg_delta_corp     = -weighted.mean(delta_corp, weight),
      avg_delta_tax      = avg_delta_iit + avg_delta_estate + avg_delta_corp,
      total_delta_iit    = -sum(delta_iit * weight) / 1e9,
      total_delta_estate = -sum(delta_estate * weight * p_inheritance) / 1e9,
      total_delta_corp   = -sum(delta_corp * weight) / 1e9,
      total_ati_baseline = sum(ati_baseline * weight) / 1e9,
    ) %>%  
    left_join(
      spending_estimates %>% 
        mutate(
          total_snap_baseline     = snap_spending_2026_base / 1e3, 
          total_snap_reform       = snap_spending_2026_housebr / 1e3, 
          total_delta_snap        = total_snap_reform - total_snap_baseline,
          total_medicaid_baseline = medicaid_spending_2026_base / 1e3, 
          total_medicaid_reform   = medicaid_spending_2026_housebr / 1e3,
          total_delta_medicaid    = total_medicaid_reform - total_medicaid_baseline,
        ) %>% 
        rename(avg_delta_snap = avg_chg_snap, avg_delta_medicaid = avg_chg_medicaid) %>% 
        mutate(avg_delta_spend = avg_delta_snap + avg_delta_medicaid) %>%
        select(agi_group = agi_group_label, starts_with('total_'), starts_with('avg_')), 
      by = 'agi_group'
    ) %>%
    mutate(
      
      avg_delta_reform = avg_delta_tax + avg_delta_spend, 
      
      # Get stacked disposable income totals by provision 
      disp_inc_baseline = total_ati_baseline + total_snap_baseline + total_medicaid_baseline, 
      disp_inc_reform   = disp_inc_baseline + total_delta_iit + total_delta_estate + total_delta_corp + total_delta_snap + total_delta_medicaid,
      
      # Express in percent change terms
      pct_chg_iit       = total_delta_iit      / disp_inc_baseline, 
      pct_chg_estate    = total_delta_estate   / disp_inc_baseline,
      pct_chg_corp      = total_delta_corp     / disp_inc_baseline,
      pct_chg_snap      = total_delta_snap     / disp_inc_baseline,
      pct_chg_medicaid  = total_delta_medicaid / disp_inc_baseline,
  
      pct_chg_tax    = (total_delta_iit + total_delta_estate + total_delta_corp) / disp_inc_baseline,
      pct_chg_spend  = (total_delta_snap + total_delta_medicaid) / disp_inc_baseline,
      pct_chg_reform = disp_inc_reform / disp_inc_baseline - 1,
      
      # Share of net change
      total_chg = disp_inc_reform - disp_inc_baseline
    ) %>% 
    return()
}

# Create website table
dist_table = calc_dist('agi_group') %>% 
  filter(agi_group != 'Negative income') %>%
  bind_rows(
    calc_dist('top10'), 
    calc_dist('top5'), 
    calc_dist('top1'), 
    calc_dist('top01'), 
  ) %>% 
  filter(!is.na(agi_group))

dist_table %>% 
  mutate(
    avg_delta_reform = round(avg_delta_reform / 5) * 5, 
    pct_chg_reform   = round(pct_chg_reform, 3),
    share_chg        = round(total_chg / sum(total_chg[1:5]), 3)
  ) %>% 
  select(-total_chg) %>% 
  write.csv()


#-------------
# Create plot
#-------------

income_group_order = c(
  'Bottom quintile', 'Second quintile', 'Middle quintile', 'Fourth quintile', 'Top quintile'
)

dist_table %>%
  slice(1:5) %>% 
  mutate(agi_group = factor(agi_group, levels = income_group_order)) %>% 
  select(agi_group, total = pct_chg_reform, `Changes to Taxes` = pct_chg_tax, `Changes to Spending` = pct_chg_spend) %>% 
  pivot_longer(cols = contains(' ')) %>% 
  ggplot(aes(x = agi_group, y = value, fill = name)) +
  geom_col() +
  geom_point(aes(y = total), size = 5, show.legend = F) + 
  theme_minimal() + 
  geom_hline(yintercept = 0) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    legend.position = "top",
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)  # This aligns the caption to the left
  ) + 
  labs(
    x = 'AGI Group', 
    y = element_blank(), 
    fill = element_blank(),
    caption = "Source: The Budget Lab calculations"
  ) + 
  scale_y_continuous(
    labels = scales::percent_format(), 
    breaks = seq(-0.05, 0.03, 0.01)
  ) +
  scale_fill_brewer(palette = 'Set1') + 
  ggtitle(
    'Figure 1. Effects of Selected Policies Consistent with House Budget Resolution, 2026', 
    subtitle = 'Percent Change in After-Tax-And-Transfer Income'
  )
