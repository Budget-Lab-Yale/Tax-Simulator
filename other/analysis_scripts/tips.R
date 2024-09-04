#-----------------------------------------------
# Script to do post-processing calculations for 
# report on "No Tax On Tips"
#-----------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)

# Set roots
tax_data_root = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024090319/baseline'
output_root   = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202409032008'
sipp_root     = '/gpfs/gibbs/project/sarin/shared/raw_data/SIPP/'


#-----------
# Load data
#-----------

# Define function to read simulator output
get_data = function(scenario, year) {
  output_root %>% 
    file.path(scenario, 'static/detail/', paste0(year, '.csv')) %>% 
    fread() %>% 
    tibble() %>%
    filter(dep_status == 0) %>% 
    left_join(
      tax_data_root %>% 
        paste0('/tax_units_', year, '.csv') %>% 
        fread() %>% 
        tibble() %>% 
        select(id, tips, tips1, tips2, tips_lh1, tips_lh2), 
      by = 'id'
    ) %>% 
    mutate(scenario = scenario, year = year) %>% 
    return()
} 

# Read Tax-Simulator data 
microdata = bind_rows(
  get_data('baseline', 2025), 
  get_data('income_tax_lh', 2025), 
  get_data('income_tax', 2025), 
  get_data('payroll_tax', 2025), 
  get_data('baseline', 2026),
  get_data('income_tax_lh', 2026), 
  get_data('income_tax', 2026), 
  get_data('payroll_tax', 2026)
)

# Load and clean up SIPP data
sipp = sipp_root %>% 
  file.path('/tip_ind_occ_full_split.csv') %>%
  fread() %>%
  tibble() %>% 
  
  # Filter to nondependent wage workers 
  filter(!is_dep, inc_wages > 0) %>% 
  mutate(
    year      = year - 1, 
    tipped    = as.integer(inc_wages_tips > 0),
    tip_share = inc_wages_tips / inc_wages,
    married   = as.integer(!is.na(marriage) & marriage < 3),
  ) %>% 
  
  # Assign wage decile 
  group_by(year) %>% 
  mutate(
    wage_decile = cut(
      x      = inc_wages,
      breaks = c(0, wtd.quantile(inc_wages, weight, seq(0.1, 0.9, 0.1)), Inf), 
      labels = F
    )
  ) %>% 
  ungroup() %>% 
  
  # Clean up and only keep the variables we want  
  select(
    u_ID, year, weight, age, n_dep, married, wages = inc_wages, wage_decile,
    tips = inc_wages_tips, tip_share,
    ind_1_code  = ind_1, 
    ind_2_code  = ind_2, 
    ind_3_code  = ind_3, 
    ind_4_code  = ind_4, 
    ind_1_wages = earn_wages_i_1,
    ind_2_wages = earn_wages_i_2,
    ind_3_wages = earn_wages_i_3,
    ind_4_wages = earn_wages_i_4,
    ind_1_tips  = earn_tip_wages_i_1,
    ind_2_tips  = earn_tip_wages_i_2,
    ind_3_tips  = earn_tip_wages_i_3,
    ind_4_tips  = earn_tip_wages_i_4,
    occ_1_code  = occ_1, 
    occ_2_code  = occ_2, 
    occ_3_code  = occ_3, 
    occ_4_code  = occ_4, 
    occ_1_wages = earn_wages_o_1,
    occ_2_wages = earn_wages_o_2,
    occ_3_wages = earn_wages_o_3,
    occ_4_wages = earn_wages_o_4,
    occ_1_tips  = earn_tip_wages_o_1,
    occ_2_tips  = earn_tip_wages_o_2,
    occ_3_tips  = earn_tip_wages_o_3,
    occ_4_tips  = earn_tip_wages_o_4
  )
  
# Create industry-level dataset 
sipp_ind = sipp %>% 
  select(u_ID, year, weight, starts_with('ind_')) %>% 
  pivot_longer(
    cols         = -c(u_ID, year, weight), 
    names_prefix = 'ind_',
    names_sep    = '_',
    names_to     = c('index', 'name')
  ) %>% 
  pivot_wider() %>% 
  filter(!is.na(wages)) %>%
  rename(ind = code) %>% 
  left_join(read_csv('../Tax-Data/resources/industry.csv'), by = 'ind') 

# Create occupation-level dataset 
sipp_occ = sipp %>% 
  select(u_ID, year, weight, starts_with('occ_')) %>% 
  pivot_longer(
    cols         = -c(u_ID, year, weight), 
    names_prefix = 'occ_',
    names_sep    = '_',
    names_to     = c('index', 'name')
  ) %>% 
  pivot_wider() %>% 
  filter(!is.na(wages)) %>%
  rename(occ = code) %>% 
  left_join(read_csv('../Tax-Data/resources/occupation.csv'), by = 'occ') 



#-----------------------------------
# Characteristics of tipped workers
#-----------------------------------

# Initialize list for holding data tables underlying tables and figures
data_files = list()

# Figure 1) Wage, age, and marital status
data_files$fig1a = sipp %>%
  group_by(`Wage decile` = wage_decile) %>% 
  summarise(
    Unmarried = weighted.mean(tips > 0, weight * !married),
    Married   = weighted.mean(tips > 0, weight * married), 
    .groups = 'drop'
  )

data_files$fig1a %>% 
  pivot_longer(-`Wage decile`) %>% 
  ggplot(aes(x = `Wage decile`, y = value, colour = name)) + 
  geom_point(size = 3) +
  geom_line() + 
  geom_hline(yintercept = 0) + 
  theme_bw() +
  labs(
    y = element_blank(), 
    colour = element_blank() 
  ) + 
  scale_x_continuous(breaks = 1:10) + 
  scale_y_continuous(breaks = seq(0, 6) / 100, labels = scales::percent_format()) + 
  theme(legend.position = 'top') + 
  ggtitle('By Wage Decile')

data_files$fig1b = sipp %>%
  filter(age >= 18) %>% 
  group_by(
    `Age group` = case_when(
      age < 25 ~ '18-24',
      age < 35 ~ '25-34',
      age < 45 ~ '35-44',
      age < 55 ~ '45-54',
      age < 65 ~ '55-64',
      T        ~ '65+'
    ), 
  ) %>% 
  summarise(
    Unmarried = weighted.mean(tips > 0, weight * !married),
    Married = weighted.mean(tips > 0, weight * married),
    .groups = 'drop'
  ) 

data_files$fig1b %>% 
  pivot_longer(-`Age group`) %>% 
  ggplot(aes(x = `Age group`, y = value, colour = name)) + 
  geom_point(size = 3) +
  geom_line(aes(x = as.integer(as.factor(`Age group`)))) + 
  geom_hline(yintercept = 0) + 
  theme_bw() +
  labs(
    x      = 'Age group', 
    y      = element_blank(), 
    colour = element_blank() 
  ) + 
  scale_y_continuous(breaks = seq(0, 6) / 100, labels = scales::percent_format()) + 
  theme(legend.position = 'top') + 
  ggtitle('By Age Group')


# Tip share of earnings calculations
sipp %>% 
  filter(tips > 0) %>% 
  reframe(
    pctile = seq(0, 100, 1),
    value  = wtd.quantile(tip_share, weight, probs = seq(0, 1, 0.01))
  ) %>% 
  print(n = 100)

sipp %>% 
  filter(
    tips > 0 & !is.na(occ_1_code) & is.na(occ_2_code) & is.na(occ_3_code) & is.na(occ_4_code)  
  ) %>% 
  reframe(
    pctile = seq(0, 100, 1),
    value  = wtd.quantile(tip_share, weight, probs = seq(0, 1, 0.01))
  ) %>% 
  print(n = 100)


# Figure 2) Industry/occupation breakdown 
data_files$fig2a = sipp_ind %>%
  filter(tips > 0, year >= 2021) %>% 
  group_by(Industry = ind_description) %>% 
  summarise(`Share of tips` = sum(weight * tips)) %>% 
  mutate(`Share of tips` = `Share of tips` / sum(`Share of tips`)) %>%  
  arrange(-`Share of tips`) %>% 
  head(10)

data_files$fig2a %>%
  mutate(rank = row_number()) %>% 
  mutate(label = scales::percent(`Share of tips`, accuracy = 0.1)) %>% 
  ggplot(aes(x = fct_reorder(Industry, -rank), y = `Share of tips`)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = label), position = position_dodge(width = 1), hjust = -0.3, size = 3) + 
  coord_flip() + 
  theme_bw() + 
  labs(x = element_blank(), y = 'Share of total tips') + 
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 6, 0.1), labels = scales::percent_format()) + 
  ggtitle('By Industry')

data_files$fig2b = sipp_occ %>%
  filter(year >= 2021) %>% 
  group_by(Industry = occ_description) %>% 
  summarise(`Share of tips` = sum(weight * tips)) %>% 
  mutate(`Share of tips` = `Share of tips` / sum(`Share of tips`)) %>%  
  arrange(-`Share of tips`) %>% 
  head(10)

data_files$fig2b %>%
  mutate(rank = row_number()) %>% 
  mutate(label = scales::percent(`Share of tips`, accuracy = 0.1)) %>% 
  ggplot(aes(x = fct_reorder(Industry, -rank), y = `Share of tips`)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = label), position = position_dodge(width = 1), hjust = -0.3, size = 3) + 
  coord_flip() + 
  theme_bw() + 
  labs(x = element_blank(), y = 'Share of total tips') + 
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 3, 0.05), labels = scales::percent_format()) + 
  ggtitle('By Occupation')


# Figure 3) Courier growth
data_files$fig3 = sipp_ind %>% 
  mutate(
    food_service = ind == 8680 | ind == 8690,
    courier      = ind == 6380, 
    other        = !food_service & !courier
  ) %>% 
  group_by(Year = year) %>% 
  summarise(
    `Food services and bars` = weighted.mean(food_service, tips * weight),
    `Courier services` = weighted.mean(courier, tips * weight),
    `Other`            = weighted.mean(other, tips * weight)
  )

data_files$fig3 %>% 
  pivot_longer(
    cols      = -Year, 
    names_to  = 'Industry'
  ) %>% 
  ggplot(aes(x = Year, y = value, fill = Industry)) + 
  geom_col() + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(2017, 2022, 1)) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(x = element_blank(), y = element_blank())


# Self-employment share of tips
sipp_root %>% 
  file.path('/tip_ind_occ_full_split.csv') %>%
  fread() %>%
  tibble() %>% 
  summarise(
    self  = sum(weight * inc_self_tips), 
    wages = sum(weight * inc_wages_tips)
  ) %>% 
  mutate(share_self = self / (self + wages))


#-----------------------------------------------
# Calculate distributional metrics under reform
#-----------------------------------------------

# Calculate deltas from baseline for each scenario
deltas = microdata %>% 
  mutate(liab = liab_iit_net + liab_pr) %>% 
  select(id, scenario, year, liab) %>% 
  pivot_wider(
    names_from  = scenario,
    values_from = liab
  ) %>% 
  mutate(
    income_tax_lh = income_tax_lh  - baseline, 
    income_tax    = income_tax  - baseline, 
    payroll_tax   = payroll_tax - baseline, 
  )


# Calculate income quintiles
quintiles = microdata %>% 
  filter(scenario == 'baseline') %>% 
  group_by(year) %>% 
  mutate(
    person_weight = weight * (1 + (filing_status == 2)), 
    quintile = cut(
      x = expanded_inc, 
      breaks = c(wtd.quantile(
        x = expanded_inc[expanded_inc > 0], 
        weight = person_weight[expanded_inc > 0], 
        probs = seq(0, 0.8, 0.2)
      ), Inf), 
      labels = 1:5
    )
  ) %>% 
  ungroup() %>% 
  mutate(ati = expanded_inc - liab_iit_net - liab_pr) %>% 
  select(year, id, weight, expanded_inc, quintile, ati, agi, tips)  
  

# Calculate distributional metrics by income 
quintile_metrics = quintiles %>% 
  filter(!is.na(quintile)) %>% 
  left_join(deltas, by = c('year', 'id')) %>%
  pivot_longer(
    cols      = c(income_tax_lh, income_tax, payroll_tax), 
    names_to  = 'scenario', 
    values_to = 'delta'
  ) %>% 
  group_by(scenario, year, quintile) %>% 
  summarise(
    `Share with tipped income`              = weighted.mean(tips > 0, weight),
    `Number with tipped income`             = sum((tips > 0) * weight),
    `Average tax change`                    = weighted.mean(delta, weight),
    `Percent change in after-tax income`    = weighted.mean(-delta / ati, weight * ati), 
    `Share with tax cut`                    = sum((delta < 0) * weight) / sum(weight), 
    `Share of tipped families with tax cut` = sum((delta < 0) * weight * (tips > 0)) / sum(weight * (tips > 0)),
    `Average tax cut`                       = -weighted.mean(delta, weight * (delta < -5)), 
    .groups = 'drop'
  )

# Calculate "distributional" metrics for whole population
total_metrics = quintiles %>% 
  filter(!is.na(quintile)) %>% 
  left_join(deltas, by = c('year', 'id')) %>%
  pivot_longer(
    cols      = c(income_tax_lh, income_tax, payroll_tax), 
    names_to  = 'scenario', 
    values_to = 'delta'
  ) %>% 
  group_by(scenario, year) %>% 
  summarise(
    `Share with tipped income`              = weighted.mean(tips > 0, weight), 
    `Number with tipped income`             = sum((tips > 0) * weight),
    `Share with tax cut`                    = sum((delta < 0) * weight) / sum(weight), 
    `Share of tipped families with tax cut` = sum((delta < 0) * weight * (tips > 0)) / sum(weight * (tips > 0)),
    `Average tax cut`                       = -weighted.mean(delta, weight * (delta < -5)), 
    .groups = 'drop'
  )


# Define function to visualize metrics
get_chart = function(yr, metric) {
  quintile_metrics %>%
    filter(year == yr) %>% 
    mutate(
      scenario = case_when(
        scenario == 'income_tax_lh' ~ '1) Income tax deducion for tips earned in specified leisure and hospitality industries',
        scenario == 'income_tax'    ~ '2) Income tax deduction', 
        scenario == 'payroll_tax'   ~ '3) Income tax deduction and payroll tax exemption'
      )
    ) %>% 
    pivot_longer(cols = contains(' ')) %>% 
    filter(name == metric) %>% 
    ggplot(aes(x = as.integer(quintile), y = value, fill = scenario)) + 
    geom_col(position = 'dodge') + 
    geom_hline(yintercept = 0) + 
    labs(
      x = 'Income quintile', 
      y = element_blank(), 
      fill = element_blank()
    ) + 
    theme_bw() + 
    theme(legend.position = 'top') + 
    guides(fill = guide_legend(ncol = 1)) 
}


# Figure 4) share of tax units with tipped income
data_files$fig4 = quintile_metrics %>% 
  filter(year == 2025, scenario == 'income_tax_lh') %>% 
  select(quintile, `Share with tipped income`)
  
data_files$fig4 %>% 
  ggplot(aes(x = as.integer(quintile), y = `Share with tipped income`)) + 
  geom_line() +
  geom_point(size = 4) + 
  geom_hline(yintercept = 0) + 
  labs(
    x = 'Income quintile', 
    y = element_blank(), 
    fill = element_blank()
  ) + 
  theme_bw() + 
  theme(legend.position = 'top') + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = scales::percent_format()) 

# Figure 5) Share of tipped families with tax cut
data_files$fig5 = quintile_metrics %>% 
  filter(year == 2025) %>% 
  select(quintile, name = scenario, value = `Share of tipped families with tax cut`) %>% 
  pivot_wider()
  
get_chart(2025, 'Share of tipped families with tax cut') +
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_text(
    aes(label = paste0(round(value, 2) * 100, '%')), 
    vjust = 1.5, position = position_dodge(0.9), size = 3
  )

# Figure 6) Average tax cut
data_files$fig6 = quintile_metrics %>% 
  filter(year == 2025) %>% 
  select(quintile, name = scenario, value = `Average tax cut`) %>% 
  pivot_wider()

get_chart(2025, 'Average tax cut') +
  scale_y_continuous(labels = scales::dollar_format()) + 
  geom_text(
    aes(label = paste0('$', round(value / 10) * 10)), 
    vjust = 1.5, position = position_dodge(0.9), size = 3
  )

# Figure 7) Percent change in after-tax income
data_files$fig7 = quintile_metrics %>% 
  filter(year == 2025) %>% 
  select(quintile, name = scenario, value = `Percent change in after-tax income`) %>% 
  pivot_wider()

get_chart(2025, 'Percent change in after-tax income') +
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_text(
    aes(label = paste0(round(value, 4) * 100, '%')), 
    vjust = 1.5, position = position_dodge(0.9), size = 3
  )

# Figure 8) 2025 vs 2026
data_files$fig8 = quintile_metrics %>%
  filter(scenario == 'income_tax') %>%
  select(quintile, name = year, value = `Percent change in after-tax income`) %>% 
  pivot_wider()

quintile_metrics %>%
  filter(scenario == 'income_tax') %>% 
  ggplot(aes(x = as.integer(quintile), y = `Percent change in after-tax income`, fill = as.factor(year))) + 
  geom_col(position = 'dodge') + 
  geom_hline(yintercept = 0) + 
  labs(
    x = 'Income quintile', 
    y = element_blank(), 
    fill = element_blank()
  ) + 
  theme_bw() + 
  theme(legend.position = 'top') + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_fill_manual(values = c('gray', '#768DD1')) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_text(
    aes(label = paste0(round(`Percent change in after-tax income`, 4) * 100, '%')), 
    vjust = 1.5, position = position_dodge(0.9), size = 3
  )



#----------------------------------
# Behavioral feedback calculations
#----------------------------------

#--------------------
# Shifting scenarios
#--------------------

# Create crosswalk for major industry groupings
industry_categories = sipp_ind %>% 
  distinct(ind) %>% 
  arrange(ind) %>%  
  mutate(
    category = case_when(
      ind <= 290  ~ 'Agriculture, Forestry, Fishing, and Hunting', 
      ind <= 490  ~ 'Mining',
      ind <= 690  ~ 'Utilities',
      ind <= 770  ~ 'Construction',
      ind <= 3990 ~ 'Manufacturing',
      ind <= 4590 ~ 'Wholesale Trade',
      ind <= 5790 ~ 'Retail Trade',
      ind <= 6390 ~ 'Transportation and Warehousing',
      ind <= 6780 ~ 'Information',
      ind <= 6992 ~ 'Finance and Insurance',
      ind <= 7190 ~ 'Real Estate and Rental and Leasing',
      ind <= 7490 ~ 'Professional, Scientific, and Technical Services',
      ind <= 7570 ~ 'Management of Companies and Enterprises', 
      ind <= 7790 ~ 'Administrative and Support and Waste Management Services',
      ind <= 7890 ~ 'Educational Services',
      ind <= 8470 ~ 'Healthcare and Social Assistance',
      ind <= 8590 ~ 'Arts, Entertainment, and Recreation',
      ind <= 8690 ~ 'Accommodation and Food Service',
      ind <= 9290 ~ 'Other Services',
      ind <= 9590 ~ 'Public Administration',
      T           ~ 'Military'
    )
  )

# Create crosswalk for major occupation groupings
occupation_categories = sipp_occ %>%
  distinct(occ) %>% 
  arrange(occ) %>%  
  mutate(
    category = case_when(
      occ <= 430  ~ 'Management', 
      occ <= 960  ~ 'Business and Financial Operations', 
      occ <= 1240 ~ 'Computer and Mathematical', 
      occ <= 1569 ~ 'Architecture and Engineering', 
      occ <= 1980 ~ 'Life, Physical, and Social Science', 
      occ <= 2060 ~ 'Community and Social Services', 
      occ <= 2180 ~ 'Legal', 
      occ <= 2555 ~ 'Education Instruction and Library', 
      occ <= 2970 ~ 'Arts, Design, Entertainment, Sports, and Media', 
      occ <= 3550 ~ 'Healthcare Practitioners and Technical', 
      occ <= 3655 ~ 'Healthcare Support',
      occ <= 3960 ~ 'Protective Service', 
      occ <= 4160 ~ 'Food Preparation and Serving',
      occ <= 4255 ~ 'Building and Grounds Cleaning and Maintenance', 
      occ <= 4655 ~ 'Personal Care and Service', 
      occ <= 4965 ~ 'Sales',
      occ <= 5940 ~ 'Office and Administrative Support', 
      occ <= 6130 ~ 'Farming, Fishing, and Forestry',
      occ <= 6950 ~ 'Construction Trades',
      occ <= 7640 ~ 'Installation, Maintenance, and Repair',
      occ <= 8990 ~ 'Production',
      occ <= 9760 ~ 'Transportation and Material Moving', 
      T           ~ 'Military'
    )
  )


# Define function to do tipping norm scenario described in text 
do_shifting_scenario = function(df, group_var, group_crosswalk, min_tip_share) { 
  
  # Detailed category metrics
  unit_totals = df %>% 
    group_by(!!sym(group_var)) %>%
    summarise(
      n_records        = n(),
      n_records_tipped = sum((tips > 0)),
      employment       = sum(weight * (wages > 0)),
      tipped_share     = weighted.mean(tips > 0, weight * (wages > 0)), 
      tips             = sum(tips * weight) / 1e9,
      wages            = sum(wages * weight) / 1e9,
      tip_share_wages  = sum(tips * weight) / sum(wages * weight), 
      .groups = 'drop'
    ) %>% 
    left_join(group_crosswalk, by = group_var) %>% 
    filter(tip_share_wages >= min_tip_share) %>% 
    group_by(category) %>% 
    mutate(
      target_max   = max(tip_share_wages), 
      target_avg   = weighted.mean(tip_share_wages, employment), 
      distance_max = target_max - tip_share_wages, 
      distance_avg = pmax(0, target_avg - tip_share_wages)
    )
  
  # Major category metrics
  group_totals = unit_totals %>% 
    summarise(
      `Group average` = mean(target_avg),
      `Group maximum` = mean(target_max)
    ) %>% 
    arrange(-`Group average`)
  
  # Total percent change in tips
  totals = unit_totals %>%
    ungroup() %>% 
    summarise(
      `Group average` = sum(wages * distance_avg) / sum(tips),
      `Group maximum` = sum(wages * distance_max) / sum(tips)
    )
  
  # Contribution to total change by major category 
  contributions = unit_totals %>% 
    summarise(
      max  = sum(wages * distance_max),
      avg  = sum(wages * distance_avg), 
      tips = sum(tips)
    ) %>% 
    mutate(
      contribution_max = max / sum(max), 
      contribution_avg = avg / sum(avg)
    ) %>% 
    arrange(-contribution_max)
  
  return(
    list(
      unit_totals   = unit_totals,
      group_totals  = group_totals, 
      totals        = totals,
      contributions = contributions
    )
  )
}


# Table 3) tip shares by major category  
data_files$tab3a = do_shifting_scenario(sipp_ind, 'ind', industry_categories, 0.01)$group_totals
data_files$tab3b = do_shifting_scenario(sipp_occ, 'occ', occupation_categories, 0.01)$group_totals

# Figure 9) estimated change in tips by assumptions
shifting_scenarios = seq(0, 0.05, 0.01) %>% 
  map(
    .f = ~ bind_rows(
      do_shifting_scenario(sipp_ind, 'ind', industry_categories, .x)$total %>% 
        mutate(`Minimum tip share` = .x, type = 'By industry'), 
      do_shifting_scenario(sipp_occ, 'occ', occupation_categories, .x)$total %>% 
        mutate(`Minimum tip share` = .x, type = 'By occupation')
    )
  ) %>% 
  bind_rows()

shifting_scenarios %>% 
  pivot_longer(cols = starts_with('Group'), names_to = 'Target tip share') %>% 
  ggplot(aes(x = `Minimum tip share`, y = value, colour = `Target tip share`)) + 
  geom_line() + 
  geom_point(size = 3) + 
  facet_wrap(~type) + 
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_text(
    aes(label = paste0(round(value, 2) * 100, '%')), 
    vjust = -1.5, size = 3
  ) + 
  theme_bw()
  

data_files$fig9 = shifting_scenarios %>% 
  arrange(type) %>% 
  select(type, `Minimum tip share`, everything())


#-------------------------------
# 1) fringe benefits elasticity
#-------------------------------

# Get projected tipped share of workers
share_tipped = microdata %>% 
  filter(scenario == 'baseline', year == 2026) %>% 
  summarise(
    share_tipped = sum(((tips1 > 0) + (tips2 > 0)) * weight) / sum(((wages1 > 0) + (wages2 > 0)) * weight)
  ) %>% 
  deframe()

# Calculate average marginal tax rates on tips for tipped workers and all workers
mtr = microdata %>% 
  filter(scenario %in% c('baseline', 'income_tax'), year == 2026) %>% 
  select(scenario, id, weight, mtr_1 = mtr_tips1, mtr_2 = mtr_tips2, wages_1 = wages1, wages_2 = wages2, tips_1 = tips1, tips_2 = tips2) %>% 
  pivot_longer(
    cols      = -c(scenario, id, weight), 
    names_sep = '_', 
    names_to  = c('name', 'index')
  ) %>% 
  pivot_wider() %>%
  group_by(scenario) %>% 
  summarise(
    all    = weighted.mean(mtr, wages * weight),
    tipped = weighted.mean(mtr, wages * weight * (tips > 0))
  ) 


mtr_baseline = mtr %>% filter(scenario == 'baseline')
mtr_policy   = mtr %>% filter(scenario == 'income_tax')

# Set elasticities 
extensive_e     = -0.357
extensive_e_alt = -0.357 / 0.91
intensive_e     = -1.051


# Calculate change in tax price for all and tipped workers
chg_tax_price        = mtr_policy$all - mtr_baseline$all
chg_tax_price_tipped = mtr_policy$tipped - mtr_baseline$tipped

# Calculate implied change in tipped workers
pp_chg_tipped    = extensive_e * chg_tax_price 
pct_chg_tipped   = pp_chg_tipped / share_tipped
new_share_tipped = share_tipped + pp_chg_tipped

# Calculate implied change in tipped workers using alternative functional form 
pct_chg_tipped_alt     = extensive_e_alt * chg_tax_price  

# Calculate intensive margin response
pct_chg_tips_intensive = exp(intensive_e * chg_tax_price_tipped) - 1
pct_chg_tips           = c(pct_chg_tipped_alt, pct_chg_tipped) + pct_chg_tips_intensive


#------------------------
# Write output data file
#------------------------

writexl::write_xlsx(data_files, path = file.path(output_root, 'tips_data.xlsx'))
  
  

