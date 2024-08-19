library(tidyverse)
library(data.table)
library(Hmisc)

# Set roots
tax_data_root = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024081317/baseline'
output_root   = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202408131926'
sipp_root     = '/gpfs/gibbs/project/sarin/shared/raw_data/SIPP/'


#-----------
# Load data
#-----------

# Define data-reading function
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
        select(id, tips, tips1, tips2), 
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
  file.path('tip_ind_full.csv') %>%
  fread() %>%
  tibble() %>% 
  filter(inc_earn > 0, !is_dep) %>% 
  group_by(year) %>% 
  mutate(
    year = year - 1, 
    parent  = as.integer(n_dep > 0), 
    married = as.integer(!is.na(marriage) & marriage < 3), 
    wage_decile = cut(
      x = inc_earn,
      breaks = c(0, wtd.quantile(inc_earn, weight, seq(0.1, 0.9, 0.1)), Inf), 
      labels = F
    )
  ) %>% 
  ungroup() %>% 
  rename(tip_share = tips_pct, wages = inc_earn, tips = inc_tip) 


sipp_industry = sipp %>% 
  select(u_ID, year, weight, wages, tip_share, starts_with('ind_'), starts_with('tips_')) %>% 
  pivot_longer(
    cols = -c(u_ID, year, weight, wages, tip_share), 
    names_sep = '_', 
    names_to = c('name', 'index')
  ) %>% 
  pivot_wider() %>% 
  filter(!is.na(ind)) 

sipp_industry %>% 
  mutate(
    restaurants_bars = ind == 8680 | ind == 8690,
    taxi             = ind == 6190,
    beauty_hair      = ind %in% c(8970, 8980, 8990),
    courier          = ind == 6380, 
    gambling_rec     = ind == 8590,
    other            = !restaurants_bars & !taxi & !beauty_hair & !courier & !gambling_rec
  ) %>% 
  group_by(year) %>% 
  summarise(
    across(
      .cols = c(restaurants_bars, taxi, beauty_hair, courier, gambling_rec, other), 
      .fns  = list(
        `Amount of tips ($B)` = ~ sum(. * tips * weight) / 1e9,
        `Share of total tips` = ~ weighted.mean(., tips * weight)
      ), 
      .names = '{col}.{fn}'
    ) 
  ) %>%
  pivot_longer(
    cols      = -year, 
    names_sep = '[.]', 
    names_to  = c('industry', 'metric')
  ) %>%  
  ggplot(aes(x = year, y = value, fill = industry)) + 
  geom_col() + 
  facet_wrap(~metric, scales = 'free') + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(2017, 2022, 1))


industries = sipp %>% 
  filter(year >= 2021) %>% 
  left_join(
    read_csv('../Tax-Data/resources/industry.csv'), by = c('ind_1' = '')
  ) %>% 
  group_by(ind, description) %>% 
  summarise(
    share_tipped = weighted.mean(tips > 0, weight), 
    tip_share    = weighted.mean(tip_share, weight * (tips > 0)),
    total_tips   = sum(tips * weight) / 1e9,
    .groups = 'drop'
  ) %>% 
  arrange(-(tip_share * (share_tipped > 0.1))) %>% 
  left_join(
    sipp %>% 
      filter(year >= 2021) %>%
      group_by(ind = ind_1) %>%
      summarise(
        wages = sum(wages * weight) / 1e9
      ), 
    by = 'ind'
  ) 




#---------------------------
# Calculate summary metrics
#---------------------------


# Wage, age, and marital status
sipp %>%
  group_by(wage_decile, married = if_else(married == 1, 'Married', 'Unmarried')) %>% 
  summarise(share_tipped = weighted.mean(tips > 0, weight)) %>% 
  ggplot(aes(x = wage_decile, y = share_tipped, colour = married)) + 
  geom_point(size = 3) +
  geom_line() + 
  geom_hline(yintercept = 0) + 
  theme_bw() +
  labs(
    x = 'Wage decile', 
    y = element_blank(), 
    colour = element_blank() 
  ) + 
  scale_x_continuous(breaks = 1:10) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(legend.position = 'top') + 
  ggtitle('By wage decile')

sipp %>%
  filter(age >= 18) %>% 
  group_by(
    age = case_when(
      age < 25 ~ '18-24',
      age < 35 ~ '25-34',
      age < 45 ~ '35-44',
      age < 55 ~ '45-54',
      age < 65 ~ '55-64',
      T        ~ '65+'
    ), 
    married = if_else(married == 1, 'Married', 'Unmarried')
  ) %>% 
  summarise(share_tipped = weighted.mean(tips > 0, weight), .groups = 'drop') %>% 
  ggplot(aes(x = age, y = share_tipped, colour = married)) + 
  geom_point(size = 3) +
  geom_line(aes(x =as.integer(as.factor(age)))) + 
  geom_hline(yintercept = 0) + 
  theme_bw() +
  labs(
    x      = 'Age group', 
    y      = element_blank(), 
    colour = element_blank() 
  ) + 
  scale_y_continuous(breaks = seq(0, 0.06, 0.01), labels = scales::percent_format()) + 
  theme(legend.position = 'top') + 
  ggtitle('By age group')

# Industry breakdown 
sipp %>%
  filter(tipped) %>% 
  left_join(
     read_csv('./resources/industry.csv', col_names = F) %>% 
       mutate(
         primary_ind = as.integer(str_sub(X1, 1, 4)), 
         description = str_sub(X1, 7)
       ), 
     by = 'primary_ind'
  ) %>%
  filter(!is.na(description)) %>% 
  group_by(description) %>% 
  summarise(
    `Share of tipped workers` = sum(weight),
    `Share of tips`           = sum(weight * tips)
  ) %>% 
  arrange(-`Share of tipped workers`) %>% 
  mutate(rank = row_number()) %>% 
  pivot_longer(
    cols     = starts_with('Share'), 
    names_to = 'Metric'
  ) %>% 
  group_by(Metric) %>%
  mutate(value = value / sum(value)) %>% 
  ungroup() %>% 
  head(20) %>% 
  mutate(label = scales::percent(value, accuracy = 0.1)) %>% 
  ggplot(aes(x = fct_reorder(description, -rank), y = value, fill = Metric)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = label), position = position_dodge(width = 1), hjust = -0.3, size = 3) + 
  coord_flip() + 
  theme_bw() + 
  theme(axis.title = element_blank(), legend.title = element_blank()) + 
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 6, 0.1), labels = scales::percent_format()) + 
  theme(legend.position = 'top')




sipp_tipped %>% 
  left_join(
    read_csv('./resources/industry.csv', col_names = F) %>% 
      mutate(
        primary_ind = as.integer(str_sub(X1, 1, 4)), 
        description = str_sub(X1, 7)
      ), 
    by = 'primary_ind'
  ) %>% 
  group_by(description) %>% 
  summarise(
    n              = sum(weight),
    mean           = weighted.mean(tip_share, weight),
    median         = wtd.quantile(tip_share, weight, 0.5), 
    share_majority = weighted.mean(tip_share >= 0.5, weight)
  ) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  arrange(-mean) %>% 
  select(-n)



microdata %>%
  filter(scenario == 'baseline', year == 2025) %>% 
  mutate(ctc = ctc_nonref + ctc_ref) %>% 
  select(id, weight, agi, tips1, tips2, wages1, wages2, liab_bc, ctc, eitc, liab_iit_net) %>%
  pivot_longer(cols = c(wages1, wages2, tips1, tips2)) %>% 
  mutate(
    variable = str_sub(name, end = -2),
    index    = str_sub(name, start = -1)
  ) %>% 
  select(-name) %>% 
  pivot_wider(names_from = variable) %>% 
  reframe(
    pctile        = c(seq(0.1, 0.9, 0.1), 0.95, 0.99),
    all_workers   = wtd.quantile(agi, weight * (wages > 0), c(seq(0.1, 0.9, 0.1), 0.95, 0.99)), 
    any_tips      = wtd.quantile(agi, weight * (tips > 0), c(seq(0.1, 0.9, 0.1), 0.95, 0.99)),
    majority_tips = wtd.quantile(agi[tips / wages >= 0.5], weight[tips / wages >= 0.5] * (tips > 0), c(seq(0.1, 0.9, 0.1), 0.95, 0.99)),
  )
  

# Counts
microdata %>%
  filter(scenario == 'baseline', year == 2025) %>% 
  mutate(ctc = ctc_nonref + ctc_ref) %>% 
  select(id, weight, tips1, tips2, wages1, wages2, liab_bc, ctc, eitc, liab_iit_net) %>%
  pivot_longer(cols = c(wages1, wages2, tips1, tips2)) %>% 
  mutate(
    variable = str_sub(name, end = -2),
    index    = str_sub(name, start = -1)
  ) %>% 
  select(-name) %>% 
  pivot_wider(names_from = variable) %>% 
  filter(wages > 0) %>%
  group_by(tipped = if_else(tips > 0, 'tipped', 'non-tipped')) %>% 
  summarise(
    no_liab_bc = weighted.mean(liab_bc == 0, weight),
    no_liab    = weighted.mean(liab_iit_net <= 0, weight),
    ctc        = weighted.mean(ctc > 0, weight),
    eitc       = weighted.mean(eitc > 0, weight)
  ) %>% 
  pivot_longer(-tipped) %>% 
  pivot_wider(names_from = tipped) %>% 
  mutate(ratio = tipped / `non-tipped`)



# 2017 imputed tip quantiles
tax_data_root %>%
  file.path('tax_units_2017.csv') %>% 
  fread() %>% 
  tibble() %>% 
  select(id, weight, tips1, tips2) %>% 
  pivot_longer(cols = starts_with('tips')) %>% 
  filter(value > 0) %>% 
  reframe(
    pctile  = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
    actual  = c(38, 111, 528, 2480,	7883, 16455,	23488,	40661),
    modeled = wtd.quantile(value, weight, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)) 
  ) %>% 
  write.csv()
  

#-----------------------------------------------
# Calculate distributional metrics under reform
#-----------------------------------------------

# Calculate deltas
deltas = microdata %>% 
  mutate(liab = liab_iit_net + liab_pr) %>% 
  select(id, scenario, year, liab) %>% 
  pivot_wider(
    names_from = scenario,
    values_from = liab, 
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
  

# Calculate metrics 
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
    `Share with tipped income`           = weighted.mean(tips > 0, weight),
    `Average tax change`                 = weighted.mean(delta, weight),
    `Percent change in after-tax income` = weighted.mean(-delta / ati, weight * ati), 
    `Share with tax cut`                 = sum((delta < 0) * weight) / sum(weight), 
    `Average tax cut`                    = -weighted.mean(delta, weight * (delta < -5)), 
    .groups = 'drop'
  )


# Visualize metrics
get_chart = function(yr, metric) {
  quintile_metrics %>%
    filter(year == yr) %>% 
    mutate(
      scenario = case_when(
        scenario == 'income_tax_lh' ~ '1) Income tax exemption for leisure and hospitality workers',
        scenario == 'income_tax'    ~ '2) Income tax exemption', 
        scenario == 'payroll_tax'   ~ '3) Income tax and payroll tax exemption'
      )
    ) %>% 
    pivot_longer(cols = contains(' ')) %>% 
    filter(name == metric) %>% 
    ggplot(aes(x = as.integer(quintile), y = value, colour = scenario)) + 
    geom_point(size = 3) +
    geom_line() + 
    geom_hline(yintercept = 0) + 
    labs(
      x = 'Income quintile', 
      y = element_blank(), 
      colour = element_blank()
    ) + 
    theme_bw() + 
    theme(legend.position = 'top') + 
    guides(colour = guide_legend(ncol = 1))
}


get_chart(2025, 'Share with tax cut') +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle('Share with tax cut')

get_chart(2025, 'Average tax cut') +
  scale_y_continuous(labels = scales::dollar_format()) +
  ggtitle('Average tax cut among those with cut')

get_chart(2025, 'Percent change in after-tax income') +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle('Percent change in after-tax income')


