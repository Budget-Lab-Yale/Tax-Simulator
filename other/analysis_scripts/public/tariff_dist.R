library(tidyverse)

output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Simulator/v1/202506090807/'

dist = output_root %>% 
  file.path('tariffs/static/supplemental/distribution.csv') %>% 
  read_csv(show_col_types = F) %>% 
  filter(
    year == 2027, 
    taxes_included == 'indirect_iit_pr_estate_cit', 
    str_sub(group, 1, 1) == 'Q' 
  ) %>% 
  select(source = indirect_tax_assumption, group, value = pct_chg_ati) %>% 
  mutate(source = if_else(source == 'consumption', 
                          '(C) Tax-Simulator results, by consumption', 
                          '(D) Tax-Simulator results, by income'), 
         value = value * 100) %>% 
  bind_rows(
    tibble(
      group = c('Quintile 1', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5'),
      value = c(-2.5, -2, -1.7, -1.5, -1.2)
    ) %>% 
      mutate(
        source = '(B) Current TBL results (by consumption)'
      )
  ) %>% 
  bind_rows(
    tibble(
      group = c('Quintile 1', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5'),
      value = c(-0.9, -1, -1, -1, -1)
    ) %>% 
      mutate(
        source = '(A) TPC (by income), Februrary tariffs'
      )
  ) 

dist %>%
  ggplot(aes(x = as.integer(as.factor(group)), y = value, label = round(value, 1))) + 
  geom_col(position = position_dodge(width = 0.9)) + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, colour = 'white') + 
  theme_bw() +
  facet_wrap(~source, ncol = 4) + 
  labs(
    fill = 'Method', 
    x = 'Income quintile', 
    y = 'Percentage points'
  ) + 
  ggtitle(
    'Comparison of tariff distribution approachs, 2027 (for tariffs as of June 1)', 
    subtitle = 'Percentage point change in after-tax income'
  )
