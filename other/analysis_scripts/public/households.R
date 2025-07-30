library(tidyverse)
library(data.table)

output = c('baseline', 'tcja', 'senate') %>% 
  map(
    .f = ~ fread(paste0('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202507031807/', .x, '/static/detail/2026.csv')) %>%
      tibble() %>% 
      mutate(scenario = .x) %>% 
      filter(id %in% c(3, 4, 7))  %>% 
      mutate(
        id = case_when(
          id == 3 ~ 1, 
          id == 4 ~ 2, 
          id == 7 ~ 3
        )
      )
  ) %>% 
  bind_rows()


output %>% 
  mutate(liab_ex_amt = liab_bc - liab_amt, ctc = ctc_nonref + ctc_ref) %>% 
  pivot_longer(
    cols = -c(scenario, id), 
    names_to = 'variable'
  ) %>% 
  pivot_wider(names_from = c(scenario, id)) %>%
  select(variable, baseline_1, tcja_1, senate_1, 
         baseline_2, tcja_2, senate_2, 
         baseline_3, tcja_3, senate_3) %>%
  filter(
    variable %in% c(
      'agi', 
      'std_ded', 
      'item_ded',
      'pe_ded',
      'txbl_inc',
      'liab_ex_amt', 
      'liab_amt',
      'ctc', 
      'eitc', 
      'liab_iit_net'
    )
  ) %>% 
  write.csv()


# Tariffs
pce_cex_ratio = 18.8 / 10.39848768

# Family 1: second decile
20000 * 1.57461300 * 0.015 * pce_cex_ratio
# Family 2: 9th decile
133000 * 0.680440536 * 0.015 * pce_cex_ratio
# Family 3: 
440000 * 0.511601447 * 0.014 * pce_cex_ratio
