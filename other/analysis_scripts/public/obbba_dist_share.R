#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# TODO
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------


library(tidyverse)

#----------------
# Set parameters
#----------------

output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Simulator/v1'

params = list(
  current_law = list(
    vintage = '202506271342', 
    scenarios = c(
      'tcja',
      'house', 
      'senate'
    )
 ), 
 current_policy = list(
   vintage = '202506271348', 
   scenarios = c(
     'house-ind',
     'senate-ind',
     'house', 
     'senate'
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
    group_dimension == 'Income', 
    group           != 'Negative income'
  ) %>%
  mutate(
    top_breakout = str_sub(group, 1, 1) != 'Q' & group != 'Overall', 
    pct_chg_ati = pct_chg_ati * 100,
  ) %>% 
  select(year, exercise, scenario, group, top_breakout, income_cutoff, pct_chg_ati, share_net_change) %>%
  pivot_longer(
    cols = c(pct_chg_ati, share_net_change)
  )
  


dist %>% 
  pivot_wider(
    names_from  = scenario, 
    values_from = value 
  ) %>% 
  write.csv()




