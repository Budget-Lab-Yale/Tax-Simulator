library(tidyverse)



ages = c('baseline', '0', '5', '16')


detail = ages %>% 
  map(
    .f = ~ file.path('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202503231145', .x, 'static/detail/2026.csv') %>% 
      read_csv() %>% 
      mutate(age = .x, .before = everything())
  ) %>% 
  bind_rows() %>% 
  filter(n_dep_ctc > 0)


dist %>% 
  filter(taxes_included == 'iit_pr', group_dimension == 'Income', group == 'Quintile 1') 


