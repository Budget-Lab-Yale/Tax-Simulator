library(tidyverse)


tax_data = read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505051342/tcja_ext/static/detail/2026.csv')
ot_data  = read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505051342/ot_with_guardrails/static/detail/2026.csv')

tax_data = tax_data %>% 
  left_join(
    read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2025042811/baseline/tax_units_2026.csv') %>% 
      select(id, ot), 
    by = 'id'
  ) %>% 
  left_join(
    ot_data %>% 
      select(id, new_agi = agi, new_liab_iit_net = liab_iit_net), 
    by = 'id'
  )

# Let's assume the data is loaded into a dataframe called 'tax_data'
# tax_data <- read_csv("your_data.csv")

# 1) Summary metrics of ot by agi group
# First, create AGI groups
agi_group_analysis <- tax_data %>%
  # Create AGI groups (adjust breaks as needed for your data)
  mutate(agi_group = case_when(
    agi < 20000 ~ "Under $20K",
    agi < 50000 ~ "$20K-$50K",
    agi < 100000 ~ "$50K-$100K",
    agi < 200000 ~ "$100K-$200K",
    agi < 400000 ~ "$200K-$400K",
    TRUE ~ "Over $400K"
  )) %>%
  # Set as factor to maintain order
  mutate(agi_group = factor(agi_group, levels = c(
    "Under $20K", "$20K-$50K", "$50K-$100K", 
    "$100K-$200K", "$200K-$400K", "Over $400K"
  ))) %>%
  # Group by AGI group
  group_by(agi_group) %>%
  # Calculate summary statistics
  summarise(
    share_filers    = sum(weight * filer),
    share_with_ot   = weighted.mean(ot > 0, weight),
    mean_ot         = weighted.mean(ot, weight),
    mean_ot_nonzero = weighted.mean(ot, weight * (ot > 0)),
    total_ot        = sum(ot * weight) / 1e9, 
  ) %>% 
  mutate(
    share_filers = share_filers / sum(share_filers), 
    share_ot     = total_ot / sum(total_ot), 
  )

print(agi_group_analysis)

agi_group_analysis %>% 
  write.csv()





tax_data %>% 
  mutate(ot_cap = if_else(filing_status == 2, 20000, 10000)) %>%
  mutate(ot_thresh = if_else(filing_status == 2, 200000, 100000)) %>% 
  group_by(filing_status) %>% 
  summarise(
    total_ot = sum(ot * weight) / 1e9,
    ot_above_cap = sum(pmax(ot - ot_cap, 0) * weight) / 1e9
  )


tax_data %>% 
  filter(dep_status == 0) %>% 
  mutate(tax_change = new_liab_iit_net - liab_iit_net, agi_change = new_agi - agi) %>% 
  arrange(tax_change) %>%
  select(id, filing_status, agi, agi_change, new_agi, ot, liab_iit_net, tax_change, starts_with('mtr_'))

library(Hmisc)
tax_data %>% 
  filter(dep_status == 0, ot > 0) %>% 
  mutate(agi_group = case_when(
    agi < 20000 ~ "Under $20K",
    agi < 50000 ~ "$20K-$50K",
    agi < 100000 ~ "$50K-$100K",
    agi < 200000 ~ "$100K-$200K",
    agi < 400000 ~ "$200K-$400K",
    TRUE ~ "Over $400K"
  )) %>%
  # Set as factor to maintain order
  mutate(agi_group = factor(agi_group, levels = c(
    "Under $20K", "$20K-$50K", "$50K-$100K", 
    "$100K-$200K", "$200K-$400K", "Over $400K"
  ))) %>%
  # Group by AGI group
  group_by(agi_group) %>% 
  reframe(name = seq(0.1, 0.9, 0.1), value = wtd.quantile(ot, weight, seq(0.1, 0.9, 0.1))) %>% 
  pivot_wider()
  


library(haven) 

raw_data = read_dta('/gpfs/gibbs/project/sarin/jar335/Repositories/Tax-Data/resources/otdata2023.dta')


raw_data %>%
  mutate(
    wages    = total_weekly_earnings * wkswork1, 
    ot       = ot_weekly_earnings * wkswork1 * (flsa_eligible > 0)
  ) %>%
  reframe(
    p = seq(0, 0.99, 0.01),
    all = wtd.quantile(hourly_wage, weight, p = seq(0, 0.99, 0.01)), 
    ot = wtd.quantile(hourly_wage, weight * (ot > 0), p = seq(0, 0.99, 0.01))
  ) %>% 
  print(n = 100)

