baseline = '/vast/palmer/scratch/sarin/jmk263/Tax-Simulator/v1/202406250850/baseline/static/detail/2026.csv' %>%
  fread() %>%
  tibble()

tax_law = '/vast/palmer/scratch/sarin/jmk263/Tax-Simulator/v1/202406250850/baseline/static/supplemental/tax_law.csv' %>%
  read_csv()

# Assign brackets
baseline %<>% 
  left_join(
    tax_law %>% 
      filter(year == 2026) %>% 
      select(filing_status, starts_with('ord.brackets')), 
    by = 'filing_status'
  ) %>% 
  mutate(
    across(
      .cols  = starts_with('liab_brac'), 
      .fns   = ~ ifelse(. == 0, 0, 1),
      .names = 'has_{col}'
    ),
    agi_eq = (agi / ((1 + int(!is.na(male2)) + .7 * n_dep)^.7)) * 2^.7,
    last_bracket = rowSums(across(.cols = starts_with('has_liab_brac'))), 
    last_bracket = if_else(agi <= 0, 1, last_bracket)
  ) 

# Calculate ETRs and total liability for people assigned to each bracket
etrs = baseline %>% 
  group_by(last_bracket) %>% 
  summarise(
    agi  = sum(pmax(0, agi_eq) * weight) / 1e9,
    liab = sum(liab_iit_net * weight) / 1e9,
    etr  = liab / agi
  )


# Initialize taus
tau = c(etrs$etr[1], rep(NA, 6))


# Loop over income bracket group
for (i in 2:7) {
  
  # For the subpopulation of people assigned to this bracket, calculate the 
  # amount of income in each of the brackets
  income_by_bracket = baseline %>%
    
    # First, filter just to this group
    filter(last_bracket == i) %>% 
    
    # Then get total income in excess of each bracket (inclusive of brackets 
    # below, i.e. cumulative)
    summarise(
      across(
        .cols  = starts_with('ord.brackets'), 
        .fns   = ~ sum(pmax(0, agi - .) * weight) / 1e9
      )
    ) %>% 
    pivot_longer(
      cols            = everything(), 
      names_prefix    = 'ord.brackets', 
      names_transform = as.integer, 
      names_to        = 'bracket'
    ) %>% 
    
    # Calculate the amount attributable to each bracket 
    mutate(diff = if_else(bracket == 7, value, value - lead(value))) %>% 
    select(diff) %>% 
    unlist()
  
  # Calculate the liability on this group's income up to bracket i, based on the
  # previously-derived set of marginal tax rates (tau). This is akin to 
  # integrate_rates_brackets()
  liab_up_to_i = 0
  for (j in (i - 1):0) {
    liab_up_to_i = liab_up_to_i + sum(income_by_bracket[j] * tau[j])
  }
  
  # Solve for the marginal rate that generates their liability
  tau[i] = (etrs$liab[i] - liab_up_to_i) / income_by_bracket[i]
}

tau
