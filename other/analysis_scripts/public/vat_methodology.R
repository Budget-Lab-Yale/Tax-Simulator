#---------------------------------------------------------
# Produces distribution charts for VAT methodology report
#---------------------------------------------------------


id = '50_10'

# Read VAT price offset for deflating other taxes
vat_price_offset = globals$output_root %>% 
  file.path(id, '/static/supplemental/vat_price_offset.csv') %>% 
  read_csv(show_col_types = F)

# Read baseline off-model revenues 
rev_baseline = globals$baseline_root %>% 
  file.path('baseline/static/totals/receipts.csv') %>% 
  read_csv(show_col_types = F) %>% 
  select(year, baseline = revenues_corp_tax)

# Read counterfactual scenario off-model revenues 
rev_reform = globals$output_root %>% 
  file.path(id, '/static/totals/receipts.csv') %>% 
  read_csv(show_col_types = F) %>% 
  select(year, reform = revenues_corp_tax) %>% 
  
  # Express corporate tax in baseline (consumer) dollars
  left_join(vat_price_offset, by = 'year') %>%
  mutate(reform = reform / cpi_factor) %>% 
  select(-ends_with('_factor'))

# Calculate change in corporate tax liability by year
corp_delta = rev_reform %>% 
  left_join(rev_baseline, by = 'year') %>% 
  mutate(delta = reform - baseline) %>%  
  
  # Determine first year of policy reform, if any, and allocate labor
  # share of changed corporate burden over time
  mutate(first_year = ifelse(sum(delta) != 0, 
                             min(year[cumsum(delta) != 0 & lag(delta, default = 0) == 0]), 
                             Inf), 
         labor_share = 0.2 * pmax(0, pmin(1, (year - first_year) / 10))) %>% 
  select(year, delta, labor_share) 



# For years 1, 10, and 30...
decomp = c(2025, 2034, 2054) %>% 
  map(.f = ~
        process_for_distribution(
          id               = id, 
          baseline_id      = 'baseline', 
          year             = .x, 
          financing        = 'none', 
          corp_delta       = corp_delta %>% filter(year == .x) %>% get_vector('delta'),
          labor_share      = corp_delta %>% filter(year == .x) %>% get_vector('labor_share'), 
          vat_price_offset = vat_price_offset %>% filter(year == .x) %>% get_vector('cpi_factor')
        ) %>% 
        mutate(Year = paste0('Year: ', .x - 2024))
  ) %>% 
  bind_rows() %>% 
  mutate(
    
    # Calculate components of gross VAT burden
    `Income: OASDI benefits`       = gross_ss                                 - ss_new, 
    `Income: Net interest`         = (txbl_int + exempt_int - first_mort_int) - debt_new, 
    `Income: Equity returns`       = (div_ord + div_pref + kg_st + kg_lt)     - equity_new, 
    `Income: Pass-through returns` = (sole_prop + part_scorp + farm)          - mixed_new,
    `Income: Other`                = vat_burden - `Income: OASDI benefits`    - `Income: Net interest` - `Income: Equity returns` - `Income: Pass-through returns`,
    `Tax: Individual income`       = liab - liab_baseline, 
    `Tax: Corporate income`        = corp_tax_labor + corp_tax_capital
    
  ) %>% 
  
  # Calculate contribution to percent change in ATI
  filter(income_group != 'Negative income') %>% 
  group_by(Year, income_group) %>% 
  summarise(
    across(
      .cols = c(contains(' '), delta), 
      .fns  = ~ sum(-. * weight) / sum((expanded_inc - liab_baseline) * weight)
    ), 
    .groups = 'drop'
  ) 



# Create plots
decomp %>% 
  select(income_group, Year, delta) %>% 
  ggplot(aes(x = as.integer(income_group), y = delta * 100, colour = Year)) + 
  geom_point(size = 3) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  labs(x = 'Income group', y = 'Percent change') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ggtitle('Percent change in after-tax income by year')

decomp %>% 
  pivot_longer(-c(income_group, Year, delta)) %>% 
  ggplot(aes(x = income_group, y = value * 100, fill = name)) + 
  geom_col() + 
  geom_point(aes(y = delta * 100), size = 3) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~Year, ncol = 3) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(-6, 2, 1)) + 
  labs(x = 'Income group', y = 'Percent change', fill = 'Source') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ggtitle('Contribution to percent change in after-tax income by year')


