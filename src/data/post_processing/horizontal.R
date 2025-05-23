#----------------------------------------------------------------------------
# horizontal.R
# 
# Calculates measures of horizontal equity 
#----------------------------------------------------------------------------


build_horizontal_tables = function(id) {
  
  #----------------------------------------------------------------------------
  # This function constructs tables to analyze horizontal equity within expanded
  # income deciles for all scenarios in the current batch. 
  # 
  # Parameters:
  #   - id : (str) scenario ID
  # 
  # Returns: void, writes two csv's per scenario. One horizontal distribution table,
  #          one summary table for simplified A-B univariate comparison
  #----------------------------------------------------------------------------
  
  calibrators = expand_grid(P = seq(0,1,0.25), e = seq(0,1,0.25))
  
  for (yr in get_scenario_info(id)$dist_years) {
    micro = file.path(globals$output_root, id, 'static/detail', paste0(yr, '.csv')) %>% 
      fread() %>% 
      tibble()
  
    1:nrow(calibrators) %>%
      map(.f = ~ get_horizontal_dist(micro, id, calibrators[.x,])) %>%
      bind_rows() %>%
      write_csv(., file = file.path(globals$output_root, id, 'static/supplemental/horizontal.csv'))
  
    construct_horizontal_comparison_figures(micro, id)
  }
}


get_horizontal_dist = function(tax_units, scen_id, calibrators) {
  
  #----------------------------------------------------------------------------
  # This helper function constructs tables to analyze horizontal equity within 
  #    expanded income deciles for a single scenario based on the normalizing
  #    income equation found in Gravelle & Gravelle (2006).
  # 
  # Parameters:
  #   - tax_units  : (DataFrame) tax filers after having passed through the simulator
  #   - scen_id : (str) scenario or baseline ID for given test
  #   - calibrators : (vec) values used to tune the income normalization equation
  # 
  # Returns: void, writes a csv of the horizontal distribution table.
  #----------------------------------------------------------------------------
  
  p = calibrators$P
  e = calibrators$e

  negative = tax_units %>% filter(expanded_inc <= 0) %>%
    mutate(
      jitter = 0,
      inc_eq = (expanded_inc / ((1 + int(!is.na(male2)) + p * n_dep)^e)) * 2^e,
      etr = case_when(
        expanded_inc == 0                   ~ 0,
        expanded_inc < 0 & liab_iit_net < 0 ~ -1 * liab_iit_net / inc_eq,
        T                                   ~ liab_iit_net / inc_eq
      ),
      tile = ifelse(inc_eq == 0, 0, -1)
    ) 
  
  sub_units = tax_units %>%
    filter(expanded_inc > 0) %>%
    mutate(
      jitter = runif(nrow(.), 0, 1) / 1000,
      inc_eq = ((expanded_inc / ((1 + int(!is.na(male2)) + p * n_dep)^e)) * 2^e) + jitter,
      etr = liab_iit_net / inc_eq,
      tile = ntiles.wtd(inc_eq, 1000, weight)
    ) %>%
    rbind(negative) %>%
    mutate(
    inc_cat = factor(case_when(
        tile == -1              ~ 'Negative',
        inc_eq == 0             ~ 'No Income',
        between(tile, 1, 99)    ~ "'1-9",
        between(tile, 100, 199) ~ "'10-19",
        between(tile, 200, 299) ~ '20-29',
        between(tile, 300, 399) ~ '30-39',
        between(tile, 400, 499) ~ '40-49',
        between(tile, 500, 599) ~ '50-59',
        between(tile, 600, 699) ~ '60-69',
        between(tile, 700, 799) ~ '70-79',
        between(tile, 800, 899) ~ '80-89',
        between(tile, 900, 990) ~ '90-99',
        T                       ~ 'Top'
      ), levels = c('Negative', 'No Income', "'1-9", "'10-19", '20-29', '30-39', '40-49',
                    '50-59', '60-69', '70-79', '80-89', '90-99', 'Top'))
    )   %>%
    mutate(inc_eq = inc_eq - jitter) %>%
    filter(! between(tile, 1, 10))
  
  sub_units %>% 
    group_by(inc_cat) %>%
    summarise(
      `id` = scen_id,
      `P` = p,
      `e` = e,
      `Average Equalized Income` = wtd.mean(inc_eq, weight),
      `Average Tax Rate` = sum(liab_iit_net * weight) / sum(inc_eq * weight),
      `Standard Deviation of Tax Rate` = sqrt(wtd.var(etr, weight, na.rm = T)),
      .groups = 'drop'
    ) %>%
    return()
}


construct_horizontal_comparison_figures = function(tax_units, scen_id) {
  
  #----------------------------------------------------------------------------
  # This helper function constructs a simple dataframe to compare figures to
  #     facilitate easier comprehension of horizontal equity. Figures are identical
  #     except for a single characteristic which affects horizontal equity.
  # 
  # Parameters:
  #   - tax_units  : (DataFrame) tax filers after having passed through the simulator
  #   - scen_id : (str) scenario or baseline ID for given test
  # 
  # Returns: void, writes a csv summary table for simplified A-B univariate comparison
  #----------------------------------------------------------------------------
  
  tax_units %>%
    mutate(
      etr = liab_iit_net / expanded_inc
    ) %>%
    filter(
      (between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & n_dep_ctc < 2 & filing_status ==2) |
        (between(expanded_inc, 5e5 * .9, 5e5 *1.1) & filing_status == 1) |
        (between(expanded_inc, 2e5 * .97, 2e5 *1.03) & filing_status == 1),
      between(etr, -1, 1)
    ) %>%
    mutate(bucket = case_when(
      between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & (n_dep_ctc == 0)                         ~ "Figure 1.1",
      between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & (n_dep_ctc == 1)                         ~ "Figure 1.2",
      between(expanded_inc, 5e5  * .9, 5e5  * 1.1) & ((kg_lt + kg_st + div_pref + div_ord) / expanded_inc < .5)   ~ "Figure 2.1",
      between(expanded_inc, 5e5  * .9, 5e5  * 1.1) & ((kg_lt + kg_st + div_pref + div_ord) / expanded_inc >= .5)  ~ "Figure 2.2",
      between(expanded_inc, 2e5  * .97, 2e5  * 1.03) & (sch_e / expanded_inc <  .75)            ~ "Figure 3.1",
      between(expanded_inc, 2e5  * .97, 2e5  * 1.03) & (sch_e / expanded_inc >= .75)            ~ "Figure 3.2",
      T                                                                                         ~ "err"
    )) %>%
    filter(bucket != "err") %>%
    group_by(bucket) %>%
    summarise(
      inc  = sum(expanded_inc * weight),
      liab = sum(liab_iit_net * weight)
    ) %>%
    mutate(
      etr      = liab / inc,
      scenario = scen_id,
      figure   = substr(bucket, 1, 8),
      dot      = ifelse(substr(bucket, 10, 10)=='1', 'First', 'Second')
    ) %>%
    select(scenario, figure, dot, etr) %>%
    pivot_wider(names_from = dot, values_from = etr) %>%
    write_csv(., file = file.path(globals$output_root, scen_id, 'static/supplemental/horizontal_characters.csv'))
}

