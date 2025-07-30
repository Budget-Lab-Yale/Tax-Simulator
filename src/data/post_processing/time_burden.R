#----------------------------------------------------------------------------
# time_burden.R
# 
# Post-processing functions to generate time burden tables for a scenario
#----------------------------------------------------------------------------



process_for_time_burden = function(id, year) {
  
  #----------------------------------------------------------------------------
  # Reads and cleans input data for a given scenario, creating tax provision
  # flags at the record level.
  # 
  # Parameters:
  #   - id   (str) : scenario ID
  #   - year (int) : year to calculate time burdens for
  # 
  # Returns: microdata with all record-level variables required to calculate
  #          time burden (df).
  #----------------------------------------------------------------------------

    
  #-----------------------
  # Read microdata input
  #-----------------------
  
  # baseline here is used to compute scenario-consistent income groups and filing status changes 
  baseline = file.path(globals$baseline_root, "baseline", "static/detail", paste0(year, ".csv")) %>% 
    fread() %>%
    tibble()
  reform = file.path(globals$output_root, id, "static/detail", paste0(year, ".csv")) %>% 
    fread() %>%
    tibble() %>%
    mutate(policy = !!id) 
  
  # Determine whether EITC eligibility precerification is law 
  precert = file.path(globals$output_root, id, "static/supplemental", "tax_law.csv") %>% 
    read_csv(show_col_types = F) %>%
    rename(yr = year) %>%
    filter(yr == year) %>%
    select(eitc.parent_precert) %>%
    unique() %>%
    pull()
  
  #------------------------------------
  # create relevant record-level flags
  #------------------------------------
  
  microdata = reform %>%
    
    # start with baseline income (expanded income can shift across static scenarios 
    # due to reporting changes like SALT cap workarounds). Also grab baseline filing status
    select(-expanded_inc) %>% 
    left_join(
      baseline %>% 
        select(id, expanded_inc, base_status = filing_status), 
      by = 'id'
    ) %>% 
    
    # Remove non-filers
    filter(filer != 0) %>%
    
    # create flags for use of IRS forms
    mutate(
      across(.cols = c("sch_e",
                       "farm",
                       "sole_prop",
                       "tip_ded",
                       "ot_ded",
                       "auto_int_ded",
                       "senior_ded",
                       "se",
                       "txbl_int",
                       "eitc",
                       "qbi_ded",
                       "liab_amt"),
             .fns  = list(function(x) as.integer(x != 0))) %>%
        rename_with(~c("sch_e_flag",
                       "sch_farm_flag",
                       "sch_c_flag",
                       "tips_flag",
                       "ot_flag",
                       "auto_flag",
                       "senior_flag",
                       "sch_se_flag",
                       "int_flag",
                       "sch_eic_flag",
                       "qbi_flag",
                       "amt_flag") ),
      sch_d_flag   = as.integer(div_ord != 0 | div_pref !=0 | txbl_kg != 0 | kg_st != 0 | kg_lt != 0),
      itemize_flag = as.integer(itemizing),
      ctc_flag     = as.integer(ctc_ref > 0 | ctc_nonref > 0),
      eitc_precert = precert, 

      # Calculate number of other nonmodeled credits
      other_credits = number_of_credits - ctc_flag - sch_eic_flag,

      # variables for whether filer filled out preferred rate calculation, is
      # no longer HoH, and has no taxable income under the counterfactual reform
      has_pref_inc  = as.integer(liab_pref > 0),
      was_hoh       = as.integer(base_status == 4 & filing_status == 1),
      no_tax        = as.integer(txbl_inc == 0 & agi >= 0)
    ) %>%
    
    # Determine income groups 
    arrange(expanded_inc) %>% 
    mutate(
      
      # Income percentile
      income_pctile = cumsum(weight * (expanded_inc >= 0)) / sum(weight * (expanded_inc >= 0)), 
      income_pctile = if_else(expanded_inc < 0, NA, income_pctile), 
      
      # Quintiles and top shares
      quintile = case_when(
        income_pctile <= 0.2 ~ 'Quintile 1',
        income_pctile <= 0.4 ~ 'Quintile 2',
        income_pctile <= 0.6 ~ 'Quintile 3',
        income_pctile <= 0.8 ~ 'Quintile 4',
        income_pctile <= 1   ~ 'Quintile 5',
      ), 
      top_10 = if_else(income_pctile > 0.9,   'Top 10%',   NA), 
      top_5  = if_else(income_pctile > 0.95,  'Top 5%',    NA), 
      top_1  = if_else(income_pctile > 0.99,  'Top 1%',    NA), 
      top_01 = if_else(income_pctile > 0.999, 'Top 0.1%',  NA)
    ) %>% 
    
    return()
}



calc_fixed_cost = function(id) {
  
  #----------------------------------------------------------------------------
  # Calculates fixed cost
  # 
  # Parameters:
  #   - id   (str) : scenario ID
  #
  # Returns: fixed cost of filing (dbl).
  #----------------------------------------------------------------------------
  
  
  # hardcoded if first year is later than 2025
  first_year = get_scenario_info(id)$years[1]

  if (first_year > 2025){
    return(497.058)
  }
  
  
  #--------------------------------
  # define relevant tax provisions
  #--------------------------------
  
  provisions = c("int_flag",
                 "sch_c_flag",
                 "sch_d_flag",
                 "has_pref_inc",
                 "sch_e_flag",
                 "tips_flag",
                 "ot_flag",
                 "auto_flag",
                 "senior_flag",
                 "sch_farm_flag",
                 "itemize_flag",
                 "qbi_flag",
                 "sch_se_flag",
                 "amt_flag",
                 "ctc_flag",
                 "sch_eic_flag",
                 "other_credits")
  
  # time cost of each provision (coefficients)
  time_costs = c(86,
                 655,
                 311,
                 59,
                 374,
                 34,
                 34,
                 86,
                 19,
                 350,
                 556, 
                 155,
                 75,
                 87,
                 34,
                 34,
                 34)
  
  # HoH-contingent items
  hoh_item   = c("itemize_flag",
                 "sch_d_flag",
                 "qbi_flag",
                 "amt_flag",
                 "ctc_flag")
  
  
  #--------------------------
  # process 2025 current law
  #--------------------------
  
  microdata = process_for_time_burden("baseline", 2025)
  
  
  #----------------------
  # calculate fixed cost
  #----------------------
  
  # fixed cost is the residual difference between the modeled time burden and
  # the IRS-reported average of 13 hours (2022)
  
  microdata %>%
    
    # marginal time burden
    mutate(
      marginal_burden = (across(.cols = all_of(provisions)) %>%
                           as.matrix() %*% time_costs) %>% as.vector()
    ) %>%
    
    # fixed cost as residual difference
    summarise(
      total_marginal_burden = sum(weight * marginal_burden),
      total_cost            = sum(weight * 780),
      total_fixed_cost      = total_cost - total_marginal_burden,
      total_taxable_filers  = sum(weight * (no_tax == 0))
    ) %>%
    summarise(
      mean_fixed_cost       = total_fixed_cost / total_taxable_filers
    ) %>%
    pull() %>%
    return()
}



calc_time_burden = function(microdata, fixed_cost) {
  
  #----------------------------------------------------------------------------
  # Calculates and adds record-level time burdens to the processed microdata
  # 
  # Parameters:
  #   - microdata  (df)  : tax unit data, ouput by process_for_time_burden()
  #   - fixed_cost (dbl) : fixed cost of filing, output by calc_fixed_cost()
  # 
  # Returns: tibble of record-level time burdens (df).
  #----------------------------------------------------------------------------
  
  #--------------------------------
  # define relevant tax provisions
  #--------------------------------
  
  provisions = c("int_flag",
                 "sch_c_flag",
                 "sch_d_flag",
                 "has_pref_inc",
                 "sch_e_flag",
                 "tips_flag",
                 "ot_flag",
                 "auto_flag",
                 "senior_flag",
                 "sch_farm_flag",
                 "itemize_flag",
                 "qbi_flag",
                 "sch_se_flag",
                 "amt_flag",
                 "ctc_flag",
                 "sch_eic_flag",
                 "other_credits")
  
  # time cost of each provision (coefficients)
  time_costs = c(86,
                 655,
                 311,
                 59,
                 374,
                 34,
                 34,
                 86,
                 19,
                 350,
                 556,
                 155,
                 75,
                 87,
                 34,
                 34 + unique(microdata$eitc_precert) * 156,
                 34)
  
  # HoH-contingent items
  hoh_item = c("itemize_flag",
               "sch_d_flag",
               "qbi_flag",
               "amt_flag",
               "ctc_flag")
  
  
  #----------------------------------
  # compute record-level time burden
  #----------------------------------
    
  microdata %>%
    mutate(
      burden = (
        across(.cols = all_of(provisions)) %>%
                  as.matrix %*% time_costs
                  - was_hoh * (rowSums(across(.cols = all_of(hoh_item))) * 11 + 34)
                  + (no_tax == 0) * fixed_cost
      ) %>% as.vector()
    ) %>%
    return()
}



calc_summary_stats = function(grouped_microdata) {

  #----------------------------------------------------------------------------
  # Aggregates record-level time burden microdata into summary stats, grouped
  # income.
  #
  # Parameters:
  #  - microdata (df) : tax unit data, grouped by policy and prespecified
  #                     income group
  #
  # Returns: tibble of average and total time burdens grouped by income (df).
  #----------------------------------------------------------------------------
  

  # aggregate time burdens by income group
  grouped_microdata %>%
    summarise(
      
      # average time burden
      mean_burden  = wtd.mean(x = burden, weights = weight) / 60,
      
      # total time burden
      total_burden = (sum(burden * weight) / 60) / 1e6,
      
      # pre-filled returns
      share_prefilled = wtd.mean(x = simple_filer, weights = weight), 
      
      .groups = 'drop'
    ) %>% 
  return()
}



build_timeburden_table = function(id) {

  #----------------------------------------------------------------------------
  # Generates time burden tables by year
  # 
  # Parameters:
  #   - id   (str) : scenario ID
  #
  # Returns: void. 
  #----------------------------------------------------------------------------

  # Calculate fixed cost
  fixed_cost = calc_fixed_cost('baseline')
  
  # Loop over years 
  tables = list()
  for (yr in get_scenario_info(id)$dist_years) {
    
    # For both reform and baseline...
    tables[[as.character(yr)]] = c('baseline', id) %>% 
      map(
        .f = function(scenario) {
          
          # Do micro-level calculations
          microdata = scenario %>% 
            process_for_time_burden(yr) %>% 
            calc_time_burden(fixed_cost)
          
          # Calculate overall averages
          output = microdata %>% 
            group_by(group = 'Overall') %>% 
            calc_summary_stats() %>% 
            
            # By quintile
            bind_rows(
              microdata %>% 
                group_by(group = replace_na(quintile, 'Negative income')) %>%
                calc_summary_stats()
            ) %>% 
            
            # Top quintile breakout
            bind_rows(
              microdata %>% group_by(group = top_10) %>% calc_summary_stats() %>% filter(!is.na(group)), 
              microdata %>% group_by(group = top_5)  %>% calc_summary_stats() %>% filter(!is.na(group)), 
              microdata %>% group_by(group = top_1)  %>% calc_summary_stats() %>% filter(!is.na(group)), 
              microdata %>% group_by(group = top_01) %>% calc_summary_stats() %>% filter(!is.na(group)) 
            )
          
          # Add year-scenario identifiers
          if (scenario == 'baseline') {
            output = output %>% 
              mutate(year = yr, scenario = scenario, .before = everything())
          } else {
            output = output %>% 
              mutate(year = yr, scenario = 'reform', .before = everything())
            output$scenario = 'reform'
          }
          
          return(output)
          
        }
      ) %>% 
      bind_rows() %>% 
      
      # Calculate deltas
      pivot_longer(
        cols     = c(mean_burden, total_burden, share_prefilled), 
        names_to = 'metric'
      ) %>% 
      arrange(metric) %>% 
      pivot_wider(
        names_from  = scenario, 
        values_from = value
      ) %>%
      mutate(
        diff     = reform - baseline, 
        pct_diff = reform / baseline - 1
      ) 
  }
  
  tables %>% 
    bind_rows() %>% 
    write_csv(
      file.path(globals$output_root, id, "static/supplemental/time_burden.csv")
    )
}
          
          
          

  
  



