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
  
  # baseline here is used to compute was_hoh
  baseline = file.path(globals$baseline_root, "baseline", "static/detail",
                       paste0(year, ".csv")) %>% 
    fread() %>%
    tibble()
  reform   = file.path(globals$output_root, id, "static/detail",
                       paste0(year, ".csv")) %>% 
    fread() %>%
    tibble() %>%
    mutate(
      policy = !!id
    )
  
  
  #------------------------------------
  # create relevant record-level flags
  #------------------------------------
  
  microdata = reform %>%
    
    # Remove non-filers
    filter(filer != 0) %>%
    
    # create flags for use of IRS forms
    mutate(
      across(.cols = c("sch_e",
                       "farm",
                       "sole_prop",
                       "se",
                       "txbl_int",
                       "eitc",
                       "qbi_ded",
                       "liab_amt"),
             .fns  = list(function(x) as.integer(x != 0)) ) %>%
        rename_with(~c("sch_e_flag",
                       "sch_farm_flag",
                       "sch_c_flag",
                       "sch_se_flag",
                       "int_flag",
                       "sch_eic_flag",
                       "qbi_flag",
                       "amt_flag") ),
      sch_d_flag   = as.integer(div_ord != 0 | div_pref !=0 | txbl_kg != 0 |
                                  kg_st != 0 | kg_lt != 0),
      itemize_flag = as.integer(itemizing),
      ctc_flag     = as.integer(ctc_ref > 0 | ctc_nonref > 0)
    ) %>%
    
    # base filing status
    left_join(baseline %>%
                mutate(
                  base_status = filing_status
                ) %>%
                select(id, base_status),
              by = 'id') %>%
    
    mutate(
      # person weighting
      weight_person = weight * (1 + (filing_status == 2)),
      
      # adjust for number of available credits
      other_credits = number_of_credits - ctc_flag - sch_eic_flag,

      # variables for whether filer filled out preferred rate calculation, is
      # no longer HoH, and has no taxable income under the future policy
      has_pref_inc  = as.integer(liab_pref > 0),
      was_hoh       = as.integer(base_status == 4 & filing_status == 1),
      no_tax        = as.integer(txbl_inc == 0 & agi >= 0)
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
  
  
  #----------------------------------
  # compute record-level time burden
  #----------------------------------
    
  microdata %>%
    mutate(
      burden = (across(.cols = all_of(provisions)) %>%
                  as.matrix %*% time_costs
                  - was_hoh * (rowSums(across(.cols = all_of(hoh_item)))
                                * 11 + 34)
                  + (no_tax == 0) * fixed_cost) %>% as.vector()
    ) %>%
    return()
}



calc_summary_stats = function(microdata) {

  #----------------------------------------------------------------------------
  # Aggregates record-level time burden microdata into summary stats, grouped
  # income.
  #
  # Parameters:
  #  - microdata (df) : tax unit data, output by calc_time_burden()
  #
  # Returns: tibble of average and total time burdens grouped by income (df).
  #----------------------------------------------------------------------------
  
  
  #-------------------------
  # Determine income groups
  #-------------------------

  # calculate income quantile thresholds
  cutoffs = wtd.quantile(
    x       = microdata %>% 
                filter(expanded_inc >= 0) %>%
                pull(expanded_inc),
    probs   = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 0.999),
    weights = microdata %>%
                filter(expanded_inc >= 0) %>%
                pull(weight_person)
  )
  
  
  # assign income groups
  microdata = microdata %>%
    mutate(
      income_group = cut(
        x              = expanded_inc,
        breaks         = c(-Inf, max(expanded_inc[expanded_inc < 0]) / 2,
                           cutoffs, Inf),
        labels         = c("Negative income", "Bottom quintile",
                           "Second quintile", "Middle quintile",
                           "Fourth quintile", "80% - 90%", "90% - 99%",
                           "99% - 99.9%", "Top 0.1%"),
        right          = T,
        include.lowest = T
      )
    )
  
  
  #-----------------------
  # Compute summary stats
  #-----------------------
  
  # aggregate time burdens by income group
  microdata %>%
    group_by(income_group, policy) %>%
    summarise(
      
      # average time burden
      mean_burden  = wtd.mean(x = burden, weights = weight),
      # total time burden
      total_burden = sum(burden * weight),
      
      # counts
      count        = sum(weight),
      simple_filer = sum(simple_filer * weight), 
      
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

  
  # empty data frame
  results = data.frame(
    income_group = c('Negative income', 'Bottom quintile', 'Second quintile',
                     'Middle quintile', 'Fourth quintile', '80% - 90%',
                     '90% - 99%', '99% - 99.9%', 'Top 0.1%', "OVERALL")
  )
  
  
  #---------------------------
  # Loop over year X policies
  #---------------------------
  
  for (year in get_scenario_info(id)$dist_years) {
    
    for (id in c("baseline", id)){
      
      # read and process data
      microdata  = process_for_time_burden(id, year)
      # calculate time burdens
      fixed_cost = calc_fixed_cost(id)
      microdata  = calc_time_burden(microdata, fixed_cost)
      output     = calc_summary_stats(microdata)
      
      # add time burdens for each policy
      results = results %>%
        left_join(output %>%
                    # compute overall time burdens
                    bind_rows(output %>%
                                reframe(tibble(
                                  mean_burden = wtd.mean(x       = mean_burden,
                                                         weights = count),
                                  policy      = id)) %>%
                                mutate(income_group = "OVERALL")
                    ) %>%
                    mutate(burden = mean_burden / 60) %>%
                    select(policy, income_group, burden) %>%
                    pivot_wider(names_from = policy, values_from = burden),
          by = c("income_group")
        )
    }
    
    results = results %>%
      mutate(
        
        # compute hour and percent changes from current law
        across(.cols  = all_of(id),
               .fns   = list(hrs_change = function(x)  x - baseline,
                             pct_change = function(x) (x - baseline)
                             / baseline * 100),
               .names = "{.fn}"),
        
        # round numbers to nearest tenths
        across(.cols  = where(is.numeric),
               .fns   = function(x) round(x,1))
      ) %>%
      
      # rename columns with year
      rename_with(.fn   = ~paste0(., "-", year),
                  .cols = all_of(tail(names(.), 4)))
  }
  
  # write csv
  file = file.path(globals$output_root, id,
                   "static/supplemental/time_burden.csv")
  write_csv(results, file)
}


#-----------
# all done!
#-----------