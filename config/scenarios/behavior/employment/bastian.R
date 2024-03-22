do_employment = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts wage earnings at the extensive margin, per Bastian (2023). 
  # Requires extensive margin wage elasticities for both primary and 
  # secondary earners. 
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of extensive-margin MTRs on 
  #                          wages1 and wages2 under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of extensive-margin MTRs on 
  #                          wages1 and wages2 under the static counterfactual
  #
  # Returns: tibble of tax units with post-adjustment wage earnings values.
  #----------------------------------------------------------------------------
  
  
  # Set random seed 
  set.seed(globals$random_seed)
  
  # Set elasticities
  e_mothers_poor  = 0.4
  e_mothers_other = 0.2
  e_else          = 0.05
  
  # Read macro projections and build inflation index
  inflation_index = globals$interface_paths %>% 
    filter(interface == 'Macro-Projections') %>% 
    distinct(path) %>% 
    slice(1) %>% 
    deframe() %>% 
    file.path(c('historical.csv', 'projections.csv')) %>% 
    map(~ read_csv(.x, show_col_types = F)) %>% 
    bind_rows() %>% 
    mutate(cpiu = cpiu_irs / cpiu_irs[year == 2023]) %>% 
    select(year, cpiu)
  
  
  #-------------------------------------------------------
  # Determine elasticity and RTW info at the record level
  #-------------------------------------------------------
  
  worker_info = tax_units %>% 
    
    # Join MTRs
    left_join(baseline_mtrs %>% 
                rename_with(.cols = -c(id, year), 
                            .fn   = ~ paste0(., '_baseline')), 
              by = c('id', 'year')) %>%
    left_join(static_mtrs, by = c('id', 'year')) %>% 
    
    # Join inflation index
    left_join(inflation_index, by = 'year') %>% 
    
    mutate(

      # Calculate tax unit-level income (roughly AGI)
      income = wages + txbl_int + div_ord + div_pref + state_ref + 
               txbl_ira_dist + txbl_pens_dist + kg_lt + kg_st + other_gains + 
               sole_prop + part_active + part_passive - part_active_loss - 
               part_passive_loss - part_179 + scorp_active + scorp_passive - 
               scorp_active_loss - scorp_passive_loss - scorp_179 + rent - 
               rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc,
      
      #------------------
      # Set elasticities
      #------------------
      
      # Calculate unmarried EITC eligibility threshold
      eitc_thresh = case_when(
        n_dep_eitc == 0 ~ eitc.po_thresh_0 + (eitc.pi_end_0 * eitc.pi_rate_0) / eitc.po_rate_0,
        n_dep_eitc == 1 ~ eitc.po_thresh_1 + (eitc.pi_end_1 * eitc.pi_rate_1) / eitc.po_rate_1,
        n_dep_eitc == 2 ~ eitc.po_thresh_2 + (eitc.pi_end_2 * eitc.pi_rate_2) / eitc.po_rate_2,
        n_dep_eitc == 3 ~ eitc.po_thresh_3 + (eitc.pi_end_3 * eitc.pi_rate_3) / eitc.po_rate_3,
      ),
      
      # Index Bastian's $80000 income cutoff to inflation
      income_threshold = 80000 * cpiu,
      
      # First earner
      e1 = case_when(
        
        # EITC eligible single mothers
        (male1 == 0) & (n_dep_ctc > 0) & (wages1 < eitc_thresh) & (filing_status != 2) ~ e_mothers_poor, 
        
        # All other mothers with family income below $80,000 
        (male1 == 0) & (n_dep_ctc > 0) & (income < income_threshold) ~ e_mothers_other, 
        
        # Others below $80,000
        (income < income_threshold & n_dep_ctc > 0) ~ e_else,
        
        # Everyone else
        TRUE ~ 0
      ),
      
      # Second earner
      e2 = case_when(
        
        # Low-income single mothers (NA for second earners by definition)
        (male1 == 0) & (n_dep_ctc > 0) & (wages1 < eitc_thresh) & (filing_status != 2) ~ e_mothers_poor, 
        
        # All other mothers with family income below $80,000 
        (male1 == 0) & (n_dep_ctc > 0) & (income < income_threshold) ~ e_mothers_other, 
        
        # Others below $80,000
        (income < income_threshold & n_dep_ctc > 0) ~ e_else,
        
        # Everyone else
        TRUE ~ 0
      ),
      
      # Calculate percent change in return-to-work
      delta_rtw1 = ((1 - mtr_wages1) - (1 - mtr_wages1_baseline)) / (1 - mtr_wages1_baseline),
      delta_rtw2 = ((1 - mtr_wages2) - (1 - mtr_wages2_baseline)) / (1 - mtr_wages2_baseline),
    )
  
  
  #-------------------------------------------------------
  # Estimate elasticity-group-level net employment change 
  #-------------------------------------------------------
  
  # Calculate implied changes for primary earners
  delta_emp = list()
  delta_emp[[1]] = worker_info %>% 
    group_by(e = e1) %>% 
    summarise(n_records = sum(!is.na(delta_rtw1) & wages1 > 0), 
              delta_rtw = weighted.mean(delta_rtw1,      weight * (wages1 > 0), na.rm = T), 
              delta_emp = weighted.mean(delta_rtw1 * e1, weight * (wages1 > 0), na.rm = T) * 
                          sum(weight * (wages1 > 0), na.rm = T)) %>% 
    filter(e > 0, n_records > 0)
  
  # Calculate implied changes for secondary earners
  delta_emp[[2]] = worker_info %>% 
    group_by(e = e2) %>% 
    summarise(n_records = sum(!is.na(delta_rtw2) & wages2 > 0),
              delta_rtw = weighted.mean(delta_rtw2,      weight * (wages2 > 0), na.rm = T), 
              delta_emp = weighted.mean(delta_rtw2 * e2, weight * (wages2 > 0), na.rm = T) * 
                           sum(weight * (wages2 > 0), na.rm = T)) %>% 
    filter(e > 0, n_records > 0)
  
  # Write summary file
  output_folder = file.path(scenario_info$output_path, 'conventional/supplemental/employment')
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  current_year = tax_units %>% 
    distinct(year) %>% 
    deframe()
  
  bind_rows(
    delta_emp[[1]] %>% mutate(earner = 'primary', .before = everything()),
    delta_emp[[2]] %>% mutate(earner = 'secondary')
  ) %>% 
    write_csv(file.path(output_folder, paste0('employment_effects_', current_year, '.csv')))
  
  
  #-------------------------------------------------------------------------------------
  # Define function to simulate new wages for a given person index and elasticity group
  #-------------------------------------------------------------------------------------
  
  change_employment = function(worker_info, person_index, e_value) {
    
    # Recode person-index-specific variable names
    worker_info %<>%
      select(id, weight, all_of(c(paste0(c('wages', 'sole_prop', 'delta_rtw', 'e'), person_index)))) %>% 
      rename_with(.cols = -c(id, weight), .fn = ~ str_sub(., end = -2))
     
    # Extract group-level change in RTW
    delta_rtw = delta_emp[[person_index]] %>% 
      filter(e == e_value) %>%
      select(delta_rtw) %>% 
      deframe() 
    
    # If net-negative 
    if (delta_rtw < -0.001) {
      
      # Calculate scale-down factor such that gross outflows equal implied net employment loss
      delta_rtw_neg = worker_info %>% 
        filter(e == e_value) %>% 
        summarise(
          delta_rtw_neg = weighted.mean(
            x     = delta_rtw, 
            w     = weight * (wages > 0) * (delta_rtw < 0), 
            na.rm = T
          )
        ) %>% 
        select(delta_rtw_neg) %>% 
        deframe()
      
      scaling_factor = delta_rtw / delta_rtw_neg
      
      # Run employment loss simulation
      new_wages = worker_info %>% 
        filter(e == e_value) %>% 
        mutate(
          
          # Calculate record-level probability of remaining employed
          pr_emp = 1 + (e * delta_rtw * scaling_factor), 
          
          # Simulate outcome
          emp   = runif(nrow(.)) < pr_emp, 
          wages = if_else(wages == 0, 0, wages * emp),
        ) %>% 
        
        # Return only ID and (reconstructed-name) wage variable
        rename_with(.cols = wages, 
                    .fn   = ~paste0('new_', ., person_index)) %>% 
        select(id, starts_with('new_'))
      
    # If net-positive...
    } else if (delta_rtw > 0.001) { 

      # Draw potential wages for this group's nonworkers: distribution is 
      # assumed to be identical to that of this elasticity group's workers 
      potential_wages = worker_info %>% 
        filter(e == e_value, wages > 0) %>% 
        reframe(wages = wtd.quantile(wages, weight, probs = 0:99/100)) %>% 
        select(wages) %>% 
        deframe() %>% 
        set_names(NULL) %>% 
        sample(size    = worker_info %>% filter(e == e_value) %>% nrow(), 
               replace = T)
      
      # Run employment gain simulation
      new_wages = worker_info %>% 
        filter(e == e_value) %>% 
        mutate(
          
          # Extract group-level net change in employment
          delta_emp = delta_emp[[person_index]] %>% 
            filter(e == e_value) %>%
            select(delta_emp) %>% 
            deframe(),
          
          # Calculate probability of employment: implied net change over eligible nonworkers 
          # Note that self-employed are excluded from being chosen to start W2 employment
          pr_emp = case_when(
            wages == 0 & sole_prop == 0 ~ delta_emp / sum(weight * (wages == 0)),
            wages == 0 & sole_prop != 0 ~ as.integer(wages > 0), 
            wages > 0                   ~ 1
          ),
          
          # Simulate transition into employment 
          emp = runif(nrow(.)) < pr_emp, 
          
          # Assign wage TODO
          wages = if_else(wages == 0, emp * potential_wages, wages) 
          
        ) %>% 
        
        # Return only ID and (reconstructed-name) wage variable
        rename_with(.cols = wages, 
                    .fn   = ~paste0('new_', ., person_index)) %>% 
        select(id, starts_with('new_'))
    
    # Else leave unchanged   
    } else {
      new_wages = worker_info %>% 
        filter(e == e_value) %>% 
        rename_with(.cols = wages, 
                    .fn   = ~paste0('new_', ., person_index)) %>% 
        select(id, starts_with('new_'))
    }
    
    return(new_wages)
  }
  
  
  #----------------------------
  # Simulate employment change
  #----------------------------
  
  new_wages1 = delta_emp[[1]]$e %>% 
    map(.f = ~ change_employment(worker_info, 1, .x)) %>%
    bind_rows()
  
  new_wages2 = delta_emp[[2]]$e %>% 
    map(.f = ~ change_employment(worker_info, 2, .x)) %>%
    bind_rows()
  
  # Update wages values an return
  tax_units %>% 
    left_join(new_wages1, by = 'id') %>% 
    left_join(new_wages2, by = 'id') %>% 
    mutate(wages1 = if_else(is.na(new_wages1), wages1, new_wages1), 
           wages2 = if_else(is.na(new_wages2), wages2, new_wages2), 
           wages  = wages1 + wages2) %>% 
    return()
}

