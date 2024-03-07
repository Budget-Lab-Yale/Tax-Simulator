do_child_earnings = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Assuming a rank-rank IGE value, adjusts earnings based on childhood 
  # exposure to an increase in family income. For each year, we look at all 
  # adults workers to determine how much their parent's after-tax income 
  # changed during childhood. We convert this value into rank-equivalent
  # units. Then, assuming a 0.34 rank-rank IGE and multiplying by 20% to 
  # reflect that 20% of the observed IGE reflects the causal mechanism of 
  # of income (the rest being selection, education, discrimination, etc), we 
  # we convert this value (year-weighted childhood exposure to additional 
  # income) into rank-units of adult earnings. Finally we convert this value
  # into dollars and rescale wages.
  # 
  # Parent income rank is imputed in Tax-Data based on the mobility matrix 
  # from Chetty et al. (2014). This paper is also where the 0.34 elasticity
  # value comes from. The causal share is assumed.
  # 
  # Parameters: 
  #   - tax_units (df) : tibble of tax units with calculated variables
  #   - scenario_info (list) : get_scenario_info() object
  #
  # Returns: tax units tibble with adjusted wages (df).
  #----------------------------------------------------------------------------
  
  #----------------
  # Set parameters
  #----------------

  # First year of policy
  start_year = 2026
  
  # Current year
  current_year = tax_units %>% 
    distinct(year) %>% 
    deframe()

  # Unconditional rank-rank IGE  
  ige = 0.34
  
  # Causal share of IGE
  causal_share = 0.2

  
  #-----------
  # Read data 
  #-----------
  
  # Read intergenerational mobility parameters
  mobility_matrix = 'resources/mobility_matrix.csv' %>%
    read_csv(show_col_types = F) %>% 
    pivot_longer(cols            = -child_rank, 
                 names_to        = 'parent_rank', 
                 names_transform = as.integer,
                 values_to       = 'pdf') %>%
    relocate(parent_rank, .before = everything()) %>% 
    arrange(parent_rank, child_rank) %>%
    
    # Aggregate bottom 10 percent of children to deal with gaps around 0 income
    mutate(child_rank = pmax(10, child_rank)) %>% 
    group_by(parent_rank, child_rank) %>% 
    summarise(pdf = sum(pdf), 
              .groups = 'drop')
  
  
  # Read baseline and static input; calculate change in income for this year 
  policy_effect_current_year = globals$output_root %>% 
    file.path('baseline/static/detail', paste0(current_year, '.csv')) %>% 
    fread() %>% 
    tibble() %>%
    mutate(liab_baseline = liab_iit_net + liab_pr_ee) %>% 
    bind_cols(
      globals$output_root %>% 
        file.path(scenario_info$ID, '/static/detail', paste0(current_year, '.csv')) %>% 
        fread() %>% 
        tibble() %>%
        mutate(liab_policy = liab_iit_net + liab_pr_ee) %>% 
        select(liab_policy)
    ) %>% 
    mutate(delta_income = liab_baseline - liab_policy) %>% 
    select(id, expanded_inc, delta_income)
  
  # Get parent rank
  parent_ranks = scenario_info$interface_paths$`Tax-Data` %>% 
    file.path(paste0('tax_units_', current_year, '.csv')) %>% 
    fread() %>% 
    tibble() %>% 
    select(id, parent_rank)
  
  
  #----------------------------------------------------
  # Calculate current-year children's change in income 
  #----------------------------------------------------
  
  # Calculate percent change in future earnings by parent rank; write
  tax_units %>% 
    
    # Join baseline info and calculate change in income
    left_join(policy_effect_current_year, by = 'id') %>% 
    
    # Assign parent income rank
    filter(n_dep_ctc > 0) %>% 
    mutate(
      parent_rank = cut(
        x = expanded_inc, 
        breaks = wtd.quantile(
          x       = .$expanded_inc, 
          weights = .$weight, 
          probs = 0:100/100
        ),
        labels = 1:100
      ) %>% as.integer()
    ) %>% 
    
    # Reshape to child basis
    select(id, weight, starts_with('dep_age'), parent_rank, expanded_inc, delta_income) %>% 
    pivot_longer(cols            = starts_with('dep_age'),
                 names_prefix    = 'dep_age', 
                 names_to        = 'child_index', 
                 names_transform = as.integer, 
                 values_to       = 'age') %>% 
    filter(age < 18) %>% 
    
    # Calculate average effect by parent rank
    group_by(parent_rank) %>% 
    summarise(delta_income = weighted.mean(delta_income, weight), 
              expanded_inc = weighted.mean(pmax(0, expanded_inc), weight)) %>% 
    
    # Convert change in income to rank-space equivalent
    mutate(rank_slope = expanded_inc - lag(expanded_inc), 
           rank_slope = ifelse(is.na(rank_slope), lead(rank_slope), rank_slope), 
           delta_rank = delta_income / rank_slope) %>%
    
    # Write to disk for use in future year
    write_csv(
      file.path(
        globals$output_root, 
        scenario_info$ID, 
        'static/supplemental/child_earnings', 
        paste0('exposure_', current_year, '.csv')
      )
    )
  

  if (current_year > start_year) {
    
    #------------------------------------------------------------
    # Calculate exposure-weighted policy effects for each cohort
    #------------------------------------------------------------
    
    # Read historical policy effects 
    policy_effect = start_year:(current_year - 1) %>% 
      map(.f = ~ globals$output_root %>% 
            file.path(scenario_info$ID, 'static/supplemental/child_earnings', paste0('exposure_', .x, '.csv')) %>% 
            read_csv(show_col_types = F) %>% 
            mutate(source_year = .x, .before = everything())) %>% 
      bind_rows() %>% 
      select(source_year, parent_rank, delta_rank) %>% 
    
      # Add ages for adults who were a child during a given year
      expand_grid(current_age = 18:100) %>% 
      mutate(source_year_age = current_age - (current_year - source_year)) %>% 
      filter(source_year_age %in% 0:17) %>% 
      
      # Get exposure-weighted average effect
      group_by(parent_rank, current_age) %>% 
      summarise(delta_rank = mean(delta_rank) * n() / 18, 
                .groups = 'drop') %>% 
      filter(!is.na(parent_rank))
    
    
    #------------------------------------
    # Apply effect to current-year wages
    #------------------------------------
    
    # Assign current-year (child) income ranks
    earnings_ranks = tax_units %>% 
      filter(wages > 0) %>% 
      mutate(
        child_rank = cut(
          x = wages, 
          breaks = wtd.quantile(
            x       = .$wages, 
            weights = .$weight, 
            probs = 0:100/100
          ), 
          labels = 1:100
        ) %>% 
          as.integer()
      ) %>% 
      select(id, child_rank)
    
    tax_units %<>% 
      left_join(earnings_ranks, by = 'id')
    
    
    # Generate rank-earnings conversion table
    rank_wage_loopkup = tax_units %>% 
      filter(wages > 0) %>% 
      
      # Calculate average earnings by rank
      group_by(child_rank) %>% 
      summarise(mean_wages = weighted.mean(wages, weight)) %>% 
      
      # Calculate wage slope at each rank
      mutate(rank_slope = mean_wages - lag(mean_wages), 
             rank_slope = ifelse(is.na(rank_slope), mean_wages, rank_slope))
    
    
    # Re-scale wages
    tax_units %<>%
      
      # Merge info on policy effect in rank terms 
      left_join(policy_effect, by = c('parent_rank' = 'parent_rank', 
                                      'age1'        = 'current_age')) %>% 
      
      # No match for policy effect implies worker was too old to be exposed to policy
      mutate(delta_rank = replace_na(delta_rank, 0)) %>% 
 
      # Merge info on rank-wage conversion factor
      left_join(rank_wage_loopkup, by = 'child_rank') %>% 
      
      # Apply effect to wages
      mutate(
        
        # Express in terms of percent change
        pct_change = if_else(
          wages > 0, 
          rank_slope * pmin(100 - child_rank, ige * causal_share * delta_rank) / wages, 
          0
        ), 
        
        # Apply to all wage variables
        across(.cols = c('wages', 'wages1', 'wages2'), 
               .fns  = ~ . * (1 + pct_change))
      ) 
    
    # Write summary file
    tax_units %>% 
      group_by(parent_rank, child_rank) %>% 
      summarise(rank_slope = weighted.mean(rank_slope, weight), 
                pct_change = weighted.mean(pct_change, weight), 
                .groups = 'drop') %>% 
      write_csv(
        file.path(
          globals$output_root, 
          scenario_info$ID, 
          'static/supplemental/child_earnings', 
          paste0('outcomes_', current_year, '.csv')
        )
      )
      
    
    # Remove intermediate values 
    tax_units %<>%
      select(-child_rank, -delta_rank, -mean_wages, -rank_slope, -pct_change)
    
  }
 
  return(tax_units)
}
  
  

