#----------------------------------------------------------------------------
# distribution.R
# 
# Post-processing functions to generate distributional tables for a scenario
#----------------------------------------------------------------------------




calc_distribution = function(id, pcts = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 0.999)) {
  
  #----------------------------------------------------------------------------
  # Calculates and writes a scenario's distributional table for a given vector
  # of years.
  # 
  # Parameters:
  #   - id (str)     : scenario ID 
  #   - pcts (dbl[]) : A vector of percentiles used define income groups
  #
  # Returns: void. Writes a dataframe for the scenario containing values, 
  #          grouped by pcts, for: average tax change, share with tax cut, 
  #          average tax cut, share with tax increase, average tax increase, 
  #          percent change in after tax income, share of total tax change.
  #----------------------------------------------------------------------------

  for (year in get_scenario_info(id)$years) {

    # Read microdata output
    baseline = file.path(globals$baseline_root, 
                         'baseline', 
                         'static', 
                         'detail', 
                         paste0(year, '.csv')) %>% 
      fread() %>% 
      tibble()
    
    scenario = file.path(globals$output_root, 
                         id, 
                         'static', 
                         'detail', 
                         paste0(year, '.csv')) %>% 
      fread() %>% 
      tibble()
    
    
    baseline %>% 
      
      # Remove dependent returns
      filter(dep_status == 0) %>% 
      
      # Pare down dataframe and join scenario liability 
      mutate(liab_baseline = liab_iit_net + liab_pr) %>%
      select(id, weight, filing_status, expanded_inc, liab_baseline) %>% 
      left_join(scenario %>% 
                  mutate(liab = liab_iit_net + liab_pr) %>% 
                  select(id, liab), 
                by = 'id') %>%
      
      mutate(
        
        # Round deltas to the nearest $10 increment
        delta = round(liab - liab_baseline, -1),
        
        # Binary dummies for if a tax unit received a meaningful raise or cut
        cut   = delta <= -10,
        raise = delta >= 10,
        
        # Create new person level weight for more representative income groups
        perwt = weight * (1 + (filing_status == 2))
        
      ) %>%
      
      # Assign income groups
      cut_var(pcts = pcts, 'expanded_inc', 'perwt') %>%
      
      # Calculate metrics by group
      group_by(`Income group` = group) %>%
        summarise(
          group_delta = sum(delta * weight),
        
          `Average tax change`   = weighted.mean(delta, weight),
          `Average tax cut`      = weighted.mean(delta, (weight * cut)),
          `Average tax increase` = weighted.mean(delta, (weight * raise)),
        
          `Share with tax cut`   = sum(weight * cut)   / sum(weight),
          `Share with tax increase` = sum(weight * raise) / sum(weight),
        
          `Percent change in after-tax income` = sum((expanded_inc - liab) * weight) / sum((expanded_inc - liab_baseline) * weight) - 1 
        ) %>%
        mutate(`Share of total tax change` = group_delta / sum(group_delta)) %>%
      
        # Clean and write to CSV
        select(`Income group`, `Average tax change`, `Share with tax cut`, 
               `Average tax cut`, `Share with tax increase`, `Average tax increase`,
               `Percent change in after-tax income`, `Share of total tax change`) %>%
        write_csv(file.path(globals$output_root, 
                            id,
                            'static',
                            'supplemental', 
                            paste0('distribution_', year, '.csv')))
  }
  
}
