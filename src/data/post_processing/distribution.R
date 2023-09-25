#----------------------------------------------------------------------------
# distribution.R
# 
# Post-processing functions to generate distributional tables for a scenario
#----------------------------------------------------------------------------



calc_distribution = function(global_root, id, year, pcts) {
  
  #----------------------------------------------------------------------------
  # Calculates and writes a scenario's distributional table for a given year. 
  # 
  # Parameters:
  #   - scenario_root (str) : vintage-level output folder 
  #   - id (str)            : scenario ID 
  #   - year (int)          : tax year for which to calculate distributional 
  #                           metrics
  #   - pcts (dbl[])        : A vector of percentiles used define income groups
  #
  # Returns: void. Writes a dataframe for the scenario containing values, 
  #          grouped by pcts, for: average tax change, share with tax cut, 
  #          average tax cut, share with tax increase, average tax increase, 
  #          percent change in after tax income, share of total tax change.
  #----------------------------------------------------------------------------
  
  # Read microdata output
  baseline = read_csv(file.path(global_root, 'baseline', 'detail', paste0(year, '.csv')))
  scenario = read_csv(file.path(global_root, id, 'detail', paste0(year, '.csv')))
  
  baseline %>% 
    
    # Remove dependent returns
    filter(dep_status == 0) %>% 
    
    # Pare down dataframe and join scenario liability 
    mutate(liab_baseline = liab_iit_net + liab_pr) %>%
    select(id, weight, expanded_income, liab_baseline) %>% 
    left_join(sim %>% 
                select(id, liab = liab_iit_net + liab_pr), 
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
    cut_var(pcts = pcts, 'expanded_income', 'perwt') %>%
    
    # Calculate metrics by group
    group_by(`Income group` = group) %>%
      summarise(
        group_delta = sum(delta * weight),
      
        `Average tax change`   = weighted.mean(delta, weight),
        `Average tax cut`      = weighted.mean(delta, (weight * cut)),
        `Average tax increase` = weighted.mean(delta, (weight * raise)),
      
        `Share with cut`   = sum(weight * cut)   / sum(weight),
        `Share with raise` = sum(weight * raise) / sum(weight),
      
        `Percent change in after-tax income` = sum((expanded_income - liab) * weight) / sum((expanded_income - liab_baseline) * 100)  
      ) %>%
      mutate(`Share of total tax change` = group_delta / sum(group_delta)) %>%
    
      # Clean and write to CSV
      select(`Income group`, `Average tax change`, `Share with tax cut`, 
             `Average tax cut`, `Share with tax increase`, `Average tax increase`,
             `Percent change in after-tax income`, `Share of total tax change`) %>%
      write_csv(file.path(global_root, 
                          id, 
                          'supplemental', 
                          paste0('distribution_', year, '.csv')))
  
}
