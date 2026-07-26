do_tips = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts tipped income to account for behavioral responses to a tax exemption
  # for tips. Models both intensive margin (existing tipped workers earning 
  # more tips) and extensive margin (more workers becoming tipped). Based on the 
  # "fringe benefits / low" scenario from TBL's 2024 "no tax on tips" report 
  # (https://budgetlab.yale.edu/research/no-tax-tips-budgetary-distributional-
  # and-tax-avoidance-considerations)
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under the static
  #                          counterfactual scenario
  #
  # Returns: tibble of tax units with post-adjustment tips values.
  #----------------------------------------------------------------------------
  
  # Define behavioral parameters and reform timing
  first_year  = 2025
  e_extensive = -0.357 / 0.91  
  e_intensive = -1.05
  
  # Get current year from the data
  current_year       = unique(tax_units$year)
  years_since_reform = max(0, current_year - first_year)

  # Skip if no income tax exemption for tips this year
  if (current_year < first_year | all(tax_units$below.tip_ded_limit == 0)) {
    return(tax_units)
  } else {
    
    # Join MTRs onto data
    new_data = tax_units %>% 
      left_join(
        baseline_mtrs %>% 
          rename_with(.cols = -c(id, year), .fn   = ~ paste0(., '_baseline')), 
        by = c('id', 'year')
      ) %>%
      left_join(static_mtrs, by = c('id', 'year'))
    
    # Calculate aggregate behavioral parameter shifts based on marginal tax rates 
    extensive_margin_increase = new_data %>%
      mutate(
        mtr_delta.1 = mtr_tips1 - mtr_tips1_baseline, 
        mtr_delta.2 = mtr_tips2 - mtr_tips2_baseline
      ) %>% 
      select(id, weight, tips.1 = tips1, tips.2 = tips2, contains('delta.')) %>% 
      pivot_longer(
        cols      = -c(id, weight), 
        names_sep = '[.]', 
        names_to  = c('name', 'index') 
      ) %>% 
      pivot_wider() %>% 
      summarise(
        total_effect = e_extensive * weighted.mean(mtr_delta, weight * tips)
      ) %>% 
      pull(total_effect)
    
    
    # Calculate the current share of tips to wages for tipped workers
    # This will be used to determine how much wage income converts to tips for newly tipped workers
    share_tips_new_tipped = new_data %>% 
      filter(tips > 0) %>% 
      summarise(share = weighted.mean(tips / wages, weight)) %>% 
      pull(share)
    
    # Calculate the percentage of wage workers who need to become tipped to achieve the 
    # extensive margin increase target
    # First, calculate the current aggregate tips and wages
    aggregates = new_data %>% 
      summarise(
        total_wages = sum(wages * weight) / 1e9, 
        total_tips  = sum(tips * weight)  / 1e9
      )
    
    # Calculate the amount of new tips needed from extensive margin
    new_tips_needed = extensive_margin_increase * aggregates$total_tips
    
    # Calculate the share of non-tipped wages that need to become tips
    # This is how we'll determine what percentage of non-tipped workers will become tipped
    non_tipped_wages = new_data %>% 
      filter(tips == 0, wages > 0) %>% 
      summarise(total = sum(wages * weight) / 1e9) %>% 
      pull(total)
    
    # Calculate the percentage of non-tipped workers who need to become tipped
    # to achieve our target extensive margin increase
    extensive_margin_p = (new_tips_needed / non_tipped_wages) / share_tips_new_tipped
    
    new_data = new_data %>%
      mutate(
        
        # First, process the intensive margin - increase tips for those who already have them, 
        # limited to cumulative wage growth since enactment, reflecting downward nominal wage rigidity
        tips1 = tips1 * pmin(1.03 ^ years_since_reform, 1 + e_intensive * (mtr_tips1 - mtr_tips1_baseline)),
        tips2 = tips2 * pmin(1.03 ^ years_since_reform, 1 + e_intensive * (mtr_tips2 - mtr_tips2_baseline)),         
        
        # Then, extensive margin: simulate tipping for both earners
        become_tipped1 = tips1 == 0 & wages1 > 0 & r.behavior1 < extensive_margin_p,
        become_tipped2 = tips2 == 0 & wages2 > 0 & r.behavior2 < extensive_margin_p & filing_status == 2,
        
        # Estimate new tips using the calculated share of tips to wages for tipped workers
        new_tips1 = if_else(become_tipped1, wages1 * share_tips_new_tipped, 0),
        new_tips2 = if_else(become_tipped2, wages2 * share_tips_new_tipped, 0),
        
        # Add to tips
        tips1 = tips1 + new_tips1, 
        tips2 = tips2 + new_tips2, 
        
        # Update overall tips
        tips = tips1 + tips2
      )
    
    # Keep adjusted variables and return
    tax_units %>% 
      mutate(tips1 = new_data$tips1, tips2 = new_data$tips2, tips = new_data$tips) %>% 
      return()
  }
}
