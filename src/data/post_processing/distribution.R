#-----------------------------------------------------------------------
# Post-processing functions to generate distributional tables for a scenario run
#-----------------------------------------------------------------------

calc_distro = function(sim, pcts) {
  #----------------------------------------------------------------------------
  # Produces a scenario's distributional table
  # 
  # Parameters:
  #   - sim (df) :  a dataframe containing tax simulator's microdata output which must contain the following columns
  #       - expanded_income        : Key income measurement
  #       - liab_iit_net           : Individual Income Tax Liability
  #       - liab_pr                : Payroll Tax Liability
  #   - pcts (vec) : A vector of quantile thresholds used to split expanded_income into groups. Expressed in decimal form
  #
  # Returns:  void, writes a dataframe for the scenario containing values, grouped by pcts, for:
  #   - AverageTaxDelta (dbl) : The wighted average delta between baseline and scenario tax liability
  #   - AverageTaxCut (dbl) : The weighted average tax cut for filers whose taxes were reduced
  #   - AverageTaxRaise (dbl) : The weighted average tax raise for filers whose taxes were increased
  #   - ShareWithCut (dbl) : The weighted percentage of filers whose taxes decreased (delta greater than -$5)
  #   - ShareWithRaise (dbl) : The weighted percentage of filers whose taxes increased (delta greater than $5)
  #   - PercentChangeInAfterTaxIncome (dbl) : Dollar weighted percentage change in filer's after tax income
  #   - TotalTaxChangeSahre (dbl) : The percentage of the total tax change for 
  #   
  #----------------------------------------------------------------------------
  
  
  sim %<>% mutate(liab_sim = liab_iit_net + liab_pr)
  
  base <- read.csv(file.path("/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator",
                             version,
                             vintage,
                            "baseline"))
  
  base %<>% mutate(liab_base = liab_iit_net + liab_pr)
  
  # Select and join only the variables we need
  select(base, ID, liab_base, weight, expanded_income) %>% 
    left_join(., select(sim, ID, liab_sim), 
              by="ID") %>%
    
    # Filter out dependent returns
    filter(dep_status == 0) %>%
    mutate(
      # Round deltas to the nearest $10 increment
      delta = round(liab_sim - liab_base, -1),
      
      # Binary dummies for if a tax unit received a meaningful raise or cut
      cut = delta < -5,
      raise = delta > 5,
      
      # Create new person level weight for more representative income groups
      perwt = weight * (1 + (filing_status == 2)) 
    ) %>%
    
    # Bucket
    cut_var(pcts = pcts, 'expanded_income', 'perwt') %>%
    
    group_by(base, group) %>%
      summarise(
        group_delta = sum(delta * weight),
      
        AverageTaxDelta = weighted.mean(delta, weight),
        AverageTaxCut = weighted.mean(delta, (weight * cut)),
        AverageTaxRaise = weighted.mean(delta, (weight * raise)),
      
        ShareWithCut = sum(cut * weight) / sum(weight),
        ShareWithRaise = sum(raise * weight) / sum(weight),
      
        PercentChangeInAfterTaxIncome = sum((expanded_income - liab_sim) * weight) / sum((expanded_income - liab_base) * 100)  
      ) %>%
      mutate(TotalTaxChangeShare = group_delta / sum(group_delta)) %>%
    
      select(group, group_delta, 
             AverageTaxDelta, AverageTaxCut, AverageTaxRaise, 
             ShareWithCut, ShareWithRaise, 
             PercentChangeInAfterTaxIncome, TotalTaxChangeShare) %>%
    
      write.csv(
        .,
        file.path(out_dir, paste0("distro_", ID, ".csv")),
        row.names = F
      )
  
}
