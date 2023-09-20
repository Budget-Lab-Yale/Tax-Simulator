#---------------------------------------------------------------------
# Function to calculate individual income tax liability after credits 
#---------------------------------------------------------------------


calc_liab = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates final individual income tax liability by allocating credit 
  # values. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #  - nonref (dbl)       : total nonrefundable credits (limited to liability)
  #  - ref (dbl)          : total refundable credits (unlimited)
  #  - ref_iit (dbl)      : refundable credits used to offset income tax 
  #                         liability
  #  - ref_other (dbl)    : refundable credits used to offset other taxes
  #  - refund (dbl)       : refundable portion of refundable credit
  #  - liab_iit (dbl)     : individual income tax liability
  #  - liab_iit_net (dbl) : individual income tax liability net of refundable
  #                         credits
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'liab_bc',         # (dbl) income tax liability before credits, including AMT and excess Premium Tax Credit repayment
    'ftc',             # (dbl) value of foreign tax credit 
    'cdctc_nonref',    # (dbl) value of nonrefundable Child and Dependent Care Credit
    'ed_nonref',       # (dbl) value of nonrefundable education credit
    'savers_nonref',   # (dbl) value of nonrefundable Saver's Credit
    'res_energy_cred', # (dbl) value of nonrefundable residential energy credits
    'old_cred',        # (dbl) value of Elderly and Disabled Credit
    'car_cred',        # (dbl) value of nonrefundable vehicle credits 
    'ctc_nonref',      # (dbl) value of nonrefundable CTC
    'gbc',             # (dbl) value of General Business Credit
    'prior_yr_cred',   # (dbl) value of credit for prior year minimum taxes
    'other_nonref',    # (dbl) value of other nonrefundable credits
    'ctc_ref',         # (dbl) value of refundable CTC
    'ed_ref',          # (dbl) value of refundable education credits (AOC)
    'net_ptc',         # (dbl) value of Premium Tax Credit, net of advance credit paid
    'eitc',            # (dbl) value of EITC
    'rebate',          # (dbl) value of rebate credit
    'cdctc_ref',       # (dbl) value of refundable Child and Dependent Care Tax Credit
    'savers_ref',      # (dbl) value of refundable Saver's credit
    'liab_niit',       # (dbl) Net Investment Income Tax liability
    'liab_seca',       # (dbl) self-employment tax liability
    'recapture_tax',   # (dbl) credit recapture
    'ira_penalty'      # (dbl) penalty paid for early withdraw from retirement account
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Limit nonrefundable credits to liability before credits
      nonref = pmin(liab_bc, ftc + 
                             cdctc_nonref + 
                             ed_nonref + 
                             savers_nonref + 
                             res_energy_cred + old_cred + 
                             car_cred +  
                             ctc_nonref + 
                             gbc +
                             prior_yr_cred + 
                             other_nonref),
      
      # Calculate liability after nonrefundable credits
      liab_ac_nonref = liab_bc - nonref, 
      
      # Apply refundable credits to remaining (non-NIIT) individual income tax
      # (equiavlent to E11601 on the 2015 PUF) 
      ref_iit = pmin(liab_ac_nonref, ctc_ref +
                                     ed_ref + 
                                     net_ptc + 
                                     eitc + 
                                     rebate + 
                                     cdctc_ref + 
                                     savers_ref), 
      
      # Calculate individual income tax liability after credits 
      # (equivalent to E08800 on the 2015 PUF)  
      liab_ac = liab_ac_nonref - ref_iit,
      
      # Calculate final individual income tax liability
      # (equivalent to E06500 on the 2015 PUF)
      liab_iit = liab_ac + liab_niit, 
      
      # Calculate other taxes paid on the 1040
      liab_other = liab_seca + recapture_tax + ira_penalty,
      
      # Apply refundable credits to other taxes 
      # (equivalent to E11602 on the 2015 PUF) 
      ref_other = pmin(liab_other, ref - ref_iit),
      
      # Determine refundable component refundable credits, i.e. the amount 
      # treated as outlays rather than tax reduction. 
      # (equivalent to E11603 on the 2015 PUF) 
      refund = ref - ref_iit - ref_other,
      
      # Define a "net income tax liability" variable, which applies all refundable
      # credits against income tax liability, possibly going negative 
      liab_iit_net = liab_iit - (ref_other + refund)
      
    ) %>% 
    
    # Keep variables to return
    select(nonref, ref, ref_iit, ref_other, refund, liab_iit, liab_iit_net) %>% 
    return()
}
