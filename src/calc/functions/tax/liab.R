#---------------------------------------------------------------------
# Function to calculate individual income tax liability after credits 
#---------------------------------------------------------------------


# Set return variables for function
return_vars$calc_liab = c('nonref', 'ref', 'ref_iit', 'ref_other', 'refund', 
                          'liab_iit', 'liab_iit_net', 'number_of_credits')


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
    'liab_bc',               # (dbl) income tax liability before credits, including AMT and excess Premium Tax Credit repayment
    'ftc',                   # (dbl) value of foreign tax credit 
    'cdctc_nonref',          # (dbl) value of nonrefundable Child and Dependent Care Credit
    'ed_nonref',             # (dbl) value of nonrefundable education credit
    'savers_nonref',         # (dbl) value of nonrefundable Saver's Credit
    'caregiver_cred_nonref', # (dbl) value of nonrefundable caregiver credit
    'res_energy_cred',       # (dbl) value of nonrefundable residential energy credits
    'old_cred',              # (dbl) value of Elderly and Disabled Credit
    'ctc_nonref',            # (dbl) value of nonrefundable CTC
    'gbc',                   # (dbl) value of General Business Credit
    'prior_yr_cred',         # (dbl) value of credit for prior year minimum taxes
    'other_nonref',          # (dbl) value of other nonrefundable credits
    'ctc_ref',               # (dbl) value of refundable CTC
    'ed_ref',                # (dbl) value of refundable education credits (AOC)
    'caregiver_cred_ref',    # (dbl) value of refundable caregiver credit
    'net_ptc',               # (dbl) value of Premium Tax Credit, net of advance credit paid
    'eitc',                  # (dbl) value of EITC
    'rebate',                # (dbl) value of rebate credit
    'wage_subsidy1',         # (dbl) value of individual wage subsidy, primary earner
    'wage_subsidy2',         # (dbl) value of individual wage subsidy, primary earner
    'cdctc_ref',             # (dbl) value of refundable Child and Dependent Care Tax Credit
    'savers_ref',            # (dbl) value of refundable Saver's credit
    'liab_niit',             # (dbl) Net Investment Income Tax liability
    'liab_surtax',           # (dbl) AGI surtax liability
    'liab_seca',             # (dbl) self-employment tax liability
    'recapture_tax',         # (dbl) credit recapture
    'ira_penalty',           # (dbl) penalty paid for early withdraw from retirement account
    
    # Tax law attributes
    'credits.repeal_ftc',        # (dbl) whether to repeal Foreign Tax Credit
    'credits.repeal_res_energy', # (dbl) whether to repeal residential energy credits
    'credits.repeal_old',        # (dbl) whether to repeal credit for the elderly/disabled
    'credits.repeal_gbc',        # (dbl) whether to repeal general business credits
    'credits.repeal_other'       # (dbl) whether to repeal all other nonrefundable credits
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Remove any repealed credits
      ftc             = if_else(credits.repeal_ftc == 0,        ftc,             0),
      res_energy_cred = if_else(credits.repeal_res_energy == 0, res_energy_cred, 0),
      old_cred        = if_else(credits.repeal_old == 0,        old_cred,        0),
      gbc             = if_else(credits.repeal_gbc == 0,        gbc,             0),
      other_nonref    = if_else(credits.repeal_other == 0,      other_nonref,    0),
      
      # Limit nonrefundable credits to liability before credits
      nonref = pmin(liab_bc, ftc + 
                             cdctc_nonref + 
                             ed_nonref + 
                             savers_nonref + 
                             caregiver_cred_nonref + 
                             res_energy_cred + 
                             old_cred + 
                             ctc_nonref + 
                             gbc +
                             prior_yr_cred + 
                             other_nonref),
      
      # Calculate liability after nonrefundable credits
      liab_ac_nonref = liab_bc - nonref, 
      
      # Apply refundable credits to remaining (non-NIIT) individual income tax
      # (equivalent to E11601 on the 2015 PUF) 
      ref = ctc_ref + 
            ed_ref + 
            net_ptc + 
            eitc + 
            rebate + 
            wage_subsidy1 + 
            wage_subsidy2 + 
            cdctc_ref + 
            savers_ref + 
            caregiver_cred_ref,
      ref_iit = pmin(liab_ac_nonref, ref), 
      
      # Calculate individual income tax liability after credits 
      # (equivalent to E08800 on the 2015 PUF)  
      liab_ac = liab_ac_nonref - ref_iit,
      
      # Calculate final individual income tax liability
      # (equivalent to E06500 on the 2015 PUF)
      liab_iit = liab_ac + liab_niit + liab_surtax, 
      
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
      liab_iit_net = liab_iit - (ref_other + refund),
      
      # Reporting variable: number of credits claimed. Used for estimating the
      # time burden of filing taxes
      number_of_credits = (ftc != 0) + 
                          (cdctc_nonref + cdctc_ref != 0)  + 
                          (ed_nonref + ed_ref != 0) + 
                          (savers_nonref + savers_ref != 0) + 
                          (res_energy_cred != 0) + 
                          (old_cred != 0) + 
                          (ctc_nonref + ctc_ref != 0) + 
                          (gbc != 0) +
                          (prior_yr_cred != 0) + 
                          (other_nonref != 0) + 
                          (net_ptc != 0) + 
                          (eitc != 0) + 
                          (rebate != 0) + 
                          (wage_subsidy1 + wage_subsidy2 != 0)
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_liab)) %>% 
    return()
}
