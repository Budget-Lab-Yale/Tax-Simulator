#-------------------------------------------
# Function to calculate itemized deductions
#-------------------------------------------

# Set return variables for function
return_vars$calc_item_ded = c('med_item_ded', 'salt_item_ded', 'mort_int_item_ded', 
                              'inv_int_item_ded', 'int_item_ded', 'char_item_ded', 
                              'casualty_item_ded', 'misc_item_ded', 'other_item_ded', 
                              'item_ded_ex_limits', 'item_ded')


calc_item_ded = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of itemized deductions. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - med_item_ded (dbl)       : deductible medical expenses
  #   - salt_item_ded (dbl)      : deductible state/local taxes
  #   - mort_int_item_ded (dbl)  : deductible mortgage interest
  #   - inv_int_item_ded (dbl)   : deductible investment interest expense
  #   - int_item_ded (dbl)       : deductible interest
  #   - char_item_ded (dbl)      : deductible charitable contributions
  #   - casualty_item_ded (dbl)  : deductible casualty/loss expense
  #   - misc_item_ded (dbl)      : deductible miscellaneous expenses
  #   - other_item_ded (dbl)     : other deductible expenses
  #   - item_ded_ex_limits (dbl) : total itemized deductions before limitations
  #   - item_ded (dbl)           : total itemized deductions
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'agi',              # (dbl) Adjusted Gross Income 
    'med_exp',          # (dbl) medical and dental expenses
    'salt_inc_sales',   # (dbl) greater of state/local income or sales taxes paid
    'salt_prop',        # (dbl) state/local real estate taxes paid
    'salt_pers',        # (dbl) state/local personal property taxes paid
    'first_mort_bal',   # (dbl) first mortgage balance
    'second_mort_bal',  # (dbl) second mortgage balance
    'first_mort_year',  # (dbl) year when incurred first mortgage 
    'second_mort_year', # (dbl) year when incurred secondary mortgage
    'first_mort_int',   # (dbl) interest paid this year on first mortgage 
    'second_mort_int',  # (dbl) interest paid this year on second mortgage
    'inv_int_exp',      # (dbl) qualifying investment interest expense 
    'auto_int_exp',     # (dbl) auto loan interest expense
    'char_cash',        # (dbl) charitable contributions made in cash
    'char_noncash',     # (dbl) noncash charitable contributions
    'casualty_exp',     # (dbl) casualty and theft losses
    'job_exp',          # (dbl) unreimbursed employee expenses
    'tax_prep_exp',     # (dbl) tax preparation fees
    'other_misc_exp',   # (dbl) other miscellaneous expenses (i.e. Sch A line 23, pre-TCJA)
    'other_item_exp',   # (dbl) other historically itemizable deductions
    
    # Tax law attributes
    'item.med_floor_agi',           # (dbl)   AGI floor above which medical expenses are deductible
    'item.med_limit',               # (int)   maximum deductible medical expenses 
    'item.salt_limit',              # (int)   maximum deductible state and local taxes
    'item.mort_bal_limit_years[]',  # (int[]) year ranges mapping to mortgage balance limitations, specified in increasing order
    'item.mort_bal_limit[]',        # (int[]) limitations on deductible mortgage interest based on mortgage balance
    'item.mort_int_limit',          # (int)   maximum deductible mortgage interest
    'item.mort_int_non_prim',       # (int)   whether deductions for non-primary residences are allowed
    'item.prim_mort_share',         # (int)
    'item.auto_int_deduction',      # (int)   whether auto loan interest is deductible from taxable income
    'item.casualty_limit',          # (int)   maximum deductible casualty and loss expenses
    'item.misc_floor_agi',          # (dbl)   AGI floor for "miscellaneous" itemized deductions
    'item.misc_limit',              # (int)   maximum deductible "miscellaneous" itemized deductions
    'char.item_floor_agi',          # (dbl)   AGI floor for charitable deduction
    'char.item_limit',              # (int)   dollar-amount limit for itemized charitable deduction (0 reflects repeal)
    'char.item_cash_limit_agi',     # (dbl)   AGI limit for noncash contributions, itemized deduction
    'char.item_noncash_limit_agi',  # (dbl)   maximum deductible above-the-line charitable contributions
    'item.pease_thresh',            # (int)   AGI threshold above which itemized deductions are limited under Pease
    'item.pease_rate',              # (dbl)   Pease limitation phaseout rate with respect to AGI
    'item.pease_max_share',         # (dbl)   maximum Pease phaseout, expressed as percent of tentative total deductions
    'item.limit',                   # (int)   maximum value of itemized deductions,
    'item.salt_floor',              # (int)   Minimum value of SALT deduction
    'item.salt_floor_thresh'        # (int)   Income above which SALT deduction phases out towards the floor
  )
  
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    
    # Add any additional required columns related to mortgage balance limitation
    prep_item_ded() %>% 
    
    mutate(
      
      # Floor AGI at 0 for this worksheet 
      agi = pmax(0, agi),
      
      
      #------------------
      # Medical expenses
      #------------------
      
      # Apply AGI-based floor 
      med_item_ded = pmax(0, med_exp - (agi * item.med_floor_agi)),
      
      # Apply dollar-amount limitation
      med_item_ded = pmin(item.med_limit, med_item_ded),
      
      
      #------
      # SALT
      #------
      
      # Calculate deduction for state and local taxes, limited if applicable
      salt_item_ded = pmin(item.salt_limit, salt_inc_sales + salt_prop + salt_pers),
      salt_item_ded = pmax(item.salt_floor, salt_item_ded - .3 * pmax(0, agi - item.salt_floor_thresh)),
      
      #----------
      # Interest 
      #----------
      
      # Mortgage interest. First, limit based on mortgage balance. The balance
      # limitation can have multiple vintages. For example, under 2023 current law, 
      # pre-1988 mortgages are unlimited, 1988-2017 mortgages above $1M are limited,
      # and post-2017 mortgages above $750K are limited. First, we assign limitations
      # to mortgages based on year incurred:
      across(.cols  = c(first_mort_year, second_mort_year),
             .fns   = ~ case_when(
                          is.na(.)                        ~ Inf,
                          . <= item.mort_bal_limit_years1 ~ item.mort_bal_limit1, 
                          . <= item.mort_bal_limit_years2 ~ item.mort_bal_limit2,
                          . <= item.mort_bal_limit_years3 ~ item.mort_bal_limit3,
                          . <= item.mort_bal_limit_years4 ~ item.mort_bal_limit4,
                          T                               ~ item.mort_bal_limit5), 
             .names = '{str_sub(.col, end = -6)}_bal_limit'),
      
      # Then we apply balance limitations. Form logic is a bit weird. Should be 
      # very careful when modeling reforms to these parameters! First, we determine
      # the effective balance limitation for those with more than one mortgage:
      smaller_bal = pmin(first_mort_bal, second_mort_bal),
      larger_bal  = pmax(first_mort_bal, second_mort_bal),
      smaller_bal_limit = if_else(first_mort_bal < second_mort_bal, 
                                  first_mort_bal_limit, 
                                  second_mort_bal_limit),
      larger_bal_limit  = if_else(first_mort_bal < second_mort_bal, 
                                  second_mort_bal_limit, 
                                  first_mort_bal_limit),
      bal_limit = pmin(larger_bal_limit, pmax(larger_bal, smaller_bal_limit)),
      
      # Apply balance limit
      deductible_share  = pmin(1, bal_limit / (first_mort_bal + second_mort_bal)),
      mort_int_item_ded = (first_mort_int + second_mort_int) * deductible_share,  
      mort_int_item_ded = if_else(item.mort_int_non_prim == 1, mort_int_item_ded, mort_int_item_ded * item.prim_mort_share),
      
      # Then, apply overall limit
      mort_int_item_ded = pmin(item.mort_int_limit, mort_int_item_ded),
      
      # Calculate deductible investment interest expense
      inv_int_item_ded = inv_int_exp,
      
      # Calculate deductible auto loan interest expense
      auto_loan_ded = item.auto_int_deduction * auto_int_exp,
      
      # Calculate total interest deduction
      int_item_ded = mort_int_item_ded + inv_int_item_ded + auto_loan_ded,
      
      
      #---------
      # Charity
      #--------- 
      
      # Apply AGI-based limits first
      char_item_ded = pmin(pmax(0, agi) * char.item_cash_limit_agi, char_cash) +
                      pmin(pmax(0, agi) * char.item_noncash_limit_agi, char_noncash),
      
      # Apply AGI-based floor
      char_item_ded = pmax(0, char_item_ded - (max(0, agi) * char.item_floor_agi)),
      
      # Apply dollar-amount limitation
      char_item_ded = pmin(char.item_limit, char_item_ded),
      
      
      #-------
      # Other 
      #-------
      
      # Deduction for casualty/theft losses
      casualty_item_ded = pmin(item.casualty_limit, casualty_exp),
      
      # Job and miscellaneous deductions historically subject to AGI-based floor
      misc_item_ded = job_exp + tax_prep_exp + other_misc_exp, 
      
      # Apply AGI-based floor, then dollar-amount limitation
      misc_item_ded = pmax(0, misc_item_ded - (agi * item.misc_floor_agi)),
      misc_item_ded = pmin(item.misc_limit, misc_item_ded), 
      
      # Other deductions
      other_item_ded = other_item_exp,
      
      #-----------------------
      # Total and limitations
      #-----------------------
      
      # Calculate tentative total itemized deductions
      item_ded = med_item_ded + salt_item_ded + int_item_ded + char_item_ded + 
                 casualty_item_ded + misc_item_ded + other_item_ded,
      
      # Record itemized deductions before limitations
      item_ded_ex_limits = item_ded,
      
      # Apply Pease limitation: start with AGI-based phaseout...  
      pease_reduction = pmax(0, agi - item.pease_thresh) * item.pease_rate,
  
      # ...then limit to some share of tentative deductions, adjusted to
      # reflect exempt categories: medical, investment interest, casualty and loss
      pease_base       = pmax(0, item_ded - med_item_ded - inv_int_exp - casualty_item_ded),
      pease_limitation = pmin(item.pease_max_share * pease_base, pease_reduction),
      
      # Apply to itemized deductions
      item_ded = item_ded - pease_limitation, 
      
      # Apply dollar-amount limitation 
      item_ded = pmin(item.limit, item_ded)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_item_ded)) %>% 
    return()
}



prep_item_ded = function(tax_unit) {

  #----------------------------------------------------------------------------
  # Itemized deduction calculator function requires exactly 5 mortgage balance
  # limitation columns. We want to support the user specifying fewer than 5. 
  # This helper function prepares the tax unit dataframe for calculation by  
  # adding those columns if missing. 
  # 
  # Not the most elegant approach, but it's exceedingly unlikely we'll be
  # simulating a policy with more than 5. If the user specifies more than 5, 
  # an exception is thrown.
  # 
  # Parameters:
  #   - tax_unit (df) : dataframe of tax unit and tax law attribute
  # 
  # Returns: same dataframe, appended with new columns for unspecified 
  #          mortgage balance limitation tax law variables (df).
  #----------------------------------------------------------------------------
  
  # Calculate number of mortgage balance limitations vintages. (2023 law has 3).
  # The code is written to handle up to 5. Here, we throw an exception if more
  # than 5 are supplied, in which case the user must augment the calculation code.
  n_ranges = get_n_cols(tax_unit, 'item.mort_bal_limit_years')
  if (n_ranges > 5) {
    stop('More than 5 mortgage balance limitation year ranges supplied')
    
  # Append unspecified columns  
  } else if (n_ranges < 5) {
    
    # Add 1-suffix in case of single range
    if (n_ranges == 1) {
      tax_unit %<>% 
        rename_with(.cols = contains('item.mort_bal_limit'), 
                    .fn   = ~ paste0(., 1))
    }
    
    # Code below requires 5 range columns. Add columns n+1 through 5:  
    tax_unit %<>% 
      bind_cols(
        expand_grid(index  = (1:5)[!(1:5 %in% 1:n_ranges)], 
                    prefix = c('item.mort_bal_limit_years', 'item.mort_bal_limit')) %>% 
          mutate(name  = paste0(prefix, index), 
                 value = NA) %>% 
          select(name, value) %>% 
          pivot_wider(names_from  = name, 
                      values_from = value)
      )
  }
  
  
  return(tax_unit)
}

