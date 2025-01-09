#----------------------------------------------------------------
# Contains functions to calculate tax liability for all tax units
#----------------------------------------------------------------


do_taxes = function(tax_units, baseline_pr_er, vars_1040, vars_payroll) { 

  #----------------------------------------------------------------------------
  # Calculates payroll and individual income taxes for all tax units. Form-
  # behavior optimization in which the filer needs to calculate taxes more
  # than once is performed here (rather than inside of a calc function.) 
  # Currently supports charitable contribution reporting optimization when
  # a deduction is available both above the line and on Schedule A.
  # 
  # Parameters:
  #   - tax_units (df)       : tibble of tax units, exogenous variables only
  #   - baseline_pr_er (df)  : tibble of baseline employer-side payroll 
  #                            liabilities. NULL if baseline
  #   - vars_1040 (str[])    : vector of (calculated) names of 1040 variables  
  #                            to return
  #   - vars_payroll (str[]) : vector of (calculated) names of payroll tax  
  #                            variables to return
  # 
  # Returns: tibble of tax units with new columns for calculated tax variables
  #          (df).  
  #----------------------------------------------------------------------------
      
  
  # Derive useful policy-independent variables
  tax_units %<>% 
    derive_vars()
  
  
  #---------------
  # Payroll taxes
  #---------------
  
  if (!is.null(baseline_pr_er)) {
    
    # Calculate first-order change in employer-side payroll taxes
    pr_changes = tax_units %>% 
      bind_cols(do_payroll_taxes(., vars_payroll)) %>% 
      left_join(baseline_pr_er, by = 'id') %>% 
      select(id, baseline1, liab_fica_er1, baseline2, liab_fica_er2)
    
    # Adjust wages so as to hold total labor compensation fixed. Assume that 85% 
    # of the marginal non-payroll tax compensation is in nontaxable fringe benefits
    tax_units %<>%
      left_join(pr_changes, by = 'id') %>% 
      mutate(
        wages1 = wages1 * if_else(
          wages1 != 0, 
          1 + 0.85 * ((1 + baseline1 / wages1) / (1 + liab_fica_er1 / wages1) - 1), 
          1
        ),
        wages2 = wages2 * if_else(
          wages2 != 0, 
          1 + 0.85 * ((1 + baseline2 / wages2) / (1 + liab_fica_er2 / wages2) - 1), 
          1
        ),
        wages  = wages1 + wages2) %>% 
      select(-baseline1, -liab_fica_er1, -baseline2, -liab_fica_er2)
  }
  
  # Do payroll taxes
  tax_units %<>% 
    bind_cols(do_payroll_taxes(., vars_payroll)) 
  
  
  #-------------------------
  # Individual income taxes
  #-------------------------
  
  # Check whether individual income taxes need to be calculated more than once.
  # If both an above-the-line and an itemized charitable deduction are available,
  # the taxpayer chooses between the two by calculating their taxes twice
  if (any(tax_units$char.above_limit > 0 & tax_units$char.item_limit > 0)) { 
    
    # Force filers to take the above-the-line deduction
    above = tax_units %>% 
      do_1040(return_vars = vars_1040,
              force_char  = T, 
              char_above  = T) %>% 
      mutate(id            = tax_units$id, 
             char_ded_type = 'above')
    
    # Force filers to take the itemized deduction
    item = tax_units %>% 
      do_1040(return_vars = vars_1040, 
              force_char  = T, 
              char_above  = F) %>% 
      mutate(id            = tax_units$id,
             char_ded_type = 'item')
    
    # Determine which is better
    opt = above %>% 
      select(above = liab_iit_net) %>% 
      bind_cols(
        item %>% 
          select(item = liab_iit_net)
      ) %>% 
      mutate(char_ded_type = if_else(above <= item, 'above', 'item')) %>% 
      select(char_ded_type)
    
    # Select optimized answer
    tax_units %<>%
      bind_cols(opt) %>%
      left_join(bind_rows(above, item), by = c('id', 'char_ded_type')) %>% 
      select(-char_ded_type)
    
  # Standard case: just calculate the 1040 once  
  } else {
    tax_units %<>% 
      bind_cols(do_1040(., vars_1040))
  }


  #----------------
  # Add other vars 
  #----------------
  

  tax_units %<>%
    mutate(
      
      # Update filer status
      filer = filer + (become_filer_ctc == 1 | become_filer_rebate == 1),
      
      # Expanded income metric for distributional tables: gross realized income 
      # plus employer's share of payroll taxes
      expanded_inc = wages + trad_contr_er1 + trad_contr_er2 + txbl_int + 
                     exempt_int + div_ord + div_pref + state_ref + 
                     txbl_ira_dist + gross_pens_dist + kg_st + kg_lt + 
                     other_gains + alimony + sole_prop + sch_e + farm + 
                     gross_ss + ui + other_inc + salt_workaround_part + 
                     salt_workaround_scorp + liab_pr_er, 
      
      # Whether taxpayer is a "simple filer" -- one in which the IRS could 
      # plausibly pre-file a return on behalf of the tax unit. Criteria is:
      # non-itemizers whose income is derived solely from wages or OASDI
      simple_filer = as.integer(
        filer & 
        !itemizing &
        txbl_int        == 0 & 
        div_ord         == 0 & 
        div_pref        == 0 & 
        state_ref       == 0 &  
        txbl_ira_dist   == 0 & 
        gross_pens_dist == 0 & 
        kg_st           == 0 & 
        kg_lt           == 0 &  
        other_gains     == 0 &
        alimony         == 0 & 
        sole_prop       == 0 & 
        sch_e           == 0 & 
        farm            == 0 & 
        ui              == 0
      )
    )
  
  # Attach AGI bracketed liability variables for post-processing
  tax_units %<>%
      bind_cols(
        integrate_rates_brackets(
          df              = .,
          n_brackets      = NULL, 
          prefix_brackets = 'ord.brackets', 
          prefix_rates    = 'ord.rates', 
          y               = 'agi',
          output_name     = 'liab_brac', 
          by_bracket      = T
        )
    )
  
  # Set "corporate tax change", a variable used to measure the off-model 
  # corporate tax revenue changes owing to business entity shifting, to 0 if 
  # not running business entity shifting behavioral feedback
  if (!('corp_tax_change' %in% colnames(tax_units))) {
    tax_units %<>% 
      mutate(corp_tax_change = 0)
  }
  
  
  
  #----------------
  # Model payments
  #----------------
  
  tax_units %>% 
    remit_taxes() %>% 
    return()
}



do_payroll_taxes = function(tax_units, return_vars) {
  
  #----------------------------------------------------------------------------
  # Calculates payroll taxes for all tax units. Currently just a wrapper for 
  # calc_pr(), but written this way for consistency with do_1040().
  # 
  # Parameters:
  #   - tax_units (df)      : tibble of tax units
  #   - return_vars (str[]) : vector of (calculated) names of variables to 
  #                           return
  #
  # Returns: tibble of tax units with selected calculate variables to return.
  #----------------------------------------------------------------------------
  
  tax_units %>% 
    calc_pr() %>%
    select(all_of(return_vars)) %>% 
    return()
}



do_1040 = function(tax_units, return_vars, force_char = F, char_above = F) {
  
  #----------------------------------------------------------------------------
  # Calculates individual income taxes for all tax units. 
  # 
  # Parameters:
  #   - tax_units (df)      : tibble of tax units
  #   - return_vars (str[]) : vector of (calculated) names of variables to 
  #                           return
  #   - force_char (bool)   : whether the calculator forces filers to report 
  #                           their charitable contribution in a specific,
  #                           mutally exclusive place on the 1040
  #   - char_above (bool)   : if forcing charitable contribution reporting, 
  #                           whether to report as an above-the-line deduction 
  #                           (F indicates reporting as itemized deduction)
  #
  # Returns: tibble of tax units with columns for all calculated variables. 
  #----------------------------------------------------------------------------
  
  
  # Create tibble-length vectors for form-behavior booleans 
  force_char = rep(force_char, nrow(tax_units))
  char_above = rep(char_above, nrow(tax_units))
  

  set.seed(globals$random_seed)
  
  tax_units %>% 
    
    mutate(sstb_sole_prop = as.integer(runif(nrow(.)) < 0.2), 
           sstb_part      = as.integer(runif(nrow(.)) < 0.2), 
           sstb_scorp     = as.integer(runif(nrow(.)) < 0.2)) %>% 
    
    # Create duplicates for variables affected by form-behavior optimization
    mutate(across(.cols  = c(char_cash, char_noncash), 
                  .fns   = ~ ., 
                  .names = '{col}_')) %>%  
    
    #-----------------------
    # Adjusted gross income
    #-----------------------
    
    # Net capital gain includable in AGI
    bind_cols(calc_kg(.)) %>% 
      
    # AGI, including taxable OASI benefits
    mutate(across(.cols = c(char_cash, char_noncash), 
                  .fns  = ~ if_else(force_char & !char_above, 0, .))) %>% 
    bind_cols(calc_agi(.)) %>% 
    mutate(char_cash    = char_cash_, 
           char_noncash = char_noncash_) %>% 
      
      
    #----------------
    # Taxable income 
    #----------------
    
    # Standard deduction
    bind_cols(calc_std_ded(.)) %>% 
      
    # Itemized deductions
    mutate(across(.cols = c(char_cash, char_noncash), 
                  .fns  = ~ if_else(force_char & char_above, 0, .))) %>%
    bind_cols(calc_item_ded(.)) %>% 
    mutate(char_cash    = char_cash_, 
           char_noncash = char_noncash_) %>% 
    
    # Personal exemptions
    bind_cols(calc_pe_ded(.)) %>% 
    
    # QBI deduction
    bind_cols(calc_qbi_ded(.)) %>% 
    
    # Taxable income and itemizer status
    bind_cols(calc_txbl_inc(.)) %>% 
    
    # Set itemized deduction variables to 0 for nonitemizers
    mutate(across(.cols = c('med_item_ded', 'salt_item_ded', 'mort_int_item_ded', 
                            'inv_int_item_ded', 'int_item_ded', 'char_item_ded', 
                            'casualty_item_ded', 'misc_item_ded', 'other_item_ded', 
                            'item_ded_ex_limits', 'item_ded'), 
                  .fns  = ~ if_else(itemizing, ., 0))) %>% 
      
      
    #--------------------------
    # Liability before credits
    #--------------------------
    
    # Liability
    bind_cols(calc_tax(.)) %>%
    
    # Alternative minimum tax
    bind_cols(calc_amt(.)) %>% 
    
      
    #---------
    # Credits
    #---------
    
    # CDCTC
    bind_cols(calc_cdctc(.)) %>% 
    
    # Education credits
    bind_cols(calc_ed_cred(.)) %>% 
    
    # Saver's credit
    bind_cols(calc_savers_cred(.)) %>% 
    
    # Caregiver credit
    # bind_cols(calc_caregiver_cred(.)) %>% 
    mutate(
      caregiver_cred_nonref = 0, 
      caregiver_cred_ref    = 0, 
    ) %>%
    
    # CTC
    bind_cols(calc_ctc(.)) %>% 
    
    # EITC
    bind_cols(calc_eitc(.)) %>% 
    
    # Rebates / UBI
    bind_cols(calc_rebate(.)) %>%
    
    # Wage subsidy
    bind_cols(calc_wage_subsidy(.)) %>%
    
      
    #----------------------
    # Liability allocation
    #----------------------
    
    # NIIT
    bind_cols(calc_niit(.)) %>% 
    
    # AGI surtax
    bind_cols(calc_agi_surtax(.)) %>%
      
    # Liability
    bind_cols(calc_liab(.)) %>% 
    
    # Select variables and return
    select(all_of(return_vars)) %>%
    return()
} 



remit_taxes = function(tax_units) { 
  
  #----------------------------------------------------------------------------
  # Models tax payments as a function of liability and income composition.
  # Splits payments into withheld or paid quarterly vs nonwithheld. Assumes 
  # tax on interest, dividends, and capital gains are paid during filing 
  # season. A simple heuristic that we can improve if intra-year receipts 
  # dynamics are ever a focus of analysis.
  # 
  # Note: the end-purpose of this function is to generate a level projection 
  # of aggregate receipts. For that reason, individual-level variables may not 
  # have an obvious interpretation. For example, refundable credits used to 
  # reduce "other taxes" are credited to individual income tax here, meaning
  # that a taxpayer's income tax payment might be negative. The reason for
  # this treatment is to reflect, at the aggregate level, general revenue
  # transfers from individual income taxes to OASI/HI trust funds -- even 
  # though, at the individual level, IRS treats refundable credits as reducing 
  # SECA.
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units
  #
  # Returns: tibble of tax units with the following new columns:
  #   - pmt_iit_nonwithheld (dbl)    : income tax paid at time of filing
  #   - pmt_iit_withheld (dbl)       : income tax withheld or paid quarterly
  #   - pmt_refund_nonwithheld (dbl) : payments for refundable credits paid 
  #                                    during filing season
  #   - pmt_refund_withheld (dbl)    : advance credits paid throughout year
  #   - pmt_pr_nonwithheld (dbl)     : payroll tax paid at time of filing
  #   - pmt_pr_withheld (dbl)        : payroll tax withheld (FICA) or paid 
  #                                    quarterly (SECA) 
  #----------------------------------------------------------------------------
  
  tax_units %>% 
    mutate(
      
      # Calculate non-withheld share of AGI
      inc_nonwithheld       = txbl_int + div_ord + div_pref + txbl_kg,
      iit_share_nonwithheld = if_else(agi == 0, 
                                      0,  
                                      pmin(1, pmax(0, inc_nonwithheld / agi))),
      
      # Calculate income tax liability net of general revenue transfers 
      # (see function documentation) 
      pmt_iit = liab_iit - ref_other, 
      
      # Allocate income tax payments 
      pmt_iit_nonwithheld = pmt_iit * iit_share_nonwithheld,
      pmt_iit_withheld    = pmt_iit - pmt_iit_nonwithheld,
      
      # Allocate refund payments. Can use withheld to model advance credits
      # (a la 2021 CTC) in the future
      pmt_refund_nonwithheld = refund,
      pmt_refund_withheld    = 0,
      
      # Allocate payroll tax payments
      pmt_pr_nonwithheld = 0, 
      pmt_pr_withheld    = liab_pr
      
    ) %>% 
    
    # Remove intermediate calculation variables
    select(-inc_nonwithheld, -iit_share_nonwithheld, -pmt_iit) %>%
    return()
}



calc_mtrs = function(tax_units, actual_liab_iit, actual_liab_pr, var, pr = T, 
                     type = 'nextdollar') {
  
  #----------------------------------------------------------------------------
  # Calculates MTR, either at the next-dollar or 0-actual extensive margin, 
  # with respect to given variable. Includes employee-side payroll taxes. 
  #
  # Note: for next-dollar MTRs, variable "wages" means non-tip, non-OT wages.
  # For extensive margin MTRs, variable "wages" means all wages, including 
  # tips and OT.
  # 
  # Always double-check whether the variable you want a tax rate for is handled
  # by the logic below!! It's not generalized to every variable due to 
  # compositional issues (i.e. some variables are components of others).
  # 
  # Parameters:
  #   - tax_units (df)          : tibble of tax units, exogenous variables only
  #   - actual_liab_iit (dbl[]) : vector of net income tax liability to compare
  #                               against
  #   - actual_liab_pr (dbl[])  : vector of total payroll tax to compare 
  #                               against
  #   - var (str)               : name of variable to increment
  #   - pr (bool)               : whether to include payroll taxes in the MTR
  #                               calculation
  #   - type (str)              : "nextdollar" for next-dollar MTR, "extensive"
  #                               for delta in tax when reducing the value to 0
  #
  # Returns: tibble of MTRs (df).
  #----------------------------------------------------------------------------
  
  # Set output variable name
  mtr_name = paste0('mtr_', var)
  
  # Next-dollar calculation
  if (type == 'nextdollar') {
    
    # Deal with composite variables. Initialize list of variables to increment
    vars = c(var)
    
    if (var %in% c('wages1', 'wages2')) {
      vars = c(var, 'wages')
    }
    if (var %in% c('sole_prop1', 'sole_prop2')) {
      vars = c(var, 'sole_prop')
    }
    if (var %in% c('farm1', 'farm2')) {
      vars = c(var, 'farm')
    }
    if (var == 'tips1') {
      vars = c('tips1', 'tips', 'wages1', 'wages')
    }
    if (var == 'tips2') {
      vars = c('tips2', 'tips', 'wages2', 'wages')
    }
    if (var == 'ot1') {
      vars = c('ot1', 'ot', 'wages1', 'wages')
    }
    if (var == 'ot2') {
      vars = c('ot2', 'ot', 'wages2', 'wages')
    }
    if (var %in% c('part_active', 'part_active_loss')) {
      vars = c(var, 'part_se1')
    }
    
    # Set new values
    new_values = tax_units %>% 
      mutate(
        across(.cols = all_of(vars), .fns  = ~ . + 1), 
        original_value = NA
      )
  }
  
  # Extensive-margin
  else if (type == 'extensive') {
    
    # Record initial value
    new_values = tax_units %>% 
      mutate(
        across(
          .cols  = all_of(var), 
          .fns   = ~ ., 
          .names = 'original_value'
        )
      )
    
    # Wages are funny because they contain sub-components
    if (var == 'wages1') { 
      new_values %<>%  
        mutate(
          wages  = wages - wages1,
          tips   = tips - tips1, 
          ot     = ot - ot1, 
          wages1 = 0,
          tips1  = 0, 
          ot1    = 0
        )
    } else if (var == 'wages2') {
      new_values %<>% 
        mutate(
          wages  = wages - wages2,
          tips   = tips - tips2, 
          ot     = ot - ot2, 
          wages2 = 0,
          tips2  = 0, 
          ot2    = 0
        )
    } else if (var == 'wages') {
      new_values %<>% 
        mutate(
          across(
            .cols = c(wages, wages1, wages2, tips, tips1, tips2, ot, ot1, ot2), 
            .fns  = ~ 0
          )
        )
      
    # For other variables that are subcomponents of other variables, record
    # other variables that need to be decrement
    } else {
      
      other_vars = c()
      if (var %in% c('sole_prop1', 'sole_prop2')) {
        other_vars = c('sole_prop')
      }
      if (var %in% c('farm1', 'farm2')) {
        other_vars = c('farm')
      }
      if (var %in% c('tips1', 'ot1')) {
        other_vars = c('wages1', 'wages')
      }
      if (var %in% c('tips2', 'ot2')) {
        other_vars = c('wages2', 'wages')
      }
      if (var %in% c('part_active', 'part_active_loss')) {
        other_vars = c('part_se1')
      }
      
      new_values %<>% 
        mutate(
          
          # Zero out variable
          across(
            .cols  = all_of(var), 
            .fns   = ~ 0
          ),
          
          # Decrement other variables
          across(
            .cols  = all_of(other_vars), 
            .fns   = ~ . - original_value
          )
        )
    }
  }
  
  # Invalid type
  else {
    new_values = tax_units %>% 
      mutate(across(.cols = all_of(vars), .fns  = ~ NA))
  }
  
  
  # Re-calculate taxes
  new_values %>% 
    do_taxes(
      baseline_pr_er = NULL,
      vars_payroll   = return_vars$calc_pr,
      vars_1040      = return_vars %>% remove_by_name('calc_pr') %>% unlist() %>% set_names(NULL)
    ) %>% 
    
    # Calculate MTR and return
    mutate(
      
      # Calculate numerator: change in taxes 
      delta_taxes = liab_iit_net - actual_liab_iit + pr * (liab_pr - actual_liab_pr),
      
      # Calculate denominator: change in variable value
      delta_var = case_when(
        type == 'nextdollar' ~ 1,
        type == 'extensive'  ~ if_else(original_value == 0, NA, -original_value), 
        TRUE                 ~ NA
      ), 
      
      # Calculate MTR
      !!mtr_name := delta_taxes / delta_var
      
    ) %>% 
    select(all_of(mtr_name)) %>% 
    return()
}



do_salt_workaround_baseline = function(tax_units) {
  
  #----------------------------------------------------------------------------
  # Adjusts baseline projected tax data values for SALT and pass-through 
  # income to reflect the so-called SALT cap workarounds in which pass-through
  # entities can elect to pay state income taxes at the entity level, which 
  # converts would-be SALT deductions into lower reported Schedule E net 
  # income. Conceptually, this function is tax calculation, not behavioral 
  # feedback. Whether states are allowed to offer this workaround is a policy
  # choice enshrined in law and regulation. This policy operates through what
  # ends up on tax returns. As such, it is applied to the baseline here rather
  # than in Tax-Data, and is accounted for in static runs, not just 
  # conventional runs.
  # 
  # Parameters: 
  #   - tax_units (df) : tibble of tax unit data  
  #
  # Returns: tibble of updated tax unit data (df).
  #----------------------------------------------------------------------------
  
  set.seed(globals$random_seed)
  
  
  tax_units %>% 
    mutate(
      
      # Determine SALT attributable to pass-through activities 
      part  = part_active + part_passive - part_active_loss - 
              part_passive_loss - part_179,
      scorp = scorp_active + scorp_passive - scorp_active_loss - 
              scorp_passive_loss - scorp_179,
      inc = wages + trad_contr_er1 + trad_contr_er2 + txbl_int + exempt_int + 
            div_ord + div_pref + state_ref + txbl_ira_dist + gross_pens_dist + 
            kg_st + kg_lt + other_gains + alimony + sole_prop + part + scorp + 
            farm + gross_ss + ui + other_inc,
      
      part_share  = if_else(inc > 0, pmin(1, pmax(0, part / inc)),  0),
      scorp_share = if_else(inc > 0, pmin(1, pmax(0, scorp / inc)), 0),
      
      salt_part  = salt_inc_sales * part_share,
      salt_scorp = pmin(salt_inc_sales - salt_part, salt_inc_sales * scorp_share),
      
      # Simulate amount moved in workaround. Probability calibrated to hit 
      # $20B annual estimate from TPC 
      salt_workaround_part  = salt_part * (!is.infinite(item.salt_limit) & 
                                             item.salt_workaround_allowed & 
                                             salt_part > 0 & 
                                             runif(nrow(.)) < 0.75),
      salt_workaround_scorp = salt_scorp * (!is.infinite(item.salt_limit) &
                                              item.salt_workaround_allowed & 
                                              salt_scorp > 0 & 
                                              runif(nrow(.)) < 0.75),
      
      # Shift SALT
      salt_inc_sales    = salt_inc_sales - salt_workaround_part - salt_workaround_scorp,
      part_active_loss  = part_active_loss  + salt_workaround_part,
      scorp_active_loss = scorp_active_loss + salt_workaround_scorp
    ) %>%
    
    return()
  
}

