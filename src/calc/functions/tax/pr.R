#-------------------------------------
# Function to calculate payroll taxes
#-------------------------------------


calc_pr = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates employment taxes for all individuals in the tax unit.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - se1          (dbl) : self-employment income for primary earner
  #   - se2          (dbl) : self-employment income for secondary earner
  #   - se           (dbl) : self-employment income for tax unit
  #   - ei           (dbl) : earned income           
  #   - liab_fica    (dbl) : FICA tax liability
  #   - liab_seca    (dbl) : SECA tax liability 
  #   - liab_seca    (dbl) : employer-side SECA tax liability 
  #   - liab_oasdi   (dbl) : OASDI tax liability
  #   - liab_hi      (dbl) : Medicare tax liability
  #   - liab_add_med (dbl) : Additional Medicare Tax liability 
  #   - liab_pr_ee   (dbl) : employees' share of payroll tax liability
  #   - liab_pr_er   (dbl) : employers' share of payroll tax liability
  #   - liab_pr      (dbl) : payroll tax liability for tax unit 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'wages1',          # (dbl) W2 wages (box 1 on W2), primary earner 
    'wages2',          # (dbl) W2 wages (box 1 on W2), secondary earner
    'trad_contr_er_1', # (dbl) pretax contributions to an employer-sponsored tax-preferred savings account, primary earner
    'trad_contr_er2',  # (dbl) pretax contributions to an employer-sponsored tax-preferred savings account, secondary earner
    'sole_prop1',      # (dbl) Schedule C net income, primary earner 
    'sole_prop2',      # (dbl) Schedule C net income, secondary earner 
    'farm1',           # (dbl) Schedule F net income, primary earner 
    'farm2',           # (dbl) Schedule F net income, secondary earner 
    'part_se1',        # (dbl) partnership income subject to employment tax, primary earner
    'part_se2',        # (dbl) partnership income subject to employment tax, secondary earner
    'filing_status',   # (int) filing status (1 = single, 2 = joint, 3 = married, filing separately, 4 = head of household) 
    
    # Tax law attributes
    'pr.seca_taxable_rate',   # (dbl)   Share of self-employment earnings subject to SECA tax
    'pr.se_thresh',           # (int)   Threshold above which total self-employment income must be reported
    'pr.oasdi_ee_rates[]',    # (dbl[]) OASDI rates, employee side 
    'pr.oasdi_ee_brackets[]', # (int[]) OASDI brackets, employee side
    'pr.oasdi_er_rates[]',    # (dbl[]) OASDI rates, employer side 
    'pr.oasdi_er_brackets[]', # (int[]) OASDI brackets, employer side
    'pr.hi_ee_rates[]',       # (dbl[]) HI rates, employee side 
    'pr.hi_ee_brackets[]',    # (int[]) HI brackets, employee side
    'pr.hi_er_rates[]',       # (dbl[]) HI rates, employer side 
    'pr.hi_er_brackets[]',    # (int[]) HI brackets, employer side
    'pr.add_med_rates[]',     # (dbl[]) Additional Medicare Tax rates
    'pr.add_med_brackets[]'   # (int[]) Additional Medicare Tax brackets
  )
  
  
  # Payroll taxes vary along many dimensions: FICA/SECA, OASDI/HI, employer/
  # employee, and primary/secondary earner in the tax unit. We need liability
  # for all combinations of these. To do so without writing 16 near-identical
  # lines of code, we apply integrate_rates_brackets() for each combination.
  # Here, we generate the function arguments which vary across these dimensions 
  # and store as list, which is later called by pmap() 
  inputs = tibble(tax = c('fica', 'seca')) %>% 
    expand_grid(fund  = c('oasdi', 'hi'),
                side  = c('ee', 'er'),
                filer = 1:2) %>% 
    mutate(brackets_prefix = paste0(if_else(tax == 'fica', '',  
                                            if_else(filer == 1, 
                                                    'primary_', 
                                                    'secondary_')),
                                    'pr.', fund, '_', side, '_brackets'), 
           rates_prefix = paste0('pr.', fund, '_', side, '_rates'), 
           inc_name     = paste0(if_else(tax == 'fica', 'gross_wages', 'txbl_se'), 
                                 filer), 
           output_name  = paste0('liab_', tax, '_', fund, '_', side, filer)) %>% 
    select(-fund, -side, -filer)
  
  fica_inputs = inputs %>% 
    filter(tax == 'fica') %>% 
    select(-tax) %>% 
    as.list()
  
  seca_inputs = inputs %>% 
    filter(tax == 'seca') %>% 
    select(-tax) %>% 
    as.list()
  
  # Now, tax calculation:
  tax_unit %>%
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Temporarily set secondary-earner variables to 0 for non-joint tax units
      across(.cols = c(wages2, pretax_contr2, sole_prop2, farm2, part_se2), 
             .fns  = ~ replace_na(., 0)), 
      
      # Calculate FICA-eligible wages
      gross_wages1 = wages1 + trad_contr_er1, 
      gross_wages2 = wages2 + trad_contr_er2, 
      gross_wages  = gross_wages1 + gross_wages2, 
      
      # Calculate SECA-eligible earnings
      se1 = sole_prop1 + farm1 + part_se1,
      se2 = sole_prop2 + farm2 + part_se2,
      se  = se1 + se2,
      
      # Calculate taxable self-employment earnings: because employer-side taxes 
      # can be thought of as labor compensation, SECA allows a deduction for 
      # employer-side taxes. The math in the tax code is actually "wrong": (1) per 
      # that logic, it should be 1 / (1 + tau), not 1 - tau; (2) the the OASDI
      # deduction rate applies to all SE income, not just that below the cap. 
      # See this link for further explanation : 
      # https://www.sfmagazine.com/articles/2017/august/how-the-self-employment-tax-is-miscalculated/
      # For this reason, we parameterize the taxable share as its own parameter, 
      # useful for unrelated reforms and reforms meant to fix this mistake. 
      # In addition, set taxable SE earnings to 0 if if tax's unit total is below
      # the reporting threshold
      txbl_se1 = pmax(0, se1) * pr.seca_taxable_rate * (se > pr.se_thresh),
      txbl_se2 = pmax(0, se2) * pr.seca_taxable_rate * (se > pr.se_thresh),
      
      # Calculate taxable earnings for Additional Medicare Tax
      txbl_inc_add_med = gross_wages + txbl_se1 + txbl_se2, 
      
      # Calculate total earned income, used elsewhere in tax code
      ei1 = wages1 + se1, 
      ei2 = wages2 + se2,
      ei  = ei1 + ei2
      
    ) %>% 
  
    # Calculate FICA tax for (OASDI/HI) X (employee/employer) X (primary/secondary) 
    bind_cols(
      pmap(.f = integrate_rates_brackets, 
           .l = fica_inputs,
            df = (.), 
            n_brackets = NULL, 
            by_bracket = F) %>% 
        bind_cols()
    ) %>%

    # Update brackets for SECA: the idea is that self-employment income "stacks"
    # after wages in determining which rate/bracket applies
    mutate(
      across(.cols  = c(contains('ee_brackets'), contains('er_brackets')), 
             .fns   = list('primary'   = ~ pmax(0, . - gross_wages1), 
                           'secondary' = ~ pmax(0, . - gross_wages2)), 
             .names = '{fn}_{col}')
    ) %>% 
    
    # Calculate SECA tax for (OASDI/HI) X (employee/employer) X (primary/secondary) 
    bind_cols(
      pmap(.f = integrate_rates_brackets, 
           .l = seca_inputs,
           df = (.), 
           n_brackets = NULL, 
           by_bracket = F) %>% 
        bind_cols()
    ) %>%
    
    # Calculate Additional Medicare tax
    bind_cols(
      integrate_rates_brackets(df              = (.), 
                               n_brackets      = NULL, 
                               brackets_prefix = 'pr.add_med_brackets', 
                               rates_prefix    = 'pr.add_med_rates', 
                               inc_name        = 'txbl_inc_add_med', 
                               output_name     = 'liab_add_med', 
                               by_bracket      = F)
    ) %>%
    
    mutate(
      
      # Aggregate payroll tax variables
      liab_fica    = liab_fica_oasdi_ee1 + liab_fica_oasdi_ee2 + 
                     liab_fica_hi_ee1    + liab_fica_hi_ee2 + 
                     liab_fica_oasdi_er1 + liab_fica_oasdi_er2 + 
                     liab_fica_hi_er1    + liab_fica_hi_er2, 
      liab_seca    = liab_seca_oasdi_ee1 + liab_seca_oasdi_ee2 + 
                     liab_seca_hi_ee1    + liab_seca_hi_ee2 +
                     liab_seca_oasdi_er1 + liab_seca_oasdi_er2 + 
                     liab_seca_hi_er1    + liab_seca_hi_er2,
      liab_seca_er = liab_seca_oasdi_er1 + liab_seca_oasdi_er2 +
                     liab_seca_hi_er1    + liab_seca_hi_er2, 
      liab_oasdi   = liab_fica_oasdi_ee1 + liab_fica_oasdi_ee2 + 
                     liab_seca_oasdi_ee1 + liab_seca_oasdi_ee2 +
                     liab_fica_oasdi_er1 + liab_fica_oasdi_er2 + 
                     liab_seca_oasdi_er1 + liab_seca_oasdi_er2,
      liab_hi      = liab_fica_hi_ee1 + liab_fica_hi_ee2 + 
                     liab_seca_hi_ee1 + liab_seca_hi_ee2 + 
                     liab_fica_hi_er1 + liab_fica_hi_er2 + 
                     liab_seca_hi_er1 + liab_seca_hi_er2 + liab_add_med,
      liab_pr_ee   = liab_fica_oasdi_ee1 + liab_fica_oasdi_ee2 + 
                     liab_fica_hi_ee1    + liab_fica_hi_ee2 + 
                     liab_seca_oasdi_ee1 + liab_seca_oasdi_ee2 + 
                     liab_seca_hi_ee1    + liab_seca_hi_ee2,
      liab_pr_er   = liab_fica_oasdi_er1 + liab_fica_oasdi_er2 + 
                     liab_fica_hi_er1    + liab_fica_hi_er2 + 
                     liab_seca_oasdi_er1 + liab_seca_oasdi_er2 + 
                     liab_seca_hi_er1    + liab_seca_hi_er2,
      liab_pr      = liab_oasdi + liab_hi,     
      
      # Set secondary-earner output variables to NA for non-joint tax units
      se2 = if_else(filing_status != 2, NA, se2)
  
    ) %>%

    # Keep variables to return
    select(se1, se2, se, ei, liab_fica, liab_seca, liab_seca_er, liab_oasdi, 
           liab_hi, liab_add_med, liab_pr_ee, liab_pr_er, liab_pr) %>%
    return()
}

