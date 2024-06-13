#-----------------------------------------------------------------------------
# Function to calculate normal individual income tax liability before credits 
#-----------------------------------------------------------------------------

# Set return variables for function
return_vars$calc_tax = c('liab_ord', 'liab_pref', 'liab_1250', 'liab_collect', 
                         'liab')


calc_tax = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates tax on taxable income. 
  # 
  # The assumptions are (1) non-1250 and non-collectible gains are eligible 
  # for preferred rates, (2) those special gains are taxed at a ordinary rates 
  # up to some maximum, and (3) all other taxable income is taxed at ordinary 
  # rates. This partial parameterization allows for flexibly bracket/rate 
  # structures, but imposes this bucketing system. To model capital-gains-at-
  # ordinary-rates reform, set preferred rates equal to ordinary rates.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #  - liab_ord (dbl)     : ordinary-rate liability (excluding special gains
  #                         taxed at ordinary rates)
  #  - liab_pref (dbl)    : preferred-rate liability (excluding special gains)
  #  - liab_1250 (dbl)    : liability on section 1250 gains 
  #  - liab_collect (dbl) : liability on collectibles gains
  #  - liab (dbl)         : total liability
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'txbl_inc',    # (dbl) taxable income
    'div_pref',    # (dbl) qualified dividends
    'kg_pref',     # (dbl) preferred-rate capital gains ("net capital gain" in the internal revenue code)  
    'kg_1250',     # (dbl) section 1250 unrecaptured gain
    'kg_collect',  # (dbl) collectibles gain
    
    # Tax law attributes
    'ord.rates[]',            # (dbl[]) ordinary tax rate schedule
    'ord.brackets[]',         # (int[]) brackets for ordinary-rate income
    'pref.rates[]',           # (dbl[]) preferred tax rate schedule
    'pref.brackets[]',        # (int[]) brackets for preferred-rate income
    'pref.unrecapture_rate',  # (dbl)   tax rate on Section 1250 unrecaptured gain 
    'pref.collectibles_rate', # (dbl)   tax rate on collectibles gain
    'pref.tax_at_ord'         # (dbl)   whether long-term capital gains and qualified dividends are taxed at ordinary rates 
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    
    #--------------------------------
    # Define taxable income concepts
    #--------------------------------
  
    mutate(
      
      # Adjusted preferred-rate capital gain: remove special gains from total
      adj_kg_pref = pmax(0, kg_pref - kg_1250 - kg_collect),
      
      # Preferred-rate income, both with and without special gains
      pref_inc     = div_pref + kg_pref,
      adj_pref_inc = div_pref + adj_kg_pref, 
      
      # Taxable income
      txbl_ord_inc      = pmax(0, txbl_inc - pref_inc),
      txbl_adj_pref_inc = pmax(0, pmin(txbl_inc, adj_pref_inc)),
      txbl_1250         = pmax(0, pmin(txbl_ord_inc - txbl_adj_pref_inc, 
                                       pmin(kg_pref, kg_1250))),
      txbl_ord_1250     = pmax(0, txbl_ord_inc + txbl_1250),
      txbl_collect      = pmax(0, pmin(txbl_ord_inc - txbl_adj_pref_inc - txbl_1250,  
                                       pmin(kg_pref, kg_collect))),
      
    ) %>% 
    
    #------------------------------------------
    # Calculate non-special preferred-rate tax
    #------------------------------------------
  
    bind_cols(
      integrate_conditional_rates_brackets(
        df              = .,
        n_brackets      = NULL, 
        prefix_brackets = 'pref.brackets', 
        prefix_rates    = 'pref.rates', 
        y               = 'txbl_inc', 
        x               = 'txbl_adj_pref_inc',
        inclusive       = T,
        output_name     = 'liab_pref', 
        by_bracket      = F
      )
    ) %>%
    
    #--------------------------------
    # Calculate tax on special gains
    #--------------------------------
    
    # Calculate rate schedule, capped at maximum, for each type of special gain 
    mutate(across(.cols  = starts_with('ord.rates'),
                  .fns   = list(unrecapture = ~ pmin(., pref.unrecapture_rate), 
                                collect     = ~ pmin(., pref.collectibles_rate)), 
                  .names = '{fn}.{col}')) %>% 
  
    # Calculate tax at ordinary rates
    bind_cols(
      pmap(.f = integrate_conditional_rates_brackets, 
           .l = list(prefix_rates = c('unrecapture.ord.rates', 'collect.ord.rates'),
                     x            = c('txbl_1250',             'txbl_collect'),
                     y            = c('txbl_ord_inc',          'txbl_ord_1250'),
                     output_name  = c('liab_1250',             'liab_collect')),
           df              = ., 
           n_brackets      = NULL, 
           prefix_brackets = 'ord.brackets',
           inclusive       = F,
           by_bracket      = F) %>% 
        bind_cols()
    ) %>% 
      
    # Limit to maximum rates
    mutate(liab_1250    = pmin(liab_1250,    kg_1250    * pref.unrecapture_rate),
           liab_collect = pmin(liab_collect, kg_collect * pref.collectibles_rate)) %>% 
    
    #------------------------------------------
    # Calculate tax on taxable ordinary income
    #------------------------------------------
  
    bind_cols(
      integrate_rates_brackets(
        df              = .,
        n_brackets      = NULL, 
        prefix_brackets = 'ord.brackets', 
        prefix_rates    = 'ord.rates', 
        y               = 'txbl_ord_inc',
        output_name     = 'liab_ord', 
        by_bracket      = F
      )
    ) %>%
    
    #---------------------------------
    # Determine overall tax liability
    #---------------------------------
    
    # Calculate ordinary-rate liability on all taxable income, as an upper bound 
    # if preferred rates or in place or as actual liability if all income is 
    # taxed at ordinary rates
    bind_cols(
      integrate_rates_brackets(
        df              = .,
        n_brackets      = NULL, 
        prefix_brackets = 'ord.brackets', 
        prefix_rates    = 'ord.rates', 
        y               = 'txbl_inc',
        output_name     = 'liab_max', 
        by_bracket      = F
      )
    ) %>%
  
    mutate(
      
      # Calculate total liability 
      liab = if_else(pref.tax_at_ord == 0, 
                     pmin(liab_max, liab_ord + liab_pref + liab_1250 + liab_collect), 
                     liab_max), 
      
      # Update preferred-rate variables in the case where all income is taxed 
      # at ordinary rates
      liab_ord     = if_else(pref.tax_at_ord == 0, liab,         0),
      liab_pref    = if_else(pref.tax_at_ord == 0, liab_pref,    0),
      liab_1250    = if_else(pref.tax_at_ord == 0, liab_1250,    0),
      liab_collect = if_else(pref.tax_at_ord == 0, liab_collect, 0)
    
    ) %>% 
  
    # Keep variables to return
    select(all_of(return_vars$calc_tax)) %>% 
    return()
}

