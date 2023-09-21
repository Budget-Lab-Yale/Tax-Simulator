#---------------------------------------------
# Helper functions for use in tax calculation
#---------------------------------------------


derive_vars = function(tax_unit) { 
  
  #----------------------------------------------------------------------------
  # Adds derived, policy-indepenent variables used elsewhere, either in tax
  # calculation or just for reporting convenience.
  # 
  # Parameters:
  #   - tax_unit (df) : dataframe of tax units
  #
  # Returns: dataframe containing new variables derived within (df). 
  #----------------------------------------------------------------------------
  
  tax_unit %>% 
    mutate(
      
      # TODO kg variables
      
      
      # Schedule E related variables
      part       = part_active + part_passive - part_active_loss - 
                   part_passive_loss - part_179,
      scorp      = scorp_active + scorp_passive - scorp_active_loss - 
                   scorp_passive_loss - scorp_179,
      part_scorp = part + scorp,
      pt         = part + scorp + sole_prop,
      net_rent   = rent - rent_loss,
      net_estate = estate - estate_loss,
      sch_e      = part_scorp + net_rent + net_estate,    
      
      # Self employment income and earned income 
      se_inc = sole_prop + farm + part_se,  # TODO look at E30400?? Need to impute. Model the variables on schedule SE
                                            # Also this should go in payroll taxes I think 
      ei     = wages + se_inc
      
    ) %>% 
    return()
}



parse_calc_fn_input = function(tax_unit, req_vars, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Checks whether tax unit(s) representation is in the proper format --
  # dataframe or list -- and converts to dataframe (tibble) if so. Also checks
  # whether input contains the required set of variables. Stops execution if
  # either check fails.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables for tax calculation
  #   - req_vars (str[])     : string vector of variable names required for 
  #                            tax calculation
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe representation of tax unit input (df). 
  #----------------------------------------------------------------------------
  
  # Convert list representation of tax unit(s) to dataframe 
  if (!is.data.frame(tax_unit)) { 
    stopifnot(is.list(tax_unit))
    tax_unit = as_tibble(tax_unit) 
  } 
  
  # Check that required variable names are supplied
  missing = c()
  given_vars = names(tax_unit) 
  for (var in req_vars) {
    if (!(var %in% given_vars)) {
      
      # For vector variables, check for either the prefix or an integer-indexed
      # version of the prefix (e.g. 'pref.brackets1')
      if (str_sub(var, -2) == '[]') {
        prefix = str_sub(var, end = -3)
        if ((prefix %in% given_vars) | paste0(prefix, 1) %in% given_vars) {
          next
        }
      }
      missing %<>% c(var)
    }
  }

  if (length(missing) > 0) {
    
    # Populate missing variables with 0s if specified
    if (fill_missings) { 
      tax_unit %<>% 
        bind_cols(
          missing %>% 
            map(.f = ~ rep(0, nrow(tax_unit))) %>% 
            set_names(missing) %>% 
            as_tibble()
        )
      
    # Throw exception otherwise  
    } else {
      stop('The following required variables were not supplied: ', paste0(missing, ' '))
    }
  }
  
  return(tax_unit)
}



integrate_rates_brackets = function(df, n_brackets, prefix_brackets, 
                                    prefix_rates, y, output_name, by_bracket) {
  
  #----------------------------------------------------------------------------
  # Given a dataframe containing columns for a schedule of tax rates/brackets
  # and an income ("y") column, calculates tax liability (and liability by 
  # bracket, if specified). In other words, a vectorized tax liability 
  # calculator. 
  # 
  # The number of rates and brackets must be the same, and if the number of 
  # brackets is not specified, the function will determine it based on the
  # supplied bracket column names.
  # 
  # Parameters:
  #   - df (df)               : a tibble with an income column and columns for 
  #                             brackets/rates
  #   - n_brackets (int)      : number of brackets. If NULL the code ascertains 
  #                             by parsing max integer from df's bracket cols
  #   - prefix_brackets (str) : string uniquely identifying bracket columns. 
  #                             Integers, corresponding to bracket number, 
  #                             follow the prefix -- unless there is just one
  #                             bracket, in which case specifying just the 
  #                             prefix is allowed
  #   - prefix_rates (str)    : string uniquely identifying rate column
  #   - y (str)               : column name for income fed to rates/brackets 
  #   - output_name (str)     : name for function output variables 
  #   - by_bracket (bool)     : whether to include bracket-specific output
  #                             columns
  #
  # Returns: a dataframe containing either a single liability column or one 
  #          column for each bracket (df).
  #----------------------------------------------------------------------------
  
  # If number of brackets isn't supplied by user, ascertain it
  if (is.null(n_brackets)) {
    n_brackets = get_n_cols(df, prefix_brackets)
    
    # Add integer index if not specified under single-bracket case
    if (n_brackets == 1 & prefix_brackets %in% names(df)) {
      df %<>% 
        rename_with(.cols = all_of(prefix_brackets), 
                    .fn   = ~ paste0(prefix_brackets, 1)) %>% 
        rename_with(.cols = all_of(prefix_rates), 
                    .fn   = ~ paste0(prefix_rates, 1))
    }
  }
  
  # Add (n+1)th bracket, used to calculate taxable income in excess of top bracket
  df[[paste0(prefix_brackets, n_brackets + 1)]] = Inf
  
  # Generate bracket-specific output names
  bracket_output_names = paste0(output_name, 1:n_brackets) 
  
  # Iterate over brackets, stored with associated output prefix
  1:n_brackets %>% 
    set_names(bracket_output_names) %>% 
    
    # For each bracket...
    map_df(function(i) {
      
      # Determine lesser of next bracket or income
      inc = pmin(df[[paste0(prefix_brackets, i + 1)]], df[[y]])

      # Calculate as excess over this bracket
      excess = pmax(0, inc - df[[paste0(prefix_brackets, i)]])
      
      # Apply rate and return
      return(excess * df[[paste0(prefix_rates, i)]])
      
    }) %>%
    
    # Generate total output column
    mutate(!!output_name := rowSums(.)) %>% 
    
    # Remove intermediate bracket-level calculations if specified
    select(all_of(output_name), 
           if (by_bracket) all_of(bracket_output_names) else c()) %>% 
    return()
}



integrate_conditional_rates_brackets = function(df, n_brackets, prefix_brackets, 
                                                prefix_rates, y, x, inclusive,
                                                output_name, by_bracket) {
  
  #----------------------------------------------------------------------------
  # Given a dataframe containing columns for a schedule of tax rates/brackets,
  # an overall income column ("y"), and a specific income column ("x"), 
  # calculates tax on x depending on which bracket y falls into. In other 
  # words, integrates tax for x when stacked after y. 
  # 
  # Less abstractly, we see this structure under current law most prominently 
  # with taxes on long-term capital gains. The tax rate faced by capital gains
  # ("x" here) depends on how much taxable income ("y" here) the taxpayer has. 
  # In that sense, tax on x is *conditional* on y. 
  #
  #
  # The number of rates and brackets must be the same, and if the number of 
  # brackets is not specified, the function will determine it based on the
  # supplied bracket column names.
  # 
  # Parameters:
  #   - df (df)               : a tibble with an income column and columns for 
  #                             brackets/rates
  #   - n_brackets (int)      : number of brackets. If NULL the code ascertains 
  #                             by parsing max integer from df's bracket cols
  #   - prefix_brackets (str) : string uniquely identifying bracket columns. 
  #                             Integers, corresponding to bracket number, 
  #                             follow the prefix -- unless there is just one
  #                             bracket, in which case specifying just the 
  #                             prefix is allowed
  #   - prefix_rates (str)    : string uniquely identifying rate column
  #   - y (str)               : column name for conditioning income variable y 
  #   - x (str)               : column name for income variable x on which tax 
  #                             is being assessed 
  #   - inclusive (bool)      : whether x is included in y. If so, tax is
  #                             limited to smaller of y and (x - y) in order to
  #                             properly account for case when x > y i.e. y < 0
  #   - output_name (str)     : name for function output variables 
  #   - by_bracket (bool)     : whether to include bracket-specific output
  #                             columns
  #
  # Returns: a dataframe containing either a single liability column or one 
  #          column for each bracket (df).
  #----------------------------------------------------------------------------
  
  # If number of brackets isn't supplied by user, ascertain it
  if (is.null(n_brackets)) {
    n_brackets = get_n_cols(df, prefix_brackets)
    
    # Add integer index if not specified under single-bracket case
    if (n_brackets == 1 & prefix_brackets %in% names(df)) {
      df %<>% 
        rename_with(.cols = all_of(prefix_brackets), 
                    .fn   = ~ paste0(prefix_brackets, 1)) %>% 
        rename_with(.cols = all_of(prefix_rates), 
                    .fn   = ~ paste0(prefix_rates, 1))
    }
  }
  
  # Add (n+1)th bracket, used to calculate taxable income in excess of top bracket
  df[[paste0(prefix_brackets, n_brackets + 1)]] = Inf
  
  # Generate bracket-specific output names
  bracket_output_names = paste0(output_name, 1:n_brackets) 
  
  # Iterate over brackets, stored with associated output prefix
  1:n_brackets %>% 
    set_names(bracket_output_names) %>% 
    
    # For each bracket...
    map_df(function(i) {
      
      # Extract vectors from dataframe for readability
      bracket      = df[[paste0(prefix_brackets, i)]]
      next_bracket = df[[paste0(prefix_brackets, i + 1)]]
      rate         = df[[paste0(prefix_rates, i)]]
      y            = df[[y]]
      x            = df[[x]]

      # Adjust brackets by amount already "filled up" by y
      adj_bracket      = pmax(0, bracket - y)
      adj_next_bracket = pmax(0, next_bracket - y) 
      
      # Calculate excess of x over adjusted bracket
      excess_x = pmax(0, pmin(adj_next_bracket, x) - adj_bracket)
      
      # Limit excess x to excess y if y includes x
      if (inclusive) {
        excess_y = pmax(0, pmin(next_bracket, y) - bracket)
        excess_x = pmin(excess_x, excess_y)
      }
      
      # Apply rate and return
      return(excess_x * rate)
      
    }) %>%
    
    # Generate total output column
    mutate(!!output_name := rowSums(.)) %>% 
    
    # Remove intermediate bracket-level calculations if specified
    select(all_of(output_name), 
           if (by_bracket) all_of(bracket_output_names) else c()) %>% 
    return()
}



get_n_cols = function(df, prefix) {
  
  #----------------------------------------------------------------------------
  # Finds the number of columns in a dataframe with names matching a prefix
  # 
  # Parameters:
  #   - df (df)      : a tibble
  #   - prefix (str) : string uniquely prefixing the columns we want to count 
  #
  # Returns: number of columns in df matching prefix (int).
  #----------------------------------------------------------------------------
  
  df %>% 
    select(starts_with(prefix)) %>% 
    names() %>% 
    length() %>% 
    return()
}







