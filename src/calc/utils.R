#---------------------------------------------
# Helper functions for use in tax calculation
#---------------------------------------------


parse_calc_fn_input = function(tax_unit, req_vars) {
  
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
  #
  # Returns: dataframe representation of tax unit input (df). 
  #----------------------------------------------------------------------------
  
  # Convert list representation of tax unit(s) to dataframe 
  if (!is.data.frame(tax_unit)) { 
    stopifnot(is.list(tax_unit))
    tax_unit = as_tibble(tax_unit) 
  } 
  
  # Check that required variable names are supplied
  given_vars = names(tax_unit)
  if (!all(req_vars %in% given_vars)) {
    missing_vars = req_vars[!(req_vars %in% given_vars)]
    stop('The following required variables were not supplied: ', paste0(missing_vars, ' '))
  }
  
  return(tax_unit)
}



integrate_rates_brackets = function(df, n_brackets, brackets_prefix, rates_prefix, inc_name, 
                                    output_name, by_bracket) {
  
  #----------------------------------------------------------------------------
  # Given a dataframe containing columns for a schedule of tax rates/brackets
  # and an income column, calculates tax liability (and liability by bracket,
  # if specified). In other words, a vectorized tax liability calculator. 
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
  #   - brackets_prefix (str) : string uniquely identifying bracket columns. 
  #                             Integers, corresponding to bracket number, 
  #                             follow the prefix
  #   - rates_prefix (str)    : string uniquely identifying rate column
  #   - inc_name (str)        : column name for income fed to rates/brackets 
  #   - output_name (str)     : name for function output variables 
  #   - by_bracket (bool)     : whether to include bracket-specific output
  #                             columns
  #
  # Returns: dataframe representation of tax unit input (df). 
  #----------------------------------------------------------------------------
  
  # Shorten variable names for readability below
  b = brackets_prefix
  r = rates_prefix
  y = inc_name
  p = paste0
  
  # If number of brackets isn't supplied by user, ascertain it
  if (is.null(n_brackets)) {
    n_brackets = df %>% 
      select(starts_with(b)) %>% 
      names() %>% 
      str_sub(-1) %>% 
      as.integer() %>% 
      max()
  }
  
  # Add (n+1)th bracket, used to calculate taxable income in excess of top bracket
  df[[paste0(b, n_brackets + 1)]] = Inf
  
  # Generate bracket-specific output names
  bracket_output_names = paste0(output_name, 1:n_brackets) 
  
  # Iterate over brackets, stored with associated output prefix
  tax = 1:n_brackets %>% 
    set_names(bracket_output_names) %>% 
    
    # Calculate extent to which taxable income exceeds this bracket, and apply rate
    map_df(~ pmax(0, pmin(df[[p(b, . + 1)]], df$y) - df[[p(b, .)]]) * df[[p(r, .)]]) %>% 
    
    # Generate total output column
    mutate(!!output_name := rowSums(.)) %>% 
    
    # Remove intermediate bracket-level calculations if specified
    select(all_of(output_name), 
           if (by_bracket) all_of(bracket_output_names) else c()) %>% 
    return()
}

