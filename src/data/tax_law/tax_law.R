#------------------------------------------------------------------
# tax-law.R
# 
# Contains functions to read and parse tax law configuration files
#------------------------------------------------------------------



build_tax_law = function(config_path, years, indexes) {
  
  #----------------------------------------------------------------------------
  # Executes all tax law parsing functions, generating the final tax law
  # dataframe for scenario. Names of subparameters are concatenated to the name
  # of the tax parameter to which they belong using a "."; for example, the
  # subparameter "po_thresh" under "ctc" becomes "ctc.po_thresh". Also, each
  # element of a vector subparameter gets its own column -- that is, list-cols
  # are unnested wide. For example, "ord.rates1", "ord.rates2", etc. 
  # 
  # Parameters:
  #   - config_path (str) : folder containing tax parameter YAML files
  #   - years (int[])     : years for which to generate tax law parameters
  #   - indexes (df)      : long-format dataframe containing growth rates of 
  #                         index measures
  #
  # Returns: dataframe wide in subparam, long in year and filing status (df).
  #----------------------------------------------------------------------------
  
  # Read baseline YAML files
  load_tax_law_input('./config/policy/baseline/tax_law/baseline') %>% 
    
    # Overwrite baseline subparams with specified changes
    map2(.f = replace_by_name, 
         .y = load_tax_law_input(config_path)) %>% 
  
    # Parse all parameters and concatenate
    map2(.f      = parse_param, 
         .y      = names(.), 
         years   = years,
         indexes = indexes) %>% 
    bind_rows() %>% 

    # Split subparameters into scalars and vectors 
    filter(!is.na(Value)) %>% 
    group_by(Parameter, Subparameter) %>% 
    mutate(Scalar = max(Element) == 1) %>% 
    ungroup() %>% 
    
    # Reshape wide
    mutate(Name = Parameter %>% 
             paste0('.') %>% 
             paste0(Subparameter) %>% 
             paste0(ifelse(Scalar, '', Element))) %>% 
    select(-contains('arameter'), -Element, -Scalar) %>% 
    pivot_wider(names_from  = Name,
                values_from = Value) %>% 
    return()
}



load_tax_law_input = function(config_path) {
  
  #----------------------------------------------------------------------------
  # Given a filepath to a tax law configuation folder, reads all tax parameter
  # YAML files and stores the contents in a tax parameter name-index list of 
  # lists. 
  # 
  # Parameters:
  #   - config_path (str) : filepath to tax law config folder
  #
  # Returns: list of raw tax parameter inputs, indexed by tax parameter name 
  #          (list).
  #----------------------------------------------------------------------------
  
  # Get list of YAML files
  param_names = config_path %>% 
    list.files(pattern = '.yaml') %>% 
    str_sub(end = -6)
  
  # Read YAML into list
  param_names %>% 
    paste0('.yaml') %>% 
    file.path(config_path, .) %>% 
    map(read_yaml) %>% 
    set_names(param_names) %>% 
    return()
}



parse_param = function(raw_input, name, years, indexes) {
  
  #----------------------------------------------------------------------------
  # Parses raw input for a given tax law parameter.
  # 
  # Parameters:
  #   - raw_input (list) : contents of tax parameter's YAML file
  #   - name (str)       : name of tax parameter
  #   - years (int[])    : years for which to generate tax law parameters
  #   - indexes (df)     : long-format dataframe containing growth rates of 
  #                        index measures
  #
  # Returns: dataframe long in subparameter name, year, filing status, and 
  #          subparameter(df).
  #----------------------------------------------------------------------------
  
  # If specified, remove indexation defaults and filing status aggregator
  indexation_defaults  = raw_input$indexation_defaults
  filing_status_mapper = raw_input$filing_status_mapper
  
  raw_input$indexation_defaults  = NULL
  raw_input$filing_status_mapper = NULL
  
  # Parse subparameters, bind together, and reshape wide
  subparams = raw_input %>% 
    parse_subparams(indexation_defaults, years, indexes) %>% 
    bind_rows() %>% 
    pivot_wider(names_from  = Subparameter, 
                values_from = Value) %>% 
    arrange(Year, Element) 
  
  # Map sub parameters to filing status
  unmapped_vars = get_unmapped_subparams(raw_input, filing_status_mapper)
  subparams %>%
    agg_by_filing_status(filing_status_mapper) %>% 
    
    # Join unmapped vars
    left_join(subparams %>% 
                select(Year, Element, all_of(unmapped_vars)), 
              by = c('Year', 'Element')) %>% 
    
    # Reshape long, clean up, and return
    pivot_longer(cols      = -c(Year, FilingStatus, Element),
                 names_to  = 'Subparameter',
                 values_to = 'Value') %>%
    mutate(Parameter = name) %>% 
    select(Parameter, Subparameter, Year, FilingStatus, Element, Value) %>%
    arrange(Subparameter, Year, FilingStatus, Element) %>%
    return()
}



generate_time_series = function(value, years, name, long = T) {
  
  #----------------------------------------------------------------------------
  # For a subparameter element, takes minimally-specified set of values and
  # creates time series for that element. In the case of a scalar value, this
  # simply means generating a time series where every year maps to the same
  # value. When the user instead supplies values for a subset of years, the 
  # unspecified years are filled in. 
  # 
  # Parameters:
  #   - value (list | any atomic) : either a scalar atomic value or a year-
  #                                 indexed list of values, representing the 
  #                                 subparameter element values in years when 
  #                                 the value changes
  #   - years (int[])             : years for which create time series
  #   - name (str)                : name of subparameter element in resulting
  #                                 dataframe
  #   - long (bool)               : whether resulting dataframe is reshaped 
  #                                 long such that the name is stored in rows
  #                                 rather than as a column name
  #
  # Returns: dataframe containing time series for subparameter element (df). 
  #----------------------------------------------------------------------------
  
  # More-involved case: time-variant policy
  if (is.list(value)) {
    
    # Create vector of specified years
    specified_years = value %>% 
      names() %>% 
      as.integer()
    
    # Convert to dataframe and populate unspecified values
    df = tibble(Year = years) %>% 
      left_join(tibble(Year  = specified_years, 
                       Value = value),
                by = 'Year') %>% 
      fill(Value)
  } 
  
  # Otherwise, simple case: time-invariant policy
  else {
    
    # Convert to list if value is a vector
    if (length(value) > 1) {
      value = list(value)
    }
    
    df = tibble(Year = years, Value = value)
  }
  
  # Unnest list-cols if all values are scalars (otherwise, leave as list-cols)
  if (all(map(value, length) == 1)) {
    df %<>% 
      unnest(Value)
  }
  
  # Rename value column
  df %<>% 
    rename(!!name := Value)
  
  # Pivot longer if specified
  if (long) {
    df %<>%
      pivot_longer(cols      = -Year, 
                   names_to  = 'Variable', 
                   values_to = 'Value') 
  }
  return(df)
}



parse_subparams = function(raw_input, indexation_defaults, years, indexes) {
  
  #----------------------------------------------------------------------------
  # Applies parse_subparam() to all raw subparameter inputs.
  # 
  # Parameters:
  #   - raw_input (list)           : list of raw_input objects as defined in
  #                                  parse_subparam(); in other words, the 
  #                                  entire parsed raw YAML list excluding 
  #                                  "indexation_defaults" and 
  #                                  "filing_status_mapper"
  #   - indexation_defaults (list) : see parse_subparam() 
  #   - years (int[])              : see parse_subparam()
  #   - indexes (df)               : see parse_subparam() 
  #
  # Returns: list of long-format dataframes with final subparameter values 
  #          (list).
  #----------------------------------------------------------------------------
  
  pmap(.f = parse_subparam, 
       .l = list(raw_input = raw_input, 
                 name      = names(raw_input)), 
       indexation_defaults = indexation_defaults,
       years               = years,
       indexes             = indexes) %>% 
    return()
}



parse_subparam = function(raw_input, indexation_defaults, years, indexes, name) {
  
  #----------------------------------------------------------------------------
  # Takes list of raw subparameter input and generates time series of 
  # subparameter values, also indexing values to inflation if specified.  
  # 
  # Parameters:
  #   - raw_input (list)           : list representation of subparameter, i.e. 
  #                                  a top-level element of YAML specification. 
  #                                  Contains an element named "value" 
  #                                  with base values, and optionally four 
  #                                  indexation elements named: "i_measure" 
  #                                  (which series is used for indexation), 
  #                                  "i_base_year" (base year of index), 
  #                                  "i_direction" (whether to round down, up, 
  #                                  or to nearest) "i_increment" (rounding 
  #                                  increment after indexation adjustment)
  #   - indexation_defaults (list) : tax parameter default values for indexation 
  #                                  rules
  #   - years (int[])              : years for which to generate subparameter 
  #                                  values 
  #   - indexes (df)               : long-format dataframe containing growth 
  #                                  rates of index measures
  #   - name (str)                 : name of subparameter
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Extract base values and convert to time series
  base_values = generate_time_series(raw_input$value, years, 'Value', F) %>% 
    unnest_if_nested() %>% 
    mutate(Subparameter = name) %>% 
    select(Subparameter, Year, Element, Value)
  
  # Return base values if subparam is unindexed
  if (is.null(raw_input$i_measure)) {
    return(base_values)
  }
  
  # Replace 'default' indexation parameters with actual default values
  i_info = raw_input %>% 
    remove_by_name('value') %>% 
    map2(indexation_defaults, replace_defaults)
  
  # Convert elements of raw subparam input to time series dataframes
  # First, determine range of years by indexation param
  i_years = i_info %>% 
    names() %>% 
    map(.f = ~ if (. != 'i_measure') { years } 
        else { min(as.integer(names(i_info$i_measure))):max(years) })
  
  # Then, generate time series for each element
  i_info = pmap(.f = generate_time_series,
                .l = list(value = i_info, 
                          years = i_years, 
                          name  = names(i_info)))
  
  # Create unified index series 
  i_info$i_measure %<>%
    left_join(indexes, by = c('Value' = 'Series', 'Year')) %>%
    mutate(Index = cumprod(1 + Growth)) %>%
    select(Year, Index)
  
  # Create series of indexation scaling factors. First, unnest list-cols
  i_info$i_base_year %<>% 
    rename(BaseYear = Value) %>%
    unnest_longer(col        = BaseYear, 
                  indices_to = 'Element') %>% 
    
    # Then, join index values for current year and base year
    left_join(i_info$i_measure %>% 
                mutate(Index = lag(Index)), 
              by = 'Year') %>% 
    left_join(i_info$i_measure %>% 
                rename(BaseYearIndex = Index), 
              by = c('BaseYear' = 'Year')) %>% 
    
    # Calculate scaling factor by dividing current-year value by base-year value
    mutate(Value    = Index / BaseYearIndex, 
           Variable = 'i_index') %>% 
    select(Year, Variable, Element, Value) 
  
  # Concatenate indexation info into single dataframe
  i_info %>% 
    remove_by_name('i_measure') %>% 
    map(unnest_if_nested) %>% 
    bind_rows() %>% 
    pivot_wider(names_from  = Variable, 
                values_from = Value) %>% 
    
    # Join base values and do indexation
    left_join(base_values, by = c('Year', 'Element')) %>%
    mutate(
      Value = case_when(
        i_direction == -1 ~ floor(Value   * i_index / i_increment) * i_increment,
        i_direction ==  1 ~ ceiling(Value * i_index / i_increment) * i_increment,
        T                 ~ round(Value   * i_index / i_increment) * i_increment
      )) %>%
    select(Subparameter, Year, Element, Value) %>%
    return()
}



agg_by_filing_status = function(subparams, filing_status_mapper) {
  
  #----------------------------------------------------------------------------
  #  Creates new aggregated subparameters grouped by filing status.
  # 
  # Parameters:
  #   - subparams (df)              : dataframe with columns for all parsed 
  #                                   subparameters
  #   - filing_status_mapper (list) : nested list where first level contains 
  #                                   names of new grouped subparameters, and 
  #                                   elements are filing-status-indexed lists 
  #                                   of mapping functions 
  # Returns: wide-format dataframe containing all parsed subparameters 
  #          including the new grouped subparameters -- essentially an updated 
  #          version of the subparams" input argument (df). 
  #----------------------------------------------------------------------------
  
  pmap(.f = apply_mapper_fns, 
       .l = list(name       = names(filing_status_mapper), 
                 mapper_fns = filing_status_mapper), 
       subparams = subparams) %>% 
    bind_rows() %>% 
    pivot_wider(names_from  = Subparameter, 
                values_from = Value) %>% 
    return()
}



apply_mapper_fns = function(subparams, name, mapper_fns) {
  
  #----------------------------------------------------------------------------
  # For a given to-be subparameter, applies all mapper functions by filing
  # status.
  # 
  # Parameters:
  #   - subparams (df)    : dataframe with columns for all parsed subparameters 
  #   - name (str)        : name of new subparameter grouped by filing status
  #   - mapper_fns (list) : list of mapping function expressed, indexed by 
  #                         filing status
  #
  # Returns: long-format dataframe of new subparameter by filing status (df).
  #----------------------------------------------------------------------------
  
  pmap(.f = apply_mapper_fn,
       .l = list(filing_status = names(mapper_fns), 
                 fn_expr       = mapper_fns), 
       data = subparams) %>% 
    bind_rows() %>% 
    mutate(Subparameter = name) %>% 
    return()
}



apply_mapper_fn = function(filing_status, fn_expr, data) {
  
  #----------------------------------------------------------------------------
  # For a given filing status, applies the specified mapping function.
  # 
  # Parameters:
  #   - filing_status (int) : filing status for mapping function expression
  #   - fn_expr (str)       : string representation of a formula; something 
  #                           like "value_single * 2" or just "value"
  #   - data (df)           : dataframe containing all variables required by
  #                           fn_expr
  #
  # Returns: dataframe containing only the resulting filing-status specific
  #          value (df).
  #----------------------------------------------------------------------------
  
  data %>% 
    mutate(FilingStatus = as.integer(filing_status), 
           Value        = !!parse_expr(fn_expr)) %>% 
    select(Year, FilingStatus, Element, Value) %>%
    return()
}



get_unmapped_subparams = function(raw_input, filing_status_mapper) {
  
  #----------------------------------------------------------------------------
  # Determines which subparameters are not referenced in mapping functions --
  # in other words, which ones are standalone subparameters to be kept rather
  # than intermediate variables which should be dropped.
  # 
  # Parameters:
  #   - raw_input (list)            : list of raw_input objects as defined in
  #                                   parse_subparam(); in other words, the 
  #                                   entire parsed raw YAML list excluding 
  #                                   "indexation_defaults" and 
  #                                   "filing_status_mapper"
  #   - filing_status_mapper (list) : nested list where first level contains 
  #                                   names of new grouped subparameters, and 
  #                                   elements are filing-status-indexed lists 
  #                                   of mapping functions 
  #
  # Returns: vector of names of subparameters unused in grouped subparameter 
  #          construction (str[]).
  #----------------------------------------------------------------------------
  
  # Get symbols used in mapper expressions
  symbols = filing_status_mapper %>% 
    unlist() %>% 
    str_split(' ') %>% 
    unlist() %>% 
    unique()
  
  # Select subparameter names that do not show up in expression symbols
  raw_input %>% 
    names() %>% 
    `[`(!(. %in% symbols)) %>% 
    return()
}



unnest_if_nested = function(df) {
  
  #----------------------------------------------------------------------------
  # For a given dataframe with a column called "Value", unnests long on Value
  # if Value is determined to be nested as per the lack of precense of another
  # column called "Element". A helper function to be used in pipe chains above.
  # 
  # Parameters:
  #   - df (df) : dataframe with "Value" column and optionally "Element" column
  #
  # Returns: unnested dataframe (df).
  #----------------------------------------------------------------------------
  
  nested = !any('Element' == colnames(df))
  if (nested) {
    df %<>% 
      unnest_longer(col        = Value, 
                    indices_to = 'Element')
  }
  return(df)
}



replace_defaults = function(supplied, default) { 
  
  #----------------------------------------------------------------------------
  # Helper function to return indexation defaults in the event the user 
  # supplies "default", indicating the tax parameter's default value should be
  # used
  #
  # Parameters:
  #   - supplied (list | any atomic) : supplied value
  #   - default (list | any atomic)  : default indexation value
  #
  # Returns: indexation info (list | any atomic).
  #----------------------------------------------------------------------------
  
  if (!is.list(supplied) && length(supplied) == 1 && supplied == 'default') {
    return(default)
  } 
  return(supplied)
}



replace_by_name = function(host, donor) {
  
  #----------------------------------------------------------------------------
  # Helper function to overwrite elements in a "host" list with identically 
  # names elements from a "donor" list. 
  #
  # Parameters:
  #   - host (list)  : named list for which to overwrite values 
  #   - donor (list) : names list containing values which will overwrite those
  #                    in host based on name index
  #
  # Returns: updated host list (list.)
  #----------------------------------------------------------------------------
  
  for (name in names(donor)) {
    host[[name]] = donor[[name]]
  }
  return(host)
}

