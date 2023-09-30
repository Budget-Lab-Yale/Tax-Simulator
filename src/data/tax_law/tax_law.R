#------------------------------------------------------------------
# tax_law.R
# 
# Contains functions to read and parse tax law configuration files
#------------------------------------------------------------------



build_tax_law = function(scenario_info, indexes) {
  
  #----------------------------------------------------------------------------
  # Executes all tax law parsing functions, generating the final tax law
  # dataframe for scenario. Names of subparameters are concatenated to the name
  # of the tax parameter to which they belong using a "."; for example, the
  # subparameter "po_thresh" under "ctc" becomes "ctc.po_thresh". Also, each
  # element of a vector subparameter gets its own column -- that is, list-cols
  # are unnested wide. For example, "ord.rates1", "ord.rates2", etc. 
  # 
  # Parameters:
  #   - scenario_info (list) : scenario info object; see get_scenario_info() 
  #                            in globals.R
  #   - indexes (df)         : long-format dataframe containing growth rates 
  #                            of index measures
  #
  # Returns: tibble wide in subparam, long in year and filing status (df).
  #----------------------------------------------------------------------------
  
  # Read counterfactual tax law parameter changes
  changes_from_baseline = scenario_info$tax_law_id %>% 
    file.path('./config/scenarios/tax_law', .) %>% 
    load_tax_law_input()
  
  # Read baseline YAML files
  tax_law = load_tax_law_input('./config/scenarios/tax_law/baseline') %>% 
    
    # Overwrite baseline subparams with specified changes
    map2(.f = replace_by_name, 
         .y = changes_from_baseline) %>% 
  
    # Parse all parameters and concatenate
    map2(.f      = parse_param, 
         .y      = names(.), 
         years   = scenario_info$years,
         indexes = indexes) %>% 
    bind_rows() %>% 

    # Split subparameters into scalars and vectors 
    filter(!is.na(value)) %>% 
    group_by(parameter, subparameter) %>% 
    mutate(scalar = max(element) == 1) %>% 
    ungroup() %>% 
    
    # Reshape wide
    mutate(name = parameter %>% 
             paste0('.') %>% 
             paste0(subparameter) %>% 
             paste0(ifelse(scalar, '', element))) %>% 
    select(-contains('arameter'), -element, -scalar) %>% 
    pivot_wider(names_from  = name,
                values_from = value)

  # Write tax law then return
  c('static', 'conventional') %>% 
    map(.f = ~ scenario_info$output_path %>% 
          file.path(.x, 'supplemental', 'tax_law.csv') %>%
          write_csv(x = tax_law, file = .))
  
  return(tax_law)
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
  
  # Parse subparameters and bind together
  subparams = raw_input %>% 
    parse_subparams(indexation_defaults, years, indexes) %>% 
    bind_rows() %>% 
    
    # Apply indexation rules, keeping only final values in wide format
    pivot_wider(names_from  = variable, 
                values_from = value) %>% 
    apply_indexation() %>% 
    select(subparameter, year, element, value) %>% 
    pivot_wider(names_from  = subparameter, 
                values_from = value) %>% 
    arrange(year, element) 
  
  # Map subparameters to filing status
  unmapped_vars = get_unmapped_subparams(raw_input, filing_status_mapper)
  subparams %>%
    agg_by_filing_status(filing_status_mapper) %>% 
    
    # Join unmapped vars
    left_join(subparams %>% 
                select(year, element, all_of(unmapped_vars)), 
              by = c('year', 'element')) %>% 
    
    # Reshape long, clean up, and return
    pivot_longer(cols      = -c(year, filing_status, element),
                 names_to  = 'subparameter',
                 values_to = 'value') %>%
    mutate(parameter = name) %>% 
    select(parameter, subparameter, year, filing_status, element, value) %>%
    arrange(subparameter, year, filing_status, element) %>%
    return()
}



generate_time_series = function(value, years, name) {
  
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
  #
  # Returns: long-format dataframe containing time series for subparameter 
  #          element (df). 
  #----------------------------------------------------------------------------
  
  # More-involved case: time-variant policy
  if (is.list(value)) {
    
    # Create vector of specified years
    specified_years = value %>% 
      names() %>% 
      as.integer()
    
    # Convert to dataframe and populate unspecified values
    df = tibble(year = years) %>% 
      left_join(tibble(year  = specified_years, 
                       value = value),
                by = 'year') %>% 
      fill(value)
  } 
  
  # Otherwise, simple case: time-invariant policy
  else {
    
    # Convert to list if value is a vector
    if (length(value) > 1) {
      value = list(value)
    }
    
    df = tibble(year = years, value = value)
  }
  
  # Unnest list-cols if all values are scalars (otherwise, leave as list-cols)
  if (all(map(value, length) == 1)) {
    df %<>% 
      unnest(value)
  }
  
  # Clean and return
  df %>% 
    mutate(variable = name) %>% 
    select(variable, year, value) %>% 
    return()
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
  # Returns: list of long-format dataframes with time series for each component
  #          of a subparameter, including indexation info if required (list).
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
  # subparameter values and indexation rules. (Does not actually apply 
  # indexation rules, which happens higher in stack for performance reasons.)
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
  # Returns: dataframe long in year and element (df).
  #----------------------------------------------------------------------------
  
  # Extract base values and convert to time series
  base_values = raw_input$value %>% 
    parse_inf() %>%
    generate_time_series(years, 'base_value') %>% 
    unnest_if_nested() %>% 
    mutate(subparameter = name) %>% 
    select(subparameter, variable, year, element, value)
  
  # Return base values if subparam is unindexed
  if (is.null(raw_input$i_measure)) {
    return(base_values)
  }
  
  # Replace 'default' indexation parameters with actual default value
  i_info = raw_input %<>% 
    remove_by_name('value')
  if (!is.null(indexation_defaults)) {
    i_info = raw_input %>% 
      map2(indexation_defaults, replace_defaults)
  }
  
  # Parse NAs in numeric indexation params
  for (i_name in c('i_base_year', 'i_direction', 'i_increment')) {
    i_info[[i_name]] %<>% 
      parse_na()
  }
  
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
    left_join(indexes, by = c('value' = 'series', 'year')) %>%
    mutate(index = cumprod(1 + growth)) %>%
    select(year, index)
  
  # Create series of indexation scaling factors. First, unnest list-cols
  i_info$i_base_year %<>% 
    rename(base_year = value) %>%
    unnest_longer(col        = base_year, 
                  indices_to = 'element') %>% 
    
    # Then, join index values for current year and base year
    left_join(i_info$i_measure %>% 
                mutate(index = lag(index)), 
              by = 'year') %>% 
    left_join(i_info$i_measure %>% 
                rename(base_year_index = index), 
              by = c('base_year' = 'year')) %>% 
    
    # Calculate scaling factor by dividing current-year value by base-year value
    mutate(value    = index / base_year_index, 
           variable = 'i_index') %>% 
    select(year, variable, element, value) 
  
  # Concatenate indexation info into single dataframe
  i_info %>% 
    remove_by_name('i_measure') %>% 
    map(unnest_if_nested) %>% 
    bind_rows() %>% 
    
    # Join base values, clean up, and return
    bind_rows(base_values) %>% 
    mutate(subparameter = name) %>%
    select(subparameter, everything()) %>%
    return()
}



apply_indexation = function(df) {
  
  #----------------------------------------------------------------------------
  # Creates indexed value series by combining base value, index, direction, 
  # and increment info if it exists; otherwise, for unindexed parameters, 
  # simply sets final value to the base value.  
  #
  # Parameters:
  #   - df (df) : dataframe with BaseValue column, plus, optionally, i_index, 
  #               i_direction, and i_increment
  #
  # Returns: dataframe with Value column (df).
  #----------------------------------------------------------------------------
  
  if ('i_index' %in% colnames(df)) {
    df %>% 
      mutate(
        value = case_when(
          is.na(i_direction) | is.na(i_index) ~ base_value,
          i_direction == -1  ~ floor(base_value   * i_index / i_increment) * i_increment,
          i_direction ==  1  ~ ceiling(base_value * i_index / i_increment) * i_increment,
          i_direction ==  0  ~ round(base_value   * i_index / i_increment) * i_increment, 
          T                  ~ -1
        )) %>%
      return()
  } else { 
    df %>% 
      mutate(value = base_value) %>%
      return()
  }
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
  
  # If there is no mapping function, return an empty year-element-filing status df
  if (is.null(filing_status_mapper)) {
    subparams %>% 
      select(year, element) %>% 
      expand_grid(filing_status = 1:4) %>% 
      return()
  
  # Otherwise apply mapper functions
  } else {
    pmap(.f = apply_mapper_fns, 
         .l = list(name       = names(filing_status_mapper), 
                   mapper_fns = filing_status_mapper), 
         subparams = subparams) %>% 
      bind_rows() %>% 
      pivot_wider(names_from  = subparameter, 
                  values_from = value) %>% 
      return()
  }
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
    mutate(subparameter = name) %>% 
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
    mutate(filing_status = as.integer(filing_status), 
           value        = !!parse_expr(fn_expr)) %>% 
    select(year, filing_status, element, value) %>%
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
  
  # If no filing status mapper, return all names
  if (is.null(filing_status_mapper)) {
    return(names(raw_input))
  }
  
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
  
  nested = !any('element' == colnames(df))
  if (nested) {
    df %<>% 
      unnest_longer(col        = value, 
                    indices_to = 'element')
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



parse_inf = function(value) {
  
  #----------------------------------------------------------------------------
  # Helper function to replace string "Inf" with R's infinity (Inf) object in 
  # a potentially multi-level nested list
  #
  # Parameters:
  #   - value (list | any atomic) : value of raw input 
  #
  # Returns: updated input object with instances of "Inf" replaced with Inf 
  #          (list | any atomic).
  #----------------------------------------------------------------------------
  
  if (is.list(value)) {
    
    # Named lists indicate year-value specification 
    if (!is.null(names(value))) {
      value %>% 
        map(~ if (any(. == 'Inf')) { as.numeric(.) } else { . }) %>% 
        return()
    
    # Unnamed lists indicate heterogeneous list YAML parsing  
    } else { 
      if (any(value == 'Inf')) {
        return(as.numeric(value))
      }
      return(value)
    }
  
  # Scalars
  } else {
    if (any(value == 'Inf')) { 
      return(as.numeric(value))
    }
    return(value)
  }
}



parse_na = function(value) {
  
  #----------------------------------------------------------------------------
  # Helper function to parse string "NA" in a specified numeric indexation rule
  # array, which indicates that value corresponding to that element should not
  # be indexed. For example, we might specify a "donut hole" policy for
  # payroll taxes as follows:
  # 
  # oasdi_er_brackets:
  #   value: [0, 60600, 400000]
  #   i_measure: default
  #   i_base_year: [1991, 1991, NA]
  #   i_direction: [1, 1, NA]
  #   i_increment: [300, 300, NA]
  #
  # The 60600 bracket would be indexed, but not the 400000 bracket. 
  #
  #
  # Parameters:
  #   - value (list | any atomic) : a vector/atomic value, or a list of vectors/
  #                                 atomic values, that can be parsed to numeric
  #
  # Returns: updated input object with instances of "NA" replaced with NA 
  #          (list | any atomic).
  #----------------------------------------------------------------------------
  
  if (is.list(value)) {
    
    # Named lists indicate year-value specification 
    if (!is.null(names(value))) {
      value %<>% 
        map(.f = ~ suppressWarnings(as.numeric(.))) 
      return(value)
    }
  }
  return(suppressWarnings(as.numeric(value)))
}

  