#--------------------------------
# utils.R 
#
# Miscellaneous helper functions
#--------------------------------


remove_by_name = function(lst, name) {
  
  #----------------------------------------------------------------------------
  # In a copy of a given list, removes an element referenced by name
  # 
  # Parameters:
  #   - lst (list) : list from which to remove element
  #   - name (str) : name of element to remove
  #
  # Returns: list with element removed (list)
  #----------------------------------------------------------------------------
  
  return(lst[names(lst) != name])
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



get_vector = function(df, name) { 
  
  #----------------------------------------------------------------------------
  # Gets a single column from a dataframe or tibble and returns it as a 
  # vector. Designed for readability in a dplyr chain. 
  # 
  # Parameters:
  #   - df (df) : dataframe to get column from
  #   - name (str) : name of column
  #
  # Returns: atomic vector representation of column.
  #----------------------------------------------------------------------------
  
  df %>% 
    select(all_of(name)) %>% 
    deframe() %>% 
    return()
}



cut_var = function(df, pcts, var, weights, labels) {
  
  #----------------------------------------------------------------------------
  # dplyr style function that splits a dataframe column into weighted quantiles. 
  # These quantiles to not need to be the same decimal length or of equal size.
  # 
  # Parameters:
  #   - df (df)       : a dataframe containing tax simulator's microdata 
  #   - pcts (vec)    : a vector of quantile thresholds used to split 
  #                     expanded_income into groups. Expressed in decimal form
  #   - var (num)     : a column of df containing numeric values
  #   - weights (num) : a column of df containing the tax filers' representative 
  #                     weights
  #   - labels (str)  : optional vector of group names. Length must be 1 longer 
  #                     than pcts.
  #
  # Returns: df with an additional column:
  #   - group (factor) : Grouping variable organized by splitting var in 
  #                      accordance with 
  #----------------------------------------------------------------------------
  
  # Make sure decimalplaces() can read pcts
  if(!all(pcts<1, na.rm=TRUE)) stop("Quantiles must be expressed in decimal form", call. = F)
  
  # Get weighted quantiles for the fine split
  thresholds <- Hmisc::wtd.quantile(unlist(select(df, !!sym(var))), 
                                    probs = pcts,
                                    weights = select(df, !!sym(weights)))
  
  #For labeling tidiness
  if(missing(labels)) {
    pcts <- c(0,pcts*100,100)
    lablr <- c(paste0(lag(pcts), "% - ", pcts, "%"))[-1]
    
  } else if((length(labels) - 1) == length(pcts)) {
    lablr <- labels
    
  } else {
    stop("The number of labels must be 1 greater than the number of quantiles")
  }
  
  # Cut var based on the pct thresholds
  df %>% 
    mutate(group = cut(!!sym(var), 
                       breaks = c(-Inf, thresholds, Inf), 
                       labels = lablr, 
                       right=T, include.lowest = T))  %>%
    return()
  
}

