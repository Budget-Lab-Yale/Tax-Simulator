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

cut_var = function(df, pcts, var, weights, labels) {
  #----------------------------------------------------------------------------
  # dplyr style function that splits a dataframe column into weighted quantiles. These quantiles to not need to be the same decimal length
  # or of equal size.
  # 
  # Parameters:
  #   - df (df) :  a dataframe containing tax simulator's microdata output which must contain the following columns
  #   - pcts (vec) : A vector of quantile thresholds used to split expanded_income into groups. Expressed in decimal form
  #   - var (num) : A column of df containing numeric values
  #   - weights (num) : A column of df containing the tax filers' representative weights
  #   - labels (str) : Optional vector of group names. Length must be 1 longer than pcts.
  #
  # Returns:  df with an additional column:
  #   - group (factor) :  Grouping variable organized by splitting var in accordance with 
  #   
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

