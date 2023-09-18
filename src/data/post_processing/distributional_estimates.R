#-----------------------------------------------------------------------
# Post-processing functions to generate distributional tables for a scenario run
#-----------------------------------------------------------------------

calc_distro = function(sim, pcts) {
  #----------------------------------------------------------------------------
  # Produces a scenario's distributional table
  # 
  # Parameters:
  #   - sim (df) :  a dataframe containing tax simulator's microdata output which must contain the following columns
  #       - expanded_income        : Key income measurement
  #       - liab_iit_net           : Individual Income Tax Liability
  #       - liab_pr                : Payroll Tax Liability
  #   - pcts (vec) : A vector of quantile thresholds used to split expanded_income into groups. Expressed in decimal form
  #
  # Returns:  void, writes a dataframe for the scenario containing values, grouped by pcts, for:
  #   - AverageTaxDelta (dbl) : The wighted average delta between baseline and scenario tax liability
  #   - AverageTaxCut (dbl) : The weighted average tax cut for filers whose taxes were reduced
  #   - AverageTaxRaise (dbl) : The weighted average tax raise for filers whose taxes were increased
  #   - ShareWithCut (dbl) : The weighted percentage of filers whose taxes decreased (delta greater than -$5)
  #   - ShareWithRaise (dbl) : The weighted percentage of filers whose taxes increased (delta greater than $5)
  #   - PercentChangeInAfterTaxIncome (dbl) : Dollar weighted percentage change in filer's after tax income
  #   - TotalTaxChangeSahre (dbl) : The percentage of the total tax change for 
  #   
  #----------------------------------------------------------------------------
  
  
  sim %<>% mutate(liab_sim = liab_iit_net - liab_pr)
  
  base <- read.csv(file.path("/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator",
                             version,
                             vintage,
                            "baseline"))
  
  base %<>% mutate(liab_base = liab_iit_net - liab_pr)
  
  # Select and join only the variables we need
  select(base, ID, liab_base, weight) %<>% 
    left_join(., select(sim, ID, liab_sim, expanded_income), 
              by="ID") %>%
    
    # Filter out dependent returns
    filter(dep_status == 0) %>%
    mutate(
      # Round deltas to the nearest $10 increment
      delta = round(liab_sim - liab_base, -1),
      
      # Binary dummies for if a tax unit received a meaningful raise or cut
      cut = delta < -5,
      raise = delta > 5,
      
      # Create new person level weight for more representative income groups
      perwt = weight * (1 + (filing_status == 2)) # can you do arithmetic with bools?
    ) %>%
    
    # Bucket
    cut_var(pcts = pcts, 'expanded_income', 'perwt') %>%
    
    group_by(., group) %>%
      summarize(
        group_delta = sum(delta),
        group_perwt = sum(perwt),
      
        AverageTaxDelta = weighted.mean(group_delta, perwt),
        AverageTaxCut = weighted.mean(sum(cut * delta), perwt),
        AverageTaxRaise = weighted.mean(sum(raise * delta), perwt),
      
        ShareWithCut = sum(cut * perwt) / sum(perwt),
        ShareWithRaise = sum(raise * perwt) / sum(perwt),
      
        PercentChangeInAfterTaxIncome = sum(expanded_income - liab_sim) / sum(expanded_income - liab_base)  
      ) %>%
      mutate(TotalTaxChangeShare = (group_delta * group_perwt) / sum(group_delta * group_perwt)) %>%
    
      select(group, group_delta, 
             AverageTaxDelta, AverageTaxCut, AverageTaxRaise, 
             ShareWithCut, ShareWithRaise, 
             PercentChangeInAfterTaxIncome, TotalTaxChangeShare) %>%
    
      write.csv(
        .,
        file.path(out_dir, paste0("distro_", ID, ".csv")),
        row.names = F
      )
  
}



cut_var = function(df, pcts, var, weights) {
  #----------------------------------------------------------------------------
  # dplyr style function that splits a dataframe column into weighted quantiles. These quantiles to not need to be the same decimal length
  # or of equal size.
  # 
  # Parameters:
  #   - df (df) :  a dataframe containing tax simulator's microdata output which must contain the following columns
  #   - pcts (vec) : A vector of quantile thresholds used to split expanded_income into groups. Expressed in decimal form
  #   - var (num) : A column of df containing numeric values
  #   - weights (num) : A column of df containing the tax filers' representative weights
  #
  # Returns:  df with an additional column:
  #   - group (factor) :  Grouping variable organized by splitting var in accordance with 
  #   
  #----------------------------------------------------------------------------
  
  # Make sure decimalplaces() can read pcts
  if(!all(pcts<1, na.rm=TRUE)) stop("Quantiles must be expressed in decimal form", call. = F)
  
  # Find the longest decimal places
  m = max(unlist(lapply(pcts, decimalplaces)))
  
  # Create breaks such that the smallest pcts() value is captured
  splits <- 1 / ( 10 ^ m)
  
  # Get weighted quantiles for the fine split
  # Think about if you want to bottom out at 0 right HERE
  thresholds <- Hmisc::wtd.quantile(unlist(select(df, !!sym(var))), 
                                    probs=seq(splits, 1-splits, splits), 
                                    weights = select(df, !!sym(weights)))
  
  # Select only the quantiles listed in pcts
  thresh_sel <- thresholds[pcts*100]
  
  # Here check if min pct is zero and adjust?

  # Cut var based on the pct thresholds
  df %>% 
    mutate(group = cut(!!sym(var), 
                       breaks = c(-Inf, thresh_sel, Inf), 
                       labels = c(paste0(pcts*100, "%"), paste0(">" , 100*tail(pcts, 1), "%")), 
                       right=T, include.lowest = T))  %>%
    return()
    
}

decimalplaces <- function(x) {
  #----------------------------------------------------------------------------
  # Counts the decimal places of a numeric
  # 
  # Parameters:
  #   - x (num) : value to be examined
  #
  # Returns:  
  #   - unnamed numeric counting decimal places
  #   
  #----------------------------------------------------------------------------
  
  # Checks if the value is within the bottom floating point value as x approaches 1
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    
    # Convert to string and count characters
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } 
  else {
    return(0)
  }
}

pcts = c(.01,.5,.9,.99)

test <- read.csv("taxsim_test.csv")

sim <- test %>% mutate(pwages2 = pwages + 1000)

sim %<>% rename(pwages2 = pwages,
                drop = pwages)








m = max(unlist(lapply(pcts, decimalplaces)))

f = (lapply(pcts, decimalplaces))

splits <- 1 / ( 10 ^ m)

# Think about if you want to bottom out at 0 right HERE
thresholds <- Hmisc::wtd.quantile(unlist(select(test, !!sym("pwages"))), probs=seq(splits, 1-splits, splits), weights = select(test,!!sym("weight")))

typeof(test$pwages)
typeof(select(test, !!sym("pwages")))

viewr <- cbind(test$pwages, select(test, !!sym("pwages")))
viewr %<>% mutate(dif = test$pwages - pwages)

#ERROR IS AT THE df$var

thresh_sel <- unname(thresholds[paste0(pcts*100, "%")])
thresh_sel2 <- unname(thresholds[pcts*100])
thresh_sel3 <- (thresholds[pcts*100])


test$group1 <- cut(test$pwages, breaks = c(thresh_sel, Inf), labels = paste0(pcts*100, "%"),right=T, include.lowest = T)
test$group2 <- cut(test$pwages, breaks = c(-Inf, thresh_sel), labels = paste0(pcts*100, "%"),right=T, include.lowest = T)
test$group3 <- cut(test$pwages, breaks = c(-Inf, thresh_sel3, Inf), labels = c(paste0(pcts*100, "%"), ">80%"),right=T, include.lowest = T)


test %<>%
  mutate(group3 = cut(!!sym("pwages"), breaks = c(-Inf, thresh_sel3, Inf), labels = c(paste0(pcts*100, "%"), paste0(">" , 100*tail(pcts, 1), "%")),right=T, include.lowest = T))

test %<>% cut_var(pcts, "pwages", "weight")
