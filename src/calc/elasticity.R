calc_mtr_elastic = function(df, var) {
  
  #----------------------------------------------------------------------------
  # Adjusts a variable based on its elasticity with respect to the Marginal 
  # Tax Rate (MTR)
  # 
  # Parameters:
  #   - var (str)   : Name of the variable to be adjusted
  #   - df (df) : dataframe containing the following columns
  #        - {var} (dbl)               : the variable to be adjusted
  #        - mtr_{var} (dbl)           : counterfactual (static) MTR with  
  #                                      respect to the variable
  #        - mtr_{var}_baseline (dbl)  : baseline MTR with respect to
  #                                      the variable
  #        - e_{var} (dbl)             : the elasticity value
  #        - e_{var}_type (str)        : the type of elasticity, must be either
  #                                      'semi', 'arc', 'netoftax', 'taxprice'
  # Returns:  the dataframe df with the addition of the following column
  #        - new_{var} (dbl)           : the post-adjustment value of var
  #----------------------------------------------------------------------------
  
  
  df %>%
    mutate("factor_{var}" := case_when(
      !!sym(paste0("e_", var, "_type")) == "semi"     ~ exp((!!sym(paste0("mtr_", var)) - 
                                  !!sym(paste0("mtr_", var, "_baseline"))) *
                              !!sym(paste0("e_", var))),
      
      !!sym(paste0("e_", var, "_type")) == "arc"      ~ (!!sym(paste0("e_", var)) * 
                                                           ( !!sym(paste0("mtr_", var)) / 
                              ((!!sym(paste0("mtr_", var)) + 
                                  !!sym(paste0("mtr_", var, "_baseline"))) / 2)
                            - 1)),
      
      !!sym(paste0("e_", var, "_type")) == "netoftax" ~ (!!sym(paste0("e_", var)) * (
                              (1 - !!sym(paste0("mtr_", var))) / 
                                (1 - !!sym(paste0("mtr_", var, "_baseline")))
                            - 1)),
      
      !!sym(paste0("e_", var, "_type")) == "taxprice" ~ (!!sym(paste0("e_", var)) * 
                              ((1 + !!sym(paste0("mtr_", var))) / 
                                 (1 + !!sym(paste0("mtr_", var, "_baseline")))
                            - 1)),
      
      # Can drop if we want to turn Tax Price into the TRUE
      TRUE               ~ 0 
      ),
      "new_{var}" := !!sym(paste0("factor_", var)) * !!sym(var) 
        ) %>%
    select(!!sym(paste0("new_", var))) %>%
    return()
}



