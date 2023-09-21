apply_mtr_elasticity = function(df, name, vars) {
  
  #----------------------------------------------------------------------------
  # Adjusts a category of variable based on their elasticity with respect to the 
  # Marginal Tax Rate (MTR)
  # 
  # Parameters:
  #   - name (str)  : Name of the 
  #   - df (df)     : dataframe containing the following columns
  #        - {name} (dbl)               : the variable to be adjusted
  #        - mtr_{name} (dbl)           : counterfactual (static) MTR with  
  #                                       respect to the variable
  #        - mtr_{name}_baseline (dbl)  : baseline MTR with respect to
  #                                       the variable
  #        - e_{name} (dbl)             : the elasticity value
  #        - e_{name}_type (str)        : the type of elasticity, must be either
  #                                       'semi', 'arc', 'netoftax', 'taxprice'
  #   - vars (str)  : 
  #
  # Returns:  the following columns
  #        - ID (dbl)         : Identification number
  #        - new_{vars} (dbl) : the post-adjustment values of vars
  #----------------------------------------------------------------------------
  
  
  df %>%
    # Rename variables to legibility and ease of use
    rename(
      el = !!sym(paste0("e_", name)),
      e_type = !!sym(paste0("e_", name, "_type")),
      mtr = !!sym(paste0("mtr_", name)),
      mtr_baseline = !!sym(paste0("mtr_", name, "_baseline"))
    ) %>%
    # Calculate elasticity factor based on type
    mutate(factor_v = case_when(
      e_type == "semi"     ~ exp((mtr - mtr_baseline) * el) - 1,
      
      e_type == "arc"      ~ (el * (mtr / ((mtr + mtr_baseline) / 2) - 1)),
      
      e_type == "netoftax" ~ (el * ((1 - mtr) / (1 - mtr_baseline) - 1)),
      
      e_type == "taxprice" ~ (el * ((1 + mtr) / (1 + mtr_baseline) - 1)),
      
      TRUE                 ~ NA 
      ),
    # Apply elasticity factor to columns of concern
      across(vars, ~ .x * (factor_v + 1))
        ) %>%
    # Rename for later use and return
    rename_with(~ paste0("new_", .x, recycle0 = T), vars) %>%
    select(ID, starts_with("new_")) %>%
    return()
}

