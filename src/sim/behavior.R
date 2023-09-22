#------------------
# TODO
#-----------------


do_behavioral_feedback = function(tax_units, scenario_info) {
  
  #----------------------------------------------------------------------------
  # TODO
  # 
  # Parameters: TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Get directory where behavioral feedback modules are located
  root = file.path('./config/scenarios/counterfactuals/', 
                   scenario_info$ID, 
                  'behavior')  
  
  # Load modules for this scenario
  modules = list.files(root)
  walk(.x    = modules, 
       .f    = load_behavior_module, 
       id    = scenario_info$ID, 
       envir = environment())
  
  # Apply behavioral feedback functions
  fns = paste0('adjust_', str_sub(modules, end = -3))
  pmap(.f    = do.call,
       .l    = list(what = fns), 
       args  = list(tax_units), 
       envir = environment()) %>% 
    return()
}



apply_mtr_elasticity = function(tax_units, name, max_adj) {
  
  #----------------------------------------------------------------------------
  # Adjusts a category of variable based on their elasticity with respect to 
  # the marginal tax rate (MTR).
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units containing the following columns
  #      - mtr_{var} (dbl)           : counterfactual (static) MTR w/r/t the 
  #                                    variable
  #      - mtr_{var}_baseline (dbl)  : baseline MTR w/r/t the variable
  #      - e_{var} (dbl)             : the elasticity value
  #      - e_{var}_type (str)        : the type of elasticity, must be one of
  #                                    ['semi', 'arc', 'netoftax', 'taxprice']
  #   - var (str)     : name (i.e. alias) of the variable we're adjusting
  #   - max_adj (dbl) : absolute value of maximum adjustment as measured by 
  #                     percent change. For example, a value of 1 means any 
  #                     adjustment greater than 100% or less than -100% will
  #                     be limited to that max value. Helps catch implausible 
  #                     responses stemming from edge cases in MTR changes.
  #
  # Returns:  the following columns
  #        - id (dbl)         : tax unit ID
  #        - new_{vars} (dbl) : the post-adjustment values of vars
  #----------------------------------------------------------------------------
  
  tax_unit  %>%
    
    # Rename variables for legibility and ease of use
    rename(
      e            = !!sym(paste0("e_", var)),
      e_type       = !!sym(paste0("e_", var, "_type")),
      mtr          = !!sym(paste0("mtr_", var)),
      mtr_baseline = !!sym(paste0("mtr_", var, "_baseline"))
    ) %>%
    
    mutate(
      
      # Calculate adjustment factor based on type
      pct_chg = case_when(
        e_type == "semi"     ~ exp((mtr - mtr_baseline) * e) - 1,
        e_type == "arc"      ~ (e * (mtr / ((mtr + mtr_baseline) / 2) - 1)),
        e_type == "netoftax" ~ (e * ((1 - mtr) / (1 - mtr_baseline) - 1)),
        e_type == "taxprice" ~ (e * ((1 + mtr) / (1 + mtr_baseline) - 1)),
        TRUE                 ~ NA 
      ),
      
      # Limit adjustment to maximum allowed
      pct_chg = pmax(-max_adj, pmin(pct, max_adj)),
    
      # Apply elasticity factor to columns of concern
      across(.cols = all_of(var),
             .fns  = ~ . * (1 + pct_chg))
    ) %>%
    
    # Select post-adjustment variable and return
    select(ID, all_of(var)) %>%
    return()
}



load_behavior_module = function(id, name, envir) { 
  
  #----------------------------------------------------------------------------
  # Executes a given behavioral feedback module script for a given scenario,
  # defining an "adjust_" function in a given environment.
  # 
  # Parameters: 
  #   - id (str)    : scenario ID
  #   - name (str)  : module name (e.g. "kg.R" containing "adjust_kg()")
  #   - envir (env) : environment in which to execute the module code
  #
  # Returns: void.
  #----------------------------------------------------------------------------

  # Get directory where behavioral feedback modules are located
  path = file.path('./config/scenarios/counterfactuals/', 
                   scenario_info$ID, 
                   'behavior', 
                   name)
  
  # Execute function definition
  sys.source(path, envir)
}
