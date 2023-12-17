#--------------------------------------------
# Functions for modeling behavioral feedback
#--------------------------------------------


do_behavioral_feedback = function(tax_units, behavior_modules, baseline_mtrs, static_mtrs) {

  #----------------------------------------------------------------------------
  # Locates and executes all behavioral feedback modules for a given scenario.
  # Modules are stored as {var}.R in the /behavior root of a scenario's config
  # folder, and each module contains a function called adjust_{var}.
  # 
  # Parameters: 
  #   - tax_units (df)           : tibble of tax unit data, pre tax calculation 
  #   - behavior_modules (str[]) : filepaths from runscript containing which 
  #                                behavioral modules to run 
  #   - baseline_mtrs (df)       : tibble of MTRs under the baseline, indexed 
  #                                by year/tax unit id 
  #   - static_mtrs (df)         : tibble of MTRs under the static 
  #                                counterfactual scenario, indexed by year/tax 
  #                                unit id
  #
  # Returns: tibble of tax units containing only the adjusted columns (df). 
  #----------------------------------------------------------------------------
  
  
  # Load modules for this scenario
  walk(.x    = behavior_modules,
       .f    = load_behavior_module, 
       envir = environment())
    
  # Apply behavioral feedback functions
  fns = behavior_modules %>% 
    str_split('/') %>% 
    map(.f = ~ .x[[1]]) %>%
    unlist() %>% 
    paste0('adjust_', .)
  
  pmap(.f    = do.call,
       .l    = list(what = fns), 
       args  = list(tax_units, baseline_mtrs, static_mtrs), 
       envir = environment()) %>% 
    bind_cols() %>%
    return()
}



apply_mtr_elasticity = function(tax_units, var, baseline_mtrs, static_mtrs, max_adj) {
  
  #----------------------------------------------------------------------------
  # Adjusts a category of variable based on their elasticity with respect to 
  # the marginal tax rate (MTR).
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units containing the following columns
  #      - e_{var} (dbl)      : the elasticity value
  #      - e_{var}_type (str) : the type of elasticity, must be one of
  #                             ['semi', 'arc', 'netoftax', 'taxprice']
  #   - var (str)          : name (i.e. alias) of the variable we're adjusting
  #   - baseline_mtrs (df) : tibble of MTRs under the baseline, including the 
  #                          column mtr_{var}
  #   - static_mtrs (df)   : tibble of MTRs under the static counterfactual 
  #                          scenario, including the column mtr_{var}
  #   - max_adj (dbl)      : absolute value of maximum adjustment as measured  
  #                          by percent change. For example, a value of 1 means 
  #                          any adjustment greater than 100% or less than 
  #                          -100% will be limited to that max value. Helps 
  #                          catch implausible responses stemming from edge 
  #                          cases in MTR changes.
  #
  # Returns: tibble with one column for the post-adjustment variable
  #----------------------------------------------------------------------------
  
  tax_units %>%
  
    # Join MTRs
    left_join(baseline_mtrs %>% 
                 rename_with(.cols = -c(id, year), 
                             .fn   = ~ paste0(., '_baseline')), 
               by = c('id', 'year')) %>%
    left_join(static_mtrs, by = c('id', 'year')) %>% 
      
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
      pct_chg = pmax(-max_adj, pmin(pct_chg, max_adj)),
    
      # Apply elasticity factor to columns of concern
      across(.cols = all_of(var),
             .fns  = ~ . * (1 + pct_chg))
    ) %>%
    
    # Select post-adjustment variable and return
    select(all_of(var)) %>%
    return()
}



load_behavior_module = function(path, envir) { 
  
  #----------------------------------------------------------------------------
  # Executes a given behavioral feedback module script for a given scenario,
  # defining an "adjust_" function in a given environment.
  # 
  # Parameters: 
  #   - path (str)  : module path (e.g. "kg_lt/70.R" containing "adjust_kg()")
  #   - envir (env) : environment in which to execute the module code
  #
  # Returns: void.
  #----------------------------------------------------------------------------

  path %>% 
    paste0('.R') %>% 
    file.path('./config/scenarios/behavior/', .) %>% 
    sys.source(envir)
}



do_salt_workaround_baseline = function(tax_units) {
  
  #----------------------------------------------------------------------------
  # Adjusts baseline projected tax data values for SALT and pass-through 
  # income to reflect the so-called SALT cap workarounds in which pass-through
  # entities can elect to pay state income taxes at the entity level, which 
  # converts would-be SALT deductions into lower reported Schedule E net 
  # income. Note that this function is unique: it's not "behavioral feedback"
  # in the traditional sense because it's being applied to the baseline, which 
  # should in general reflect baseline tax policy. Despite this theoretical 
  # awkwardness, it's easiest in practice to include this part of our 
  # projections here, so we do. Nothing is ever perfectly clean :) 
  # 
  # Parameters: 
  #   - tax_units (df) : tibble of tax unit data  
  #
  # Returns: tibble of updated tax unit data (df).
  #----------------------------------------------------------------------------
  
  set.seed(76)
  
  
  tax_units %>% 
    mutate(
      
      # Determine SALT attributable to pass-through activities 
      part  = part_active + part_passive - part_active_loss - 
              part_passive_loss - part_179,
      scorp = scorp_active + scorp_passive - scorp_active_loss - 
              scorp_passive_loss - scorp_179,
      inc = wages + trad_contr_er1 + trad_contr_er2 + txbl_int + exempt_int + 
            div_ord + div_pref + state_ref + txbl_ira_dist + gross_pens_dist + 
            kg_st + kg_lt + other_gains + alimony + sole_prop + part + scorp + 
            farm + gross_ss + ui + other_inc,
      
      part_share  = if_else(inc > 0, pmin(1, pmax(0, part / inc)),  0),
      scorp_share = if_else(inc > 0, pmin(1, pmax(0, scorp / inc)), 0),
      
      salt_part  = salt_inc_sales * part_share,
      salt_scorp = pmin(salt_inc_sales - salt_part, salt_inc_sales * scorp_share),
        
      # Simulate amount moved in workaround. Probability calibrated to hit 
      # $20B annual estimate from TPC 
      salt_workaround_part  = salt_part * (!is.infinite(item.salt_limit) & 
                                           item.salt_workaround_allowed & 
                                           salt_part > 0 & 
                                           runif(nrow(.)) < 0.75),
      salt_workaround_scorp = salt_scorp * (!is.infinite(item.salt_limit) &
                                            item.salt_workaround_allowed & 
                                            salt_scorp > 0 & 
                                            runif(nrow(.)) < 0.75),
      
      # Shift SALT
      salt_inc_sales    = salt_inc_sales - salt_workaround_part - salt_workaround_scorp,
      part_active_loss  = part_active_loss  + salt_workaround_part,
      scorp_active_loss = scorp_active_loss + salt_workaround_scorp
    ) %>%
    
    return()
  
}