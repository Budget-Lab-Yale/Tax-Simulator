#------------------------------------------
# Function to calculate standard deduction
#------------------------------------------

# Set return variables for function
return_vars$calc_std_ded = c('std_ded')


calc_std_ded = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates standard deduction. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - std_ded (dbl) : value of standard deduction (even if itemizing)
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'dep_status', # (bool) whether tax filer is a dependent
    'ei',         # (dbl)  earned income of tax unit
    'age1',       # (int)  age of primary filer
    'age2',       # (int)  age of secondary filer
    'blind1',     # (bool) whether primary filer is blind
    'blind2',     # (bool) whether secondary filer is blind
    'agi',        # (dbl)  Adjusted Gross Income
    
    # Tax law attributes
    'std.value',            # (int) base value of standard deduction
    'std.bonus',            # (int) bonus value per instances of nondependent adults who are either aged 65+ or blind
    'std.dep_floor',        # (int) Minimum standard deduction for dependents
    'std.dep_earned_bonus', # (int) Amount of bonus deduction added to dependent's earned income
    'std.bonus_other',      # (int) Amount of bonus deduction added for any other reason
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # For nondependents, calculate standard deduction bonus value
      age_bonus1  = age1 >= 65,
      age_bonus2  = !is.na(age2) & (age2 >= 65),
      n_bonuses   = age_bonus1 + age_bonus2 + blind1 + (!is.na(blind2) & blind2),
      
      # Temporary bonus for elderly filers only
      elderly_bonus = (age_bonus1 + age_bonus2) * std.bonus_elderly_temp_value,
      elderly_bonus = pmax(0, elderly_bonus - (.04 * pmax(0, agi-std.bonus_elderly_temp_thresh))),
      
      bonus_value = std.bonus * n_bonuses + elderly_bonus,
      
      # Calculate nondependent total standard deduction
      std_ded = std.value + bonus_value + std.bonus_other,
      
      # Calculate dependent standard deduction
      dep_std_ded = pmax(std.dep_floor, ei + std.dep_earned_bonus),
      
      # Limit dependent standard deduction to actual standard deduction
      dep_std_ded = pmin(std_ded, dep_std_ded),
      
      # Assign final deduction value based on dependent status
      std_ded = if_else(!dep_status, std_ded, dep_std_ded)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_std_ded)) %>% 
    return()
}

