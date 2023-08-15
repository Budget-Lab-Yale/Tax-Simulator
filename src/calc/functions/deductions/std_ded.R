#------------------------------------------
# Function to calculate standard deduction
#------------------------------------------


calc_std_ded = function(tax_unit) {

  #----------------------------------------------------------------------------
  # Calculates standard deduction. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - std_ded (dbl) : value of standard deduction (even if itemizing)
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'dep_status',          # (bool, self) whether tax filer is a dependent
    'wages',               # (dbl, self)  W2 wages
    'age1',                # (int, self)  age of primary filer
    'age2',                # (int, self)  age of secondary filer
    'blind1',              # (bool, self) whether primary filer is blind
    'blind2',              # (bool, self) whether secondary filer is blind
    'std.value',           # (int, law)   base value of standard deduction
    'std.bonus',           # (int, law)   bonus value per instances of non- 
                           #              dependent adults who are either aged 
                           #              65+ or blind
    'std.dep_floor',       # (int, law)   Minimum standard deduction for 
                           #              dependent returns
    'std.dep_earned_bonus' # (int, law)   Amount of bonus deduction added to
                           #              dependent's earned income
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # For nondependents, calculate standard deduction bonus value
      n_bonuses   = (age1 >= 65) + (age2 >= 65) + blind1 + blind2,
      bonus_value = std.bonus * n_bonuses,
      
      # Calculate nondependent total standard deduction
      std_ded = std.value + bonus_value,
      
      # Calculate dependent standard deduction
      dep_std_ded = pmax(std.dep_floor, wages + std.dep_earned_bonus),
      
      # Limit dependent standard deduction to actual standard deduction
      dep_std_ded = pmin(std_ded, dep_std_ded),
      
      # Assign final deduction value based on dependent status
      std_ded = if_else(!dep_status, std_ded, dep_std_ded)
      
    ) %>% 
    
    # Keep variables to return
    select(std_ded) %>% 
    return()
}

