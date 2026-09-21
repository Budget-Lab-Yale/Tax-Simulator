#-----------------------------------------------
# Function to calculate state taxable income
#-----------------------------------------------

# Set return variables for function
return_vars$calc_st_txbl = c('st_txbl_inc')


calc_st_txbl = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates state taxable income: state base less deductions and
  # exemptions, plus deduction addbacks.
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_txbl_inc (dbl) : state taxable income
  #----------------------------------------------------------------------------

  req_vars = c(
    'st_agi',     # (dbl) state income base
    'st_ded',     # (dbl) state deduction
    'st_addback', # (dbl) state deduction addbacks
    'st_exempt',  # (dbl) state exemption allowance
    'st_child_ded' # (dbl) state child deduction
  )

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(st_txbl_inc = pmax(0, st_agi - st_ded + st_addback - st_exempt -
                                st_child_ded)) %>%
    select(all_of(return_vars$calc_st_txbl)) %>%
    return()
}
