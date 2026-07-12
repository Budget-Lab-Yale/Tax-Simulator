#--------------------------------------------------
# Function to calculate state exemption allowances
#--------------------------------------------------

# Set return variables for function
return_vars$calc_st_exempt = c('st_exempt')


calc_st_exempt = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates state personal/dependent exemption allowances, including
  # aged/blind additional amounts and the high-income disallowance (IL-style
  # cliff at the phase-out threshold when po_type = 0).
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_exempt (dbl) : total state exemption allowance
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',            # (dbl)  federal AGI
    'filing_status',  # (int)  filing status (1 single, 2 MFJ, 3 MFS, 4 HoH)
    'n_dep',          # (int)  number of dependents
    'age1',           # (int)  age of primary filer
    'age2',           # (int)  age of secondary filer (NA if none)
    'blind1',         # (bool) whether primary filer is blind
    'blind2',         # (bool) whether secondary filer is blind

    # State tax law
    'st_exempt.personal_amount', # (dbl) exemption per taxpayer/spouse
    'st_exempt.dep_amount',      # (dbl) exemption per dependent
    'st_exempt.aged_addl',       # (dbl) additional exemption per person 65+
    'st_exempt.blind_addl',      # (dbl) additional exemption per blind person
    'st_exempt.po_thresh',       # (dbl) AGI disallowance threshold (mapped)
    'st_exempt.po_type'          # (int) 0 = cliff (full disallowance)
  )

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      n_taxpayers = 1 + (filing_status == 2),
      n_aged      = (age1 >= 65) + (filing_status == 2 & !is.na(age2) & age2 >= 65),
      n_blind     = blind1 + (!is.na(blind2) & blind2),

      st_exempt_gross = n_taxpayers * st_exempt.personal_amount +
                        n_dep       * st_exempt.dep_amount +
                        n_aged      * st_exempt.aged_addl +
                        n_blind     * st_exempt.blind_addl,

      # High-income disallowance (cliff when po_type = 0; other phase-out
      # types added as states require them)
      st_exempt = if_else(agi > st_exempt.po_thresh & st_exempt.po_type == 0,
                          0, st_exempt_gross)
    ) %>%
    select(all_of(return_vars$calc_st_exempt)) %>%
    return()
}
