#--------------------------------------------------
# Function to calculate state exemption allowances
#--------------------------------------------------

# Set return variables for function
return_vars$calc_st_exempt = c('st_exempt')


calc_st_exempt = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates state personal/dependent exemption allowances, including
  # aged/blind additional amounts and the high-income disallowance: an
  # IL-style cliff at the phase-out threshold (po_type = 0) or a CT Table
  # A-style stepped reduction of po_reduction_per_step for each po_step (or
  # fraction thereof) of income above the threshold (po_type = 1). The
  # phase-out income measure is federal AGI or state AGI per po_agi_base.
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
    'st_agi',         # (dbl)  state income base (calculated upstream in the pipe)
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
    'st_exempt.po_type',         # (int) 0 = cliff, 1 = stepped reduction
    'st_exempt.po_step',         # (dbl) income step size for po_type 1
    'st_exempt.po_reduction_per_step', # (dbl) reduction per step for po_type 1
    'st_exempt.po_agi_base'      # (int) phase-out income: 1 fed AGI, 2 state AGI
  )

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      n_taxpayers = 1 + (filing_status == 2),
      n_aged      = (age1 >= 65) + (filing_status == 2 & !is.na(age2) & age2 >= 65),
      n_blind     = coalesce(blind1, 0) + (!is.na(blind2) & blind2),

      st_exempt_gross = n_taxpayers * st_exempt.personal_amount +
                        n_dep       * st_exempt.dep_amount +
                        n_aged      * st_exempt.aged_addl +
                        n_blind     * st_exempt.blind_addl,

      # High-income disallowance: cliff (po_type 0) or stepped reduction of
      # po_reduction_per_step per po_step, or fraction thereof, of income
      # over the threshold (po_type 1; CT-1040 Table A)
      po_income = if_else(st_exempt.po_agi_base == 2, st_agi, agi),
      st_exempt = case_when(
        st_exempt.po_type == 1 ~
          pmax(0, st_exempt_gross - st_exempt.po_reduction_per_step *
                    ceiling(pmax(0, po_income - st_exempt.po_thresh) /
                              st_exempt.po_step)),
        po_income > st_exempt.po_thresh ~ 0,
        TRUE ~ st_exempt_gross
      )
    ) %>%
    select(all_of(return_vars$calc_st_exempt)) %>%
    return()
}
