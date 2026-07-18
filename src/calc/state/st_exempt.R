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
    'st_bid',         # (dbl)  business carve-out deduction (calc_st_agi; OH MAGI)
    'st_age_package_forgone', # (int) aged package forgone for EITC/CLI (calc_st_agi)
    'filing_status',  # (int)  filing status (1 single, 2 MFJ, 3 MFS, 4 HoH)
    'dep_status',     # (bool) whether filer is a dependent
    'n_dep',          # (int)  number of dependents
    'age1',           # (int)  age of primary filer
    'age2',           # (int)  age of secondary filer (NA if none)
    'blind1',         # (bool) whether primary filer is blind
    'blind2',         # (bool) whether secondary filer is blind

    # State tax law
    'st_exempt.personal_amount', # (dbl) exemption per taxpayer (or per return)
    'st_exempt.personal_per_return', # (int) personal_amount is per RETURN (CT)
    'st_exempt.dep_amount',      # (dbl) exemption per dependent
    'st_exempt.aged_addl',       # (dbl) additional exemption per person 65+
    'st_exempt.blind_addl',      # (dbl) additional exemption per blind person
    'st_exempt.aged_blind_addl_excl_eitc', # (int) add-ons forgone with EITC/CLI (VA)
    'st_exempt.po_thresh',       # (dbl) AGI disallowance threshold (mapped)
    'st_exempt.po_type',         # (int) 0 = cliff, 1 = stepped reduction
    'st_exempt.po_step',         # (dbl) income step size for po_type 1
    'st_exempt.po_reduction_per_step', # (dbl) reduction per step for po_type 1
    'st_exempt.po_agi_base',     # (int) phase-out income base (st_income_base enum)
    'st_exempt.tier_income_base', # (int) tiered-amount income base (enum)
    'st_exempt.dep_filer_zero'   # (int) dependent filers get zero exemption (OH)
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Phase-out income base per the uniform enum (st_income_base)
  po_income_v = st_income_base(tax_unit, tax_unit$st_exempt.po_agi_base)

  # Income-tiered per-exemption amount (OH 5747.025): where the tier family
  # is encoded, one amount applies to taxpayer, spouse, and each dependent,
  # selected by income band ((lower, upper] semantics; zero above the top
  # bound encodes the HB 96 high-income denial). Overrides personal_amount
  # and dep_amount
  tier_amount_v = NULL
  tier_ub = st_family_matrix(tax_unit, 'st_exempt.tier_bounds')
  if (!is.null(tier_ub)) {
    tier_amt = st_family_matrix(tax_unit, 'st_exempt.tier_amounts',
                                1:ncol(tier_ub), require_sentinel = FALSE)
    tier_income = st_income_base(tax_unit, tax_unit$st_exempt.tier_income_base)
    tier_amount_v = st_band_value(tier_income, tier_ub, tier_amt)
  }

  tax_unit %>%
    mutate(

      # personal_amount is per TAXPAYER by default (x2 for MFJ); a state
      # whose form publishes a per-RETURN amount (CT Table A) sets
      # personal_per_return = 1 and transcribes the form value directly
      # (2026-07-17 review item #5)
      n_taxpayers = if_else(st_exempt.personal_per_return == 1,
                            1, 1 + (filing_status == 2)),
      n_aged      = (age1 >= 65) + (filing_status == 2 & !is.na(age2) & age2 >= 65),
      n_blind     = coalesce(blind1, 0) + (!is.na(blind2) & blind2),

      # Aged/blind add-ons are part of the age package where a state makes
      # it mutually exclusive with the EITC/CLI (VA); zeroed when the unit
      # took the EITC side of that choice (decided in calc_st_agi)
      addl_factor = if_else(st_exempt.aged_blind_addl_excl_eitc == 1 &
                              st_age_package_forgone == 1, 0, 1),

      # Tiered per-exemption amount overrides the flat amounts where encoded;
      # dependent filers get zero where flagged (OH 5747.025(B))
      st_personal_v = if (is.null(tier_amount_v)) st_exempt.personal_amount
                      else tier_amount_v,
      st_dep_v      = if (is.null(tier_amount_v)) st_exempt.dep_amount
                      else tier_amount_v,
      dep_filer_factor = if_else(st_exempt.dep_filer_zero == 1 & dep_status == 1,
                                 0, 1),

      st_exempt_gross = (n_taxpayers * st_personal_v * dep_filer_factor +
                         n_dep       * st_dep_v +
                         n_aged      * st_exempt.aged_addl * addl_factor +
                         n_blind     * st_exempt.blind_addl * addl_factor),

      # High-income disallowance: cliff (po_type 0) or stepped reduction of
      # po_reduction_per_step per po_step, or fraction thereof, of income
      # over the threshold (po_type 1; CT-1040 Table A)
      po_income = po_income_v,
      st_exempt = case_when(
        st_exempt.po_type == 1 ~
          pmax(0, st_exempt_gross -
                  st_step_reduction(po_income, st_exempt.po_thresh,
                                    st_exempt.po_step,
                                    st_exempt.po_reduction_per_step)),
        po_income > st_exempt.po_thresh ~ 0,
        TRUE ~ st_exempt_gross
      )
    ) %>%
    select(all_of(return_vars$calc_st_exempt)) %>%
    return()
}
