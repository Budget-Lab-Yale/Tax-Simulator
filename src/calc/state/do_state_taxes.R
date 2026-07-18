#---------------------------------------------------------------------------
# do_state_taxes.R
#
# Orchestrator for state individual income tax calculation, mirroring
# do_1040(). Operates on tax units AFTER the federal pass, with one state's
# st_-prefixed tax law columns already joined (per-state loop in Phase 4).
# Design: other/state_tax_research/state_tax_implementation_plan.md §2.3
#---------------------------------------------------------------------------


do_state_taxes = function(tax_units, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # Calculates state individual income tax for all tax units under one
  # state's law. Expects federal-pass outputs (agi, txbl_inc, itemizing,
  # itemized components, eitc, ctc_*, cdctc_*, std_ded) and st_* law columns
  # on the input tibble.
  #
  # Parameters:
  #   - tax_units (df) : tibble of tax units post-federal calculation, with
  #                      one state's law columns joined
  #
  # Returns: tibble of state-calculated variables (df).
  #----------------------------------------------------------------------------

  tax_units %>%

    # Fill parameters absent from this state's config (feature-not-present)
    ensure_st_params() %>%

    # State AGI: starting point plus additions minus subtractions
    bind_cols(calc_st_agi(., credit_tables = credit_tables)) %>%

    # Deductions and addbacks
    bind_cols(calc_st_ded(.)) %>%

    # Exemptions
    bind_cols(calc_st_exempt(.)) %>%

    # State child deductions (North Carolina-style AGI tables)
    bind_cols(calc_st_child_ded(.)) %>%

    # Taxable income
    bind_cols(calc_st_txbl(.)) %>%

    # Tax before credits (rate schedule + recapture)
    bind_cols(calc_st_tax(.)) %>%

    # Credits
    bind_cols(calc_st_credits(., credit_tables = credit_tables)) %>%

    # Liability and state-filer flag
    bind_cols(calc_st_liab(.)) %>%

    # Narrow taxes and state transfers that sit outside the broad IIT base
    bind_cols(calc_st_special(.)) %>%

    # Return calculated state variables only
    select(all_of(unname(unlist(return_vars[str_detect(names(return_vars),
                                                       '^calc_st_')])))) %>%
    return()
}



ensure_st_params = function(tax_units) {

  #----------------------------------------------------------------------------
  # Guarantees that every optional state law parameter column exists,
  # defaulting to a neutral no-feature value. States encode only the
  # parameters for features they have (plan §2.2 convention); bind_rows()
  # across states yields NA for others, and single-state law slices may lack
  # the columns entirely. Core parameters (st_agi.start_point, st_ord.*) are
  # NOT defaulted here -- their absence is an error caught by req_vars.
  #
  # Parameters:
  #   - tax_units (df) : tibble with one state's law columns joined
  #
  # Returns: tibble with all optional st_* parameter columns present (df).
  #----------------------------------------------------------------------------

  defaults = st_param_defaults()

  # Add absent columns, then replace NAs with defaults
  for (p in names(defaults)) {
    if (!(p %in% colnames(tax_units))) {
      tax_units[[p]] = defaults[[p]]
    } else {
      tax_units[[p]] = coalesce(tax_units[[p]], defaults[[p]])
    }
  }

  # Dependent-filer standard deduction falls back to the regular amount
  tax_units[['st_ded.std_dependent']] = coalesce(
    tax_units[['st_ded.std_dependent']], tax_units[['st_ded.std_amount']]
  )

  # Vector params (household credit tables, CTC tiers, CDCTC anchors) are
  # feature-gated on their first element existing; add sentinel if absent
  for (p in st_param_vector_sentinels()) {
    if (!(p %in% colnames(tax_units))) {
      tax_units[[p]] = NA_real_
    }
  }

  return(tax_units)
}
