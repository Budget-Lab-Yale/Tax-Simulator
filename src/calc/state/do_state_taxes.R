#---------------------------------------------------------------------------
# do_state_taxes.R
#
# Orchestrator for state individual income tax calculation, mirroring
# do_1040(). Operates on tax units AFTER the federal pass, with one state's
# st_-prefixed tax law columns already joined (per-state loop in Phase 4).
# Design: other/state_tax_research/state_tax_implementation_plan.md §2.3
#---------------------------------------------------------------------------


do_state_taxes = function(tax_units, credit_tables = NULL, law_mfs = NULL) {

  #----------------------------------------------------------------------------
  # Calculates state individual income tax for all tax units under one
  # state's law. Expects federal-pass outputs (agi, txbl_inc, itemizing,
  # itemized components, eitc, ctc_*, cdctc_*, std_ded) and st_* law columns
  # on the input tibble.
  #
  # Where the state offers a MARRIED-SEPARATE ELECTION (st_ord.split_election),
  # the whole pipeline is run a second and third time on per-spouse half-units
  # and the couple takes whichever is cheaper -- see st_split_election().
  #
  # Parameters:
  #   - tax_units (df)  : tibble of tax units post-federal calculation, with
  #                       one state's law columns joined
  #   - law_mfs (df)    : ONE row of this state-year's law, resolved at filing
  #                       status 3, with the state/year/filing_status keys
  #                       dropped -- i.e. the same shape as the columns already
  #                       joined onto tax_units. Required only by an electing
  #                       state, because the filing-status mapper is resolved
  #                       at join time: a half-unit built by relabelling
  #                       filing_status would still carry the JOINT parameter
  #                       values, so the married-separate row has to be
  #                       supplied rather than derived
  #
  # Returns: tibble of state-calculated variables (df).
  #----------------------------------------------------------------------------

  tax_units %<>% ensure_st_params()

  joint = st_pipeline(tax_units, credit_tables)

  # Married-separate election, opt-in per state and computed only where any
  # row of the slice declares it. Strictly a no-op otherwise: a state that
  # leaves split_election at 0 never enters the branch at all
  if (any(tax_units$st_ord.split_election == 1)) {
    joint = st_split_election(tax_units, joint, credit_tables, law_mfs)
  }

  return(joint)
}


st_pipeline = function(tax_units, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # The state calculator proper: one pass of the stage sequence over units
  # whose parameters have already been backfilled by ensure_st_params().
  # Factored out of do_state_taxes() so the married-separate election can run
  # it again on per-spouse half-units.
  #
  # Parameters:
  #   - tax_units (df)     : tibble with law columns joined and params filled
  #   - credit_tables (df) : dense credit schedules
  #
  # Returns: tibble of state-calculated variables (df).
  #----------------------------------------------------------------------------

  tax_units %>%

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
