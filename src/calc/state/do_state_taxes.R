#---------------------------------------------------------------------------
# do_state_taxes.R
#
# Orchestrator for state individual income tax calculation, mirroring
# do_1040(). Operates on tax units AFTER the federal pass, with one state's
# st_-prefixed tax law columns already joined (per-state loop in Phase 4).
# Design: research/state_tax/plan.md §2.3
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

  tax_units %<>% ensure_st_params() %>% ensure_st_data()

  joint = st_pipeline(tax_units, credit_tables)

  # Own-state income tax as an itemized deduction (st_ded.salt_own_tax_impute;
  # HI Worksheet A-2, an election independent of the federal one). State
  # income tax paid is observed only for federal itemizers; for everyone else
  # it is taken to be the unit's own state tax after nonrefundable credits
  # (payments in the year ~ liability, as TAXSIM and PolicyEngine assume).
  # The deduction lowers the tax it is based on, so the pipeline is re-run
  # to a fixed point -- three further passes; each shrinks the gap by the
  # marginal state rate (HI 8.25%: $23 after two, under $2 after three)
  if (any(tax_units$st_ded.salt_own_tax_impute == 1)) {
    impute = tax_units$st_ded.salt_own_tax_impute == 1 & tax_units$itemizing == 0
    for (pass in 1:3) {
      tax_units$salt_inc_sales = if_else(
        impute,
        pmax(0, joint$st_tax_pre_credit - joint$st_credits_nonref),
        tax_units$salt_inc_sales
      )
      joint = st_pipeline(tax_units, credit_tables)
    }
  }
  # Diagnostic: TRUE where the married-separate election won. Who owns
  # non-wage income is unobserved and the split halves it, so cross-model
  # predicates need to see which couples the convention moved
  joint$st_split_used = FALSE
  joint$st_split_gain = 0   # joint liability less the split liability, where the split wins

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

    # Elective whole-income table replacing schedule and deduction (AR)
    calc_st_alt_table(credit_tables = credit_tables) %>%

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



ensure_st_data = function(tax_units, schema = supplemental_schema()) {

  #----------------------------------------------------------------------------
  # The data twin of ensure_st_params(): guarantees that every OPTIONAL DATA
  # column in config/interfaces/supplemental_inputs_schema.yaml exists on the
  # units, resolved in this order per variable v (decision S43):
  #   1. an overlay column sup.v is present (attach_supplemental_inputs()):
  #      v = coalesce(sup.v, default), has_v = !is.na(sup.v); if v already
  #      existed it is overwritten here, inside the state context only
  #      (schema `shadow: allow`, the Tax-Data placeholder case);
  #   2. else v is present natively (Tax-Data absorbed it): untouched, has_v = TRUE;
  #   3. else the schema's tax_data_equivalent column is present (Tax-Data
  #      carries the concept under its own name, e.g. trad_contr_er1 for
  #      elective_deferral1): v = that column, has_v = TRUE -- so a producer
  #      filling the placeholder switches the provision on with no code change;
  #   4. else v = default, has_v = FALSE.
  # The sup.* columns are dropped afterwards, so the stages read bare names.
  # A stage gates on has_v only where the default is not self-evidently
  # inert; the invariance test requires that with every has_v FALSE the
  # state outputs equal today's.
  #
  # Parameters:
  #   - tax_units (df) : tibble with one state's law columns joined
  #
  # Returns: tibble with all schema data columns and has_<v> flags present (df).
  #----------------------------------------------------------------------------

  vars = schema$vars
  for (i in seq_len(nrow(vars))) {
    v  = vars$variable[i]
    sv = paste0(SUP_PREFIX, v)
    hv = paste0('has_', v)
    default = schema$defaults[[v]]
    if (sv %in% colnames(tax_units)) {
      tax_units[[hv]] = !is.na(tax_units[[sv]])
      tax_units[[v]]  = coalesce(sup_cast(tax_units[[sv]], vars$type[i]), default)
      tax_units[[sv]] = NULL
    } else if (v %in% colnames(tax_units)) {
      tax_units[[hv]] = TRUE
    } else if (!is.na(vars$tax_data_equivalent[i]) &&
               vars$tax_data_equivalent[i] %in% colnames(tax_units)) {
      tax_units[[v]]  = coalesce(sup_cast(tax_units[[vars$tax_data_equivalent[i]]],
                                          vars$type[i]), default)
      tax_units[[hv]] = TRUE
    } else {
      tax_units[[v]]  = default
      tax_units[[hv]] = FALSE
    }
  }
  tax_units
}
