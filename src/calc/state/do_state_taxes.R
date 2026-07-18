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
    bind_cols(calc_st_agi(.)) %>%

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



st_param_defaults = function() {

  #----------------------------------------------------------------------------
  # Neutral no-feature default for every optional scalar state law parameter.
  # This named vector doubles as the registry of legal scalar parameter names:
  # ensure_st_params() consumes it for backfilling, and
  # st_param_name_registry() consumes it for load-time name validation
  # (2026-07-17 review items #1/#2). A parameter read by any state calculator
  # MUST appear here (or in a vector family below) or the name validator will
  # reject every state that encodes it.
  #
  # Returns: named numeric vector of defaults, names are full column names
  #          (num[]).
  #----------------------------------------------------------------------------

  c(

    # programs.yaml
    'st_programs.broad_iit'        = 1,
    'st_programs.narrow_iit'       = 0,
    'st_programs.ltcg_excise'      = 0,
    'st_programs.wftc'             = 0,

    # agi.yaml
    'st_agi.start_point'           = 1,
    'st_agi.add_exempt_int'        = 0,
    'st_agi.own_state_exempt'      = 0,
    'st_agi.sub_us_int'            = 0,
    'st_agi.sub_state_ref'         = 0,
    'st_agi.ss_sub_share'          = 0,
    'st_agi.ss_full_sub_65plus'    = 0,
    'st_agi.ss_full_sub_5564'      = 0,
    'st_agi.ss_5564_agi_limit'     = Inf,
    'st_agi.ss_full_sub_allages'   = 0,
    'st_agi.ss_allages_agi_limit'  = Inf,
    'st_agi.pension_excl_under65'  = 0,
    'st_agi.pension_excl_65plus'   = 0,
    'st_agi.pension_excl_min_age'  = Inf,
    'st_agi.pension_cap_incl_ss'   = 0,
    'st_agi.retirement_excl_style' = 0,
    'st_agi.retirement_excl_min_age' = Inf,
    'st_agi.retirement_excl_under65' = 0,
    'st_agi.retirement_excl_65plus' = 0,
    'st_agi.retirement_excl_earned_cap' = 0,
    'st_agi.sub_char_nonitem_floor' = Inf,
    'st_agi.add_overtime_ded'      = 0,
    'st_agi.cap_gains_excl_share'  = 0,
    'st_agi.div_excl_share'        = 0,
    'st_agi.ss_taxable_gross_cap_share' = Inf,
    'st_agi.pension_sub_share'     = 0,
    'st_agi.ira_sub_share'         = 0,

    # ded.yaml
    'st_ded.std_amount'            = 0,
    'st_ded.std_dependent'         = NA,   # falls back to std_amount
    'st_ded.std_dependent_style'   = 0,
    'st_ded.std_dependent_floor'   = 0,
    'st_ded.std_dependent_earned_add' = 0,
    'st_ded.std_aged_addl'          = 0,
    'st_ded.std_blind_addl'         = 0,
    'st_ded.std_char_share'         = 0,
    'st_ded.std_char_floor'         = 0,
    'st_ded.item_allowed'          = 0,
    'st_ded.item_coupling'         = 0,
    'st_ded.salt_addback'          = 0,
    'st_ded.item_component_style'  = 0,
    'st_ded.item_include_medical'  = 0,
    'st_ded.item_include_mortgage' = 0,
    'st_ded.item_include_investment' = 0,
    'st_ded.item_include_charity'  = 0,
    'st_ded.item_include_casualty' = 0,
    'st_ded.item_include_misc'     = 0,
    'st_ded.item_include_other'    = 0,
    'st_ded.item_include_prop_tax' = 0,
    'st_ded.item_include_pers_tax' = 0,
    'st_ded.item_include_income_sales_tax' = 0,
    'st_ded.item_prop_tax_cap'     = Inf,
    'st_ded.pease'                 = 0,
    'st_ded.pease_thresh'          = Inf,
    'st_ded.item_limit_style'      = 0,
    'st_ded.item_limit_agi_base'   = 2,
    'st_ded.item_limit_thresh'     = Inf,
    'st_ded.item_limit_rate'       = 0,
    'st_ded.item_limit_max_nonprotected_share' = 0,
    'st_ded.item_limit_protect_medical' = 0,
    'st_ded.item_limit_protect_investment' = 0,
    'st_ded.item_limit_protect_casualty' = 0,
    'st_ded.item_limit_protect_other' = 0,
    'st_ded.item_limit_po_thresh'  = Inf,
    'st_ded.item_limit_po_width'   = 50000,
    'st_ded.item_limit_share1'     = 0,
    'st_ded.item_limit_tier2_thresh' = Inf,
    'st_ded.item_limit_tier2_width'  = 50000,
    'st_ded.item_limit_share2'     = 0,
    'st_ded.char_only_thresh1'     = Inf,
    'st_ded.char_only_share1'      = 1,
    'st_ded.char_only_thresh2'     = Inf,
    'st_ded.char_only_share2'      = 1,
    'st_ded.addback_cap_thresh'    = Inf,
    'st_ded.addback_cap'           = Inf,
    'st_ded.addback_incl_std'      = 0,

    # exempt.yaml
    'st_exempt.personal_amount'    = 0,
    'st_exempt.dep_amount'         = 0,
    'st_exempt.aged_addl'          = 0,
    'st_exempt.blind_addl'         = 0,
    'st_exempt.po_thresh'          = Inf,
    'st_exempt.po_type'            = 0,
    'st_exempt.po_step'            = 1,
    'st_exempt.po_reduction_per_step' = 0,
    'st_exempt.po_agi_base'        = 1,

    # child_ded.yaml (AGI-tabled deductions, e.g. North Carolina)
    'st_child_ded.style'           = 0,

    # ord.yaml (recapture)
    'st_ord.rates1'                = 0,
    'st_ord.brackets1'             = 0,
    'st_ord.recapture_agi_start'   = Inf,
    'st_ord.recapture_width'       = 50000,

    # surtax.yaml (post-nonrefundable-credit taxable-income surtax)
    'st_surtax.taxable_income_threshold' = Inf,
    'st_surtax.taxable_income_rate' = 0,
    'st_surtax.taxable_income_round' = 0,

    # credits.yaml
    'st_credits.eitc_match'        = 0,
    'st_credits.eitc_refundable'   = 1,
    'st_credits.eitc_less_household_credit' = 0,
    'st_credits.eitc_child_bonus'  = 0,
    'st_credits.dep_credit_style'  = 0,
    'st_credits.dep_credit_young_amount' = 0,
    'st_credits.dep_credit_other_amount' = 0,
    'st_credits.dep_credit_po_thresh' = Inf,
    'st_credits.dep_credit_po_per_1k' = 0,
    'st_credits.ctc_style'         = 0,
    'st_credits.ctc_match_share'   = 0,
    'st_credits.ctc_fed_base_per_child' = 1000,
    'st_credits.ctc_min_per_child' = 0,
    'st_credits.ctc_min_child_age' = 0,
    'st_credits.ctc_max_child_age' = 16,
    'st_credits.ctc_young_age_limit' = 0,
    'st_credits.ctc_young_amount'  = 0,
    'st_credits.ctc_old_amount'    = 0,
    'st_credits.ctc_po_thresh'     = Inf,
    'st_credits.ctc_po_rate'       = 0,
    'st_credits.ctc_pct_of_eitc'   = 0,
    'st_credits.ctc_max_age'       = 16,
    'st_credits.cdctc_match'       = 0,
    'st_credits.cdctc_refundable'  = 1,
    'st_credits.cdctc_style'       = 0,
    'st_credits.cdctc_rate_max'    = 0,
    'st_credits.cdctc_rate_floor'  = 0,
    'st_credits.cdctc_rate_po_per_1k' = 0,
    'st_credits.cdctc_rate_po_start'  = Inf,
    'st_credits.prop_tax_credit_rate' = 0,
    'st_credits.credit_agi_limit'  = Inf,
    'st_credits.prop_tax_credit_max' = 0,
    'st_credits.prop_tax_credit_po_thresh' = Inf,
    'st_credits.prop_tax_credit_po_step' = 1,
    'st_credits.prop_tax_credit_po_rate' = 0,
    'st_credits.prop_tax_credit_restrict_aged_dep' = 0,
    'st_credits.fatc_young_amount' = 0,
    'st_credits.fatc_old_amount'   = 0,
    'st_credits.fatc_young_age_limit' = 0,
    'st_credits.fatc_max_child_age'   = 16,
    'st_credits.fatc_po_start'     = 0,
    'st_credits.fatc_po_step'      = 5000,
    'st_credits.fatc_po_zero'      = 0,
    'st_credits.hh_mfs_half'       = 0,
    'st_credits.family_credit_style' = 0,
    'st_credits.exempt_credit_style' = 0,
    'st_credits.exempt_credit_personal' = 0,
    'st_credits.exempt_credit_aged' = 0,
    'st_credits.exempt_credit_blind' = 0,
    'st_credits.exempt_credit_dep' = 0,
    'st_credits.exempt_credit_po_thresh' = Inf,
    'st_credits.exempt_credit_po_width' = 1,
    'st_credits.exempt_credit_po_per_step' = 0,
    'st_credits.earned_credit_style' = 0,
    'st_credits.earned_credit_age_min' = Inf,
    'st_credits.earned_credit_agi_limit' = Inf,
    'st_credits.earned_credit_earned_limit' = Inf,
    'st_credits.earned_credit_round' = 0,
    'st_credits.earned_credit_refundable' = 0,
    'st_credits.earned_credit_mfs_eligible' = 0,
    'st_credits.young_child_credit_style' = 0,
    'st_credits.young_child_credit_amount' = 0,
    'st_credits.young_child_credit_max_age' = -1,
    'st_credits.young_child_credit_phaseout_start' = Inf,
    'st_credits.young_child_credit_phaseout_per_100' = 0,
    'st_credits.young_child_credit_zero_income_enabled' = 0,
    'st_credits.young_child_credit_zero_income_wage_limit' = -Inf,
    'st_credits.young_child_credit_zero_income_loss_limit' = -Inf,
    'st_credits.young_child_credit_zero_income_agi_limit' = -Inf,

    # filing.yaml
    'st_filing.req_type'           = 0,
    'st_filing.req_income_thresh'  = Inf,
    'st_filing.req_income_thresh_dep' = Inf,
    'st_filing.req_if_fed_filer'   = 0,

    # investment_income.yaml (NH/TN-style narrow taxes)
    'st_investment_income.interest_share' = 0,
    'st_investment_income.ordinary_div_share' = 0,
    'st_investment_income.qualified_div_share' = 0,
    'st_investment_income.exemption_amount' = 0,
    'st_investment_income.filing_threshold' = Inf,
    'st_investment_income.age_exemption'  = 0,
    'st_investment_income.blind_exemption' = 0,
    'st_investment_income.rate'           = 0,
    'st_investment_income.full_age_min_age' = Inf,
    'st_investment_income.full_age_income_limit' = -Inf,
    'st_investment_income.age_100_full_exempt' = 0,
    'st_investment_income.blind_full_exempt' = 0,
    'st_investment_income.blind_mfj_exempt_share' = 0,

    # capital_gains.yaml (Washington-style excise tax)
    'st_capital_gains.model_coverage_share' = 0,
    'st_capital_gains.standard_deduction' = Inf,
    'st_capital_gains.charitable_threshold' = Inf,
    'st_capital_gains.charitable_max_deduction' = 0,
    'st_capital_gains.base_rate' = 0,
    'st_capital_gains.surtax_rate' = 0,
    'st_capital_gains.surtax_threshold' = Inf,

    # transfers.yaml (Washington Working Families Tax Credit)
    'st_transfers.wftc_min_age' = Inf,
    'st_transfers.wftc_max_age' = -Inf,
    'st_transfers.wftc_mfs_eligible' = 0,
    'st_transfers.wftc_inv_inc_limit' = -Inf,
    'st_transfers.wftc_phaseout_width1' = 1,
    'st_transfers.wftc_phaseout_width2' = 1,
    'st_transfers.wftc_phaseout_width3' = 1,
    'st_transfers.wftc_phaseout_width4' = 1,
    'st_transfers.wftc_max_amount1' = 0,
    'st_transfers.wftc_max_amount2' = 0,
    'st_transfers.wftc_max_amount3' = 0,
    'st_transfers.wftc_max_amount4' = 0,
    'st_transfers.wftc_min_amount' = 0,
    'st_transfers.wftc_max_income_single1' = 0,
    'st_transfers.wftc_max_income_single2' = 0,
    'st_transfers.wftc_max_income_single3' = 0,
    'st_transfers.wftc_max_income_single4' = 0,
    'st_transfers.wftc_max_income_joint1' = 0,
    'st_transfers.wftc_max_income_joint2' = 0,
    'st_transfers.wftc_max_income_joint3' = 0,
    'st_transfers.wftc_max_income_joint4' = 0
  )
}



st_param_vector_sentinels = function() {

  #----------------------------------------------------------------------------
  # First-element column names that gate vector-family features. Vector params
  # (household credit tables, CTC tiers, CDCTC anchors, stepped recapture,
  # retirement factor tables) are feature-gated on their first element
  # existing and being non-NA; ensure_st_params() adds an NA sentinel when a
  # state's law lacks the family entirely.
  #
  # Returns: character vector of column names (str[]).
  #----------------------------------------------------------------------------

  c('st_credits.hh_agi_bounds_single1', 'st_credits.hh_agi_bounds_other1',
    'st_credits.ctc_tier1_bound', 'st_credits.family_credit_f1_bounds1',
    'st_child_ded.agi_bounds1', 'st_credits.pct_credit_agi_bounds1',
    'st_ord.step_recap_start1', 'st_agi.retire_sub_factor_bounds1')
}



st_param_name_registry = function() {

  #----------------------------------------------------------------------------
  # The complete set of state law parameter names the calculators read,
  # expressed as exact scalar names plus regex patterns for vector families
  # (whose element count varies by state). Consumed by
  # validate_state_param_names() to reject unknown/misspelled YAML parameters
  # at load time (2026-07-17 review items #1/#2). The \d* suffix is optional
  # because a single-element vector parses to a suffix-less column.
  #
  # Returns: list with scalars (str[]) and families (regex str[]).
  #----------------------------------------------------------------------------

  list(
    scalars = c(
      names(st_param_defaults()),

      # Live parameters read outside the calculators
      'st_agi.conformity_group'   # consumed by state_conformity_groups_for_law()
    ),
    families = c(
      # st_tax.R: rate schedule + NY-style recapture, CT-style stepped recapture
      '^st_ord\\.(rates|brackets)\\d*$',
      '^st_ord\\.step_recap_(start|incr|amount|max)\\d*$',

      # st_agi.R: CT-style retirement subtraction factor table
      '^st_agi\\.retire_sub_factor_bounds\\d*$',
      '^st_agi\\.retire_sub_factors\\d*$',

      # st_child_ded.R: NC-style AGI-tabled child deduction
      '^st_child_ded\\.(agi_bounds|amounts)\\d*$',

      # st_credits.R: NY household credit tables
      '^st_credits\\.hh_agi_bounds_(single|other)\\d*$',
      '^st_credits\\.hh_amount_single\\d*$',
      '^st_credits\\.hh_(base|incr)_other\\d*$',

      # st_credits.R: CT-style percentage-of-tax credit table
      '^st_credits\\.pct_credit_(agi_bounds|rates)\\d*$',

      # st_credits.R: NY-style CDCTC share anchors and expense caps
      '^st_credits\\.cdctc_share_(agi_bounds|start|end)\\d*$',
      '^st_credits\\.cdctc_expense_caps\\d*$',

      # st_credits.R: CO-style tiered CTC (tier anchor is mid-name)
      '^st_credits\\.ctc_tier\\d+_bound$',
      '^st_credits\\.ctc_tier_(shares|amounts)\\d*$',

      # st_credits.R: KY-style family-size credit tables
      '^st_credits\\.family_credit_f\\d+_(bounds|rates)\\d*$',

      # st_credits.R: CalEITC-style earned credit, child-count-binned params
      paste0('^st_credits\\.earned_credit_(phasein_rate|max|phaseout_start|',
             'phaseout_rate|agi_safe_harbor)\\d*$')
    )
  )
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
