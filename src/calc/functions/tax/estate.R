#-------------------------------------------------------------------------------
# Function to calculate estate tax liability (expected-value, per record)
#
# Locked-spec reduced form (other/estate_tax/new_estate_modeling_thoughts.md §2,
# §10c-h), ported from other/estate_tax/estate_module.R. Pure and weight-free:
# liability is computed CONDITIONAL on the record's death event this year;
# mortality enters only as a weight at aggregation (see src/sim/estate.R).
#
# calc_estate() runs inside the do_taxes() chain (the "Estate tax" section
# there) so that each pass -- and any behavioral repricing -- recomputes
# estate liability on its own frame. Unlike the 1040 calc functions it is
# deliberately NOT registered in return_vars: calc_mtrs() rebuilds vars_1040
# from that registry, and estate columns must not enter do_1040()'s final
# select. do_taxes() instead drops and rebinds ESTATE_OUTPUT_COLS, keeping
# the section idempotent under MTR recomputes. The frozen measurement
# parameters arrive as an argument (globals$estate_params) so that reforms
# structurally cannot override them.
#-------------------------------------------------------------------------------

# Tax-Data wealth and debt columns (estate_module.R constants). Gross estate
# is the sum of asset values; debts are subtracted explicitly (the SOI-derived
# f_ded covers only non-debt deductions). These are the single source of truth
# for the economic balance sheet: the wealth tax aliases them as
# WEALTH_ASSET_COLS / WEALTH_DEBT_COLS (src/calc/functions/tax/wealth.R) so
# estate + wealth stay in lockstep on what "net worth" means.
ESTATE_ASSET_COLS = c(
  'value.cash', 'value.equities', 'value.bonds', 'value.dc', 'value.db',
  'value.life_ins', 'value.annuities', 'value.trusts', 'value.other_fin',
  'value.pass_throughs', 'value.primary_home', 'value.other_home',
  'value.re_fund', 'value.other_nonfin'
)

ESTATE_DEBT_COLS = c(
  'value.primary_mortgage', 'value.other_mortgage', 'value.credit_lines',
  'value.credit_cards', 'value.installment_debt', 'value.other_debt'
)

# Columns produced by calc_estate(). do_taxes() drops these before rebinding
# (MTR-loop frames already carry them from the prior pass)
ESTATE_OUTPUT_COLS = c(
  'liab_estate_nodsue', 'liab_estate_dsue', 'estate_p_dsue',
  'estate_distributable'
)


calc_estate = function(tax_unit, estate_params, fill_missings = FALSE) {

  #----------------------------------------------------------------------------
  # Calculates per-record estate tax liability conditional on death this year,
  # under the scenario's estate law (estate.* columns, joined via the standard
  # tax law join) and the frozen measurement bridge (estate_params).
  #
  # Per-record pipeline:
  #   reported = economic_gross * r * [1 + (rho_pt - 1) * s_pt]   [valuation]
  #   taxable  = max(reported - debts - f_ded(bin) * reported, 0) [deductions]
  #   base     = max(taxable - switch * income_tax_ded, 0) [Sec. 2053 ded.,
  #              + gamma * reported              gift add-back; switch = the
  #                                              estate.income_tax_ded lever]
  #   L(base, excl) = max(T(base) - T(excl), 0), T = graduated tentative
  #                   schedule (unified credit as a credit at the exclusion)
  #
  # State structure (latent, blended downstream by probability weights):
  #   joint (filing_status == 2 & q_death2 > 0), both-die event:
  #     both branches = L(base, 2 * exemption); p_dsue = 0
  #   single (everyone else, incl. widows):
  #     no-DSUE branch = L(base, exemption)
  #     DSUE branch    = L(base, exemption + dsue)
  #     p_dsue = p_dsue(bin), or 0 if portability is repealed
  # Two COMPLETE liability calcs per single record because the unified-credit
  # kink is nonlinear: an expected DSUE inside one calc would understate tax
  # for records straddling the kink.
  #
  # Parameters:
  #   - tax_unit (df | list)  : tax unit(s) with required variables (below)
  #   - estate_params (list)  : frozen measurement parameters from
  #                             get_estate_params() (src/sim/estate.R):
  #                             r, rho_pt, gamma, bins (tibble: size_bin, lo,
  #                             hi, f_ded, p_dsue, f_dsue)
  #   - fill_missings (bool)  : whether to populate unsupplied variables with
  #                             0s (used in testing, not in simulation)
  #
  # Optional input columns (absent => 0, all other callers unaffected):
  #   - estate_income_tax_ded (dbl) : decedent's income tax at death,
  #                                   deductible against the taxable estate
  #                                   (Sec. 2053-style). Conditional-on-death
  #                                   dollars, set by run_one_year()'s
  #                                   kg_dynamics deemed-realization dead-leg
  #                                   recompute. Enters the BASE only:
  #                                   estate_distributable stays scenario-
  #                                   invariant (fixed-wealth convention).
  #                                   Gated by the estate.income_tax_ded law
  #                                   switch (baseline default 1)
  #   - estate_concealed_frac (dbl) : share of gross assets concealed from the
  #                                   tax authority under a wealth tax, set by
  #                                   the conventional-pass wealth-avoidance
  #                                   module (src/behavior/wealth/avoidance.R;
  #                                   hidden-ledger ruling R4).
  #                                   Concealed wealth escapes the reported
  #                                   estate: it enters the BASE only (as
  #                                   estate_concealed_frac * reported_gross),
  #                                   so estate_distributable -- the heir
  #                                   allocator's bequest ladder -- stays
  #                                   invariant and heirs inherit the hidden
  #                                   wealth unchanged. Absent on the static
  #                                   pass (no behavior) => 0 => static stays
  #                                   the clean law-only counterfactual
  #   - estate_base_bump (dbl)      : perturbation to the taxable estate BASE,
  #                                   used ONLY by calc_mtrs(var = 'estate') to
  #                                   measure the marginal estate rate as a
  #                                   +$1 right-derivative. It enters INSIDE
  #                                   the estate_base pmax so the bump gives
  #                                   +d(liab)/d(base) directly: positive by
  #                                   construction, un-switched (not gated by
  #                                   estate.income_tax_ded), 0 when the pmax
  #                                   floor binds, 0 below the exemption.
  #                                   The resulting mtr_estate is a
  #                                   CONDITIONAL-ON-DEATH statutory price --
  #                                   the marginal estate rate IF the estate
  #                                   event occurs, DSUE-blended downstream.
  #                                   It deliberately contains NO mortality:
  #                                   mortality lives in the kg Bellman
  #                                   (bm_vec) and the aggregation weights.
  #                                   Do NOT "correct" it by multiplying in
  #                                   estate_m.
  #                                   Legal/timing scope: the deduction this
  #                                   rate prices (via mtr_estate_ded =
  #                                   estate.income_tax_ded x mtr_estate) is
  #                                   the DEATH-TRIGGERED income tax stamped
  #                                   by the kg dead-leg recompute
  #                                   (estate_income_tax_ded) -- the same
  #                                   liability priced by the Bellman's F and
  #                                   the deemed-realization term -- NOT
  #                                   lifetime CG taxes or a broader income-
  #                                   tax deduction
  #
  # Returns: dataframe with the following variables:
  #   - liab_estate_nodsue (dbl)   : liability conditional on death, no-DSUE
  #                                  state (joint records: the both-die calc)
  #   - liab_estate_dsue (dbl)     : liability conditional on death, DSUE
  #                                  state (joint records: same as no-DSUE)
  #   - estate_p_dsue (dbl)        : probability of the DSUE state (0 for
  #                                  joint records and when portability = 0)
  #   - estate_distributable (dbl) : reported gross net of debts and non-debt
  #                                  deductions (heir allocator input)
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'filing_status',    # (int) filing status
    'q_death2',         # (dbl) secondary earner mortality probability (spouse-
                        #       alive indicator here; the rate itself is a
                        #       weight, applied at aggregation)
    ESTATE_ASSET_COLS,  # (dbl) wealth by asset class
    ESTATE_DEBT_COLS,   # (dbl) debts by liability class

    # Tax law attributes
    'estate.exemption',      # (dbl)   basic exclusion amount
    'estate.brackets[]',     # (int[]) tentative tax schedule bracket lower bounds
    'estate.rates[]',        # (dbl[]) tentative tax schedule rates
    'estate.portability',    # (int)   whether DSUE portability is in effect
    'estate.income_tax_ded'  # (int)   whether the decedent's income tax at
                             #         death is deductible against the
                             #         taxable estate
  )

  bins = estate_params$bins

  df = tax_unit %>%

    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings)

  # Default the optional income tax deduction when the caller doesn't supply it
  if (!('estate_income_tax_ded' %in% names(df))) {
    df$estate_income_tax_ded = 0
  }

  # Default the optional concealment fraction (hidden-ledger R4) when the caller
  # doesn't supply it -- e.g. the static pass, which runs no behavior module
  if (!('estate_concealed_frac' %in% names(df))) {
    df$estate_concealed_frac = 0
  }

  # Default the optional base perturbation (estate-MTR measurement input; see
  # parameter doc) when the caller doesn't supply it -- every non-MTR caller
  if (!('estate_base_bump' %in% names(df))) {
    df$estate_base_bump = 0
  }

  df %<>%

    mutate(

      # Economic gross estate, debts, and pass-through share. Wealth is in raw
      # (non-VAT-adjusted) dollars by construction; see run_one_year()
      economic_gross = rowSums(across(all_of(ESTATE_ASSET_COLS), ~ replace_na(., 0))),
      estate_debts   = rowSums(across(all_of(ESTATE_DEBT_COLS),  ~ replace_na(., 0))),
      s_pt           = if_else(economic_gross > 0,
                               replace_na(value.pass_throughs, 0) / economic_gross,
                               0),

      # Valuation bridge: economic wealth -> reported gross estate. rho_pt
      # captures closely-held business valuation discounts (minority interest,
      # marketability) concentrated in pass-through wealth
      reported_gross = economic_gross * estate_params$r *
                       (1 + (estate_params$rho_pt - 1) * s_pt),

      # Per-bin assumed parameters, looked up on reported gross
      bin_idx = findInterval(reported_gross, c(bins$lo, Inf), rightmost.closed = FALSE),
      f_ded   = bins$f_ded[bin_idx],
      p_dsue  = bins$p_dsue[bin_idx],
      f_dsue  = bins$f_dsue[bin_idx],

      # Taxable estate and unified base with the lifetime-gift add-back. The
      # income tax deduction AND the hidden-ledger concealment (R4) enter the
      # BASE only -- the former gated by the estate.income_tax_ded law switch --
      # so estate_distributable (the heir allocator's bequest-mass ladder) stays
      # scenario-invariant under the fixed-wealth convention. Concealment is a
      # fraction of gross assets applied to reported_gross (concealed dollars in
      # reported terms); the gift add-back is deliberately left on the FULL
      # reported_gross (conservative -- hidden gifts are not netted out).
      # estate_base_bump (the +$1 estate-MTR perturbation) sits INSIDE the
      # pmax so the bump is correctly inert when the floor binds; at exact
      # kinks (zero base, exemption boundary, bracket edges) the resulting
      # MTR is the standard next-dollar right-derivative
      estate_distributable = pmax(reported_gross - estate_debts - f_ded * reported_gross, 0),
      estate_base          = pmax(estate_distributable -
                                  estate.income_tax_ded * estate_income_tax_ded -
                                  estate_concealed_frac * reported_gross +
                                  estate_base_bump, 0) +
                             estate_params$gamma * reported_gross,

      # Exclusion amounts for the three liability calculations. Joint records
      # (married filing jointly with a living spouse) are modeled at the
      # both-die event with two unified credits; the DSUE channel applies only
      # to singles (incl. widows already single in the cross-section)
      married    = filing_status == 2 & replace_na(q_death2, 0) > 0,
      excl_joint = 2 * estate.exemption,
      excl_wo    = estate.exemption,
      excl_w     = estate.exemption + f_dsue * reported_gross
    )

  # Tentative tax at the base and at each exclusion (vectorized graduated
  # schedule; reads however many estate.brackets*/estate.rates* elements the
  # scenario supplies)
  tentative = function(y) {
    integrate_rates_brackets(
      df              = df,
      n_brackets      = NULL,
      prefix_brackets = 'estate.brackets',
      prefix_rates    = 'estate.rates',
      y               = y,
      output_name     = 'tentative',
      by_bracket      = FALSE
    )$tentative
  }
  t_base  = tentative('estate_base')
  t_joint = tentative('excl_joint')
  t_wo    = tentative('excl_wo')
  t_w     = tentative('excl_w')

  df %>%
    mutate(

      # Unified credit: tentative tax on the exclusion offsets tentative tax
      # on the base. Equals top-rate * (base - exclusion) whenever both exceed
      # the top bracket threshold, i.e. always at current-law exemptions
      liab_joint = pmax(t_base - t_joint, 0),
      liab_wo    = pmax(t_base - t_wo,    0),
      liab_w     = pmax(t_base - t_w,     0),

      liab_estate_nodsue = if_else(married, liab_joint, liab_wo),
      liab_estate_dsue   = if_else(married, liab_joint, liab_w),
      estate_p_dsue      = if_else(married, 0, p_dsue * (estate.portability == 1))
    ) %>%
    select(liab_estate_nodsue, liab_estate_dsue, estate_p_dsue,
           estate_distributable) %>%
    return()
}
