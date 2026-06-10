#-------------------------------------------------------------------------------
# Function to calculate estate tax liability (expected-value, per record)
#
# Locked-spec reduced form (other/estate_tax/new_estate_modeling_thoughts.md §2,
# §10c-h), ported from other/estate_tax/estate_module.R. Pure and weight-free:
# liability is computed CONDITIONAL on the record's death event this year;
# mortality enters only as a weight at aggregation (see src/sim/estate.R).
#
# Unlike the 1040 calc functions, calc_estate() is NOT part of the do_taxes()
# chain (estate tax does not interact with income tax) and is not registered
# in return_vars. It is called once per year in run_one_year(), outside the
# MTR loop, and takes the frozen measurement parameters as an argument so that
# reforms structurally cannot override them.
#-------------------------------------------------------------------------------

# Tax-Data wealth and debt columns (estate_module.R constants). Gross estate
# is the sum of asset values; debts are subtracted explicitly (the SOI-derived
# f_ded covers only non-debt deductions).
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


calc_estate = function(tax_unit, estate_params, fill_missings = FALSE) {

  #----------------------------------------------------------------------------
  # Calculates per-record estate tax liability conditional on death this year,
  # under the scenario's estate law (estate.* columns, joined via the standard
  # tax law join) and the frozen measurement bridge (estate_params).
  #
  # Per-record pipeline:
  #   reported = economic_gross * r * [1 + (rho_pt - 1) * s_pt]   [valuation]
  #   taxable  = max(reported - debts - f_ded(bin) * reported, 0) [deductions]
  #   base     = taxable + gamma * reported                       [gift add-back]
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
    'estate.exemption',   # (dbl)   basic exclusion amount
    'estate.brackets[]',  # (int[]) tentative tax schedule bracket lower bounds
    'estate.rates[]',     # (dbl[]) tentative tax schedule rates
    'estate.portability'  # (int)   whether DSUE portability is in effect
  )

  bins = estate_params$bins

  df = tax_unit %>%

    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>%

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

      # Taxable estate and unified base with the lifetime-gift add-back
      estate_distributable = pmax(reported_gross - estate_debts - f_ded * reported_gross, 0),
      estate_base          = estate_distributable + estate_params$gamma * reported_gross,

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
