#-------------------------------------------------------------------------------
# Function to calculate annual net-worth (wealth) tax liability, per record.
#
# A wealth tax is structurally "the estate tax minus death": a balance-sheet /
# stock tax that lives in the do_taxes() chain, aggregates with weights, books
# receipts, and feeds distribution — but stripped of mortality, DSUE /
# portability / both-die, the gift / Sec. 2053 / valuation death-time
# adjustments, and the FY death-year+1 receipts lag. It taxes ECONOMIC net worth
# directly (raw Sigma assets - Sigma debts, no valuation discount, no
# measurement file), so unlike calc_estate() it takes no frozen-parameter
# argument.
#
# calc_wealth() reads the MATERIALIZED net_worth column (assembled once in
# run_one_year, src/sim/run.R), NOT the raw value.* columns. That single column
# serves three roles: (1) calculator input here; (2) the +$1 MTR bump target in
# calc_mtrs() (registering mtr_var = net_worth reprices the marginal statutory
# wealth rate without touching value.* / estate); (3) the isolation point the
# conventional-pass avoidance module overwrites with the avoided base (leaving
# value.* — hence estate and capital income — intact).
#
# Like calc_estate(), it is deliberately NOT registered in return_vars:
# calc_mtrs() rebuilds vars_1040 from that registry, and wealth columns must not
# enter do_1040()'s final select. do_taxes() instead drops and rebinds
# WEALTH_OUTPUT_COLS (gated by calc_wealth_flag), keeping the section idempotent
# under MTR recomputes. liab_wealth stays a SEPARATE column (like liab_estate_*),
# never folded into liab_iit.
#-------------------------------------------------------------------------------

# Net-worth column definitions. estate.R is the single source of truth for the
# economic balance sheet (it is sourced before this file — alphabetical order
# within src/calc/functions/tax/); alias here so estate + wealth stay in
# lockstep on what "net worth" means.
WEALTH_ASSET_COLS = ESTATE_ASSET_COLS
WEALTH_DEBT_COLS  = ESTATE_DEBT_COLS

# Marketable (publicly traded / liquid) vs closely-held (private business and
# other nonfinancial) asset partition, used by the avoidance module's dual
# elasticities (the standalone Wealth-Tax-Simulator's public_e / private_e
# split, sim.R::do_avoidance). The retirement class maps to BOTH Tax-Data
# columns value.dc + value.db. Together these two vectors tile WEALTH_ASSET_COLS.
WEALTH_MARKETABLE_COLS = c(
  'value.cash', 'value.equities', 'value.bonds', 'value.dc', 'value.db',
  'value.life_ins', 'value.annuities', 'value.trusts', 'value.other_fin'
)
WEALTH_CLOSELY_HELD_COLS = c(
  'value.pass_throughs', 'value.primary_home', 'value.other_home',
  'value.re_fund', 'value.other_nonfin'
)

# Columns produced by calc_wealth(). do_taxes() drops these before rebinding
# (MTR-loop frames already carry them from the prior pass)
WEALTH_OUTPUT_COLS = c('liab_wealth')


calc_wealth = function(tax_unit, fill_missings = FALSE) {

  #----------------------------------------------------------------------------
  # Calculates per-record annual wealth tax liability under the scenario's
  # wealth law (wealth.* columns, joined via the standard tax law join).
  #
  # Per-record pipeline (no death state, no valuation bridge — a wealth tax is
  # linear through integrate_rates_brackets, so there is exactly ONE calc per
  # record and no separate exemption-kink rationale, unlike estate's two-branch
  # unified-credit calc):
  #   wealth_base = max(net_worth - exemption, 0)
  #   liab_wealth = graduated schedule applied to wealth_base
  # The graduated schedule's bracket lower bounds are measured on the
  # POST-exemption base (brackets start at 0); the exemption is a separate
  # indexed subparameter (wealth.yaml), not the zero-rated bottom bracket.
  #
  # Parameters:
  #   - tax_unit (df | list) : tax unit(s) with required variables (below)
  #   - fill_missings (bool) : whether to populate unsupplied variables with
  #                            0s (used in testing, not in simulation)
  #
  # Returns: dataframe with one variable:
  #   - liab_wealth (dbl) : annual wealth tax liability (0 under baseline law,
  #                         where exemption = Inf so the base is 0)
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes (net_worth is the materialized economic-net-worth
    # column from run_one_year, NOT the raw value.* columns)
    'filing_status',
    'net_worth',

    # Tax law attributes
    'wealth.exemption',    # (dbl)   exemption amount (filing-status keyed; Inf
                           #         under baseline law => no wealth tax)
    'wealth.brackets[]',   # (dbl[]) graduated schedule bracket lower bounds,
                           #         measured on the post-exemption base
    'wealth.rates[]'       # (dbl[]) graduated schedule rates
  )

  df = tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      # Taxable net worth in excess of the exemption. Under baseline law the
      # exemption is Inf, so wealth_base is 0 for every record and liab_wealth
      # is exactly 0 (the dormancy guarantee)
      wealth_base = pmax(net_worth - wealth.exemption, 0)
    )

  # Graduated wealth schedule (vectorized; reads however many
  # wealth.brackets*/wealth.rates* elements the scenario supplies)
  integrate_rates_brackets(
    df              = df,
    n_brackets      = NULL,
    prefix_brackets = 'wealth.brackets',
    prefix_rates    = 'wealth.rates',
    y               = 'wealth_base',
    output_name     = 'liab_wealth',
    by_bracket      = FALSE
  ) %>%
    select(liab_wealth) %>%
    return()
}
