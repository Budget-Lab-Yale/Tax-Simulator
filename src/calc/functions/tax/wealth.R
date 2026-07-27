#-------------------------------------------------------------------------------
# Function to calculate annual wealth tax liability, per record
#
# A wealth tax is the estate tax without the death: a tax on a stock rather than a
# flow, computed inside do_taxes(), aggregated with weights, booked to receipts and
# fed to the distribution tables. What it does without is mortality, portability
# between spouses, the death-time adjustments for gifts and the decedent's income
# tax, and the lag in booking receipts.
#
# It taxes economic net worth directly, assets less debts, with no valuation
# discount, so unlike the estate tax it needs no measurement parameters.
#
# It reads the materialized net worth column rather than the asset columns. That
# column does three jobs: it is the input here; it is what calc_mtrs() bumps to
# price the marginal wealth rate, which leaves the assets and the estate alone; and
# it is what the avoidance module overwrites with the avoided base, again leaving
# the assets, and so the estate and capital income, intact.
#
# As with the estate tax, this is deliberately not registered in return_vars, and
# liability stays in its own column rather than being folded into the income tax.
#-------------------------------------------------------------------------------

# The balance sheet is defined once, in estate.R, which is sourced first. Aliased
# here so the two taxes agree on what net worth means.
WEALTH_ASSET_COLS = ESTATE_ASSET_COLS
WEALTH_DEBT_COLS  = ESTATE_DEBT_COLS

# Assets split into marketable, meaning publicly traded and liquid, and closely
# held, meaning private business and other nonfinancial wealth. The avoidance module
# applies a different elasticity to each. Retirement wealth covers both the
# defined-contribution and defined-benefit columns. Between them the two lists cover
# every asset.
WEALTH_MARKETABLE_COLS = c(
  'value.cash', 'value.equities', 'value.bonds', 'value.dc', 'value.db',
  'value.life_ins', 'value.annuities', 'value.trusts', 'value.other_fin'
)
WEALTH_CLOSELY_HELD_COLS = c(
  'value.pass_throughs', 'value.primary_home', 'value.other_home',
  'value.re_fund', 'value.other_nonfin'
)

# What calc_wealth() produces. do_taxes() drops these before rebinding, since a
# frame in the marginal rate loop already carries them.
WEALTH_OUTPUT_COLS = c('liab_wealth')


calc_wealth = function(tax_unit, fill_missings = FALSE) {

  #----------------------------------------------------------------------------
  # Calculates each record's annual wealth tax liability under the scenario's wealth
  # law.
  #
  # The graduated schedule applies directly to net worth, with no exemption term of
  # its own and no valuation discount. A bracket rated at zero
  # bottom bracket (wealth.brackets1 = 0, wealth.rates1 = 0) plays the role of
  # the exemption: everything below the first positive-rate threshold is
  # untaxed, so the exemption indexes together with the rest of the schedule
  # (no split between an indexed exemption and frozen-nominal widths). Brackets
  # are in absolute net-worth dollars and filing-status-resolved upstream by the
  # tax law join (wealth.yaml's filing_status_mapper). A negative net worth sits
  # below the bottom bracket, so integrate_rates_brackets returns exactly 0.
  #
  # This is the income-tax representation (std-deduction-as-0%-bracket), not
  # estate's tentative-tax-minus-unified-credit form — a wealth tax has no
  # credit mechanic, so the simpler single-schedule form is exact.
  #
  # Parameters:
  #   - tax_unit (df | list) : tax unit(s) with required variables (below)
  #   - fill_missings (bool) : whether to populate unsupplied variables with
  #                            0s (used in testing, not in simulation)
  #
  # Returns: dataframe with one variable:
  #   - liab_wealth (dbl) : annual wealth tax liability (0 under baseline law,
  #                         where the single bracket is 0%-rated)
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes (net_worth is the materialized economic-net-worth
    # column from run_one_year, NOT the raw value.* columns). filing_status is
    # not read here — the schedule is filing-status-resolved at join time — but
    # is required so the tax law join can apply wealth.yaml's mapper.
    'filing_status',
    'net_worth',

    # Tax law attributes
    'wealth.brackets[]',   # (dbl[]) graduated schedule bracket lower bounds, in
                           #         absolute net-worth dollars (bracket 1 = 0,
                           #         the 0%-rated "exemption" floor)
    'wealth.rates[]'       # (dbl[]) graduated schedule rates (rate 1 = 0)
  )

  df = tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Graduated wealth schedule applied directly to absolute net worth (vectorized;
  # reads however many wealth.brackets*/wealth.rates* elements the scenario
  # supplies). The 0%-rated bottom bracket is the exemption; negative net worth
  # falls below it and yields 0.
  integrate_rates_brackets(
    df              = df,
    n_brackets      = NULL,
    prefix_brackets = 'wealth.brackets',
    prefix_rates    = 'wealth.rates',
    y               = 'net_worth',
    output_name     = 'liab_wealth',
    by_bracket      = FALSE
  ) %>%
    select(liab_wealth) %>%
    return()
}
