#-------------------------------------------------------------------------------
# corp/paths.R
#
# Contains functions to work out who bears a corporate tax change, and by how
# much, in each year
#-------------------------------------------------------------------------------

# A corporate tax increase reaches households five ways. Dividends, interest, rent
# and pass-through income are cut. The value of corporate equity is marked down.
# Capital gains change, both because fewer shares are sold and because each sells
# for less. Households save less, which the wealth model then carries. And the
# individual income tax on all of that falls, which shows up on its own in the
# conventional revenue estimate.
#
# The paths here are all analytic: deterministic functions of the corporate
# revenue change, the macro projections, and the parameters in the economy leg's
# corp.yaml. Nothing is saved between passes, so any worker can recompute them.
#
# Conventional runs only. The static pass keeps the reform's law and nothing else,
# and the distribution tables are unaffected.
#
# Notes: other/corporate_incidence/



#-------------------------------------------------------------------------------
# Constants
#
# The economic parameters live in the economy leg's corp.yaml, where they can be
# overridden per scenario. What is left here is numerical.
#-------------------------------------------------------------------------------

CORP_SPEC_VERSION = 1L

# Which flows count as income from outside the tax system, and so enter the
# wealth model's forcing through corp_dY_exog:
#
#   in   dividends, interest on the debt rollover ramp, rent net of losses, and
#        the pass-through lines at their capital weight
#   out  realized gains, retirement distributions, and the proceeds of selling any
#        marked-down asset
#
# The ones left out are not income at all: they move resources the household
# already holds. Their tax consequence still arrives, through the change in tax.
# Adding them to the income side as well would count the markdown twice.
#
# The pass-through column list and weight are read at runtime from
# src/sim/wealth_dynamics.R rather than here, because files are sourced
# alphabetically.
CORP_FLOWS_DIV  = c('div_ord', 'div_pref')
CORP_FLOWS_INT  = c('txbl_int', 'exempt_int')
CORP_FLOWS_RENT = c('rent', 'rent_loss')
CORP_FLOWS_INTERNAL = c('kg_st', 'kg_lt', 'kg_1250', 'kg_collect',
                        'kg_lt_basis', 'txbl_ira_dist', 'txbl_pens_dist',
                        'gross_pens_dist')

# How exposed each asset column is to corporate equity, which is what decides how
# much of the markdown it takes. Columns not listed take none, including every
# pass-through asset: leaving those alone keeps the estate valuation bridge frozen.
# Defined benefit balances are never marked down, since a shortfall there falls on
# the plan sponsor and joins the unallocated remainder.
#
# Equities are fully exposed by construction. The others are imputed from the SCF
# and ICI equity shares and are placeholders pending outside measurement. Values in
# the economy leg's corp.yaml.
corp_asset_exposure = function() {
  c('value.equities' = economy_param('corp', 'asset_exposure_equities'),
    'value.dc'       = economy_param('corp', 'asset_exposure_dc'),
    'value.trusts'   = economy_param('corp', 'asset_exposure_trusts'),
    'value.re_fund'  = economy_param('corp', 'asset_exposure_re_fund'))
}

# The corporate share of dividends is corp.omega_div.

# The corporate equity share of realized long-term gains, as against sales of
# pass-through interests, real estate and everything else, is corp.omega_kg. A
# placeholder of about a half, pending SOI measurement.

# The share of the corporate tax change falling on normal rather than
# above-normal returns is corp.sigma_n. Taxes on normal returns get shifted; taxes
# on rents get capitalized into asset prices. The shipped 0.375 follows OTA at 63%
# and TPC at 60% supernormal.

# The corporate share of the economy's normal capital stock is corp.kappa. What
# shifts away from normal returns is split by it: the corporate share stays on
# corporate flows and the rest lands on noncorporate ones. A placeholder of 0.40
# pending the Flow of Funds pull. Whether owner-occupied housing counts moves it
# between roughly a quarter and a half.

# corp.theta scales the cut to flows, absorbing the difference between economic
# profit as the national accounts measure it and profit that is taxable in the US.
# The placeholder of 1 makes every distribution fall in proportion to the hit to
# aggregate after-tax profit, pending measurement.

# corp.theta_res is the share of the tax change borne by foreign owners,
# nonprofits and defined benefit plans. It is used only by the reconciliation
# diagnostic, as the honest remainder: nothing grosses the household side up to
# match the revenue line. A placeholder of 0.40, following Rosenthal and Austin's
# 26% foreign plus nonprofits, insurers and the defined benefit slice.

# Capital shifts between uses only as it is replaced, so the speed of that is the
# economic depreciation rate, corp.delta_nipa. The same 0.057 that
# do_capital_adjustment() uses in src/data/economy.R.

# Equity is discounted at the 10-year Treasury rate in the enactment year plus
# corp.equity_premium. Nominal, because the distributions being discounted are
# nominal. The markdown does not depend on this rate for a permanent change; it
# matters only for temporary ones and for the present value of capital shifting.

# Paths run this many years past the simulation, with a growing perpetuity beyond
# that.
CORP_PV_TAIL_YEARS = 80L

# Reject a revenue path that runs materially both ways, which is the signature of
# a depreciation provision rather than a rate change. The absolute floor keeps
# rounding wobble from tripping it.
CORP_SEESAW_RETRACE_MAX = 0.10
CORP_SEESAW_ABS_FLOOR   = 0.5

# Bound on the markdown. Going from a 21% to a 28% rate implies about 9%, so
# anything near 50% means the inputs are mis-scaled rather than the policy large.
CORP_MU_MAX = 0.5

CORP_EPS = 1e-9



#-------------------------------------------------------------------------------
# Reading the inputs, and deciding whether the channel runs at all
#-------------------------------------------------------------------------------

# Cache of decisions and resolved paths, by scenario. Each SLURM worker is a
# separate process and builds its own; everything here is a cheap function of
# files on disk.
.corp_cache = new.env(parent = emptyenv())


corp_ome_roots = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Locates the off-model estimates for both legs: this scenario's own, and the
  # baseline's.
  #
  # Returns: list of the two paths.
  #----------------------------------------------------------------------------

  scen = scenario_info$interface_paths$`Off-Model-Estimates`
  base = interface_root('Off-Model-Estimates', 'baseline')
  if (length(base) == 0) {
    base = interface_root('Off-Model-Estimates')   # first-row default
  }
  if (length(base) == 0 || is.null(scen)) {
    stop('corp_incidence: cannot resolve Off-Model-Estimates interface paths ',
         'for scenario "', scenario_info$ID, '".')
  }
  list(scenario = scen, baseline = base)
}



corp_read_ome_wedge = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reads the off-model corporate revenue change, scenario less baseline, in
  # billions by calendar year. Reads the whole horizon of the files rather than
  # just the simulation years, since the eligibility guard and the enactment date
  # need the full declared path.
  #
  # Since the statutory rate moved on-model, what is left here is everything else:
  # international provisions, credits and base-broadeners. An off-model vintage
  # must be regenerated to exclude the rate, or the rate is counted twice. A
  # pure-rate scenario reads zeros, so nothing is double-counted in the meantime.
  #
  # Returns: tibble of year and the revenue change.
  #----------------------------------------------------------------------------

  roots = corp_ome_roots(scenario_info)

  read_leg = function(root, nm) {
    f = file.path(root, 'revenues.csv')
    if (!file.exists(f)) {
      stop('corp_incidence: Off-Model-Estimates revenues.csv missing: ', f)
    }
    read_csv(f, show_col_types = FALSE) %>%
      transmute(year, !!nm := corporate)
  }

  read_leg(roots$scenario, 'reform') %>%
    full_join(read_leg(roots$baseline, 'baseline'), by = 'year') %>%
    arrange(year) %>%
    transmute(year,
              w = replace_na(reform, 0) - replace_na(baseline, 0))
}



corp_rate_incidence_wedge = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Computes the revenue change from a statutory rate change on-model, in billions
  # over the whole macro horizon. This replaces the rate portion of the off-model
  # estimate, and everything downstream consumes it the same way.
  #
  # The rate comes from this scenario's tax law, over the simulation years, and is
  # held at its last in-window value thereafter. Years before the policy carry no
  # change.
  #
  # Returns: tibble of year and the revenue change, or NULL where the rate does
  #          not change or the tax law files are unavailable.
  #----------------------------------------------------------------------------

  rate_series = corp_rate_read_series(
    file.path(globals$output_root, scenario_info$ID,
              'static/supplemental/tax_law.csv'))
  if (is.null(rate_series)) return(NULL)

  rev_corp = read_macro_spliced(scenario_info$interface_paths$`Macro-Projections`) %>%
    distinct(year, .keep_all = TRUE) %>%
    arrange(year) %>%
    transmute(year, rev_corp)

  # Hold the rate at its last value past the simulation window. Years before the
  # policy stay empty, which reads as no change.
  full_rate = rev_corp %>%
    select(year) %>%
    left_join(rate_series, by = 'year') %>%
    arrange(year) %>%
    fill(t0, t, .direction = 'down')

  w = corp_rate_delta(full_rate, rev_corp, static = FALSE) %>%
    rename(w = delta)

  if (!any(abs(w$w) > CORP_EPS)) return(NULL)
  w
}



corp_read_wedge = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Sums the on-model rate change and the off-model residual into the total
  # corporate revenue change the channel works from.
  #
  # Returns: tibble of year and the revenue change.
  #----------------------------------------------------------------------------

  ome  = corp_read_ome_wedge(scenario_info)
  rate = corp_rate_incidence_wedge(scenario_info)

  if (is.null(rate)) return(ome)

  ome %>%
    rename(w_ome = w) %>%
    full_join(rate %>% rename(w_rate = w), by = 'year') %>%
    arrange(year) %>%
    transmute(year, w = replace_na(w_ome, 0) + replace_na(w_rate, 0))
}



corp_meta_path = function(ome_root) {
  file.path(ome_root, 'corporate_meta.yaml')
}


corp_read_meta = function(ome_root) {

  #----------------------------------------------------------------------------
  # Reads the declaration the channel requires before it will run, which sits next
  # to the off-model revenue file. An absent file returns NULL and the caller turns
  # the channel off with a warning. A file that is present but wrong stops the run:
  # a false declaration is an input error, not a way of opting out.
  #
  # Three fields are required. gross_of_offset must be true, meaning the revenue
  # figure does not already net out the individual tax the change induces; a figure
  # that does would count that offset twice. provision_type must name a rate
  # change, since only then does the revenue path stand in for the path of
  # after-tax profit. beyond_horizon says whether the change persists past the end
  # of the file, which the present value needs.
  #
  # A rate change per year may also be given, which is cross-checked against the
  # revenue figure at warning level, along with free-text provenance.
  #
  # Returns: the validated declaration, or NULL where the file is absent.
  #----------------------------------------------------------------------------

  path = corp_meta_path(ome_root)
  if (!file.exists(path)) return(NULL)

  meta = read_yaml(path)

  problems = character(0)
  if (!isTRUE(meta$gross_of_offset)) {
    problems = c(problems, paste0(
      'gross_of_offset must be true. A JCT-benchmarked (offset-embedded, ',
      'Nunns) corporate line is NOT gross-of-offset; booking the on-model ',
      'endogenous offset on top of it double-counts (combined revenue ',
      'understated, bounded by the embedded offset -- D1 / section 8.4). ',
      'Re-derive or gross up the input, or remove corporate_meta.yaml to ',
      'run status quo.'))
  }
  if (!identical(as.character(meta$provision_type %||% ''), 'rate')) {
    problems = c(problems, paste0(
      "provision_type must be 'rate' (got '", meta$provision_type %||% '<missing>',
      "'). Only rate/rent-heavy provisions satisfy the D13 eligibility gate ",
      '(receipts path == after-tax profit path proxy). Depreciation-type ',
      'provisions are PERMANENTLY ineligible (old-capital revaluation is ',
      'sign-flipped, P13\'); keep them on their own interface.'))
  }
  bh = as.character(meta$beyond_horizon %||% '')
  if (!bh %in% c('extend', 'zero')) {
    problems = c(problems, paste0(
      "beyond_horizon must be 'extend' or 'zero' (got '",
      if (nzchar(bh)) bh else '<missing>',
      "'). The perfect-foresight markdown needs the wedge's permanence past ",
      'the file horizon.'))
  }

  if (length(problems) > 0) {
    stop('corp_incidence: INVALID corporate_meta.yaml at ', path, ':\n  - ',
         paste(problems, collapse = '\n  - '),
         '\nA present-but-invalid declaration is a hard input error (fail-',
         'closed contract, CONSIDERATIONS section 8.14).')
  }

  meta$beyond_horizon = bh
  meta
}



corp_seesaw_check = function(w_path) {

  #----------------------------------------------------------------------------
  # Rejects a revenue path that runs materially in both directions. That pattern
  # means a timing provision rather than a rate change: bonus depreciation, for
  # instance, costs $61B in 2026 and then raises $35B by 2030. Such a provision
  # revalues existing capital in the opposite direction to a rate change, so the
  # machinery here would get the sign wrong.
  #
  # A rate change on a fixed base moves receipts one way, give or take rounding, so
  # the absolute floor keeps small wobble from tripping this.
  #
  # Returns: list of whether it passed, how much offsetting mass there was, and a
  #          message.
  #----------------------------------------------------------------------------

  pos = sum(pmax(w_path, 0), na.rm = TRUE)
  neg = sum(pmax(-w_path, 0), na.rm = TRUE)
  small = min(pos, neg)
  big   = max(pos, neg)

  retrace = if (big > 0) small / big else 0
  ok = !(small > CORP_SEESAW_ABS_FLOOR && retrace > CORP_SEESAW_RETRACE_MAX)

  msg = if (ok) NA_character_ else sprintf(
    paste0('corporate wedge path runs both ways: $%.1fB one direction vs ',
           '$%.1fB the other (retrace %.0f%%, guard %.0f%%). This is the ',
           'depreciation/timing-provision receipts signature, which is NOT a ',
           'valid after-tax profit path (gate (G), D13) -- its old-capital ',
           'revaluation is sign-flipped (P13\'). The rate-type declaration in ',
           'corporate_meta.yaml contradicts the data.'),
    big, small, 100 * retrace, 100 * CORP_SEESAW_RETRACE_MAX)

  list(ok = ok, retrace = retrace, msg = msg)
}



scenario_uses_corp_incidence = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Decides whether the channel runs. There is no switch to set: it turns on when a
  # non-baseline scenario has a corporate revenue change somewhere, carries a valid
  # declaration, and passes the eligibility guard.
  #
  # A revenue change with no declaration leaves the channel off, with one warning
  # naming the reason. The scenario then falls back to the off-model revenue line
  # and the distributional smear, with no on-model offset. A declaration that is
  # present but contradicted stops the run.
  #
  # Cached per scenario. To compare with and without, point the scenario at a
  # different off-model vintage.
  #
  # Returns: TRUE if the channel runs for this scenario (bool).
  #----------------------------------------------------------------------------

  if (scenario_info$ID == 'baseline') return(FALSE)

  key = paste0('gate|', scenario_info$ID)
  hit = .corp_cache[[key]]
  if (!is.null(hit)) return(hit$active)

  wedge  = corp_read_wedge(scenario_info)
  has_w  = any(abs(wedge$w) > CORP_EPS)
  roots  = corp_ome_roots(scenario_info)

  # A nonzero on-model statutory-rate change SELF-DECLARES the eligibility
  # contract: it is a rate provision by construction (provision_type 'rate'),
  # gross of the endogenous individual offset (a pure corporate receipts number),
  # and permanent past the horizon (beyond_horizon 'extend'). No
  # corporate_meta.yaml is required for it -- the file governs only a residual
  # OME corporate wedge. When only an OME residual is present (no rate change),
  # the original OME meta contract applies unchanged.
  rate_wedge  = corp_rate_incidence_wedge(scenario_info)
  rate_active = !is.null(rate_wedge)
  if (rate_active) {
    meta = list(gross_of_offset = TRUE, provision_type = 'rate',
                beyond_horizon = 'extend', produced_by = 'on-model corp_rate')
  } else {
    meta = corp_read_meta(roots$scenario)   # hard-stops if present-but-invalid
  }

  active = FALSE
  if (!has_w) {
    # No corporate delta: dormant regardless of metadata (delta-only doctrine).
    active = FALSE
  } else if (is.null(meta)) {
    warning(paste0(
      'corp_incidence: scenario "', scenario_info$ID, '" has a nonzero ',
      'corporate receipts delta but its Off-Model-Estimates vintage (',
      roots$scenario, ') carries no corporate_meta.yaml declaration. The ',
      'on-model corporate channel stays OFF (fail-closed, CONSIDERATIONS ',
      'section 8.14): status quo off-model corporate receipts line + ',
      'distribution smear, and NO on-model individual offset / estate / ',
      'wealth interaction. If this input is a gross-of-offset RATE provision, ',
      'add corporate_meta.yaml next to revenues.csv to activate.'),
      call. = FALSE)
    active = FALSE
  } else {
    seesaw = corp_seesaw_check(wedge$w)
    if (!seesaw$ok) {
      stop('corp_incidence: scenario "', scenario_info$ID, '": ', seesaw$msg)
    }

    # Enactment must not predate the sim window: the enactment-year markdown
    # (the announcement capital loss, D7) would be silently missed.
    t0 = min(wedge$year[abs(wedge$w) > CORP_EPS])
    if (t0 < min(scenario_info$years)) {
      stop('corp_incidence: scenario "', scenario_info$ID, '": the corporate ',
           'wedge begins in ', t0, ' but the simulation starts in ',
           min(scenario_info$years), '. The enactment-year equity markdown ',
           'would be missed. Start `years` at or before ', t0,
           ' (house convention: one year before the policy).')
    }

    if (t0 > max(scenario_info$years)) {
      message('corp_incidence: scenario "', scenario_info$ID, '": corporate ',
              'wedge begins in ', t0, ', after the sim window ends (',
              max(scenario_info$years), '); channel dormant this run.')
      active = FALSE
    } else {
      active = TRUE
    }
  }

  .corp_cache[[key]] = list(active = active, wedge = wedge, meta = meta)
  active
}



corp_check_run_compat = function(scenario_info, vat_price_offset) {

  #----------------------------------------------------------------------------
  # Checks the preconditions for an active channel. The paths are built from raw
  # dollar national aggregates and land on raw dollar flows and stocks, and the
  # reconciliation diagnostic assumes the full population, so the channel takes the
  # shared guard for raw-dollar channels and nothing further.
  #
  # Returns: invisibly TRUE; stops on violation.
  #----------------------------------------------------------------------------

  check_raw_data_channel_compat('corp_incidence', scenario_info,
                                vat_price_offset)
}



#-------------------------------------------------------------------------------
# Reading the macro aggregates
#-------------------------------------------------------------------------------

corp_read_macro = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reads the macro series the paths need, splicing historical and projected
  # values. After-tax corporate profit is pre-tax profit less corporate receipts.
  # The Treasury rate gives the discount rate, and the interest, rent and
  # proprietors' income aggregates give the bases for the noncorporate lines.
  #
  # Pre-tax profit exists only in the projections, which is fine, since years
  # before enactment never need it.
  #
  # Note that after-tax profit is returned as pi_at rather than pi. A missing
  # column named pi would silently resolve to the constant instead of erroring.
  #
  # Returns: tibble of the series by year.
  #----------------------------------------------------------------------------

  macro_root = scenario_info$interface_paths$`Macro-Projections`
  if (is.null(macro_root)) {
    stop('corp_incidence: scenario_info$interface_paths$`Macro-Projections` ',
         'is NULL; cannot form the after-tax profit path.')
  }

  raw = read_macro_spliced(macro_root) %>%
    distinct(year, .keep_all = TRUE) %>%
    arrange(year)
  need = c('rev_corp', 'tsy_10y', 'gdp_interest', 'gdp_rent', 'gdp_proprietors')
  missing = setdiff(need, names(raw))
  if (length(missing) > 0 || !('gdp_corp' %in% names(raw))) {
    stop('corp_incidence: Macro-Projections vintage at ', macro_root,
         ' lacks required column(s): ',
         paste(c(missing, setdiff('gdp_corp', names(raw))), collapse = ', '),
         '. gdp_corp (NIPA pre-tax corporate profits) ships in projections.csv',
         " of current vintages; check the economy leg's interfaces.yaml.")
  }

  raw %>%
    transmute(year,
              pi_at = gdp_corp - rev_corp,
              tsy_10y,
              gdp_interest, gdp_rent, gdp_proprietors)
}



corp_rollover_ramp = function() {

  #----------------------------------------------------------------------------
  # Builds the share of debt that has rolled over by a given number of years after
  # enactment, from the maturity schedule do_capital_adjustment() also uses. Debt is
  # roughly fully rolled by year ten.
  #
  # Returns: function of years since enactment, giving the cumulative share.
  #----------------------------------------------------------------------------

  sched = read_csv('./resources/debt_maturities.csv', show_col_types = FALSE) %>%
    arrange(tenor) %>%
    mutate(cum = pmin(cumsum(share), 1))

  function(t_since) {
    out = rep(0, length(t_since))
    pos = t_since >= 1
    idx = pmin(t_since[pos], max(sched$tenor))
    out[pos] = sched$cum[match(idx, sched$tenor)]
    # Past the end of the schedule, and for any missing maturity, treat debt as
    # fully rolled. Exactly 1, which also avoids the running sum's rounding dust.
    out[pos][is.na(out[pos]) | t_since[pos] > max(sched$tenor)] = 1
    out
  }
}



#-------------------------------------------------------------------------------
# Building the paths
#-------------------------------------------------------------------------------

corp_env_knobs = function() {

  #----------------------------------------------------------------------------
  # Reads the three parameters a sensitivity run is most likely to move. They live
  # in the economy leg, so a scenario overrides them in an alternative folder and
  # the choice is recorded in the vintage.
  #
  # Returns: list of the normal-return share, the corporate capital share, and
  #          whether markets price the change as permanent.
  #----------------------------------------------------------------------------

  read_num = function(name, lo, hi) {
    x = suppressWarnings(as.numeric(economy_param('corp', name)))
    if (!is.finite(x) || x < lo || x > hi) {
      stop('corp_incidence: assumption corp.', name, ' = "', x,
           '" is not a number in [', lo, ', ', hi, '].')
    }
    x
  }

  list(
    sigma_n = read_num('sigma_n', 0, 1),
    kappa   = read_num('kappa',   0, 1),
    priced_as_permanent = isTRUE(as.logical(economy_param('corp', 'priced_as_permanent')))
  )
}



corp_build_paths_core = function(wedge, macro, sim_years, beyond_horizon,
                                 sigma_n, kappa, theta = economy_param('corp', 'theta'),
                                 omega_div = economy_param('corp', 'omega_div'),
                                 delta_nipa = economy_param('corp', 'delta_nipa'),
                                 erp = economy_param('corp', 'equity_premium'),
                                 priced_as_permanent = FALSE,
                                 roll_fn = NULL,
                                 pt_weight = NULL) {

  #----------------------------------------------------------------------------
  # Turns the corporate revenue change into the year-by-year paths the record step
  # needs. Pure in its inputs, so the self-checks can drive it with made-up series.
  #
  # The steps, for each year after enactment:
  #
  #   1. Split the revenue change into the part falling on above-normal returns
  #      and the part falling on normal returns.
  #   2. Work out how much capital has turned over since enactment. Only replaced
  #      capital can shift between corporate and noncorporate use, so this sets how
  #      much of the normal-return burden has migrated.
  #   3. Add up what still lands on corporate flows, and divide by after-tax profit
  #      to get the proportional cut to distributions.
  #   4. Take the part of the burden that is capitalized into share prices, and
  #      discount it forward to get the markdown. Capital that has migrated away is
  #      excluded: it is no longer priced in corporate equity.
  #   5. Allocate what migrated across interest, rent and pass-through income in
  #      proportion to their national totals. Interest delivers only the rolled-over
  #      share as a cut to flows, since existing contracts are fixed; the rest shows
  #      up in the reconciliation as a revaluation.
  #
  # Beyond the end of the revenue file, the change either persists, growing with
  # profit, or drops to zero, according to the declaration. Profit itself grows at
  # its trailing five-year rate. Past the end of the grid a growing perpetuity
  # closes the present values.
  #
  # Parameters:
  #   - wedge (df)          : the corporate revenue change by year, in billions
  #   - macro (df)          : the macro series
  #   - sim_years (int[])   : simulation years
  #   - beyond_horizon (str): 'extend' or 'zero'
  #   - roll_fn (function)  : cumulative debt rollover by years since enactment
  #   - pt_weight (dbl)     : capital weight on pass-through income
  #
  # Returns: list of the full-grid paths, the simulation-year slice, the discount
  #          rate, the enactment year, the tail growth rate, and the parameters
  #          used.
  #----------------------------------------------------------------------------

  if (is.null(pt_weight)) pt_weight = wealth_cap_flows_pt_weight()
  if (is.null(roll_fn))   roll_fn   = corp_rollover_ramp()

  w_by_year = setNames(wedge$w, as.character(wedge$year))
  t0 = min(wedge$year[abs(wedge$w) > CORP_EPS])
  if (!is.finite(t0)) stop('corp_incidence: wedge path is identically zero.')

  grid = min(sim_years):(max(sim_years) + CORP_PV_TAIL_YEARS)

  # Profit over the grid, extended at its trailing growth rate
  pi_known = macro %>% filter(is.finite(pi_at))
  if (nrow(pi_known) < 6) {
    stop('corp_incidence: fewer than 6 years of pi = gdp_corp - rev_corp in ',
         'Macro-Projections; cannot form the profit path.')
  }
  g_tail = pi_known %>%
    slice_tail(n = 6) %>%
    summarise(g = (pi_at[n()] / pi_at[1])^(1 / (n() - 1)) - 1) %>%
    pull(g)

  pi_grid = pi_known$pi_at[match(grid, pi_known$year)]
  last_known = max(pi_known$year)
  beyond = which(grid > last_known)
  if (length(beyond) > 0) {
    pi_last = pi_known$pi_at[pi_known$year == last_known]
    pi_grid[beyond] = pi_last * (1 + g_tail)^(grid[beyond] - last_known)
  }

  # The revenue change over the grid
  w_grid = w_by_year[as.character(grid)]
  file_last = max(wedge$year)
  # A year inside the file's range but absent from it carries no change.
  w_grid[is.na(w_grid) & grid <= file_last] = 0
  if (beyond_horizon == 'zero') {
    w_grid[is.na(w_grid)] = 0
  } else {
    # Continue the last value in the file, growing with profit.
    w_last = w_by_year[as.character(file_last)]
    ext = which(is.na(w_grid))
    if (length(ext) > 0) {
      pi_at_last = pi_grid[match(file_last, grid)]
      if (is.na(pi_at_last)) {
        stop('corp_incidence: pi undefined at the OME file horizon (',
             file_last, '); cannot extend the wedge.')
      }
      w_grid[ext] = w_last * pi_grid[ext] / pi_at_last
    }
  }
  w_grid = unname(w_grid)

  # Profit has to exist wherever the revenue change is nonzero.
  need_pi = abs(w_grid) > CORP_EPS
  if (any(need_pi & !is.finite(pi_grid))) {
    bad = grid[need_pi & !is.finite(pi_grid)][1]
    stop('corp_incidence: pi = gdp_corp - rev_corp unavailable for year ', bad,
         ' but the corporate wedge is nonzero there. gdp_corp exists only in ',
         'Macro-Projections projections.csv; check the vintage.')
  }

  # Capital turnover since enactment, and the split of the revenue change
  t_since = pmax(grid - t0, 0)
  eta  = if_else(grid < t0, 0, 1 - (1 - delta_nipa)^t_since)
  roll = roll_fn(pmax(grid - t0, 0))

  w_rent = (1 - sigma_n) * w_grid
  w_norm = sigma_n * w_grid

  h_c = w_rent + w_norm * ((1 - eta) + eta * kappa)
  phi = if_else(abs(w_grid) > CORP_EPS & is.finite(pi_grid) & pi_grid > 0,
                -theta * h_c / pi_grid, 0)

  # The capitalized part, and the present values, discounted at a rate fixed at
  # enactment
  tsy_t0 = macro$tsy_10y[match(t0, macro$year)]
  if (!is.finite(tsy_t0)) {
    # Fall back to the first simulation year that has a rate. Enactment can only
    # precede the projections in unusual vintages, and those years are inert.
    tsy_t0 = macro$tsy_10y[match(min(sim_years), macro$year)]
  }
  if (!is.finite(tsy_t0)) {
    stop('corp_incidence: tsy_10y unavailable at enactment year ', t0, '.')
  }
  r = tsy_t0 / 100 + erp
  if (r <= g_tail + 0.005) {
    stop(sprintf(paste0('corp_incidence: discount rate r = %.3f is not ',
                        'safely above the tail growth rate g = %.3f; the ',
                        'Gordon terminal PV diverges. Check tsy_10y / ',
                        'corp.equity_premium / the pi series.'),
                 r, g_tail))
  }

  price_w = w_grid
  if (priced_as_permanent) {
    # Markets are assumed not to believe the sunset, so for pricing the change
    # continues from its last nonzero level, growing with profit. The cut to flows
    # still follows the statute.
    live = which(abs(w_grid) > CORP_EPS)
    if (length(live) > 0) {
      last_live = max(live)
      if (last_live < length(grid)) {
        idx = (last_live + 1):length(grid)
        price_w[idx] = w_grid[last_live] * pi_grid[idx] / pi_grid[last_live]
      }
    }
  }
  price_rent = (1 - sigma_n) * price_w
  price_norm = sigma_n * price_w
  price_hit  = price_rent + (1 - kappa) * (1 - eta) * price_norm

  n = length(grid)
  M = numeric(n)
  V = numeric(n)
  # Value of the growing tail past the end of the grid
  M[n] = price_hit[n] * (1 + g_tail) / (r - g_tail)
  V[n] = pi_grid[n]   * (1 + g_tail) / (r - g_tail)
  for (i in (n - 1):1) {
    M[i] = (price_hit[i + 1] + M[i + 1]) / (1 + r)
    V[i] = (pi_grid[i + 1]   + V[i + 1]) / (1 + r)
  }
  # The change is a surprise at enactment, so nothing is priced in beforehand.
  M[grid < t0] = 0

  mu = if_else(V > CORP_EPS, M / V, 0)
  if (any(abs(mu) > CORP_MU_MAX, na.rm = TRUE)) {
    bad = grid[which.max(abs(mu))]
    stop(sprintf(paste0('corp_incidence: |mu| = %.3f at year %d exceeds the ',
                        'sanity bound %.2f. The naive ceiling for a 21->28 ',
                        'hike is ~0.09; a markdown this large means ',
                        'mis-scaled inputs (units? wrong baseline OME leg?), ',
                        'not policy.'),
                 max(abs(mu)), bad, CORP_MU_MAX))
  }

  # Allocating the migrated burden across the noncorporate lines
  base_int  = macro$gdp_interest    [match(grid, macro$year)]
  base_rent = macro$gdp_rent        [match(grid, macro$year)]
  base_pt   = macro$gdp_proprietors [match(grid, macro$year)] * pt_weight

  N = (1 - kappa) * eta * w_norm
  need_bases = abs(N) > CORP_EPS
  if (any(need_bases & (!is.finite(base_int) | !is.finite(base_rent) |
                        !is.finite(base_pt)))) {
    # Only simulation years need these; the present value tail does not.
    in_sim = need_bases & grid %in% sim_years
    if (any(in_sim & (!is.finite(base_int) | !is.finite(base_rent) |
                      !is.finite(base_pt)))) {
      bad = grid[in_sim & (!is.finite(base_int) | !is.finite(base_rent) |
                           !is.finite(base_pt))][1]
      stop('corp_incidence: Macro-Projections gdp_interest/gdp_rent/',
           'gdp_proprietors unavailable for sim year ', bad, '.')
    }
  }
  denom   = base_int + base_rent + base_pt
  sh_int  = if_else(is.finite(denom) & denom > 0, base_int  / denom, 0)
  sh_rent = if_else(is.finite(denom) & denom > 0, base_rent / denom, 0)
  sh_pt   = if_else(is.finite(denom) & denom > 0, base_pt   / denom, 0)

  H_int_pot = N * sh_int
  H_int     = H_int_pot * roll        # delivered as flow (rolled paper only)
  drho_int  = H_int_pot * (1 - roll)  # named delta-rho revaluation line
  H_rent    = N * sh_rent
  H_pt      = N * sh_pt

  # The factors the record step applies
  fac_div  = 1 + omega_div * phi
  g_int    = if_else(is.finite(base_int)  & base_int  > 0, H_int  / base_int,  0)
  g_rent   = if_else(is.finite(base_rent) & base_rent > 0, H_rent / base_rent, 0)
  g_ptcap  = if_else(is.finite(base_pt)   & base_pt   > 0, H_pt   / base_pt,   0)
  fac_int  = 1 - g_int
  fac_rent = 1 - g_rent
  fac_pt   = 1 - pt_weight * g_ptcap

  facs = c(fac_div, fac_int, fac_rent, fac_pt)
  if (any(!is.finite(facs)) || any(facs < 0)) {
    stop('corp_incidence: a record scaling factor is non-finite or negative ',
         '(min = ', min(facs), '). The wedge is implausibly large relative ',
         'to the aggregate income bases; refusing to clamp silently.')
  }

  omega_dc = unname(corp_asset_exposure()['value.dc'])
  by_year = tibble(
    year = grid, w = w_grid, pi_at = pi_grid, eta = eta, roll = roll,
    w_rent = w_rent, w_norm = w_norm, h_c = h_c, phi = phi,
    price_hit = price_hit, M = M, V = V, mu = mu,
    N_noncorp = N, H_int = H_int, drho_int = drho_int,
    H_rent = H_rent, H_pt = H_pt,
    fac_div = fac_div, fac_int = fac_int, fac_rent = fac_rent,
    g_ptcap = g_ptcap, fac_pt = fac_pt,
    mu_ret = omega_dc * mu
  )

  list(by_year = by_year,
       sim     = by_year %>% filter(year %in% sim_years),
       r       = r,
       t0      = t0,
       g_tail  = g_tail,
       knobs   = list(sigma_n = sigma_n, kappa = kappa, theta = theta,
                      omega_div = omega_div, pt_weight = pt_weight,
                      beyond_horizon = beyond_horizon,
                      priced_as_permanent = priced_as_permanent))
}



corp_assert_paths = function(paths) {

  #----------------------------------------------------------------------------
  # Checks two things that must hold on any set of built paths. Runs on every
  # resolve, and is cheap.
  #
  # First, the markdown must be consistent with itself: this year's value, carried
  # forward one year at the discount rate, has to equal next year's price hit plus
  # next year's markdown. That is checked on every interior year, and then again by
  # summing the present value directly from several years, so that a misaligned
  # index in the recursion cannot certify itself.
  #
  # Second, nothing may move before enactment.
  #
  # The behavior of the paths themselves, such as the markdown being constant under
  # a permanent change, is checked on made-up inputs by corp_selfcheck_paths(),
  # where the right answer is known in closed form.
  #
  # Returns: invisibly TRUE; stops on violation.
  #----------------------------------------------------------------------------

  b = paths$by_year
  r = paths$r
  n = nrow(b)

  # The markdown against itself, on interior years
  lhs = b$M[-n] * (1 + r)
  rhs = b$price_hit[-1] + b$M[-1]
  live = b$year[-n] >= paths$t0
  if (!isTRUE(all.equal(lhs[live], rhs[live], tolerance = 1e-8))) {
    stop('corp_incidence: markdown telescoping M_t(1+r) = hit_{t+1} + M_{t+1} ',
         'FAILED (max abs dev ', max(abs(lhs[live] - rhs[live])), '). ',
         'Internal path-construction bug.')
  }
  # And against a directly summed present value, from up to five years
  live_idx = which(b$year >= paths$t0 & b$year < max(b$year))
  if (length(live_idx) > 0) {
    for (i in unique(round(seq(min(live_idx), max(live_idx), length.out = 5)))) {
      s = (i + 1):n
      direct = sum(b$price_hit[s] / (1 + r)^(s - i)) +
               b$M[n] / (1 + r)^(n - i)
      if (!isTRUE(all.equal(direct, b$M[i],
                            tolerance = 1e-8, scale = max(1, abs(direct))))) {
        stop('corp_incidence: markdown backward recursion disagrees with the ',
             'direct PV sum at year ', b$year[i], ' (', b$M[i], ' vs ', direct,
             '). Internal path-construction bug.')
      }
    }
  }

  # Nothing moves before enactment
  pre = b$year < paths$t0
  if (any(pre)) {
    inert = all(abs(b$mu[pre]) < 1e-12) &&
            all(abs(b$fac_div[pre] - 1) < 1e-12) &&
            all(abs(b$fac_int[pre] - 1) < 1e-12) &&
            all(abs(b$fac_rent[pre] - 1) < 1e-12) &&
            all(abs(b$fac_pt[pre] - 1) < 1e-12)
    if (!inert) {
      stop('corp_incidence: pre-enactment years are not inert (mu or a flow ',
           'factor differs from its identity value before ', paths$t0, ').')
    }
  }

  invisible(TRUE)
}



corp_resolve_paths = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reads everything from disk and builds the paths: the revenue change and its
  # declaration, the macro aggregates, and the debt rollover schedule. Then checks
  # them. Deterministic and cheap, so any worker can call it without anything
  # having been saved for it.
  #
  # Returns: the paths, plus the declaration and the revenue change they came from.
  #----------------------------------------------------------------------------

  gate_key = paste0('gate|', scenario_info$ID)
  gate = .corp_cache[[gate_key]]
  if (is.null(gate)) {
    if (!scenario_uses_corp_incidence(scenario_info)) {
      stop('corp_incidence: corp_resolve_paths called for scenario "',
           scenario_info$ID, '" but the channel is not active for it.')
    }
    gate = .corp_cache[[gate_key]]
  }
  if (!isTRUE(gate$active)) {
    stop('corp_incidence: corp_resolve_paths called for scenario "',
         scenario_info$ID, '" but the channel is not active for it.')
  }

  knobs = corp_env_knobs()
  paths = corp_build_paths_core(
    wedge               = gate$wedge,
    macro               = corp_read_macro(scenario_info),
    sim_years           = scenario_info$years,
    beyond_horizon      = gate$meta$beyond_horizon,
    sigma_n             = knobs$sigma_n,
    kappa               = knobs$kappa,
    priced_as_permanent = knobs$priced_as_permanent,
    roll_fn             = corp_rollover_ramp(),
    pt_weight           = wealth_cap_flows_pt_weight()
  )
  corp_assert_paths(paths)

  # Where the declaration gives the legislated rate change, check the revenue
  # figure against the rate times pre-tax profit, at warning level. Recovering
  # pre-tax profit needs the baseline rate, so both must be given.
  if (!is.null(gate$meta$delta_tau) && !is.null(gate$meta$tau_baseline)) {
    dt   = gate$meta$delta_tau
    tau0 = as.numeric(gate$meta$tau_baseline)
    yrs  = intersect(as.integer(names(dt)), paths$sim$year)
    if (length(yrs) > 0 && is.finite(tau0) && tau0 > 0 && tau0 < 1) {
      row = paths$sim[match(yrs, paths$sim$year), ]
      implied = as.numeric(dt[as.character(yrs)]) * row$pi_at / (1 - tau0)
      ratio = ifelse(abs(implied) > CORP_EPS, row$w / implied, NA)
      off = is.finite(ratio) & (ratio < 0.5 | ratio > 2)
      if (any(off)) {
        warning('corp_incidence: wedge vs legislated-rate cross-check is off ',
                'by more than 2x in year(s) ',
                paste(yrs[off], collapse = ', '),
                ' (w / [Delta-tau * Pi] = ',
                paste(sprintf('%.2f', ratio[off]), collapse = ', '),
                '). The receipts path may not be a clean rate-change proxy, ',
                'or theta/US-taxable scaling differs from the declaration.',
                call. = FALSE)
      }
    }
  }

  paths$meta  = gate$meta
  paths$wedge = gate$wedge
  paths
}



corp_get_paths = function(scenario_info) {

  # Caches the resolved paths by scenario. The parameters do not change within a
  # process, so one set of paths serves every year and pass.
  key = paste0('paths|', scenario_info$ID)
  hit = .corp_cache[[key]]
  if (!is.null(hit)) return(hit)
  paths = corp_resolve_paths(scenario_info)
  .corp_cache[[key]] = paths
  paths
}



