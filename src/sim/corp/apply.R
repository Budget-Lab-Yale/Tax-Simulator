#-------------------------------------------------------------------------------
# apply.R
#
# Contains functions to apply the corporate tax change to records
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Applying the shock to records
#-------------------------------------------------------------------------------

corp_apply_to_records = function(tax_units, paths, year,
                                 kg_dynamics_active = FALSE) {

  #----------------------------------------------------------------------------
  # Applies the year's corporate tax change to records. Runs at the head of a
  # conventional pass, before the wealth haircut and the behavior modules. The
  # static pass never sees it.
  #
  # Income flows are cut: dividends, interest, rent net of losses, and the
  # pass-through lines. Interest is ramped, since debt reprices only as it rolls
  # over. Every dollar cut is accumulated into corp_dY_exog, computed here rather
  # than recovered later by differencing files.
  #
  # Asset values are marked down, each column by its own exposure to corporate
  # equity. This differs from the wealth haircut, which scales the whole balance
  # sheet by one factor: that has to leave the pass-through share and its valuation
  # discount alone, whereas this is a change in the price of equity. Basis never
  # scales. Net worth is recomputed so that the wealth tax and the estate base
  # reprice on the smaller stock.
  #
  # Capital gains move two ways, and which one applies depends on whether the
  # scenario also runs the gains model. Without it, both margins are applied here
  # per record: fewer shares are sold because payouts fall, which scales the flow
  # and its basis together, and each share sells for less, which cuts the gain
  # while leaving basis alone. With the gains model, neither is applied here. The
  # price margin instead debits the stock of gains, and the quantity margin is
  # applied after the gains model has run. Doing both would count them twice.
  #
  # Retirement distributions scale with the balances they are drawn from. IRA
  # withdrawals come from defined-contribution balances, so they scale where such
  # a balance exists. Pensions scale by the record's defined-contribution share of
  # the two, measured before the markdown, because a defined benefit is never
  # marked down and scaling it would cut income with no matching loss of wealth.
  #
  # Gains and retirement distributions stay out of corp_dY_exog. Neither is income
  # from outside the tax system; both move resources the household already holds.
  #
  # A year before enactment returns the frame untouched.
  #
  # Parameters:
  #   - tax_units (df)          : conventional-pass tax units, before behavior
  #   - paths (list)            : the scenario's corporate paths
  #   - year (int)              : simulation year
  #   - kg_dynamics_active (bool) : whether the scenario runs the gains model
  #
  # Returns: tax units with flows cut, assets marked down, net worth recomputed,
  #          and the diagnostic columns corp_dY_exog, corp_markdown and
  #          corp_flow_factor (df).
  #----------------------------------------------------------------------------

  i = match(year, paths$sim$year)
  if (is.na(i)) {
    stop('corp_incidence: no path row for year ', year,
         ' (sim paths cover ', min(paths$sim$year), ':', max(paths$sim$year), ').')
  }
  p = paths$sim[i, ]

  # Nothing to do before enactment, or in any year where the paths are flat.
  inert = abs(p$mu) < CORP_EPS &&
          abs(p$fac_div - 1) < CORP_EPS && abs(p$fac_int - 1) < CORP_EPS &&
          abs(p$fac_rent - 1) < CORP_EPS && abs(p$fac_pt - 1) < CORP_EPS &&
          abs(p$phi) < CORP_EPS
  if (inert) return(tax_units)

  g = function(col) wealth_dyn_safe_col(tax_units, col)

  # Everything below reads values from before the change.
  #
  # The income cut, computed directly from the factors about to be applied. The
  # pass-through net matches the one wealth_dynamics.R forms, over income legs
  # only: the earner splits are payroll bookkeeping rather than cash income.
  pt_net = g('sole_prop') +
           (g('part_active') + g('part_passive') -
            g('part_active_loss') - g('part_passive_loss') - g('part_179')) +
           (g('scorp_active') + g('scorp_passive') -
            g('scorp_active_loss') - g('scorp_passive_loss') - g('scorp_179')) +
           g('farm')

  dY_exog = (p$fac_div  - 1) * (g('div_ord') + g('div_pref')) +
            (p$fac_int  - 1) * (g('txbl_int') + g('exempt_int')) +
            (p$fac_rent - 1) * (g('rent') - g('rent_loss')) +
            (p$fac_pt   - 1) * pt_net

  # The record's overall markdown, for diagnostics, and the split between
  # retirement balance types, both measured before the markdown.
  asset_exposure = corp_asset_exposure()
  exposure_cols = intersect(names(asset_exposure), names(tax_units))
  markdown_amt  = rep(0, nrow(tax_units))
  for (a in exposure_cols) {
    markdown_amt = markdown_amt + asset_exposure[[a]] * p$mu * g(a)
  }
  gross_pre = wealth_dyn_economic_gross(tax_units)

  omega_dc = unname(asset_exposure['value.dc'])
  dc  = g('value.dc')
  db  = g('value.db')
  dc_share = if_else(dc + db > CORP_EPS, dc / (dc + db), 0)
  fac_ira  = 1 - omega_dc * p$mu * as.numeric(dc > CORP_EPS)
  fac_pens = 1 - omega_dc * p$mu * dc_share

  # Gains, for runs without the gains model
  omega_kg = economy_param('corp', 'omega_kg')
  kg_quantity_fac = 1 + omega_kg * p$phi
  kg_lt_delta = omega_kg * (p$phi * g('kg_lt') -
                            p$mu * pmax(g('kg_lt') + g('kg_lt_basis'), 0))

  # Column lists, intersected so a missing column is skipped
  div_cols  = intersect(CORP_FLOWS_DIV,  names(tax_units))
  int_cols  = intersect(CORP_FLOWS_INT,  names(tax_units))
  rent_cols = intersect(CORP_FLOWS_RENT, names(tax_units))
  pt_cols   = intersect(c(WEALTH_CAP_FLOWS_PT, WEALTH_CAP_FLOWS_SE_COMPANIONS),
                        names(tax_units))
  ira_cols  = intersect('txbl_ira_dist', names(tax_units))
  pens_cols = intersect(c('txbl_pens_dist', 'gross_pens_dist'), names(tax_units))
  asset_cols = intersect(ESTATE_ASSET_COLS, names(tax_units))

  # Debts are left alone, so take them off the original frame.
  debts = rowSums(cols_matrix(tax_units, WEALTH_DEBT_COLS))

  out = tax_units %>%
    mutate(
      across(all_of(div_cols),  ~ . * p$fac_div),
      across(all_of(int_cols),  ~ . * p$fac_int),
      across(all_of(rent_cols), ~ . * p$fac_rent),
      across(all_of(pt_cols),   ~ . * p$fac_pt),
      across(all_of(ira_cols),  ~ . * fac_ira),
      across(all_of(pens_cols), ~ . * fac_pens))

  # Mark down each exposed asset column by its own exposure.
  for (a in exposure_cols) {
    out[[a]] = out[[a]] * (1 - asset_exposure[[a]] * p$mu)
  }

  # Gains, only where the gains model is not running
  if (!kg_dynamics_active) {
    if ('kg_lt' %in% names(out))       out$kg_lt = out$kg_lt + kg_lt_delta
    if ('kg_lt_basis' %in% names(out)) out$kg_lt_basis = out$kg_lt_basis * kg_quantity_fac
    if ('kg_st' %in% names(out))       out$kg_st = out$kg_st * kg_quantity_fac
  }

  out %>%
    mutate(
      # Recompute net worth off the marked-down balance sheet, as the wealth
      # haircut also does, so the wealth tax and estate base reprice on it.
      net_worth = rowSums(across(all_of(asset_cols), ~ replace_na(., 0))) - debts,
      corp_dY_exog     = dY_exog,
      corp_markdown    = if_else(gross_pre > CORP_EPS, markdown_amt / gross_pre, 0),
      corp_flow_factor = p$phi)
}



#-------------------------------------------------------------------------------
# Handing the gain adjustments to the gains model
#-------------------------------------------------------------------------------

corp_kg_state_exposed_value = function(tax_units) {

  #----------------------------------------------------------------------------
  # Values the corporate equity behind each record's unrealized gains, weighting
  # each asset class by its exposure. Only the classes the gains model tracks
  # count: retirement balances and trusts are exposed to corporate equity but the
  # gains model does not follow them, so their markdown never reaches the stock of
  # gains.
  #
  # The gains model sums this to cells and debits the stock by the markdown times
  # this value. The debit is dollar for dollar because basis does not move, so the
  # gain absorbs the whole price change.
  #
  # Returns: numeric vector, one per record.
  #----------------------------------------------------------------------------

  asset_exposure = corp_asset_exposure()
  kg_value_cols = intersect(names(asset_exposure),
                            paste0('value.', KG_DYN_ASSET_CLASSES))
  v = rep(0, nrow(tax_units))
  for (a in kg_value_cols) {
    v = v + asset_exposure[[a]] * wealth_dyn_safe_col(tax_units, a)
  }
  v
}



corp_kg_state_debit_by_year = function(scenario_info, baseline_cells) {

  #----------------------------------------------------------------------------
  # Builds the debit to the stock of gains, one vector over ages per year, as the
  # year's markdown times the exposed equity value in each cell.
  #
  # Recomputed from the current markdown each year rather than accumulated through
  # the recurrence. That way, as the markdown shrinks and prices recover, the
  # credit back happens on its own.
  #
  # Returns: list of vectors by year, or NULL where the channel is off.
  #----------------------------------------------------------------------------

  if (scenario_info$ID == 'baseline' ||
      !scenario_uses_corp_incidence(scenario_info)) {
    return(NULL)
  }
  paths = corp_get_paths(scenario_info)

  out = list()
  for (t in scenario_info$years) {
    bt = baseline_cells[[as.character(t)]]
    if (is.null(bt) || is.null(bt$V_corp_exposed)) {
      stop('corp_incidence: baseline kg cells for year ', t, ' lack the ',
           'V_corp_exposed column, so they predate the corporate channel. ',
           'Re-run the kg frozen pass; a stale inputs_cache.rds is the usual ',
           'cause.')
    }
    mu_t = paths$sim$mu[match(t, paths$sim$year)]
    if (is.na(mu_t)) mu_t = 0
    out[[as.character(t)]] = setNames(mu_t * bt$V_corp_exposed,
                                      as.character(bt$age))
  }
  out
}



corp_apply_kg_quantity_to_records = function(tax_units, paths, year) {

  #----------------------------------------------------------------------------
  # Scales realized gains for the change in sale volume, in runs with the gains
  # model. Shares sold back to the company track after-tax payouts, which the
  # gains model cannot produce on its own: it knows marginal rates and mortality,
  # not payout policy. Basis scales with the flow, since fewer lots are sold and
  # the taxable share of each is unchanged.
  #
  # The price side is deliberately absent here. In these runs it debits the stock
  # of gains instead, which is exact because that stock is measured in gains.
  #
  # Runs after the gains model has allocated to records, and only where the gains
  # model is running. Without it, both margins are applied inside
  # corp_apply_to_records() instead. Applying both would count this twice.
  #
  # Gains at death are left alone: death is not a sale.
  #
  # Returns: tax units with realized gains and basis scaled (df).
  #----------------------------------------------------------------------------

  i = match(year, paths$sim$year)
  if (is.na(i)) {
    stop('corp_incidence: no path row for year ', year, ' (kg quantity term).')
  }
  p = paths$sim[i, ]
  fac = 1 + economy_param('corp', 'omega_kg') * p$phi
  if (abs(fac - 1) < CORP_EPS) return(tax_units)

  for (col in intersect(c('kg_lt', 'kg_st', 'kg_lt_basis'), names(tax_units))) {
    tax_units[[col]] = tax_units[[col]] * fac
  }
  tax_units
}



