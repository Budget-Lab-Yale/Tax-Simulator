#-------------------------------------------------------------------------------
# apply.R
#
# Record appliers: the flow / stock / kg hits, and the kg bathtub glue.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Record applier (built-in conventional-pass step, run_one_year)
#-------------------------------------------------------------------------------

corp_apply_to_records = function(tax_units, paths, year,
                                 kg_dynamics_active = FALSE) {

  #----------------------------------------------------------------------------
  # Applies the year's corporate shock to the record frame, at the head of a
  # conventional-side pass (BEFORE the wealth haircut applier and the behavior
  # modules -- FORMAL_MODEL section 7; static side never sees this, D5).
  #
  # Flows (the D16 IN-list; every applied dollar accumulates analytically into
  # the detail column corp_dY_exog, NEVER by differencing files):
  #   - div_ord/div_pref            x fac_div  = 1 + omega_div * phi_t
  #   - txbl_int/exempt_int         x fac_int  (rollover-ramped)
  #   - rent/rent_loss (net pair)   x fac_rent
  #   - pass-through lines + SE companions (WEALTH_CAP_FLOWS_PT[_WEIGHT])
  #                                 x fac_pt   = 1 - 0.2 * g_ptcap
  #
  # Stocks: exposed value.* columns x (1 - omega_a * mu_t) -- column-specific,
  # NOT the wealth channel's uniform (1 - f) (different design goal: the
  # haircut must keep s_pt/rho_pt invariant; the markdown is an equity-price
  # event). net_worth is recomputed from the marked-down balance sheet (debts
  # untouched) so calc_wealth reprices liab_wealth and calc_estate the estate
  # base. BASIS NEVER SCALES (P5).
  #
  # Gains (D18, one rule, two entry points):
  #   - non-kg runs (kg_dynamics_active = FALSE): the exact per-record form
  #       kg_lt'       = kg_lt + omega_kg * [phi_t * kg_lt
  #                                          - mu_t * max(kg_lt + kg_lt_basis, 0)]
  #       kg_lt_basis' = kg_lt_basis * (1 + omega_kg * phi_t)
  #       kg_st'       = kg_st * (1 + omega_kg * phi_t)
  #     (quantity margin phi co-scales basis -- fewer buyback-forced sales;
  #      the price margin mu hits the SALE VALUE, basis fixed).
  #   - kg runs (kg_dynamics_active = TRUE): kg columns are NOT touched here.
  #     The price margin enters as the bathtub gain-state debit and the phi
  #     quantity term is applied AFTER kg_dyn_apply_to_records in
  #     run_one_year -- applying either here too would double-count.
  #   kg deltas stay OUT of corp_dY_exog (internal conversions, P9).
  #
  # Retirement (P7 two-pocket lemma; OUT of corp_dY_exog). P7 as stated:
  # every cash flow sourced from a MARKED-DOWN stock must scale with the
  # markdown -- so the scaling conditions on the record's OBSERVED source
  # balance (a distribution with no marked-down balance behind it gets no
  # phantom cut; the markdown is proportional, so balance SIZE never matters,
  # only the source mix):
  #   - txbl_ira_dist x (1 - omega_dc * mu_t * 1{value.dc > 0}) -- IRA/DC
  #     draws are definitionally dc-type; scale iff a dc balance exists;
  #   - txbl_pens_dist/gross_pens_dist x (1 - omega_dc * mu_t * dc_share_i),
  #     dc_share_i = value.dc / (value.dc + value.db) on the PRE-markdown
  #     balance sheet: DB-sourced pensions are defined benefits whose balance
  #     is never debited (D10), so scaling them would create a phantom income
  #     cut with no booked resource loss (the reverse P7 violation).
  #
  # Diagnostics (conventional detail only): corp_dY_exog (per-record UNWEIGHTED
  # dollars, negative for a hike), corp_markdown (record-effective markdown
  # fraction of gross assets), corp_flow_factor (phi_t).
  #
  # Pre-enactment years return the frame UNTOUCHED (byte-exact dormancy).
  #
  # Parameters:
  #   - tax_units (df)       : conventional-pass base frame (pre-behavior)
  #   - paths (list)         : corp_get_paths(scenario_info)
  #   - year (int)           : simulation year
  #   - kg_dynamics_active   : TRUE when the scenario runs kg_dynamics (the
  #                            gain adjustments then route through the bathtub)
  #
  # Returns: transformed tax_units (+ diagnostic columns).
  #----------------------------------------------------------------------------

  i = match(year, paths$sim$year)
  if (is.na(i)) {
    stop('corp_incidence: no path row for year ', year,
         ' (sim paths cover ', min(paths$sim$year), ':', max(paths$sim$year), ').')
  }
  p = paths$sim[i, ]

  # Byte-exact dormancy before enactment (and for any inert year).
  inert = abs(p$mu) < CORP_EPS &&
          abs(p$fac_div - 1) < CORP_EPS && abs(p$fac_int - 1) < CORP_EPS &&
          abs(p$fac_rent - 1) < CORP_EPS && abs(p$fac_pt - 1) < CORP_EPS &&
          abs(p$phi) < CORP_EPS
  if (inert) return(tax_units)

  g = function(col) wealth_dyn_safe_col(tax_units, col)

  # --- everything reads PRE values first --------------------------------------
  # Analytic dY_exog from the applied scalings (D16 rider). The pass-through
  # net mirrors wealth_dyn_capital_total's f_pt_net (income legs only; the SE
  # companions are payroll-base bookkeeping, not cash income).
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

  # Record-effective markdown (diagnostic) and the retirement source split,
  # both on the PRE-markdown balance sheet.
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

  # kg adjustments (non-kg runs only; see docstring)
  omega_kg = economy_param('corp', 'omega_kg')
  kg_quantity_fac = 1 + omega_kg * p$phi
  kg_lt_delta = omega_kg * (p$phi * g('kg_lt') -
                            p$mu * pmax(g('kg_lt') + g('kg_lt_basis'), 0))

  # --- column lists (intersect for robustness, wealth-applier style) ----------
  div_cols  = intersect(CORP_FLOWS_DIV,  names(tax_units))
  int_cols  = intersect(CORP_FLOWS_INT,  names(tax_units))
  rent_cols = intersect(CORP_FLOWS_RENT, names(tax_units))
  pt_cols   = intersect(c(WEALTH_CAP_FLOWS_PT, WEALTH_CAP_FLOWS_SE_COMPANIONS),
                        names(tax_units))
  ira_cols  = intersect('txbl_ira_dist', names(tax_units))
  pens_cols = intersect(c('txbl_pens_dist', 'gross_pens_dist'), names(tax_units))
  asset_cols = intersect(ESTATE_ASSET_COLS, names(tax_units))

  # Debts untouched: compute once from the original frame.
  debts = rowSums(cols_matrix(tax_units, WEALTH_DEBT_COLS))

  out = tax_units %>%
    mutate(
      across(all_of(div_cols),  ~ . * p$fac_div),
      across(all_of(int_cols),  ~ . * p$fac_int),
      across(all_of(rent_cols), ~ . * p$fac_rent),
      across(all_of(pt_cols),   ~ . * p$fac_pt),
      across(all_of(ira_cols),  ~ . * fac_ira),
      across(all_of(pens_cols), ~ . * fac_pens))

  # Exposed stocks: column-specific markdown.
  for (a in exposure_cols) {
    out[[a]] = out[[a]] * (1 - asset_exposure[[a]] * p$mu)
  }

  # Gains (non-kg runs).
  if (!kg_dynamics_active) {
    if ('kg_lt' %in% names(out))       out$kg_lt = out$kg_lt + kg_lt_delta
    if ('kg_lt_basis' %in% names(out)) out$kg_lt_basis = out$kg_lt_basis * kg_quantity_fac
    if ('kg_st' %in% names(out))       out$kg_st = out$kg_st * kg_quantity_fac
  }

  out %>%
    mutate(
      # Recompute the stored net-worth stock from the marked-down balance
      # sheet (same recipe as wealth_dyn_apply_to_records / run_one_year), so
      # calc_wealth and calc_estate reprice on the post-markdown stock.
      net_worth = rowSums(across(all_of(asset_cols), ~ replace_na(., 0))) - debts,
      corp_dY_exog     = dY_exog,
      corp_markdown    = if_else(gross_pre > CORP_EPS, markdown_amt / gross_pre, 0),
      corp_flow_factor = p$phi)
}



#-------------------------------------------------------------------------------
# kg_dynamics glue (D18: one rule, two entry points)
#-------------------------------------------------------------------------------

corp_kg_state_exposed_value = function(tax_units) {

  #----------------------------------------------------------------------------
  # Per-record omega-weighted C-corp equity VALUE underlying the kg gain
  # state: only the kg asset classes with corporate exposure (the
  # corp.asset_exposure_* names intersected with value.{KG_DYN_ASSET_CLASSES} --
  # equities and re_fund; dc/trusts are exposed assets but NOT kg classes, so
  # their markdown never enters the kg state). kg_dyn_aggregate_cells sums
  # this to cells; the corporate gain-state debit is then
  #     D_a(t) = mu_t * V_corp_exposed_a(t)
  # -- the dollar value markdown, which debits the gain state dollar-for-
  # dollar (P5: basis fixed, the gain absorbs the entire price hit).
  #
  # Returns: numeric vector, one row per record.
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
  # The per-year corporate gain-state debit vectors for the kg bathtub
  # (kg_dyn_run_bathtub_pass): for each sim year t, a vector over the bathtub
  # ages of D_a(t) = mu_t * V_corp_exposed_a(t), in gain dollars (>= 0 for a
  # hike). RECOMPUTED FROM THE CURRENT mu_t EACH YEAR, never accumulated
  # through the recurrence -- the credit-back as the markdown shrinks (P3's
  # recovery appreciation) is automatic. Returns NULL when the corporate
  # channel is not active for the scenario.
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
           'V_corp_exposed column. The kg cell aggregation predates the ',
           'corporate channel -- re-run the kg frozen pass (a stale ',
           'inputs_cache.rds is the usual cause).')
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
  # The D18 QUANTITY margin in kg_dynamics runs: buyback-forced sale volume
  # tracks after-tax payouts -- a margin the kg_dynamics realization rule
  # (which knows MTRs and mortality, not payout policy) cannot produce. Scales
  # the realization flow kg_lt / kg_st by (1 + omega_kg * phi_t) and co-scales
  # kg_lt_basis (fewer lots sold; the taxable gain ratio is preserved). The
  # PRICE margin deliberately does NOT appear here: in kg runs it enters as
  # the bathtub gain-state debit (corp_kg_state_debit_by_year), which is exact
  # because the state is gain-denominated.
  #
  # ENTRY-POINT EXCLUSIVITY: applied in run_one_year AFTER
  # kg_dyn_apply_to_records, and ONLY when the scenario runs kg_dynamics; the
  # non-kg entry point is corp_apply_to_records' kg block (skipped there via
  # kg_dynamics_active = TRUE). Applying both double-counts the phi term.
  # Deemed death gains (kg_deemed_full / kg_deemed) are left untouched: death
  # is not a buyback-driven sale.
  #
  # Returns: tax_units with scaled kg_lt / kg_st / kg_lt_basis.
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



