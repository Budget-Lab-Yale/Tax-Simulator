#-------------------------------------------------------------------------------
# sigma_conversion.R
#
# Contains functions to convert labor income into unrealized capital gains
#-------------------------------------------------------------------------------

# Assume that owner-managers can take top salary and active pass-through income as
# unrealized equity appreciation instead, and do more of it as the tax advantage of
# doing so widens. The advantage is the record's own marginal rate on the income
# less the tax a dollar of unrealized gain eventually pays, which the gains model
# prices.
#
# Who can do this: a record with some active business income whose taxable income
# reaches the top bracket. That is a threshold rather than a marginal rate, because
# QBI and similar provisions push measured rates below the statutory top rate. What
# they can convert is all wages plus three quarters of active pass-through income,
# the labor content of it. The pool is known to be too broad on the pass-through
# legs; the composition diagnostic reports it.
#
# The response is to the current year's change in the advantage, with no phase-in.
# A narrowing advantage converts income back, clamped so that no leg goes negative.
#
# The work happens in two places, through one shared function so the two cannot
# disagree. The pre-pass computes conversions per record, aggregates them to age
# cells, and adds the result to the stock of gains at year end, writing out only
# the cell totals. The behavior module then recomputes the same conversions from
# the same inputs, applies them to records, and checks the total against what the
# pre-pass wrote.
#
# How much converts is calibrated; how it splits between this and entity shifting
# is an output, not a setting. Running the two in sequence is what stops the same
# dollar moving twice.
#-------------------------------------------------------------------------------

SIGMA_CONV_VERSION = paste('2026-07-12 re-derivation to ETI-0.25 (0.08 -> 0.16;',
                           'driver = entity-shifting tau_eq repricing +',
                           'evasion cross-base fix, measured during the',
                           'estate-margins build)')

#-------------------------------------------------------------------------------
# How the response is calibrated
#
# A five point increase in the top ordinary rate is run with the whole behavior
# stack and targeted to an elasticity of taxable income of 0.25 for the top group,
# the Saez, Slemrod and Giertz central estimate. Taxable income here excludes net
# capital gains. The response is solved by running that reform twice, once with
# conversion and once without, and interpolating onto the target.
#
# The gains model's own calibration is separate: its target is realizations and
# this one excludes them, so the two are fitted on different bases.
#
# The value is conditional on the rest of the stack. Entity shifting and evasion
# supply about 0.22 of the 0.25 target on their own, so what is calibrated here is
# the residual. Re-derive it whenever the entity-shifting parameters, the evasion
# elasticities, the charity elasticity, the pool defined below, or the Tax-Data
# vintage changes. Those dependencies are declared in the calibration file, and the
# parse-time check stops the run when one of them moves.
#
# On the economics: the evidence on taxable income disciplines the total response at
# the top. With entity shifting and evasion already in the stack, a large
# independent conversion margin would count the same behavior twice. A small value
# is also the defensible reading given that the pool above is too broad on the
# pass-through legs.
#-------------------------------------------------------------------------------

# The response parameter, and the labor share applied to active pass-through
# income, live in the calibration file with their provenance. They belong to a
# scenario, so read them where they are used rather than capturing them here.

# Set this to write out the record-level conversions, for validation. An ordinary
# run writes only the cell totals.
SIGMA_RECORD_DUMP = identical(Sys.getenv('SIGMA_RECORD_DUMP'), '1')

# The marginal rates this needs on both legs, one per income line. The
# pass-through rates include self-employment tax, since calc_mtrs() bumps the
# earner splits alongside.
SIGMA_REQUIRED_MTRS = c('mtr_wages1', 'mtr_wages2', 'mtr_part_active',
                        'mtr_sole_prop1', 'mtr_scorp_active')
SIGMA_REQUIRED_MTR_VARS = c('wages1', 'wages2', 'part_active',
                            'sole_prop1', 'scorp_active')

# The columns the pool comes from, read out of raw Tax-Data. Taxable income and
# the marginal rates come from the detail files instead.
SIGMA_TD_COLS = c('id', 'weight', 'filing_status', 'age1', 'age2',
                  'wages1', 'wages2', 'part_active', 'scorp_active',
                  'sole_prop')

# How far the module's recomputed total may sit from what the pre-pass wrote.
# Loose enough to absorb the small drift in pass-through legs when the wealth
# haircut or the corporate step ran before the behavior modules, since both scale
# those legs. That drift compounds over the horizon, so a tolerance tight enough
# for the first decade fails in the out-years. The errors this guard exists to
# catch diverge by half or more, not a few percent.
SIGMA_CONSERVE_RTOL = 0.05

# The check fails only if the divergence exceeds this as well as the relative bar.
# The total is a net flow that can sit near zero, when a package moves rates in
# opposite directions, and then a trivial dollar difference reads as a large
# relative one. A real error here is in the billions.
SIGMA_CONSERVE_ATOL = 2.5e8


scenario_uses_sigma = function(scenario_info) {
  'conversion' %in% (scenario_info$resolved_behavior$spec$families %||% character())
}



sigma_top_thresholds = function(tax_law, years) {

  #----------------------------------------------------------------------------
  # Finds where the top ordinary bracket starts, per year and filing status, as the
  # highest bracket with a value. A threshold rather than a marginal rate, because
  # QBI and similar provisions push measured rates below the statutory top rate.
  #
  # Returns: tibble of year, filing status and threshold.
  #----------------------------------------------------------------------------

  bracket_cols = grep('^ord\\.brackets\\d+$', names(tax_law), value = TRUE)
  if (length(bracket_cols) == 0) {
    stop('sigma_conversion: tax_law has no ord.brackets columns; cannot ',
         'resolve the top-bracket gate threshold.')
  }
  bracket_cols = bracket_cols[order(as.integer(
    sub('^ord\\.brackets', '', bracket_cols)))]

  out = tax_law %>%
    filter(year %in% years) %>%
    select(year, filing_status, all_of(bracket_cols)) %>%
    distinct()

  vals = as.matrix(out[, bracket_cols])
  top  = apply(vals, 1, function(r) {
    ok = which(is.finite(r))
    if (length(ok) == 0) NA_real_ else r[max(ok)]
  })

  out = out %>%
    mutate(sigma_thresh = as.numeric(top)) %>%
    select(year, filing_status, sigma_thresh)

  if (any(!is.finite(out$sigma_thresh))) {
    stop('sigma_conversion: non-finite top-bracket threshold for (year, ',
         'filing status) rows: ',
         paste(out$year[!is.finite(out$sigma_thresh)],
               out$filing_status[!is.finite(out$sigma_thresh)],
               sep = '/', collapse = ', '))
  }
  out
}



sigma_check_mtr_registration = function(scenario_info) {

  # Stop unless the runscript registers every marginal rate this needs, rather than
  # silently labelling a static score as conventional.
  missing = setdiff(SIGMA_REQUIRED_MTR_VARS,
                    scenario_info$mtr_vars %||% character())
  if (length(missing) > 0) {
    stop('sigma_conversion: scenario "', scenario_info$ID, '" must register ',
         'mtr_vars for all sigma wedge legs (',
         paste(SIGMA_REQUIRED_MTR_VARS, collapse = ' '),
         ', plus kg_lt for the kg bathtub). Missing: ',
         paste(missing, collapse = ', '), '.')
  }
  invisible(TRUE)
}



sigma_build_ctx = function(scenario_info, tax_law, baseline_root,
                           sample_ids, pct_sample,
                           sigma = kg_conversion('conv')) {

  #----------------------------------------------------------------------------
  # Assembles what the pre-pass needs to compute conversions. Checks the parameters
  # and the registered marginal rates, and resolves the thresholds by year. Each
  # year's own inputs are read when that year comes up.
  #----------------------------------------------------------------------------

  if (!is.finite(sigma) || sigma < 0 || sigma > 5) {
    stop('sigma_conversion: assumption sigma.conv must be a finite nonnegative ',
         'value (percent of pool per pp of wedge); got ', format(sigma), '.')
  }
  sigma_check_mtr_registration(scenario_info)

  list(
    scenario_info = scenario_info,
    baseline_root = baseline_root,
    sample_ids    = sample_ids,
    pct_sample    = pct_sample,
    thresholds    = sigma_top_thresholds(tax_law, scenario_info$years),
    sigma         = sigma
  )
}



sigma_load_year_inputs = function(ctx, year) {

  #----------------------------------------------------------------------------
  # Assembles one year's pool. The income legs and demographics come from raw
  # Tax-Data, which is the right unit system since these channels refuse a VAT
  # scenario. Taxable income and the marginal rates come from the two legs' static
  # detail.
  #----------------------------------------------------------------------------

  scenario_info = ctx$scenario_info
  tax_data_root = scenario_info$interface_paths$`Tax-Data`

  td = file.path(tax_data_root, paste0('tax_units_', year, '.csv')) %>%
    fread(select = SIGMA_TD_COLS, showProgress = FALSE) %>%
    as_tibble() %>%
    filter(id %in% ctx$sample_ids) %>%
    mutate(weight = weight / ctx$pct_sample)

  read_detail = function(root, cols) {
    f = file.path(root, paste0(year, '.csv'))
    have = names(fread(f, nrows = 0, showProgress = FALSE))
    missing_cols = setdiff(cols, have)
    if (length(missing_cols) > 0) {
      stop('sigma_conversion: detail file ', f, ' lacks column(s): ',
           paste(missing_cols, collapse = ', '),
           '. The sigma wedge needs per-leg MTRs written by the static ',
           'pass; check the runscript mtr_vars registration on BOTH the ',
           'baseline and the scenario rows.')
    }
    fread(f, select = cols, showProgress = FALSE) %>% as_tibble()
  }

  static_det = read_detail(
    file.path(scenario_info$output_path, 'static', 'detail'),
    c('id', 'txbl_inc', SIGMA_REQUIRED_MTRS))
  baseline_det = read_detail(
    file.path(ctx$baseline_root, 'baseline', 'static', 'detail'),
    c('id', SIGMA_REQUIRED_MTRS)) %>%
    rename_with(.cols = -id, .fn = ~ paste0(., '_baseline'))

  td %>%
    mutate(age_cohort = sigma_age_cohort(filing_status, age1, age2)) %>%
    left_join(static_det,   by = 'id') %>%
    left_join(baseline_det, by = 'id')
}



sigma_age_cohort = function(filing_status, age1, age2) {

  # Same age convention as the gains model: joint records take the older spouse,
  # clipped to the cohort grid.
  age = if_else(filing_status == 2, pmax(age1, age2, na.rm = TRUE), age1)
  pmax(KG_DYN_AGE_MIN, pmin(KG_DYN_AGE_MAX, age))
}



sigma_compute_conversions = function(pool, thresholds_t,
                                     tau_eq_B_col, tau_eq_S_col, sigma) {

  #----------------------------------------------------------------------------
  # The shared function. Both the pre-pass and the behavior module compute
  # conversions through this, from the same inputs, so the two cannot disagree.
  #
  # For each income leg, the change in the tax advantage of converting is the change
  # in that leg's own marginal rate less the change in the tax a dollar of unrealized
  # gain pays. Conversion is the response parameter times that, times the leg,
  # clamped so no leg can go negative. A missing rate leaves that leg alone.
  #
  # Parameters:
  #   - pool (df)            : one row per record, with the five income legs, taxable
  #                            income, and the marginal rate on each leg under both
  #                            the baseline and the scenario
  #   - thresholds_t (df)    : the top bracket threshold by filing status
  #   - tau_eq_B_col (dbl[]) : tax on a dollar of gain by age, baseline
  #   - tau_eq_S_col (dbl[]) : the same under the scenario
  #   - sigma (dbl)          : the response parameter
  #
  # Returns: one row per record, with the conversion on each leg, the total, the pool
  #          and the wedge changes behind them.
  #----------------------------------------------------------------------------

  dtau_eq = as.numeric(tau_eq_S_col[as.character(pool$age_cohort)]) -
            as.numeric(tau_eq_B_col[as.character(pool$age_cohort)])

  pt_labor_share = kg_conversion('pt_labor_share')

  out = pool %>%
    left_join(thresholds_t, by = 'filing_status') %>%
    mutate(
      dtau_eq = dtau_eq,

      # Who can convert: some active business income, and taxable income at or above
      # the top bracket.
      has_active = (!is.na(part_active)  & part_active  > 0) |
                   (!is.na(scorp_active) & scorp_active > 0) |
                   (!is.na(sole_prop)    & sole_prop    > 0),
      gate = has_active & !is.na(txbl_inc) & !is.na(sigma_thresh) &
             txbl_inc >= sigma_thresh,

      # A leg only participates when it is positive
      pool_w1    = if_else(gate & !is.na(wages1) & wages1 > 0, wages1, 0),
      pool_w2    = if_else(gate & !is.na(wages2) & wages2 > 0, wages2, 0),
      pool_part  = if_else(gate & !is.na(part_active) & part_active > 0,
                           pt_labor_share * part_active, 0),
      pool_scorp = if_else(gate & !is.na(scorp_active) & scorp_active > 0,
                           pt_labor_share * scorp_active, 0),
      pool_sole  = if_else(gate & !is.na(sole_prop) & sole_prop > 0,
                           pt_labor_share * sole_prop, 0),
      pool_total = pool_w1 + pool_w2 + pool_part + pool_scorp + pool_sole,

      # The change in each leg's advantage, scenario against baseline
      dW_w1    = (mtr_wages1       - mtr_wages1_baseline)       - dtau_eq,
      dW_w2    = (mtr_wages2       - mtr_wages2_baseline)       - dtau_eq,
      dW_part  = (mtr_part_active  - mtr_part_active_baseline)  - dtau_eq,
      dW_scorp = (mtr_scorp_active - mtr_scorp_active_baseline) - dtau_eq,
      dW_sole  = (mtr_sole_prop1   - mtr_sole_prop1_baseline)   - dtau_eq,

      # The conversion itself, clamped to the leg
      conv_w1    = sigma_leg_conv(sigma, dW_w1,    pool_w1,    wages1),
      conv_w2    = sigma_leg_conv(sigma, dW_w2,    pool_w2,    wages2),
      conv_part  = sigma_leg_conv(sigma, dW_part,  pool_part,  part_active),
      conv_scorp = sigma_leg_conv(sigma, dW_scorp, pool_scorp, scorp_active),
      conv_sole  = sigma_leg_conv(sigma, dW_sole,  pool_sole,  sole_prop),
      conv_total = conv_w1 + conv_w2 + conv_part + conv_scorp + conv_sole
    ) %>%
    select(id, weight, age_cohort, gate, pool_total,
           pool_w1, pool_w2, pool_part, pool_scorp, pool_sole,
           dtau_eq, dW_w1, dW_w2, dW_part, dW_scorp, dW_sole,
           conv_w1, conv_w2, conv_part, conv_scorp, conv_sole, conv_total)

  out
}



sigma_leg_conv = function(sigma, dW, pool_leg, leg_value) {

  # The response times the wedge change times the leg, clamped to the leg
  leg = replace_na(as.numeric(leg_value), 0)
  conv = if_else(is.na(dW) | pool_leg <= 0, 0, sigma * dW * pool_leg)
  pmin(pmax(conv, -abs(leg)), abs(leg))
}



sigma_aggregate_inflow = function(conv,
                                  ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Sum to cells, over the whole age grid. A record with no age converts nothing
  # anyway, since its priced gain is missing and every leg is left alone, so drop
  # those rather than carry an unnamed entry.
  agg = conv %>%
    filter(!is.na(age_cohort)) %>%
    group_by(age_cohort) %>%
    summarise(inflow = sum(weight * conv_total), .groups = 'drop')

  inflow = setNames(rep(0, length(ages_bathtub)), as.character(ages_bathtub))
  inflow[as.character(agg$age_cohort)] = agg$inflow
  inflow
}



sigma_make_tracker = function(conv, conv_inflow, sigma, thresholds_t, year) {

  #----------------------------------------------------------------------------
  # What gets written into the state file: the cell totals, the size and composition
  # of the pool, the average wedges, the thresholds, and the response parameter,
  # which the behavior module checks against its own.
  #----------------------------------------------------------------------------

  gated = conv %>% filter(gate)
  wsum  = function(x, w) sum(w * x)
  wmean = function(x, w) { s = sum(w); if (s > 0) sum(w * x) / s else NA_real_ }

  pool_dollars = wsum(gated$pool_total, gated$weight)
  ord_dW = with(gated,
    (dW_w1 * pool_w1 + dW_w2 * pool_w2 + dW_part * pool_part +
     dW_scorp * pool_scorp + dW_sole * pool_sole))
  mean_dW = if (pool_dollars > 0) wsum(ord_dW, gated$weight) / pool_dollars
            else NA_real_

  list(
    version           = SIGMA_CONV_VERSION,
    year              = year,
    sigma             = sigma,
    conv_inflow       = conv_inflow,
    conv_total        = sum(conv_inflow),
    pool_records      = nrow(gated),
    pool_weighted     = sum(gated$weight),
    pool_dollars      = pool_dollars,
    pool_dollars_wages = wsum(gated$pool_w1 + gated$pool_w2, gated$weight),
    pool_dollars_pt   = wsum(gated$pool_part + gated$pool_scorp +
                             gated$pool_sole, gated$weight),
    conv_dollars_wages = wsum(gated$conv_w1 + gated$conv_w2, gated$weight),
    conv_dollars_pt   = wsum(gated$conv_part + gated$conv_scorp +
                             gated$conv_sole, gated$weight),
    mean_dW_pooled    = mean_dW,
    mean_dtau_eq      = wmean(gated$dtau_eq, gated$weight),
    thresholds        = thresholds_t
  )
}



sigma_compute_year = function(ctx, year, tau_eq_B_col, tau_eq_S_col,
                              ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Runs one year of the pre-pass: read the inputs, compute the conversions, sum them
  # to cells, and assemble what gets written out.
  #
  # Returns: list of the cell totals and the record of them.
  #----------------------------------------------------------------------------

  thresholds_t = ctx$thresholds %>%
    filter(year == !!year) %>%
    select(filing_status, sigma_thresh)

  pool = sigma_load_year_inputs(ctx, year)
  conv = sigma_compute_conversions(
    pool         = pool,
    thresholds_t = thresholds_t,
    tau_eq_B_col = tau_eq_B_col,
    tau_eq_S_col = tau_eq_S_col,
    sigma        = ctx$sigma
  )

  conv_inflow = sigma_aggregate_inflow(conv, ages_bathtub)
  tracker     = sigma_make_tracker(conv, conv_inflow, ctx$sigma,
                                   thresholds_t, year)

  if (SIGMA_RECORD_DUMP) {
    dump_dir = file.path(ctx$scenario_info$output_path, 'conventional',
                         'supplemental', 'sigma_conversion_dump')
    dir.create(dump_dir, recursive = TRUE, showWarnings = FALSE)
    conv %>%
      filter(gate) %>%
      write_csv(file.path(dump_dir, paste0(year, '.csv')))
  }

  message(sprintf(
    paste0('sigma_conversion: year %d, sigma = %.2f: pool = $%.1fB over ',
           '%s gated records (weighted %.2fM); mean pooled dW = %s; ',
           'conversion inflow = $%.2fB.'),
    year, ctx$sigma, tracker$pool_dollars / 1e9, tracker$pool_records,
    tracker$pool_weighted / 1e6,
    ifelse(is.na(tracker$mean_dW_pooled), 'NA',
           sprintf('%.4f', tracker$mean_dW_pooled)),
    tracker$conv_total / 1e9))

  list(conv_inflow = conv_inflow, tracker = tracker)
}



#-------------------------------------------------------------------------------
# Helpers for the behavior module
#-------------------------------------------------------------------------------

sigma_module_recompute = function(tax_units, baseline_mtrs, static_mtrs,
                                  scenario_info, state, year) {

  #----------------------------------------------------------------------------
  # Recomputes the conversions inside the behavior module, from the same inputs the
  # pre-pass used: the live frame's income legs, the year's marginal rates, and the
  # priced gain and thresholds the pre-pass wrote out. Checks the total against what
  # the pre-pass wrote.
  #
  # Returns: the conv frame (one row per tax_units row, original order).
  #----------------------------------------------------------------------------

  tracker    = state$sigma
  cell_table = state$cell_table

  if (is.null(tracker)) {
    stop('sigma_conversion: kg state file for year ', year, ' carries no ',
         'sigma tracker. The bathtub pre-pass must run with the ',
         'conversion/sigma module registered in the behavior column ',
         '(it computes conversions and injects the gain-state inflow); ',
         're-run the pipeline with the current runscript.')
  }
  if (!isTRUE(all.equal(tracker$sigma, kg_conversion('conv')))) {
    stop('sigma_conversion: sigma.conv drift between the pre-pass (',
         tracker$sigma, ') and the behavior module (',
         kg_conversion('conv'), '). The resolved assumption must be ',
         'identical across pipeline phases.')
  }

  tau_eq_B_col = setNames(cell_table$tau_eq_B, as.character(cell_table$age))
  tau_eq_S_col = setNames(cell_table$tau_eq_S, as.character(cell_table$age))

  # Taxable income comes from the static detail, as it does in the pre-pass. The
  # live frame has none before do_taxes() has run.
  static_txbl = file.path(scenario_info$output_path, 'static', 'detail',
                          paste0(year, '.csv')) %>%
    fread(select = c('id', 'txbl_inc'), showProgress = FALSE) %>%
    as_tibble()

  mtr_b = baseline_mtrs %>%
    filter(year == !!year) %>%
    select(id, all_of(SIGMA_REQUIRED_MTRS)) %>%
    rename_with(.cols = -id, .fn = ~ paste0(., '_baseline'))
  mtr_s = static_mtrs %>%
    filter(year == !!year) %>%
    select(id, all_of(SIGMA_REQUIRED_MTRS))

  pool = tax_units %>%
    select(id, weight, filing_status, age1, age2,
           wages1, wages2, part_active, scorp_active, sole_prop) %>%
    mutate(age_cohort = sigma_age_cohort(filing_status, age1, age2)) %>%
    left_join(static_txbl, by = 'id') %>%
    left_join(mtr_b, by = 'id') %>%
    left_join(mtr_s, by = 'id')

  conv = sigma_compute_conversions(
    pool         = pool,
    thresholds_t = tracker$thresholds,
    tau_eq_B_col = tau_eq_B_col,
    tau_eq_S_col = tau_eq_S_col,
    sigma        = kg_conversion('conv')
  )

  # The recomputed conversions must match what the pre-pass added to the stock of
  # gains. Exact where the live frame's legs still equal raw Tax-Data; the tolerance
  # absorbs the small scaling the wealth and corporate steps apply ahead of this.
  module_total = sum(conv$weight * conv$conv_total)
  prepass_total = tracker$conv_total
  denom = max(abs(prepass_total), 1e6)
  rel   = abs(module_total - prepass_total) / denom
  message(sprintf(
    paste0('sigma_conversion: year %d conservation check: module $%.4fB vs ',
           'pre-pass $%.4fB (rel diff %.2e).'),
    year, module_total / 1e9, prepass_total / 1e9, rel))
  if (rel > SIGMA_CONSERVE_RTOL &&
      abs(module_total - prepass_total) > SIGMA_CONSERVE_ATOL) {
    stop(sprintf(
      paste0('sigma_conversion: conservation failure in year %d: module ',
             'recompute (%.6g) vs persisted cell inflow (%.6g), rel diff ',
             '%.3e > %.1e and abs diff $%.3fB > $%.3fB. Records applied and ',
             'dollars injected into the gain state have diverged; check that ',
             'pre-pass and module see the same MTR frames, thresholds, and ',
             'pool legs.'),
      year, module_total, prepass_total, rel, SIGMA_CONSERVE_RTOL,
      abs(module_total - prepass_total) / 1e9, SIGMA_CONSERVE_ATOL / 1e9))
  }

  conv
}



sigma_apply_conversions = function(tax_units, conv) {

  #----------------------------------------------------------------------------
  # Takes the conversions off the live frame. Wages come down together, preserving
  # the residual between the total and the two earners' shares. Pass-through legs come
  # down with their earner splits, where they have them.
  #
  # The converted dollars do not appear in realized gains. They are unrealized, and
  # the tax on them arrives in later years through the gains model.
  #----------------------------------------------------------------------------

  stopifnot(identical(conv$id, tax_units$id))

  scale_or_1 = function(base, delta) {
    if_else(!is.na(base) & base > 0 & delta != 0,
            (base - delta) / base, 1)
  }
  part_factor = scale_or_1(tax_units$part_active, conv$conv_part)
  sole_factor = scale_or_1(tax_units$sole_prop,   conv$conv_sole)

  tax_units %>%
    mutate(
      wages1 = wages1 - conv$conv_w1,
      wages2 = wages2 - conv$conv_w2,
      wages  = wages  - conv$conv_w1 - conv$conv_w2,

      part_active = part_active * part_factor,
      part_se1    = part_se1    * part_factor,
      part_se2    = part_se2    * part_factor,

      sole_prop  = sole_prop  * sole_factor,
      sole_prop1 = sole_prop1 * sole_factor,
      sole_prop2 = sole_prop2 * sole_factor,

      scorp_active = scorp_active - conv$conv_scorp
    )
}
