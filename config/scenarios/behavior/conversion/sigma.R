#-------------------------------------------------------------------------------
# SIGMA_PROVENANCE
#
# Income-conversion response (the top-tax exercise's sigma): owner-managers
# repackage top salary and active pass-through compensation as unrealized
# equity appreciation when the ordinary-vs-equity-path wedge widens. Live
# design rulings: other/top_tax/DESIGN_LOCK.md; machinery in
# src/sim/sigma_conversion.R (shared pure function) and src/sim/kg_dynamics.R
# (tau_eq recursion + gain-state injection).
#
# Forcing/object pair: the forcing is the per-record, per-leg WEDGE CHANGE
# Delta W = Delta(mtr_leg) - Delta(tau_eq(age)) — ordinary legs from the
# record's own calculator-measured MTRs (static reform vs baseline, the
# standard MTR-frame convention), equity leg from the tau_eq recursion (the
# expected PV tax per dollar entering the kg gain state, priced by finite
# difference against the exact bathtub recurrence dynamics). The object that
# moves is the payment FORM of labor compensation: gated records' wage/PT
# legs shrink and the converted dollars enter the kg bathtub's deviation
# stock, where they realize at the holder's age-specific rate and meet the
# death regime like any other gain. Nothing is added to record kg_lt here —
# converted gains are unrealized; taxation arrives in later years through
# the cell machinery.
#
# No phase-in phi(t): a memoryless annual response to the current-year wedge
# gap, the same convention as entity shifting and evasion. sigma central =
# 0.08 (percent of pool per pp of wedge, env knob SIGMA_CONV), CALIBRATED
# 2026-07-08 to a top-subset ETI of 0.25 (the SSG central, taxable income
# excl. gains) on the +5pp top-ordinary validation leg with the full stack
# running — the residual conversion margin after entity shifting and evasion
# supply ~0.22 of the target on their own. This resolved DESIGN_LOCK ruling
# 2's double-count caveat (the original asserted 0.6 central was a
# total-response anchor and overshot: full-stack ETI 0.431). Full provenance
# + staleness conditions: SIGMA_CALIB_PROVENANCE in src/sim/sigma_conversion.R.
#
# Module order is PINNED and asserted: kg_dynamics -> conversion/sigma ->
# entity_shifting -> evasion (charity may sit anywhere). kg must run first
# (this module consumes its state file, and the kg applier must not see
# sigma-reduced legs it never modeled); entity shifting and evasion must run
# after so their responses operate on the post-conversion compensation base
# (sequential order is what prevents double-moving the same dollar).
#-------------------------------------------------------------------------------

do_conversion = function(tax_units, baseline_mtrs, static_mtrs,
                         scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Applies the sigma income-conversion response. Record-level conversions
  # are recomputed here via the SAME shared pure function the bathtub
  # pre-pass used (sigma_compute_conversions; DESIGN_LOCK ruling 7 — only
  # the cell tracker is persisted), then hard-checked for conservation
  # against the cell inflow the pre-pass injected into the gain state.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units with calculated variables
  #   - baseline_mtrs (df)   : year-id indexed MTRs under baseline; must
  #                            carry mtr_wages1/2, mtr_part_active,
  #                            mtr_sole_prop1, mtr_scorp_active
  #   - static_mtrs (df)     : same columns under the static counterfactual
  #   - scenario_info (list) : get_scenario_info() object
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with wage/PT legs reduced by the
  #          conversion response (SECA companions co-scaled).
  #----------------------------------------------------------------------------

  year    = tax_units$year[1]
  modules = scenario_info$behavior_modules %||% character()

  # --- Guard 1: sigma requires kg_dynamics (the gain state IS the kg
  # bathtub; tau_eq comes from its machinery). No fallbacks.
  if (!any(startsWith(modules, 'kg_dynamics/'))) {
    stop('do_conversion(): the sigma conversion module requires kg_dynamics ',
         'in the behavior column (the converted dollars live in the kg ',
         'bathtub gain state and the equity-leg wedge tau_eq is computed by ',
         'its machinery). Scenario "', scenario_info$ID, '" has: ',
         paste(modules, collapse = ' '), '.')
  }

  # --- Guard 2: pinned relative order for whichever of the four families
  # are present: kg_dynamics -> conversion -> entity_shifting -> evasion.
  fam_pos = function(prefix) {
    i = which(startsWith(modules, prefix))
    if (length(i) == 0) NA_integer_ else min(i)
  }
  order_req = c(kg_dynamics = fam_pos('kg_dynamics/'),
                conversion  = fam_pos('conversion/'),
                entity      = fam_pos('entity_shifting/'),
                evasion     = fam_pos('evasion/'))
  present = order_req[!is.na(order_req)]
  if (is.unsorted(present, strictly = TRUE)) {
    stop('do_conversion(): behavior modules must run in the pinned order ',
         'kg_dynamics -> conversion/sigma -> entity_shifting -> evasion ',
         '(sequential order prevents double-moving the same dollar). ',
         'Scenario "', scenario_info$ID, '" has: ',
         paste(modules, collapse = ' '), '.')
  }

  # --- Guard 3: required MTRs registered and present in both frames.
  required = SIGMA_REQUIRED_MTRS
  missing  = if (is.null(static_mtrs) || is.null(baseline_mtrs)) required else
             setdiff(required, intersect(names(static_mtrs),
                                         names(baseline_mtrs)))
  if (length(missing) > 0) {
    stop('do_conversion(): the sigma module requires registered MTRs for ',
         'wages1, wages2, part_active, sole_prop1, and scorp_active ',
         '(mtr_vars = "wages1 wages2 part_active sole_prop1 scorp_active ',
         'kg_lt ..."). The runscript for scenario "', scenario_info$ID,
         '" is missing: ', paste(missing, collapse = ', '), '.')
  }

  # --- Guard 4: the year's kg state file with the sigma tracker.
  state_path = kg_dyn_state_path(scenario_info, year)
  if (!file.exists(state_path)) {
    stop('do_conversion(): missing kg bathtub state file at ', state_path,
         '. The bathtub pre-pass (which also computes sigma conversions ',
         'and injects the gain-state inflow) must run before the ',
         'conventional pass.')
  }
  state = readRDS(state_path)

  message('do_conversion(): applying sigma income conversion (',
          SIGMA_CONV_VERSION, '; sigma = ', SIGMA_CONV, ')')

  conv = sigma_module_recompute(
    tax_units     = tax_units,
    baseline_mtrs = baseline_mtrs,
    static_mtrs   = static_mtrs,
    scenario_info = scenario_info,
    state         = state,
    year          = year
  )

  sigma_apply_conversions(tax_units, conv)
}
