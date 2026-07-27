#-------------------------------------------------------------------------------
# conversion/sigma.R
#
# Contains the income conversion response
#-------------------------------------------------------------------------------

# Assume that owner-managers can take top salary and active pass-through income as
# unrealized equity appreciation instead, and do more of it as the tax advantage of
# doing so widens. The machinery is in src/sim/sigma_conversion.R, which this module
# calls, and the gains model prices the equity side.
#
# What moves is the form in which labor is paid. A record's wage and pass-through
# legs shrink, and the converted dollars enter the stock of unrealized gains, where
# they realize at that holder's rate and meet whatever happens to gains at death.
# Nothing is added to realized gains here: converted gains are unrealized, and the
# tax on them arrives in later years.
#
# The response is to the current year's change in the advantage, with no phase-in,
# the same convention entity shifting and evasion use.
#
# The response parameter is calibrated as a residual, since entity shifting and
# evasion supply most of the target on their own. The value and its provenance are in
# config/calibrations/kg/conversion.yaml, and the method is described in
# src/sim/sigma_conversion.R. Calibrating it this way is what settled the concern
# that the response would double-count what those two modules already do: the value
# originally asserted was a total-response figure and overshot.
#
# On order: this needs the gains model to have run first, since it reads its state
# and the gains model must not see legs this module has already reduced. And entity
# shifting and evasion need to run after, so that they respond to what is left.
# Running them in sequence is what stops the same dollar moving twice. The loader
# enforces all of that before the run starts.

do_conversion = function(tax_units, baseline_mtrs, static_mtrs,
                         scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Applies the conversion response. The record-level conversions are recomputed here
  # through the same function the pre-pass used, since only the cell totals were
  # written out, and then checked against those totals.
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

  year = tax_units$year[1]

  # This module used to open with two guards: that kg_dynamics was present, and
  # that the families ran in the pinned order. Both are now settled before the
  # run starts -- the loader sorts the stack and behavior_validate_spec()
  # refuses a conversion module without the bathtub (src/sim/behavior.R). The
  # input guards below stay, because they are about this scenario's MTRs rather
  # than the shape of the stack.

  # --- Guard: required MTRs registered and present in both frames.
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

  # --- Guard: the year's kg state file with the sigma tracker.
  state_path = kg_dyn_state_path(scenario_info, year)
  if (!file.exists(state_path)) {
    stop('do_conversion(): missing kg bathtub state file at ', state_path,
         '. The bathtub pre-pass (which also computes sigma conversions ',
         'and injects the gain-state inflow) must run before the ',
         'conventional pass.')
  }
  state = readRDS(state_path)

  message('do_conversion(): applying sigma income conversion (',
          SIGMA_CONV_VERSION, '; sigma = ', kg_conversion('conv'), ')')

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
