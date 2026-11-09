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
# The bathtub pre-pass is where the conversion is worked out, because the stock of
# gains has to carry the inflow for every year before any single year is simulated.
# This module applies what the pre-pass computed rather than computing it again, so
# the dollars entering the stock and the dollars leaving the records are the same
# dollars. The marginal rate frames this module is handed go unread for that reason.
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
  # Applies the conversion response, taking the per-record conversions the pre-pass
  # wrote into the year's state file.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units with calculated variables
  #   - baseline_mtrs (df)   : year-id indexed MTRs under baseline, unread here
  #   - static_mtrs (df)     : same columns under the counterfactual, unread here
  #   - scenario_info (list) : get_scenario_info() object
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with wage/PT legs reduced by the
  #          conversion response (SECA companions co-scaled).
  #----------------------------------------------------------------------------

  year = tax_units$year[1]

  # This module used to open with three guards: that kg_dynamics was present, that
  # the families ran in the pinned order, and that this scenario registered the
  # marginal rates the wedge is measured from. The first two are settled before the
  # run starts, by the loader and by behavior_validate_spec() (src/sim/behavior.R).
  # The third belongs to the pre-pass, which is where the rates are now read.

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

  conv = sigma_module_conversions(
    tax_units     = tax_units,
    scenario_info = scenario_info,
    state         = state,
    year          = year
  )

  sigma_apply_conversions(tax_units, conv)
}
