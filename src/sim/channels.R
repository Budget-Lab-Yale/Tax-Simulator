#-------------------------------------------------------------------------------
# channels.R
#
# Shared preconditions for the mechanical channels that read raw Tax-Data /
# national aggregates rather than the post-adjustment record frame: kg_dynamics,
# the wealth bathtub, and corporate incidence. Each channel keeps its own
# *_check_run_compat() wrapper for its channel-specific requirements and calls
# this for the conditions all of them share.
#-------------------------------------------------------------------------------



check_raw_data_channel_compat = function(channel, scenario_info,
                                         vat_price_offset) {

  #----------------------------------------------------------------------------
  # Refuses a run whose global settings are incompatible with a raw-dollar
  # channel. All three channels form state (or analytic paths) in raw dollars
  # while the per-record bases they land on are VAT-adjusted, so that
  # adjustment would put the channel in an inconsistent unit system.
  # Full sample is required because every channel's cell aggregates -- and the
  # corporate conservation diagnostic -- assume full-population weights.
  #
  # Parameters:
  #   - channel (str)             : channel name, used in the error messages
  #   - scenario_info (list)      : scenario info; see get_scenario_info()
  #   - vat_price_offset (df)     : VAT price offset series, or NULL
  #
  # Returns: invisibly TRUE; stops on violation.
  #----------------------------------------------------------------------------

  if (!isTRUE(all.equal(globals$pct_sample, 1))) {
    stop(channel, ' requires pct_sample = 1 (full sample). Its cell aggregates ',
         'assume full-population weights; sparse-cell noise at smaller samples ',
         'would masquerade as policy response. Re-run with pct_sample = 1.')
  }

  vat_active = !is.null(vat_price_offset) &&
               'cpi_factor' %in% colnames(vat_price_offset) &&
               any(abs(vat_price_offset$cpi_factor - 1) > 1e-10, na.rm = TRUE)
  if (vat_active) {
    stop(channel, ' is not currently compatible with VAT scenarios: its ',
         'raw-dollar state would mix with VAT-scaled per-record bases. Run ',
         'the reform without a VAT.')
  }

  invisible(TRUE)
}
