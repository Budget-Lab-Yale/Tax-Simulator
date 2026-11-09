#-------------------------------------------------------------------------------
# channels.R
#
# Contains the preconditions shared by the mechanical channels
#-------------------------------------------------------------------------------

# The gains model, the wealth model and corporate incidence all read raw Tax-Data
# and national aggregates rather than the adjusted record frame, so they share a
# set of requirements. Each keeps its own wrapper for what is particular to it and
# calls this for the rest.
#-------------------------------------------------------------------------------



check_raw_data_channel_compat = function(channel, scenario_info,
                                         vat_price_offset) {

  #----------------------------------------------------------------------------
  # Refuses a run whose settings do not suit these channels. Each of them works in
  # raw dollars while the record-level bases they land on are adjusted for a VAT, so
  # a VAT scenario would leave the channel in inconsistent units. And the full sample
  # is required, because their cell aggregates assume the whole population.
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
