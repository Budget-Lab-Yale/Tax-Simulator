#-------------------------------------------------------------------------------
# test_provenance_guard.R
#
# Parses the files touched by the calibration-provenance guard and exercises
# kg_dyn_check_calibration_provenance() in matched / mismatched configs.
# Run twice (default applier, then KG_APPLIER_ALLOCATION=R) to cover both the
# clean path and the applier-mismatch path.
#-------------------------------------------------------------------------------

files = c('src/sim/kg_dynamics.R', 'src/sim/run.R',
          'other/kg_model_tests/calibrate.R')
for (f in files) { invisible(parse(f)); cat('PARSE OK:', f, '\n') }

source('src/sim/kg_dynamics.R')
cat('\nlive KG_RESPONSE_FORM =', KG_DYN_RESPONSE_FORM,
    '| active eta =', kg_dyn_active_eta(),
    '| active timeable_share =', kg_dyn_active_timeable_share(),
    '\n  (levels eta =', KG_DYN_DEFAULT_ETA, ', logs eta =', KG_DYN_DEFAULT_ETA_LOGS,
    ')\n\n')

# The guard compares the LIVE form's calibrated pair + Tax-Data vintage. Under
# the default (levels) form that vintage is 2026070814.
cal_td_vintage = KG_DYN_CALIB_PROVENANCE$forms[[KG_DYN_RESPONSE_FORM]]$tax_data_vintage

mk = function(td, macro) list(interface_paths = list(`Tax-Data` = td,
                                                      `Macro-Projections` = macro))
matchv = mk(sprintf('/x/Tax-Data/v1/%s/baseline', cal_td_vintage),
            '/x/Macro-Projections/v3/2026022522/baseline')
badv   = mk('/x/Tax-Data/v1/2099999999/baseline',
            '/x/Macro-Projections/v3/2026022522/baseline')

run_check = function(label, si) {
  warned = FALSE
  res = withCallingHandlers(
    kg_dyn_check_calibration_provenance(si),
    warning = function(w) { warned <<- TRUE; invokeRestart('muffleWarning') },
    message = function(m) invokeRestart('muffleMessage'))
  cat(sprintf('[%-22s] returned=%-5s warned=%s\n', label, res, warned))
}

run_check('matching vintages', matchv)
run_check('bad Tax-Data vintage', badv)
