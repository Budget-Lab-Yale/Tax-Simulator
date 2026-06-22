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
cat('\nlive KG_DYN_APPLIER_ALLOCATION =', KG_DYN_APPLIER_ALLOCATION,
    '| KG_DYN_DEFAULT_PSI =', KG_DYN_DEFAULT_PSI,
    '| KG_DYN_SHARE_PLANNED =', KG_DYN_SHARE_PLANNED, '\n\n')

mk = function(td, macro) list(interface_paths = list(`Tax-Data` = td,
                                                      `Macro-Projections` = macro))
matchv = mk('/x/Tax-Data/v1/2026050315/baseline',
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
