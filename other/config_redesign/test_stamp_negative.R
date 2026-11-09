#-------------------------------------------------------------------------------
# test_stamp_negative.R
#
# The negative test. Everything else in this suite proves that a CLEAN
# configuration passes; that is only half the claim, and the less important half.
# This proves the check actually bites: corrupt one thing at a time in a scratch
# copy of a calibration file, and confirm the run stops with a message that names
# what moved.
#
# A staleness check nobody has watched fail is a check nobody should trust.
#
#   sbatch other/config_redesign/run_tests.sbatch . \
#          other/config_redesign/test_stamp_negative.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)
source('./src/misc/scenario_config.R')
source('./src/sim/behavior.R')
source('./src/misc/calibrations.R')

n_pass = 0; n_fail = 0
check = function(label, ok) {
  if (isTRUE(ok)) { n_pass <<- n_pass + 1; cat('PASS  ', label, '\n') }
  else            { n_fail <<- n_fail + 1; cat('FAIL  ', label, '\n') }
}

# The vintages a clean run of the shipped bathtub calibration expects.
LIVE_VINTAGES = list(tax_data = '2026070814', macro_projections = '2026022522')

scratch = file.path(tempdir(), 'stamp_neg')
dir.create(scratch, showWarnings = FALSE)

# A scenario that binds a scratch copy of the bathtub file, so the shipped one is
# never touched.
make_spec = function(path) {
  list(alternative = 'negative_test',
       kg_dynamics = list(bathtub = path),
       kg_pieces   = 'bathtub',
       modules     = character(),
       listed      = character(),
       families    = character(),
       waivers     = list())
}

# Each scratch copy keeps the BASE NAME bathtub.yaml and varies its folder. That
# is not cosmetic: entries are labelled '{file stem}.{entry}', so a copy called
# something else would be labelled differently and a waiver written against
# `bathtub.eta_logs` would silently not apply to it. Same reason a generated sweep
# file has to be .../sweeps/eta_15/bathtub.yaml rather than .../eta_15.yaml.
corrupt = function(tag, edit) {
  dir.create(file.path(scratch, tag), showWarnings = FALSE, recursive = TRUE)
  path = file.path(scratch, tag, 'bathtub.yaml')
  txt  = readLines('config/calibrations/kg/bathtub.yaml', warn = FALSE)
  writeLines(edit(txt), path)
  # a fresh cache each time -- the loader memoizes by path, and these files are
  # written specifically to be read once
  rm(list = ls(envir = .calib_cache), envir = .calib_cache)
  path
}

run = function(path) {
  tryCatch({
    calib_check_staleness(make_spec(path), LIVE_VINTAGES, enforce = TRUE)
    NULL
  }, error = function(e) conditionMessage(e))
}

#-------------------------------------------------------------------------------
# Control: the shipped file, with the vintages it was derived under, is clean.
#-------------------------------------------------------------------------------

clean = corrupt('clean', function(txt) txt)
check('the shipped calibration passes unchanged', is.null(run(clean)))

#-------------------------------------------------------------------------------
# Arm 1 -- the upstream data vintage moved
#-------------------------------------------------------------------------------

err = tryCatch({
  calib_check_staleness(make_spec(clean),
                        list(tax_data = '2099010101',
                             macro_projections = '2026022522'),
                        enforce = TRUE)
  NULL
}, error = function(e) conditionMessage(e))
check('a moved data vintage stops the run',
      !is.null(err) && grepl('STALE CALIBRATION', err))
check('and the message names the vintage that moved',
      !is.null(err) && grepl('2099010101', err))

#-------------------------------------------------------------------------------
# Arm 2 -- a file the calibration declares itself invalidated by has changed
#-------------------------------------------------------------------------------

p = corrupt('hash', function(txt)
  sub('^(    src/sim/kg/constants[.]R: )[0-9a-f]{32}$',
      paste0('\\1', strrep('f', 32)), txt))
err = run(p)
check('a changed dependency file stops the run',
      !is.null(err) && grepl('STALE CALIBRATION', err))
check('and the message names the file that changed',
      !is.null(err) && grepl('src/sim/kg/constants[.]R has changed', err))

#-------------------------------------------------------------------------------
# Arm 3 -- a setting the calibration was conditioned on has moved
#
# The arm that only exists because of this phase: before it, the conditioning set
# was prose in a comment and changing a model-form switch cost nothing.
#-------------------------------------------------------------------------------

p = corrupt('cond', function(txt)
  sub('settings.kg.timing_ref_wedge: 0.05',
      'settings.kg.timing_ref_wedge: 0.99', txt, fixed = TRUE))
err = run(p)
check('a moved conditioning setting stops the run',
      !is.null(err) && grepl('STALE CALIBRATION', err))
check('and the message names the setting and both values',
      !is.null(err) && grepl('timing_ref_wedge', err) && grepl('0.99', err))

#-------------------------------------------------------------------------------
# Arm 4 -- a dated waiver in the POINTING file gets past the stop, loudly
#-------------------------------------------------------------------------------

waived_spec = make_spec(clean)
waived_spec$waivers = list(
  `bathtub.eta_logs` = list(date = '2026-07-26', reason = 'negative test'))

rm(list = ls(envir = .calib_cache), envir = .calib_cache)
msgs = character()
err = withCallingHandlers(
  tryCatch({
    calib_check_staleness(waived_spec,
                          list(tax_data = '2099010101',
                               macro_projections = '2026022522'),
                          enforce = TRUE)
    NULL
  }, error = function(e) conditionMessage(e)),
  message = function(m) { msgs <<- c(msgs, conditionMessage(m))
                          invokeRestart('muffleMessage') })

# eta_logs is waived; the other three entries are not, so the run still stops --
# which is the right behaviour and worth asserting rather than assuming.
check('a waiver is reported under a banner',
      any(grepl('WAIVED', msgs)) && any(grepl('negative test', msgs)))
check('waiving one entry does not waive the others',
      !is.null(err) && grepl('timeable_share_logs', err) &&
      !grepl('eta_logs: pinned', err))

# The file stem IS the label prefix, which is easy to get wrong and silent when
# you do -- so assert it rather than leave it implied.
check('a waiver keyed to the wrong file stem does not apply', {
  mis = make_spec(clean)
  mis$waivers = list(`nosuchfile.eta_logs` = list(date = '2026-07-26',
                                                  reason = 'wrong stem'))
  rm(list = ls(envir = .calib_cache), envir = .calib_cache)
  e = tryCatch({
    calib_check_staleness(mis, list(tax_data = '2099010101',
                                    macro_projections = '2026022522'),
                          enforce = TRUE)
    NULL
  }, error = function(e) conditionMessage(e))
  !is.null(e) && grepl('eta_logs', e)
})

cat(sprintf('\n%d passed, %d failed\n', n_pass, n_fail))
if (n_fail > 0) { cat('STAMP_NEGATIVE_TESTS_FAIL\n'); quit(status = 1) }
cat('STAMP_NEGATIVE_TESTS_PASS\n')
