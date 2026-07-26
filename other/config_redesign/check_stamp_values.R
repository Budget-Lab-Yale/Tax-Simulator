#-------------------------------------------------------------------------------
# check_stamp_values.R
#
# Safety net for the calibration-stamp move. The four files under
# config/calibrations/kg/ carry the SAME values that are still live in the
# economy leg's kg.yaml and sigma.yaml, plus the entity-shifting constants that
# are still hardcoded in the module. This check reads both sides and compares
# them, so the switchover is a proven no-op rather than a careful copy nobody
# verified.
#
# It also checks the things a copy can get wrong quietly: nothing lost, nothing
# gained, no entry appearing in two files at once.
#
#   sbatch other/config_redesign/run_tests.sbatch . \
#          other/config_redesign/check_stamp_values.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(library(yaml))

n_pass = 0; n_fail = 0
check = function(label, ok) {
  if (isTRUE(ok)) { n_pass <<- n_pass + 1; cat('PASS  ', label, '\n') }
  else            { n_fail <<- n_fail + 1; cat('FAIL  ', label, '\n') }
}

strip_meta = function(d) d[setdiff(names(d), '_channel')]

old_kg    = strip_meta(read_yaml('config/scenarios/economy/default/kg.yaml'))
old_sigma = strip_meta(read_yaml('config/scenarios/economy/default/sigma.yaml'))

new_bathtub  = strip_meta(read_yaml('config/calibrations/kg/bathtub.yaml'))
new_settings = strip_meta(read_yaml('config/calibrations/kg/settings.yaml'))
new_conv     = strip_meta(read_yaml('config/calibrations/kg/conversion.yaml'))
new_entity   = strip_meta(read_yaml('config/calibrations/kg/entity_shifting.yaml'))

#-------------------------------------------------------------------------------
# 1. Every kg entry lands in exactly one of the two new files, unchanged
#-------------------------------------------------------------------------------

split_names = c(names(new_bathtub), names(new_settings))

check('every kg entry is carried over',
      setequal(names(old_kg), split_names))
check('no kg entry landed in both new files',
      !any(duplicated(split_names)))
check('the split is calibrated vs everything else',
      all(vapply(new_bathtub,  function(e) identical(e$kind, 'calibrated'),
                 logical(1))) &&
      !any(vapply(new_settings, function(e) identical(e$kind, 'calibrated'),
                  logical(1))))

for (nm in names(old_kg)) {
  new_entry = if (nm %in% names(new_bathtub)) new_bathtub[[nm]] else new_settings[[nm]]
  check(sprintf('kg.%s value unchanged', nm),
        identical(old_kg[[nm]]$value, new_entry$value))
  check(sprintf('kg.%s provenance unchanged', nm),
        identical(old_kg[[nm]], new_entry))
}

#-------------------------------------------------------------------------------
# 2. sigma's entries land in conversion.yaml, unchanged
#-------------------------------------------------------------------------------

check('every sigma entry is carried over',
      setequal(names(old_sigma), names(new_conv)))
for (nm in names(old_sigma)) {
  check(sprintf('sigma.%s value unchanged', nm),
        identical(old_sigma[[nm]]$value, new_conv[[nm]]$value))
  check(sprintf('sigma.%s provenance unchanged', nm),
        identical(old_sigma[[nm]], new_conv[[nm]]))
}

#-------------------------------------------------------------------------------
# 3. The entity-shifting constants match what the module still hardcodes
#
# These have no old config entry to compare against -- they were literals in the
# module file -- so the module source is the reference. Read the numbers straight
# out of it rather than restating them here, which would only prove the test
# agrees with itself.
#-------------------------------------------------------------------------------

mod = readLines('src/behavior/entity_shifting/pearce_prisinzano.R', warn = FALSE)
num_after = function(pattern) {
  hit = grep(pattern, mod, value = TRUE)[1]
  if (is.na(hit)) return(NA_real_)
  as.numeric(regmatches(hit, regexpr('[0-9]+[.]?[0-9]*', hit)))
}

check('entity semi-elasticity numerator matches the module',
      isTRUE(all.equal(new_entity$semi_elasticity_raw$value,
                       num_after('^\\s*e\\s*=\\s*0[.]3788'))))
check('entity denominator matches the module',
      isTRUE(all.equal(new_entity$pt_share_of_business_income$value, 0.6)) &&
      any(grepl('0[.]3788\\s*/\\s*0[.]6', mod)))
check('entity alpha matches the module',
      isTRUE(all.equal(new_entity$current_payout_share$value,
                       num_after('^\\s*alpha\\s*='))))
check('entity beta_legacy matches the module',
      isTRUE(all.equal(new_entity$beta_legacy$value,
                       num_after('^\\s*beta_legacy\\s*='))))

#-------------------------------------------------------------------------------
# 4. Shape checks on the new files
#-------------------------------------------------------------------------------

for (f in c('bathtub', 'settings', 'conversion', 'entity_shifting')) {
  path = file.path('config/calibrations/kg', paste0(f, '.yaml'))
  d = read_yaml(path)
  check(sprintf('%s.yaml declares a channel role', f),
        identical(d[['_channel']]$role, 'state'))
  entries = strip_meta(d)
  check(sprintf('%s.yaml every entry has a value and a kind', f),
        all(vapply(entries,
                   function(e) is.list(e) && !is.null(e$value) && !is.null(e$kind),
                   logical(1))))
}

check('the generated files say they are generated',
      all(vapply(c('bathtub', 'conversion'),
                 function(f) any(grepl('GENERATED FILE',
                   readLines(file.path('config/calibrations/kg',
                                       paste0(f, '.yaml')), n = 3))),
                 logical(1))))
check('settings.yaml says it is hand-editable',
      any(grepl('HAND-EDITABLE',
                readLines('config/calibrations/kg/settings.yaml', n = 3))))

cat(sprintf('\n%d passed, %d failed\n', n_pass, n_fail))
if (n_fail > 0) { cat('STAMP_VALUE_CHECK_FAIL\n'); quit(status = 1) }
cat('STAMP_VALUE_CHECK_PASS\n')
