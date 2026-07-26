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

# The reference side is the economy leg's kg.yaml and sigma.yaml as they stood
# BEFORE the move, read out of git rather than out of the working tree -- they no
# longer exist there. That makes this a claim about history: these values have not
# changed since the commit that last held them in the old location.
REF = 'e6f8955a5'   # Phase 5a, the last commit with both copies present
from_git = function(path) {
  txt = suppressWarnings(system2('git', c('show', paste0(REF, ':', path)),
                                 stdout = TRUE, stderr = FALSE))
  if (length(txt) == 0) stop('could not read ', path, ' at ', REF)
  strip_meta(yaml.load(paste(txt, collapse = '\n')))
}

old_kg    = from_git('config/scenarios/economy/default/kg.yaml')
old_sigma = from_git('config/scenarios/economy/default/sigma.yaml')

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
  # Provenance must be carried over intact. The one permitted ADDITION is
  # conditioned_on: the conditioning set was prose inside each note before, and
  # making it machine-readable is the point of the move, so compare with it
  # removed and assert separately that it appeared.
  # Two permitted differences, both deliberate and both checked separately:
  # conditioned_on is an ADDITION (the conditioning set was prose before), and
  # invalidated_by_hashes moved because the accessor rename changed those files.
  # The dependency LIST must be untouched -- that is what says the calibration
  # still depends on the same things.
  ignore = c('conditioned_on', 'invalidated_by_hashes')
  check(sprintf('kg.%s provenance unchanged', nm),
        identical(old_kg[[nm]][setdiff(names(old_kg[[nm]]), ignore)],
                  new_entry[setdiff(names(new_entry), ignore)]))
  check(sprintf('kg.%s depends on the same files', nm),
        identical(old_kg[[nm]]$invalidated_by, new_entry$invalidated_by) &&
        setequal(names(old_kg[[nm]]$invalidated_by_hashes),
                 names(new_entry$invalidated_by_hashes)))
}

#-------------------------------------------------------------------------------
# 2. sigma's entries land in conversion.yaml, unchanged
#-------------------------------------------------------------------------------

check('every sigma entry is carried over',
      setequal(names(old_sigma), names(new_conv)))
for (nm in names(old_sigma)) {
  check(sprintf('sigma.%s value unchanged', nm),
        identical(old_sigma[[nm]]$value, new_conv[[nm]]$value))
  ig = c('invalidated_by_hashes')
  check(sprintf('sigma.%s provenance unchanged', nm),
        identical(old_sigma[[nm]][setdiff(names(old_sigma[[nm]]), ig)],
                  new_conv[[nm]][setdiff(names(new_conv[[nm]]), ig)]))
  check(sprintf('sigma.%s depends on the same files', nm),
        identical(old_sigma[[nm]]$invalidated_by, new_conv[[nm]]$invalidated_by))
}

#-------------------------------------------------------------------------------
# 3. The entity-shifting constants match what the module still hardcodes
#
# These had no config entry to compare against -- they were literals in the module
# file. So the reference is the module AS IT STOOD AT REF, read out of git: the
# working copy now reads the calibration file, which is the change being checked.
# Read the numbers out of that source rather than restating them here, which would
# only prove the test agrees with itself.
#-------------------------------------------------------------------------------

mod = suppressWarnings(system2(
  'git', c('show', paste0(REF, ':src/behavior/entity_shifting/pearce_prisinzano.R')),
  stdout = TRUE, stderr = FALSE))
if (length(mod) == 0) stop('could not read the entity module at ', REF)
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

live_mod = readLines('src/behavior/entity_shifting/pearce_prisinzano.R',
                     warn = FALSE)
check('the entity module no longer hardcodes its parameters',
      !any(grepl('^\\s*(alpha|beta_legacy)\\s*=\\s*[0-9]', live_mod)) &&
      !any(grepl('^\\s*e\\s*=\\s*0[.]3788', live_mod)))
check('the entity module reads them from the calibration file',
      sum(grepl('kg_entity\\(', live_mod)) >= 3)

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

#-------------------------------------------------------------------------------
# 5. The conditioning set is now machine-readable, and it matches settings.yaml
#
# This is the part that has teeth: a calibration records the model-form switches
# it was derived under, and the parse-time check compares that record against the
# live settings. If these two ever disagree the run stops -- so they had better
# agree the moment the block is written.
#-------------------------------------------------------------------------------

settings_vals = lapply(new_settings, function(e) e$value)

for (nm in names(new_bathtub)) {
  cond = new_bathtub[[nm]]$conditioned_on
  check(sprintf('bathtub.%s records its conditioning set', nm),
        !is.null(cond) && length(cond) > 0)
  for (key in names(cond)) {
    parts = strsplit(key, '.', fixed = TRUE)[[1]]
    check(sprintf('bathtub.%s conditioned_on %s is well-formed', nm, key),
          length(parts) == 3 && parts[1] == 'settings' && parts[2] == 'kg')
    live = settings_vals[[parts[3]]]
    check(sprintf('bathtub.%s conditioned_on %s matches settings.yaml', nm, key),
          !is.null(live) && isTRUE(all.equal(live, cond[[key]])))
  }
}

cat(sprintf('\n%d passed, %d failed\n', n_pass, n_fail))
if (n_fail > 0) { cat('STAMP_VALUE_CHECK_FAIL\n'); quit(status = 1) }
cat('STAMP_VALUE_CHECK_PASS\n')
