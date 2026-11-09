#-------------------------------------------------------------------------------
# test_calibration_writer.R
#
# The writer that lets a calibrator end by writing its own entry
# (src/misc/calibration_writer.R). Two claims matter and both are easy to get
# wrong silently:
#
#   1. Everything it does not mean to change, it does not change. These files are
#      mostly comment and the comments are the provenance, so a writer that
#      reflows or drops them is worse than the hand-copying it replaces. The
#      round-trip test below is byte-exact.
#
#   2. A value that fails to reproduce does not land. The asymmetry between "in
#      place" and ".proposed" is the only thing standing between a re-run and a
#      silent estimate change, so it is tested in both directions.
#
# Run: sbatch other/config_redesign/run_tests.sbatch . other/config_redesign/test_calibration_writer.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
source('src/misc/calibration_writer.R')

pass = 0
fail = 0

check = function(label, ok, detail = '') {
  if (isTRUE(ok)) {
    pass <<- pass + 1
    cat(sprintf('PASS  %s\n', label))
  } else {
    fail <<- fail + 1
    cat(sprintf('FAIL  %s%s\n', label, if (nzchar(detail)) paste0(' -- ', detail) else ''))
  }
}

SHIPPED = 'config/calibrations/kg/bathtub.yaml'
tmp     = file.path(tempdir(), 'calib_writer_tests')
dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

#-------------------------------------------------------------------------------
# 1. Splitting and rejoining the real shipped file changes nothing at all.
#-------------------------------------------------------------------------------

text  = paste(readLines(SHIPPED, warn = FALSE), collapse = '\n')
parts = calib_split_blocks(text)

check('the real file splits into its five top-level entries',
      identical(parts$keys, c('_channel', 'eta', 'eta_logs',
                              'timeable_share', 'timeable_share_logs')),
      paste(parts$keys, collapse = ','))

rejoined = paste(c(parts$preamble, unlist(parts$blocks)), collapse = '\n')
check('split then rejoin is byte-exact', identical(rejoined, text))

#-------------------------------------------------------------------------------
# 2. Reproducing the pinned value writes in place, and touches only that entry.
#-------------------------------------------------------------------------------

target = file.path(tmp, 'bathtub.yaml')
invisible(file.copy(SHIPPED, target, overwrite = TRUE))

check('the current value is read back off the file',
      identical(calib_current_value(target, 'eta_logs'), '1.6625'),
      calib_current_value(target, 'eta_logs'))

fields = list(
  kind = 'calibrated',
  set  = '2026-07-26',
  target = calib_prose('A test target sentence, written long enough that the writer
                        has to wrap it across more than one line to prove that it
                        wraps prose the way the hand-written entries next to it are
                        wrapped.'),
  derived_under  = list(tax_data = '2026070814', macro_projections = '2026022522'),
  invalidated_by = c('src/sim/kg/constants.R', 'src/sim/kg/timing.R'),
  conditioned_on = list(settings.kg.applier_allocation = '0.5',
                        settings.kg.timing_window      = 1),
  rederive       = 'other/kg_model_tests/form_ab/measure_efull_logs.R',
  active_when    = list(kg.response_form = 'logs'),
  note           = calib_prose('A test note.'))

before = readLines(target, warn = FALSE)
out    = calib_write_entry(target, 'eta_logs', 1.6625, fields)
after  = readLines(target, warn = FALSE)

check('a reproduced value is written in place', identical(out, target))
check('no .proposed file is left behind', !file.exists(paste0(target, '.proposed')))
check('the value survives the rewrite',
      identical(calib_current_value(target, 'eta_logs'), '1.6625'))

# The other entries, and the file's preamble, must be untouched.
p_before = calib_split_blocks(paste(before, collapse = '\n'))
p_after  = calib_split_blocks(paste(after,  collapse = '\n'))
untouched = setdiff(p_before$keys, 'eta_logs')
same = map_lgl(untouched, function(k)
  identical(p_before$blocks[[match(k, p_before$keys)]],
            p_after$blocks[[match(k, p_after$keys)]]))

check('every other entry is byte-identical', all(same),
      paste(untouched[!same], collapse = ','))
check('the file preamble is byte-identical',
      identical(p_before$preamble, p_after$preamble))
check('the entry keeps its position in the file', identical(p_before$keys, p_after$keys))

#-------------------------------------------------------------------------------
# 3. The generated block is well-formed YAML that says what it was told to.
#-------------------------------------------------------------------------------

reread = yaml::read_yaml(target)
e      = reread$eta_logs

check('the rewritten file still parses as YAML', is.list(reread))
check('the value round-trips as a number', isTRUE(all.equal(e$value, 1.6625)))
check('the kind round-trips', identical(e$kind, 'calibrated'))
check('a vintage stays a string, not an integer',
      identical(e$derived_under$tax_data, '2026070814'))
check('a numeric condition stays numeric',
      isTRUE(all.equal(e$conditioned_on$settings.kg.timing_window, 1)))
check('a string-valued condition keeps its quotes',
      identical(e$conditioned_on$settings.kg.applier_allocation, '0.5'))
check('the dependency list round-trips',
      identical(unlist(e$invalidated_by),
                c('src/sim/kg/constants.R', 'src/sim/kg/timing.R')))
check('active_when round-trips', identical(e$active_when$kg.response_form, 'logs'))
check('prose fields come back as one wrapped string',
      length(e$note) == 1 && grepl('A test note', e$note))

#-------------------------------------------------------------------------------
# 4. Hashes are computed from the files, at write time.
#-------------------------------------------------------------------------------

live = unname(tools::md5sum('src/sim/kg/constants.R'))
check('the hash written is the hash of the file on disk',
      identical(e$invalidated_by_hashes[['src/sim/kg/constants.R']], live),
      paste(e$invalidated_by_hashes[['src/sim/kg/constants.R']], 'vs', live))
check('the hash block sits directly after the list it describes',
      match('invalidated_by_hashes', names(e)) == match('invalidated_by', names(e)) + 1)

#-------------------------------------------------------------------------------
# 5. A value that does NOT reproduce goes to .proposed, and the file is untouched.
#-------------------------------------------------------------------------------

target2 = file.path(tmp, 'drift.yaml')
invisible(file.copy(SHIPPED, target2, overwrite = TRUE))
snapshot = readLines(target2, warn = FALSE)

out2 = calib_write_entry(target2, 'eta_logs', 1.8100, fields)

check('a drifted value goes to .proposed',
      identical(out2, paste0(target2, '.proposed')))
check('the shipped file is left exactly as it was',
      identical(readLines(target2, warn = FALSE), snapshot))
check('the proposed file carries the new value',
      identical(calib_current_value(paste0(target2, '.proposed'), 'eta_logs'), '1.81'))

#-------------------------------------------------------------------------------
# 6. Tolerance, and the refusals.
#-------------------------------------------------------------------------------

target3 = file.path(tmp, 'tol.yaml')
invisible(file.copy(SHIPPED, target3, overwrite = TRUE))
out3 = calib_write_entry(target3, 'eta_logs', 1.6626, fields, tol = 1e-3)
check('a value inside tolerance counts as reproduced', identical(out3, target3))

invisible(file.copy(SHIPPED, target3, overwrite = TRUE))
out4 = calib_write_entry(target3, 'eta_logs', 1.6626, fields, tol = 0)
check('the same value at tol = 0 does not', identical(out4, paste0(target3, '.proposed')))

check('an entry that does not exist is refused',
      inherits(try(calib_write_entry(target3, 'not_an_entry', 1, fields),
                   silent = TRUE), 'try-error'))
check('a file that does not exist is refused',
      inherits(try(calib_write_entry(file.path(tmp, 'nope.yaml'), 'eta_logs', 1, fields),
                   silent = TRUE), 'try-error'))
check('a dependency that does not exist is refused',
      inherits(try(calib_hash_files('src/sim/kg/does_not_exist.R'), silent = TRUE),
               'try-error'))

#-------------------------------------------------------------------------------

cat(sprintf('\n%d passed, %d failed\n', pass, fail))
if (fail == 0) cat('CALIB_WRITER_TESTS_PASS\n') else quit(status = 1)
