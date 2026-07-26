#-------------------------------------------------------------------------------
# check_runscripts.R
#
# Cheap parse gate for the runscript library: for every CSV under
# config/runscripts/ (archive/ excluded), confirm the eight-column schema holds
# and that each row's economy leg resolves and passes the staleness check. Stops
# short of parse_globals(), which reads Tax-Data and creates output trees -- the
# things that actually break on a bad migration are the schema check and the
# resolution, and both are cheap.
#
# Run via sbatch (never the login node):
#   sbatch other/config_redesign/run_tests.sbatch . other/config_redesign/check_runscripts.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)
# A couple of src files reference globals at source time (see
# other/config_redesign/parse_check.sbatch, which declares the same shim).
return_vars = list()

invisible(lapply(
  list.files('./src', pattern = '[.]R$', recursive = TRUE) %>%
    purrr::keep(~ .x != 'main.R' & !startsWith(.x, 'slurm/') & !startsWith(.x, 'tests/')),
  function(f) tryCatch(source(file.path('./src', f)),
                       error = function(e)
                         cat('note: could not source', f, '--',
                             conditionMessage(e), '\n'))))

economy_defaults = config_load_defaults('economy')

# archive/ is frozen on the old schema on purpose, and private/ is untracked
# one-off work that the rebuild migrates on demand rather than up front (see
# other/config_redesign/REBUILD_STATUS.md). Neither is expected to pass, and a
# check that always reports failures is a check nobody reads.
files = list.files('./config/runscripts', pattern = '[.]csv$', recursive = TRUE) %>%
  purrr::discard(~ startsWith(.x, 'archive/') || startsWith(.x, 'private/'))

n_ok = 0; n_bad = 0; bad_names = c()

for (f in files) {
  path = file.path('./config/runscripts', f)
  res = tryCatch({
    rs = readr::read_csv(path, show_col_types = FALSE,
                         col_types = readr::cols(.default = 'c'))
    validate_runscript_columns(rs, f)
    for (i in seq_len(nrow(rs))) {
      eco = config_resolve('economy', economy_defaults, alternative = rs$economy[i])
      suppressMessages(config_check_staleness(
        leg                = 'economy',
        defaults           = economy_defaults,
        resolved           = eco,
        interface_vintages = config_interface_vintages(eco),
        cross_values       = list(economy = eco$values),
        enforce            = CONFIG_ENFORCE_STALENESS))

      # The behavior leg: does the folder exist, do its module files exist, and
      # is the stack a shape the model can run? This is what the deleted
      # in-module order guards used to catch, one scenario at a time, mid-run.
      behavior = if ('behavior' %in% names(rs)) rs$behavior[i] else NA_character_
      spec = behavior_resolve(behavior)
      suppressWarnings(behavior_validate_spec(spec, rs$ID[i]))
    }
    'ok'
  }, error = function(e) conditionMessage(e))

  if (identical(res, 'ok')) {
    n_ok = n_ok + 1
  } else {
    n_bad = n_bad + 1
    bad_names = c(bad_names, f)
    cat('\n=== FAIL ', f, '\n', res, '\n', sep = '')
  }
}

cat(sprintf('\n%d runscripts parse and resolve; %d do not\n', n_ok, n_bad))
if (n_bad > 0) {
  cat('Failing:\n  - ', paste(bad_names, collapse = '\n  - '), '\n', sep = '')
}
cat('RUNSCRIPT_CHECK_DONE\n')
