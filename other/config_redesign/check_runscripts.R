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

# Interface metadata, replicated from parse_globals(). Interfaces always read
# from the production root regardless of where output goes.
interface_versions_raw = read_yaml('./config/interfaces/interface_versions.yaml')
output_roots           = read_yaml('./config/output_roots.yaml')
interface_meta = names(interface_versions_raw) %>%
  purrr::discard(~ .x == 'Tax-Simulator') %>%
  purrr::set_names(.) %>%
  purrr::map(~ list(
    key  = stringr::str_to_lower(stringr::str_replace_all(.x, '[ -]', '_')),
    root = file.path(output_roots$production, interface_versions_raw[[.x]]$type,
                     .x, paste0('v', interface_versions_raw[[.x]]$version))
  ))

# Does every interface directory this row names actually exist? parse_globals()
# checks this, but only once a run is under way -- and this gate deliberately
# stops short of parse_globals(), so a runscript naming a vintage that is not
# there resolves clean here and dies in SLURM Phase 0 instead.
#
# The failure is not hypothetical. An economy alternative can pin an interface
# VINTAGE but not its VERSION, which is repo-pinned in interface_versions.yaml as
# plumbing; a vintage lives UNDER a version, so bumping a version strands every
# alternative pinning an older vintage of that interface. Off-Model-Estimates
# going to v5 on 2026-07-22 stranded nine of them, and nothing said so.
missing_interfaces = function(eco) {
  purrr::keep(names(interface_meta), function(nm) {
    m = interface_meta[[nm]]
    v = eco$values$interfaces[[paste0(m$key, '_vintage')]]
    i = eco$values$interfaces[[paste0(m$key, '_id')]]
    if (is.null(v) || is.null(i)) return(TRUE)
    !dir.exists(file.path(m$root, as.character(v), as.character(i)))
  })
}

# Reported separately from the parse tally, and deliberately not fatal: the fix
# is a decision about the pins (regenerate the vintages under the new version, or
# retire the runscripts), not about the runscript files themselves. Folding it
# into the pass/fail count would leave this gate permanently red, which is how a
# check stops being read.
unreachable = list()

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

      gone = missing_interfaces(eco)
      if (length(gone) > 0) {
        # tryCatch() evaluates this block in the caller's frame, which is the
        # global environment, so a plain assignment lands where the tally is
        # read. (`<<-` here would skip globalenv and look for the name among the
        # attached packages.)
        unreachable[[f]] = sort(unique(c(unreachable[[f]], gone)))
      }

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

      # And the calibration files this scenario points at: are the values in them
      # still consistent with the data vintages, the code and the settings this
      # run would use?
      suppressMessages(suppressWarnings(calib_check_staleness(
        behavior_spec      = spec,
        interface_vintages = config_interface_vintages(eco),
        enforce            = CONFIG_ENFORCE_STALENESS)))
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

if (length(unreachable) > 0) {
  cat(sprintf(paste0('\n%d runscripts resolve but name an interface vintage that ',
                     'does not exist on disk.\nThey will die in SLURM Phase 0. ',
                     'See the note at the top of this file.\n'),
              length(unreachable)))
  for (f in names(unreachable)) {
    cat('  - ', f, ': ', paste(unreachable[[f]], collapse = ', '), '\n', sep = '')
  }
} else {
  cat('\nEvery interface directory named by a live runscript exists.\n')
}

cat('RUNSCRIPT_CHECK_DONE\n')
