#-----------------------------------------------------------------------
# common.R
#
# Shared utilities for SLURM-based parallelization. Provides functions
# to reconstitute the R environment from serialized state and to map
# SLURM array task IDs to (scenario, year) work units.
#-----------------------------------------------------------------------



reconstitute_environment = function(staging_dir) {

  #--------------------------------------------------------------------------
  # Reconstitutes the full R environment from serialized .rds files so that
  # SLURM worker and aggregation scripts operate identically to main.R.
  #
  # Parameters:
  #   - staging_dir (str) : path to _slurm_staging directory
  #
  # Returns: list of runtime args (stacked, delete_detail)
  #--------------------------------------------------------------------------

  # Load required packages (same as main.R)
  suppressPackageStartupMessages(
    invisible(capture.output(
      lapply(readLines('./requirements.txt'), library, character.only = T)
    ))
  )

  # Source all function scripts (defines functions + populates return_vars).
  # Skip the entry point, the slurm drivers, src/tests/ (one test file runs
  # assertions at source time), and src/behavior/ (modules are loaded by path at
  # scenario time, and sourcing them all would leave whichever variant came last
  # defining do_{family}). Keep this predicate in lockstep with main.R and
  # setup.R.
  return_vars <<- list()
  list.files('./src', recursive = T) %>%
    walk(.f = ~ {
      if (.x != 'main.R' && !startsWith(.x, 'slurm/') &&
          !startsWith(.x, 'tests/') && !startsWith(.x, 'behavior/')) {
        source(file.path('./src/', .x))
      }
    })

  # Overwrite globals and return_vars with exact Phase 0 state
  globals      <<- readRDS(file.path(staging_dir, 'globals.rds'))
  return_vars  <<- readRDS(file.path(staging_dir, 'return_vars.rds'))

  # Seed the RNG stream. main.R inherits the seed set inside parse_globals, but a
  # worker is a fresh R process that never runs it, so without this any code
  # drawing without its own set.seed call diverges from main.R. Older
  # globals.rds files lack the field, so fall back to the default seed.
  set.seed(globals$random_seed %||% 76)

  # Assign counterfactual_ids to global env (needed by calc_rev_est,
  # build_1040_report, and other post-processing functions as free variable)
  counterfactual_ids <<- readRDS(file.path(staging_dir, 'counterfactual_ids.rds'))

  # Return runtime args
  runtime_args = readRDS(file.path(staging_dir, 'runtime_args.rds'))
  return(runtime_args)
}



get_task = function(staging_dir, phase) {

  #--------------------------------------------------------------------------
  # Maps the current SLURM_ARRAY_TASK_ID to a work unit based on the manifest
  # built in Phase 0. A per-year phase carries the consecutive years its task
  # runs, one or several depending on the batch size Phase 0 was given. The
  # pre-pass phases 1B, 2B, 2MW and 2W carry a single NA, one job per scenario
  # running all years in sequence.
  #
  # Parameters:
  #   - staging_dir (str) : path to _slurm_staging directory
  #   - phase (str)       : pipeline phase, e.g. '1', '2A', '2B'
  #
  # Returns: list with $scenario (str) and $years (int[], or a single NA)
  #--------------------------------------------------------------------------

  task_id  = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
  manifest = readRDS(file.path(staging_dir, 'manifest.rds'))

  # Filter to current phase and index by 1-based task ID
  phase_tasks = manifest %>%
    filter(phase == !!as.character(phase))

  if (task_id < 1 || task_id > nrow(phase_tasks)) {
    stop(paste0('SLURM_ARRAY_TASK_ID ', task_id,
                ' out of range for phase ', phase,
                ' (', nrow(phase_tasks), ' tasks)'))
  }

  row = phase_tasks[task_id, ]
  return(list(scenario = row$scenario, years = row$years[[1]]))
}



report_driver_error = function(e, where, staging_dir = NULL) {

  #--------------------------------------------------------------------------
  # Reports a driver failure and stops with a nonzero status.
  #
  # The message goes to three places, because twice on 2026-07-30 a phase exited
  # nonzero with nothing in its log: an empty condition message from an rlang
  # error, and then a message that never reached the log at all, the array log
  # ending mid-stream. A phase that fails without saying why costs hours. So the
  # text is flushed to both streams and, where a staging directory is known,
  # written to its own small file under logs/errors/, which survives a truncated
  # write to the shared array log.
  #
  # Parameters:
  #   - e (condition)     : the caught error
  #   - where (str)       : which driver and phase failed
  #   - staging_dir (str) : staging directory, for the sidecar file
  #
  # Returns: never; quits with status 1.
  #--------------------------------------------------------------------------

  task_id = Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA')
  header  = paste0('ERROR in ', where, ' (task ', task_id, '): ',
                   conditionMessage(e))

  # The failing call and the condition's class, not a traceback: by the time a
  # tryCatch handler runs the stack has unwound, so traceback() would show this
  # function rather than the code that failed. The class matters because an rlang
  # or vctrs condition often carries the only clue to which layer threw.
  call_txt = if (is.null(conditionCall(e))) '(no call recorded)' else
               paste(deparse(conditionCall(e)), collapse = ' ')
  body = paste0(header, '\n',
                '  failing call : ', call_txt, '\n',
                '  condition    : ', paste(class(e), collapse = ', '), '\n')

  message(body)
  cat(body)
  flush(stderr())
  flush(stdout())

  if (!is.null(staging_dir)) {
    dir.create(file.path(staging_dir, 'logs', 'errors'),
               recursive = TRUE, showWarnings = FALSE)
    writeLines(body,
               file.path(staging_dir, 'logs', 'errors',
                         paste0(gsub('[^A-Za-z0-9]+', '_', where), '_',
                                task_id, '.txt')))
  }

  quit(status = 1)
}
