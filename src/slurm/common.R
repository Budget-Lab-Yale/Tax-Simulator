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
  # Mirror main.R / setup.R exactly: skip the entry point, the slurm drivers,
  # src/tests/ (test files, one of which runs assertions at source time), and
  # src/behavior/ (modules are loaded by path at scenario time, and sourcing
  # them all would leave the last variant of a family defining do_{family}).
  # Keeping this predicate in lockstep with main.R is the 'workers operate
  # identically to main.R' contract (see CLAUDE.md).
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

  # Seed the RNG stream. main.R inherits the seed set inside parse_globals();
  # a worker is a fresh R process that never runs it, so without this any
  # bare-RNG code (e.g. a behavior module that skips the set.seed convention)
  # draws from a time/PID-initialized stream -- non-reproducible run to run
  # and divergent from main.R. Older globals.rds lack the field; fall back.
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
  # built in Phase 0. Phase 1, 2A, 2C tasks have a year; Phase 2B tasks have
  # year = NA (one job per scenario, all years processed sequentially within
  # the job).
  #
  # Parameters:
  #   - staging_dir (str) : path to _slurm_staging directory
  #   - phase (str)       : pipeline phase ('1', '2A', '2B', or '2C')
  #
  # Returns: list with $scenario (str) and $year (int or NA)
  #--------------------------------------------------------------------------

  task_id  = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
  manifest = readRDS(file.path(staging_dir, 'manifest.rds'))

  # Filter to current phase (string compare) and index by 1-based task ID
  phase_tasks = manifest %>%
    filter(phase == !!as.character(phase))

  if (task_id < 1 || task_id > nrow(phase_tasks)) {
    stop(paste0('SLURM_ARRAY_TASK_ID ', task_id,
                ' out of range for phase ', phase,
                ' (', nrow(phase_tasks), ' tasks)'))
  }

  row = phase_tasks[task_id, ]
  return(list(scenario = row$scenario, year = row$year))
}
