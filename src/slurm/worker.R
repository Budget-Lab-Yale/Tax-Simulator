#-----------------------------------------------------------------------
# worker.R
#
# Phase 1, 2A, and 2C workers. Phase 2B has its own driver (bathtub.R).
# Each SLURM array task calls run_one_year() for a single (scenario, year)
# combination with the appropriate pass_type:
#
#   Phase 1   : baseline year, pass_type = 'both' (no behavior modules)
#   Phase 2A  : cf year, pass_type = 'static'      (static + MTRs only)
#   Phase 2C  : cf year, pass_type = 'conventional' (reads precomputed
#               static MTRs from year_{y}_static.rds and bathtub state
#               from kg_dynamics_state/{y}.rds for kg_dynamics scenarios)
#
# Per-year .rds files use phase-specific suffixes so Phase 3a aggregation
# can read both static and conventional outputs:
#
#   Phase 1   : year_{y}.rds        (legacy name; baseline static + null conv)
#   Phase 2A  : year_{y}_static.rds (mtrs + static_totals)
#   Phase 2C  : year_{y}_conv.rds   (conventional_totals)
#
# CLI args:
#   Rscript src/slurm/worker.R <staging_dir> <phase>
#
# Environment:
#   SLURM_ARRAY_TASK_ID maps to (scenario, year) via manifest.rds
#-----------------------------------------------------------------------


args = commandArgs(trailingOnly = T)
if (length(args) < 2) {
  stop('Usage: Rscript src/slurm/worker.R <staging_dir> <phase>')
}

staging_dir = args[1]
phase       = args[2]

tryCatch({

  # Reconstitute environment (packages, source files, globals, return_vars)
  source('./src/slurm/common.R')
  runtime_args = reconstitute_environment(staging_dir)

  # Map SLURM_ARRAY_TASK_ID to (scenario, year)
  task = get_task(staging_dir, phase)

  cat(paste0('Phase ', phase, ': running scenario=', task$scenario,
             ' year=', task$year, '\n'))

  # Load scenario config
  config = readRDS(file.path(staging_dir, task$scenario, 'config.rds'))

  # Load baseline MTRs (needed for the behavior modules on the conventional and
  # conv-no-wealth passes)
  baseline_mtrs = NULL
  if (phase %in% c('2C', '2N')) {
    prebuilt_path = file.path(staging_dir, 'baseline', 'baseline_mtrs.rds')
    if (file.exists(prebuilt_path)) {
      baseline_mtrs = readRDS(prebuilt_path)
    } else {
      year_files = list.files(
        file.path(staging_dir, 'baseline'),
        pattern = '^year_.*\\.rds$',
        full.names = T
      )
      if (length(year_files) == 0) {
        stop('No baseline year results found and no prebuilt baseline_mtrs.rds')
      }
      baseline_mtrs = year_files %>%
        map(~ readRDS(.x)$mtrs) %>%
        bind_rows()
    }
  }

  # For Phase 2C/2N: load this scenario's static_mtrs from Phase 2A's per-year
  # .rds. Every 2C/2N task has a matching 2A task that wrote this file, so a
  # missing file means a broken/partial staging dir (e.g. a manual phase
  # re-run after cleanup) -- fail here rather than silently handing behavior
  # modules static_mtrs = NULL (main.R always threads the in-memory MTRs)
  static_mtrs_year = NULL
  if (phase %in% c('2C', '2N')) {
    static_rds = file.path(staging_dir, task$scenario,
                           paste0('year_', task$year, '_static.rds'))
    if (!file.exists(static_rds)) {
      stop('Phase ', phase, ' task (', task$scenario, ', ', task$year,
           ') found no static-pass results at ', static_rds,
           ' -- Phase 2A output is missing; re-run the pipeline from setup')
    }
    static_mtrs_year = readRDS(static_rds)$mtrs
  }

  # Map phase to pass_type
  pass_type = switch(phase,
    '1'  = 'both',
    '2A' = 'static',
    '2N' = 'conventional_no_wealth',
    '2C' = 'conventional',
    stop('Unknown phase: ', phase)
  )

  # Run simulation for this (scenario, year)
  result = run_one_year(
    year                 = task$year,
    scenario_info        = config$scenario_info,
    tax_law              = config$tax_law,
    baseline_mtrs        = baseline_mtrs,
    indexes              = config$indexes,
    vat_price_offset     = config$vat_price_offset,
    excess_growth_offset = config$excess_growth_offset,
    pass_type            = pass_type,
    static_mtrs_year     = static_mtrs_year
  )

  # Save result with phase-appropriate filename. The conv-no-wealth (2N) result
  # carries no totals (intermediate pass; its detail is consumed by the wealth
  # pre-pass 2W from disk) -- saved for symmetry, not read by Phase 3a.
  out_path = switch(phase,
    '1'  = paste0('year_', task$year, '.rds'),
    '2A' = paste0('year_', task$year, '_static.rds'),
    '2N' = paste0('year_', task$year, '_convnw.rds'),
    '2C' = paste0('year_', task$year, '_conv.rds')
  )
  saveRDS(result, file.path(staging_dir, task$scenario, out_path))

  cat(paste0('Phase ', phase, ': completed scenario=', task$scenario,
             ' year=', task$year, '\n'))

}, error = function(e) {
  message(paste0('ERROR in worker (phase=', phase, '): ', e$message))
  quit(status = 1)
})
