#-----------------------------------------------------------------------
# worker.R
#
# Phases 1 and 2 of SLURM pipeline: each SLURM array task calls
# run_one_year() for a single (scenario, year) combination. Phase 1
# handles baseline years; Phase 2 handles counterfactual years.
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
phase       = as.integer(args[2])

# Wrap in tryCatch so SLURM gets a nonzero exit code on failure
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

  # Load baseline MTRs
  baseline_mtrs = NULL
  if (phase == 2) {
    # Check for pre-assembled baseline MTRs (baseline_vintage case)
    prebuilt_path = file.path(staging_dir, 'baseline', 'baseline_mtrs.rds')
    if (file.exists(prebuilt_path)) {
      baseline_mtrs = readRDS(prebuilt_path)
    } else {
      # Assemble from Phase 1 per-year result files
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

  # Run simulation for this (scenario, year)
  result = run_one_year(
    year                 = task$year,
    scenario_info        = config$scenario_info,
    tax_law              = config$tax_law,
    baseline_mtrs        = baseline_mtrs,
    indexes              = config$indexes,
    vat_price_offset     = config$vat_price_offset,
    excess_growth_offset = config$excess_growth_offset
  )

  # Save result
  saveRDS(result, file.path(staging_dir, task$scenario,
                            paste0('year_', task$year, '.rds')))

  cat(paste0('Phase ', phase, ': completed scenario=', task$scenario,
             ' year=', task$year, '\n'))

}, error = function(e) {
  message(paste0('ERROR in worker (phase=', phase, '): ', e$message))
  quit(status = 1)
})
