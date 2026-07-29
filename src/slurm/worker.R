#-----------------------------------------------------------------------
# worker.R
#
# Phase 1, 2A, 2MN, 2M, 2N, and 2C workers. Phases 2B, 2MW and 2W have their own
# drivers (bathtub.R, mech_wealth.R and wealth.R). Each SLURM array task calls
# run_one_year() for a single scenario and year, with the phase's pass_type:
#
#   Phase 1   : baseline year, pass_type = 'both'
#   Phase 2A  : counterfactual year, pass_type = 'static'
#   Phase 2MN : counterfactual year, pass_type = 'mechanical_no_wealth'
#   Phase 2M  : counterfactual year, pass_type = 'mechanical'
#   Phase 2N  : counterfactual year, pass_type = 'conventional_no_wealth'
#   Phase 2C  : counterfactual year, pass_type = 'conventional'
#
# Per-year .rds files carry phase-specific suffixes so Phase 3a can read each
# rung's outputs separately:
#
#   Phase 1   : year_{y}.rds        (baseline static, null conventional)
#   Phase 2A  : year_{y}_static.rds (MTRs and static totals)
#   Phase 2MN : year_{y}_mechnw.rds (no totals, detail read by Phase 2MW)
#   Phase 2M  : year_{y}_mech.rds   (mechanical totals)
#   Phase 2N  : year_{y}_convnw.rds (no totals, detail read by Phase 2W)
#   Phase 2C  : year_{y}_conv.rds   (conventional totals)
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

  # Install this scenario's resolved configuration. A SLURM worker is a fresh R
  # process that never runs do_scenario, so without this every economy_param()
  # read errors. See src/misc/scenario_config.R.
  config_activate(economy  = config$scenario_info$resolved_economy,
                  behavior = config$scenario_info$resolved_behavior)

  # Load the two MTR sets the behavior modules difference. Where the mechanical
  # rung ran, both come from its frame: reform law on one side, baseline law on the
  # other. Where it did not, the mechanical frame is the static frame, so the
  # baseline scenario's MTRs and the Phase 2A static ones stand in unchanged.
  mech_mtr_path = NULL
  if (phase %in% c('2C', '2N')) {
    mech_mtr_path = file.path(staging_dir, task$scenario,
                              paste0('year_', task$year, '_mech.rds'))
    if (!file.exists(mech_mtr_path)) mech_mtr_path = NULL
  }

  baseline_mtrs = NULL
  if (phase %in% c('2C', '2N') && !is.null(mech_mtr_path)) {
    baseline_mtrs = readRDS(mech_mtr_path)$mtrs_baseline_law
  }
  if (phase %in% c('2C', '2N') && is.null(baseline_mtrs)) {
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

  # Read this scenario's static MTRs from the Phase 2A per-year file. Every 2C
  # and 2N task has a matching 2A task that wrote it, so a missing file means a
  # partial staging directory -- stop rather than hand the behavior modules a
  # null set of static MTRs
  # Phase 2M reads them too, not for a behavior module -- it runs none -- but so
  # the crossing diagnostic can difference the two frames
  static_mtrs_year = NULL
  if (phase %in% c('2C', '2N') && !is.null(mech_mtr_path)) {
    static_mtrs_year = readRDS(mech_mtr_path)$pass_mtrs
  }
  if (phase %in% c('2C', '2N', '2M') && is.null(static_mtrs_year)) {
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
    '1'   = 'both',
    '2A'  = 'static',
    '2MN' = 'mechanical_no_wealth',
    '2M'  = 'mechanical',
    '2N'  = 'conventional_no_wealth',
    '2C'  = 'conventional',
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
    pass_type            = pass_type,
    static_mtrs_year     = static_mtrs_year,
    tax_law_baseline     = config$tax_law_baseline
  )

  # Save result under the phase's filename. The conv-no-wealth result carries no
  # totals: Phase 2W reads its detail from disk instead.
  out_path = switch(phase,
    '1'   = paste0('year_', task$year, '.rds'),
    '2A'  = paste0('year_', task$year, '_static.rds'),
    '2MN' = paste0('year_', task$year, '_mechnw.rds'),
    '2M'  = paste0('year_', task$year, '_mech.rds'),
    '2N'  = paste0('year_', task$year, '_convnw.rds'),
    '2C'  = paste0('year_', task$year, '_conv.rds')
  )
  saveRDS(result, file.path(staging_dir, task$scenario, out_path))

  cat(paste0('Phase ', phase, ': completed scenario=', task$scenario,
             ' year=', task$year, '\n'))

}, error = function(e) {
  message(paste0('ERROR in worker (phase=', phase, '): ', e$message))
  quit(status = 1)
})
