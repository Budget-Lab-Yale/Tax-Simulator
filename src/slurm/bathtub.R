#-----------------------------------------------------------------------
# bathtub.R
#
# Phase 2B worker. Runs the kg_dynamics bathtub pre-pass for a single
# counterfactual scenario: aggregates baseline cells from Tax-Data, builds
# tau lists from baseline and reform tax law, runs the year-by-year
# recurrence, and writes per-year state files under the scenario's
# conventional/supplemental/kg_dynamics_state/.
#
# Income-conversion scenarios need one thing from upstream: the marginal
# rate frames the mechanical rung priced, reform law and baseline law, which
# is what the conversion wedge is measured from and what the behavior module
# on the conventional pass reads. Phase 2M writes them per year and this
# phase gathers them. Everything else run_bathtub_pass() builds itself, from
# the raw Tax-Data pool legs and the static detail Phase 2A wrote.
#
# CLI args:
#   Rscript src/slurm/bathtub.R <staging_dir>
#
# Environment:
#   SLURM_ARRAY_TASK_ID maps to a counterfactual scenario via manifest.rds
#-----------------------------------------------------------------------


args = commandArgs(trailingOnly = T)
if (length(args) < 1) {
  stop('Usage: Rscript src/slurm/bathtub.R <staging_dir>')
}

staging_dir = args[1]

tryCatch({

  # Reconstitute environment (packages, source files, globals, return_vars)
  source('./src/slurm/common.R')
  runtime_args = reconstitute_environment(staging_dir)

  # Map SLURM_ARRAY_TASK_ID to a CF scenario (year is NA for Phase 2B)
  task = get_task(staging_dir, '2B')

  cat(paste0('Phase 2B: running bathtub for scenario=', task$scenario, '\n'))

  # Load scenario config
  config        = readRDS(file.path(staging_dir, task$scenario, 'config.rds'))
  scenario_info = config$scenario_info

  # Install this scenario's resolved configuration. A SLURM worker is a fresh R
  # process that never runs do_scenario, so without this every economy_param()
  # read errors. See src/misc/scenario_config.R.
  config_activate(economy  = scenario_info$resolved_economy,
                  behavior = scenario_info$resolved_behavior)

  if (!scenario_uses_kg_dynamics(scenario_info)) {
    cat(paste0('Phase 2B: scenario=', task$scenario,
               ' does not use kg_dynamics; skipping.\n'))
    quit(status = 0)
  }

  # Gather the mechanical rung's rate frames, which Phase 2M wrote one file per
  # year. Absent for a scenario with no live transmission channel, and then the
  # mechanical frame is the static frame and the pre-pass reads static detail.
  mech_mtrs = NULL
  mech_files = file.path(staging_dir, task$scenario,
                         paste0('year_', scenario_info$years, '_mech.rds'))
  if (all(file.exists(mech_files))) {
    parts = lapply(mech_files, readRDS)
    mech_mtrs = list(
      reform   = bind_rows(lapply(parts, `[[`, 'pass_mtrs')),
      baseline = bind_rows(lapply(parts, `[[`, 'mtrs_baseline_law'))
    )
    if (nrow(mech_mtrs$reform) == 0 || nrow(mech_mtrs$baseline) == 0) {
      stop('Phase 2B: the mechanical rung wrote no marginal rates for scenario ',
           task$scenario, '. A scenario reaching this phase with mechanical ',
           'files present must have priced them.')
    }
    cat(paste0('Phase 2B: read mechanical rung rate frames for ',
               length(mech_files), ' year(s).\n'))
  } else if (any(file.exists(mech_files))) {
    stop('Phase 2B: scenario ', task$scenario, ' has mechanical rate files for ',
         sum(file.exists(mech_files)), ' of ', length(mech_files), ' years. ',
         'Either every year ran the mechanical rung or none did.')
  }

  # Run the bathtub pre-pass (defined in src/sim/run.R)
  run_bathtub_pass(scenario_info, config$tax_law,
                   vat_price_offset = config$vat_price_offset,
                   mech_mtrs        = mech_mtrs)

  cat(paste0('Phase 2B: completed bathtub for scenario=', task$scenario, '\n'))

}, error = function(e) {
  message(paste0('ERROR in bathtub worker: ', conditionMessage(e)))
  quit(status = 1)
})
