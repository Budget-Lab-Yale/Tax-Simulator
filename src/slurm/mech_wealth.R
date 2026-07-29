#-----------------------------------------------------------------------
# mech_wealth.R
#
# Phase 2MW worker. Runs the wealth bathtub pre-pass for the mechanical
# rung of a single counterfactual scenario: reads the mech-no-wealth
# detail from Phase 2MN and the baseline static detail from Phase 1,
# assigns age-percentile cells, builds the forcing with behavior off, and
# runs the per-living-record deficit recurrence across years. Writes the
# per-year deficit state under the scenario's
# mechanical/supplemental/wealth_dynamics_state/, which the Phase 2M
# workers apply as the wealth haircut.
#
# The recurrence depends on year t-1, so years run sequentially. Runs
# after Phases 2MN and 1 and before Phase 2M, one job per scenario.
#
# CLI args:
#   Rscript src/slurm/mech_wealth.R <staging_dir>
#
# Environment:
#   SLURM_ARRAY_TASK_ID maps to a counterfactual scenario via manifest.rds
#-----------------------------------------------------------------------


args = commandArgs(trailingOnly = T)
if (length(args) < 1) {
  stop('Usage: Rscript src/slurm/mech_wealth.R <staging_dir>')
}

staging_dir = args[1]

tryCatch({

  # Reconstitute environment (packages, source files, globals, return_vars)
  source('./src/slurm/common.R')
  runtime_args = reconstitute_environment(staging_dir)

  # Map SLURM_ARRAY_TASK_ID to a CF scenario (year is NA for Phase 2MW)
  task = get_task(staging_dir, '2MW')

  cat(paste0('Phase 2MW: running mechanical wealth bathtub pre-pass for scenario=',
             task$scenario, '\n'))

  # Load scenario config
  config        = readRDS(file.path(staging_dir, task$scenario, 'config.rds'))
  scenario_info = config$scenario_info

  # Install this scenario's resolved configuration. A SLURM worker is a fresh R
  # process that never runs do_scenario, so without this every economy_param()
  # read errors. See src/misc/scenario_config.R.
  config_activate(economy  = scenario_info$resolved_economy,
                  behavior = scenario_info$resolved_behavior)

  if (!scenario_uses_wealth_dynamics(scenario_info)) {
    cat(paste0('Phase 2MW: scenario=', task$scenario,
               ' does not activate the wealth channel (s = 0); skipping.\n'))
    quit(status = 0)
  }

  # Run the wealth bathtub pre-pass for the mechanical rung (defined in
  # src/sim/wealth_dynamics.R)
  run_wealth_bathtub_pass(scenario_info, config$tax_law,
                          vat_price_offset = config$vat_price_offset,
                          leg              = 'mechanical')

  cat(paste0('Phase 2MW: completed mechanical wealth bathtub pass for scenario=',
             task$scenario, '\n'))

}, error = function(e) {
  message(paste0('ERROR in mechanical wealth worker: ', e$message))
  quit(status = 1)
})
