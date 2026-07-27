#-----------------------------------------------------------------------
# bathtub.R
#
# Phase 2B worker. Runs the kg_dynamics bathtub pre-pass for a single
# counterfactual scenario: aggregates baseline cells from Tax-Data, builds
# tau lists from baseline and reform tax law, runs the year-by-year
# recurrence, and writes per-year state files under the scenario's
# conventional/supplemental/kg_dynamics_state/.
#
# Income-conversion scenarios need no extra handling here.
# run_bathtub_pass() builds the conversion context itself from the raw
# Tax-Data pool legs and the baseline and scenario static detail MTRs, all
# of which Phase 2A has already written.
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

  # Run the bathtub pre-pass (defined in src/sim/run.R)
  run_bathtub_pass(scenario_info, config$tax_law,
                   vat_price_offset = config$vat_price_offset)

  cat(paste0('Phase 2B: completed bathtub for scenario=', task$scenario, '\n'))

}, error = function(e) {
  message(paste0('ERROR in bathtub worker: ', e$message))
  quit(status = 1)
})
