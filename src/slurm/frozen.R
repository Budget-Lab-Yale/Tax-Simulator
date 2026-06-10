#-----------------------------------------------------------------------
# frozen.R — Phase 1B worker
#
# Runs the kg_dynamics frozen mechanical pre-pass for a single
# counterfactual scenario. Aggregates baseline cells from Tax-Data, runs
# the frozen-realization recurrence (r_S = r_B, no Bellman), and persists
# per-year mechanical state files under the scenario's
# static/supplemental/kg_dynamics_mech_state/ directory, plus the
# inputs cache reused by Phase 2B's bathtub pass.
#
# Must complete before Phase 2A (the static workers inject the mechanical
# state into records). Needs only Tax-Data and the staged tax law, so it
# runs in parallel with Phase 1.
#
# No-op for scenarios that don't include any kg_dynamics/ behavior module.
#
# CLI args:
#   Rscript src/slurm/frozen.R <staging_dir>
#
# Environment:
#   SLURM_ARRAY_TASK_ID maps to a counterfactual scenario via manifest.rds
#-----------------------------------------------------------------------


args = commandArgs(trailingOnly = T)
if (length(args) < 1) {
  stop('Usage: Rscript src/slurm/frozen.R <staging_dir>')
}

staging_dir = args[1]

tryCatch({

  # Reconstitute environment (packages, source files, globals, return_vars)
  source('./src/slurm/common.R')
  runtime_args = reconstitute_environment(staging_dir)

  # Map SLURM_ARRAY_TASK_ID to a CF scenario (year is NA for Phase 1B)
  task = get_task(staging_dir, '1B')

  cat(paste0('Phase 1B: running frozen mechanical pass for scenario=',
             task$scenario, '\n'))

  # Load scenario config
  config        = readRDS(file.path(staging_dir, task$scenario, 'config.rds'))
  scenario_info = config$scenario_info

  if (!scenario_uses_kg_dynamics(scenario_info)) {
    cat(paste0('Phase 1B: scenario=', task$scenario,
               ' does not use kg_dynamics; skipping.\n'))
    quit(status = 0)
  }

  # Run the frozen pre-pass (defined in src/sim/run.R)
  run_frozen_pass(scenario_info, config$tax_law,
                  vat_price_offset     = config$vat_price_offset,
                  excess_growth_offset = config$excess_growth_offset)

  cat(paste0('Phase 1B: completed frozen pass for scenario=',
             task$scenario, '\n'))

}, error = function(e) {
  message(paste0('ERROR in frozen worker: ', e$message))
  quit(status = 1)
})
