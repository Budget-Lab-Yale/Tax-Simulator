#-----------------------------------------------------------------------
# bathtub.R — Phase 2B worker
#
# Runs the kg_dynamics bathtub pre-pass for a single counterfactual
# scenario. Aggregates baseline cells from Tax-Data, builds tau lists from
# baseline + reform tax law, runs the sequential year-by-year recurrence,
# and persists per-year state files under the scenario's
# conventional/supplemental/kg_dynamics_state/ directory.
#
# sigma income-conversion scenarios (conversion/sigma in the behavior
# column) need no extra handling here: run_bathtub_pass() builds the sigma
# context internally (raw Tax-Data pool legs + baseline/scenario static
# detail MTRs — both available, since Phase 2B depends on 2A which depends
# on Phase 1) and the bathtub pass computes conversions, injects the
# gain-state inflow, and persists the cell tracker in the state files.
#
# No-op for scenarios that don't include any kg_dynamics/ behavior module.
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

  # Install this scenario's resolved assumptions as the active set. A SLURM
  # worker is a fresh R process that never runs do_scenario, so without this
  # every assumption() read errors (fail-closed by design -- see
  # src/misc/assumptions.R). scenario_info rides in on config.rds, so nothing
  # extra is serialized.
  assumptions_activate(scenario_info$assumptions)

  if (!scenario_uses_kg_dynamics(scenario_info)) {
    cat(paste0('Phase 2B: scenario=', task$scenario,
               ' does not use kg_dynamics; skipping.\n'))
    quit(status = 0)
  }

  # Run the bathtub pre-pass (defined in src/sim/run.R)
  run_bathtub_pass(scenario_info, config$tax_law,
                   vat_price_offset     = config$vat_price_offset,
                   excess_growth_offset = config$excess_growth_offset)

  cat(paste0('Phase 2B: completed bathtub for scenario=', task$scenario, '\n'))

}, error = function(e) {
  message(paste0('ERROR in bathtub worker: ', e$message))
  quit(status = 1)
})
