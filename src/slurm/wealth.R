#-----------------------------------------------------------------------
# wealth.R — Phase 2W worker
#
# Runs the wealth bathtub pre-pass for a single counterfactual scenario.
# Reads the scenario's conv-no-wealth detail (Phase 2N) and the baseline
# static detail (Phase 1), assigns (age x net-worth-percentile) cells,
# builds the conventional forcing ΔT⁰ = Δ(liab_iit_pr + liab_wealth), and
# runs the per-living-record deficit recurrence sequentially across years
# (the recurrence depends on year t-1, so it is NOT year-parallel). Writes
# the per-year deficit state under the scenario's
# conventional/supplemental/wealth_dynamics_state/ directory, which the
# Phase 2C final conventional workers apply as the wealth haircut.
#
# Must run AFTER Phase 2N (conv-no-wealth detail) and Phase 1 (baseline
# detail), and BEFORE Phase 2C. One job per s>0 scenario.
#
# No-op for scenarios that do not activate the channel (s = 0).
#
# CLI args:
#   Rscript src/slurm/wealth.R <staging_dir>
#
# Environment:
#   SLURM_ARRAY_TASK_ID maps to a counterfactual scenario via manifest.rds
#-----------------------------------------------------------------------


args = commandArgs(trailingOnly = T)
if (length(args) < 1) {
  stop('Usage: Rscript src/slurm/wealth.R <staging_dir>')
}

staging_dir = args[1]

tryCatch({

  # Reconstitute environment (packages, source files, globals, return_vars)
  source('./src/slurm/common.R')
  runtime_args = reconstitute_environment(staging_dir)

  # Map SLURM_ARRAY_TASK_ID to a CF scenario (year is NA for Phase 2W)
  task = get_task(staging_dir, '2W')

  cat(paste0('Phase 2W: running wealth bathtub pre-pass for scenario=',
             task$scenario, '\n'))

  # Load scenario config
  config        = readRDS(file.path(staging_dir, task$scenario, 'config.rds'))
  scenario_info = config$scenario_info

  # Install this scenario's resolved assumptions as the active set. A SLURM
  # worker is a fresh R process that never runs do_scenario, so without this
  # every assumption() read errors (fail-closed by design -- see
  # src/misc/assumptions.R). scenario_info rides in on config.rds, so nothing
  # extra is serialized.
  assumptions_activate(scenario_info$assumptions)

  if (!scenario_uses_wealth_dynamics(scenario_info)) {
    cat(paste0('Phase 2W: scenario=', task$scenario,
               ' does not activate the wealth channel (s = 0); skipping.\n'))
    quit(status = 0)
  }

  # Run the wealth bathtub pre-pass (defined in src/sim/wealth_dynamics.R)
  run_wealth_bathtub_pass(scenario_info, config$tax_law,
                          vat_price_offset = config$vat_price_offset)

  cat(paste0('Phase 2W: completed wealth bathtub pass for scenario=',
             task$scenario, '\n'))

}, error = function(e) {
  message(paste0('ERROR in wealth worker: ', e$message))
  quit(status = 1)
})
