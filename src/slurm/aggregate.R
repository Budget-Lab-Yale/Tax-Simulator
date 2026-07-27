#-----------------------------------------------------------------------
# aggregate.R
#
# Phases 3a, 3b, and 4 of SLURM pipeline.
#
# Phase 3a: Aggregation — reads per-year .rds results, writes totals
#           CSVs and receipts for each scenario (including baseline).
# Phase 3b: Post-processing — builds 1040 reports, revenue estimates,
#           distribution tables, and time burden for counterfactuals.
# Phase 4:  Stacked — stacked 1040 reports, stacked revenue estimates,
#           and optional detail purge.
#
# CLI args:
#   Rscript src/slurm/aggregate.R <staging_dir> <phase>
#
# Environment:
#   SLURM_ARRAY_TASK_ID maps to scenario for phases 3a/3b.
#   Phase 4 ignores SLURM_ARRAY_TASK_ID.
#-----------------------------------------------------------------------


args = commandArgs(trailingOnly = T)
if (length(args) < 2) {
  stop('Usage: Rscript src/slurm/aggregate.R <staging_dir> <phase>')
}

staging_dir = args[1]
phase       = args[2]

# Wrap in tryCatch for clean SLURM error handling
tryCatch({

  # Reconstitute environment
  source('./src/slurm/common.R')
  runtime_args = reconstitute_environment(staging_dir)


  #--------------------------------------------------------------------
  # Phase 3a: Aggregation (1 job per scenario including baseline)
  #--------------------------------------------------------------------

  if (phase == '3a') {

    task_id = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))

    # Build the ordered scenario list from the manifest rather than by looking for
    # a baseline config on disk. The staging directory persists across runs of one
    # vintage, so a baseline config left by an earlier run would shift the array
    # indexing and drop the last counterfactual
    manifest = readRDS(file.path(staging_dir, 'manifest.rds'))
    all_scenarios = c()
    if (any(manifest$phase == '1')) {
      all_scenarios = c('baseline')
    }
    all_scenarios = c(all_scenarios, counterfactual_ids)

    if (task_id < 1 || task_id > length(all_scenarios)) {
      stop(paste0('SLURM_ARRAY_TASK_ID ', task_id,
                  ' out of range for phase 3a (',
                  length(all_scenarios), ' scenarios)'))
    }

    scenario_id = all_scenarios[task_id]
    cat(paste0('Phase 3a: aggregating scenario=', scenario_id, '\n'))

    # Load config
    config = readRDS(file.path(staging_dir, scenario_id, 'config.rds'))
    scenario_info = config$scenario_info

    # Activate the scenario's legs: aggregation reads configuration too, for the
    # corporate distribution smear and the housing structure share
    config_activate(economy  = scenario_info$resolved_economy,
                    behavior = scenario_info$resolved_behavior)

    # Read all per-year results and assemble a per-year list of the shape run_sim
    # produces in one process. Baseline results are in year_{y}.rds, holding the
    # static totals and a null conventional; a counterfactual's are split across
    # the Phase 2A and Phase 2C files.
    output = scenario_info$years %>% map(function(y) {
      if (scenario_id == 'baseline') {
        readRDS(file.path(staging_dir, scenario_id, paste0('year_', y, '.rds')))
      } else {
        static_rds = file.path(staging_dir, scenario_id,
                                paste0('year_', y, '_static.rds'))
        conv_rds   = file.path(staging_dir, scenario_id,
                                paste0('year_', y, '_conv.rds'))
        s_res = readRDS(static_rds)
        c_res = readRDS(conv_rds)

        # A scenario with no behavior has no conventional totals; fall back to the
        # static ones so the aggregation has something to write
        ct = c_res$conventional_totals
        if (is.null(ct)) ct = s_res$static_totals

        list(mtrs                = s_res$mtrs,
             static_totals       = s_res$static_totals,
             conventional_totals = ct)
      }
    })

    # --- Write static outputs ---
    # Write through the same helper run_sim uses; see src/sim/run.R
    write_pass_outputs(
      output               = output,
      root                 = file.path(scenario_info$output_path, 'static'),
      totals_slot          = 'static_totals',
      vat_price_offset     = config$vat_price_offset,
      scenario_info        = scenario_info
    )

    # --- Write conventional outputs (skip for baseline) ---
    if (scenario_id != 'baseline') {
      write_pass_outputs(
        output               = output,
        root                 = file.path(scenario_info$output_path, 'conventional'),
        totals_slot          = 'conventional_totals',
        vat_price_offset     = config$vat_price_offset,
        scenario_info        = scenario_info
      )
    }

    cat(paste0('Phase 3a: completed scenario=', scenario_id, '\n'))
  }


  #--------------------------------------------------------------------
  # Phase 3b: Post-processing (1 job per counterfactual)
  #--------------------------------------------------------------------

  if (phase == '3b') {

    task_id = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))

    if (task_id < 1 || task_id > length(counterfactual_ids)) {
      stop(paste0('SLURM_ARRAY_TASK_ID ', task_id,
                  ' out of range for phase 3b (',
                  length(counterfactual_ids), ' counterfactuals)'))
    }

    scenario_id = counterfactual_ids[task_id]
    cat(paste0('Phase 3b: post-processing scenario=', scenario_id, '\n'))

    # Activate the scenario's legs: post-processing reads the corporate foreign
    # share and the housing structure share
    .si = get_scenario_info(scenario_id)
    config_activate(economy  = .si$resolved_economy,
                    behavior = .si$resolved_behavior)

    # Formatted 1040 report
    build_1040_report(scenario_id)

    # Revenue estimates
    calc_rev_est(scenario_id)

    # Distribution tables
    build_distribution_tables(scenario_id, baseline_id = 'baseline')

    # Effective tax rate levels, under accrual income definitions and the
    # stock-based corporate incidence conventions
    build_distribution_etrs(scenario_id)

    # Time burden tables
    build_timeburden_table(scenario_id)

    # Horizontal equity
    build_horizontal_table(scenario_id)

    # Capital gains bathtub diagnostics
    scenario_info = get_scenario_info(scenario_id)
    kg_dyn_build_summary(scenario_info)

    cat(paste0('Phase 3b: completed scenario=', scenario_id, '\n'))
  }


  #--------------------------------------------------------------------
  # Phase 4: Stacked reports + optional detail purge (single job)
  #--------------------------------------------------------------------

  if (phase == '4') {

    cat('Phase 4: running stacked post-processing\n')

    if (runtime_args$stacked == 1) {
      build_stacked_1040_reports(counterfactual_ids)
      calc_stacked_rev_est(counterfactual_ids)
    }

    if (runtime_args$delete_detail == 1) {
      purge_detail()
    }

    cat('Phase 4: completed\n')
  }

}, error = function(e) {
  message(paste0('ERROR in aggregate (phase=', phase, '): ', e$message))
  quit(status = 1)
})
