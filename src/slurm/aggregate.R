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

    # Build ordered list of scenarios: baseline (if ran) + counterfactuals
    all_scenarios = c()
    if (file.exists(file.path(staging_dir, 'baseline', 'config.rds'))) {
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

    # Read all per-year results. For baseline, results live in year_{y}.rds
    # (Phase 1 writes both static + null conventional via pass_type='both').
    # For counterfactuals, Phase 2A writes year_{y}_static.rds (mtrs +
    # static_totals) and Phase 2C writes year_{y}_conv.rds (conventional_totals).
    # We synthesize a per-year list with the same shape as the legacy
    # monolithic result so the rest of this function is unchanged.
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

        # If conv_totals is NULL (no-behavior CF), fall back to static_totals
        # so downstream aggregation has something to write.
        ct = c_res$conventional_totals
        if (is.null(ct)) ct = s_res$static_totals

        list(mtrs                = s_res$mtrs,
             static_totals       = s_res$static_totals,
             conventional_totals = ct)
      }
    })

    # --- Write static outputs ---
    static_root = file.path(scenario_info$output_path, 'static')

    config$vat_price_offset %>%
      write_csv(file.path(static_root, 'supplemental', 'vat_price_offset.csv'))
    config$excess_growth_offset %>%
      write_csv(file.path(static_root, 'supplemental', 'excess_growth_offset.csv'))

    static_totals_pr = output %>%
      map(.f = ~.x$static_totals$pr) %>%
      bind_rows() %>%
      write_csv(file.path(static_root, 'totals', 'payroll.csv'))

    static_totals_1040 = output %>%
      map(.f = ~.x$static_totals$`1040`) %>%
      bind_rows() %>%
      write_csv(file.path(static_root, 'totals', '1040.csv'))

    output %>%
      map(.f = ~.x$static_totals$`1040_by_agi`) %>%
      bind_rows() %>%
      write_csv(file.path(static_root, 'totals', '1040_by_agi.csv'))

    static_totals_estate = output %>%
      map(.f = ~.x$static_totals$estate) %>%
      bind_rows() %>%
      write_csv(file.path(static_root, 'totals', 'estate.csv'))

    static_totals_pr %>%
      left_join(static_totals_1040,   by = 'year') %>%
      left_join(static_totals_estate, by = 'year') %>%
      calc_receipts(
        scenario_root         = static_root,
        vat_root              = scenario_info$interface_paths$`Value-Added-Tax-Model`,
        other_root            = scenario_info$interface_paths$`Macro-Projections`,
        cost_recovery_root    = scenario_info$interface_paths$`Cost-Recovery-Simulator`,
        off_model_root        = scenario_info$interface_paths$`Off-Model-Estimates`,
        excess_growth_all_rev = scenario_info$excess_growth_all_rev
      )

    # --- Write conventional outputs (skip for baseline) ---
    if (scenario_id != 'baseline') {

      conv_root = file.path(scenario_info$output_path, 'conventional')

      config$vat_price_offset %>%
        write_csv(file.path(conv_root, 'supplemental', 'vat_price_offset.csv'))
      config$excess_growth_offset %>%
        write_csv(file.path(conv_root, 'supplemental', 'excess_growth_offset.csv'))

      conv_totals_pr = output %>%
        map(.f = ~.x$conventional_totals$pr) %>%
        bind_rows() %>%
        write_csv(file.path(conv_root, 'totals', 'payroll.csv'))

      conv_totals_1040 = output %>%
        map(.f = ~.x$conventional_totals$`1040`) %>%
        bind_rows() %>%
        write_csv(file.path(conv_root, 'totals', '1040.csv'))

      output %>%
        map(.f = ~.x$conventional_totals$`1040_by_agi`) %>%
        bind_rows() %>%
        write_csv(file.path(conv_root, 'totals', '1040_by_agi.csv'))

      conv_totals_estate = output %>%
        map(.f = ~.x$conventional_totals$estate) %>%
        bind_rows() %>%
        write_csv(file.path(conv_root, 'totals', 'estate.csv'))

      conv_totals_pr %>%
        left_join(conv_totals_1040,   by = 'year') %>%
        left_join(conv_totals_estate, by = 'year') %>%
        calc_receipts(
          scenario_root         = conv_root,
          vat_root              = scenario_info$interface_paths$`Value-Added-Tax-Model`,
          other_root            = scenario_info$interface_paths$`Macro-Projections`,
          cost_recovery_root    = scenario_info$interface_paths$`Cost-Recovery-Simulator`,
          off_model_root        = scenario_info$interface_paths$`Off-Model-Estimates`,
          excess_growth_all_rev = scenario_info$excess_growth_all_rev
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

    # Formatted 1040 report
    build_1040_report(scenario_id)

    # Revenue estimates
    calc_rev_est(scenario_id)

    # Distribution tables
    build_distribution_tables(scenario_id, baseline_id = 'baseline')

    # Time burden tables
    build_timeburden_table(scenario_id)

    # Horizontal equity
    build_horizontal_table(scenario_id)

    # KG dynamics bathtub diagnostics (no-op for non-kg_dynamics scenarios)
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
