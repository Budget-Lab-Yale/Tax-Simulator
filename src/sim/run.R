#-----------------------------------------------
# run.R
#
# Contains functions to execute full simulation
#-----------------------------------------------



do_scenario = function(ID, baseline_mtrs) {
  
  #----------------------------------------------------------------------------
  # Executes full simulation for a given scenario. 
  # 
  # Parameters:
  #   - ID (str)           : scenario ID
  #   - baseline_mtrs (df) : tibble of baseline MTRs indexed by year/tax unit 
  #                          ID; NULL if this scenario is the baseline or if 
  #                          no MTR variables were specified 
  #
  # Returns: tibble of baseline MTRs if this scenario is the baseline (df); 
  #          NULL otherwise.
  #----------------------------------------------------------------------------
  
  if (globals$multicore != 'scenario') {
    print(paste0("Running scenario ", "'", ID, "'"))
  }
  
  # Get scenario info
  scenario_info = get_scenario_info(ID)


  #-----------------
  # Initialize data
  #-----------------

  # Calculate VAT price offset 
  vat_price_offset = get_vat_price_offset(
    macro_root = scenario_info$interface_paths$`Macro-Projections`, 
    vat_root   = scenario_info$interface_paths$`Value-Added-Tax-Model`, 
    years      = scenario_info$years
  )
  
  # Calculate excess growth offset
  excess_growth_offset = get_excess_growth_offset(
    excess_growth = scenario_info$excess_growth, 
    start_year    = scenario_info$excess_growth_start_year, 
    years         = scenario_info$years
  )
  
  # Get price and wage index series
  indexes = generate_indexes(
    macro_root           = scenario_info$interface_paths$`Macro-Projections`, 
    vat_price_offset     = vat_price_offset, 
    excess_growth_offset = excess_growth_offset
  )
  
  # Build (and write) tax law
  tax_law = build_tax_law(scenario_info, indexes)


  #----------------
  # Run simulation
  #----------------

  uses_kg = ID != 'baseline' &&
            any(startsWith(scenario_info$behavior_modules %||% character(),
                           'kg_dynamics/'))

  if (uses_kg) {

    # 3-step: static-only run, bathtub pre-pass, conventional-only run.
    # Required because the bathtub recurrence may need this scenario's static
    # MTRs (v2 cell-MTR mode) to compute r_S(a, t). The static run writes
    # static detail (with mtr_kg_lt when registered) and totals; the bathtub
    # consumes those; the conventional run reads precomputed kg_dynamics_state
    # via the behavior module.

    static_mtrs = run_sim(scenario_info        = scenario_info,
                          tax_law              = tax_law,
                          baseline_mtrs        = baseline_mtrs,
                          indexes              = indexes,
                          vat_price_offset     = vat_price_offset,
                          excess_growth_offset = excess_growth_offset,
                          pass_type            = 'static')

    run_bathtub_pass(scenario_info, tax_law)

    run_sim(scenario_info        = scenario_info,
            tax_law              = tax_law,
            baseline_mtrs        = baseline_mtrs,
            indexes              = indexes,
            vat_price_offset     = vat_price_offset,
            excess_growth_offset = excess_growth_offset,
            pass_type            = 'conventional',
            static_mtrs_all      = static_mtrs)

  } else {

    # Fused static/conventional simulation (current behavior for non-
    # kg_dynamics scenarios and baseline).
    static_mtrs = run_sim(scenario_info        = scenario_info,
                          tax_law              = tax_law,
                          baseline_mtrs        = baseline_mtrs,
                          indexes              = indexes,
                          vat_price_offset     = vat_price_offset,
                          excess_growth_offset = excess_growth_offset)
  }
  
  
  #--------------------
  # Do post-processing
  #--------------------
  
  if (ID != 'baseline') {
    
    # Formatted 1040 report
    build_1040_report(ID)
    
    # Revenue estimates
    calc_rev_est(ID)
    
    # Distribution tables
    build_distribution_tables(ID, baseline_id = 'baseline')
    
    # Time burden tables
    build_timeburden_table(ID)

    # Horizontal equity
    build_horizontal_table(ID)
  }
  
  # Return MTRs if running baseline
  if (ID == 'baseline') {
    return(static_mtrs)
  }
}



run_sim = function(scenario_info, tax_law, baseline_mtrs,
                   indexes, vat_price_offset, excess_growth_offset,
                   pass_type = c('both', 'static', 'conventional'),
                   static_mtrs_all = NULL) {

  #----------------------------------------------------------------------------
  # Runs simulation for all years of a scenario. Three modes (mirroring
  # run_one_year):
  #
  #   pass_type = 'both' (default): static + conventional in one pass per year,
  #     writes both totals files. Returns combined static MTRs.
  #
  #   pass_type = 'static': static-only across all years; writes static totals
  #     and receipts. Returns combined static MTRs.
  #
  #   pass_type = 'conventional': conventional-only across all years; writes
  #     conventional totals and receipts. Caller must supply static_mtrs_all
  #     (combined across years from a prior 'static' run); the loop filters
  #     per year and threads to run_one_year.
  #
  # Used by do_scenario() for the kg_dynamics 3-step (static, bathtub,
  # conventional) and by SLURM aggregate.R Phase 3a indirectly via per-year
  # workers.
  #
  # Parameters:
  #   - scenario_info (list)      : scenario info object; see get_scenario_info()
  #   - tax_law (df)              : tax law tibble; see build_tax_law()
  #   - baseline_mtrs             : tibble of baseline MTRs indexed by year/tax
  #                                 unit ID; NULL if this scenario is the
  #                                 baseline or if no MTR variables were specified
  #   - indexes (df)              : tibble of growth rates for various economic
  #                                 indexes ; see generate_indexes()
  #   - vat_price_offset (df)     : series of price level adjustment factors to
  #                                 reflect introduction of a VAT
  #   - excess_growth_offset (df) : income adjustment factors reflecting excess
  #                                 real GDP growth scenario
  #   - pass_type (str)           : 'both' (default), 'static', or 'conventional'
  #   - static_mtrs_all (df)      : combined static MTRs across years (required
  #                                 when pass_type='conventional' and the
  #                                 scenario has behavior modules that consume
  #                                 static_mtrs)
  #
  # Returns: tibble of marginal tax rates (only when pass_type %in% c('both',
  #          'static')); invisible NULL otherwise.
  #----------------------------------------------------------------------------

  pass_type = match.arg(pass_type)

  per_year = function(year) {
    smy = NULL
    if (!is.null(static_mtrs_all)) {
      smy = static_mtrs_all %>% filter(year == !!year)
    }
    run_one_year(year                 = year,
                 scenario_info        = scenario_info,
                 tax_law              = tax_law,
                 baseline_mtrs        = baseline_mtrs,
                 indexes              = indexes,
                 vat_price_offset     = vat_price_offset,
                 excess_growth_offset = excess_growth_offset,
                 pass_type            = pass_type,
                 static_mtrs_year     = smy)
  }

  # Run simulation for all years (parallel or sequential depending on settings)
  if (globals$multicore == 'year') {
    output = mclapply(X = scenario_info$years, FUN = per_year,
                      mc.cores = min(32, detectCores(logical = F)))
  } else {
    output = list()
    for (t in seq_along(scenario_info$years)) {
      output[[t]] = per_year(scenario_info$years[t])
    }
  }

  # --- Write static outputs (only when this run actually ran the static pass) ---
  if (pass_type %in% c('both', 'static')) {

    static_root = file.path(scenario_info$output_path, 'static')

    vat_price_offset %>%
      write_csv(file.path(static_root, 'supplemental', 'vat_price_offset.csv'))
    excess_growth_offset %>%
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

    static_totals_pr %>%
      left_join(static_totals_1040, by = 'year') %>%
      calc_receipts(
        scenario_root         = static_root,
        vat_root              = scenario_info$interface_paths$`Value-Added-Tax-Model`,
        other_root            = scenario_info$interface_paths$`Macro-Projections`,
        cost_recovery_root    = scenario_info$interface_paths$`Cost-Recovery-Simulator`,
        off_model_root        = scenario_info$interface_paths$`Off-Model-Estimates`,
        excess_growth_all_rev = scenario_info$excess_growth_all_rev
      )
  }

  # --- Write conventional outputs (skip for baseline; only when conv pass ran) ---
  if (pass_type %in% c('both', 'conventional') && scenario_info$ID != 'baseline') {

    conv_root = file.path(scenario_info$output_path, 'conventional')

    vat_price_offset %>%
      write_csv(file.path(conv_root, 'supplemental', 'vat_price_offset.csv'))
    excess_growth_offset %>%
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

    conv_totals_pr %>%
      left_join(conv_totals_1040, by = 'year') %>%
      calc_receipts(
        scenario_root         = conv_root,
        vat_root              = scenario_info$interface_paths$`Value-Added-Tax-Model`,
        other_root            = scenario_info$interface_paths$`Macro-Projections`,
        cost_recovery_root    = scenario_info$interface_paths$`Cost-Recovery-Simulator`,
        off_model_root        = scenario_info$interface_paths$`Off-Model-Estimates`,
        excess_growth_all_rev = scenario_info$excess_growth_all_rev
      )
  }

  # Return combined MTRs (only meaningful when static pass ran in this call)
  if (pass_type %in% c('both', 'static')) {
    return(output %>% map(.f = ~ .x$mtrs) %>% bind_rows())
  }
  invisible(NULL)
}



run_one_year = function(year, scenario_info, tax_law, baseline_mtrs,
                        indexes, vat_price_offset, excess_growth_offset,
                        pass_type = c('both', 'static', 'conventional'),
                        static_mtrs_year = NULL) {

  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation. Three modes:
  #
  #   pass_type = 'both' (default, used by main.R sequential and SLURM Phase 1):
  #     Loads tax_units, runs static pass + conventional pass in one process,
  #     writes both detail files. Returns mtrs + static_totals + conventional_totals.
  #
  #   pass_type = 'static' (SLURM Phase 2A): runs only the static pass,
  #     including MTR calc. Returns mtrs + static_totals.
  #
  #   pass_type = 'conventional' (SLURM Phase 2C): runs only the conventional
  #     pass. Caller must supply `static_mtrs_year` (typically read from the
  #     Phase 2A per-year .rds) so behavioral modules see correct static MTRs.
  #     Returns conventional_totals.
  #
  # Parameters:
  #   - year (int)                : year to run
  #   - scenario_info (list)      : scenario info object; see get_scenario_info()
  #   - tax_law (df)              : tax law tibble; see build_tax_law()
  #   - baseline_mtrs             : tibble of baseline MTRs indexed by year/tax
  #                                 unit ID; NULL if this scenario is the baseline
  #                                 or if no MTR variables were specified
  #   - indexes (df)              : tibble of growth rates for various economic
  #                                 indexes ; see generate_indexes()
  #   - vat_price_offset (df)     : series of price level adjustment factors to
  #                                 reflect introduction of a VAT
  #   - excess_growth_offset (df) : income adjustment factors reflecting excess
  #                                 real GDP growth scenario
  #   - pass_type (str)           : 'both', 'static', or 'conventional'
  #   - static_mtrs_year (df)     : pre-computed static MTRs (only required in
  #                                 pass_type='conventional' when has_behavior)
  #
  # Returns: list with subset of {mtrs, static_totals, conventional_totals}
  # depending on pass_type.
  #----------------------------------------------------------------------------

  pass_type = match.arg(pass_type)

  if (globals$multicore != 'year') {
    print(paste0('Running ', year, ' (', pass_type, ') for scenario ',
                 "'", scenario_info$ID, "'"))
  }



  #--------------------------------
  # Load and process tax unit data
  #--------------------------------

  # Read data
  tax_units = scenario_info$interface_paths$`Tax-Data` %>%
    read_microdata(year) %>%

    # Subset records if running with a sample of the full data
    filter(id %in% globals$sample_ids) %>%
    mutate(weight        = weight / globals$pct_sample,
           year          = year,
           decedent_flag = 0L) %>%

    # Assign random numbers
    bind_cols(globals$random_numbers) %>%

    # Recode filing status if tax law departs from traditional options
    left_join(tax_law %>%
                distinct(year, filing.repeal_hoh),
              by = 'year') %>%
    mutate(filing_status = if_else(filing.repeal_hoh == 1 & filing_status == 4,
                                   1,
                                   filing_status)) %>%

    # Join tax law
    left_join(tax_law, by = c('year', 'filing_status')) %>%

    # Account for tax law changes manifesting as reporting changes
    do_salt_workaround_baseline() %>%

    # Adjust Social Security benefits for VAT-driven price level increase
    do_ss_cola(year, vat_price_offset) %>%

    # Adjust capital income for VAT-drive price level increase
    do_capital_adjustment(year, vat_price_offset) %>%

    # Adjust intensive-margin variables for excess real GDP growth
    do_excess_growth(scenario_info, excess_growth_offset) %>%

    # Compute CPI ratio for capital gains basis indexation
    calc_kg_cpi_ratio(indexes, year)


  #----------
  # Do taxes
  #----------

  # Read baseline payroll taxes
  baseline_pr_er = NULL
  if (scenario_info$ID != 'baseline') {
    baseline_pr_er = globals$baseline_root %>%
      file.path('baseline/static/detail', paste0(year, '.csv')) %>%
      fread() %>%
      tibble() %>%
      select(id, baseline1 = liab_fica_er1, baseline2 = liab_fica_er2)
  }

  # List calculated tax variables
  vars_1040 = return_vars %>%
    remove_by_name('calc_pr') %>%
    unlist() %>%
    set_names(NULL)


  # --- STATIC PASS ---
  static_totals    = NULL
  tax_units_static = NULL
  if (pass_type %in% c('both', 'static')) {

    # Use %>% (not %<>%) so original tax_units stays unmodified for conventional pass
    tax_units_static = tax_units %>%
      do_taxes(baseline_pr_er = baseline_pr_er,
               vars_1040      = vars_1040,
               vars_payroll   = return_vars$calc_pr)

    # Calculate static marginal tax rates
    static_mtrs_year = NULL
    if (!is.null(scenario_info$mtr_vars)) {
      static_mtrs_year = scenario_info$mtr_vars %>%
        map2(.y = scenario_info$mtr_types,
             .f = ~ calc_mtrs(
               tax_units       = tax_units_static %>%
                                   select(-all_of(return_vars %>%
                                   unlist() %>%
                                   set_names(NULL))),
               actual_liab_iit = tax_units_static$liab_iit_net,
               actual_liab_pr  = tax_units_static$liab_pr,
               var             = .x,
               pr              = F,
               type            = .y
            )
        ) %>%
        bind_cols() %>%
        mutate(id   = tax_units_static$id,
               year = year) %>%
        relocate(id, year)

      # Add MTRs to static tax units dataframe
      tax_units_static %<>%
        left_join(static_mtrs_year %>%
                    select(-year),
                  by = 'id')
    }

    # Write static detail
    tax_units_static %>%
      select(all_of(globals$detail_vars), starts_with('mtr_')) %>%
      write_csv(file.path(scenario_info$output_path, 'static', 'detail',
                          paste0(year, '.csv')))

    # Get static totals
    static_totals = list(pr            = get_pr_totals(tax_units_static, year),
                          `1040`        = get_1040_totals(tax_units_static, year),
                          `1040_by_agi` = get_1040_totals(tax_units_static, year, T))
  }


  # --- CONVENTIONAL PASS ---
  has_behavior        = length(scenario_info$behavior_modules) > 0
  conventional_totals = NULL

  if (pass_type %in% c('both', 'conventional')) {

    if (has_behavior) {

      # Behavioral feedback on original (unmodified) tax_units
      tax_units_conv = tax_units %>%
        do_behavioral_feedback(behavior_modules = scenario_info$behavior_modules,
                               baseline_mtrs    = baseline_mtrs,
                               static_mtrs      = static_mtrs_year,
                               scenario_info    = scenario_info,
                               indexes          = indexes) %>%
        do_taxes(baseline_pr_er = baseline_pr_er,
                 vars_1040      = vars_1040,
                 vars_payroll   = return_vars$calc_pr)

      # Calculate conventional marginal tax rates
      if (!is.null(scenario_info$mtr_vars)) {
        conv_mtrs = scenario_info$mtr_vars %>%
          map2(.y = scenario_info$mtr_types,
               .f = ~ calc_mtrs(
                 tax_units       = tax_units_conv %>%
                                     select(-all_of(return_vars %>%
                                     unlist() %>%
                                     set_names(NULL))),
                 actual_liab_iit = tax_units_conv$liab_iit_net,
                 actual_liab_pr  = tax_units_conv$liab_pr,
                 var             = .x,
                 pr              = F,
                 type            = .y
              )
          ) %>%
          bind_cols() %>%
          mutate(id   = tax_units_conv$id,
                 year = year) %>%
          relocate(id, year)

        tax_units_conv %<>%
          left_join(conv_mtrs %>%
                      select(-year),
                    by = 'id')
      }

      # Write conventional detail
      tax_units_conv %>%
        select(all_of(globals$detail_vars), starts_with('mtr_')) %>%
        write_csv(file.path(scenario_info$output_path, 'conventional', 'detail',
                            paste0(year, '.csv')))

      # Get conventional totals
      conventional_totals = list(pr            = get_pr_totals(tax_units_conv, year),
                                  `1040`        = get_1040_totals(tax_units_conv, year),
                                  `1040_by_agi` = get_1040_totals(tax_units_conv, year, T))

    } else if (scenario_info$ID != 'baseline') {

      # No behavior: copy static detail to conventional output. In 'both' mode
      # we have tax_units_static in memory; in 'conventional' mode we copy the
      # already-written static csv directly.
      conv_path = file.path(scenario_info$output_path, 'conventional', 'detail',
                            paste0(year, '.csv'))
      if (!is.null(tax_units_static)) {
        tax_units_static %>%
          select(all_of(globals$detail_vars), starts_with('mtr_')) %>%
          write_csv(conv_path)
      } else {
        static_path = file.path(scenario_info$output_path, 'static', 'detail',
                                paste0(year, '.csv'))
        file.copy(static_path, conv_path, overwrite = TRUE)
      }

      # In 'both' mode static_totals is in scope; in 'conventional' mode the
      # caller (worker) is responsible for substituting the static_totals from
      # Phase 2A's per-year .rds. We return NULL here.
      conventional_totals = static_totals
    }
  }

  # Return required data
  return(list(mtrs                = static_mtrs_year,
              static_totals       = static_totals,
              conventional_totals = conventional_totals))
}



run_bathtub_pass = function(scenario_info, tax_law) {

  #----------------------------------------------------------------------------
  # Orchestrates the kg_dynamics bathtub pre-pass for one scenario. Aggregates
  # baseline cells from Tax-Data, builds scalar tau lists from baseline+reform
  # tax law (v1; v2 will replace with cell-MTR vectors), and runs the
  # sequential year-by-year recurrence via kg_dyn_run_bathtub_pass(). Side
  # effect: writes per-year state files under
  # {scenario_output}/conventional/supplemental/kg_dynamics_state/.
  #
  # Called by do_scenario() for non-baseline scenarios that include any
  # behavior module under kg_dynamics/. The behavior module then reads its
  # year's state file in the conventional pass.
  #
  # Parameters:
  #   - scenario_info (list) : output of get_scenario_info()
  #   - tax_law (df)         : output of build_tax_law() — reform's joined
  #                            tax law tibble
  #
  # Returns: invisibly NULL.
  #----------------------------------------------------------------------------

  baseline_cells = kg_dyn_aggregate_baseline_cells_from_taxdata(
    scenario_info = scenario_info,
    sample_ids    = globals$sample_ids,
    pct_sample    = globals$pct_sample
  )

  # v2 cell-MTR mode activates when the scenario's runscript registers
  # mtr_vars = "kg_lt" (so the static pass produced mtr_kg_lt in detail).
  # Otherwise fall back to v1 scalar tau from pref.rates3.
  use_cellmtr = !is.null(scenario_info$mtr_vars) &&
                ('kg_lt' %in% scenario_info$mtr_vars)

  if (use_cellmtr) {
    tau_lists = kg_dyn_build_cellmtr_tau_lists(
      scenario_info = scenario_info,
      baseline_root = globals$baseline_root,
      sample_ids    = globals$sample_ids,
      pct_sample    = globals$pct_sample
    )
  } else {
    tau_lists = kg_dyn_build_scalar_tau_lists(
      scenario_info = scenario_info,
      tax_law       = tax_law,
      baseline_root = globals$baseline_root
    )
  }

  kg_dyn_run_bathtub_pass(
    scenario_info  = scenario_info,
    tax_law        = tax_law,
    baseline_cells = baseline_cells,
    baseline_tau   = tau_lists$baseline_tau,
    reform_tau     = tau_lists$reform_tau
  )

  invisible(NULL)
}
