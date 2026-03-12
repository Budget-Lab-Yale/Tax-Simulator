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
  
  # Run fused static/conventional simulation
  static_mtrs = run_sim(scenario_info        = scenario_info,
                        tax_law              = tax_law,
                        baseline_mtrs        = baseline_mtrs,
                        indexes              = indexes,
                        vat_price_offset     = vat_price_offset,
                        excess_growth_offset = excess_growth_offset)
  
  
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
                   indexes, vat_price_offset, excess_growth_offset) {

  #----------------------------------------------------------------------------
  # Runs full simulation for a given scenario, producing both static and
  # conventional results in a single pass over the data.
  #
  # Parameters:
  #   - scenario_info (list)      : scenario info object; see get_scenario_info()
  #   - tax_law (df)              : tax law tibble; see build_tax_law()
  #   - baseline_mtrs             : tibble of baseline MTRs indexed by year/tax
  #                                 unit  ID; NULL if this scenario is the
  #                                 baseline or if no MTR variables were specified
  #   - indexes (df)              : tibble of growth rates for various economic
  #                                 indexes ; see generate_indexes()
  #   - vat_price_offset (df)     : series of price level adjustment factors to
  #                                 reflect introduction of a VAT
  #   - excess_growth_offset (df) : income adjustment factors reflecting excess
  #                                 real GDP growth scenario
  #
  # Returns: tibble of marginal tax rates (df).
  #----------------------------------------------------------------------------

  # Run simulation for all years (parallel or sequential depending on settings)
  if (globals$multicore == 'year') {

    # Parallel execution of years
    output = mclapply(
      X = scenario_info$years,
      FUN = function(year) {
        run_one_year(year                 = year,
                     scenario_info        = scenario_info,
                     tax_law              = tax_law,
                     baseline_mtrs        = baseline_mtrs,
                     indexes              = indexes,
                     vat_price_offset     = vat_price_offset,
                     excess_growth_offset = excess_growth_offset)
      },
      mc.cores = min(32, detectCores(logical = F))
    )
  } else {

    # Sequential execution
    output = list()
    for (t in seq_along(scenario_info$years)) {

      year = scenario_info$years[t]

      # Run simulation of year
      output[[t]] = run_one_year(year                 = year,
                                 scenario_info        = scenario_info,
                                 tax_law              = tax_law,
                                 baseline_mtrs        = baseline_mtrs,
                                 indexes              = indexes,
                                 vat_price_offset     = vat_price_offset,
                                 excess_growth_offset = excess_growth_offset)
    }
  }

  # --- Write static outputs ---
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

  # --- Write conventional outputs (skip for baseline) ---
  if (scenario_info$ID != 'baseline') {

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

  # Return MTRs
  output %>%
    map(.f = ~ .x$mtrs) %>%
    bind_rows() %>%
    return()
}



run_one_year = function(year, scenario_info, tax_law, baseline_mtrs,
                        indexes, vat_price_offset, excess_growth_offset) {

  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation, producing both static and
  # conventional results in a single pass over the data.
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
  #
  # Returns: list of:
  #  - mtrs (df)                  : tibble of static marginal tax rates for this
  #                                 year
  #  - static_totals (list)       : list of tibbles for this year's static tax
  #                                 aggregates
  #  - conventional_totals (list) : list of tibbles for this year's conventional
  #                                 tax aggregates (NULL for baseline)
  #----------------------------------------------------------------------------

  if (globals$multicore != 'year') {
    print(paste0('Running ', year, ' for scenario ', "'", scenario_info$ID, "'"))
  }



  #--------------------------------
  # Load and process tax unit data
  #--------------------------------

  # Read data
  tax_units = scenario_info$interface_paths$`Tax-Data` %>%
    read_microdata(year) %>%

    # Subset records if running with a sample of the full data
    filter(id %in% globals$sample_ids) %>%
    mutate(weight = weight / globals$pct_sample,
           year   = year) %>%

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


  # --- CONVENTIONAL PASS ---
  has_behavior        = length(scenario_info$behavior_modules) > 0
  conventional_totals = NULL

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

    # No behavior: write static data to conventional output (replaces file copy)
    tax_units_static %>%
      select(all_of(globals$detail_vars), starts_with('mtr_')) %>%
      write_csv(file.path(scenario_info$output_path, 'conventional', 'detail',
                          paste0(year, '.csv')))

    conventional_totals = static_totals
  }

  # Return required data
  return(list(mtrs                = static_mtrs_year,
              static_totals       = static_totals,
              conventional_totals = conventional_totals))
}
