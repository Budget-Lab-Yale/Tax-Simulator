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
  
  # Get scenario info
  scenario_info = get_scenario_info(ID)

  
  #-----------------
  # Initialize data
  #-----------------

  # Get indexation data
  indexes = generate_indexes(scenario_info$interface_paths$`Macro-Projections`)
  
  # Build (and write) tax law
  tax_law = build_tax_law(scenario_info, indexes)
  
  
  #----------------
  # Run simulation
  #----------------
  
  # Run static simulation
  static_mtrs = run_sim(scenario_info = scenario_info, 
                        tax_law       = tax_law, 
                        static        = T,
                        baseline_mtrs = NULL, 
                        static_mtrs   = NULL)
  
  # Run simulation with behavioral feedback if modules are specified
  static_only = length(scenario_info$behavior_modules) == 0
  if (!static_only) {
    run_sim(scenario_info = scenario_info,
            tax_law       = tax_law, 
            static        = F,
            baseline_mtrs = baseline_mtrs, 
            static_mtrs   = static_mtrs)
  
  # Else, for static-only counterfactual runs, copy static runs to scenario's  
  # conventional folder (baseline only has a static subfolder by definition)
  } else if (ID != 'baseline') {
    
    # Set root variables
    static_path = file.path(scenario_info$output_path, 'static')
    conv_path   = file.path(scenario_info$output_path, 'conventional')
    
    # Copy all files
    static_path %>% 
      list.files(recursive = T) %>% 
      map(.f = ~ file.copy(from      = file.path(static_path, .x), 
                           to        = file.path(conv_path,   .x), 
                           overwrite = T))  
  }
  
  # Return MTRs if running baseline
  if (ID == 'baseline') {
    return(static_mtrs)
  }
}



run_sim = function(scenario_info, tax_law, static, baseline_mtrs, static_mtrs) {
  
  #----------------------------------------------------------------------------
  # Runs simulation instance for a given scenario, either static or 
  # conventional. 
  # 
  # Parameters:
  #   - scenario_info (list) : scenario info object; see get_scenario_info()
  #   - tax_law (df)         : tax law tibble; see build_tax_law()
  #   - static (bool)        : whether to run the scenario in static mode
  #   - baseline_mtrs        : tibble of baseline MTRs indexed by year/tax unit 
  #                            ID; NULL if this scenario is the baseline or if 
  #                            no MTR variables were specified 
  #   - static_mtrs          : tibble of MTRs for the static counterfactual 
  #                            scenario run, indexed by year/tax unit ID; NULL 
  #                            if this run is in static mode or if no MTR 
  #                            variables were specified
  #
  # Returns: tibble of marginal tax rates (df).
  #----------------------------------------------------------------------------
  
  # Get output root for this scenario and behvioral feedback assumptions
  output_root = file.path(scenario_info$output_path, if_else(static, 
                                                             'static', 
                                                             'conventional'))
  
  # Initial table of NOLs
  nols = initialize_nols(scenario_info$years)
  
  # Run simulation for all years
  output = list() 
  for (t in seq_along(scenario_info$years)) {
    
    year = scenario_info$years[t]
    
    # Run simulation of year 
    output[[t]] = run_one_year(year          = year,
                               scenario_info = scenario_info, 
                               tax_law       = tax_law,
                               static        = static,
                               baseline_mtrs = baseline_mtrs, 
                               static_mtrs   = static_mtrs, 
                               nols          = nols)
    
    # Update table of NOLs and write 
    nols = update_nols(nols   = nols, 
                       year   = year,
                       amount = output[[t]]$totals$`1040`$excess_bus_loss) 
  }
  
  # Write NOLs file
  nols %>% 
    write_csv(file.path(output_root, 'totals', 'nols.csv'))
  
  # Write totals files
  totals_pr = output %>%
    map(.f = ~.x$totals$pr) %>% 
    bind_rows() %>% 
    write_csv(file.path(output_root, 'totals', 'payroll.csv'))
  
  totals_1040 = output %>% 
    map(.f = ~.x$totals$`1040`) %>% 
    bind_rows() %>% 
    write_csv(file.path(output_root, 'totals', '1040.csv'))
  
  output %>% 
    map(.f = ~.x$totals$`1040_by_agi`) %>% 
    bind_rows() %>% 
    write_csv(file.path(output_root, 'totals', '1040_by_agi.csv'))
    
  # Calculate and write receipts
  totals_pr %>%  
    left_join(totals_1040, by = 'year') %>% 
    calc_receipts(output_root, 
                  scenario_info$interface_paths$`Corporate-Tax-Model`, 
                  scenario_info$interface_paths$`Estate-Tax-Model`) 

  # Return MTRs
  output %>% 
    map(.f = ~ .x$mtrs) %>% 
    bind_rows() %>% 
    return()
}



run_one_year = function(year, scenario_info, tax_law, static, baseline_mtrs, 
                        static_mtrs, nols) {
  
  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation. 
  # 
  # Parameters:
  #   - year (int)           : year to run
  #   - scenario_info (list) : scenario info object; see get_scenario_info()
  #   - tax_law (df)         : tax law tibble; see build_tax_law()
  #   - static (bool)        : whether to run the scenario in static mode
  #   - baseline_mtrs        : tibble of baseline MTRs indexed by year/tax unit 
  #                            ID; NULL if this scenario is the baseline or if 
  #                            no MTR variables were specified 
  #   - static_mtrs          : tibble of MTRs for the static counterfactual 
  #                            scenario run, indexed by year/tax unit ID; NULL 
  #                            if this run is in static mode or if no MTR 
  #                            variables were specified 
  #   - nols (df)            : tibble of endogeneously calculated net operating 
  #                            losses to distribute 
  #
  # Returns: list of:
  #  - mtrs (df)     : tibble of marginal tax rates for this year
  #  - totals (list) : list of tibbles for this year's tax aggregates (`pr` for
  #                    payroll taxes, `1040` for individual income taxes)
  #----------------------------------------------------------------------------
  
  print(paste0('Running ', year, ' for scenario ', "'", scenario_info$ID, "'",
               if_else(static & scenario_info$ID != 'baseline', '(static)', '')))
  
  
  #--------------------------------
  # Load and process tax unit data 
  #--------------------------------
  
  # Read data
  tax_units = scenario_info$interface_paths$`Tax-Data` %>%  
    read_microdata(year) %>% 
  
    # Subset records if running with a sample of full data
    filter(id %in% globals$sample_ids) %>% 
    mutate(weight = weight / globals$pct_sample, 
           year   = year) %>% 
  
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

    # Allocate net operating losses attributable to some prior-year modeled policy
    distribute_nols(nols, year) 
  

  #---------------------------
  # Model behavioral feedback
  #---------------------------
  
  # Only apply behavioral feedback for non-static (and by extension, non-baseline) runs
  if (!static) {
    tax_units %<>%  
      do_behavioral_feedback(behavior_modules = scenario_info$behavior_modules, 
                             baseline_mtrs    = baseline_mtrs, 
                             static_mtrs      = static_mtrs)
  }
  
  
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

  # Calculate taxes
  tax_units %<>% 
    do_taxes(baseline_pr_er = baseline_pr_er,
             vars_1040      = vars_1040,
             vars_payroll   = return_vars$calc_pr)
  
  # Calculate marginal tax rates
  mtrs = NULL
  if (!is.null(scenario_info$mtr_vars)) {
    mtrs = scenario_info$mtr_vars %>%
      map2(.y = scenario_info$mtr_types, 
           .f = ~ calc_mtrs(
             tax_units = tax_units %>% 
                           select(-all_of(return_vars %>% 
                           unlist() %>% 
                           set_names(NULL))), 
             liab_baseline = tax_units$liab_pr_ee + tax_units$liab_iit_net,
             var           = .x,
             type          = .y
          )
      ) %>% 
      bind_cols() %>% 
      mutate(id   = tax_units$id,
             year = year) %>% 
      relocate(id, year)
    
    # Add to tax units dataframe 
    tax_units %<>% 
      left_join(mtrs %>% 
                  select(-year), 
                by = 'id')
  }
  
  
  #-----------------
  # Post-processing
  #-----------------
  
  # Write microdata
  if (year %in% scenario_info$dist_years) {
    tax_units %>%  
      select(all_of(globals$detail_vars), starts_with('mtr_')) %>% 
      write_csv(file.path(scenario_info$output_path, 
                          if_else(static, 'static', 'conventional'),
                          'detail', 
                          paste0(year, '.csv')))
  }
  
  # Get totals from microdata
  totals = list(pr            = get_pr_totals(tax_units, year), 
                `1040`        = get_1040_totals(tax_units, year), 
                `1040_by_agi` = get_1040_totals(tax_units, year, T))
  
  # Return required data
  return(list(mtrs   = mtrs, 
              totals = totals))
} 
