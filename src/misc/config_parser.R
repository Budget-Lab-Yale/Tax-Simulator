#-----------------------------------------------------------------------
# config_parser.R 
# 
# Contains functions to parse runtime options and build interface paths
#-----------------------------------------------------------------------




parse_globals = function(runscript_name, scenario_id, local, vintage, 
                         baseline_vintage, pct_sample, multicore) {
  
  #----------------------------------------------------------------------------
  # Parses data interface versioning requirements and runscript; generates 
  # scenario-specific, version-consistent filepaths for data interfaces. 
  # Confirms that these filepaths exist. 
  # 
  # Parameters:
  #   - runscript_name (str)   : name of runscript CSV file 
  #   - scenario_id (str)      : optional name of scenario ID contained in 
  #                              the runscript; "NULL" indicates all 
  #   - local (int)            : whether this is a local run (1) or a production
  #                              run (0)  
  #   - vintage (str)          : optional argument (NULL if not provided) to 
  #                              manually supply output vintage folder rather 
  #                              than being dynamically generated. Of the format
  #                              YYYYMMDDHHMM.
  #   - baseline_vintage (str) : optional argument (NULL if not provided) to 
  #                              skip the baseline run and instead use an existing
  #                              baseline run for MTRs and revenue estimates. Of 
  #                              the format YYYYMMDDHHMM. 
  #   - pct_sample (dbl)       : share of records used in simulation 
  #   - multicore (str)        : dimension across which to parallelize code. One
  #                              of three values: 'none', 'scenario', or 'year'. 
  #                              Given enough cores, choose the dimension with 
  #                              the largest N (generally 'year'). But note that
  #                              some behavioral feedback modules require
  #                              sequential calculation of year, in which case
  #                              'year' is not a valid option and will result in
  #                              a race condition. Always review before running!
  #
  # Returns: list of 8:
  #   - random_numbers (df)  : tibble of random numbers used across simulations
  #   - runscript (df)       : tibble representation of the runscripts CSV
  #   - interface_paths (df) : tibble with ID-interface-filepath info in rows 
  #   - output_root (str)    : path where output data is written
  #   - baseline_root (str)  : path where baseline data is written/read from
  #   - pct_sample (dbl)     : share of records used in simulation 
  #   - sample_ids (int[])   : vector of tax unit IDs comprising the
  #                            sample population (all IDs for 100%)
  #   - detail_vars (str[])  : vector of microdata output column names
  #   - multicore (str)      : parallelization setting (see arguments)
  #----------------------------------------------------------------------------
  
  # Set random seed 
  set.seed(76)
  
  # Read and parse data dependency interface file paths
  output_roots       = read_yaml('./config/interfaces/output_roots.yaml')
  interface_versions = read_yaml('./config/interfaces/interface_versions.yaml') %>% 
    map2(.x = ., 
         .y = names(.), 
         .f = ~ file.path(output_roots$production, 
                          .x$type, 
                          .y, 
                          paste0('v', .x$version))) %>% 
    as_tibble() %>% 
    pivot_longer(cols      = everything(), 
                 names_to  = 'interface', 
                 values_to = 'path') %>% 
    filter(interface != 'Tax-Simulator')
  
  # Get default vintages/scenario IDs
  interface_defaults = read_yaml('./config/interfaces/interface_versions.yaml') %>% 
    map(.f = ~ .x[c('default_vintage', 'default_id')])
  
  # Set model version and vintage
  version = read_yaml('./config/interfaces/interface_versions.yaml')$`Tax-Simulator`$version
  if (is.null(vintage)) {
    vintage = format(Sys.time(), '%Y%m%d%H%M')
  }

  # Determine and create directory for model output
  output_branch = file.path('Tax-Simulator', paste0('v', version), vintage)
  output_root   = file.path(output_roots$production, 'model_data', output_branch)
  if (local == 1) {
    output_root = file.path(output_roots$local, 'model_data', output_branch)
  }
  dir.create(output_root, recursive = T, showWarnings = F)
  
  # Determine baseline output path 
  if (is.null(baseline_vintage)) {
    baseline_root = output_root
  } else {
    baseline_root = output_root %>% 
      str_remove(paste0('/',vintage)) %>% 
      file.path(baseline_vintage)
    if (!dir.exists(baseline_root)) {
      stop('User-supplied vintage for baseline does not exist!')
    }
  }
  
  # Read runscript
  runscript = runscript_name %>% 
    paste0('.csv') %>%
    file.path('./config/runscripts/', .) %>% 
    read_csv(show_col_types = F)
  
  # Add nonspecified default vintages and scenario IDs to runscript
  for (dep in names(interface_defaults)) {
    
    # Skip specified interfaces
    if (dep == 'Tax-Simulator' | (paste0('dep.', dep, '.vintage') %in% colnames(runscript))) {
      if (dep == 'Tax-Simulator' | (paste0('dep.', dep, '.ID') %in% colnames(runscript))) {
        next
      } else {
        stop('Users specifying which vintage to use must also supply an ID within that directory.')
      }
    } 
    
    if (dep == 'Tax-Simulator' | (paste0('dep.', dep, '.ID') %in% colnames(runscript))) {
      if (dep == 'Tax-Simulator' | (paste0('dep.', dep, '.vintage') %in% colnames(runscript))) {
        next
      } else {
        runscript[[paste0('dep.', dep, '.vintage')]] = interface_defaults[[dep]]$default_vintage
        next
      }
    }
    
    # Add columns
    runscript[[paste0('dep.', dep, '.vintage')]] = interface_defaults[[dep]]$default_vintage
    runscript[[paste0('dep.', dep, '.ID')]]      = interface_defaults[[dep]]$default_id
  }
  
  # Subset runscript to specified ID, if supplied
  if (!is.null(scenario_id)) {
    runscript %<>% 
      filter(ID == scenario_id)
  }
  
  # Write dependencies CSV; this is a vintage-level file which lists all 
  # other model vintages on which these Tax-Simmulator results are dependent
  dependencies = runscript %>% 
    select(TaxSimulatorID = ID, starts_with('dep.')) %>% 
    mutate(across(.cols = everything(),
                  .fns  = as.character)) %>% 
    pivot_longer(cols         = -TaxSimulatorID, 
                 names_prefix = 'dep.', 
                 names_sep    = '[.]', 
                 names_to     = c('interface', 'series')) %>% 
    pivot_wider(names_from  = series, 
                values_from = value) %>% 
    left_join(read_yaml('./config/interfaces/interface_versions.yaml') %>% 
                map(~ .$version) %>% 
                as_tibble() %>% 
                pivot_longer(cols      = everything(), 
                             names_to  = 'interface', 
                             values_to = 'version'), 
              by = 'interface') %>% 
    rename(scenario = ID, ID = TaxSimulatorID) %>% 
    relocate(version, .before = vintage) %>% 
    write_csv(file.path(output_root, 'dependencies.csv'))
  
  
  # Write Tax-Simulator-specific behavioral assumptions
  runscript %>% 
    select(ID, tax_law, behavior) %>%
    write_csv(file.path(output_root, 'behavioral_assumptions.csv'))
  
  
  # Create filepaths for data interfaces
  interface_paths = dependencies %>%     
    left_join(interface_versions, by = 'interface') %>% 
    mutate(path = file.path(path, vintage, scenario)) %>% 
    select(ID, interface, path)

  
  # Confirm that each path exists, throwing exception if not
  for (path in interface_paths$path) {
    if (!dir.exists(path)) {
      msg = paste0("Error: can't find directory '", path, "'. Confirm ",
                   "the interface version is correct and that the vintage exists.")
      stop(msg)
    }
  }
  
  # Confirm that user has supplied valid multicore argument
  if (!(multicore %in% c('none', 'scenario', 'year'))) {
    stop("Invalid argument for 'multicore' runtime parameter")
  }
  
  # Tax unit ID in sample
  sample_ids = interface_paths %>% 
    filter(interface == 'Tax-Data') %>%
    slice(1) %>% 
    get_vector('path') %>% 
    read_microdata(2017) %>%
    sample_frac(size = pct_sample) %>% 
    get_vector('id')
  
  # Precalculate random numbers for consistency across scenarios 
  random_numbers = tibble(
    r.bus_loss        = runif(length(sample_ids)),  # Excess business loss limitation eligibility rate
    r.cdctc_takeup    = runif(length(sample_ids)),  # CDCTC takeup rate
    r.salt_workaround = runif(length(sample_ids)),  # SALT workaround participation rate
    r.behavior1       = runif(length(sample_ids)),  # Spare random number for use in behavioral modules
    r.behavior2       = runif(length(sample_ids)),  # Spare random number for use in behavioral modules
    r.behavior3       = runif(length(sample_ids))   # Spare random number for use in behavioral modules
  )
  
  # Specifiy microdata output variable
  detail_vars = c(
    'id', 'weight', 'filer', 'dep_status', 'filing_status', 'male1', 'male2', 
    'age1', 'age2', 'n_dep','n_dep_ctc', 'dep_age1', 'dep_age2', 'dep_age3', 
    'wages1', 'wages2', 'wages', 'txbl_int', 'exempt_int', 'se', 'div_ord', 
    'div_pref', 'txbl_kg', 'kg_st', 'kg_lt', 'sole_prop', 'sch_e', 'farm', 
    'part_scorp', 'gross_ss', 'txbl_ss', 'above_ded', 'agi', 'expanded_inc', 
    'std_ded', 'item_ded', 'med_item_ded', 'salt_item_ded', 'first_mort_int', 
    'mort_int_item_ded', 'inv_int_item_ded', 'int_item_ded', 'char_item_ded', 
    'casualty_item_ded', 'misc_item_ded', 'other_item_ded', 'item_ded_ex_limits', 
    'itemizing', 'pe_ded', 'qbi_ded', 'txbl_inc', 'liab_ord', 'liab_pref', 
    'liab_amt', 'liab_bc', 'cdctc_nonref', 'ctc_nonref', 'ed_nonref', 'nonref', 
    'ed_ref', 'eitc', 'cdctc_ref', 'ctc_ref', 'rebate', 'ref', 'liab_niit', 
    'liab_iit', 'liab_iit_net', 'liab_fica_er1', 'liab_fica_er2', 'liab_seca', 
    'liab_pr_ee', 'liab_pr', 'simple_filer', 'number_of_credits',
    'liab_brac1', 'liab_brac2', 'liab_brac3', 'liab_brac4', 'liab_brac5',
    'liab_brac6', 'liab_brac7'
  )
  
  
  # Return runtime args and interface paths  
  return(list(random_numbers  = random_numbers,
              runscript       = runscript,
              interface_paths = interface_paths, 
              output_root     = output_root,
              baseline_root   = baseline_root,
              pct_sample      = pct_sample,
              sample_ids      = sample_ids, 
              detail_vars     = detail_vars,
              multicore       = multicore))
}



get_scenario_info = function(id) {
  
  #----------------------------------------------------------------------------
  # Given a scenario ID, retrieves and transforms scenario-specific runtime
  # arguments and interface file paths.
  # 
  # Parameters:
  #   - id (str) : scenario ID 
  #
  # Returns: list of 3: 
  #   - id (str)                 : scenario ID
  #   - tax_law_id (str)         : str
  #   - output_path (str)        : path to root of output folder
  #   - interface_paths (list)   : list of scenario-specific interface paths
  #   - years (int[])            : years to run
  #   - dist_years (int[])       : years for which to generate microdata output
  #                                and distribution tables 
  #   - mtr_vars (str[])         : variables to calculate MTRs for
  #   - mtr_vars (str[])         : MTR types (same index as mtr_vars)
  #   - behavior_modules (str[]) : names of behavioral feedback modules to run
  #----------------------------------------------------------------------------
  
  # Scenario-specific output paths
  output_root = file.path(ifelse(id == 'baseline', 
                                 globals$baseline_root, 
                                 globals$output_root), 
                          id)
  for (type in c('static', 'conventional')) {
    dir.create(file.path(output_root, type, 'detail'),       
               recursive    = T, 
               showWarnings = F)
    dir.create(file.path(output_root, type, 'totals'),
               recursive    = T, 
               showWarnings = F)
    dir.create(file.path(output_root, type, 'supplemental'),
               recursive    = T, 
               showWarnings = F)
  }
  dir.create(file.path(output_root, 'static/supplemental/child_earnings'), 
             showWarnings = F)
  
  # List of interface paths, named by interface
  interface_paths = globals$interface_paths %>% 
    filter(ID == id) %>% 
    distinct(interface, path) %>% 
    pivot_wider(names_from  = interface, 
                values_from = path) %>% 
    as.list()
  
  # List of scenario-specific runscript, named by column name
  runscript_items = globals$runscript %>% 
    filter(ID == id) %>% 
    as.list()
  
  # Name of tax law scenario
  tax_law_id = runscript_items$tax_law
  
  # Behavioral feedback module names (formatted as {var}/{module})
  behavior_modules = NULL
  if (!is.na(runscript_items$behavior)) {
    behavior_modules = str_split_1(runscript_items$behavior, ' ')
  }
  
  # Years to run. Parse based on format supplied
  years_input = as.character(runscript_items$years)
  if (str_detect(years_input, ':')) {
    years_input = str_split_1(years_input, ':') 
  }
  if (length(years_input) == 1) {
    years = c(as.integer(years_input))
  } else if (length(years_input) == 2) {
    years = as.integer(years_input[1]):as.integer(years_input[2])
  } else {
    stop('Invalid input for years column in runscript_items')
  }
  
  # Distribution table and microdata output years
  if (is.na(runscript_items$dist_years)) {
    dist_years = years
  } else if (str_detect(as.character(runscript_items$dist_years), ':')) {
    dist_years_input = str_split_1(as.character(runscript_items$dist_years), ':')
    dist_years = as.integer(dist_years_input[1]):as.integer(dist_years_input[2]) 
  } else {
    dist_years = runscript_items$dist_years %>%
      as.character() %>%
      str_split_1(' ') %>% 
      as.integer()
  }
  
  # Names of variables for which to calculate marginal tax rates
  mtr_vars = NULL
  if (!is.na(runscript_items$mtr_vars)) {
    mtr_vars = str_split_1(runscript_items$mtr_vars, ' ')
  }
   
  # Types of MTRs, with same index as MTR vars above
  mtr_types = NULL
  if (!is.na(runscript_items$mtr_types)) {
    mtr_types = str_split_1(runscript_items$mtr_types, ' ')
  }
   
  # Return as named list
  return(list(ID               = id,
              output_path      = output_root,
              interface_paths  = interface_paths,
              tax_law_id       = tax_law_id,
              behavior_modules = behavior_modules, 
              years            = years, 
              dist_years       = dist_years,
              mtr_vars         = mtr_vars,
              mtr_types        = mtr_types))
}



