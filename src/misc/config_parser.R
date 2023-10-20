#-----------------------------------------------------------------------
# config_parser.R 
# 
# Contains functions to parse runtime options and build interface paths
#-----------------------------------------------------------------------



parse_globals = function(runscript_name, user_id, local) {
  
  #----------------------------------------------------------------------------
  # Parses data interface versioning requirements and runscript; generates 
  # scenario-specific, version-consistent filepaths for data interfaces. 
  # Confirms that these filepaths exist. 
  # 
  # Parameters:
  #   - runscript_name (str) : name of runscript CSV file 
  #   - user_id (str)        : Yale NetID, used for local runs in which output
  #                            is stored on user-specific scratch folder
  #   - local (int)          : whether this is a local run (1) or a production
  #                            run (0)  
  #
  # Returns: list of 3: 
  #   - runtime_args (df)    : tibble representation of the runscripts CSV
  #   - interface_paths (df) : tibble with ID-interface-filepath info in rows 
  #   - output_root (str)    : path where output data is written
  #----------------------------------------------------------------------------
  
  # Read and parse data dependency interface file paths
  output_roots       = read_yaml('./output_roots.yaml')
  interface_versions = read_yaml('./interface_versions.yaml') %>% 
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
  
  
  # Set model version and vintage
  version = read_yaml('./interface_versions.yaml')$`Tax-Simulator`$version
  st      = Sys.time()
  vintage = paste0(year(st), 
                   month(st) %>%
                     paste0('0', .) %>% 
                     str_sub(-2), 
                   day(st) %>%
                     paste0('0', .) %>% 
                     str_sub(-2), 
                   hour(st) %>%
                     paste0('0', .) %>% 
                     str_sub(-2))
  
  # Determine and create directory for model output
  output_branch = file.path('Tax-Simulator', paste0('v', version), vintage)
  output_root   = file.path(output_roots$production, 'model_data ', output_branch)
  if (local == 1) {
    output_root = file.path(output_roots$local, user_id, output_branch)
  }
  dir.create(output_root, recursive = T, showWarnings = F)
  
  
  # Read runtime arguments 
  runtime_args = runscript_name %>% 
    paste0('.csv') %>%
    file.path('./config/runscripts/', .) %>% 
    read_csv()
    
  
  # Write dependencies CSV; this is a vintage-level file which lists all 
  # other model vintages on which these Tax-Simmulator results are dependent
  dependencies = runtime_args %>% 
    select(TaxSimulatorID = ID, starts_with('dep.')) %>% 
    mutate(across(.cols = everything(),
                  .fns  = as.character)) %>% 
    pivot_longer(cols         = -TaxSimulatorID, 
                 names_prefix = 'dep.', 
                 names_sep    = '[.]', 
                 names_to     = c('interface', 'series')) %>% 
    pivot_wider(names_from  = series, 
                values_from = value) %>% 
    left_join(read_yaml('./interface_versions.yaml') %>% 
                map(~ .$version) %>% 
                as_tibble() %>% 
                pivot_longer(cols      = everything(), 
                             names_to  = 'interface', 
                             values_to = 'version'), 
              by = 'interface') %>% 
    rename(scenario = ID, ID = TaxSimulatorID) %>% 
    relocate(version, .before = vintage) %>% 
    write_csv(file.path(output_root, 'dependencies.csv'))
  
  
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
  
  # Return runtime args and interface paths  
  return(list(runtime_args    = runtime_args,
              interface_paths = interface_paths, 
              output_root     = output_root))
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
  #   - mtr_vars (str[])         : variables to calculate MTRs for
  #   - behavior_modules (str[]) : names of behavioral feedback modules to run
  #   - sample_ids (int[])       : vector of tax unit IDs comprising the
  #                                sample population (all IDs for 100%)
  #----------------------------------------------------------------------------
  
  # Scenario-specific output paths
  output_root = file.path(globals$output_root, id)
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
  
  # List of interface paths, named by interface
  interface_paths = globals$interface_paths %>% 
    filter(ID == id) %>% 
    distinct(interface, path) %>% 
    pivot_wider(names_from  = interface, 
                values_from = path) %>% 
    as.list()
  
  # List of scenario-specific runtime args, named by column name
  runtime_args = globals$runtime_args %>% 
    filter(ID == id) %>% 
    as.list()
  
  # Name of tax law scenario
  tax_law_id = runtime_args$tax_law
  
  # Behavioral feedback module names (formatted as {var}/{module})
  behavior_modules = NULL
  if (!is.na(runtime_args$behavior)) {
    behavior_modules = str_split_1(runtime_args$behavior, ' ')
  }
  
  # Years to run
  years = runtime_args$first_year:runtime_args$last_year
  
  # Names of variables for which to calculate marginal tax rates
  mtr_vars = NULL
  if (!is.na(runtime_args$mtr_vars)) {
    mtr_vars = str_split_1(runtime_args$mtr_vars, ' ')
  }
  mtr_vars = runtime_args$mtr_vars %>%
    str_split_1(' ')
  
  # Tax unit ID in sample
  set.seed(76)
  sample_ids = interface_paths$`Tax-Data` %>%
    read_microdata(years[1]) %>%
    sample_frac(size = runtime_args$pct_sample) %>% 
    get_vector('id')
    
  # Return as named list
  return(list(ID               = id,
              output_path      = output_root,
              interface_paths  = interface_paths,
              tax_law_id       = tax_law_id,
              behavior_modules = behavior_modules, 
              years            = years, 
              mtr_vars         = mtr_vars, 
              sample_ids       = sample_ids))
}



