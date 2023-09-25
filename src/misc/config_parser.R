#-----------------------------------------------------------------------
# config_parser.R 
# 
# Contains functions to parse runtime options and build interface paths
#-----------------------------------------------------------------------



parse_globals = function(runscript_path, user_id, local) {
  
  #----------------------------------------------------------------------------
  # Parses data interface versioning requirements and runscript; generates 
  # scenario-specific, version-consistent filepaths for data interfaces. 
  # Confirms that these filepaths exist. 
  # 
  # Parameters:
  #   - runscript_path (str) : filepath for runscript CSV file 
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
  vintage = paste0(year(st), month(st), day(st), hour(st))
  
  # Determine and create directory for model output
  output_branch = file.path('Tax-Simulator', paste0('v', version), vintage)
  output_root   = file.path(output_roots$production, 'model_data ', output_branch)
  if (local == 1) {
    output_root = file.path(output_roots$local, user_id, output_branch)
  }
  dir.create(output_root, recursive = T)
  
  # Read runtime arguments 
  runtime_args = read_csv(runscript_path)
  
  # Create filepaths for data interfaces
  interface_paths = runtime_args %>% 
    select(TaxSimulatorID = ID, starts_with('dep.')) %>% 
    mutate(across(.cols = everything(),
                  .fns  = as.character)) %>% 
    pivot_longer(cols         = -TaxSimulatorID, 
                 names_prefix = 'dep.', 
                 names_sep    = '[.]', 
                 names_to     = c('interface', 'series')) %>% 
    pivot_wider(names_from  = series, 
                values_from = value) %>%     
    left_join(interface_versions, by = 'interface') %>% 
    mutate(path = file.path(path, vintage, ID)) %>% 
    select(-vintage, -ID, ID = TaxSimulatorID, interface, path)

  # Confirm that each path exists, throwing exception if not
  for (path in interface_paths$path) {
    if (!dir.exists(path)) {
      shorter_path = str_remove(interface_paths$path, data_root)
      msg = paste0("Error: can't find directory '", shorter_path, "'. Confirm ",
                   "the interface version is correct and that the vintage exists.")
      stop(msg)
    }
  }
  
  # Return runtime args and interface paths  
  return(list(runtime_args    = runtime_args,
              interface_paths = interface_paths, 
              output_root     = output_root))
}



get_scenario_info = function(globals, id) {
  
  #----------------------------------------------------------------------------
  # Given a scenario ID, retrieves and transforms scenario-specific runtime
  # arguments and interface file paths.
  # 
  # Parameters:
  #   - globals (list) : globals object, as return by parse_globals()
  #   - id (int)       : scenario ID 
  #
  # Returns: list of 3: 
  #   - ID (int)                 : scenario ID
  #   - config_path (str)        : path to scenario config folder
  #   - output_path (str)        : path to root of output folder
  #   - interface_paths (list)   : list of scenario-specific interface paths
  #   - years (int[])            : years to run
  #   - mtr_vars (str[])         : variables to calculate MTRs for
  #   - behavior_modules (str[]) : names of behavioral feedback modules to run
  #   - sample_ids (int[])       : vector of tax unit IDs comprising the
  #                                sample population (all IDs for 100%)
  #----------------------------------------------------------------------------
  
  # Scenario-specific configuration path
  subfolder = 'counterfactuals'
  if (id == 'baseline') {
    subfolder = ''
  }
  config_path = file.path('./config/scenarios', subfolder, id)
  
  # Scenario-specific output path
  output_root = file.path(globals$output_root, id)
  dir.create(output_root,                            recursive = T)
  dir.create(file.path(output_root, 'detail'),       recursive = T)
  dir.create(file.path(output_root, 'totals'),       recursive = T)
  dir.create(file.path(output_root, 'supplemental'), recursive = T)
  
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
  
  # Years to run
  years = runtime_args$first_year:runtime_args$last_year
  
  # Names of variables for which to calculate marginal tax rates
  mtr_vars = runtime_args$mtr_vars %>%
    str_split_1(' ')
  
  # Behavioral feedback function names
  behavior_fns = NULL
  if (id != 'baseline') {
    behavior_modules = './config/scenarios/counterfactuals/' %>% 
      file.path(id, 'behavior') %>% 
      list.files() %>% 
      str_sub(end = -3)
  }
  
  # Tax unit ID in sample
  set.seed(76)
  sample_ids = tibble(id = 1:100) %>% # TODO read PUF and sample
    sample_frac(size = runtime_args$pct_sample) %>% 
    get_vector('id')
    
  # Return as named list
  return(list(ID               = id,
              config_path      = config_path, 
              output_path      = output_root,
              interface_paths  = interface_paths,
              years            = years, 
              mtr_vars         = mtr_vars, 
              behavior_modules = behavior_modules, 
              sample_ids       = sample_ids))
}



