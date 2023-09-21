#-----------------------------------------------------------------------
# config_parser.R 
# 
# Contains functions to parse runtime options and build interface paths
#-----------------------------------------------------------------------



parse_globals = function(runscript_path) {
  
  #----------------------------------------------------------------------------
  # Parses data interface versioning requirements and runscript; generates 
  # scenario-specific, version-consistent filepaths for data interfaces. 
  # Confirms that these filepaths exist. 
  # 
  # Parameters:
  #   - runscript_path (str) : filepath for runscript CSV file 
  #
  # Returns: list of 2: runtime_args (df), a tibble representation of the 
  #          runscripts CSV; and interface_paths (df), a tibble with ID-
  #          interface-filepath info in rows 
  #----------------------------------------------------------------------------
  
  # Set directory for shared data folder
  data_root = '/gpfs/gibbs/project/sarin/shared/'
  
  # Read and parse data dependency interface file paths
  interface_versions = read_yaml('./interface_versions.yaml') %>% 
    map2(.x = ., 
         .y = names(.), 
         .f = ~ file.path(data_root, .x$type, .y, paste0('v', .x$version))) %>% 
    as_tibble() %>% 
    pivot_longer(cols      = everything(), 
                 names_to  = 'interface', 
                 values_to = 'path')
  
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
              interface_paths = interface_paths))
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
  #   - ID (int)               : scenario ID
  #   - config_path (str)      : path to scenario config folder
  #   - interface_paths (list) : list of scenario-specific interface paths
  #   - years (int[])          : years to run
  #   - mtr_vars (str[])       : variables to calculate MTRs for
  #---------------------------------------------------------------------------
  
  # Scenario-specific configuration path
  subfolder = 'counterfactuals'
  if (id == 'baseline') {
    subfolder = ''
  }
  config_path = file.path('./config/scenarios', subfolder, id)
  
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
  
  # Vector of years to run
  years = runtime_args$first_year:runtime_args$last_year
  
  # Vector of names of variables for which to calculate marginal tax rates
  mtr_vars = runtime_args$mtr_vars %>%
    str_split_1(' ')
  
  # Return as named list
  return(list(ID              = id,
              config_path     = config_path, 
              interface_paths = interface_paths,
              years           = years, 
              mtr_vars        = mtr_vars))
}



