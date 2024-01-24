#------------------------------------------------------------
# generate_params.R 
#
# Generates combinations of tax law parameter files for 2024
# launch interactive simulator tool
#------------------------------------------------------------

library(tidyverse)
library(yaml)

#------------------------
# Set runtime parameters 
#------------------------

# Project name 
project_name = 'tcja_simulator'

# output folders
output_runscripts = './config/runscripts/policy_runs/tcja/simulator'
output_taxlaw     = './config/scenarios/tax_law/policy_runs/tcja_ext/interactive'

# Run script defaults
tax_data_vintage          = 2024010800
tax_data_id               = 'baseline'
macro_projections_vintage = 2023121116 
macro_projections_id      = 'baseline'
corp_tax_vintage          = 2024010916
estate_tax_vintage        = 2024011917
estate_tax_id             = 'baseline'
tax_law_root              = 'policy_runs/tcja_ext/interactive/'
behavior                  = NA
years                     = '2023:2053'
dist_years                = '2026'
mtr_vars                  = NA
mtr_types                 = NA


#---------------------
# Parse scenario info
#---------------------

# Get root containing parameters
project_root_input = file.path('./other/param_generator/input/', project_name)

# Read and copy scenario metadata
scenario_map = read_yaml(file.path(project_root_input, 'scenario_map.yaml'))
file.copy(from      = file.path(project_root_input, 'scenario_map.yaml'), 
          to        = file.path(output_taxlaw, 'scenario_map.yaml'), 
          overwrite = T)


# Read tax law files
raw_yaml = list()
for (dim_name in names(scenario_map)) {
  
  # Skip if no on-model runs
  on_model_scenarios = scenario_map[[dim_name]]$on_model
  if (is.null(on_model_scenarios)) {
    next
  }
   
  # Read and parse YAML files
  raw_yaml[[dim_name]] = list()
  for (scenario in names(on_model_scenarios)) {
    file_names = list.files(file.path(project_root_input, dim_name, scenario)) 
    raw_yaml[[dim_name]][[scenario]] = map(
      .x = file_names, 
      .f = ~ read_yaml(file.path(project_root_input, dim_name, scenario, .x))
    ) %>% 
      set_names(file_names)
  }
}


#------------------------------
# Generate policy combinations
#------------------------------

# Build combinations
combo_ids = scenario_map %>% 
  map(.f = ~ names(.x$on_model)) %>% 
  do.call(what = expand_grid, 
          args = .) %>% 
  mutate(id = do.call(what = paste0, 
                      args = .), 
         .before = everything())

#--------------------------
# Build tax law parameters
#--------------------------

# Define helper function to add YAML to master list 
add_yaml = function(to_add, dest) { 
  
  # Skip if nothing to add (i.e. current law)
  if (length(to_add) == 0) {
    return(dest)
  }
  
  # Skip if no YAML files (i.e. current law)
  for (file_name in names(to_add)) {
    if (file_name %in% names(dest)) {
      dest[[file_name]] = append(dest[[file_name]], to_add[[file_name]]
      ) 
    } else {
      dest[[file_name]] = to_add[[file_name]]
    }
  }
  
  return(dest)
}


# Loop over scenarios
for (i in 1:nrow(combo_ids)) {

  # Extract scenario parameters in list form
  scenario_info = as.list(combo_ids[i, 2:ncol(combo_ids)])
  
  # Initialize list of yaml files for this scenario 
  yaml_files = list()
  
  # Loop over dimensions 
  for (dim_name in names(scenario_info)) {
    yaml_files = add_yaml(
      to_add = raw_yaml[[dim_name]][[scenario_info[[dim_name]]]], 
      dest   = yaml_files 
    )
  }
  
  # Write all files 
  id = combo_ids$id[i]
  if (i == 1) {
    id = 'baseline'
  }
  scenario_root = file.path(output_taxlaw, id)
  dir.create(scenario_root, showWarnings = F)
  for (file_name in names(yaml_files)) {
    write_yaml(
      x    = yaml_files[[file_name]], 
      file = file.path(scenario_root, file_name)
    )
  }
}


#------------------
# Build runscripts
#------------------

# Initialize template
runscripts = expand_grid(
  ID                                = combo_ids$id, 
  `dep.Tax-Data.vintage`            = tax_data_vintage, 
  `dep.Tax-Data.ID`                 = tax_data_id,
  `dep.Macro-Projections.vintage`   = macro_projections_vintage,
  `dep.Macro-Projections.ID`        = macro_projections_id,
  `dep.Corporate-Tax-Model.vintage` = corp_tax_vintage,
  `dep.Estate-Tax-Model.vintage`    = estate_tax_vintage,
  `dep.Estate-Tax-Model.ID`         = estate_tax_id,
  tax_law                           = tax_law_root,
  behavior                          = behavior,
  years                             = years, 
  dist_years                        = dist_years, 
  mtr_vars                          = mtr_vars,
  mtr_types                         = mtr_types
) %>% 
  mutate(tax_law    = if_else(row_number() == 1, 'baseline', paste0(tax_law, ID)),
         dist_years = if_else(row_number() == 1, years, dist_years)) %>% 
  mutate(`dep.Corporate-Tax-Model.ID` = paste0(str_sub(ID , start = -3, end = -2), 
                                               '_', 
                                               str_sub(ID, start = -1)), 
         .after = `dep.Corporate-Tax-Model.vintage`) %>% 
  mutate(ID = if_else(row_number() == 1, 'baseline', ID)) %>% 
  write_csv(file.path(output_runscripts, 'interactive_simulator_runs.csv'))


