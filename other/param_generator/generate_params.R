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
tax_law_root              = 'policy_runs/tcja_ext/interactive/'
behavior                  = NA
first_year                = 2023
last_year                 = 2033
mtr_vars                  = NA
mtr_types                 = NA


# Corporate tax policy mapping to scenario folders 
corp_map = expand_grid(
  corp_rate = c('21', '15', '25', '28'), 
  corp_base = c('current_law', 'current_policy')
) %>% 
  mutate(
    corp_tax_id = paste0( 
      if_else(corp_rate == '21', 'baseline', corp_rate), 
      '_', 
      if_else(corp_base == 'current_law', 'baseline', 'extenders')
    )
  )
   

# Estate tax policy mapping to scenario folders
estate_map = tibble(
  estate        = c('current_law', 'current_policy'),
  estate_tax_id = c('baseline', 'tcja_extension')
)

#---------------------
# Read raw YAML files
#---------------------

# Get root containing parameters
project_root_input = file.path('./other/param_generator/input/', project_name)

# Parse on-model dimensions
dim_names = list.dirs(project_root_input, full.names = F, recursive = F) 
  
# Parse dimension values
dims = dim_names %>% 
  map(.f = ~ file.path(project_root_input, .x) %>% 
               list.dirs(full.names = F, recursive = F)) %>% 
  set_names(dim_names)

# Read yaml files
raw_yaml = list()
for (dim_name in dim_names) {
  raw_yaml[[dim_name]] = list()
  for (dim_value in dims[[dim_name]]) {
    file_names = list.files(file.path(project_root_input, dim_name, dim_value)) 
    raw_yaml[[dim_name]][[dim_value]] = map(
      .x = file_names, 
      .f = ~ read_yaml(file.path(project_root_input, dim_name, dim_value, .x))
    ) %>% 
      set_names(file_names)
  }
}


#------------------------------
# Generate policy combinations
#------------------------------

# Build parameter map
param_map = do.call(expand_grid, dims) %>% 
  expand_grid(corp_map, estate_map) %>% 
  mutate(id = as.character(row_number()), .before = everything(), 
         id = if_else(id == 1, 'baseline', id)) 

# List version
param_map_list = 1:nrow(param_map) %>% 
  map(.f = ~ param_map %>% 
        slice(.x) %>% 
        select(-ends_with('_id')) %>%  
        as.list())

# Write map file
param_map %>% 
  select(-ends_with('_id')) %>% 
  write_csv(file.path(output_taxlaw, 'map.csv'))


#--------------------------
# Build tax law parameters
#--------------------------

# Define helper function to add YAML to master list 
add_yaml = function(to_add, dest) { 
  
  # Skip if nothing to add (i.e. current law)
  if (length(to_add) == 0) {
    return(dest)
  }
  
  # Add # Skip if no YAML files (i.e. current law)
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
for (i in 1:length(param_map_list)) {
  
  # Initialize list of yaml files for this scenario 
  yaml_files = list()
  
  # Loop over dimensions 
  for (dim_name in names(param_map_list[[i]])) {
    if (!(dim_name %in% c('corp_rate', 'corp_base', 'estate')))
    yaml_files = add_yaml(
      to_add = raw_yaml[[dim_name]][[param_map_list[[i]][[dim_name]]]], 
      dest   = yaml_files 
    )
  }
  
  # Write all files 
  if (i == 1) {
    i = 'baseline'
  }
  scenario_root = file.path(output_taxlaw, i)
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
runscript_template = tibble(
  ID                                = NA, 
  `dep.Tax-Data.vintage`            = tax_data_vintage, 
  `dep.Tax-Data.ID`                 = tax_data_id,
  `dep.Macro-Projections.vintage`   = macro_projections_vintage,
  `dep.Macro-Projections.ID`        = macro_projections_id,
  `dep.Corporate-Tax-Model.vintage` = corp_tax_vintage,
  `dep.Estate-Tax-Model.vintage`    = estate_tax_vintage,
  tax_law                           = tax_law_root,
  behavior                          = behavior,
  first_year                        = first_year, 
  last_year                         = last_year, 
  mtr_vars                          = mtr_vars,
  mtr_types                         = mtr_types
)

# Loop over scenarios, create runscript, and write
for (id in param_map$id) {
  
  runscript_template %>% 
    mutate(ID = id, tax_law = paste0(tax_law, id)) %>% 
    left_join(param_map %>% 
                select(ID = id, 
                       `dep.Corporate-Tax-Model.ID` = corp_tax_id,
                       `dep.Estate-Tax-Model.ID`    = estate_tax_id), 
              by = 'ID') %>% 
    relocate(`dep.Corporate-Tax-Model.ID`, .after = `dep.Corporate-Tax-Model.vintage`) %>% 
    relocate(`dep.Estate-Tax-Model.ID`,    .after = `dep.Estate-Tax-Model.vintage`) %>% 
    write_csv(file.path(output_runscripts, paste0(id, '.csv')))
}


