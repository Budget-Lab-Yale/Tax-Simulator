#-----------------------------------------
# Main entry point into the tax simulator
#-----------------------------------------



#---------------------
# Configure simulator
#---------------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Set global params
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

# Read runscripts (TODO -- set path in command line args)
runscript_path = './config/runscripts/baseline.csv'
runtime_args   = read_csv(runscript_path)

# Create filepaths for data interfaces
interface_paths = runtime_args %>% 
  select(ID, starts_with('data.')) %>% 
  pivot_longer(cols         = -ID,
               names_to     = 'interface', 
               names_prefix = 'data.', 
               values_to    = 'vintage') %>% 
  left_join(interface_versions, by = 'interface') %>% 
  mutate(path = file.path(path, vintage))

#------------------------------------------------------------------------------
# TODO loop logic that proceeds by scenario ID....first, parse scenario params
#------------------------------------------------------------------------------

scenario_ID = 1

scenario_interface_paths = interface_paths %>% 
  filter(ID == scenario_ID) %>% 
  distinct(interface, path) %>% 
  pivot_wider(names_from  = interface, 
              values_from = path) %>% 
  as.list()

scenario_runtime_args = runtime_args %>% 
  filter(ID == scenario_ID) %>% 
  as.list()

years = scenario_runtime_args$first_year:scenario_runtime_args$last_year

#-----------
# Load data 
#-----------

# Placeholder! Ugly! Reads historical inflation series for tax law generation
indexes = read_csv(file.path(scenario_interface_paths$inflation_data, 'CPIAUCNS.csv')) %>% 
  mutate(Year = year(DATE), Month = month(DATE)) %>% 
  select(Year, Month, cpi = CPIAUCNS) %>% 
  left_join(read_csv(file.path(scenario_interface_paths$inflation_data, 'SUUR0000SA0.csv')) %>% 
              mutate(Year = year(DATE), Month = month(DATE)) %>% 
              select(Year, Month, chained_cpi = SUUR0000SA0), 
            by = c('Year', 'Month')) %>% 
  mutate(FY = if_else(Month < 9, Year, Year + 1)) %>% 
  filter(FY > min(FY) + 1) %>% 
  select(-Year) %>% 
  pivot_longer(cols      = -c(FY, Month), 
               names_to  = 'Series', 
               values_to = 'Value') %>% 
  group_by(Series, Year = FY) %>% 
  summarise(Value = mean(Value)) %>% 
  bind_rows(read_csv(file.path(scenario_interface_paths$inflation_data, 'awi.csv'))) %>%
  group_by(Series) %>% 
  mutate(Growth = Value / lag(Value) - 1) %>% 
  select(-Value) %>% 
  filter(!is.na(Growth))


#---------------
# Build tax law
#---------------

# Load tax law functions
source('./src/misc/utils.R')
source('./src/data/tax_law/tax_law.R')

# Build baseline tax law 
tax_law = build_tax_law(config_path = './config/tax_law/baseline/tax_law/baseline', 
                        years       = years, 
                        indexes     = indexes)


#----------------------
# Produce output files
#----------------------



