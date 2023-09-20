#-----------------------------------------
# Main entry point into the tax simulator
#-----------------------------------------



#---------------------
# Configure simulator
#---------------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)


# Set global (scenario-independent) variables
source('./src/misc/config_parser.R')
runscript_path = './config/runscripts/baseline.csv' # TODO take as cmd line arg
globals = parse_globals(runscript_path)


#------------------------------------------------------------------------------
# TODO loop logic that proceeds by scenario ID....first, parse scenario params
#------------------------------------------------------------------------------

id = 'baseline'

scenario = get_scenario_info(globals, id)


#-----------
# Load data 
#-----------

# Placeholder! Ugly! Reads historical inflation series for tax law generation
indexes = read_csv(file.path(scenario$interface_paths$`Inflation-Data`, 'CPIAUCNS.csv')) %>% 
  mutate(Year = year(DATE), Month = month(DATE)) %>% 
  select(Year, Month, cpi = CPIAUCNS) %>% 
  left_join(read_csv(file.path(scenario$interface_paths$`Inflation-Data`, 'SUUR0000SA0.csv')) %>% 
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
  bind_rows(read_csv(file.path(scenario$interface_paths$`Inflation-Data`, 'awi.csv'))) %>%
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
                        years       = scenario$years, 
                        indexes     = indexes)


#-----------------
# Calculate taxes
#-----------------

source('./src/calc/utils.R')

#----------------------
# Produce output files
#----------------------



