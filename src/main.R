#-----------------------------------------
# Main entry point into the tax simulator
#-----------------------------------------



#---------------------
# Configure simulator
#---------------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Configure settings
years = 2014:2024


#-----------
# Load data 
#-----------

# Placeholder! Reads historical inflation series for tax law generation
indexes = read_csv('./resources/CPIAUCNS.csv') %>% 
  mutate(Year = year(DATE), Month = month(DATE)) %>% 
  select(Year, Month, cpi = CPIAUCNS) %>% 
  left_join(read_csv('./resources/SUUR0000SA0.csv') %>% 
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
  bind_rows(read_csv('./resources/awi.csv')) %>%
  group_by(Series) %>% 
  mutate(Growth = Value / lag(Value) - 1) %>% 
  select(-Value) %>% 
  filter(!is.na(Growth))


#---------------
# Run scenarios
#---------------

# Load tax law functions
source('./src/misc/utils.R')
source('./src/data/tax_law/tax_law.R')

# Build baseline tax law 
tax_law = build_tax_law(config_path = './config/policy/baseline/tax_law/baseline', 
                        years       = years, 
                        indexes     = indexes)


#----------------------
# Produce output files
#----------------------



