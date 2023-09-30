#-----------------------------------------
# Main entry point into the tax simulator
#-----------------------------------------



#---------------------
# Configure simulator
#---------------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# cmd line args TODO
runscript_name = 'kg_28' 
user_id = 'jar335'
local = 1

# Set global (scenario-independent) variables
source('./src/misc/utils.R')
source('./src/misc/config_parser.R')
globals = parse_globals(runscript_name, user_id, local)

# Get list of non-baseline scenarios 
counterfactual_ids = globals$runtime_args %>% 
  filter(ID != 'baseline') %>% 
  get_vector('ID')


#---------------
# Run scenarios
#---------------

# Run baseline
baseline_mtrs = do_scenario('baseline')

# Run counterfactuals 
walk(.f = do_scenario, 
     .x = counterfactual_ids, 
     baseline_mtrs = baseline_mtrs)


#-----------------
# Post-processing
#-----------------

# Calculate revenue estimates
calc_rev_est()

# Calculate stacked revenue estimates
calc_stacked()


