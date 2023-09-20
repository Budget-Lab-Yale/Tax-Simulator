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


#---------------
# Run scenarios
#---------------

# Run baseline
baseline = run_scenario('baseline')

# Run counterfactuals
# TODO walk(globals$runtime_args$ID, run_scenario)


#-----------------
# Post-processing
#-----------------

# Calculate revenue estimates
# TODO calc_rev_est()
# TODO calc_stacked()


