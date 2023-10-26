#-----------------------------------------
# Main entry point into the tax simulator
#-----------------------------------------



#---------------------
# Configure simulator
#---------------------

# Set random number generator seed
set.seed(76)

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Source all function scripts
return_vars = list()
list.files('./src', recursive = T) %>% 
  map(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# cmd line args TODO
runscript_name = 'kg_28' 
user_id    = 'jar335'
local      = 1
vintage    = NULL
pct_sample = 0.02
stacked    = 1

# Set global (scenario-independent) variables
globals = parse_globals(runscript_name = runscript_name, 
                        user_id        = user_id, 
                        local          = local, 
                        vintage        = vintage, 
                        pct_sample     = pct_sample)

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
calc_rev_est(counterfactual_ids = counterfactual_ids, 
             global_root        = globals$output_root)

# Calculate stacked revenue estimates
if (stacked == 1) {
  calc_stacked(counterfactual_ids = counterfactual_ids, 
               global_root        = globals$output_root)
}

