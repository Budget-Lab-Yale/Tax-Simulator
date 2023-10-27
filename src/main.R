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
runscript_name   = 'kg_2pp' 
user_id          = 'jar335'
local            = 1
vintage          = '2023102714'
pct_sample       = 1
stacked          = 1
baseline_vintage = '2023102714'

# Set global (scenario-independent) variables
globals = parse_globals(runscript_name   = runscript_name, 
                        user_id          = user_id, 
                        local            = local, 
                        vintage          = vintage, 
                        baseline_vintage = baseline_vintage,
                        pct_sample       = pct_sample)

# Get list of non-baseline scenarios 
counterfactual_ids = globals$runtime_args %>% 
  filter(ID != 'baseline') %>% 
  get_vector('ID')


#---------------
# Run scenarios
#---------------

# Run baseline if specified
if (is.null(baseline_vintage)) {
  baseline_mtrs = do_scenario('baseline')  
  
# Otherwise, load baseline marginal tax rates 
} else{
  baseline_mtrs = get_scenario_info(counterfactual_ids[1])$years %>% 
    map(.f = ~ globals$baseline_root %>%  
          file.path('baseline/static/detail', paste0(.x, '.csv')) %>%
          fread() %>% 
          tibble() %>% 
          mutate(year = .x) %>% 
          return()) %>%
    bind_rows() %>% 
    select(id, year, starts_with('mtr_'))
}


# Run counterfactuals 
walk(.f = do_scenario, 
     .x = counterfactual_ids, 
     baseline_mtrs = baseline_mtrs)


#-----------------
# Post-processing
#-----------------

# Calculate revenue estimates
calc_rev_est(counterfactual_ids)

# Calculate stacked revenue estimates
if (stacked == 1) {
  calc_stacked(counterfactual_ids)
}

# Calculate distributional estimates
counterfactual_ids %>% 
  map(calc_distribution)

