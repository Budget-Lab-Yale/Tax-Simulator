#-----------------------------------------
# Main entry point into the tax simulator
#-----------------------------------------
  


#---------------------
# Configure simulator
#---------------------


# Load required packages
lapply(readLines('./requirements.txt'), library, character.only = T)

# Source all function scripts
return_vars = list()
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Parse command line arguments
args             = commandArgs()
runscript_name   = 'tests/charity'
user_id          = 'jar335' 
local            = 1
vintage          = NULL       
pct_sample       = 0.2
stacked          = 0
baseline_vintage = NULL

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


#-------------------------------
# Post-processing and reporting
#-------------------------------

# Generate 1040 reports
create_1040_reports(counterfactual_ids)
if (stacked == 1) {
  create_stacked_1040_reports(counterfactual_ids)
}

# Generate revenue estimates
calc_rev_est(counterfactual_ids)
if (stacked == 1) {
  calc_stacked_rev_est(counterfactual_ids)
}

# Generate distributional estimates
build_all_distribution_tables(counterfactual_ids)
if (stacked == 1) {
  build_all_stacked_distribution_tables(counterfactual_ids)
}

