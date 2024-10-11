#-----------------------------------------
# main.R
#
# Main entry point into the tax simulator
#-----------------------------------------



#---------------------
# Configure simulator
#---------------------

# Load required packages
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = T)    
  ))
)

# Source all function scripts
return_vars = list()
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Get user id 
user_id = get_user_id()


#------------------------
# Set runtime parameters
#------------------------

runscript_names  = 'public/auto/auto'
scenario_id      = NULL
local            = 0
vintage          = NULL
pct_sample       = 1
stacked          = 1
baseline_vintage = NULL
delete_detail    = 0
multicore        = 0


# Override default runtime args if executed from the command line
args = commandArgs(trailingOnly = T)
if (length(args) > 0) {
  runscript_names                         = args[1]
  if (args[2] == 'NULL') scenario_id      = NULL else scenario_id = args[2]
  user_id                                 = args[3]
  local                                   = as.integer(args[4])
  if (args[5] == 'NULL') vintage          = NULL else vintage = args[5]
  pct_sample                              = as.integer(args[6])
  stacked                                 = as.integer(args[7])
  if (args[8] == 'NULL') baseline_vintage = NULL else baseline_vintage = args[8]
  delete_detail                           = args[9]
  multicore                               = args[10]
} 


#----------------------
# Runscript-level loop
#----------------------

# Runscript names are separated by four underscores
for (runscript_name in str_split_1(runscript_names, '____')) {
  
  # Set global (scenario-independent) variables
  globals = parse_globals(
    runscript_name   = runscript_name,
    scenario_id      = scenario_id,
    local            = local, 
    vintage          = vintage, 
    baseline_vintage = baseline_vintage,
    pct_sample       = pct_sample
  )
  
  # Get list of non-baseline scenarios 
  counterfactual_ids = globals$runscript %>% 
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
            select(id, year, starts_with('mtr_')) %>% 
            return()) %>%
      bind_rows() 
  }
  
  # Run counterfactual scenarios
  if (multicore == 1) {
    mc_out = mclapply(X        = counterfactual_ids, 
                      FUN      = do_scenario, baseline_mtrs, 
                      mc.cores = min(16, detectCores(logical = F)))
  } else {
    walk(.x = counterfactual_ids, 
         .f = ~ do_scenario(.x, baseline_mtrs))
  }
  
  
  #---------------------------------------
  # Stacked post-processing and reporting
  #---------------------------------------
  
  print('Running stacked post-processing routines')
  
  if (stacked == 1) {
    build_stacked_1040_reports(counterfactual_ids)
    calc_stacked_rev_est(counterfactual_ids)
  }
  
  # Delete microdata files
  if (delete_detail == 1) {
    purge_detail()
  }
}


