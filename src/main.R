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

# Source all function scripts. Behavior modules are loaded by path at scenario
# time (see load_behavior_module), so sourcing them here would leave whichever
# variant came last defining do_{family}.
return_vars = list()
list.files('./src', recursive = T) %>%
  walk(.f = ~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/') && !startsWith(.x, 'behavior/')) source(file.path('./src/', .x)))


#------------------------
# Set runtime parameters
#------------------------


runscript_names  = 'baseline/baseline'
scenario_id      = NULL
local            = 1
vintage          = NULL
pct_sample       = 1
stacked          = 1
baseline_vintage = NULL
delete_detail    = 0
multicore        = 'none'   # one of 'none', 'scenario', or 'year'


# Override default runtime args if executed from the command line
args = commandArgs(trailingOnly = T)
if (length(args) > 0) {
  cli              = parse_cli_args(args, context = 'main')
  runscript_names  = cli$runscript_names
  scenario_id      = cli$scenario_id
  local            = cli$local
  vintage          = cli$vintage
  pct_sample       = cli$pct_sample
  stacked          = cli$stacked
  baseline_vintage = cli$baseline_vintage
  delete_detail    = cli$delete_detail
  multicore        = cli$multicore
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
    pct_sample       = pct_sample, 
    multicore        = multicore
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
      map(
        ~ globals$baseline_root %>%  
          file.path('baseline/static/detail', paste0(.x, '.csv')) %>%
          fread() %>% 
          tibble() %>% 
          mutate(year = .x) %>%
          select(id, year, starts_with('mtr_')) %>% 
          return()
      ) %>%
      bind_rows() 
  }
  
  # Run counterfactual scenarios
  if (globals$multicore == 'scenario') {
    mc_out = mclapply(
      X        = counterfactual_ids, 
      FUN      = do_scenario, baseline_mtrs, 
      mc.cores = min(32, detectCores(logical = F))
    )
  } else {
    walk(.x = counterfactual_ids, .f = ~ do_scenario(.x, baseline_mtrs))
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


