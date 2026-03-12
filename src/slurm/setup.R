#-----------------------------------------------------------------------
# setup.R
#
# Phase 0 of SLURM pipeline: parses globals, builds per-scenario
# configurations (tax law, indexes, offsets), and serializes everything
# to .rds files for downstream SLURM array jobs. Runs synchronously on
# the login node.
#
# CLI args: same as main.R except multicore
#   Rscript src/slurm/setup.R <runscript> <scenario_id> <user_id>
#           <local> <vintage> <pct_sample> <stacked> <baseline_vintage>
#           <delete_detail>
#-----------------------------------------------------------------------



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
  walk(.f = ~ {
    if (!(.x %in% c('main.R')) && !startsWith(.x, 'slurm/')) {
      source(file.path('./src/', .x))
    }
  })


#------------------------
# Set runtime parameters
#------------------------

args = commandArgs(trailingOnly = T)
if (length(args) < 9) {
  stop('Usage: Rscript src/slurm/setup.R <runscript> <scenario_id> <user_id> ',
       '<local> <vintage> <pct_sample> <stacked> <baseline_vintage> <delete_detail>')
}

runscript_name  = args[1]
scenario_id     = if (args[2] == 'NULL') NULL else args[2]
user_id         = args[3]
local           = as.integer(args[4])
vintage         = if (args[5] == 'NULL') NULL else args[5]
pct_sample      = as.numeric(args[6])
stacked         = as.integer(args[7])
baseline_vintage = if (args[8] == 'NULL') NULL else args[8]
delete_detail   = as.integer(args[9])


#--------------------------------------
# Parse globals and build scenario data
#--------------------------------------

# Redirect stdout to stderr so stray print() calls from sourced functions
# don't corrupt the metadata output parsed by slurm_run.sh
sink(stderr())

# Always set multicore to 'none' — workers never do nested parallelism
globals = parse_globals(
  runscript_name   = runscript_name,
  scenario_id      = scenario_id,
  local            = local,
  vintage          = vintage,
  baseline_vintage = baseline_vintage,
  pct_sample       = pct_sample,
  multicore        = 'none'
)

# Get counterfactual scenario IDs
counterfactual_ids = globals$runscript %>%
  filter(ID != 'baseline') %>%
  get_vector('ID')

# Determine which scenarios need configs built
has_baseline = is.null(baseline_vintage)
scenarios_to_build = counterfactual_ids
if (has_baseline) {
  scenarios_to_build = c('baseline', counterfactual_ids)
}

# Create staging directory
staging_dir = file.path(globals$output_root, '_slurm_staging')
dir.create(staging_dir, recursive = T, showWarnings = F)
dir.create(file.path(staging_dir, 'logs'), showWarnings = F)


#---------------------------------
# Build and serialize per-scenario
# configs (tax law, indexes, etc.)
#---------------------------------

for (sid in scenarios_to_build) {

  # Create scenario staging subdirectory
  scenario_staging = file.path(staging_dir, sid)
  dir.create(scenario_staging, recursive = T, showWarnings = F)

  # Get scenario info (also creates output directories)
  scenario_info = get_scenario_info(sid)

  # Calculate offsets
  vat_price_offset = get_vat_price_offset(
    macro_root = scenario_info$interface_paths$`Macro-Projections`,
    vat_root   = scenario_info$interface_paths$`Value-Added-Tax-Model`,
    years      = scenario_info$years
  )

  excess_growth_offset = get_excess_growth_offset(
    excess_growth = scenario_info$excess_growth,
    start_year    = scenario_info$excess_growth_start_year,
    years         = scenario_info$years
  )

  # Generate indexes
  indexes = generate_indexes(
    macro_root           = scenario_info$interface_paths$`Macro-Projections`,
    vat_price_offset     = vat_price_offset,
    excess_growth_offset = excess_growth_offset
  )

  # Build tax law (also writes tax_law.csv to supplemental as side effect)
  tax_law = build_tax_law(scenario_info, indexes)

  # Serialize config
  saveRDS(
    list(scenario_info        = scenario_info,
         tax_law              = tax_law,
         indexes              = indexes,
         vat_price_offset     = vat_price_offset,
         excess_growth_offset = excess_growth_offset),
    file.path(scenario_staging, 'config.rds')
  )
}


#-----------------------------------------
# If baseline_vintage provided, load and
# serialize existing baseline MTRs
#-----------------------------------------

if (!has_baseline) {
  # Need years from first counterfactual's scenario info
  cf_years = get_scenario_info(counterfactual_ids[1])$years

  baseline_mtrs = cf_years %>%
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

  # Save to baseline subdirectory
  dir.create(file.path(staging_dir, 'baseline'), recursive = T, showWarnings = F)
  saveRDS(baseline_mtrs, file.path(staging_dir, 'baseline', 'baseline_mtrs.rds'))
}


#------------------
# Build manifest
#------------------

manifest = tibble(phase = integer(), scenario = character(), year = integer())

# Phase 1: baseline year tasks (skip if baseline_vintage provided)
if (has_baseline) {
  baseline_info = get_scenario_info('baseline')
  manifest = manifest %>%
    bind_rows(tibble(
      phase    = 1L,
      scenario = 'baseline',
      year     = baseline_info$years
    ))
}

# Phase 2: counterfactual × year tasks
for (sid in counterfactual_ids) {
  si = get_scenario_info(sid)
  manifest = manifest %>%
    bind_rows(tibble(
      phase    = 2L,
      scenario = sid,
      year     = si$years
    ))
}


#-----------------------------------------
# Serialize shared state for workers
#-----------------------------------------

saveRDS(globals,            file.path(staging_dir, 'globals.rds'))
saveRDS(return_vars,        file.path(staging_dir, 'return_vars.rds'))
saveRDS(counterfactual_ids, file.path(staging_dir, 'counterfactual_ids.rds'))
saveRDS(manifest,           file.path(staging_dir, 'manifest.rds'))

saveRDS(
  list(stacked       = stacked,
       delete_detail = delete_detail),
  file.path(staging_dir, 'runtime_args.rds')
)


#--------------------------------------
# Print metadata for slurm_run.sh to
# parse via eval
#--------------------------------------

# Restore stdout for metadata output
sink()

n_phase1    = sum(manifest$phase == 1)
n_phase2    = sum(manifest$phase == 2)
n_scenarios = length(counterfactual_ids)

cat(paste0('STAGING_DIR="', staging_dir, '"\n'))
cat(paste0('N_PHASE1=',    n_phase1,    '\n'))
cat(paste0('N_PHASE2=',    n_phase2,    '\n'))
cat(paste0('N_SCENARIOS=', n_scenarios, '\n'))
cat(paste0('STACKED=',     stacked,     '\n'))
