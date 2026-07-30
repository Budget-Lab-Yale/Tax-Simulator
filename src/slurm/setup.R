#-----------------------------------------------------------------------
# setup.R
#
# Phase 0 of SLURM pipeline: parses globals, builds per-scenario
# configurations (tax law, indexes, offsets), and serializes everything
# to .rds files for downstream SLURM array jobs. Runs synchronously on
# the login node.
#
# CLI args: same as main.R except multicore (parsed by parse_cli_args), plus a
# trailing years-per-task batch size supplied by slurm_run.sh
#   Rscript src/slurm/setup.R <runscript> <scenario_id> <local> <vintage>
#           <pct_sample> <stacked> <baseline_vintage> <delete_detail>
#           [years_per_task]
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

# Source all function scripts. Behavior modules are loaded by path at scenario
# time, for the reason main.R gives.
return_vars = list()
list.files('./src', recursive = T) %>%
  walk(.f = ~ {
    if (!(.x %in% c('main.R')) && !startsWith(.x, 'slurm/') &&
        !startsWith(.x, 'tests/') && !startsWith(.x, 'behavior/')) {
      source(file.path('./src/', .x))
    }
  })


#------------------------
# Set runtime parameters
#------------------------

args = commandArgs(trailingOnly = T)
cli  = parse_cli_args(head(args, 8), context = 'slurm_setup')

# How many consecutive years one array task of a per-year phase runs. Scheduling
# plumbing rather than a modelling choice, so it stays out of the vintage
# manifest, which records what the run assumed.
years_per_task = if (length(args) >= 9) suppressWarnings(as.integer(args[9])) else 1L
if (is.na(years_per_task) || years_per_task < 1) {
  stop('years_per_task must be a positive integer, got "', args[9], '"')
}

runscript_name  = cli$runscript_names
if (grepl('____', runscript_name, fixed = TRUE)) {
  stop('Multi-runscript invocations (____-separated) are not supported by ',
       'the SLURM pipeline; submit one slurm_run.sh per runscript')
}
scenario_id      = cli$scenario_id
local            = cli$local
vintage          = cli$vintage
pct_sample       = cli$pct_sample
stacked          = cli$stacked
baseline_vintage = cli$baseline_vintage
delete_detail    = cli$delete_detail

# Check the keyword before anything is submitted, so a typo fails on the login
# node rather than in Phase 2W on a compute node hours later
invisible(resolve_detail_purge(delete_detail))


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

  # Get scenario info and create the output directory tree, which build_tax_law
  # below writes into
  scenario_info = get_scenario_info(sid)
  ensure_scenario_dirs(scenario_info)

  # Activate this scenario's legs before anything reads a value: build_tax_law
  # reads configuration, and so do the channel predicates below that decide which
  # phases are emitted.
  config_activate(economy  = scenario_info$resolved_economy,
                  behavior = scenario_info$resolved_behavior)

  # Calculate offsets
  vat_price_offset = get_vat_price_offset(
    macro_root = scenario_info$interface_paths$`Macro-Projections`,
    vat_root   = scenario_info$interface_paths$`Value-Added-Tax-Model`,
    years      = scenario_info$years
  )

  # Generate indexes
  indexes = generate_indexes(
    macro_root       = scenario_info$interface_paths$`Macro-Projections`,
    vat_price_offset = vat_price_offset
  )

  # Build tax law, which also writes tax_law.csv to supplemental
  tax_law = build_tax_law(scenario_info, indexes)

  # Baseline law over the same years, which the mechanical pass prices its second
  # set of marginal rates under. Only that pass reads it
  tax_law_baseline = NULL
  if (scenario_runs_mechanical(scenario_info) &&
      !is.null(scenario_info$mtr_vars)) {
    tax_law_baseline = build_baseline_tax_law(scenario_info, indexes)
  }

  # Serialize config
  saveRDS(
    list(scenario_info    = scenario_info,
         tax_law          = tax_law,
         tax_law_baseline = tax_law_baseline,
         indexes          = indexes,
         vat_price_offset = vat_price_offset),
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
      ~ {
        # Read id and the MTR columns only, resolved from the header. Detail
        # files run to 98 columns and 150MB
        path = globals$baseline_root %>%
          file.path('baseline/static/detail', paste0(.x, '.csv'))
        keep = names(fread(path, nrows = 0)) %>%
          `[`(. == 'id' | startsWith(., 'mtr_'))
        path %>%
          fread(select = keep) %>%
          tibble() %>%
          mutate(year = .x) %>%
          select(id, year, starts_with('mtr_')) %>%
          return()
      }
    ) %>%
    bind_rows()

  # Save to baseline subdirectory
  dir.create(file.path(staging_dir, 'baseline'), recursive = T, showWarnings = F)
  saveRDS(baseline_mtrs, file.path(staging_dir, 'baseline', 'baseline_mtrs.rds'))
}


#------------------
# Build manifest
#------------------

year_batches = function(years, size) {

  #----------------------------------------------------------------------------
  # Groups a scenario's years into consecutive batches of at most `size`, one
  # manifest row each. At size 1 this is one batch per year, which is what the
  # pipeline has always emitted.
  #
  # Parameters:
  #   - years (int[]) : the scenario's simulation years, ascending
  #   - size (int)    : years per array task
  #
  # Returns: list of integer vectors (list).
  #----------------------------------------------------------------------------

  split(years, ceiling(seq_along(years) / size)) %>% unname()
}


# The phases, in dependency order. A parallel array runs one task per scenario
# and batch of years; a pre-pass runs one job per scenario, sequentially within
# the job. Each row carries its years as a list element, so a pre-pass row holds
# a single NA.
#
#   '1'   baseline years, static, parallel array
#   '1B'  frozen mechanical pre-pass, before 2A, whose workers inject its state
#   '2A'  counterfactual years, static, parallel array
#   '2MN' counterfactual years, mech-no-wealth, parallel array. Measures the
#         mechanical rung's forcing on the base before erosion
#   '2MW' mechanical wealth bathtub pre-pass. Reads the 2MN and baseline detail
#   '2M'  counterfactual years, mechanical, parallel array
#   '2B'  gains bathtub pre-pass
#   '2N'  counterfactual years, conv-no-wealth, parallel array. Measures the
#         forcing and the capital bundle MTR on the base before erosion
#   '2W'  wealth bathtub pre-pass. Reads the 2N and baseline detail and writes
#         the per-year deficit state 2C applies
#   '2C'  counterfactual years, conventional, parallel array
manifest = tibble(phase = character(), scenario = character(), years = list())

# Phase 1: baseline year tasks, skipped when a baseline vintage was supplied
if (has_baseline) {
  baseline_info = get_scenario_info('baseline')
  manifest = manifest %>%
    bind_rows(tibble(
      phase    = '1',
      scenario = 'baseline',
      years    = year_batches(baseline_info$years, years_per_task)
    ))
}

# Phases 2A and 2C: one task per counterfactual scenario and batch of years
# Phases 1B and 2B: one task per counterfactual scenario
for (sid in counterfactual_ids) {
  si = get_scenario_info(sid)

  # Activate this scenario's legs: scenario_uses_wealth_dynamics below reads the
  # economy leg, and the loop above may have left another scenario's installed
  config_activate(economy  = si$resolved_economy,
                  behavior = si$resolved_behavior)

  manifest = manifest %>%
    bind_rows(tibble(
      phase    = '1B',
      scenario = sid,
      years    = list(NA_integer_)
    )) %>%
    bind_rows(tibble(
      phase    = '2A',
      scenario = sid,
      years    = year_batches(si$years, years_per_task)
    )) %>%
    bind_rows(tibble(
      phase    = '2B',
      scenario = sid,
      years    = list(NA_integer_)
    )) %>%
    bind_rows(tibble(
      phase    = '2C',
      scenario = sid,
      years    = year_batches(si$years, years_per_task)
    ))

  # Emit the mechanical rung only for scenarios with a transmission channel live.
  # Without one the rung equals the static one and the reporting layer reads static
  # totals in its place
  uses_wealth_si = scenario_uses_wealth_dynamics(si)
  if (scenario_runs_mechanical(si)) {
    manifest = manifest %>%
      bind_rows(tibble(
        phase    = '2M',
        scenario = sid,
        years    = year_batches(si$years, years_per_task)
      ))

    # The mechanical rung's own drawdown forcing, on the same pattern as the
    # conventional rung's
    if (uses_wealth_si) {
      manifest = manifest %>%
        bind_rows(tibble(
          phase    = '2MN',
          scenario = sid,
          years    = year_batches(si$years, years_per_task)
        )) %>%
        bind_rows(tibble(
          phase    = '2MW',
          scenario = sid,
          years    = list(NA_integer_)
        ))
    }
  }

  # Emit the wealth bathtub's year array and pre-pass only for scenarios that
  # activate the channel. The workers would no-op for the rest anyway
  if (uses_wealth_si) {
    manifest = manifest %>%
      bind_rows(tibble(
        phase    = '2N',
        scenario = sid,
        years    = year_batches(si$years, years_per_task)
      )) %>%
      bind_rows(tibble(
        phase    = '2W',
        scenario = sid,
        years    = list(NA_integer_)
      ))
  }
}


#-----------------------------------------
# Serialize shared state for workers
#-----------------------------------------

saveRDS(globals,            file.path(staging_dir, 'globals.rds'))
saveRDS(return_vars,        file.path(staging_dir, 'return_vars.rds'))
saveRDS(counterfactual_ids, file.path(staging_dir, 'counterfactual_ids.rds'))
saveRDS(manifest,           file.path(staging_dir, 'manifest.rds'))

# Write the submission map the shell reads. Each phase keeps its global manifest
# indices, and slurm_run.sh submits only the slice belonging to one scenario, so
# that a scenario advances as soon as its own prerequisites finish rather than
# waiting at a phase-wide barrier.
indexed_manifest = manifest %>%
  group_by(phase) %>%
  mutate(task_id = row_number()) %>%
  ungroup()

phase_bounds = function(sid, phase_name) {
  ids = indexed_manifest %>%
    filter(phase == phase_name, scenario == sid) %>%
    pull(task_id)

  if (length(ids) == 0) {
    return(list(first = NA_integer_, last = NA_integer_))
  }
  if (!identical(ids, seq.int(min(ids), max(ids)))) {
    stop('Non-contiguous manifest tasks for scenario=', sid,
         ', phase=', phase_name)
  }
  list(first = min(ids), last = max(ids))
}

submission_plan = tibble(
  scenario         = character(),
  phase1b_task     = integer(),
  phase2a_first    = integer(),
  phase2a_last     = integer(),
  phase2b_task     = integer(),
  phase2n_first    = integer(),
  phase2n_last     = integer(),
  phase2w_task     = integer(),
  phase2c_first    = integer(),
  phase2c_last     = integer(),
  aggregate_task   = integer(),
  postprocess_task = integer(),
  # Appended at the end: slurm_run.sh reads plan columns by position, so a new
  # column goes last rather than renumbering the existing ones
  phase2mn_first   = integer(),
  phase2mn_last    = integer(),
  phase2mw_task    = integer(),
  phase2m_first    = integer(),
  phase2m_last     = integer()
)

if (length(counterfactual_ids) > 0) {
  submission_plan = map2_dfr(
    counterfactual_ids,
    seq_along(counterfactual_ids),
    function(sid, scenario_index) {
      p1b = phase_bounds(sid, '1B')
      p2a = phase_bounds(sid, '2A')
      p2mn = phase_bounds(sid, '2MN')
      p2mw = phase_bounds(sid, '2MW')
      p2m = phase_bounds(sid, '2M')
      p2b = phase_bounds(sid, '2B')
      p2n = phase_bounds(sid, '2N')
      p2w = phase_bounds(sid, '2W')
      p2c = phase_bounds(sid, '2C')

      tibble(
        scenario         = sid,
        phase1b_task     = p1b$first,
        phase2a_first    = p2a$first,
        phase2a_last     = p2a$last,
        phase2b_task     = p2b$first,
        phase2n_first    = p2n$first,
        phase2n_last     = p2n$last,
        phase2w_task     = p2w$first,
        phase2c_first    = p2c$first,
        phase2c_last     = p2c$last,
        aggregate_task   = scenario_index + as.integer(has_baseline),
        postprocess_task = scenario_index,
        phase2mn_first   = p2mn$first,
        phase2mn_last    = p2mn$last,
        phase2mw_task    = p2mw$first,
        phase2m_first    = p2m$first,
        phase2m_last     = p2m$last
      )
    }
  )
}

write_tsv(submission_plan,
          file.path(staging_dir, 'submission_plan.tsv'),
          na = 'NA')

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

n_phase1    = sum(manifest$phase == '1')
n_phase1b   = sum(manifest$phase == '1B')
n_phase2a   = sum(manifest$phase == '2A')
n_phase2mn  = sum(manifest$phase == '2MN')
n_phase2mw  = sum(manifest$phase == '2MW')
n_phase2m   = sum(manifest$phase == '2M')
n_phase2b   = sum(manifest$phase == '2B')
n_phase2n   = sum(manifest$phase == '2N')
n_phase2w   = sum(manifest$phase == '2W')
n_phase2c   = sum(manifest$phase == '2C')
n_scenarios = length(counterfactual_ids)

cat(paste0('STAGING_DIR="', staging_dir, '"\n'))
cat(paste0('N_PHASE1=',    n_phase1,    '\n'))
cat(paste0('N_PHASE1B=',   n_phase1b,   '\n'))
cat(paste0('N_PHASE2A=',   n_phase2a,   '\n'))
cat(paste0('N_PHASE2MN=',  n_phase2mn,  '\n'))
cat(paste0('N_PHASE2MW=',  n_phase2mw,  '\n'))
cat(paste0('N_PHASE2M=',   n_phase2m,   '\n'))
cat(paste0('N_PHASE2B=',   n_phase2b,   '\n'))
cat(paste0('N_PHASE2N=',   n_phase2n,   '\n'))
cat(paste0('N_PHASE2W=',   n_phase2w,   '\n'))
cat(paste0('N_PHASE2C=',   n_phase2c,   '\n'))
cat(paste0('N_SCENARIOS=', n_scenarios, '\n'))
cat(paste0('STACKED=',     stacked,     '\n'))
