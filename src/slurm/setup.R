#-----------------------------------------------------------------------
# setup.R
#
# Phase 0 of SLURM pipeline: parses globals, builds per-scenario
# configurations (tax law, indexes, offsets), and serializes everything
# to .rds files for downstream SLURM array jobs. Runs synchronously on
# the login node.
#
# CLI args: same as main.R except multicore (parsed by parse_cli_args)
#   Rscript src/slurm/setup.R <runscript> <scenario_id> <local> <vintage>
#           <pct_sample> <stacked> <baseline_vintage> <delete_detail>
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
    if (!(.x %in% c('main.R')) && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/')) {
      source(file.path('./src/', .x))
    }
  })


#------------------------
# Set runtime parameters
#------------------------

args = commandArgs(trailingOnly = T)
cli  = parse_cli_args(args, context = 'slurm_setup')

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
      ~ {
        # id plus the mtr_ columns only, resolved from the header: detail files
        # are 98 columns / ~150MB (perf audit §2.7)
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

# Phase encoding (string for clarity; old integer scheme is gone):
#   '1'   baseline-year tasks         (parallel array; static-only)
#   '1B'  cf frozen mechanical pass   (one job per cf; sequential within job;
#                                      no-op for non-kg_dynamics scenarios;
#                                      must precede 2A — static workers inject
#                                      its state)
#   '2A'  cf static-only year tasks   (parallel array)
#   '2B'  cf bathtub pre-pass         (one job per cf; sequential within job;
#                                      no-op for non-kg_dynamics scenarios)
#   '2N'  cf conv-no-wealth year      (parallel array; only for s>0 wealth
#                                      scenarios; produces ΔT⁰ ingredients +
#                                      mtr_cap_bundle on the un-eroded base)
#   '2W'  cf wealth bathtub pre-pass  (one job per s>0 cf; sequential within
#                                      job; reads 2N + baseline detail, writes
#                                      the per-year deficit state 2C applies)
#   '2C'  cf conventional-only year   (parallel array)
manifest = tibble(phase = character(), scenario = character(), year = integer())

# Phase 1: baseline year tasks (skip if baseline_vintage provided)
if (has_baseline) {
  baseline_info = get_scenario_info('baseline')
  manifest = manifest %>%
    bind_rows(tibble(
      phase    = '1',
      scenario = 'baseline',
      year     = baseline_info$years
    ))
}

# Phase 2A and 2C: counterfactual × year tasks (static and conventional)
# Phase 1B and 2B: one task per counterfactual scenario
for (sid in counterfactual_ids) {
  si = get_scenario_info(sid)
  manifest = manifest %>%
    bind_rows(tibble(
      phase    = '1B',
      scenario = sid,
      year     = NA_integer_
    )) %>%
    bind_rows(tibble(
      phase    = '2A',
      scenario = sid,
      year     = si$years
    )) %>%
    bind_rows(tibble(
      phase    = '2B',
      scenario = sid,
      year     = NA_integer_
    )) %>%
    bind_rows(tibble(
      phase    = '2C',
      scenario = sid,
      year     = si$years
    ))

  # Wealth bathtub: the conv-no-wealth year-array (2N) and the sequential
  # pre-pass (2W), emitted only for s>0 scenarios (a year-array of no-op tasks
  # for every non-wealth scenario would be wasteful; the in-worker gate is a
  # belt-and-suspenders no-op anyway).
  if (scenario_uses_wealth_dynamics(si)) {
    manifest = manifest %>%
      bind_rows(tibble(
        phase    = '2N',
        scenario = sid,
        year     = si$years
      )) %>%
      bind_rows(tibble(
        phase    = '2W',
        scenario = sid,
        year     = NA_integer_
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

# Shell-facing submission map. Each phase keeps its existing global manifest
# indices, but slurm_run.sh submits only the contiguous slice belonging to one
# scenario. That lets each scenario advance as soon as its own prerequisites
# finish instead of waiting at a phase-wide array barrier.
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
  postprocess_task = integer()
)

if (length(counterfactual_ids) > 0) {
  submission_plan = map2_dfr(
    counterfactual_ids,
    seq_along(counterfactual_ids),
    function(sid, scenario_index) {
      p1b = phase_bounds(sid, '1B')
      p2a = phase_bounds(sid, '2A')
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
        postprocess_task = scenario_index
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
n_phase2b   = sum(manifest$phase == '2B')
n_phase2n   = sum(manifest$phase == '2N')
n_phase2w   = sum(manifest$phase == '2W')
n_phase2c   = sum(manifest$phase == '2C')
n_scenarios = length(counterfactual_ids)

cat(paste0('STAGING_DIR="', staging_dir, '"\n'))
cat(paste0('N_PHASE1=',    n_phase1,    '\n'))
cat(paste0('N_PHASE1B=',   n_phase1b,   '\n'))
cat(paste0('N_PHASE2A=',   n_phase2a,   '\n'))
cat(paste0('N_PHASE2B=',   n_phase2b,   '\n'))
cat(paste0('N_PHASE2N=',   n_phase2n,   '\n'))
cat(paste0('N_PHASE2W=',   n_phase2w,   '\n'))
cat(paste0('N_PHASE2C=',   n_phase2c,   '\n'))
cat(paste0('N_SCENARIOS=', n_scenarios, '\n'))
cat(paste0('STACKED=',     stacked,     '\n'))
