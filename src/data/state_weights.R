# =============================================================================
# state_weights.R  --  the state-weight RUNTIME for Tax-Simulator
#
# What this file is now (2026-09-12): the jurisdiction set and the runtime
# dispatcher `build_state_weights()`, which src/sim/run.R calls. Nothing else.
#
# What it used to be, and where that went. Until 2026-09-11 this file was ~1700
# lines that also BUILT the weights and the non-filer population it rests on:
# the HT2 and ACS readers, the SOI and SSA table readers, the LODES and QWI
# fetchers, the calibration and gradient engines, the age-band and income-tier
# conventions. All of that moved to **Tax-Data** with the rest of the
# population-construction workstream (JI, 2026-09-11): a population in PUF
# schema is Tax-Data's mission, and Tax-Simulator holds the state tax LAW and
# CONSUMES weights through the interface. The moved code is
# `Tax-Data/src/nonfilers/state_weights.R` (with `asec_tax_units.R` and
# `filing_model.R` beside it) and `Tax-Data/research/state_weights/`; the
# pointer this repo keeps is `research/state_weights/MOVED.md`.
#
# The split is exactly the asymmetry the research conventions named: the
# builder may read a consumer's schema, but nothing the model RUNS may depend
# on the builder. Only `build_state_weights()` was ever on the run path, and it
# never called anything that moved -- verified before the deletion by grepping
# every export of the three modules across src/, which returned nothing outside
# the modules and their own tests.
#
# Since 2026-09-29 (Tax-Data Phase 4 P5) the dispatcher READS
# `state_weights_{year}.csv.gz` from the State-Weights interface, pinned like
# any other dependency and produced by Tax-Data's `src/main_state_weights.R`.
# It has no fitting engine.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# -----------------------------------------------------------------------------
# Jurisdiction set. 50 states + DC are modeled; PR and OA (SOI "Other Areas") are
# carried as no-tax buckets so weights still sum to the national total.
# -----------------------------------------------------------------------------
STATE_JURISDICTIONS <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
  "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
  "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
  "WV","WI","WY")                       # 51 modeled
NONTAX_BUCKETS <- c("PR","OA")          # carried, no state income tax calc


# -----------------------------------------------------------------------------
# build_state_weights(): the runtime dispatcher (plan §2.1). Returns long
# split weights (id, state, weight) for the requested states. Federal
# aggregates are invariant to state mode regardless, because federal totals
# use the untouched `weight` column.
#
# Methods:
#   "interface"   -- reads state_weights_{year}.csv.gz from `root`, the pinned
#                    State-Weights vintage: long (id, state, weight), weight > 0,
#                    with Σ over all 53 jurisdictions = the record's national
#                    weight (asserted by the producer, and again here). Checked
#                    before the state filter: every record of `tax_units` is
#                    present, and its weights sum to its national weight (the
#                    file's weights are full-sample; `tax_units$weight` is
#                    already divided by pct_sample, so the file is rescaled the
#                    same way). A missing year file stops -- the producer emits
#                    every year 2017-2097, later years carrying the last fit
#                    year's shares, so a gap is a broken vintage, not a rule to
#                    apply here.
#   "placeholder" -- EACH requested jurisdiction gets 1/53 of the national
#                    weight (fixed denominator 53). Kept for A/B reproduction
#                    of pre-interface runs only. State LEVELS are meaningless
#                    AND the emitted rows do NOT sum to weight_i when a subset
#                    of states is requested: Σ over emitted states =
#                    (n_states / 53) * weight_i.
# -----------------------------------------------------------------------------
STATE_WEIGHT_SUM_TOL = 1e-6   # relative, on Σ_state weight vs national weight

build_state_weights = function(tax_units, year,
                               method = c('interface', 'placeholder'),
                               states = NULL, root = NULL, pct_sample = 1) {

  method = match.arg(method)
  jurisdictions = c(STATE_JURISDICTIONS, NONTAX_BUCKETS)
  if (is.null(states)) {
    states = jurisdictions
  }
  unknown = setdiff(states, jurisdictions)
  if (length(unknown) > 0) {
    stop('build_state_weights(): unknown jurisdiction(s) ', paste(unknown, collapse = ' '))
  }

  if (method == 'placeholder') {
    return(
      tax_units %>%
        select(id, weight) %>%
        expand_grid(state = states) %>%
        mutate(weight = weight / length(jurisdictions)) %>%
        select(id, state, weight)
    )
  }

  if (is.null(root)) {
    stop('build_state_weights(method = "interface"): no State-Weights interface path; ',
         'the runscript row needs dep.State-Weights.vintage/ID (or a default in ',
         'config/interfaces/interface_versions.yaml)')
  }
  f = file.path(root, paste0('state_weights_', year, '.csv.gz'))
  if (!file.exists(f)) {
    stop('build_state_weights(): no state weights for ', year, ' in the pinned vintage: ', f)
  }
  sw = data.table::fread(f) %>%
    as_tibble() %>%
    filter(id %in% tax_units$id)

  # Every record, once, with its national weight recovered over ALL jurisdictions
  totals = sw %>%
    group_by(id) %>%
    summarise(w_file = sum(weight), .groups = 'drop')
  check = tax_units %>%
    select(id, weight) %>%
    left_join(totals, by = 'id')
  if (any(is.na(check$w_file))) {
    stop('build_state_weights(): ', sum(is.na(check$w_file)), ' of ', nrow(check),
         ' records in TY', year, ' have no row in ', basename(f),
         '. The State-Weights vintage was fit on a different Tax-Data vintage.')
  }
  rel_gap = abs(check$w_file / pct_sample - check$weight) / check$weight
  if (max(rel_gap) > STATE_WEIGHT_SUM_TOL) {
    stop('build_state_weights(): state weights do not sum to the national weight for ',
         sum(rel_gap > STATE_WEIGHT_SUM_TOL), ' records in TY', year,
         ' (largest relative gap ', signif(max(rel_gap), 3), ')')
  }

  sw %>%
    filter(state %in% states) %>%
    mutate(weight = weight / pct_sample) %>%
    select(id, state, weight)
}


# -----------------------------------------------------------------------------
# validate_state_weights_pins(): id coherence at parse time. A State-Weights
# vintage records, in the dependencies.csv beside its scenario directory, the
# Tax-Data vintage it was fit on; every runscript row that reads it must read
# that same Tax-Data vintage. Called by parse_globals() once the interface
# paths are known. Returns TRUE invisibly; stops on a mismatch.
#   dependencies    tibble(ID, interface, version, vintage, scenario), the run's
#   interface_paths tibble(ID, interface, path)
# -----------------------------------------------------------------------------
validate_state_weights_pins = function(dependencies, interface_paths) {
  sw_rows = interface_paths %>% filter(interface == 'State-Weights')
  if (nrow(sw_rows) == 0) return(invisible(TRUE))
  for (i in seq_len(nrow(sw_rows))) {
    dep_file = file.path(dirname(sw_rows$path[i]), 'dependencies.csv')
    if (!file.exists(dep_file)) {
      stop('State-Weights vintage at ', sw_rows$path[i], ' has no dependencies.csv; ',
           'cannot tell which Tax-Data vintage it was fit on')
    }
    fit_on = readr::read_csv(dep_file, show_col_types = FALSE) %>%
      filter(interface == 'Tax-Data') %>%
      pull(vintage)
    reads = dependencies %>%
      filter(ID == sw_rows$ID[i], interface == 'Tax-Data') %>%
      pull(vintage)
    if (length(fit_on) != 1 || length(reads) != 1 || fit_on != reads) {
      stop("Scenario '", sw_rows$ID[i], "' reads Tax-Data ", paste(reads, collapse = ','),
           ' but its State-Weights vintage was fit on Tax-Data ', paste(fit_on, collapse = ','),
           '. Pin a State-Weights vintage built on the same Tax-Data vintage.')
    }
  }
  invisible(TRUE)
}
