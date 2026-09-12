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
# When the Phase 1 fit lands, this dispatcher gains a method that READS
# `state_weights_{year}.csv` from the Tax-Data interface, pinned like any other
# dependency. It does not regain a fitting engine.
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
# split weights (id, state, weight). The real methods satisfy
# Σ_state w_{i,state} = weight_i across ALL 53 jurisdictions; the placeholder
# does not (see below). Federal aggregates are invariant to the with-state mode
# regardless, because federal totals use the untouched `weight` column.
#
# Methods:
#   "placeholder" -- gives EACH requested jurisdiction 1/53 of the national
#                    weight (fixed denominator 53). Exists so the Phase 4
#                    orchestration can run before the Phase 1 bake-off lands.
#                    State LEVELS are meaningless (every state = 1/53 of the
#                    nation) AND the emitted rows do NOT sum to weight_i when a
#                    subset of states is requested: Σ over emitted states =
#                    (n_states / 53) * weight_i. Do not reconstruct a national
#                    total by summing state.csv under the placeholder. The file
#                    contract and downstream machinery are real.
#   "calibration" -- Approach A (fit_calibration); not yet wired to HT2/ACS
#                    target ingestion at runtime.
#   "gradient"    -- Approach B (fit_gradient); ditto.
# -----------------------------------------------------------------------------
build_state_weights = function(tax_units, year,
                               method = c('placeholder', 'calibration', 'gradient'),
                               states = NULL) {

  method = match.arg(method)
  jurisdictions = c(STATE_JURISDICTIONS, NONTAX_BUCKETS)
  if (is.null(states)) {
    states = jurisdictions
  }

  if (method != 'placeholder') {
    stop('build_state_weights(): method "', method, '" is not yet wired into ',
         'the runtime (Phase 1 bake-off pending); use "placeholder"')
  }

  tax_units %>%
    select(id, weight) %>%
    expand_grid(state = states) %>%
    mutate(weight = weight / length(jurisdictions)) %>%
    select(id, state, weight) %>%
    return()
}
