#------------------------------------------------------------------
# state_tax_law.R
#
# Contains functions to read and parse STATE tax law configuration
# files (config/scenarios/tax_law_state/). Reuses the federal parsing
# machinery in tax_law.R (load_tax_law_input, parse_param) per state,
# prefixing parameter names with "st_" and adding a state join key.
# Design: other/state_tax_research/state_tax_implementation_plan.md §2.2
#------------------------------------------------------------------


build_state_tax_law = function(states, years, indexes,
                               state_tax_law_id = 'baseline',
                               output_path = NULL) {

  #----------------------------------------------------------------------------
  # Builds the state tax law dataframe for the given jurisdictions: for each
  # state, reads its baseline YAML directory, overlays reform changes at the
  # subparameter level (identical semantics to build_tax_law), parses with the
  # existing parameter machinery, and prefixes names with "st_" -- e.g. the
  # subparameter "rates" under "ord" in il/ord.yaml becomes column
  # "st_ord.rates1". Output is long in (state, year, filing_status), wide in
  # subparameter, suitable for joining to tax units on
  # (year, filing_status) within a per-state loop.
  #
  # State law is encoded 2017-forward (the microdata floor; plan §5.1), so
  # parsing starts at 2017 regardless of the requested years.
  #
  # Parameters:
  #   - states (str[])          : 2-letter postal codes, e.g. c('IL','CO','NY').
  #                               No-tax jurisdictions with no config directory
  #                               are skipped with a message
  #   - years (int[])           : simulation years to return
  #   - indexes (df)            : long-format dataframe containing growth rates
  #                               of index measures; see generate_indexes()
  #   - state_tax_law_id (str)  : reform directory under
  #                               config/scenarios/tax_law_state/, or 'baseline'.
  #                               A reform missing a given state's subdirectory
  #                               means no changes for that state
  #   - output_path (str)       : scenario output root; when supplied, writes
  #                               supplemental/tax_law_state.csv for both run
  #                               types (mirrors build_tax_law)
  #
  # Returns: tibble long in state, year, and filing status; wide in st_-prefixed
  #          subparameter columns (df).
  #----------------------------------------------------------------------------

  root = './config/scenarios/tax_law_state'

  # Reform root must exist if specified (a typo should fail loudly)
  if (state_tax_law_id != 'baseline' &&
      !dir.exists(file.path(root, state_tax_law_id))) {
    stop('Cannot find the user-supplied state tax law configuration directory')
  }

  state_tax_law = states %>%
    map(.f = ~ parse_one_state(.x, root, state_tax_law_id, years, indexes)) %>%
    bind_rows()

  # Write state tax law if output path supplied, then return
  if (!is.null(output_path)) {
    c('static', 'conventional') %>%
      map(.f = ~ output_path %>%
            file.path(.x, 'supplemental', 'tax_law_state.csv') %>%
            write_csv(x = state_tax_law, file = .))
  }

  return(state_tax_law)
}



parse_one_state = function(st, root, state_tax_law_id, years, indexes) {

  #----------------------------------------------------------------------------
  # Reads, overlays, and parses tax law for a single state. Helper for
  # build_state_tax_law().
  #
  # Parameters:
  #   - st (str)               : 2-letter postal code
  #   - root (str)             : state tax law config root
  #   - state_tax_law_id (str) : see build_state_tax_law()
  #   - years (int[])          : simulation years to return
  #   - indexes (df)           : see build_state_tax_law()
  #
  # Returns: tibble long in year and filing status, wide in st_-prefixed
  #          subparameter columns, with a state column; NULL if the state has
  #          no baseline config directory (no-tax jurisdiction) (df).
  #----------------------------------------------------------------------------

  baseline_path = file.path(root, 'baseline', tolower(st))

  # No-tax jurisdictions carry no config; skip (weights still cover them)
  if (!dir.exists(baseline_path)) {
    message('No state tax law config for ', st, '; treating as no-tax jurisdiction')
    return(NULL)
  }

  tax_law = load_tax_law_input(baseline_path)

  # Overwrite baseline subparams with reform changes, if any for this state
  reform_path = file.path(root, state_tax_law_id, tolower(st))
  if (state_tax_law_id != 'baseline' && dir.exists(reform_path)) {
    changes_from_baseline = load_tax_law_input(reform_path)
    for (param in names(changes_from_baseline)) {
      for (subparam in names(changes_from_baseline[[param]])) {
        tax_law[[param]][[subparam]] = changes_from_baseline[[param]][[subparam]]
      }
    }
  }

  # Parse all parameters and concatenate; state law floor is 2017 (plan §5.1)
  tax_law %>%
    map2(.f      = parse_param,
         .y      = names(.),
         years   = 2017:max(years),
         indexes = indexes) %>%
    bind_rows() %>%

    # Split subparameters into scalars and vectors
    filter(!is.na(value)) %>%
    group_by(parameter, subparameter) %>%
    mutate(scalar = max(element) == 1) %>%
    ungroup() %>%

    # Reshape wide, prefixing with "st_" to avoid federal column collisions
    mutate(name = 'st_' %>%
             paste0(parameter) %>%
             paste0('.') %>%
             paste0(subparameter) %>%
             paste0(ifelse(scalar, '', element))) %>%
    select(-contains('arameter'), -element, -scalar) %>%
    pivot_wider(names_from  = name,
                values_from = value) %>%
    filter(year %in% years) %>%
    mutate(state = st) %>%
    select(state, everything()) %>%
    return()
}
