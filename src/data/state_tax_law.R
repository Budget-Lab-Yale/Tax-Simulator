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

  # Dense lookup schedules remain row data instead of becoming thousands of
  # scalar YAML columns. The calculator selects the applicable table year.
  attr(state_tax_law, 'credit_tables') = build_state_credit_tables(
    states, root, state_tax_law_id
  )

  # Write state tax law if output path supplied, then return
  if (!is.null(output_path)) {
    c('static', 'conventional') %>%
      map(.f = ~ output_path %>%
            file.path(.x, 'supplemental', 'tax_law_state.csv') %>%
            write_csv(x = state_tax_law, file = .))
  }

  return(state_tax_law)
}



build_state_credit_tables = function(states, root, state_tax_law_id) {

  #----------------------------------------------------------------------------
  # Reads optional dense state-credit schedules. A credit_tables.csv file
  # lives beside the state's YAML with one row per inclusive income range,
  # generalized key concept (child count, family size, ... -- whatever the
  # published table is keyed by), and filing status (0 = all statuses).
  # Reform files replace a baseline credit/year pair (2026-07-17 review
  # item #7: dense tables belong here, not in long YAML vectors).
  #
  # Returns: tibble with credit_id, state, year, filing_status, key_concept,
  #          income_lower, income_upper, and value; empty tibble when no
  #          state has a table.
  #----------------------------------------------------------------------------

  required = c('credit_id', 'state', 'year', 'filing_status', 'key_concept',
               'income_lower', 'income_upper', 'value')
  empty = tibble(
    credit_id = character(), state = character(), year = integer(),
    filing_status = integer(), key_concept = integer(),
    income_lower = double(), income_upper = double(), value = double()
  )

  read_one = function(path, st) {
    if (!file.exists(path)) {
      return(empty)
    }
    table = read_csv(
      path,
      col_types = cols(
        credit_id = col_character(), state = col_character(), year = col_integer(),
        filing_status = col_integer(), key_concept = col_integer(),
        income_lower = col_double(), income_upper = col_double(),
        value = col_double()
      ),
      show_col_types = FALSE
    )
    if (!identical(names(table), required)) {
      stop('State credit table has an invalid schema: ', path)
    }
    if (any(toupper(table$state) != toupper(st)) ||
        any(table$income_lower > table$income_upper) ||
        any(table$key_concept < 0) ||
        any(!(table$filing_status %in% 0:4)) ||
        anyDuplicated(table[c('credit_id', 'state', 'year', 'filing_status',
                              'key_concept', 'income_lower', 'income_upper')])) {
      stop('State credit table has invalid rows: ', path)
    }
    overlap = table %>%
      arrange(credit_id, state, year, filing_status, key_concept,
              income_lower, income_upper) %>%
      group_by(credit_id, state, year, filing_status, key_concept) %>%
      summarise(
        overlap = any(income_lower[-1] <= income_upper[-n()]),
        .groups = 'drop'
      )
    if (any(overlap$overlap)) {
      stop('State credit table has overlapping ranges: ', path)
    }

    # A table version must be all-statuses (0) or fully status-keyed --
    # mixing would double-match units in the lookup
    mixed = table %>%
      group_by(credit_id, state, year) %>%
      summarise(
        mixed = any(filing_status == 0) & any(filing_status != 0),
        .groups = 'drop'
      )
    if (any(mixed$mixed)) {
      stop('State credit table mixes filing_status 0 with status-keyed ',
           'rows within one table version: ', path)
    }
    table %>% mutate(state = toupper(state))
  }

  tables = map_dfr(toupper(states), function(st) {
    baseline = read_one(file.path(root, 'baseline', tolower(st),
                                  'credit_tables.csv'), st)
    reform = if (state_tax_law_id != 'baseline') {
      read_one(file.path(root, state_tax_law_id, tolower(st),
                         'credit_tables.csv'), st)
    } else {
      empty
    }
    if (nrow(reform) == 0) {
      return(baseline)
    }
    baseline %>%
      anti_join(distinct(reform, credit_id, state, year),
                by = c('credit_id', 'state', 'year')) %>%
      bind_rows(reform)
  })

  return(tables)
}



state_credit_tables_for_year = function(credit_tables, state_code, tax_year) {

  #----------------------------------------------------------------------------
  # Selects each table's most recent published version on or before a
  # simulation year. This makes a post-publication carry-forward visible in
  # configuration and lets a reform introduce a later version without code.
  #----------------------------------------------------------------------------

  if (is.null(credit_tables) || nrow(credit_tables) == 0) {
    return(credit_tables)
  }

  credit_tables %>%
    filter(state == toupper(state_code), year <= tax_year) %>%
    group_by(credit_id) %>%
    filter(year == max(year)) %>%
    ungroup()
}



load_state_conformity_groups = function(
  config_path = './config/scenarios/tax_law_state/conformity_groups.yaml'
) {

  #----------------------------------------------------------------------------
  # Loads the generic fixed/selective-conformity contract. Numeric group zero
  # is rolling conformity; positive groups identify a shared federal
  # reference-law overlay rather than a particular jurisdiction.
  #----------------------------------------------------------------------------

  if (!file.exists(config_path)) {
    stop('Cannot find the state conformity-group configuration')
  }
  raw = read_yaml(config_path)
  group_ids = suppressWarnings(as.integer(names(raw)))
  if (length(raw) == 0 || anyNA(group_ids) || anyDuplicated(group_ids)) {
    stop('State conformity groups must have unique numeric identifiers')
  }

  value_chr = function(x, field) {
    value = x[[field]]
    if (is.null(value) || length(value) == 0 || is.na(value) || value == '') {
      NA_character_
    } else {
      as.character(value)
    }
  }
  groups = imap_dfr(raw, function(x, id) {
    if (!is.list(x)) {
      stop('Each state conformity group must be a mapping')
    }
    tibble(
      conformity_group    = as.integer(id),
      label                = value_chr(x, 'label'),
      ready                = isTRUE(x$ready),
      reference_tax_law_id = value_chr(x, 'reference_tax_law_id')
    )
  }) %>%
    arrange(conformity_group)

  rolling = groups %>% filter(conformity_group == 0)
  if (nrow(rolling) != 1 || !rolling$ready ||
      !is.na(rolling$reference_tax_law_id)) {
    stop('Conformity group 0 must be a ready rolling group with no reference law')
  }
  invalid_ready = groups %>%
    filter(conformity_group != 0, ready, is.na(reference_tax_law_id))
  if (nrow(invalid_ready) > 0) {
    stop('Ready fixed/selective conformity groups need a reference_tax_law_id')
  }

  return(groups)
}



state_conformity_groups_for_law = function(state_tax_law, conformity_groups) {

  #----------------------------------------------------------------------------
  # Resolves each selected state to its one conformity group for the supplied
  # year. The group lives in state-law YAML so a state can move to a new
  # reference package over time without calculator changes.
  #----------------------------------------------------------------------------

  if (is.null(state_tax_law) || nrow(state_tax_law) == 0) {
    return(tibble())
  }
  group_values = if ('st_agi.conformity_group' %in% names(state_tax_law)) {
    # A state that does not declare a group is rolling conformity (group 0).
    # In a mixed states list (e.g. CA + IL) the wide pivot leaves the column
    # NA for every non-declaring state, so coalesce before the finite check.
    coalesce(state_tax_law$st_agi.conformity_group, 0)
  } else {
    rep(0, nrow(state_tax_law))
  }
  if (any(!is.finite(group_values)) || any(group_values != floor(group_values))) {
    stop('State conformity_group values must be finite integers')
  }

  selected = state_tax_law %>%
    transmute(state, conformity_group = as.integer(group_values)) %>%
    distinct()
  if (anyDuplicated(selected$state)) {
    stop('A state must use one conformity group within a state-law build')
  }

  missing_groups = setdiff(selected$conformity_group,
                           conformity_groups$conformity_group)
  if (length(missing_groups) > 0) {
    stop('State law references undefined conformity group(s): ',
         paste(sort(missing_groups), collapse = ', '))
  }

  selected %>%
    left_join(conformity_groups, by = 'conformity_group') %>%
    return()
}



build_state_reference_tax_laws = function(state_tax_law, indexes,
                                           conformity_groups) {

  #----------------------------------------------------------------------------
  # Builds exactly one federal reference law per ready conformity group. The
  # caller caches the resulting contexts by group rather than calculating one
  # per state.
  #----------------------------------------------------------------------------

  selected = state_conformity_groups_for_law(state_tax_law, conformity_groups) %>%
    filter(conformity_group != 0, ready) %>%
    distinct(conformity_group, reference_tax_law_id)
  if (nrow(selected) == 0) {
    return(list())
  }

  reference_laws = map(
    selected$reference_tax_law_id,
    ~ build_tax_law_from_id(
      tax_law_id = .x,
      years      = sort(unique(state_tax_law$year)),
      indexes    = indexes
    )
  )
  names(reference_laws) = as.character(selected$conformity_group)
  return(reference_laws)
}



build_state_reference_contexts = function(tax_units_calc, normal_tax_law,
                                           reference_tax_laws, vars_1040) {

  #----------------------------------------------------------------------------
  # Recalculates federal variables used by state law under each ready reference
  # law. Input records already include static payroll pass-through or a single
  # behavioral response from the scenario pass, so behavior is never repeated.
  #----------------------------------------------------------------------------

  if (length(reference_tax_laws) == 0) {
    return(list())
  }

  normal_law_vars = setdiff(names(normal_tax_law), c('year', 'filing_status'))
  calculated_vars = c(
    fed_calc_vars(), 'expanded_inc', 'simple_filer', 'corp_tax_change'
  )
  reference_input = tax_units_calc %>%
    select(-any_of(c(normal_law_vars, calculated_vars)),
           -starts_with('mtr_'), -starts_with('liab_brac'))

  if ('filing_status_input' %in% names(reference_input)) {
    reference_input %<>%
      mutate(filing_status = filing_status_input)
  }

  contexts = map(reference_tax_laws, function(reference_tax_law) {
    reference_input %>%
      left_join(reference_tax_law, by = c('year', 'filing_status')) %>%
      do_taxes(
        baseline_pr_er = NULL,
        vars_1040      = vars_1040,
        vars_payroll   = return_vars$calc_pr
      )
  })
  return(contexts)
}



state_tax_context_for_group = function(tax_units_calc, conformity_group,
                                        group_ready, state_tax_contexts) {

  #----------------------------------------------------------------------------
  # Chooses the scenario calculation for rolling or presently-unavailable
  # groups, and a cached reference calculation for ready fixed/selective groups.
  # Federal-reform safety for unavailable groups is enforced before this point.
  #----------------------------------------------------------------------------

  if (conformity_group == 0 || !group_ready) {
    return(tax_units_calc)
  }
  context = state_tax_contexts[[as.character(conformity_group)]]
  if (is.null(context)) {
    stop('Missing reference-law calculation context for conformity group ',
         conformity_group)
  }
  return(context)
}



validate_state_federal_conformity = function(state_tax_law, tax_law_id,
                                              conformity_groups =
                                                load_state_conformity_groups()) {

  #----------------------------------------------------------------------------
  # Stops a federal-reform calculation before its altered federal outputs can
  # be used by a state whose fixed/selective reference-law group is not ready.
  #----------------------------------------------------------------------------

  if (is.null(state_tax_law) || nrow(state_tax_law) == 0 ||
      identical(tax_law_id, 'baseline')) {
    return(invisible(TRUE))
  }

  affected_states = state_conformity_groups_for_law(
    state_tax_law, conformity_groups
  ) %>%
    filter(conformity_group != 0, !ready) %>%
    pull(state)

  if (length(affected_states) > 0) {
    stop(
      'Cannot calculate state results for a federal tax-law reform with ',
      'fixed/selective federal conformity until the generic reference-law ',
      'bridge is available. Affected states: ',
      paste(affected_states, collapse = ', '),
      '. Use the federal baseline tax law or exclude these jurisdictions.'
    )
  }

  invisible(TRUE)
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

  # Documentation-only entries (top-level documented_not_modeled key) are
  # transcription, not parameters: drop before parsing so they never become
  # columns. Citations inside the block are enforced by the conventions test
  tax_law = map(tax_law, function(param) {
    param[['documented_not_modeled']] = NULL
    param
  })

  # Parse all parameters and concatenate; state law floor is 2017 (plan §5.1)
  parsed = tax_law %>%
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
    select(state, everything())

  # A name the calculators do not read must fail here, not no-op downstream
  validate_state_param_names(names(parsed), st)

  return(parsed)
}



validate_state_param_names = function(parsed_names, st) {

  #----------------------------------------------------------------------------
  # Guards against silently-inert configuration (2026-07-17 review items
  # #1/#2). Every parsed subparameter column must be a name the calculators
  # read: an unknown name would otherwise no-op while ensure_st_params()
  # backfills the intended parameter with its neutral default, so a
  # misspelled or renamed parameter becomes a wrong answer with no error
  # anywhere. Legal names come from st_param_name_registry(); values encoded
  # purely for documentation belong under a top-level documented_not_modeled
  # key, which parse_one_state() skips.
  #
  # Parameters:
  #   - parsed_names (str[]) : column names of one state's parsed law
  #   - st (str)             : 2-letter postal code, for the error message
  #
  # Returns: TRUE invisibly (throws on unknown parameter names).
  #----------------------------------------------------------------------------

  registry = st_param_name_registry()
  candidates = setdiff(parsed_names, c('state', 'year', 'filing_status'))
  unknown = candidates %>%
    setdiff(registry$scalars) %>%
    keep(~ !any(str_detect(.x, registry$families)))

  if (length(unknown) > 0) {
    stop(
      'Unknown state tax law parameter(s) for ', st, ': ',
      paste(sort(unknown), collapse = ', '),
      '. The calculators do not read these names, so they would silently ',
      'have no effect while the intended parameters default to no-op ',
      'values. Fix the name (see st_param_name_registry()), or move ',
      'documentation-only entries under a documented_not_modeled key.'
    )
  }

  invisible(TRUE)
}
