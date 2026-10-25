# =============================================================================
# supplemental_inputs.R  --  the supplemental-inputs RUNTIME for Tax-Simulator
#
# Optional DATA columns for the state calculator: variables the pinned
# Tax-Data vintage does not carry (rent paid, tenure, pension source, ...),
# published by another model -- Affordability-Index first, Tax-Data itself
# later -- under one file contract, and joined onto the tax units inside the
# per-state loop. Decision S43; contract and design in
# research/state_tax/supplemental_inputs_interface.md.
#
# The pattern is State-Weights (src/data/state_weights.R): a yaml entry in
# config/interfaces/interface_versions.yaml, a runscript opt-in, a pin on the
# producer vintage, a parse-time check that the producer was built on the
# Tax-Data vintage the row reads, and a reader called once per year inside
# run_one_year(). Two things differ. The opt-in is a LIST OF VARIABLES
# (`supplemental_inputs` runscript column), not a method switch, so a run says
# exactly which imputations it used; and the reader fills NOTHING -- an
# uncovered record is NA, and ensure_st_data() (do_state_taxes.R) applies the
# schema's neutral default and sets a has_<v> flag, so coverage is reported
# in static/supplemental/supplemental_inputs_coverage.csv rather than patched.
#
# Invariants, each with a test in src/tests/state/test_supplemental_inputs_reader.R:
#   - a run that does not opt in never pins, reads or depends on a producer;
#   - federal output is byte-identical with or without an overlay, because the
#     columns are joined only inside get_state_totals();
#   - a provision gated on an absent or all-default overlay produces exactly
#     today's state liability.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

SUPPLEMENTAL_CONTRACT = 'supplemental_inputs'   # interface_versions.yaml `contract:` value
SUP_PREFIX            = 'sup.'                   # overlay columns on the way in, like `r.` random numbers


# -----------------------------------------------------------------------------
# Producers and schema
# -----------------------------------------------------------------------------

supplemental_input_interfaces = function(path = './config/interfaces/interface_versions.yaml') {

  #----------------------------------------------------------------------------
  # Names of the interface_versions.yaml entries that carry the
  # supplemental-inputs contract, i.e. every model whose vintages can be read
  # by build_supplemental_inputs(). One entry per producer, never one shared
  # "Supplemental-Inputs" namespace: the pin check reads the PRODUCER's
  # dependencies.csv, and two producers may be pinned at once.
  #
  # Returns: character vector of interface names (str[]).
  #----------------------------------------------------------------------------

  iv = read_yaml(path)
  names(iv)[map_lgl(iv, ~ identical(.x$contract, SUPPLEMENTAL_CONTRACT))]
}



supplemental_schema = local({

  #----------------------------------------------------------------------------
  # Reads and caches config/interfaces/supplemental_inputs_schema.yaml, the
  # consumer-side contract for optional data columns (the data twin of
  # st_param_schema()). Cached per path because ensure_st_data() runs per
  # state-year.
  #
  # Returns: list with
  #   vars     tibble(variable, key, type, split, shadow, tax_data_equivalent)
  #   defaults named list of typed neutral defaults
  #----------------------------------------------------------------------------

  cache = list()
  function(path = './config/interfaces/supplemental_inputs_schema.yaml') {
    if (!is.null(cache[[path]])) {
      return(cache[[path]])
    }
    raw = read_yaml(path)
    if (length(raw) == 0 || is.null(names(raw)) || anyDuplicated(names(raw))) {
      stop('supplemental_inputs_schema.yaml is malformed: variables must be uniquely named')
    }
    field = function(name, default) unname(map_chr(raw, ~ .x[[name]] %||% default))
    vars = tibble(
      variable            = names(raw),
      key                 = field('key',    NA_character_),
      type                = field('type',   NA_character_),
      split               = field('split',  'whole'),
      shadow              = field('shadow', 'forbid'),
      tax_data_equivalent = field('tax_data_equivalent', NA_character_),
      values              = unname(map(raw, ~ unlist(.x$values)))
    )
    bad = vars %>%
      filter(!(key %in% c('unit', 'state', 'person')) | !(type %in% c('dbl', 'int', 'lgl')) |
             !(split %in% c('halve', 'whole', 'person')) | !(shadow %in% c('forbid', 'allow')) |
             (key == 'person' & !grepl('[12]$', variable)))
    if (nrow(bad) > 0) {
      stop('supplemental_inputs_schema.yaml: bad key/type/split/shadow (or a person ',
           'variable not ending in 1 or 2) for ', paste(bad$variable, collapse = ', '))
    }
    defaults = map2(raw, vars$type, function(x, type) {
      if (is.null(x$default)) stop('supplemental_inputs_schema.yaml: no default for a variable')
      sup_cast(x$default, type)
    })
    names(defaults) = vars$variable
    cache[[path]] <<- list(vars = vars, defaults = defaults)
    cache[[path]]
  }
})



sup_cast = function(x, type) {
  # Coercion NAs are detected by the caller (build_supplemental_inputs stops
  # when a cast introduces one), so the base warning is noise here
  suppressWarnings(switch(type,
         dbl = as.numeric(x),
         int = as.integer(x),
         lgl = as.logical(x),
         stop('sup_cast(): unknown type ', type)))
}



sup_out_of_domain = function(raw, type, values = NULL) {

  #----------------------------------------------------------------------------
  # Flags producer values that sup_cast() would silently reinterpret rather
  # than turn into NA: a fraction cast to int (1.6 -> 1), a number other than
  # 0/1 cast to lgl (2 -> TRUE), or a code outside the schema's `values`.
  #
  # Parameters:
  #   - raw (vec)    : the column as read, before sup_cast()
  #   - type (str)   : schema type (dbl | int | lgl)
  #   - values (vec) : allowed values from the schema, or NULL for any
  #
  # Returns: logical vector, TRUE where a non-NA value is out of domain.
  #----------------------------------------------------------------------------

  num = suppressWarnings(as.numeric(raw))
  bad = switch(type,
               dbl = rep(FALSE, length(raw)),
               int = !is.na(num) & num != round(num),
               lgl = if (is.logical(raw)) rep(FALSE, length(raw)) else
                       !is.na(raw) & !(toupper(as.character(raw)) %in% c('0', '1', 'TRUE', 'FALSE')))
  if (!is.null(values)) {
    bad = bad | (!is.na(raw) & !(sup_cast(raw, type) %in% sup_cast(values, type)))
  }
  bad
}



supplemental_split_classes = function(schema = supplemental_schema()) {

  #----------------------------------------------------------------------------
  # How the married-separate election (st_split.R) treats each optional data
  # column, read from the schema rather than listed in st_split.R so a new
  # variable is classified where it is declared.
  #
  # Returns: list(halve = str[], person_bases = str[], person_empty = named
  #          list of the slot-2 defaults).
  #----------------------------------------------------------------------------

  v = schema$vars
  person = v %>% filter(split == 'person')
  bases  = unique(sub('[12]$', '', person$variable))
  empty  = schema$defaults[paste0(bases, 2)]
  list(halve        = v$variable[v$split == 'halve'],
       person_bases = bases,
       person_empty = empty)
}



# -----------------------------------------------------------------------------
# Runscript parsing and parse-time validation (called by parse_globals())
# -----------------------------------------------------------------------------

parse_supplemental_value = function(x) {

  #----------------------------------------------------------------------------
  # Parses a runscript `supplemental_inputs` cell: space-delimited variable
  # names. No 'all' keyword -- a run names exactly what it consumes.
  #
  # Returns: character vector (str[]), possibly empty.
  #----------------------------------------------------------------------------

  if (is.null(x) || is.na(x)) return(character())
  v = str_split_1(str_trim(as.character(x)), '\\s+')
  v[nzchar(v)]
}



parse_years_value = function(x) {

  #----------------------------------------------------------------------------
  # Parses a runscript `years` cell ('{start}:{end}') into an integer sequence.
  #----------------------------------------------------------------------------

  parts = as.integer(str_split_1(str_trim(as.character(x)), ':'))
  if (length(parts) == 1) return(parts)
  seq(parts[1], parts[2])
}



runscript_years_by_id = function(runscript) {
  setNames(lapply(runscript$years, parse_years_value), runscript$ID)
}



blank_supplemental_pins = function(runscript,
                                   producers = supplemental_input_interfaces()) {

  #----------------------------------------------------------------------------
  # A producer vintage is a dependency of rows that OPT IN by naming variables
  # in `supplemental_inputs`. Every other row gets its producer pins blanked,
  # exactly as parse_globals() does for State-Weights on federal-only rows, so
  # the dependencies table and interface paths carry no producer row and no
  # directory is required to exist. A row that opts in with no pin stops:
  # no producer carries a default vintage while provisional, and resolving to
  # one silently would make the run depend on something the runscript never
  # named.
  #
  # Also drops a stray dep.<producer>.ID column that parse_globals' default
  # loop creates when the yaml entry has a default_id but no default_vintage.
  #
  # Returns: the runscript tibble with producer pins blanked where not opted in.
  #----------------------------------------------------------------------------

  opted_in = if ('supplemental_inputs' %in% colnames(runscript)) {
    !is.na(runscript$supplemental_inputs) & nzchar(str_trim(runscript$supplemental_inputs))
  } else {
    rep(FALSE, nrow(runscript))
  }

  pinned_any = rep(FALSE, nrow(runscript))
  for (p in producers) {
    v_col = paste0('dep.', p, '.vintage')
    id_col = paste0('dep.', p, '.ID')
    if (!(v_col %in% colnames(runscript))) {
      if (id_col %in% colnames(runscript)) runscript[[id_col]] = NULL
      next
    }
    if (!(id_col %in% colnames(runscript))) {
      stop('Runscript pins ', v_col, ' without ', id_col)
    }
    runscript[[v_col]][!opted_in] = NA
    runscript[[id_col]][!opted_in] = NA
    pinned_any = pinned_any | (!is.na(runscript[[v_col]]) & !is.na(runscript[[id_col]]))
  }

  if (any(opted_in & !pinned_any)) {
    stop("Scenario(s) ", paste(runscript$ID[opted_in & !pinned_any], collapse = ', '),
         " set `supplemental_inputs` but pin no producer vintage; add ",
         "dep.<producer>.vintage and dep.<producer>.ID for one of: ",
         paste(producers, collapse = ', '))
  }
  runscript
}



validate_runscript_supplemental = function(runscript,
                                           producers = supplemental_input_interfaces()) {

  #----------------------------------------------------------------------------
  # Cross-row rules for the `supplemental_inputs` column, checked before any
  # scenario subsetting (same place as validate_runscript_states()):
  #   1. a row may opt in only if it is a state-mode row (`states` set) --
  #      the overlay is state-calculator data and must not touch a federal run;
  #   2. every state-mode counterfactual uses the baseline row's variable set
  #      and producer pins: supplemental data is a baseline attribute, and a
  #      counterfactual carrying different data would score data as policy.
  #
  # Returns: TRUE invisibly; stops on violation.
  #----------------------------------------------------------------------------

  if (!('supplemental_inputs' %in% colnames(runscript))) {
    return(invisible(TRUE))
  }
  opted_in = !is.na(runscript$supplemental_inputs) & nzchar(str_trim(runscript$supplemental_inputs))
  has_states = if ('states' %in% colnames(runscript)) !is.na(runscript$states) else
               rep(FALSE, nrow(runscript))
  if (any(opted_in & !has_states)) {
    stop("Scenario(s) ", paste(runscript$ID[opted_in & !has_states], collapse = ', '),
         " set `supplemental_inputs` without `states`; supplemental inputs are ",
         "state-calculator data and cannot be requested by a federal-only row")
  }

  bl = runscript[runscript$ID == 'baseline', ]
  cf = runscript[runscript$ID != 'baseline' & has_states, ]
  if (nrow(bl) == 0 || nrow(cf) == 0) {
    return(invisible(TRUE))
  }
  bl_vars = sort(parse_supplemental_value(bl$supplemental_inputs[1]))
  for (i in seq_len(nrow(cf))) {
    cf_vars = sort(parse_supplemental_value(cf$supplemental_inputs[i]))
    if (!identical(bl_vars, cf_vars)) {
      stop("Scenario '", cf$ID[i], "' requests supplemental inputs (",
           paste(cf_vars, collapse = ' '), ") but the baseline row requests (",
           paste(bl_vars, collapse = ' '), "); supplemental inputs are frozen across scenarios")
    }
    for (p in producers) {
      v_col = paste0('dep.', p, '.vintage'); id_col = paste0('dep.', p, '.ID')
      if (!(v_col %in% colnames(runscript))) next
      same = identical(cf[[v_col]][i], bl[[v_col]][1]) && identical(cf[[id_col]][i], bl[[id_col]][1])
      if (!same) {
        stop("Scenario '", cf$ID[i], "' pins ", p, ' ', cf[[v_col]][i], '/', cf[[id_col]][i],
             ' but the baseline row pins ', bl[[v_col]][1], '/', bl[[id_col]][1],
             '; supplemental inputs are frozen across scenarios')
      }
    }
  }
  invisible(TRUE)
}



validate_supplemental_stored_baseline = function(runscript, baseline_root,
                                                 producers = supplemental_input_interfaces()) {

  #----------------------------------------------------------------------------
  # The baseline_vintage counterpart of validate_runscript_supplemental()'s
  # freeze rule: a runscript with no baseline row reuses a stored baseline, so
  # each state-mode counterfactual must request the variables that baseline
  # ran with (its static/supplemental/supplemental_inputs_coverage.csv; none
  # when the file is absent) and pin the same producer vintage/ID (its
  # vintage-level dependencies.csv).
  #
  # Parameters:
  #   - runscript (df)      : full runscript tibble (all rows)
  #   - baseline_root (str) : the stored baseline vintage directory
  #   - producers (str[])   : producer interface names
  #
  # Returns: TRUE invisibly; stops on violation.
  #----------------------------------------------------------------------------

  if (any(runscript$ID == 'baseline') || !('states' %in% colnames(runscript))) {
    return(invisible(TRUE))
  }
  cf = runscript[!is.na(runscript$states), ]
  if (nrow(cf) == 0) {
    return(invisible(TRUE))
  }

  coverage_path = file.path(baseline_root, 'baseline', 'static', 'supplemental',
                            'supplemental_inputs_coverage.csv')
  stored_vars = if (file.exists(coverage_path)) {
    sort(unique(read_csv(coverage_path, show_col_types = FALSE)$variable))
  } else {
    character()
  }
  deps_path = file.path(baseline_root, 'dependencies.csv')
  stored_deps = if (file.exists(deps_path)) {
    read_csv(deps_path, col_types = cols(.default = 'c')) %>%
      filter(ID == 'baseline', interface %in% producers)
  } else {
    tibble(ID = character(), interface = character(), vintage = character(), scenario = character())
  }

  for (i in seq_len(nrow(cf))) {
    cf_vars = if ('supplemental_inputs' %in% colnames(cf)) {
      sort(parse_supplemental_value(cf$supplemental_inputs[i]))
    } else {
      character()
    }
    if (!identical(cf_vars, stored_vars)) {
      stop("Scenario '", cf$ID[i], "' requests supplemental inputs (",
           paste(cf_vars, collapse = ' '), ") but the stored baseline at ", baseline_root,
           " ran with (", paste(stored_vars, collapse = ' '),
           "); supplemental inputs are frozen across scenarios")
    }
    if (length(cf_vars) == 0) next
    for (p in producers) {
      v_col = paste0('dep.', p, '.vintage'); id_col = paste0('dep.', p, '.ID')
      cf_pin = if (v_col %in% colnames(cf)) c(cf[[v_col]][i], cf[[id_col]][i]) else c(NA, NA)
      stored = stored_deps %>% filter(interface == p)
      stored_pin = if (nrow(stored) == 1) c(stored$vintage, stored$scenario) else c(NA, NA)
      if (!identical(as.character(cf_pin), as.character(stored_pin))) {
        stop("Scenario '", cf$ID[i], "' pins ", p, ' ', cf_pin[1], '/', cf_pin[2],
             ' but the stored baseline pins ', stored_pin[1], '/', stored_pin[2],
             '; supplemental inputs are frozen across scenarios')
      }
    }
  }
  invisible(TRUE)
}



tax_data_vintages_match = function(a, b, years = NULL,
                                   path = './config/interfaces/tax_data_equivalences.yaml') {

  #----------------------------------------------------------------------------
  # Whether two Tax-Data vintage names may be treated as the same records for
  # a join keyed on `id`: identical names, or a declared equivalence in
  # config/interfaces/tax_data_equivalences.yaml covering every requested year.
  # Used by validate_state_weights_pins() and validate_supplemental_pins().
  #
  # Parameters:
  #   - a, b (str)   : vintage names
  #   - years (int[]): run years that must be covered by the declaration; NULL
  #                    means the caller has no year information, in which case
  #                    only identical names match
  #
  # Returns: TRUE/FALSE.
  #----------------------------------------------------------------------------

  if (length(a) != 1 || length(b) != 1) return(FALSE)
  if (identical(as.character(a), as.character(b))) return(TRUE)
  if (is.null(years) || !file.exists(path)) return(FALSE)
  for (e in read_yaml(path)$equivalences) {
    if (all(c(a, b) %in% as.character(e$vintages)) && all(years %in% unlist(e$years))) {
      return(TRUE)
    }
  }
  FALSE
}



read_supplemental_manifest = function(root) {
  f = file.path(root, 'manifest.csv')
  if (!file.exists(f)) {
    stop('Supplemental-inputs vintage at ', root, ' has no manifest.csv')
  }
  m = readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = 'c'))
  need = c('variable', 'key', 'type')
  if (!all(need %in% names(m))) {
    stop('manifest.csv at ', root, ' lacks column(s) ', paste(setdiff(need, names(m)), collapse = ', '))
  }
  m
}



validate_supplemental_pins = function(dependencies, interface_paths, runscript,
                                      schema = supplemental_schema(),
                                      producers = supplemental_input_interfaces()) {

  #----------------------------------------------------------------------------
  # Parse-time checks once the interface paths are known (the twin of
  # validate_state_weights_pins()):
  #   - every pinned producer vintage has a dependencies.csv naming the
  #     Tax-Data vintage it was built on, and that vintage matches the one the
  #     row reads (name-identical or declared equivalent for the run's years);
  #   - every pinned producer has a manifest.csv;
  #   - every requested variable is in the consumer schema, is published by
  #     EXACTLY one producer pinned on that row, and the manifest's key and
  #     type agree with the schema.
  #
  # Parameters:
  #   dependencies    tibble(ID, interface, version, vintage, scenario)
  #   interface_paths tibble(ID, interface, path)
  #   runscript       the (possibly subset) runscript tibble
  #
  # Returns: TRUE invisibly; stops on violation.
  #----------------------------------------------------------------------------

  rows = interface_paths %>% filter(interface %in% producers)
  if (nrow(rows) == 0) {
    return(invisible(TRUE))
  }
  years_by_id = runscript_years_by_id(runscript)

  manifests = list()
  for (i in seq_len(nrow(rows))) {
    dep_file = file.path(dirname(rows$path[i]), 'dependencies.csv')
    if (!file.exists(dep_file)) {
      stop('Supplemental-inputs vintage at ', rows$path[i], ' has no dependencies.csv; ',
           'cannot tell which Tax-Data vintage it was built on')
    }
    built_on = readr::read_csv(dep_file, show_col_types = FALSE) %>%
      filter(interface == 'Tax-Data') %>%
      pull(vintage)
    reads = dependencies %>%
      filter(ID == rows$ID[i], interface == 'Tax-Data') %>%
      pull(vintage)
    if (!tax_data_vintages_match(built_on, reads, years_by_id[[rows$ID[i]]])) {
      stop("Scenario '", rows$ID[i], "' reads Tax-Data ", paste(reads, collapse = ','),
           ' but its ', rows$interface[i], ' vintage was built on Tax-Data ',
           paste(built_on, collapse = ','), ' (no declared equivalence covering ',
           'the run years in config/interfaces/tax_data_equivalences.yaml)')
    }
    manifests[[paste(rows$ID[i], rows$interface[i])]] = read_supplemental_manifest(rows$path[i]) %>%
      mutate(ID = rows$ID[i], interface = rows$interface[i])
  }
  manifests = bind_rows(manifests)

  if (!('supplemental_inputs' %in% colnames(runscript))) {
    return(invisible(TRUE))
  }
  for (i in seq_len(nrow(runscript))) {
    vars = parse_supplemental_value(runscript$supplemental_inputs[i])
    if (length(vars) == 0) next
    id = runscript$ID[i]
    unknown = setdiff(vars, schema$vars$variable)
    if (length(unknown) > 0) {
      stop("Scenario '", id, "' requests supplemental input(s) not in ",
           'config/interfaces/supplemental_inputs_schema.yaml: ', paste(unknown, collapse = ', '))
    }
    for (v in vars) {
      pub = manifests %>% filter(ID == id, variable == v)
      if (nrow(pub) != 1) {
        stop("Scenario '", id, "': supplemental input '", v, "' is published by ",
             nrow(pub), ' pinned producer(s) (', paste(pub$interface, collapse = ', '),
             '); exactly one is required')
      }
      sch = schema$vars %>% filter(variable == v)
      if (!identical(pub$key, sch$key) || !identical(pub$type, sch$type)) {
        stop("Scenario '", id, "': ", pub$interface, " publishes '", v, "' as ",
             pub$key, '/', pub$type, ' but the schema declares ', sch$key, '/', sch$type)
      }
    }
  }
  invisible(TRUE)
}



# -----------------------------------------------------------------------------
# Reader (called once per year inside run_one_year()) and attach (per state)
# -----------------------------------------------------------------------------

build_supplemental_inputs = function(tax_units, year, variables, roots,
                                     states = NULL, state_weights = NULL,
                                     pct_sample = 1, schema = supplemental_schema()) {

  #----------------------------------------------------------------------------
  # Reads the requested variables for one year from the pinned producer
  # vintages and returns them keyed for the per-state join, plus a coverage
  # table. Fills nothing: an uncovered record is NA.
  #
  # Checks: year file present for every key type needed; required columns
  # present; key unique; no orphan ids (full sample only -- a producer id not
  # in this year's tax units is the symptom of a wrong vintage, and the parse-
  # time pin check is the first guard); known states; types cast without
  # introducing NAs; no collision with an existing column unless the schema
  # allows shadowing. Overlay columns are carried under SUP_PREFIX so no
  # federal req_vars can ever name one.
  #
  # Parameters:
  #   - tax_units (df)     : this year's post-subsample tax units (id, weight, ...)
  #   - year (int)         : tax year
  #   - variables (str[])  : requested variable names (scenario_info$supplemental_inputs)
  #   - roots (list)       : producer interface name -> scenario directory
  #   - states (str[])     : requested states (state-keyed rows filtered to these)
  #   - state_weights (df) : long (id, state, weight), for weighted coverage
  #   - pct_sample (dbl)   : run sample share
  #
  # Returns: NULL when nothing is requested, else list(
  #   unit     = tibble(id, sup.<v>, ...) or NULL,
  #   state    = tibble(id, state, sup.<v>, ...) or NULL,
  #   coverage = tibble(year, variable, key, interface, vintage, state,
  #                     n_records_covered, share_records, share_weighted))
  #----------------------------------------------------------------------------

  if (is.null(variables) || length(variables) == 0) {
    return(NULL)
  }
  unknown = setdiff(variables, schema$vars$variable)
  if (length(unknown) > 0) {
    stop('build_supplemental_inputs(): unknown variable(s) ', paste(unknown, collapse = ', '))
  }
  if (length(roots) == 0) {
    stop('build_supplemental_inputs(): variables requested but no producer interface path; ',
         'the runscript row needs dep.<producer>.vintage/ID')
  }
  vars = schema$vars %>% filter(variable %in% variables)

  # Collisions with columns already on the tax units
  clash = vars %>% filter(variable %in% names(tax_units), shadow != 'allow')
  if (nrow(clash) > 0) {
    stop('build_supplemental_inputs(): ', paste(clash$variable, collapse = ', '),
         ' already exist(s) on the tax units and the schema does not allow shadowing')
  }

  # Which producer publishes which requested variable (exactly one each)
  manifests = imap(roots, ~ read_supplemental_manifest(.x) %>% mutate(interface = .y)) %>%
    bind_rows() %>%
    filter(variable %in% vars$variable)
  dup = manifests %>% count(variable) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    stop('build_supplemental_inputs(): ', paste(dup$variable, collapse = ', '),
         ' published by more than one pinned producer')
  }
  missing = setdiff(vars$variable, manifests$variable)
  if (length(missing) > 0) {
    stop('build_supplemental_inputs(): no pinned producer publishes ', paste(missing, collapse = ', '))
  }

  jurisdictions = c(STATE_JURISDICTIONS, NONTAX_BUCKETS)
  out = list(unit = NULL, state = NULL)
  coverage = list()

  for (key_type in c('unit', 'state')) {
    need = vars %>% filter(key %in% (if (key_type == 'unit') c('unit', 'person') else 'state'))
    if (nrow(need) == 0) next
    key_cols = if (key_type == 'unit') 'id' else c('id', 'state')
    merged = NULL

    for (p in unique(manifests$interface[manifests$variable %in% need$variable])) {
      p_vars = need$variable[need$variable %in% manifests$variable[manifests$interface == p]]
      f = file.path(roots[[p]], sprintf('supplemental_%s_%d.csv.gz', key_type, year))
      if (!file.exists(f)) {
        stop('build_supplemental_inputs(): no ', key_type, '-keyed supplemental inputs for ',
             year, ' in the pinned ', p, ' vintage: ', f)
      }
      d = data.table::fread(f) %>% as_tibble()
      absent = setdiff(c(key_cols, p_vars), names(d))
      if (length(absent) > 0) {
        stop('build_supplemental_inputs(): ', basename(f), ' lacks column(s) ',
             paste(absent, collapse = ', '))
      }
      d = d %>% select(all_of(c(key_cols, p_vars)))
      if (pct_sample >= 1 && any(!(d$id %in% tax_units$id))) {
        stop('build_supplemental_inputs(): ', sum(!(d$id %in% tax_units$id)), ' id(s) in ',
             basename(f), ' are not in the TY', year, ' tax units; the ', p,
             ' vintage was built on a different Tax-Data vintage')
      }
      d = d %>% filter(id %in% tax_units$id)
      if (anyDuplicated(d[key_cols]) > 0) {
        stop('build_supplemental_inputs(): duplicate (', paste(key_cols, collapse = ', '),
             ') keys in ', basename(f))
      }
      if (key_type == 'state') {
        bad_state = setdiff(unique(d$state), jurisdictions)
        if (length(bad_state) > 0) {
          stop('build_supplemental_inputs(): unknown state(s) ', paste(bad_state, collapse = ' '),
               ' in ', basename(f))
        }
        if (!is.null(states)) d = d %>% filter(state %in% states)
      }
      for (v in p_vars) {
        type = need$type[need$variable == v]
        was_na = is.na(d[[v]])
        out_of_domain = sup_out_of_domain(d[[v]], type, need$values[[which(need$variable == v)]])
        if (any(out_of_domain)) {
          stop('build_supplemental_inputs(): ', sum(out_of_domain), ' value(s) of ', v, ' in ',
               basename(f), ' are outside its domain (type ', type, ', e.g. ',
               d[[v]][which(out_of_domain)[1]], ')')
        }
        d[[v]] = sup_cast(d[[v]], type)
        if (any(is.na(d[[v]]) & !was_na)) {
          stop('build_supplemental_inputs(): ', v, ' in ', basename(f), ' cannot be read as ', type)
        }
        vintage = basename(dirname(roots[[p]]))
        if (key_type == 'unit') {
          covered = tax_units %>% select(id, weight) %>%
            left_join(d %>% select(id, value = all_of(v)), by = 'id') %>%
            mutate(covered = !is.na(value))
          coverage[[length(coverage) + 1]] = tibble(
            year = year, variable = v, key = need$key[need$variable == v], interface = p,
            vintage = vintage, state = NA_character_,
            n_records_covered = sum(covered$covered),
            share_records     = mean(covered$covered),
            share_weighted    = sum(covered$weight[covered$covered]) / sum(covered$weight))
        } else {
          for (st in (if (is.null(states)) unique(d$state) else states)) {
            sw = if (is.null(state_weights)) NULL else state_weights %>% filter(state == st)
            covered = tax_units %>% select(id) %>%
              left_join(d %>% filter(state == st) %>% select(id, value = all_of(v)), by = 'id') %>%
              mutate(covered = !is.na(value))
            w_share = if (is.null(sw)) NA_real_ else {
              cw = covered %>% left_join(sw %>% select(id, w = weight), by = 'id') %>%
                mutate(w = replace_na(w, 0))
              if (sum(cw$w) > 0) sum(cw$w[cw$covered]) / sum(cw$w) else NA_real_
            }
            coverage[[length(coverage) + 1]] = tibble(
              year = year, variable = v, key = 'state', interface = p, vintage = vintage,
              state = st, n_records_covered = sum(covered$covered),
              share_records = mean(covered$covered), share_weighted = w_share)
          }
        }
      }
      d = d %>% rename_with(~ paste0(SUP_PREFIX, .x), all_of(p_vars))
      merged = if (is.null(merged)) d else full_join(merged, d, by = key_cols)
    }
    out[[key_type]] = merged
  }

  out$coverage = bind_rows(coverage)
  out
}



attach_supplemental_inputs = function(tax_units, supplemental, state) {

  #----------------------------------------------------------------------------
  # Joins the overlay columns (SUP_PREFIX-ed) onto one state's calculation
  # context: unit-keyed values by id, state-keyed values filtered to `state`
  # then by id. Pure; row order preserved. ensure_st_data() resolves the
  # prefixed columns to their bare names with defaults and has_<v> flags.
  #
  # Returns: tax_units with sup.* columns added (df).
  #----------------------------------------------------------------------------

  if (is.null(supplemental)) {
    return(tax_units)
  }
  if (!is.null(supplemental$unit)) {
    tax_units = tax_units %>% left_join(supplemental$unit, by = 'id')
  }
  if (!is.null(supplemental$state)) {
    tax_units = tax_units %>%
      left_join(supplemental$state %>% filter(state == !!state) %>% select(-state), by = 'id')
  }
  tax_units
}



write_supplemental_coverage = function(output, static_root) {

  #----------------------------------------------------------------------------
  # Writes static/supplemental/supplemental_inputs_coverage.csv -- one row per
  # (year, variable[, state]) saying which optional data variables were live,
  # from which producer vintage, and the share of records (and of weight)
  # covered -- so a reader of the results knows which provisions were on.
  # Nothing is written when no year carried an overlay. Called by run_sim()
  # and by the SLURM aggregator (Phase 3a) alike.
  #
  # Parameters:
  #   - output (list)     : per-year run_one_year() results
  #   - static_root (str) : the scenario's static output directory
  #----------------------------------------------------------------------------

  coverage = output %>%
    map(~ .x$static_totals$supplemental_coverage) %>%
    compact() %>%
    bind_rows()
  if (nrow(coverage) > 0) {
    write_csv(coverage, file.path(static_root, 'supplemental', 'supplemental_inputs_coverage.csv'))
  }
  invisible(coverage)
}
