#-----------------------------------------------------------------------
# config_parser.R
#
# Parses runtime options and the runscript, resolves each scenario's
# three-leg configuration (tax_law pointer, economy leg, behavior leg),
# builds version-consistent interface paths, and writes the run manifest.
#
# A scenario row is exactly: ID + three leg pointers (tax_law, economy,
# behavior) + computational scope (years, dist_years, mtr_vars, mtr_types).
# It names FILES, never values -- every value the runscript used to carry
# now lives in a folder under config/scenarios/{leg}/alternatives/, which is
# what gives it provenance. Any other column is fatal, with a message naming
# its replacement (see config/scenarios/README.md).
#-----------------------------------------------------------------------


# The runscript schema, in full. Anything else is fatal: a value that used to
# be a CSV cell now belongs in an alternative folder, and a typo should die at
# parse rather than silently do nothing.
RUNSCRIPT_FIXED_COLS = c('ID', 'tax_law', 'economy', 'behavior', 'years',
                         'dist_years', 'mtr_vars', 'mtr_types')

# Retired column -> replacement, for the hard-error message.
ALT_HINT = paste0('an economy alternative folder ',
                  '(config/scenarios/economy/alternatives/<name>/')
GONE = 'nothing -- the excess-growth machinery was removed from the model'
RUNSCRIPT_RETIRED = c(
  'dep.{X}.vintage'        = paste0(ALT_HINT, 'interfaces.yaml -> {x}_vintage)'),
  'dep.{X}.ID'             = paste0(ALT_HINT, 'interfaces.yaml -> {x}_id)'),
  'assumptions'            = 'the economy / behavior columns',
  'assumption.{ch}.{nm}'   = 'an economy or behavior alternative folder entry {ch}.{nm} (see config/scenarios/README.md)',
  's'                      = paste0(ALT_HINT, 'wealth.yaml -> financing_profile: flat:<s>)'),
  'wealth_financing'       = paste0(ALT_HINT, 'wealth.yaml -> financing_profile)'),
  'excess_growth'          = GONE,
  'excess_growth_start_year' = GONE,
  'excess_growth_all_rev'  = GONE,
  'corp_incidence_phasein' = 'nothing (it was never read)',
  'first_year'             = 'the years column ({start}:{end})',
  'last_year'              = 'the years column ({start}:{end})'
)



validate_runscript_columns = function(runscript, runscript_name) {

  #----------------------------------------------------------------------------
  # Enforces the three-leg runscript schema: the eight fixed columns and
  # nothing else. Collects ALL violations and stops once, mapping each retired
  # column to its replacement.
  #----------------------------------------------------------------------------

  cols = colnames(runscript)
  bad  = c()

  describe_retired = function(col) {
    if (str_detect(col, '^dep[.].+[.]vintage$')) {
      iface = col %>% str_remove('^dep[.]') %>% str_remove('[.]vintage$')
      key   = iface %>% str_to_lower() %>% str_replace_all('[ -]', '_')
      return(paste0(col, ' : name an economy alternative whose interfaces.yaml ',
                    'gives ', key, '_vintage'))
    }
    if (str_detect(col, '^dep[.].+[.]ID$')) {
      iface = col %>% str_remove('^dep[.]') %>% str_remove('[.]ID$')
      key   = iface %>% str_to_lower() %>% str_replace_all('[ -]', '_')
      return(paste0(col, ' : name an economy alternative whose interfaces.yaml ',
                    'gives ', key, '_id'))
    }
    if (str_detect(col, '^(economy|behavior)[.]')) {
      return(paste0(col, ' : per-value override columns were removed -- ',
                    'put the value in an alternative folder and name the ',
                    'folder in the ', str_extract(col, '^(economy|behavior)'),
                    ' column'))
    }
    if (str_detect(col, '^assumption[.]')) {
      return(paste0(col, ' : use ', RUNSCRIPT_RETIRED[['assumption.{ch}.{nm}']]))
    }
    if (col %in% names(RUNSCRIPT_RETIRED)) {
      return(paste0(col, ' : use ', RUNSCRIPT_RETIRED[[col]]))
    }
    paste0(col, ' : unknown column (the runscript schema is a strict whitelist)')
  }

  for (col in setdiff(cols, RUNSCRIPT_FIXED_COLS)) {
    bad = c(bad, describe_retired(col))
  }

  missing = setdiff(c('ID', 'tax_law', 'years'), cols)
  if (length(missing) > 0) {
    bad = c(bad, paste0('missing required column(s): ', paste(missing, collapse = ', ')))
  }

  if (length(bad) > 0) {
    stop(paste0(
      "Invalid runscript '", runscript_name, "':\n  - ",
      paste(bad, collapse = '\n  - '), '\n',
      'A runscript names FILES, not values: the schema is exactly\n  ',
      paste(RUNSCRIPT_FIXED_COLS, collapse = ', '), '\n',
      'Every per-value column mechanism (dep.*, assumption.*, s /\n',
      'wealth_financing, excess_growth*, and the dotted {leg}.{channel}.{name}\n',
      'form) was removed in the three-leg redesign; there is no fallback.\n',
      'Mapping table and auto-rewrite: config/scenarios/README.md,\n',
      '  python3 other/migrations/migrate_runscripts.py --check <runscript>\n'))
  }

  invisible(TRUE)
}



read_runscript = function(runscript_name, scenario_id) {

  #----------------------------------------------------------------------------
  # Reads and validates a runscript CSV, fills absent optional leg columns,
  # and subsets to the requested scenario. The baseline row is always
  # retained: whether baseline actually RUNS is governed by baseline_vintage
  # (main.R / src/slurm/setup.R), but its interface paths and scenario info
  # must remain resolvable either way -- post-processing looks them up by
  # ID == 'baseline' (e.g. get_other_taxes() in distribution.R), and dropping
  # the row crashes Phase 3b in scenario-subset runs.
  #----------------------------------------------------------------------------

  runscript = runscript_name %>%
    paste0('.csv') %>%
    file.path('./config/runscripts/', .) %>%
    read_csv(show_col_types = FALSE)

  validate_runscript_columns(runscript, runscript_name)

  for (col in c('economy', 'behavior', 'dist_years', 'mtr_vars', 'mtr_types')) {
    if (!(col %in% colnames(runscript))) {
      runscript[[col]] = NA_character_
    }
  }

  if (!is.null(scenario_id)) {
    if (!(scenario_id %in% runscript$ID)) {
      stop("Scenario ID '", scenario_id, "' not found in runscript")
    }
    runscript %<>%
      filter(ID %in% c('baseline', scenario_id))
  }

  runscript
}



parse_globals = function(runscript_name, scenario_id, local, vintage,
                         baseline_vintage, pct_sample, multicore) {

  #----------------------------------------------------------------------------
  # Parses data interface versioning requirements and the runscript, resolves
  # every scenario's economy and behavior legs, generates version-consistent
  # interface filepaths (confirming they exist), and writes the run manifest.
  #
  # Parameters:
  #   - runscript_name (str)   : name of runscript CSV file
  #   - scenario_id (str)      : optional name of scenario ID contained in
  #                              the runscript; "NULL" indicates all
  #   - local (int)            : whether this is a local run (1) or a production
  #                              run (0)
  #   - vintage (str)          : optional argument (NULL if not provided) to
  #                              manually supply output vintage folder rather
  #                              than being dynamically generated. Of the format
  #                              YYYYMMDDHHMM.
  #   - baseline_vintage (str) : optional argument (NULL if not provided) to
  #                              skip the baseline run and instead use an existing
  #                              baseline run for MTRs and revenue estimates. Of
  #                              the format YYYYMMDDHHMM.
  #   - pct_sample (dbl)       : share of records used in simulation
  #   - multicore (str)        : dimension across which to parallelize code. One
  #                              of three values: 'none', 'scenario', or 'year'.
  #                              Given enough cores, choose the dimension with
  #                              the largest N (generally 'year'). But note that
  #                              some behavioral feedback modules require
  #                              sequential calculation of year, in which case
  #                              'year' is not a valid option and will result in
  #                              a race condition. Always review before running!
  #
  # Returns: list of:
  #   - random_numbers (df)   : tibble of random numbers used across simulations
  #   - random_seed (int)     : the seed those draws were made under
  #   - runscript (df)        : tibble representation of the runscript CSV
  #   - scenario_configs (list): per-ID resolved configuration (economy leg,
  #                             behavior leg, interface paths)
  #   - interface_paths (df)  : tibble with ID-interface-filepath info in rows
  #   - output_root (str)     : path where output data is written
  #   - baseline_root (str)   : path where baseline data is written/read from
  #   - pct_sample (dbl)      : share of records used in simulation
  #   - sample_ids (int[])    : vector of tax unit IDs comprising the
  #                             sample population (all IDs for 100%)
  #   - detail_vars (str[])   : vector of microdata output column names
  #   - multicore (str)       : parallelization setting (see arguments)
  #   - economy_defaults / behavior_defaults : each leg's default layer
  #----------------------------------------------------------------------------

  # Set random seed. Stored in the returned globals so behavior modules can
  # re-seed before RNG use (the CLAUDE.md module convention) and so SLURM
  # workers -- fresh R processes that never run this function -- can seed
  # identically (src/slurm/common.R). This block stays FIRST: nothing between
  # here and the sample_frac/runif block below may consume RNG, or every draw
  # in the model shifts.
  random_seed = 76
  set.seed(random_seed)

  # Read interface metadata: type + version per interface. (Interface VINTAGES
  # are economy-leg configuration; the version is repo-pinned here because it
  # tracks code compatibility, not world description.)
  output_roots           = read_yaml('./config/output_roots.yaml')
  interface_versions_raw = read_yaml('./config/interfaces/interface_versions.yaml')
  interface_meta = names(interface_versions_raw) %>%
    discard(.p = ~ .x == 'Tax-Simulator') %>%
    set_names(.) %>%
    map(.f = ~ list(
      key  = str_to_lower(str_replace_all(.x, '[ -]', '_')),
      root = file.path(output_roots$production,
                       interface_versions_raw[[.x]]$type,
                       .x,
                       paste0('v', interface_versions_raw[[.x]]$version)),
      version = interface_versions_raw[[.x]]$version
    ))

  # Set model version and vintage
  version = interface_versions_raw$`Tax-Simulator`$version
  if (is.null(vintage)) {
    vintage = format(Sys.time(), '%Y%m%d%H%M')
  }

  # Determine and create directory for model output
  output_branch = file.path('Tax-Simulator', paste0('v', version), vintage)
  output_root   = file.path(output_roots$production, 'model_data', output_branch)
  if (local == 1) {
    output_root = file.path(output_roots$local, 'model_data', output_branch)
  }
  dir.create(output_root, recursive = T, showWarnings = F)

  # Determine baseline output path
  if (is.null(baseline_vintage)) {
    baseline_root = output_root
  } else {
    baseline_root = output_root %>%
      str_remove(paste0('/',vintage)) %>%
      file.path(baseline_vintage)

    if (!dir.exists(baseline_root)) {
      stop('User-supplied vintage for baseline does not exist!')
    }

    if(baseline_vintage != vintage) {
      dir.create(file.path(output_root, 'baseline'), showWarnings = T)

      file.copy(
        list.files(file.path(baseline_root, 'baseline'), full.names = T),
        file.path(output_root, 'baseline'),
        recursive = T
      )
    }
  }

  # Read and validate runscript (strict three-leg schema; baseline row retained)
  runscript = read_runscript(runscript_name, scenario_id)

  # Load each leg's default layer.
  economy_defaults  = config_load_defaults('economy')
  behavior_defaults = config_load_defaults('behavior')

  # Resolve every scenario's legs and interface paths, and run the parse-time
  # staleness check (once, here, rather than per worker mid-array).
  scenario_configs = resolve_all_scenarios(
    runscript         = runscript,
    economy_defaults  = economy_defaults,
    behavior_defaults = behavior_defaults,
    interface_meta    = interface_meta
  )

  # Flat ID-interface-path view, in runscript row order then interface_meta
  # order. Row 1 is the baseline row's first interface by convention
  # (interface_root() relies on the baseline row leading).
  interface_paths = names(scenario_configs) %>%
    map(.f = function(id) {
      tibble(
        ID        = id,
        interface = names(scenario_configs[[id]]$interface_paths),
        path      = unlist(scenario_configs[[id]]$interface_paths, use.names = FALSE)
      )
    }) %>%
    bind_rows()

  # Confirm that each path exists, throwing exception if not
  for (path in interface_paths$path) {
    if (!dir.exists(path)) {
      msg = paste0("Error: can't find directory '", path, "'. Confirm ",
                   "the interface version is correct and that the vintage exists.")
      stop(msg)
    }
  }

  # Write the run manifest
  write_run_manifest(
    output_root       = output_root,
    runscript         = runscript,
    scenario_configs  = scenario_configs,
    interface_meta    = interface_meta,
    economy_defaults  = economy_defaults,
    behavior_defaults = behavior_defaults
  )

  # Confirm that user has supplied valid multicore argument
  if (!(multicore %in% c('none', 'scenario', 'year'))) {
    stop("Invalid argument for 'multicore' runtime parameter")
  }

  # Tax unit IDs in sample. The id universe is the UNION of ids across all
  # simulation years: Tax-Data adds records in projection years (e.g. new
  # top-tail entrants absent from earlier files), so an id set built from any
  # single year silently drops them from every year of the simulation.
  # (Caught 2026-06-10 via the estate tax: the previous 2017-based id set
  # dropped 935 weight-1 records on vintage 2026060918 — all with gross
  # wealth above $50M, $8.2T in total — depressing expected estate tax ~30%.)
  # KNOWN LIMITATION (pre-dates the redesign, preserved for byte-identity):
  # the universe is read from the FIRST runscript row's Tax-Data root even if
  # another scenario overrides the Tax-Data vintage.
  tax_data_root = interface_paths %>%
    filter(interface == 'Tax-Data') %>%
    slice(1) %>%
    get_vector('path')
  sim_years = runscript$years %>%
    as.character() %>%
    map(.f = ~ as.integer(str_split_1(.x, ':'))) %>%
    map(.f = ~ .x[1]:.x[length(.x)]) %>%
    unlist() %>%
    unique()
  sample_ids = sim_years %>%
    map(.f = ~ fread(file.path(tax_data_root, paste0('tax_units_', .x, '.csv')),
                     select = 'id', showProgress = FALSE)$id) %>%
    unlist() %>%
    unique() %>%
    tibble(id = .) %>%
    sample_frac(size = pct_sample) %>%
    get_vector('id')

  # Precalculate random numbers for consistency across scenarios. Keyed by
  # id and JOINED to tax units each year (run_one_year): the per-year id
  # universe varies, so positional binding would misalign draws.
  random_numbers = tibble(
    id                = sample_ids,
    r.bus_loss        = runif(length(sample_ids)),             # Excess business loss limitation eligibility rate
    r.cdctc_takeup    = runif(length(sample_ids)),             # CDCTC takeup rate
    r.salt_workaround = runif(length(sample_ids)),             # SALT workaround participation rate
    r.oasdi_exp       = round(rexp(length(sample_ids), 1/4)),  # For OASDI claiming year imputation in do_ss_cola()
    r.new_car         = runif(length(sample_ids)),             # For imputation of p(new car | car loan interest) for auto loan deduction
    r.behavior1       = runif(length(sample_ids)),             # Spare random number for use in behavioral modules
    r.behavior2       = runif(length(sample_ids)),             # Spare random number for use in behavioral modules
    r.behavior3       = runif(length(sample_ids)),             # Spare random number for use in behavioral modules
    r.eitc_precert    = runif(length(sample_ids))              # For EITC pre-certification check
  )

  # Specifiy microdata output variable
  detail_vars = c(
    'id', 'weight', 'filer', 'dep_status', 'filing_status', 'male1', 'male2',
    'age1', 'age2', 'n_dep','n_dep_ctc', 'dep_age1', 'dep_age2', 'dep_age3',
    'wages1', 'wages2', 'wages', 'txbl_int', 'exempt_int', 'se', 'div_ord',
    'div_pref', 'txbl_kg', 'kg_st', 'kg_lt', 'sole_prop', 'sch_e', 'farm',
    'part_scorp', 'gross_ss', 'txbl_ss', 'auto_int_ded', 'above_ded', 'agi',
    'expanded_inc', 'std_ded', 'item_ded', 'med_item_ded', 'salt_item_ded',
    'first_mort_int', 'mort_int_item_ded', 'inv_int_item_ded', 'int_item_ded',
    'char_item_ded', 'casualty_item_ded', 'misc_item_ded', 'other_item_ded',
    'item_ded_ex_limits', 'itemizing', 'pe_ded', 'qbi_ded', 'tip_ded', 'ot_ded',
    'senior_ded', 'txbl_inc', 'liab_ord', 'liab_pref', 'liab_amt', 'liab_bc',
    'cdctc_nonref', 'ctc_nonref', 'ed_nonref', 'nonref', 'ed_ref', 'eitc',
    'cdctc_ref', 'ctc_ref', 'rebate', 'ref', 'liab_niit', 'liab_iit',
    'liab_iit_net', 'liab_fica_er1', 'liab_fica_er2', 'liab_seca', 'liab_pr_ee',
    'liab_pr', 'simple_filer', 'number_of_credits', 'kg_lt_infl_adj',
    'alt_max_cap_binds', 'decedent_flag', 'estate_m', 'estate_p_dsue',
    'liab_estate_nodsue', 'liab_estate_dsue', 'estate_distributable',
    'net_worth', 'liab_wealth'
  )


  # Return runtime args, resolved configurations, and interface paths
  return(list(random_numbers      = random_numbers,
              random_seed         = random_seed,
              runscript           = runscript,
              scenario_configs    = scenario_configs,
              interface_paths     = interface_paths,
              output_root         = output_root,
              baseline_root       = baseline_root,
              pct_sample          = pct_sample,
              sample_ids          = sample_ids,
              detail_vars         = detail_vars,
              multicore           = multicore,
              economy_defaults    = economy_defaults,
              behavior_defaults   = behavior_defaults,
              config_schema       = 3L))
}



resolve_all_scenarios = function(runscript, economy_defaults, behavior_defaults,
                                 interface_meta) {

  #----------------------------------------------------------------------------
  # Resolves every runscript row's economy and behavior legs, builds the
  # scenario's interface paths from the resolved vintages, and runs the
  # parse-time checks: economy staleness, and the shape of the behavior stack.
  #
  # Both legs resolve here rather than at scenario time so that a runscript
  # naming a folder that does not exist, a module file that was deleted, or an
  # inconsistent kg binding fails in the first seconds of the run instead of an
  # hour in. The behavior spec rides inside the resolved behavior object, so
  # every SLURM driver that already carries scenario_info gets it for free.
  #
  # Returns: named-by-ID list of
  #   - economy, behavior : config_resolve() outputs; behavior$spec is the
  #                         resolved module stack (see behavior_resolve())
  #   - interface_paths   : named list, interface name -> path
  #----------------------------------------------------------------------------

  out = list()

  for (i in seq_len(nrow(runscript))) {
    id  = runscript$ID[i]
    row = runscript %>% slice(i) %>% as.list()

    economy = config_resolve('economy', economy_defaults,
                             alternative = row$economy)

    # The behavior leg holds no value entries (its modules carry their own
    # parameters), so config_resolve() here only validates the folder; the
    # stack itself comes from behavior.yaml via the loader.
    behavior      = config_resolve('behavior', behavior_defaults,
                                   alternative = row$behavior)
    behavior$spec = behavior_resolve(row$behavior)
    behavior_validate_spec(behavior$spec, id)

    # Interface paths from the resolved vintages/IDs
    interface_paths = interface_meta %>%
      map(.f = function(m) {
        v  = economy$values$interfaces[[paste0(m$key, '_vintage')]]
        sid = economy$values$interfaces[[paste0(m$key, '_id')]]
        if (is.null(v) || is.null(sid)) {
          stop('economy interfaces channel is missing entries for ', m$key)
        }
        file.path(m$root, as.character(v), as.character(sid))
      })

    interface_vintages = config_interface_vintages(economy)

    config_check_staleness(
      leg                = 'economy',
      defaults           = economy_defaults,
      resolved           = economy,
      interface_vintages = interface_vintages,
      cross_values       = list(economy  = economy$values,
                                behavior = behavior$values),
      enforce            = CONFIG_ENFORCE_STALENESS
    )

    # The same check over every calibration file this scenario points at. It runs
    # here, next to the economy leg's, for the same reason: parse time is the one
    # place both the main.R path and the SLURM path pass through, so a stale
    # calibration cannot reach a cluster run unnoticed.
    calib_check_staleness(
      behavior_spec      = behavior$spec,
      interface_vintages = interface_vintages,
      enforce            = CONFIG_ENFORCE_STALENESS
    )

    out[[id]] = list(economy         = economy,
                     behavior        = behavior,
                     interface_paths = interface_paths)
  }

  out
}



write_run_manifest = function(output_root, runscript, scenario_configs,
                              interface_meta, economy_defaults,
                              behavior_defaults) {

  #----------------------------------------------------------------------------
  # Writes the vintage-root manifest:
  #   - dependencies.csv       : interface version/vintage/scenario per row
  #                              (derived view of the economy leg's interfaces
  #                              channel; kept for downstream tooling)
  #   - scenarios.csv          : one row per scenario, the three leg pointers
  #                              plus computational scope
  #   - scenario_config.csv    : every resolved value across both legs with
  #                              kind, role, override flag and source
  #   - calibrations.csv       : per scenario, every calibration value in use,
  #                              its file, its kind, and whether the scenario
  #                              bound the file or read it from a fixed path
  #   - behavioral_assumptions.csv : per scenario, the tax law, the behavior
  #                              alternative, and the stack it resolved to
  #                              (kg pieces + ordered module paths)
  #   - code_version.csv       : git commit + dirty flag
  #----------------------------------------------------------------------------

  # dependencies.csv -- deterministic order: runscript row order x
  # interface_meta order. Columns match the historical shape.
  names(scenario_configs) %>%
    map(.f = function(id) {
      eco = scenario_configs[[id]]$economy
      tibble(
        ID        = id,
        interface = names(interface_meta),
        version   = map_chr(interface_meta, .f = ~ as.character(.x$version)),
        vintage   = map_chr(interface_meta,
                            .f = ~ as.character(eco$values$interfaces[[paste0(.x$key, '_vintage')]])),
        scenario  = map_chr(interface_meta,
                            .f = ~ as.character(eco$values$interfaces[[paste0(.x$key, '_id')]]))
      )
    }) %>%
    bind_rows() %>%
    write_csv(file.path(output_root, 'dependencies.csv'))

  # scenarios.csv -- the composition record
  runscript %>%
    mutate(economy  = replace_na(as.character(economy), 'default'),
           behavior = as.character(behavior)) %>%
    select(ID, tax_law, economy, behavior, years, dist_years, mtr_vars, mtr_types) %>%
    write_csv(file.path(output_root, 'scenarios.csv'))

  # scenario_config.csv -- every resolved value, both legs
  names(scenario_configs) %>%
    map(.f = ~ bind_rows(
      config_manifest('economy',  economy_defaults,
                      scenario_configs[[.x]]$economy,  .x),
      config_manifest('behavior', behavior_defaults,
                      scenario_configs[[.x]]$behavior, .x))) %>%
    bind_rows() %>%
    write_csv(file.path(output_root, 'scenario_config.csv'))

  # calibrations.csv -- which calibration file supplied which value, and whether
  # the scenario bound it or read it from a fixed path. Written so a past vintage
  # can be read back without the code that produced it.
  names(scenario_configs) %>%
    map(.f = ~ calib_manifest(scenario_configs[[.x]]$behavior$spec, .x)) %>%
    bind_rows() %>%
    write_csv(file.path(output_root, 'calibrations.csv'))

  # behavioral_assumptions.csv -- the response record. The behavior cell names a
  # folder now, so the folder name alone would not tell a reader of an old
  # vintage what actually ran: the resolved stack is written out alongside it,
  # in execution order.
  names(scenario_configs) %>%
    map(.f = function(id) {
      spec = scenario_configs[[id]]$behavior$spec
      tibble(
        ID          = id,
        tax_law     = runscript$tax_law[runscript$ID == id],
        behavior    = spec$alternative,
        kg_dynamics = if (length(spec$kg_pieces) == 0) 'none'
                      else paste(spec$kg_pieces, collapse = ' '),
        modules     = paste(spec$modules, collapse = ' '))
    }) %>%
    bind_rows() %>%
    write_csv(file.path(output_root, 'behavioral_assumptions.csv'))

  # Record the code version the run was produced under. Without this, the
  # defaults in git cannot be reconstructed for a past vintage.
  tibble(
    commit = system2('git', c('rev-parse', 'HEAD'), stdout = TRUE, stderr = FALSE),
    dirty  = length(system2('git', c('status', '--porcelain'), stdout = TRUE, stderr = FALSE)) > 0
  ) %>%
    write_csv(file.path(output_root, 'code_version.csv'))

  invisible(NULL)
}



ensure_scenario_dirs = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Creates a scenario's output directory tree. Called once per scenario by
  # do_scenario() and src/slurm/setup.R -- get_scenario_info() itself is a
  # pure lookup and no longer touches the filesystem.
  #----------------------------------------------------------------------------

  for (type in c('static', 'conventional')) {
    dir.create(file.path(scenario_info$output_path, type, 'detail'),
               recursive = T, showWarnings = F)
    dir.create(file.path(scenario_info$output_path, type, 'totals'),
               recursive = T, showWarnings = F)
    dir.create(file.path(scenario_info$output_path, type, 'supplemental'),
               recursive = T, showWarnings = F)
  }
  dir.create(file.path(scenario_info$output_path, 'static/supplemental/child_earnings'),
             showWarnings = F)

  invisible(NULL)
}



get_scenario_info = function(id, g = globals) {

  #----------------------------------------------------------------------------
  # Given a scenario ID, assembles scenario-specific runtime information from
  # the resolutions cached in globals. Pure lookup: no filesystem side
  # effects (see ensure_scenario_dirs) and no re-resolution.
  #
  # Parameters:
  #   - id (str) : scenario ID
  #   - g (list) : the globals object (defaulted; passed explicitly where a
  #                worker holds it under another name)
  #
  # Returns: named list (see below)
  #----------------------------------------------------------------------------

  # Scenario-specific output root
  output_root = file.path(ifelse(id == 'baseline',
                                 g$baseline_root,
                                 g$output_root),
                          id)

  config = g$scenario_configs[[id]]
  if (is.null(config)) {
    stop("Scenario ID '", id, "' has no resolved configuration (not in this runscript?)")
  }

  # List of scenario-specific runscript row, named by column name
  runscript_items = g$runscript %>%
    filter(ID == id) %>%
    as.list()

  # Module paths for this scenario's behavioral feedback, already in execution
  # order with the kg applier injected (see behavior_resolve()). NULL rather
  # than character(0) when there is no response: run.R tests length() and the
  # distinction never mattered, but NULL is what it has always been handed.
  behavior_modules = config$behavior$spec$modules
  if (length(behavior_modules) == 0) behavior_modules = NULL

  # Years to run; distribution/microdata years default to all years
  years      = parse_year_spec(runscript_items$years)
  dist_years = if (is.na(runscript_items$dist_years)) years
               else parse_year_spec(runscript_items$dist_years)

  # Names of variables for which to calculate marginal tax rates
  mtr_vars = NULL
  if (!is.na(runscript_items$mtr_vars)) {
    mtr_vars = str_split_1(runscript_items$mtr_vars, ' ')
  }

  # Types of MTRs, with same index as MTR vars above
  mtr_types = NULL
  if (!is.na(runscript_items$mtr_types)) {
    mtr_types = str_split_1(runscript_items$mtr_types, ' ')
  }

  # Return as named list
  return(list(ID                       = id,
              output_path              = output_root,
              interface_paths          = config$interface_paths,
              tax_law_id               = runscript_items$tax_law,
              behavior_modules         = behavior_modules,
              years                    = years,
              dist_years               = dist_years,
              mtr_vars                 = mtr_vars,
              mtr_types                = mtr_types,
              resolved_economy         = config$economy,
              resolved_behavior        = config$behavior))
}
