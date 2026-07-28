#-----------------------------------------------------------------------
# config_parser.R
#
# Contains functions to parse the runscript and resolve each scenario's
# configuration
#
# A scenario row is an ID, pointers to its tax law, economy and behavior, and the
# scope of the computation: which years to run, which to build distribution tables
# for, and which marginal tax rates to calculate. It names folders, never values.
# Every value a runscript used to carry now lives in a folder instead, which is
# what gives it a record of where it came from. Any other column stops the run,
# with a message naming what replaced it.
#-----------------------------------------------------------------------


# The whole runscript schema. Anything else stops the run: a value now belongs in
# an alternative folder, and a misspelled column should fail rather than be
# ignored.
RUNSCRIPT_FIXED_COLS = c('ID', 'tax_law', 'economy', 'behavior', 'years',
                         'dist_years', 'mtr_vars', 'mtr_types')

# What each retired column was replaced by, for the error message
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
  # Checks the runscript's columns: the eight above and nothing else. Collects
  # every problem and stops once, naming what replaced each retired column.
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
  # Reads and checks a runscript, filling in any leg column left out and cutting to
  # the requested scenario.
  #
  # The baseline row is always kept. Whether the baseline actually runs depends on
  # whether an existing one was supplied, but its paths must stay resolvable either
  # way: post-processing looks the baseline up by name, and dropping the row breaks
  # a run of a single scenario.
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



tax_data_sample_universe = function(interface_paths, runscript) {

  #----------------------------------------------------------------------------
  # Builds the population from every Tax-Data path/year pair used by the run.
  #
  # Tax-Data vintages do not necessarily contain the same IDs, and projection
  # years can add new IDs within a vintage. Pairing each scenario's path with its
  # own year specification includes both kinds of additions without trying to
  # read irrelevant path/year combinations.
  #
  # Returns: unique vector of tax-unit IDs, before sampling.
  #----------------------------------------------------------------------------

  tax_data_sources = interface_paths %>%
    filter(interface == 'Tax-Data') %>%
    select(ID, path) %>%
    left_join(runscript %>% select(ID, years), by = 'ID')

  if (nrow(tax_data_sources) == 0) {
    stop('Cannot build the sample universe: the run has no Tax-Data paths')
  }
  if (any(is.na(tax_data_sources$years))) {
    missing_ids = tax_data_sources$ID[is.na(tax_data_sources$years)]
    stop('Cannot build the sample universe: no runscript years found for ',
         paste(unique(missing_ids), collapse = ', '))
  }

  tax_data_files = tax_data_sources %>%
    mutate(year = map(years, parse_year_spec)) %>%
    select(path, year) %>%
    unnest_longer(year) %>%
    distinct(path, year)

  map2(
    tax_data_files$path,
    tax_data_files$year,
    ~ fread(file.path(.x, paste0('tax_units_', .y, '.csv')),
            select = 'id', showProgress = FALSE)$id
  ) %>%
    unlist(use.names = FALSE) %>%
    unique()
}



parse_globals = function(runscript_name, scenario_id, local, vintage,
                         baseline_vintage, pct_sample, multicore) {

  #----------------------------------------------------------------------------
  # Parses the runtime arguments and the runscript, resolves every scenario's
  # economy and behavior legs, builds and checks the paths to the input data, and
  # writes the run manifest.
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

  # Set the random seed. It is returned so that behavior modules can re-seed before
  # drawing, and so that SLURM workers, which are new R processes that never run
  # this function, seed the same way.
  #
  # This block stays first. Nothing between here and the draws below may use the
  # generator, or every random number in the model shifts.
  random_seed = 76
  set.seed(random_seed)

  # Read each input interface's type and version. The vintage is scenario
  # configuration, but the version is pinned here, because it tracks which code the
  # interface is compatible with rather than anything about the world.
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

  # Read and check the runscript
  runscript = read_runscript(runscript_name, scenario_id)

  # Load each leg's default layer.
  economy_defaults  = config_load_defaults('economy')
  behavior_defaults = config_load_defaults('behavior')

  # Resolve every scenario's legs and input paths, and check for stale
  # calibrations. Done once here rather than in each worker.
  scenario_configs = resolve_all_scenarios(
    runscript         = runscript,
    economy_defaults  = economy_defaults,
    behavior_defaults = behavior_defaults,
    interface_meta    = interface_meta
  )

  # One row per scenario and interface, in runscript order. The baseline row comes
  # first, which interface_root() relies on.
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

  # Which tax units are in the sample. Take the union across every Tax-Data
  # path/year pair the run uses: vintages can contain different IDs, and projection
  # years can add new entrants within a vintage. Building the universe from 2017
  # alone once dropped 935 records holding $8.2T in wealth, which cut expected
  # estate tax by about a third.
  sample_ids = tax_data_sample_universe(interface_paths, runscript) %>%
    tibble(id = .) %>%
    sample_frac(size = pct_sample) %>%
    get_vector('id')

  # Draw the random numbers up front so they are the same across scenarios. Keyed
  # by ID and joined to tax units each year: the set of IDs varies by year, so
  # binding them by position would misalign the draws.
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
  # Resolves every scenario's economy and behavior legs, builds its input paths
  # from the resolved vintages, and runs the checks: stale calibrations, and the
  # shape of the behavior list.
  #
  # Both legs resolve here rather than when the scenario runs, so that a runscript
  # naming a folder that does not exist, a module that was deleted, or an
  # inconsistent gains binding fails in the first seconds rather than an hour in.
  # The resolved behavior list travels inside the scenario object, so every SLURM
  # driver already has it.
  #
  # Returns: list by scenario ID of the two resolved legs and the input paths.
  #----------------------------------------------------------------------------

  out = list()

  for (i in seq_len(nrow(runscript))) {
    id  = runscript$ID[i]
    row = runscript %>% slice(i) %>% as.list()

    economy = config_resolve('economy', economy_defaults,
                             alternative = row$economy)

    # The behavior leg holds no values, since its modules carry their own
    # parameters, so this only checks the folder. The module list itself comes from
    # behavior.yaml.
    behavior      = config_resolve('behavior', behavior_defaults,
                                   alternative = row$behavior)
    behavior$spec = behavior_resolve(row$behavior)
    behavior_validate_spec(behavior$spec, id)

    # Input paths, from the resolved vintages
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

    # The same check over every calibration file the scenario points at, run here
    # for the same reason: parse time is the one place both the sequential and the
    # SLURM paths pass through, so a stale calibration cannot reach a cluster run
    # unnoticed.
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
  # Writes the manifest at the root of the vintage, in six files: the input
  # versions and vintages, the three leg pointers per scenario, every resolved
  # value with its kind and whether the scenario overrode it, every calibration in
  # use and where it came from, the behavior list each scenario resolved to, and
  # the git commit the run was produced under.
  #
  # Returns: invisible NULL (writes files as a side effect)
  #----------------------------------------------------------------------------

  # The input versions and vintages, in runscript order
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

  # What each scenario is made of
  runscript %>%
    mutate(economy  = replace_na(as.character(economy), 'default'),
           behavior = as.character(behavior)) %>%
    select(ID, tax_law, economy, behavior, years, dist_years, mtr_vars, mtr_types) %>%
    write_csv(file.path(output_root, 'scenarios.csv'))

  # Every resolved value, from both legs
  names(scenario_configs) %>%
    map(.f = ~ bind_rows(
      config_manifest('economy',  economy_defaults,
                      scenario_configs[[.x]]$economy,  .x),
      config_manifest('behavior', behavior_defaults,
                      scenario_configs[[.x]]$behavior, .x))) %>%
    bind_rows() %>%
    write_csv(file.path(output_root, 'scenario_config.csv'))

  # Which calibration file supplied which value, and whether the scenario chose
  # that file or read it from a fixed path. Written so that an old vintage can be
  # read back without the code that produced it.
  names(scenario_configs) %>%
    map(.f = ~ calib_manifest(scenario_configs[[.x]]$behavior$spec, .x)) %>%
    bind_rows() %>%
    write_csv(file.path(output_root, 'calibrations.csv'))

  # What responses each scenario ran. The runscript names a folder, and the folder
  # name alone would not tell a later reader what was in it, so the resolved list
  # is written out too, in the order it ran.
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

  # The code the run was produced under, without which the configuration behind an
  # old vintage cannot be recovered.
  tibble(
    commit = system2('git', c('rev-parse', 'HEAD'), stdout = TRUE, stderr = FALSE),
    dirty  = length(system2('git', c('status', '--porcelain'), stdout = TRUE, stderr = FALSE)) > 0
  ) %>%
    write_csv(file.path(output_root, 'code_version.csv'))

  invisible(NULL)
}



ensure_scenario_dirs = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Creates a scenario's output folders. Called once per scenario;
  # get_scenario_info() is a lookup and does not touch the filesystem.
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
  # Assembles one scenario's runtime information from what was already resolved at
  # parse time. A lookup: it neither re-resolves anything nor touches the
  # filesystem.
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

  # The scenario's behavior modules, in the order they run and with the gains
  # module added. NULL rather than an empty vector where there is no response, which
  # is what run.R has always been given.
  behavior_modules = config$behavior$spec$modules
  if (length(behavior_modules) == 0) behavior_modules = NULL

  # Years to run. Distribution years default to all of them.
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
