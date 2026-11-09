#-------------------------------------------------------------------------------
# behavior.R
#
# Contains functions to load and run a scenario's behavioral responses
#-------------------------------------------------------------------------------
#
# A scenario's behavior cell names a FOLDER under config/scenarios/behavior/
# (the reserved word `default`, or a path under alternatives/), and that folder
# holds one file, behavior.yaml, with two sections:
#
#   kg_dynamics : `none`, or the pieces of the capital-gains bathtub machinery
#                 this scenario binds -- see behavior_read_yaml() for the two
#                 forms the value may take
#   modules     : a bare list of paths to module files under src/behavior/
#   waivers     : optional. Dated acceptances of a staleness finding on a value
#                 in one of the calibration files this scenario binds. They live
#                 HERE, in the pointing file, and not in the calibration file
#                 itself, for one reason: a calibration file is written by its
#                 calibrator, so a waiver in it would be erased by the next
#                 re-derivation -- which is right for a waiver that the
#                 re-derivation resolves, and wrong for one that says "this
#                 scenario knowingly runs against an older data vintage".
#
# There is no registry of modules and no list of known names. The loader takes any
# path that exists, sources it, and calls do_{family}, where the family is the
# file's parent folder. Adding a behavior means writing one file and listing it,
# and nothing is rejected for being unfamiliar.
#
# Each module's parameters live in its own file, with citations, because the module
# is the only thing that reads them. A variant is a separate file: charity/50.R and
# charity/100.R are the two elasticities, and that is the whole mechanism.
#
# Modules do not run in the order they are listed. Some read what an earlier one
# wrote, so the loader sorts them against the fixed order below. Families not in
# that list do not care about order and run last, as listed.
#-------------------------------------------------------------------------------


# The order responses run in. Each family here has a reason to come before the
# ones after it:
#   kg_dynamics     writes the stock of gains the next two read
#   conversion      moves ordinary income into that stock
#   entity_shifting moves pass-through income, priced off that stock
#   evasion         hides a share of reported income
#   wealth          hides a share of reported net worth, reading what evasion hid
#   charity         responds to the income tax rates the moves above settle on
#   estate          reads everything hidden above
# A family a scenario does not run drops out. What matters is the relative order,
# not the position.
BEHAVIOR_FAMILY_ORDER = c('kg_dynamics', 'conversion', 'entity_shifting',
                          'evasion', 'wealth', 'charity', 'estate')

# The module that turns the gains model's results into record-level realizations.
# Added automatically whenever the gains model runs, so a scenario never lists it:
# it is not an optional response but part of the machinery.
BEHAVIOR_KG_APPLIER = 'src/behavior/kg_dynamics/turnover.R'

# The pieces of the gains machinery a scenario can name a calibration file for.
# The recurrence itself is required whenever the gains model runs; conversion is
# the response built on top of it.
#
# Entity shifting is deliberately absent, even though it reads the priced gain when
# the gains model is running. Its parameters are published constants, so there is
# nothing to vary per scenario and nothing that can go stale, and it also runs in
# scenarios with no gains model to bind to. It reads them from a fixed path
# instead.
BEHAVIOR_KG_PIECES = c('bathtub', 'conversion')


do_behavioral_feedback = function(tax_units, behavior_modules, baseline_mtrs,
                                  static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Loads and runs a scenario's behavioral feedback modules, in the order the
  # loader already put them in.
  #
  # Parameters:
  #   - tax_units (df)           : tibble of tax unit data, pre tax calculation
  #   - behavior_modules (str[]) : ordered module paths (repo-relative, with
  #                                the .R extension) from the resolved behavior
  #                                spec -- see behavior_resolve()
  #   - baseline_mtrs (df)       : tibble of MTRs under the baseline, indexed
  #                                by year/tax unit id
  #   - static_mtrs (df)         : tibble of MTRs under the static
  #                                counterfactual scenario, indexed by year/tax
  #                                unit id
  #   - scenario_info (list)     : get_scenario_info() object
  #   - indexes (df)             : generate_indexes() object (see economy.R)
  # Returns: tibble of tax units with update values for specified columns (df).
  #----------------------------------------------------------------------------


  # Load modules for this scenario
  walk(.x    = behavior_modules,
       .f    = load_behavior_module,
       envir = environment())

  # Each module file defines do_{family}, where family is its parent folder
  fns = paste0('do_', behavior_family(behavior_modules))

  # Execute behavioral feedback functions sequentially
  for (fn in fns) {
    tax_units = do.call(
      what  = fn,
      args  = list(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes),
      envir = environment()
    )
  }

  return(tax_units)
}



behavior_family = function(paths) {

  #----------------------------------------------------------------------------
  # A module's family is its parent folder: src/behavior/charity/50.R is family
  # charity and defines do_charity(). That is the whole convention.
  #----------------------------------------------------------------------------

  basename(dirname(paths))
}



behavior_order = function(modules) {

  #----------------------------------------------------------------------------
  # Sorts module paths into execution order: families named in
  # BEHAVIOR_FAMILY_ORDER first, in that order; every other family after them,
  # in the order given. Stable, so two modules of the same family keep their
  # relative order.
  #----------------------------------------------------------------------------

  rank = match(behavior_family(modules), BEHAVIOR_FAMILY_ORDER)
  rank[is.na(rank)] = length(BEHAVIOR_FAMILY_ORDER) + 1L
  modules[order(rank, seq_along(modules))]
}



behavior_read_yaml = function(alternative) {

  #----------------------------------------------------------------------------
  # Reads one behavior.yaml. An alternative's sections replace the default's
  # outright rather than merging: half one list of responses laid over half another
  # is not something anyone means. A section the alternative leaves out is inherited.
  #
  # Parameters:
  #   - alternative (str) : reserved word `default`, or a path under
  #                         config/scenarios/behavior/alternatives/
  #
  # Returns: list(kg_dynamics, modules) as written, unvalidated
  #----------------------------------------------------------------------------

  path = file.path(config_leg_path('behavior', alternative), 'behavior.yaml')
  if (!file.exists(path)) {
    stop('The behavior leg alternative "', alternative, '" has no behavior.yaml',
         ' (expected ', path, ')')
  }
  read_yaml(path)
}



behavior_resolve = function(alternative = NULL) {

  #----------------------------------------------------------------------------
  # Resolves one scenario's responses: which gains machinery it binds and which
  # modules it runs, in the order they run and with the gains module added.
  #
  # Parameters:
  #   - alternative (str) : the runscript's behavior cell. NULL/NA/'' or
  #                         'default' means the default layer (no response at
  #                         all).
  #
  # Returns: list of
  #   - alternative  : identifier, for messages and the manifest
  #   - kg_dynamics  : 'none', or a named list piece -> stamp path ('' where
  #                    no stamp exists yet)
  #   - kg_pieces    : bound piece names; empty when the machinery is off
  #   - modules      : ordered module paths, applier injected
  #   - listed       : the module paths as written, before ordering (kept so
  #                    the migration can assert the sort changed nothing)
  #   - families     : families of `modules`, in execution order
  #   - waivers      : named list, '{calibration file stem}.{entry}' ->
  #                    {date, reason}
  #----------------------------------------------------------------------------

  alternative = if (is.null(alternative) || length(alternative) == 0 ||
                    is.na(alternative) || !nzchar(as.character(alternative)))
                  CONFIG_DEFAULT_NAME
                else as.character(alternative)

  spec = behavior_read_yaml(CONFIG_DEFAULT_NAME)
  if (!identical(alternative, CONFIG_DEFAULT_NAME)) {
    alt = behavior_read_yaml(alternative)
    for (section in names(alt)) spec[[section]] = alt[[section]]
  }

  listed = as.character(spec$modules %||% character())

  # The gains section can be written three ways, all the same downstream: none, a
  # list of the pieces bound, or a mapping of each piece to the calibration file
  # holding its value. The list form let the pieces be declared before those files
  # existed; the mapping form is where this is headed, and moving from one to the
  # other needs no change here.
  kg = spec$kg_dynamics
  if (is.null(kg) || identical(as.character(kg), 'none')) {
    kg_dynamics = 'none'
    kg_pieces   = character()
  } else if (is.null(names(kg))) {
    kg_pieces   = as.character(kg)
    kg_dynamics = set_names(as.list(rep('', length(kg_pieces))), kg_pieces)
  } else {
    kg_pieces   = names(kg)
    kg_dynamics = kg
  }

  # The gains module is part of the machinery rather than a response, so it goes in
  # whenever the machinery is on. A scenario that lists it by hand is caught below.
  modules = if (length(kg_pieces) > 0) c(BEHAVIOR_KG_APPLIER, listed) else listed
  modules = behavior_order(modules)

  waivers = spec$waivers %||% list()
  for (key in names(waivers)) {
    if (!all(c('date', 'reason') %in% names(waivers[[key]]))) {
      stop('Behavior alternative "', alternative, '" has a waiver on ', key,
           ' without both `date` and `reason`. An undated waiver is ',
           'indistinguishable from a finding nobody has looked at.')
    }
  }

  list(alternative = alternative,
       kg_dynamics = kg_dynamics,
       kg_pieces   = kg_pieces,
       modules     = modules,
       listed      = listed,
       families    = behavior_family(modules),
       waivers     = waivers)
}



behavior_validate_spec = function(spec, id = NULL) {

  #----------------------------------------------------------------------------
  # Checks one scenario's resolved responses before the run starts, on every
  # scenario in the runscript, rather than an hour in when a module happens to
  # execute.
  #
  # Everything here is about the shape of the list. Nothing is rejected for naming
  # an unfamiliar family: closing that interface is the mistake this design exists
  # to avoid.
  #
  # Returns: invisibly TRUE; stops on a violation, and warns where evasion runs
  #          without the estate response.
  #----------------------------------------------------------------------------

  where    = if (is.null(id)) '' else paste0(' (scenario "', id, '")')
  problems = c()

  # A module that does not exist would otherwise fail an hour into the run
  for (m in spec$modules) {
    if (!file.exists(m)) {
      problems = c(problems, paste0('module file does not exist: ', m))
    }
  }
  if (anyDuplicated(spec$modules) > 0) {
    problems = c(problems, paste0(
      'the same module is listed twice: ',
      paste(unique(spec$modules[duplicated(spec$modules)]), collapse = ', ')))
  }

  # The gains module is added by the machinery, never listed
  if (BEHAVIOR_KG_APPLIER %in% spec$listed) {
    problems = c(problems, paste0(
      BEHAVIOR_KG_APPLIER, ' must not be listed under modules: it is the kg ',
      'machinery itself, and setting kg_dynamics brings it in automatically'))
  }

  bad_pieces = setdiff(spec$kg_pieces, BEHAVIOR_KG_PIECES)
  if (length(bad_pieces) > 0) {
    problems = c(problems, paste0(
      'unknown kg_dynamics piece(s): ', paste(bad_pieces, collapse = ', '),
      ' -- the pieces are ', paste(BEHAVIOR_KG_PIECES, collapse = ', ')))
  }

  # A bound piece must name a file that exists. Otherwise the run gets as far as
  # the first read of that value.
  if (length(spec$kg_pieces) > 0) {
    for (piece in spec$kg_pieces) {
      path = spec$kg_dynamics[[piece]]
      if (is.null(path) || !nzchar(as.character(path))) {
        problems = c(problems, paste0(
          'kg_dynamics binds `', piece, '` without naming a calibration file ',
          '-- write it as `', piece, ': <path>`'))
      } else if (!file.exists(as.character(path))) {
        problems = c(problems, paste0(
          'kg_dynamics binds `', piece, '` to a calibration file that does not ',
          'exist: ', path))
      }
    }
  }

  kg_on = length(spec$kg_pieces) > 0
  if (kg_on && !('bathtub' %in% spec$kg_pieces)) {
    problems = c(problems, paste0(
      'kg_dynamics is active but does not bind `bathtub`, which is the state ',
      'recurrence the other pieces are built on'))
  }

  # Each optional piece and the module that uses it must agree both ways: a bound
  # piece nobody runs is dead configuration, and a module whose piece is unbound
  # would read a value from nowhere.
  for (piece in c('conversion')) {
    has_module = piece %in% spec$families
    has_piece  = piece %in% spec$kg_pieces
    if (kg_on && has_module && !has_piece) {
      problems = c(problems, paste0(
        'a ', piece, '/ module is listed but kg_dynamics does not bind the `',
        piece, '` piece'))
    }
    if (has_piece && !has_module) {
      problems = c(problems, paste0(
        'kg_dynamics binds the `', piece, '` piece but no ', piece,
        '/ module is listed to use it'))
    }
  }

  # Conversion is built on the recurrence: the income it moves lands in the stock of
  # gains, and it is priced off the same machinery.
  if (!kg_on && 'conversion' %in% spec$families) {
    problems = c(problems, paste0(
      'a conversion/ module is listed but kg_dynamics is none -- the converted ',
      'dollars live in the kg bathtub gain state and the equity-leg wedge is ',
      'computed by its machinery, so conversion cannot run without it'))
  }

  # Concealed wealth has to reach the reported estate, and the estate module is what
  # carries it there. Wealth without estate would leave concealed wealth visible to
  # the estate tax, so this stops the run rather than warning.
  if ('wealth' %in% spec$families && !('estate' %in% spec$families)) {
    problems = c(problems, paste0(
      'a wealth/ module is listed with no estate/ module -- the wealth ',
      'concealment fractions it persists are consumed by the estate module, ',
      'and without it concealed net worth stays visible to the estate tax'))
  }

  if (length(problems) > 0) {
    stop(paste0('Invalid behavior spec "', spec$alternative, '"', where, ':\n  - ',
                paste(problems, collapse = '\n  - '), '\n',
                'Modules run in this order: ',
                paste(spec$modules, collapse = ' -> '), '\n'))
  }

  # A warning rather than an error: a calibration run on the income side alone may
  # legitimately leave the estate response out. A scored run may not, since evaded
  # income would stay visible to the estate tax.
  if ('evasion' %in% spec$families && !('estate' %in% spec$families)) {
    warning('Behavior spec "', spec$alternative, '"', where, ' runs an evasion/ ',
            'module with no estate/ module: evaded income will NOT be removed ',
            'from the reported estate base. Correct for an income-side-only ',
            'calibration run, wrong for anything else.', call. = FALSE,
            immediate. = TRUE)
  }

  invisible(TRUE)
}



apply_mtr_elasticity = function(tax_units, var, baseline_mtrs, static_mtrs, max_adj) {
  
  #----------------------------------------------------------------------------
  # Adjusts a category of variable based on their elasticity with respect to 
  # the marginal tax rate (MTR).
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units containing the following columns
  #      - e_{var} (dbl)      : the elasticity value
  #      - e_{var}_type (str) : the type of elasticity, must be one of
  #                             ['semi', 'arc', 'netoftax', 'taxprice']
  #   - var (str)          : name (i.e. alias) of the variable we're adjusting
  #   - baseline_mtrs (df) : tibble of MTRs under the baseline, including the 
  #                          column mtr_{var}
  #   - static_mtrs (df)   : tibble of MTRs under the static counterfactual 
  #                          scenario, including the column mtr_{var}
  #   - max_adj (dbl)      : absolute value of maximum adjustment as measured  
  #                          by percent change. For example, a value of 1 means 
  #                          any adjustment greater than 100% or less than 
  #                          -100% will be limited to that max value. Helps 
  #                          catch implausible responses stemming from edge 
  #                          cases in MTR changes.
  #
  # Returns: tibble with one column for the post-adjustment variable
  #----------------------------------------------------------------------------
  
  
  tax_units %>%
  
    # Join MTRs
    left_join(baseline_mtrs %>% 
                 rename_with(.cols = -c(id, year), 
                             .fn   = ~ paste0(., '_baseline')), 
               by = c('id', 'year')) %>%
    left_join(static_mtrs, by = c('id', 'year')) %>% 
      
    # Rename variables for legibility and ease of use
    rename(
      e            = !!sym(paste0("e_", var)),
      e_type       = !!sym(paste0("e_", var, "_type")),
      mtr          = !!sym(paste0("mtr_", var)),
      mtr_baseline = !!sym(paste0("mtr_", var, "_baseline"))
    ) %>%
    
    mutate(
      
      # Calculate adjustment factor based on type
      pct_chg = case_when(
        e_type == "semi"     ~ exp((mtr - mtr_baseline) * e) - 1,
        e_type == "arc"      ~ (e * (mtr / ((mtr + mtr_baseline) / 2) - 1)),
        e_type == "netoftax" ~ (e * ((1 - mtr) / (1 - mtr_baseline) - 1)),
        e_type == "taxprice" ~ (e * ((1 + mtr) / (1 + mtr_baseline) - 1)),
        TRUE                 ~ NA 
      ),
      
      # Limit adjustment to maximum allowed
      pct_chg = pmax(-max_adj, pmin(pct_chg, max_adj)),
    
      # Apply elasticity factor to columns of concern
      across(.cols = all_of(var),
             .fns  = ~ . * (1 + pct_chg))
    ) %>%
    
    # Select post-adjustment variable and return
    select(all_of(var)) %>%
    return()
}



load_behavior_module = function(path, envir) {

  #----------------------------------------------------------------------------
  # Runs one module file, defining its do_{family} function in the given
  # environment. Modules are loaded by path when the scenario runs, never sourced at
  # startup. That is what lets two files in one family both define the same function
  # while only the one this scenario names is in scope, and it is why main.R and the
  # two SLURM setup files skip src/behavior/ when they source src/ recursively.
  #
  # Parameters:
  #   - path (str)  : repo-relative module path, e.g.
  #                   "src/behavior/charity/50.R" (defines do_charity())
  #   - envir (env) : environment in which to execute the module code
  #
  # Returns: void.
  #----------------------------------------------------------------------------

  sys.source(path, envir)
}


