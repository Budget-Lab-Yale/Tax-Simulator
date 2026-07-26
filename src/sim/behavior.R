#-------------------------------------------------------------------------------
# behavior.R
#
# The behavior leg: which responses a scenario runs, and in what order.
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
# Modules stay PLUGGABLE. There is no registry and no list of known names: the
# loader takes any path that exists, sources it, and calls do_{family}, where
# family is the file's parent folder name. Adding a behavior is writing one
# file and listing it. Nothing is ever rejected for being unfamiliar.
#
# Their parameters live inside the module files, with citations, because the
# module is their only reader. Variants are separate files -- charity/50.R and
# charity/100.R are the -0.5 and -1.0 elasticities, and that is the whole
# mechanism.
#
# Execution ORDER is not the order they are listed in. Some responses read what
# an earlier one wrote (the hidden-ledger chain: evasion, then wealth, then
# estate; conversion needs the bathtub gain state), so the loader sorts the
# list against one pinned family order, declared once below. Families outside
# that order are order-insensitive and run last, in the order listed. This
# single sort replaces five hand-written order guards that used to live in the
# module files and each stopped the run on a different subset of the same rule.
#-------------------------------------------------------------------------------


# The pinned execution order. Every family here has a reason to precede the
# ones after it:
#   kg_dynamics     -- writes the gain state the next two read
#   conversion      -- moves ordinary dollars into the gain pool
#   entity_shifting -- moves pass-through dollars, priced off the gain state
#   evasion         -- hides a share of reported income
#   wealth          -- hides a share of reported net worth, reading evasion's
#                      per-record factors
#   charity         -- responds to the income-side rates the moves above settle
#   estate          -- terminal consumer of the concealment factors above
# A family absent from a scenario simply drops out; relative order is what
# matters, not position.
BEHAVIOR_FAMILY_ORDER = c('kg_dynamics', 'conversion', 'entity_shifting',
                          'evasion', 'wealth', 'charity', 'estate')

# The kg bathtub applier. Injected automatically whenever kg_dynamics is
# active, so a scenario never lists it: it is not an optional response but the
# step that translates bathtub state into per-record realizations.
BEHAVIOR_KG_APPLIER = 'src/behavior/kg_dynamics/turnover.R'

# The pieces of the kg machinery a scenario may BIND -- that is, name a
# calibration file for. `bathtub` is the state recurrence itself and is required
# whenever kg_dynamics is active; `conversion` is the response built on top of it.
#
# Entity shifting is deliberately NOT here, even though it does read the bathtub's
# tau_eq when one is running. Its parameters are published constants, so there is
# nothing about them to vary per scenario and nothing that can go stale, and the
# module also runs in entity-only scenarios where no bathtub exists to bind to.
# It reads them from a fixed path instead (src/misc/calibrations.R).
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
  # A module's family is its parent folder name: src/behavior/charity/50.R is
  # family `charity` and defines do_charity(). This is the whole of the
  # convention binding a file to a hook.
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
  # Reads one behavior.yaml. The alternative's sections REPLACE the default's
  # wholesale -- a behavior spec is a stack, and half a stack overlaid on
  # another half is not a thing anyone means. A section the alternative omits
  # is inherited from the default layer.
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
  # Resolves one scenario's behavior spec: which kg machinery it binds and
  # which module files it runs, in execution order with the kg applier
  # injected.
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

  # kg_dynamics takes one of three written forms, all meaning the same thing to
  # everything downstream:
  #   none                              -- the machinery is off
  #   [bathtub, conversion]             -- these pieces are bound, and their
  #                                        parameters are still wherever they
  #                                        live today (no stamp file yet)
  #   {bathtub: path/to/stamp.yaml}     -- these pieces are bound, each to the
  #                                        calibration file that carries its
  #                                        value and provenance
  # The list form exists so the pieces could be declared before the stamped
  # files existed; the mapping form is where this is going, and adding paths to
  # a list form needs no code change here.
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

  # The applier is the machinery, not a response: it goes in whenever the
  # machinery is on, and a scenario that lists it by hand is caught in
  # behavior_validate_spec().
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
  # Parse-time checks on one resolved behavior spec. These replace the five
  # order guards that used to sit inside the module files: the difference is
  # that these run before the run starts, on every scenario in the runscript,
  # rather than an hour in when a module happens to execute.
  #
  # Everything here is about the SHAPE of the stack. Nothing is rejected for
  # naming an unfamiliar family -- that would close the pluggable interface,
  # which is the mistake this design exists to avoid.
  #
  # Returns: TRUE invisibly; stops on any violation, warns on the evasion
  #          consistency contract
  #----------------------------------------------------------------------------

  where    = if (is.null(id)) '' else paste0(' (scenario "', id, '")')
  problems = c()

  # A path that does not exist would otherwise fail an hour into the run
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

  # The applier comes from kg_dynamics being on, never from the module list
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

  # A bound piece must name a file, and the file must exist. Without this the
  # run gets as far as the first read of that value before failing.
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

  # Each optional piece must agree with the module that uses it, in both
  # directions: a bound piece nobody runs is dead configuration, and a module
  # whose piece is unbound would read a value from nowhere.
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

  # Conversion is built ON the bathtub: the dollars it moves land in the gain
  # state, and its price wedge comes out of the same machinery.
  if (!kg_on && 'conversion' %in% spec$families) {
    problems = c(problems, paste0(
      'a conversion/ module is listed but kg_dynamics is none -- the converted ',
      'dollars live in the kg bathtub gain state and the equity-leg wedge is ',
      'computed by its machinery, so conversion cannot run without it'))
  }

  # Wealth concealment has to reach the reported estate, and that propagation
  # lives in the estate module (which also owns estate_concealed_frac). Wealth
  # without estate would silently leave concealed wealth visible to the estate
  # tax, so this one is fatal rather than a warning.
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

  # Not fatal: an income-side-only calibration run legitimately omits the
  # estate leg. A product run must not -- evaded income that stays visible to
  # the estate tax was the activation bug of 2026-07-16.
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
  # Executes one behavior module file, defining its do_{family} function in a
  # given environment. Modules are loaded here, by path, at scenario time --
  # never sourced at startup, which is what lets two files in one family both
  # define do_{family} while only the one this scenario names is in scope.
  # main.R, src/slurm/setup.R and src/slurm/common.R skip src/behavior/ when
  # they source src/ recursively, for that reason.
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


