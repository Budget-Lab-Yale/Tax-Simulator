#-------------------------------------------------------------------------------
# calibrations.R
#
# Reading the calibration files under config/calibrations/, and checking at parse
# time that the values in them still match the world the run is about to happen
# in.
#
# WHY THIS EXISTS. Before the rebuild, not one of the five calibrated values in
# this model was written by the script that calibrated it -- every one had been
# hand-copied out of a log -- and four of the five "how to re-derive this"
# pointers were broken. A number and the run that produced it could drift apart
# silently, and did. A calibration file is the fix: the calibrator writes it, and
# it carries enough about its own derivation that the model can refuse to use it
# when that derivation no longer describes the run.
#
# TWO WAYS A FILE IS REACHED, and the difference is not cosmetic:
#
#   BOUND    the scenario's behavior leg names the file, through the kg_dynamics
#            section. Values that a scenario may legitimately want to differ on
#            live here, because binding is what puts the choice in the run
#            manifest. This is how an eta sweep works: a generated file with the
#            same shape, bound by its own behavior alternative, so the sweep is
#            recorded in the vintage instead of vanishing with a shell variable.
#            -> bathtub.yaml, conversion.yaml
#
#            A generated sweep file must keep the same BASE NAME as the file it
#            stands in for -- config/calibrations/kg/sweeps/eta_15/bathtub.yaml,
#            not .../eta_15.yaml. Entries are labelled '{file stem}.{entry}', so
#            renaming the file renames every label, and a waiver written against
#            `bathtub.eta_logs` would quietly stop applying.
#
#   FIXED    one path, the same for every scenario. For the model-form switches
#            and judgment calls that the calibrations are conditioned on, and so
#            cannot vary underneath them.
#            -> settings.yaml
#
# WHAT IS NOT HERE. A behavioral module's own parameters. Every number a pluggable
# behavior module reads lives in that module's file, with its citation, and a
# variant is a copy of the file -- no exceptions. Entity shifting briefly had its
# published constants in a file here and it was wrong twice over: it split one rule
# into a rule plus a carve-out, and the module also runs in scenarios with no
# bathtub to bind to. What belongs here is the machinery's own calibration, read by
# src/sim/kg/, not a module's assumptions.
#
# Both accessors are FAIL-CLOSED. Reading a bound value in a scenario that never
# bound the file is an error, not a default, because a default here would be a
# number nobody chose.
#-------------------------------------------------------------------------------


CALIB_ROOT    = './config/calibrations'
CALIB_KG_ROOT = file.path(CALIB_ROOT, 'kg')

# Which kg_dynamics pieces are BOUND (see the header). `entity_shifting` is a
# piece a scenario declares -- the module prices its retained-earnings leg off the
# bathtub when one is running -- but its values are published constants read from
# a fixed path, because there is nothing about them to vary or to go stale.
CALIB_BOUND_PIECES = c('bathtub', 'conversion')

# Parsed calibration files, keyed by path. Calibration files do not change during
# a run, and a forked worker gets its own copy of this environment.
.calib_cache = new.env(parent = emptyenv())



calib_load = function(path) {

  #----------------------------------------------------------------------------
  # Reads and caches one calibration file. Returns its entries with the reserved
  # `_channel` block peeled off.
  #----------------------------------------------------------------------------

  key = normalizePath(path, mustWork = FALSE)
  if (exists(key, envir = .calib_cache, inherits = FALSE)) {
    return(get(key, envir = .calib_cache, inherits = FALSE))
  }

  if (!file.exists(path)) {
    stop('No calibration file at ', path,
         '. A behavior alternative points at it, or a fixed-path reader ',
         'expects it; either way the run cannot proceed without the value.')
  }

  raw = read_yaml(path)
  raw[['_channel']] = NULL

  # Same schema as any configuration entry: a known kind, and the fields that
  # kind owes. A calibration file with an undocumented number in it should die
  # at load, which is the whole point of the exercise.
  config_validate(paste0('calibration (', basename(path), ')'),
                  set_names(list(raw), tools::file_path_sans_ext(basename(path))))

  assign(key, raw, envir = .calib_cache)
  raw
}



calib_entry = function(entries, name, where) {

  #----------------------------------------------------------------------------
  # One entry out of a loaded calibration file, or a stop naming what is there.
  #----------------------------------------------------------------------------

  if (!(name %in% names(entries))) {
    stop('Calibration file ', where, ' has no entry `', name, '`. It has: ',
         paste(names(entries), collapse = ', '), '.')
  }
  entries[[name]]$value
}



calib_bound_path = function(piece) {

  #----------------------------------------------------------------------------
  # The file the active scenario bound to one kg_dynamics piece. Fail-closed:
  # a scenario that did not bind the piece has no business reading its values,
  # and a default would be a number nobody chose.
  #----------------------------------------------------------------------------

  spec = .calib_active_spec()

  if (identical(spec$kg_dynamics, 'none') || length(spec$kg_pieces) == 0) {
    stop('A `', piece, '` calibration value was read, but this scenario does ',
         'not bind the capital-gains machinery at all (kg_dynamics: none in ',
         'behavior alternative "', spec$alternative, '"). Either the scenario ',
         'should bind it, or this code path should not be running.')
  }
  if (!(piece %in% spec$kg_pieces)) {
    stop('A `', piece, '` calibration value was read, but behavior alternative "',
         spec$alternative, '" binds only: ',
         paste(spec$kg_pieces, collapse = ', '), '.')
  }

  path = spec$kg_dynamics[[piece]]
  if (is.null(path) || !nzchar(as.character(path))) {
    stop('Behavior alternative "', spec$alternative, '" binds the `', piece,
         '` piece but gives no calibration file for it. Write the piece as ',
         '`', piece, ': <path to its calibration file>` rather than as a bare ',
         'name.')
  }
  as.character(path)
}



.calib_active_spec = function() {

  #----------------------------------------------------------------------------
  # The active scenario's resolved behavior spec, or a stop. Reaches into the
  # same run-state the economy accessor uses, so the failure mode of reading a
  # value before activating a scenario is identical for both.
  #----------------------------------------------------------------------------

  if (!exists('behavior', envir = .scenario_config_active, inherits = FALSE) ||
      is.null(get('behavior', envir = .scenario_config_active, inherits = FALSE))) {
    stop('A calibration value was read before any scenario was activated. ',
         'Call config_activate() first (do_scenario and the SLURM workers do ',
         'this).')
  }
  spec = get('behavior', envir = .scenario_config_active, inherits = FALSE)$spec
  if (is.null(spec)) {
    stop('The active behavior leg carries no resolved spec, so no calibration ',
         'binding can be looked up.')
  }
  spec
}



# --- The accessors -------------------------------------------------------------
# Named for the file rather than for what the value means, so a reader can always
# find the provenance: kg_bathtub('eta_logs') is in bathtub.yaml under eta_logs.

kg_bathtub = function(name) {
  path = calib_bound_path('bathtub')
  calib_entry(calib_load(path), name, path)
}

kg_conversion = function(name) {
  path = calib_bound_path('conversion')
  calib_entry(calib_load(path), name, path)
}

kg_setting = function(name) {
  path = file.path(CALIB_KG_ROOT, 'settings.yaml')
  calib_entry(calib_load(path), name, path)
}



calib_settings_values = function() {

  #----------------------------------------------------------------------------
  # Every value in settings.yaml, as channel -> name -> value. Shaped for the
  # staleness check, which compares a calibration's recorded conditions against
  # what the run actually has, and for `active_when` lookups that name
  # `kg.response_form`.
  #----------------------------------------------------------------------------

  entries = calib_load(file.path(CALIB_KG_ROOT, 'settings.yaml'))
  list(kg = map(entries, 'value'))
}



calib_check_staleness = function(behavior_spec, interface_vintages,
                                 enforce = CONFIG_ENFORCE_STALENESS) {

  #----------------------------------------------------------------------------
  # Runs the staleness check over every calibration file the resolved scenario
  # points at. Called at parse time, once per scenario, alongside the economy
  # leg's check -- which is what covers the SLURM path too, since Phase 0 of the
  # cluster pipeline calls the same parser.
  #
  # Three things can make a calibrated value stale, and all three are checked by
  # the shared implementation in scenario_config.R:
  #   - the upstream data vintages it was derived under are not the ones this run
  #     uses
  #   - a file it declares itself invalidated by has changed content since the
  #     value was pinned
  #   - a configuration value it was conditioned on has moved
  # The third arm is what makes the model-form switches in settings.yaml
  # load-bearing: a calibration records the settings it was derived under, and
  # changing one of those settings stops every capital-gains run until the value
  # is re-derived or the entry carries a dated waiver.
  #
  # Parameters:
  #   - behavior_spec (list)      : behavior_resolve() output for this scenario
  #   - interface_vintages (list) : interface key -> vintage in use
  #   - enforce (bool)            : TRUE stops on staleness, FALSE warns
  #
  # Returns: character vector of findings (empty if clean)
  #----------------------------------------------------------------------------

  # The fixed-path file is checked on every scenario; bound files only when this
  # scenario actually binds them, since an unbound file is not in use.
  paths = file.path(CALIB_KG_ROOT, 'settings.yaml')
  for (piece in intersect(CALIB_BOUND_PIECES, behavior_spec$kg_pieces)) {
    p = behavior_spec$kg_dynamics[[piece]]
    if (!is.null(p) && nzchar(as.character(p))) paths = c(paths, as.character(p))
  }

  settings = calib_settings_values()
  findings = c()

  for (path in unique(paths)) {
    entries = calib_load(path)

    # The shared checker speaks in terms of a leg's `defaults` and a `resolved`
    # object. A calibration file is the degenerate case of both: one channel,
    # named for the file, with nothing overridden. Presenting it that way means
    # there is one staleness implementation in this codebase rather than two
    # that drift.
    channel  = tools::file_path_sans_ext(basename(path))
    defaults = list(entries = set_names(list(entries), channel), roles = NULL)
    resolved = list(
      leg         = 'calibration',
      alternative = path,
      values      = c(set_names(list(map(entries, 'value')), channel),
                      settings),
      roles       = NULL,
      overrides   = tibble(channel = character(), name = character(),
                           default = character(), value = character(),
                           source  = character()),
      # Two sources of waiver, merged: any the calibration file carries on its
      # own entries, plus any the POINTING behavior alternative declares for this
      # file. The second is the durable kind -- a re-derivation rewrites the
      # calibration file and clears its waivers, which is correct, but must not
      # silently clear a scenario's deliberate acceptance of an older vintage.
      waivers     = modifyList(calib_waivers(entries, channel),
                               calib_pointed_waivers(behavior_spec, channel)))

    findings = c(findings, config_check_staleness(
      leg                = paste0('calibration (', basename(path), ')'),
      defaults           = defaults,
      resolved           = resolved,
      interface_vintages = interface_vintages,
      cross_values       = list(economy = NULL, behavior = NULL,
                                settings = settings),
      enforce            = enforce))
  }

  findings
}



calib_waivers = function(entries, channel) {

  #----------------------------------------------------------------------------
  # The dated waivers a calibration file carries on its own entries.
  #
  # A waiver in a GENERATED file looks wrong at first glance -- the calibrator
  # rewrites the file, so it would rewrite the waiver away. That is the intended
  # behaviour: re-deriving a value is exactly the thing that should clear its
  # waiver. A waiver that must survive re-derivation belongs on the entry in the
  # file that POINTS here, which is a human-owned file.
  #
  # Returns: named list keyed '{channel}.{name}', matching the label the
  #          staleness checker builds
  #----------------------------------------------------------------------------

  out = list()
  for (nm in names(entries)) {
    w = entries[[nm]]$waiver
    if (is.null(w)) next
    if (!all(c('date', 'reason') %in% names(w))) {
      stop('Calibration entry `', nm, '` has a waiver without both `date` and ',
           '`reason`. An undated waiver is indistinguishable from a value ',
           'nobody has looked at.')
    }
    out[[paste(channel, nm, sep = '.')]] = w
  }
  out
}



calib_pointed_waivers = function(behavior_spec, channel) {

  #----------------------------------------------------------------------------
  # The waivers a scenario's behavior alternative declares against one
  # calibration file. Keys in behavior.yaml are '{file stem}.{entry}' -- e.g.
  # `bathtub.eta_logs` -- which is deliberately the same label the staleness
  # checker builds, so they are passed through unchanged.
  #----------------------------------------------------------------------------

  declared = behavior_spec$waivers %||% list()
  if (length(declared) == 0) return(list())

  prefix = paste0(channel, '.')
  declared[names(declared)[startsWith(names(declared), prefix)]]
}



calib_manifest = function(behavior_spec, id) {

  #----------------------------------------------------------------------------
  # Manifest rows for the calibration values a scenario used: which file, which
  # entry, the value, its kind, and whether the file was bound by the scenario
  # or read from a fixed path. Written into the vintage so a past run can be
  # read back without the code.
  #----------------------------------------------------------------------------

  rows = list()

  add = function(path, binding) {
    entries = calib_load(path)
    nms     = names(entries)
    if (length(nms) == 0) return(NULL)
    tibble(
      ID      = id,
      file    = path,
      binding = binding,
      name    = nms,
      value   = vapply(nms,
                       function(n) paste(as.character(entries[[n]]$value),
                                         collapse = ' '),
                       character(1), USE.NAMES = FALSE),
      kind    = vapply(nms,
                       function(n) as.character(entries[[n]]$kind %||% NA),
                       character(1), USE.NAMES = FALSE),
      waived  = vapply(nms,
                       function(n) !is.null(entries[[n]]$waiver),
                       logical(1), USE.NAMES = FALSE))
  }

  rows[['settings']] = add(file.path(CALIB_KG_ROOT, 'settings.yaml'), 'fixed')
  for (piece in intersect(CALIB_BOUND_PIECES, behavior_spec$kg_pieces)) {
    p = behavior_spec$kg_dynamics[[piece]]
    if (!is.null(p) && nzchar(as.character(p))) {
      rows[[piece]] = add(as.character(p), paste0('bound:', piece))
    }
  }

  bind_rows(rows)
}
