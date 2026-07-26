#-------------------------------------------------------------------------------
# scenario_config.R
#
# The scenario-configuration resolution engine for the three-leg architecture:
# a scenario is (tax_law, economy, behavior). Tax law keeps its own machinery
# (src/data/tax_law.R); this file serves the other two legs with ONE shared
# implementation, generalized from src/misc/assumptions.R:
#
#   - economy  : exogenous world description + mechanical transmission.
#                Channels under config/scenarios/economy/. Each channel carries
#                a role -- 'state' (read on both run types) or 'transmission'
#                (conventional-only; reading one on the static pass is an error,
#                which is what makes "static = law-only" a checked definition).
#   - behavior : agent responses to tax changes, relative to baseline. Its
#                module list has its own loader (src/sim/behavior.R); this
#                engine serves the behavior leg's folder shape only.
#
# Every leg has the same folder shape, the one tax law already uses:
#
#   config/scenarios/{leg}/default/          -- a complete specification
#   config/scenarios/{leg}/alternatives/...  -- sparse deltas over it
#
# A runscript cell is either the reserved word `default` or a path under that
# leg's alternatives/ (nesting is arbitrary and human-named). Precedence within
# a scenario is exactly two layers:
#
#   the alternative's files > the default files
#
# A runscript names FILES, never values: there is no per-value override
# column. A CSV cell carries no provenance; an entry in an alternative folder
# does. Sweeps and A/B corners are generated alternative folders, written by
# the same generator that writes the runscript rows.
#
# Values are SCENARIO-scoped: read them at the point of use via
# economy_param() / behavior_param(), which resolve against the scenario
# installed by config_activate(). Never capture one at source time.
#
# Provenance schema is inherited from the assumptions layer unchanged (kinds
# calibrated / sourced / judgment / structural, staleness on derived_under +
# invalidated_by hashes, dated `acknowledged` waivers, `active_when` guards),
# with three additions:
#   - role        : 'state' | 'transmission', economy leg only; declared per
#                   channel via a reserved `_channel:` block, overridable per
#                   entry
#   - locked      : entry can never be overridden by an alternative (the estate
#                   valuation bridge)
#   - enforcement : 'stop' (default) | 'warn', how hard a staleness finding
#                   for this entry lands. 'warn' exists for the estate
#                   measurement bridge, which the model deliberately runs
#                   against older Tax-Data vintages (historical comparisons);
#                   src/sim/estate.R has warned at the point of use since
#                   before the redesign, and a locked entry has no override
#                   escape hatch
#   - pointer values : a calibrated entry's value may be a folder/file pointer
#                   (with optional pointer_root); its staleness rides the
#                   invalidated_by file hashes like any other, which is how a
#                   TABLE-valued calibration (the wealth s-profile) joins the
#                   staleness net
#   - conditioned_on : map of {leg}.{channel}.{name}: value on a calibrated
#                   entry; checked at staleness time against the live resolved
#                   values (a calibration conditioned on a config magnitude --
#                   e.g. sigma on charity.e = -1 -- can no longer be caught by
#                   file hashes once magnitudes are config)
#-------------------------------------------------------------------------------


# Leg roots. tax_law is deliberately absent: it has different (subparameter-
# replacement) semantics and its own parser.
CONFIG_LEG_ROOTS = list(
  economy  = './config/scenarios/economy',
  behavior = './config/scenarios/behavior'
)

# The reserved cell value naming the default layer. A folder of this name under
# alternatives/ would be unreachable, so it is an error rather than a silent
# shadow.
CONFIG_DEFAULT_NAME = 'default'


config_leg_path = function(leg, name) {

  #----------------------------------------------------------------------------
  # Folder holding one layer of one leg: the reserved word `default` maps to
  # {root}/default, anything else to {root}/alternatives/{name}. The one place
  # the two-folder shape is written down.
  #----------------------------------------------------------------------------

  root = CONFIG_LEG_ROOTS[[leg]]
  if (identical(name, CONFIG_DEFAULT_NAME)) file.path(root, CONFIG_DEFAULT_NAME)
  else file.path(root, 'alternatives', name)
}

# Whether a stale calibration halts the run (see assumptions.R for the history
# of this switch; it carried over TRUE).
CONFIG_ENFORCE_STALENESS = TRUE

# Required fields by kind -- identical to ASSUMPTION_SCHEMA.
CONFIG_ENTRY_SCHEMA = list(
  calibrated = c('value', 'kind', 'set', 'target', 'derived_under',
                 'invalidated_by', 'rederive'),
  sourced    = c('value', 'kind', 'citation'),
  judgment   = c('value', 'kind', 'note'),
  structural = c('value', 'kind', 'note')
)

CONFIG_ROLES = c('state', 'transmission')

# How hard a staleness finding lands, per entry (see the header note).
CONFIG_ENFORCEMENTS = c('stop', 'warn')

# Run-scoped store of the active scenario's resolved legs (and the current
# pass). An environment rather than a global binding so a forked worker
# (multicore = 'scenario') mutates only its own copy.
.scenario_config_active = new.env(parent = emptyenv())

# Run-scoped state that is not configuration (e.g. estate measurement params
# loaded once per run). Replaces ad-hoc `globals$x <<-` mutation.
.run_state = new.env(parent = emptyenv())



run_state_set = function(name, value) {

  #----------------------------------------------------------------------------
  # Stores a run-scoped object (fork-safe). See .run_state above.
  #----------------------------------------------------------------------------

  assign(name, value, envir = .run_state)
  invisible(NULL)
}



run_state_get = function(name) {

  #----------------------------------------------------------------------------
  # Reads a run-scoped object stored by run_state_set(); fail-closed.
  #----------------------------------------------------------------------------

  if (!exists(name, envir = .run_state, inherits = FALSE)) {
    stop("run_state_get('", name, "') read before run_state_set() stored it")
  }
  get(name, envir = .run_state, inherits = FALSE)
}



config_load_defaults = function(leg) {

  #----------------------------------------------------------------------------
  # Reads the default layer of a leg: every channel YAML under
  # {leg root}/default/, validated.
  #
  # Parameters:
  #   - leg (str) : 'economy' or 'behavior'
  #
  # Returns: list of
  #   - entries : channel -> name -> entry list (entry-level `role` filled in
  #               from the channel `_channel` block)
  #   - roles   : channel -> channel-default role (economy leg; NULL otherwise)
  #----------------------------------------------------------------------------

  leg  = match.arg(leg, names(CONFIG_LEG_ROOTS))
  root = config_leg_path(leg, CONFIG_DEFAULT_NAME)
  if (!dir.exists(root)) {
    stop('No default layer found for the ', leg, ' leg (expected ', root, ')')
  }

  # `default` is the reserved cell value, so a folder of that name under
  # alternatives/ could never be selected. Catch it here rather than let it sit
  # there looking usable.
  shadow = file.path(CONFIG_LEG_ROOTS[[leg]], 'alternatives', CONFIG_DEFAULT_NAME)
  if (dir.exists(shadow)) {
    stop('`default` is a reserved runscript cell value naming the ', leg,
         ' leg default layer, so ', shadow, ' can never be selected. Rename it.')
  }

  files = list.files(root, pattern = '[.]yaml$', full.names = TRUE)
  files = files[basename(files) != 'behavior.yaml']
  if (length(files) == 0 && leg != 'behavior') {
    stop('No channel files found under ', root)
  }
  # The behavior leg carries no value entries at all: its content is
  # behavior.yaml (the kg binding plus the module list), which
  # src/sim/behavior.R reads, and its modules keep their own parameters. So an
  # empty entry set is the normal state there, not a missing file.
  if (length(files) == 0) {
    return(list(entries = list(), roles = NULL))
  }

  raw = files %>%
    set_names(tools::file_path_sans_ext(basename(.))) %>%
    map(read_yaml)

  # Peel the reserved `_channel` block off each file and stamp its role onto
  # entries that do not declare their own.
  roles   = list()
  entries = list()
  for (channel in names(raw)) {
    block = raw[[channel]]
    meta  = block[['_channel']]
    block[['_channel']] = NULL

    channel_role = meta$role
    if (leg == 'economy') {
      if (is.null(channel_role) || !(channel_role %in% CONFIG_ROLES)) {
        stop('economy channel `', channel, '` must declare _channel: {role: ',
             'state|transmission}')
      }
      roles[[channel]] = channel_role
      for (nm in names(block)) {
        if (is.list(block[[nm]]) && is.null(block[[nm]]$role)) {
          block[[nm]]$role = channel_role
        }
      }
    } else if (!is.null(meta)) {
      stop('behavior channel `', channel, '` carries a _channel block; roles ',
           'apply to the economy leg only')
    }

    entries[[channel]] = block
  }

  config_validate(leg, entries)

  list(entries = entries, roles = if (leg == 'economy') roles else NULL)
}



config_validate = function(leg, entries) {

  #----------------------------------------------------------------------------
  # Confirms every entry declares a known kind and carries the fields that kind
  # requires, plus the leg-specific additions (role, locked, pointer paths).
  # Errors list all problems at once.
  #
  # Parameters:
  #   - leg (str)      : 'economy' or 'behavior'
  #   - entries (list) : channel -> name -> entry
  #
  # Returns: TRUE invisibly; stops on any violation
  #----------------------------------------------------------------------------

  problems = c()

  for (channel in names(entries)) {
    channel_entries = entries[[channel]]

    if (!is.list(channel_entries) || is.null(names(channel_entries))) {
      problems = c(problems, sprintf('%s: file is not a mapping of named entries', channel))
      next
    }

    for (nm in names(channel_entries)) {
      entry = channel_entries[[nm]]
      label = sprintf('%s.%s', channel, nm)

      if (!is.list(entry)) {
        problems = c(problems, sprintf(
          '%s: must be a mapping with at least `value` and `kind` (got a bare value)', label))
        next
      }

      kind = entry$kind
      if (is.null(kind) || !(kind %in% names(CONFIG_ENTRY_SCHEMA))) {
        problems = c(problems, sprintf(
          '%s: kind must be one of %s (got %s)', label,
          paste(names(CONFIG_ENTRY_SCHEMA), collapse = '/'),
          if (is.null(kind)) 'nothing' else as.character(kind)))
        next
      }

      if (!is.null(entry$acknowledged) &&
          !all(c('date', 'reason') %in% names(entry$acknowledged))) {
        problems = c(problems, sprintf(
          '%s: acknowledged block requires both `date` and `reason`', label))
      }
      if (!is.null(entry$active_when) &&
          !all(grepl('^[a-z_]+[.][a-z0-9_]+$', names(entry$active_when)))) {
        problems = c(problems, sprintf(
          '%s: active_when keys must be {channel}.{name}', label))
      }
      if (!is.null(entry$conditioned_on) &&
          !all(grepl('^(economy|behavior)[.][a-z0-9_]+[.][a-z0-9_]+$',
                     names(entry$conditioned_on)))) {
        problems = c(problems, sprintf(
          '%s: conditioned_on keys must be {leg}.{channel}.{name}', label))
      }
      if (!is.null(entry$role) && !(entry$role %in% CONFIG_ROLES)) {
        problems = c(problems, sprintf(
          '%s: role must be state or transmission (got %s)', label, entry$role))
      }
      if (!is.null(entry$locked) && !is.logical(entry$locked)) {
        problems = c(problems, sprintf('%s: locked must be true/false', label))
      }
      if (!is.null(entry$enforcement) &&
          !(entry$enforcement %in% CONFIG_ENFORCEMENTS)) {
        problems = c(problems, sprintf(
          '%s: enforcement must be stop or warn (got %s)', label,
          as.character(entry$enforcement)))
      }

      missing = setdiff(CONFIG_ENTRY_SCHEMA[[kind]], names(entry))
      if (length(missing) > 0) {
        problems = c(problems, sprintf(
          '%s: kind `%s` requires %s', label, kind, paste(missing, collapse = ', ')))
      }

      # A calibrated entry's dependency list is what the staleness check reads;
      # a path that does not exist would silently never trip it. Same for a
      # pointer value: a dangling pointer should die at load, not mid-run.
      if (identical(kind, 'calibrated')) {
        for (f in entry$invalidated_by) {
          if (!file.exists(f)) {
            problems = c(problems, sprintf(
              '%s: invalidated_by names a file that does not exist (%s)', label, f))
          }
        }
        if (!is.null(entry$pointer_root)) {
          target = file.path(entry$pointer_root, as.character(entry$value))
          if (!dir.exists(target) && !file.exists(target)) {
            problems = c(problems, sprintf(
              '%s: pointer value `%s` does not exist under %s',
              label, entry$value, entry$pointer_root))
          }
        }
      }
    }
  }

  if (length(problems) > 0) {
    stop(paste0(
      'Invalid ', leg, ' configuration:\n  - ',
      paste(problems, collapse = '\n  - '), '\n'))
  }

  invisible(TRUE)
}



config_resolve = function(leg, defaults, alternative = NULL) {

  #----------------------------------------------------------------------------
  # Resolves one scenario's configuration for one leg: the default layer,
  # overlaid by the alternative the runscript cell names (if any). Those two
  # layers are the whole of the precedence rule -- runscripts name files, not
  # values.
  #
  # Parameters:
  #   - leg (str)         : 'economy' or 'behavior'
  #   - defaults (list)   : output of config_load_defaults(leg)
  #   - alternative (str) : path under the leg's alternatives/ (nesting
  #                         allowed); NULL/NA/'' or 'default' means the default
  #                         layer alone
  #
  # Returns: list of
  #   - leg, alternative : identifiers
  #   - values           : channel -> name -> resolved value
  #   - roles            : channel -> name -> role (economy leg)
  #   - overrides        : tibble(channel, name, default, value, source)
  #   - waivers          : named list, '{channel}.{name}' -> {date, reason},
  #                        the dated acknowledgments this alternative carries
  #----------------------------------------------------------------------------

  leg = match.arg(leg, names(CONFIG_LEG_ROOTS))

  alternative = if (is.null(alternative) || length(alternative) == 0 ||
                    is.na(alternative) || !nzchar(as.character(alternative)))
                  CONFIG_DEFAULT_NAME
                else as.character(alternative)

  values = defaults$entries %>% map(.f = ~ map(.x, 'value'))
  roles  = if (leg == 'economy') {
    defaults$entries %>% map(.f = ~ map(.x, 'role'))
  } else NULL

  overrides = tibble(channel = character(), name = character(),
                     default = character(), value = character(),
                     source  = character())
  waivers   = list()

  record = function(overrides, channel, name, new_value, source) {
    bind_rows(overrides, tibble(
      channel = channel,
      name    = name,
      default = paste(as.character(defaults$entries[[channel]][[name]]$value),
                      collapse = ' '),
      value   = paste(as.character(new_value), collapse = ' '),
      source  = source))
  }

  check_known = function(channel, nm, source, waiver_only = FALSE) {
    if (!(channel %in% names(defaults$entries))) {
      stop(source, " names an unknown ", leg, " channel: ", channel)
    }
    if (!(nm %in% names(defaults$entries[[channel]]))) {
      stop(source, " names an unknown ", leg, " entry: ", channel, '.', nm)
    }
    # A waiver accepts a staleness finding; it does not change the value, so
    # `locked` has nothing to protect against.
    if (isTRUE(defaults$entries[[channel]][[nm]]$locked) && !waiver_only) {
      stop(channel, '.', nm, ' is locked (', source, ' tried to override it). ',
           'Locked entries are never scenario-overridable.')
    }
  }

  # The alternative: a sparse delta over the default layer
  if (!identical(alternative, CONFIG_DEFAULT_NAME)) {
    alt_path = config_leg_path(leg, alternative)
    if (!dir.exists(alt_path)) {
      stop('Unknown ', leg, " alternative '", alternative, "' -- no folder ",
           alt_path)
    }

    label = paste0(leg, " alternative '", alternative, "'")
    for (f in list.files(alt_path, pattern = '[.]yaml$', full.names = TRUE)) {
      if (basename(f) == 'behavior.yaml') next
      channel = tools::file_path_sans_ext(basename(f))
      alt_entries = read_yaml(f)
      for (nm in names(alt_entries)) {
        check_known(channel, nm, label,
                    waiver_only = is.null(alt_entries[[nm]]$value))

        # A dated waiver: the alternative accepts a staleness finding on this
        # entry rather than changing its value. Humans write these; they show
        # up in the manifest and print a banner at parse time.
        waiver = alt_entries[[nm]]$waiver
        if (!is.null(waiver)) {
          if (!all(c('date', 'reason') %in% names(waiver))) {
            stop(label, ' waiver on ', channel, '.', nm,
                 ' requires both `date` and `reason`')
          }
          waivers[[paste(channel, nm, sep = '.')]] = waiver
        }

        new_value = alt_entries[[nm]]$value
        if (is.null(new_value)) {
          if (!is.null(waiver)) next          # waiver-only entry, value unchanged
          stop(label, ' override ', channel, '.', nm,
               ' must supply a `value` (or a dated `waiver` block)')
        }
        overrides = record(overrides, channel, nm, new_value,
                           paste0('alternative:', alternative))
        values[[channel]][[nm]] = new_value
      }
    }
  }

  list(leg = leg, alternative = alternative, values = values, roles = roles,
       overrides = overrides, waivers = waivers)
}



config_activate = function(economy = NULL, behavior = NULL) {

  #----------------------------------------------------------------------------
  # Installs a scenario's resolved legs as the active configuration. Call once
  # per scenario, before any calculation reads a value. Either leg may be NULL
  # (its accessor then fail-closes), which the phased migration relies on.
  #----------------------------------------------------------------------------

  assign('economy',  economy,  envir = .scenario_config_active)
  assign('behavior', behavior, envir = .scenario_config_active)
  if (!exists('pass', envir = .scenario_config_active, inherits = FALSE)) {
    assign('pass', NA_character_, envir = .scenario_config_active)
  }
  invisible(NULL)
}



config_set_pass = function(pass) {

  #----------------------------------------------------------------------------
  # Declares which run type is executing ('static' or 'conventional'). The
  # economy accessor uses it to refuse transmission-channel reads on the
  # static pass. Set at the top of each pass; NA between passes.
  #----------------------------------------------------------------------------

  if (!is.na(pass)) pass = match.arg(pass, c('static', 'conventional'))
  assign('pass', pass, envir = .scenario_config_active)
  invisible(NULL)
}



config_param = function(leg, channel, name) {

  #----------------------------------------------------------------------------
  # Reads one resolved value from the active scenario. Internal; use
  # economy_param() / behavior_param().
  #----------------------------------------------------------------------------

  if (!exists(leg, envir = .scenario_config_active, inherits = FALSE) ||
      is.null(get(leg, envir = .scenario_config_active, inherits = FALSE))) {
    stop(leg, "_param('", channel, "', '", name, "') was read before any ",
         'scenario was activated for the ', leg, ' leg. Call ',
         'config_activate() first (do_scenario / the SLURM workers do this).')
  }

  resolved = get(leg, envir = .scenario_config_active, inherits = FALSE)

  if (!(channel %in% names(resolved$values))) {
    stop('Unknown ', leg, ' channel: ', channel)
  }
  if (!(name %in% names(resolved$values[[channel]]))) {
    stop('Unknown ', leg, ' entry: ', channel, '.', name)
  }

  if (leg == 'economy') {
    pass = get('pass', envir = .scenario_config_active, inherits = FALSE)
    role = resolved$roles[[channel]][[name]] %||% 'state'
    if (identical(pass, 'static') && identical(role, 'transmission')) {
      stop("economy_param('", channel, "', '", name, "') read on the STATIC ",
           'pass, but ', channel, '.', name, ' is a transmission entry -- ',
           'static results are law-only by definition.')
    }
  }

  resolved$values[[channel]][[name]]
}



economy_param  = function(channel, name) config_param('economy',  channel, name)
behavior_param = function(channel, name) config_param('behavior', channel, name)



config_interface_vintages = function(resolved_economy) {

  #----------------------------------------------------------------------------
  # Derives the interface-vintage list the staleness check compares
  # derived_under blocks against, from the resolved economy leg itself
  # ({name}_vintage entries of the interfaces channel). This is the
  # simplification the fold buys: the pins and the live values live in the
  # same system.
  #
  # Returns: named list, interface key -> vintage (character)
  #----------------------------------------------------------------------------

  entries = resolved_economy$values$interfaces
  if (is.null(entries)) {
    stop('resolved economy leg has no interfaces channel')
  }
  keys = names(entries) %>% keep(.p = ~ endsWith(.x, '_vintage'))
  keys %>%
    set_names(str_remove(keys, '_vintage$')) %>%
    map(.f = ~ as.character(entries[[.x]]))
}



config_file_hash = function(path) {

  #----------------------------------------------------------------------------
  # Content hash of a dependency file (md5 rather than a git blob hash so the
  # check works in a dirty tree and outside git).
  #----------------------------------------------------------------------------

  unname(tools::md5sum(path))
}



config_check_staleness = function(leg, defaults, resolved, interface_vintages,
                                  cross_values = NULL,
                                  enforce = CONFIG_ENFORCE_STALENESS) {

  #----------------------------------------------------------------------------
  # For every entry of kind `calibrated`, confirms that (a) the data vintages
  # it was derived under match the ones this run resolves to, (b) the files in
  # invalidated_by have not changed since it was pinned, and (c) any
  # conditioned_on config values match the live resolved values.
  #
  # A value the scenario deliberately overrode is skipped: overriding IS the
  # acknowledgment, and the override is recorded in the manifest.
  #
  # Parameters:
  #   - leg (str)                 : 'economy' or 'behavior'
  #   - defaults (list)           : output of config_load_defaults(leg)
  #   - resolved (list)           : output of config_resolve(leg, ...)
  #   - interface_vintages (list) : interface key -> vintage in use (from
  #                                 config_interface_vintages())
  #   - cross_values (list)       : list(economy = <values>, behavior =
  #                                 <values>) for conditioned_on checks;
  #                                 legs not supplied are skipped with a finding
  #   - enforce (bool)            : TRUE stops on staleness, FALSE warns
  #
  # Returns: character vector of findings (empty if clean); stops when
  #          enforcing, except for entries declaring `enforcement: warn`,
  #          which always warn and are returned alongside
  #----------------------------------------------------------------------------

  overridden = paste(resolved$overrides$channel, resolved$overrides$name, sep = '.')

  findings = c()   # enforcement: stop
  soft     = c()   # enforcement: warn
  notes    = c()

  for (channel in names(defaults$entries)) {
    for (nm in names(defaults$entries[[channel]])) {

      entry = defaults$entries[[channel]][[nm]]
      if (!identical(entry$kind, 'calibrated')) next

      label = paste(channel, nm, sep = '.')
      if (label %in% overridden) next

      # A dated waiver from the pointing alternative: recorded loudly, checked
      # no further.
      waiver = resolved$waivers[[label]]
      if (!is.null(waiver)) {
        notes = c(notes, sprintf('%s: WAIVED (%s) -- %s', label,
                                 waiver$date, waiver$reason))
        next
      }

      entry_findings = c()

      # Skip entries the live configuration does not read (see assumptions.R
      # for the kg response-form history of active_when).
      if (!is.null(entry$active_when)) {
        inactive = FALSE
        for (cond in names(entry$active_when)) {
          parts = str_split_1(cond, '[.]')
          live  = resolved$values[[parts[1]]][[parts[2]]]
          if (!identical(as.character(live),
                         as.character(entry$active_when[[cond]]))) {
            inactive = TRUE
          }
        }
        if (inactive) next
      }

      # An explicit, dated waiver, carried into the manifest.
      if (!is.null(entry$acknowledged)) {
        notes = c(notes, sprintf(
          '%s: ACKNOWLEDGED STALE (%s) -- %s', label,
          entry$acknowledged$date %||% 'undated',
          entry$acknowledged$reason %||% 'no reason recorded'))
        next
      }

      # (a) data vintages
      for (dep in names(entry$derived_under)) {
        pinned = as.character(entry$derived_under[[dep]])
        live   = interface_vintages[[dep]]
        if (is.null(live)) {
          entry_findings = c(entry_findings, sprintf(
            '%s: derived_under names an interface this run does not use (%s)',
            label, dep))
          next
        }
        if (!identical(as.character(live), pinned)) {
          entry_findings = c(entry_findings, sprintf(
            '%s: pinned against %s vintage %s, this run uses %s\n      re-derive with %s',
            label, dep, pinned, live, entry$rederive))
        }
      }

      # (b) dependency file contents
      recorded = entry$invalidated_by_hashes
      if (is.null(recorded)) {
        entry_findings = c(entry_findings, sprintf(
          '%s: no invalidated_by_hashes recorded -- run config_repin_hashes()',
          label))
      } else {
        for (f in entry$invalidated_by) {
          pinned_hash = recorded[[f]]
          if (is.null(pinned_hash)) {
            entry_findings = c(entry_findings, sprintf(
              '%s: no hash recorded for dependency %s', label, f))
            next
          }
          if (!identical(config_file_hash(f), as.character(pinned_hash))) {
            entry_findings = c(entry_findings, sprintf(
              '%s: %s has changed since this value was pinned on %s\n      re-derive with %s, or override the value in an alternative',
              label, f, entry$set, entry$rederive))
          }
        }
      }

      # (c) conditioned-on config values
      for (cond in names(entry$conditioned_on)) {
        parts = str_split_1(cond, '[.]')       # leg.channel.name
        live_leg = cross_values[[parts[1]]]
        if (is.null(live_leg)) {
          entry_findings = c(entry_findings, sprintf(
            '%s: conditioned_on %s but the %s leg was not supplied for the check',
            label, cond, parts[1]))
          next
        }
        live = live_leg[[parts[2]]][[parts[3]]]
        if (!isTRUE(all.equal(live, entry$conditioned_on[[cond]]))) {
          entry_findings = c(entry_findings, sprintf(
            '%s: calibrated under %s = %s, this run resolves it to %s\n      re-derive with %s, or override the value in an alternative',
            label, cond, as.character(entry$conditioned_on[[cond]]),
            paste(as.character(live), collapse = ' '), entry$rederive))
        }
      }

      if (identical(entry$enforcement, 'warn')) {
        soft = c(soft, entry_findings)
      } else {
        findings = c(findings, entry_findings)
      }
    }
  }

  if (length(soft) > 0) {
    warning(paste0(
      '\n', strrep('-', 78), '\n',
      'STALE CALIBRATION, warn-level (', leg, ' leg)\n\n',
      'These entries declare `enforcement: warn`: the run continues, but the\n',
      'values below were derived under inputs this run does not use.\n\n  - ',
      paste(soft, collapse = '\n  - '), '\n',
      strrep('-', 78), '\n'), call. = FALSE)
  }

  if (length(notes) > 0) {
    message(paste0(
      '\n', strrep('-', 78), '\n',
      'WAIVED / ACKNOWLEDGED-STALE CALIBRATIONS (', leg, ' leg)\n\n  - ',
      paste(notes, collapse = '\n  - '), '\n',
      strrep('-', 78), '\n'))
  }

  if (length(findings) > 0) {
    banner = paste0(
      '\n', strrep('-', 78), '\n',
      'STALE CALIBRATION (', leg, ' leg)\n\n',
      'One or more calibrated values no longer match the inputs they were\n',
      'derived under. Either re-derive and re-pin, or override the value in\n',
      'an alternative (which records the choice in the manifest).\n\n  - ',
      paste(findings, collapse = '\n  - '), '\n',
      strrep('-', 78), '\n')

    if (enforce) {
      stop(banner)
    } else {
      warning(banner, call. = FALSE)
    }
  }

  return(c(findings, soft))
}



config_repin_hashes = function(leg, layer = CONFIG_DEFAULT_NAME, channel = NULL) {

  #----------------------------------------------------------------------------
  # Records the current content hash of every calibrated entry's
  # invalidated_by files in one layer of one leg. Run AFTER re-deriving a value
  # (or after a verified behavior-preserving refactor of a dependency) -- it
  # is the acknowledgment step, never wired into a run.
  #----------------------------------------------------------------------------

  leg  = match.arg(leg, names(CONFIG_LEG_ROOTS))
  root = config_leg_path(leg, layer)

  files = list.files(root, pattern = '[.]yaml$', full.names = TRUE)
  files = files[basename(files) != 'behavior.yaml']
  if (!is.null(channel)) {
    files = files[tools::file_path_sans_ext(basename(files)) %in% channel]
    if (length(files) == 0) stop('No ', leg, ' channel file for: ', channel)
  }

  for (f in files) {
    entries = read_yaml(f)
    touched = FALSE

    for (nm in names(entries)) {
      if (nm == '_channel') next
      if (!identical(entries[[nm]]$kind, 'calibrated')) next
      hashes = entries[[nm]]$invalidated_by %>%
        set_names(.) %>%
        map(config_file_hash)
      entries[[nm]]$invalidated_by_hashes = hashes
      touched = TRUE
    }

    if (touched) {
      write_yaml(entries, f)
      message('Re-pinned dependency hashes in ', f)
    }
  }

  invisible(NULL)
}



config_manifest = function(leg, defaults, resolved, id) {

  #----------------------------------------------------------------------------
  # Flattens one scenario's resolved leg into manifest rows: every value, its
  # kind and role, whether the scenario overrode it, and from where.
  #
  # Returns: tibble of ID, leg, alternative, channel, name, value, kind, role,
  #          overridden, source
  #----------------------------------------------------------------------------

  overrides = resolved$overrides %>%
    select(channel, name, source)

  names(resolved$values) %>%
    map(.f = function(ch) {
      entries = resolved$values[[ch]]
      nms     = names(entries)
      tibble(
        channel = rep(ch, length(nms)),
        name    = nms,
        value   = vapply(nms,
                         function(n) paste(as.character(entries[[n]]), collapse = ' '),
                         character(1), USE.NAMES = FALSE),
        kind    = vapply(nms,
                         function(n) as.character(defaults$entries[[ch]][[n]]$kind),
                         character(1), USE.NAMES = FALSE),
        role    = vapply(nms,
                         function(n) {
                           if (leg == 'economy')
                             as.character(resolved$roles[[ch]][[n]] %||% 'state')
                           else NA_character_
                         },
                         character(1), USE.NAMES = FALSE))
    }) %>%
    bind_rows() %>%
    left_join(overrides, by = c('channel', 'name')) %>%
    mutate(ID          = id,
           leg         = leg,
           alternative = resolved$alternative,
           overridden  = !is.na(source),
           source      = replace_na(source, 'default')) %>%
    relocate(ID, leg, alternative)
}



parse_year_spec = function(x) {

  #----------------------------------------------------------------------------
  # Parses a runscript year specification: '2030', '2026:2035', or a
  # space-delimited list '2027 2030 2033'. The single implementation that
  # replaces the two divergent parsers in config_parser.R.
  #
  # Returns: sorted integer vector; stops on anything malformed
  #----------------------------------------------------------------------------

  x = trimws(as.character(x))
  if (length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop('Empty year specification')
  }

  parse_one = function(tok) {
    if (grepl(':', tok, fixed = TRUE)) {
      ends = str_split_1(tok, ':')
      if (length(ends) != 2 || anyNA(suppressWarnings(as.integer(ends)))) {
        stop("Malformed year range '", tok, "' -- expected {start}:{end}")
      }
      return(as.integer(ends[1]):as.integer(ends[2]))
    }
    year = suppressWarnings(as.integer(tok))
    if (is.na(year)) stop("Malformed year '", tok, "'")
    year
  }

  out = str_split_1(x, '\\s+') %>%
    map(parse_one) %>%
    unlist() %>%
    unique() %>%
    sort()

  # Keep a contiguous range as a compact sequence, which is what the old
  # `{start}:{end}` parser produced. Values are identical either way, but the
  # compact form's deferred as.character() serializes differently, and year
  # labels reach disk inside .rds state files (kg's life-table extension) --
  # so materializing here would break byte-identity against pre-redesign runs
  # for no gain.
  if (length(out) > 1 && all(diff(out) == 1L)) out[1]:out[length(out)] else out
}
