#-------------------------------------------------------------------------------
# scenario_config.R
#
# Contains functions to resolve a scenario's economy and behavior configuration
#-------------------------------------------------------------------------------

# A scenario is three things: its tax law, its economy, and its behavior. Tax law
# has its own parser, in src/data/tax_law.R, because it replaces subparameters
# rather than whole values. This file serves the other two.
#
# The economy leg describes the world the policy acts on. Its files are grouped
# into channels, and each channel is one of two kinds. A state channel can be read
# on either pass. A transmission channel describes how a tax change propagates, so
# reading one on the static pass is an error.
#
# The behavior leg lists how agents respond. Its module list has its own loader in
# src/sim/behavior.R; what this file handles is the folder shape.
#
# Both legs are laid out the way tax law already is: a default folder holding a
# complete specification, and alternative folders holding sparse changes to it. A
# runscript cell names either the default or one of those folders. There are only
# two layers, and the alternative wins.
#
# A runscript names folders, never values. A cell in a spreadsheet carries no
# record of where a number came from; an entry in a folder does. A sweep is
# therefore a set of generated folders, written by whatever writes the runscript
# rows.
#
# Values belong to a scenario, so read them where they are used, through
# economy_param() or behavior_param(), which resolve against whichever scenario
# config_activate() installed. Never read one at source time.
#
# Every entry declares what kind of number it is and owes provenance accordingly:
# a calibrated value owes the conditions it was derived under, a sourced value a
# citation, and a judgment or structural value a note. Calibrated values can go
# stale, which is checked against the data vintages they were derived under, the
# hashes of the files they depend on, and any configuration values they were
# conditioned on. That last check exists because a calibration can depend on
# another configured number, which no file hash would catch.
#
# Four things can also be declared per entry. A role, on the economy leg, marking
# the channel as state or transmission. A lock, meaning no alternative may override
# it, which the estate valuation bridge uses. An enforcement level, so that a stale
# finding warns instead of stopping: the estate bridge is deliberately run against
# older Tax-Data vintages for historical comparisons, and being locked it has no
# override to fall back on. And a pointer, where the value names a file or folder
# rather than a number, which is how the table-valued wealth saving profile is
# covered by the staleness check.
#-------------------------------------------------------------------------------


# Where each leg lives. Tax law is absent on purpose: it replaces subparameters
# rather than values, and has its own parser.
CONFIG_LEG_ROOTS = list(
  economy  = './config/scenarios/economy',
  behavior = './config/scenarios/behavior'
)

# The reserved name for the default layer. A folder of the same name under
# alternatives would be unreachable, so it is an error rather than ignored.
CONFIG_DEFAULT_NAME = 'default'


config_leg_path = function(leg, name) {

  #----------------------------------------------------------------------------
  # Locates one layer of one leg. The only place the two-folder shape is written
  # down.
  #
  # Returns: path to the folder (str).
  #----------------------------------------------------------------------------

  root = CONFIG_LEG_ROOTS[[leg]]
  if (identical(name, CONFIG_DEFAULT_NAME)) file.path(root, CONFIG_DEFAULT_NAME)
  else file.path(root, 'alternatives', name)
}

# Whether a stale calibration stops the run.
CONFIG_ENFORCE_STALENESS = TRUE

# What each kind of entry must declare
CONFIG_ENTRY_SCHEMA = list(
  calibrated = c('value', 'kind', 'set', 'target', 'derived_under',
                 'invalidated_by', 'rederive'),
  sourced    = c('value', 'kind', 'citation'),
  judgment   = c('value', 'kind', 'note'),
  structural = c('value', 'kind', 'note')
)

CONFIG_ROLES = c('state', 'transmission')

# How hard a stale finding lands
CONFIG_ENFORCEMENTS = c('stop', 'warn')

# The active scenario's resolved legs, and which pass is running. An environment
# rather than a global, so that a forked worker changes only its own copy.
.scenario_config_active = new.env(parent = emptyenv())

# Run-level state that is not configuration, such as the estate measurement
# parameters, loaded once per run.
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
  # Reads an object stored by run_state_set(); errors if it was never set.
  #----------------------------------------------------------------------------

  if (!exists(name, envir = .run_state, inherits = FALSE)) {
    stop("run_state_get('", name, "') read before run_state_set() stored it")
  }
  get(name, envir = .run_state, inherits = FALSE)
}



config_load_defaults = function(leg) {

  #----------------------------------------------------------------------------
  # Reads and checks a leg's default layer: every channel file under it.
  #
  # Parameters:
  #   - leg (str) : 'economy' or 'behavior'
  #
  # Returns: list of the entries by channel and name, with each entry's role
  #          filled in from its channel, and the channel roles themselves.
  #----------------------------------------------------------------------------

  leg  = match.arg(leg, names(CONFIG_LEG_ROOTS))
  root = config_leg_path(leg, CONFIG_DEFAULT_NAME)
  if (!dir.exists(root)) {
    stop('No default layer found for the ', leg, ' leg (expected ', root, ')')
  }

  # A folder named default under alternatives could never be selected, since that
  # name is reserved. Catch it here rather than leave it sitting there.
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
  # The behavior leg carries no values. Its content is behavior.yaml, which
  # src/sim/behavior.R reads, and its modules hold their own parameters. So no
  # entries is the normal state there, not a missing file.
  if (length(files) == 0) {
    return(list(entries = list(), roles = NULL))
  }

  raw = files %>%
    set_names(tools::file_path_sans_ext(basename(.))) %>%
    map(read_yaml)

  # Take the channel block off each file and give its role to any entry that does
  # not declare one.
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
  # Checks that every entry declares a known kind and carries what that kind
  # requires, along with the role, lock and pointer fields. Reports every problem
  # at once rather than the first.
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
      # The kg settings count as a third source a calibration can be conditioned
      # on, alongside the two legs. The kg calibrations depend on the model-form
      # switches there, so changing a switch stops the run.
      if (!is.null(entry$conditioned_on) &&
          !all(grepl('^(economy|behavior|settings)[.][a-z0-9_]+[.][a-z0-9_]+$',
                     names(entry$conditioned_on)))) {
        problems = c(problems, sprintf(
          '%s: conditioned_on keys must be {economy|behavior|settings}.{channel}.{name}',
          label))
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

      # The staleness check reads these paths, so one that does not exist would
      # never trip it. A pointer to a missing file should fail at load rather than
      # midway through a run.
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
  # Resolves one leg for one scenario: the default layer, with the alternative the
  # runscript names laid over it. Those two layers are the whole of the rule.
  #
  # Parameters:
  #   - leg (str)         : 'economy' or 'behavior'
  #   - defaults (list)   : the leg's default layer
  #   - alternative (str) : path under the leg's alternatives; the default layer
  #                         alone if absent
  #
  # Returns: list of the resolved values, their roles, a table of what the
  #          scenario overrode and from where, and any dated waivers the
  #          alternative carries.
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
    # A waiver accepts a stale finding without changing the value, so a lock has
    # nothing to prevent here.
    if (isTRUE(defaults$entries[[channel]][[nm]]$locked) && !waiver_only) {
      stop(channel, '.', nm, ' is locked (', source, ' tried to override it). ',
           'Locked entries are never scenario-overridable.')
    }
  }

  # The alternative, holding only what it changes
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

        # A dated waiver, accepting a stale finding rather than changing the
        # value. Written by hand, recorded in the manifest, and announced at
        # parse time.
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
  # Installs a scenario's resolved legs as the active configuration. Call once per
  # scenario, before anything reads a value. Either leg may be absent, in which
  # case reading from it is an error.
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
  # Records which pass is running, so that reading a transmission channel on the
  # static pass can be refused. Set at the top of each pass, and empty between
  # them.
  #----------------------------------------------------------------------------

  if (!is.na(pass)) {
    pass = match.arg(pass, c('static', 'mechanical', 'conventional'))
  }
  assign('pass', pass, envir = .scenario_config_active)
  invisible(NULL)
}



config_param = function(leg, channel, name) {

  #----------------------------------------------------------------------------
  # Reads one value from the active scenario. Use economy_param() or
  # behavior_param() instead of calling this.
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
  # Collects the data vintages this run uses, from the economy leg's interfaces
  # channel. The staleness check compares each calibration's recorded vintages
  # against these. Both live in the same place, which is why no separate
  # bookkeeping is needed.
  #
  # Returns: named list of interface to vintage.
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
  # Hashes a file's contents. md5 rather than a git hash, so the check works in a
  # dirty tree and outside a repository.
  #----------------------------------------------------------------------------

  unname(tools::md5sum(path))
}



config_check_staleness = function(leg, defaults, resolved, interface_vintages,
                                  cross_values = NULL,
                                  enforce = CONFIG_ENFORCE_STALENESS) {

  #----------------------------------------------------------------------------
  # Checks every calibrated entry three ways: that the data vintages it was derived
  # under are the ones this run uses, that the files it depends on have not changed
  # since it was pinned, and that any configuration values it was conditioned on
  # still hold.
  #
  # An entry the scenario overrode is skipped. Overriding it is the acknowledgment,
  # and the override is recorded in the manifest.
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

      # A dated waiver from the alternative that named this entry. Announced, and
      # checked no further.
      waiver = resolved$waivers[[label]]
      if (!is.null(waiver)) {
        notes = c(notes, sprintf('%s: WAIVED (%s) -- %s', label,
                                 waiver$date, waiver$reason))
        next
      }

      entry_findings = c()

      # Skip entries the live configuration does not read.
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

      # A dated waiver, carried into the manifest
      if (!is.null(entry$acknowledged)) {
        notes = c(notes, sprintf(
          '%s: ACKNOWLEDGED STALE (%s) -- %s', label,
          entry$acknowledged$date %||% 'undated',
          entry$acknowledged$reason %||% 'no reason recorded'))
        next
      }

      # The data vintages
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

      # The files it depends on
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

      # The configuration values it was conditioned on
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



config_repin_hashes = function(leg, layer = CONFIG_DEFAULT_NAME, channel = NULL,
                               dry_run = FALSE) {

  #----------------------------------------------------------------------------
  # Re-records the hashes of the files each calibrated entry depends on. Run it
  # after re-deriving a value, or after changing one of those files in a way that
  # has been verified to leave the model's output identical. It is never called
  # during a run.
  #
  # The hash lines are edited as text, leaving the rest of each file untouched. The
  # comments in these files are the record of where each number came from, so
  # parsing and rewriting the YAML would destroy what the file exists for.
  #
  # Parameters:
  #   - leg (str)      : 'economy' or 'behavior'
  #   - layer (str)    : 'default', or an alternative's path
  #   - channel (chr)  : optional, restrict to these channels
  #   - dry_run (bool) : report what would change without writing it
  #
  # Returns: invisibly, a tibble of the hashes that changed.
  #----------------------------------------------------------------------------

  leg  = match.arg(leg, names(CONFIG_LEG_ROOTS))
  root = config_leg_path(leg, layer)

  files = list.files(root, pattern = '[.]yaml$', full.names = TRUE)
  files = files[basename(files) != 'behavior.yaml']
  if (!is.null(channel)) {
    files = files[tools::file_path_sans_ext(basename(files)) %in% channel]
    if (length(files) == 0) stop('No ', leg, ' channel file for: ', channel)
  }

  changed = list()

  for (f in files) {
    lines   = readLines(f, warn = FALSE)
    entries = read_yaml(f)

    # Which files each entry is pinned against, and what they hash to now. The
    # parse is used only to find out what needs hashing.
    wanted = list()
    for (nm in names(entries)) {
      if (nm == '_channel') next
      if (!identical(entries[[nm]]$kind, 'calibrated')) next
      for (dep in unlist(entries[[nm]]$invalidated_by)) {
        wanted[[dep]] = config_file_hash(dep)
      }
    }
    if (length(wanted) == 0) next

    # A hash line is a path followed by 32 hex characters. Matching on both keeps
    # this off anything else naming the same path, including a comment.
    for (dep in names(wanted)) {
      esc     = gsub('([.|()\\^{}+$*?\\[\\]])', '\\\\\\1', dep)
      pattern = paste0('^(\\s+)', esc, ':\\s*([0-9a-f]{32})\\s*$')
      hits    = grep(pattern, lines)
      for (i in hits) {
        old = sub(pattern, '\\2', lines[i])
        if (identical(old, wanted[[dep]])) next
        lines[i] = sub(pattern, paste0('\\1', dep, ': ', wanted[[dep]]), lines[i])
        changed[[length(changed) + 1]] = tibble(
          file = f, line = i, dependency = dep,
          old_hash = old, new_hash = wanted[[dep]])
      }
    }

    if (length(changed) > 0 && !dry_run) writeLines(lines, f)
  }

  out = bind_rows(changed)

  if (nrow(out) == 0) {
    message('No dependency hashes needed re-pinning in ', root)
  } else {
    message(if (dry_run) 'Would re-pin ' else 'Re-pinned ', nrow(out),
            ' dependency hash(es) in ', root)
    print(as.data.frame(out %>% select(file, dependency, old_hash, new_hash)))
  }

  invisible(out)
}



config_manifest = function(leg, defaults, resolved, id) {

  #----------------------------------------------------------------------------
  # Flattens one resolved leg into manifest rows: every value, its kind and role,
  # and whether the scenario overrode it.
  #
  # Returns: tibble of one row per value. No rows for the behavior leg, which
  #          carries none; its module list is recorded separately.
  #----------------------------------------------------------------------------

  if (length(resolved$values) == 0) {
    return(tibble(ID = character(), leg = character(), alternative = character(),
                  channel = character(), name = character(), value = character(),
                  kind = character(), role = character(),
                  overridden = logical(), source = character()))
  }

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
  # Parses a runscript's years, given as a single year, a range, or a
  # space-separated list.
  #
  # Returns: sorted integer vector; stops on anything malformed.
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

  # Return a contiguous range as a sequence rather than an expanded vector. The
  # values are the same either way, but the two serialize differently, and year
  # labels reach disk inside the state files. Expanding here would change those
  # files for no gain.
  if (length(out) > 1 && all(diff(out) == 1L)) out[1]:out[length(out)] else out
}
