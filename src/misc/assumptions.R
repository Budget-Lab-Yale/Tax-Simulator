#-------------------------------------------------------------------------------
# assumptions.R
#
# Model ASSUMPTIONS: the economic magnitudes and model-form switches that
# describe how the world works, as distinct from tax LAW (what the policy is)
# and from numerical plumbing (epsilons, tolerances, guard caps).
#
# Defaults live in config/assumptions/{channel}.yaml, one file per channel,
# mirroring the way config/scenarios/tax_law splits by topic. Every entry
# carries its provenance inline -- kind, when it was set, what it was set
# against -- so a value and its story cannot drift apart.
#
# A scenario may override any of them, because a scenario is any counterfactual,
# not only a policy change. Two override mechanisms, mirroring the two that
# already exist in the runscript:
#
#   - assumption.{channel}.{name} column   single value, follows dep.{X}.vintage
#   - assumptions column                   folder of override files, follows tax_law
#
# Precedence: dotted column > override folder > default.
#
# IMPORTANT: assumption values are SCENARIO-scoped, so they must never be frozen
# at source time the way a top-level constant is. Read them at the point of use
# via assumption(), which resolves against the scenario activated by
# assumptions_activate(). Sourcing order then does not matter, and two scenarios
# in one run cannot contaminate each other.
#-------------------------------------------------------------------------------


# Root of the default assumption files
ASSUMPTIONS_ROOT = './config/assumptions'

# Whether a stale calibration halts the run. TRUE since 2026-07-25, when the
# last channel was migrated. A stale value now STOPS the run rather than warning
# into a log nobody reads -- the previous warn-only guards (KG_DYN_CALIB_
# PROVENANCE, WEALTH_DYN_PROVENANCE) and the never-installed pre-push hook are
# what this replaces. Three legitimate ways past a stop, all of them visible in
# the vintage: re-derive and re-pin, override the value in the runscript, or
# record a dated `acknowledged` block in the config.
ASSUMPTIONS_ENFORCE_STALENESS = TRUE

# Required fields by kind. The schema check is what stops a new assumption from
# being added without saying where it came from.
ASSUMPTION_SCHEMA = list(
  calibrated = c('value', 'kind', 'set', 'target', 'derived_under',
                 'invalidated_by', 'rederive'),
  sourced    = c('value', 'kind', 'citation'),
  judgment   = c('value', 'kind', 'note'),
  structural = c('value', 'kind', 'note')
)

# Run-scoped store of the currently active resolved set. Populated per scenario
# by assumptions_activate(). Kept in an environment rather than a global binding
# so that a forked worker (multicore = 'scenario') mutates only its own copy.
.assumptions_active = new.env(parent = emptyenv())



assumptions_load_defaults = function(root = ASSUMPTIONS_ROOT) {

  #----------------------------------------------------------------------------
  # Reads every channel file under config/assumptions/ and validates it against
  # ASSUMPTION_SCHEMA.
  #
  # Parameters:
  #   - root (str) : directory containing the channel YAML files
  #
  # Returns: named list of channels, each a named list of assumption entries
  #----------------------------------------------------------------------------

  files = list.files(root, pattern = '[.]yaml$', full.names = TRUE)
  if (length(files) == 0) {
    stop('No assumption files found under ', root)
  }

  defaults = files %>%
    set_names(tools::file_path_sans_ext(basename(.))) %>%
    map(read_yaml)

  assumptions_validate(defaults)

  return(defaults)
}



assumptions_validate = function(defaults) {

  #----------------------------------------------------------------------------
  # Confirms every entry declares a known kind and carries the fields that kind
  # requires. Errors list all problems at once rather than one per run.
  #
  # Parameters:
  #   - defaults (list) : nested channel -> name -> entry list
  #
  # Returns: TRUE invisibly; stops on any violation
  #----------------------------------------------------------------------------

  problems = c()

  for (channel in names(defaults)) {
    entries = defaults[[channel]]

    if (!is.list(entries) || is.null(names(entries))) {
      problems = c(problems, sprintf('%s: file is not a mapping of named entries', channel))
      next
    }

    for (nm in names(entries)) {
      entry = entries[[nm]]
      label = sprintf('%s.%s', channel, nm)

      if (!is.list(entry)) {
        problems = c(problems, sprintf(
          '%s: must be a mapping with at least `value` and `kind` (got a bare value)', label))
        next
      }

      kind = entry$kind
      if (is.null(kind) || !(kind %in% names(ASSUMPTION_SCHEMA))) {
        problems = c(problems, sprintf(
          '%s: kind must be one of %s (got %s)', label,
          paste(names(ASSUMPTION_SCHEMA), collapse = '/'),
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

      missing = setdiff(ASSUMPTION_SCHEMA[[kind]], names(entry))
      if (length(missing) > 0) {
        problems = c(problems, sprintf(
          '%s: kind `%s` requires %s', label, kind, paste(missing, collapse = ', ')))
      }

      # A calibrated entry's dependency list is what the staleness check reads;
      # a path that does not exist would silently never trip it.
      if (identical(kind, 'calibrated')) {
        for (f in entry$invalidated_by) {
          if (!file.exists(f)) {
            problems = c(problems, sprintf(
              '%s: invalidated_by names a file that does not exist (%s)', label, f))
          }
        }
      }
    }
  }

  if (length(problems) > 0) {
    stop(paste0(
      'Invalid assumption configuration:\n  - ',
      paste(problems, collapse = '\n  - '), '\n'))
  }

  invisible(TRUE)
}



assumptions_resolve = function(defaults, runscript_items) {

  #----------------------------------------------------------------------------
  # Applies a scenario's overrides to the defaults.
  #
  # Parameters:
  #   - defaults (list)        : output of assumptions_load_defaults()
  #   - runscript_items (list) : one scenario's runscript row, as a named list
  #
  # Returns: list of 2:
  #   - values (list)    : channel -> name -> resolved value (bare, not the entry)
  #   - overrides (df)   : tibble of channel, name, default, value, source for
  #                        every value the scenario changed (0 rows if none)
  #----------------------------------------------------------------------------

  values = defaults %>%
    map(.f = ~ map(.x, 'value'))

  overrides = tibble(channel  = character(),
                     name     = character(),
                     default  = character(),
                     value    = character(),
                     source   = character())

  record = function(overrides, channel, name, new_value, source) {
    bind_rows(overrides, tibble(
      channel = channel,
      name    = name,
      default = as.character(defaults[[channel]][[name]]$value),
      value   = as.character(new_value),
      source  = source))
  }

  # 1. Override folder (the tax_law pattern), if named
  folder = runscript_items$assumptions
  if (!is.null(folder) && length(folder) > 0 && !is.na(folder) && nzchar(as.character(folder))) {

    folder_path = file.path(ASSUMPTIONS_ROOT, as.character(folder))
    if (!dir.exists(folder_path)) {
      stop("Assumption override folder not found: '", folder_path, "'")
    }

    for (f in list.files(folder_path, pattern = '[.]yaml$', full.names = TRUE)) {
      channel = tools::file_path_sans_ext(basename(f))
      if (!(channel %in% names(defaults))) {
        stop("Assumption override folder '", folder, "' names an unknown channel: ", channel)
      }
      for (nm in names(read_yaml(f))) {
        if (!(nm %in% names(defaults[[channel]]))) {
          stop("Assumption override '", channel, '.', nm, "' has no default to override")
        }
        new_value = read_yaml(f)[[nm]]$value
        if (is.null(new_value)) {
          stop("Assumption override '", channel, '.', nm, "' must supply a `value`")
        }
        overrides = record(overrides, channel, nm, new_value, paste0('folder:', folder))
        values[[channel]][[nm]] = new_value
      }
    }
  }

  # 2. Dotted columns (the dep.{interface}.vintage pattern), which win
  dotted = names(runscript_items) %>%
    keep(.p = ~ startsWith(.x, 'assumption.'))

  for (col in dotted) {
    raw = runscript_items[[col]]
    if (is.null(raw) || length(raw) == 0 || all(is.na(raw)) || !nzchar(as.character(raw)[1])) {
      next
    }

    parts = str_split_1(str_remove(col, '^assumption[.]'), '[.]')
    if (length(parts) != 2) {
      stop("Malformed runscript column '", col,
           "' -- expected assumption.{channel}.{name}")
    }
    channel = parts[1]
    nm      = parts[2]

    if (!(channel %in% names(defaults))) {
      stop("Runscript column '", col, "' names an unknown assumption channel: ", channel)
    }
    if (!(nm %in% names(defaults[[channel]]))) {
      stop("Runscript column '", col, "' names an unknown assumption: ", channel, '.', nm)
    }

    # Match the default's type: a numeric assumption stays numeric, a string
    # switch (response_form) stays a string.
    default_value = defaults[[channel]][[nm]]$value
    new_value     = as.character(raw)[1]
    if (is.numeric(default_value)) {
      new_value = suppressWarnings(as.numeric(new_value))
      if (is.na(new_value)) {
        stop("Runscript column '", col, "' must be numeric (default is ",
             default_value, ")")
      }
    } else if (is.logical(default_value)) {
      new_value = as.logical(new_value)
      if (is.na(new_value)) {
        stop("Runscript column '", col, "' must be TRUE/FALSE")
      }
    }

    overrides = record(overrides, channel, nm, new_value, 'runscript')
    values[[channel]][[nm]] = new_value
  }

  return(list(values = values, overrides = overrides))
}



assumptions_activate = function(resolved) {

  #----------------------------------------------------------------------------
  # Installs a scenario's resolved assumption values as the active set. Call
  # once per scenario, before any calculation reads an assumption.
  #
  # Parameters:
  #   - resolved (list) : output of assumptions_resolve()
  #
  # Returns: nothing (invisible NULL)
  #----------------------------------------------------------------------------

  assign('values',    resolved$values,    envir = .assumptions_active)
  assign('overrides', resolved$overrides, envir = .assumptions_active)

  invisible(NULL)
}



assumption = function(channel, name) {

  #----------------------------------------------------------------------------
  # Reads one assumption from the active scenario's resolved set. This is the
  # ONLY supported way to read an assumption: it must not be captured at source
  # time, because the value is scenario-scoped.
  #
  # Parameters:
  #   - channel (str) : channel name, i.e. the YAML file stem (e.g. 'kg')
  #   - name (str)    : assumption name within that channel (e.g. 'eta')
  #
  # Returns: the resolved value
  #----------------------------------------------------------------------------

  if (!exists('values', envir = .assumptions_active, inherits = FALSE)) {
    stop('assumption("', channel, '", "', name, '") was read before any scenario ',
         'was activated. Call assumptions_activate() first (do_scenario / the ',
         'SLURM worker do this).')
  }

  values = get('values', envir = .assumptions_active, inherits = FALSE)

  if (!(channel %in% names(values))) {
    stop('Unknown assumption channel: ', channel)
  }
  if (!(name %in% names(values[[channel]]))) {
    stop('Unknown assumption: ', channel, '.', name)
  }

  return(values[[channel]][[name]])
}



assumptions_file_hash = function(path) {

  #----------------------------------------------------------------------------
  # Content hash of a dependency file, used by the staleness check. md5 rather
  # than a git blob hash so the check works in a dirty tree and outside git.
  #
  # Parameters:
  #   - path (str) : file path relative to the repo root
  #
  # Returns: str, the md5 hash
  #----------------------------------------------------------------------------

  unname(tools::md5sum(path))
}



assumptions_check_staleness = function(defaults, resolved, interface_vintages,
                                       enforce = TRUE) {

  #----------------------------------------------------------------------------
  # For every entry of kind `calibrated`, confirms that (a) the data vintages it
  # was derived under match the ones this run is using, and (b) the files listed
  # in invalidated_by have not changed since it was pinned.
  #
  # A value the scenario deliberately overrode is skipped: overriding IS the
  # acknowledgment, and the override is recorded in the manifest.
  #
  # Parameters:
  #   - defaults (list)           : output of assumptions_load_defaults()
  #   - resolved (list)           : output of assumptions_resolve()
  #   - interface_vintages (list) : interface name -> vintage in use, lowercased
  #                                 and underscored to match the YAML keys
  #                                 (e.g. tax_data, macro)
  #   - enforce (bool)            : TRUE to stop on staleness, FALSE to warn.
  #                                 Warn mode exists only for the migration
  #                                 window; production is enforce = TRUE.
  #
  # Returns: character vector of findings (empty if clean); stops when enforcing
  #----------------------------------------------------------------------------

  overridden = paste(resolved$overrides$channel, resolved$overrides$name, sep = '.')

  findings = c()
  notes    = c()

  for (channel in names(defaults)) {
    for (nm in names(defaults[[channel]])) {

      entry = defaults[[channel]][[nm]]
      if (!identical(entry$kind, 'calibrated')) next

      label = paste(channel, nm, sep = '.')
      if (label %in% overridden) next

      # Skip entries the live configuration does not read. The kg calibrated
      # pairs are per response form: under response_form = 'logs' the levels
      # eta and timeable share never enter a calculation, so their staleness
      # cannot affect any number this run produces. An entry declares this with
      #   active_when:
      #     kg.response_form: levels
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

      # An explicit, dated waiver: the author has looked at this mismatch and
      # accepted it. Deliberately verbose and checked into the config rather
      # than being an env-var escape hatch, and it is carried into the manifest
      # so a vintage records that it ran on an acknowledged-stale value.
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
          findings = c(findings, sprintf(
            '%s: derived_under names an interface this run does not use (%s)',
            label, dep))
          next
        }
        if (!identical(as.character(live), pinned)) {
          findings = c(findings, sprintf(
            '%s: pinned against %s vintage %s, this run uses %s\n      re-derive with %s',
            label, dep, pinned, live, entry$rederive))
        }
      }

      # (b) dependency file contents
      recorded = entry$invalidated_by_hashes
      if (is.null(recorded)) {
        findings = c(findings, sprintf(
          '%s: no invalidated_by_hashes recorded -- run assumptions_repin_hashes()',
          label))
        next
      }
      for (f in entry$invalidated_by) {
        pinned_hash = recorded[[f]]
        if (is.null(pinned_hash)) {
          findings = c(findings, sprintf(
            '%s: no hash recorded for dependency %s', label, f))
          next
        }
        if (!identical(assumptions_file_hash(f), as.character(pinned_hash))) {
          findings = c(findings, sprintf(
            '%s: %s has changed since this value was pinned on %s\n      re-derive with %s, or override the value in the runscript',
            label, f, entry$set, entry$rederive))
        }
      }
    }
  }

  if (length(notes) > 0) {
    message(paste0(
      '\nAcknowledged-stale calibrations in this run:\n  - ',
      paste(notes, collapse = '\n  - '), '\n'))
  }

  if (length(findings) > 0) {
    banner = paste0(
      '\n', strrep('-', 78), '\n',
      'STALE CALIBRATION\n\n',
      'One or more calibrated assumptions no longer match the inputs they were\n',
      'derived under. Either re-derive and re-pin, or override the value in the\n',
      'runscript (which records the choice in the manifest).\n\n  - ',
      paste(findings, collapse = '\n  - '), '\n',
      strrep('-', 78), '\n')

    if (enforce) {
      stop(banner)
    } else {
      warning(banner, call. = FALSE)
    }
  }

  return(findings)
}



assumptions_repin_hashes = function(channel = NULL, root = ASSUMPTIONS_ROOT) {

  #----------------------------------------------------------------------------
  # Records the current content hash of every calibrated entry's invalidated_by
  # files. Run this AFTER re-deriving a value and updating it in the YAML --
  # it is the acknowledgment step, not an automatic fixer, so it should never
  # be wired into a run.
  #
  # Parameters:
  #   - channel (str) : optional single channel to re-pin; NULL for all
  #   - root (str)    : directory containing the channel YAML files
  #
  # Returns: nothing (invisible NULL); rewrites the YAML files in place
  #----------------------------------------------------------------------------

  files = list.files(root, pattern = '[.]yaml$', full.names = TRUE)
  if (!is.null(channel)) {
    files = files[tools::file_path_sans_ext(basename(files)) %in% channel]
    if (length(files) == 0) stop('No assumption file for channel: ', channel)
  }

  for (f in files) {
    entries = read_yaml(f)
    touched = FALSE

    for (nm in names(entries)) {
      if (!identical(entries[[nm]]$kind, 'calibrated')) next
      hashes = entries[[nm]]$invalidated_by %>%
        set_names(.) %>%
        map(assumptions_file_hash)
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



assumptions_manifest = function(defaults, resolved, id) {

  #----------------------------------------------------------------------------
  # Flattens one scenario's resolved set into manifest rows: every assumption
  # actually used, its kind, whether the scenario overrode it, and from where.
  # Written per vintage so that any output can later be traced to exactly the
  # assumptions that produced it.
  #
  # Parameters:
  #   - defaults (list) : output of assumptions_load_defaults()
  #   - resolved (list) : output of assumptions_resolve()
  #   - id (str)        : scenario ID
  #
  # Returns: tibble of ID, channel, name, value, kind, overridden, source
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
                         function(n) as.character(defaults[[ch]][[n]]$kind),
                         character(1), USE.NAMES = FALSE))
    }) %>%
    bind_rows() %>%
    left_join(overrides, by = c('channel', 'name')) %>%
    mutate(ID         = id,
           overridden = !is.na(source),
           source     = replace_na(source, 'default')) %>%
    relocate(ID)
}
