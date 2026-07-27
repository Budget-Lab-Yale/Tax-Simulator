#-------------------------------------------------------------------------------
# calibration_writer.R
#
# Contains functions to let a calibration script write its own result
#-------------------------------------------------------------------------------

# A calibrated value should never be retyped. The calibrator ends by writing its
# own entry, so the number travels as a file rather than through a log and someone's
# hands. Every one of the model's calibrated values used to be transcribed that way,
# which is how a shipped number and the run behind it came to disagree.
#
# Two requirements pull against each other here.
#
# The comments have to survive. In a calibration file the comments are the record of
# where the value came from: what it targets, what it was derived under, and why it
# is what it is. Writing the file back out through a YAML writer deletes all of them.
# So this writer works on text, replacing one block and copying every other byte
# through.
#
# And a changed value must not land quietly. Calibrated means a procedure produced
# the number, so a re-run that produces a different one is a finding rather than a
# routine update: the model may have moved, the data may have moved, or the
# calibration may be less well identified than it looks. A re-run that reproduces the
# shipped value writes in place. One that does not writes its result alongside and
# stops, so that the author can compare the two and move the new one into place if
# the change is real.
#
# Sourced at startup with the rest of src/misc, but it depends on nothing else, so a
# calibration script that does not otherwise load the model can source it alone.



calib_split_blocks = function(text) {

  #----------------------------------------------------------------------------
  # Splits a calibration file into its entries, in order.
  #
  # An entry runs from its key to the line before the next key, so the comments
  # above an entry travel with the entry below them, which is how these files are
  # written.
  #
  # Parameters:
  #   - text (str) : the whole file
  #
  # Returns: list of the preamble, the keys, and the blocks.
  #----------------------------------------------------------------------------

  lines  = strsplit(text, '\n', fixed = TRUE)[[1]]
  starts = which(grepl('^[A-Za-z_][A-Za-z0-9_]*:', lines))

  if (length(starts) == 0) {
    stop('No top-level entries found -- this does not look like a calibration file.')
  }

  ends = c(starts[-1] - 1, length(lines))

  list(preamble = paste(lines[seq_len(starts[1] - 1)], collapse = '\n'),
       keys     = sub(':.*$', '', lines[starts]),
       blocks   = map2(starts, ends, ~ paste(lines[.x:.y], collapse = '\n')))
}



calib_wrap = function(text, indent = '    ', width = 76) {

  #----------------------------------------------------------------------------
  # Wraps prose for a YAML block, so a generated note reads like the
  # hand-written ones next to it.
  #----------------------------------------------------------------------------

  words = strsplit(trimws(gsub('[[:space:]]+', ' ', text)), ' ', fixed = TRUE)[[1]]
  lines = c()
  cur   = indent

  for (w in words) {
    if (nchar(cur) > nchar(indent) && nchar(cur) + 1 + nchar(w) > width) {
      lines = c(lines, cur)
      cur   = paste0(indent, w)
    } else {
      cur = if (nchar(cur) > nchar(indent)) paste(cur, w) else paste0(cur, w)
    }
  }
  paste(c(lines, cur), collapse = '\n')
}



calib_render_field = function(name, value) {

  #----------------------------------------------------------------------------
  # One field of an entry, rendered. Handles the four shapes calibration entries
  # actually use: a scalar, a named map of scalars (derived_under,
  # conditioned_on, active_when, invalidated_by_hashes), an unnamed vector (
  # invalidated_by), and prose marked for a block scalar by wrapping it in
  # calib_prose().
  #
  # Deliberately narrow. A field shape this does not recognise is a sign the
  # entry schema grew, which is a thing to notice rather than to coerce.
  #----------------------------------------------------------------------------

  if (inherits(value, 'calib_prose')) {
    return(sprintf('  %s: >\n%s', name, calib_wrap(unclass(value))))
  }

  if (is.list(value) || (!is.null(names(value)) && length(value) > 1)) {
    inner = imap_chr(as.list(value),
                     ~ sprintf('    %s: %s', .y, calib_scalar(.x)))
    return(paste(c(sprintf('  %s:', name), inner), collapse = '\n'))
  }

  if (length(value) > 1) {
    inner = sprintf('    - %s', as.character(value))
    return(paste(c(sprintf('  %s:', name), inner), collapse = '\n'))
  }

  sprintf('  %s: %s', name, calib_scalar(value))
}



calib_scalar = function(x) {

  #----------------------------------------------------------------------------
  # A scalar as YAML. Strings that could be read as numbers or dates are
  # quoted, because '2026070814' is a Tax-Data vintage and 2026070814 is an
  # integer that will not compare equal to one.
  #----------------------------------------------------------------------------

  if (is.numeric(x) || is.logical(x)) return(format(x, trim = TRUE))
  s = as.character(x)
  if (grepl('^[-0-9]', s)) sprintf("'%s'", s) else s
}



calib_prose = function(text) {

  #----------------------------------------------------------------------------
  # Marks a field's value as prose, to be rendered as a wrapped block scalar.
  #----------------------------------------------------------------------------

  structure(text, class = 'calib_prose')
}



calib_hash_files = function(paths) {

  #----------------------------------------------------------------------------
  # md5 of each file an entry declares itself invalidated by, keyed by path.
  # Computed at write time from the files themselves, which is the point: the
  # hashes in a generated entry describe the code the value was actually
  # measured under, not the code someone believed it was.
  #----------------------------------------------------------------------------

  missing = paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop('Cannot hash a dependency that does not exist: ',
         paste(missing, collapse = ', '))
  }
  set_names(as.list(unname(tools::md5sum(paths))), paths)
}



calib_render_entry = function(name, value, fields) {

  #----------------------------------------------------------------------------
  # A whole entry block: the value first, then the provenance fields in the
  # order given.
  #----------------------------------------------------------------------------

  body = imap_chr(fields, ~ calib_render_field(.y, .x))
  paste(c(sprintf('%s:', name),
          sprintf('  value: %s', calib_scalar(value)),
          body, ''),
        collapse = '\n')
}



calib_current_value = function(path, entry) {

  #----------------------------------------------------------------------------
  # The value an entry currently carries, as a string, or NA if the file or the
  # entry is not there yet.
  #----------------------------------------------------------------------------

  if (!file.exists(path)) return(NA_character_)
  parts = calib_split_blocks(paste(readLines(path, warn = FALSE), collapse = '\n'))
  i     = match(entry, parts$keys)
  if (is.na(i)) return(NA_character_)

  m = regmatches(parts$blocks[[i]],
                 regexpr('(?m)^  value:[^\n]*', parts$blocks[[i]], perl = TRUE))
  if (length(m) == 0) return(NA_character_)
  trimws(gsub("^  value:|'", '', m))
}



calib_write_entry = function(path, entry, value, fields, tol = 0) {

  #----------------------------------------------------------------------------
  # Writes one entry of a calibration file, in place if the value is unchanged
  # and to `<path>.proposed` if it is not.
  #
  # The asymmetry is the whole design. A proving re-run is supposed to reproduce
  # its pinned value, and when it does, rewriting in place is safe and refreshes
  # the dependency hashes and the set date against the code that just ran. When
  # it does NOT reproduce it, something has moved that the author needs to look
  # at, so the new number lands beside the old one and the run says so loudly
  # rather than committing an estimate change nobody reviewed.
  #
  # Parameters:
  #   - path (str)    : the calibration file
  #   - entry (str)   : which entry to replace
  #   - value         : the measured value
  #   - fields (list) : provenance fields, in the order they should be written.
  #                     Prose fields (target, note) should be wrapped in
  #                     calib_prose(). Pass invalidated_by and this function
  #                     hashes it for you.
  #   - tol (num)     : relative tolerance for calling the value reproduced.
  #                     0 means exactly.
  #
  # Returns: the path actually written, invisibly
  #----------------------------------------------------------------------------

  if (!file.exists(path)) {
    stop('No calibration file at ', path, '. This writer replaces one entry of ',
         'an existing file; it does not create the file, because the other ',
         'entries in it belong to other calibrators.')
  }

  # Hash the declared dependencies now, from the files as they are, and put the
  # hashes straight after the list they describe.
  if (!is.null(fields$invalidated_by)) {
    at = match('invalidated_by', names(fields))
    fields = append(fields,
                    list(invalidated_by_hashes = calib_hash_files(fields$invalidated_by)),
                    after = at)
  }

  text  = paste(readLines(path, warn = FALSE), collapse = '\n')
  parts = calib_split_blocks(text)
  i     = match(entry, parts$keys)
  if (is.na(i)) {
    stop('Calibration file ', path, ' has no entry `', entry, '`. It has: ',
         paste(parts$keys, collapse = ', '), '. A calibrator replaces an ',
         'existing entry rather than inventing one, so that the entry and its ',
         'readers are added in the same deliberate step.')
  }

  # The comments above an entry travel with it, so the generated block has to
  # carry them forward; the leading comment lines of the old block are kept.
  old_lines = strsplit(parts$blocks[[i]], '\n', fixed = TRUE)[[1]]
  lead      = old_lines[seq_len(match(TRUE, grepl('^[A-Za-z_]', old_lines)) - 1)]

  new_block = paste(c(lead, calib_render_entry(entry, value, fields)),
                    collapse = '\n')

  parts$blocks[[i]] = new_block
  out = paste(c(parts$preamble, unlist(parts$blocks)), collapse = '\n')

  # Did the re-run reproduce what is shipped?
  old = calib_current_value(path, entry)
  reproduced =
    !is.na(old) &&
    !is.na(suppressWarnings(as.numeric(old))) &&
    !is.na(suppressWarnings(as.numeric(value))) &&
    (if (tol == 0) identical(trimws(as.character(value)), trimws(old))
     else abs(as.numeric(value) - as.numeric(old)) <=
          tol * max(abs(as.numeric(old)), .Machine$double.eps))

  if (reproduced) {
    writeLines(out, path)
    cat(sprintf('\n%s: %s reproduced its pinned value (%s). Written in place.\n',
                basename(path), entry, old))
    return(invisible(path))
  }

  proposed = paste0(path, '.proposed')
  writeLines(out, proposed)

  cat(sprintf('\n%s\n', strrep('=', 78)))
  cat(sprintf('DRIFT -- %s.%s did NOT reproduce its pinned value.\n',
              basename(path), entry))
  cat(sprintf('  shipped  : %s\n', old))
  cat(sprintf('  measured : %s\n', as.character(value)))
  cat(sprintf('\nThe shipped file is UNCHANGED. The new entry is at:\n  %s\n', proposed))
  cat('\nThis is a finding, not a routine update: a calibrated value moving means\n')
  cat('the model moved, the data moved, or the calibration is less identified\n')
  cat('than it looks. Read the difference, decide, and if the drift is real:\n')
  cat(sprintf('  diff %s %s\n  mv %s %s\n', path, proposed, proposed, path))
  cat(sprintf('%s\n', strrep('=', 78)))

  invisible(proposed)
}
