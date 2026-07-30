#--------------------------------
# utils.R 
#
# Miscellaneous helper functions
#--------------------------------


remove_by_name = function(lst, name) {
  
  #----------------------------------------------------------------------------
  # In a copy of a given list, removes an element referenced by name
  # 
  # Parameters:
  #   - lst (list) : list from which to remove element
  #   - name (str) : name of element to remove
  #
  # Returns: list with element removed (list)
  #----------------------------------------------------------------------------
  
  return(lst[names(lst) != name])
}



replace_by_name = function(host, donor) {
  
  #----------------------------------------------------------------------------
  # Helper function to overwrite elements in a "host" list with identically 
  # names elements from a "donor" list. 
  #
  # Parameters:
  #   - host (list)  : named list for which to overwrite values 
  #   - donor (list) : names list containing values which will overwrite those
  #                    in host based on name index
  #
  # Returns: updated host list (list.)
  #----------------------------------------------------------------------------
  
  for (name in names(donor)) {
    host[[name]] = donor[[name]]
  }
  return(host)
}



get_vector = function(df, name) { 
  
  #----------------------------------------------------------------------------
  # Gets a single column from a dataframe or tibble and returns it as a 
  # vector. Designed for readability in a dplyr chain. 
  # 
  # Parameters:
  #   - df (df) : dataframe to get column from
  #   - name (str) : name of column
  #
  # Returns: atomic vector representation of column.
  #----------------------------------------------------------------------------
  
  df %>% 
    select(all_of(name)) %>% 
    deframe() %>% 
    return()
}



resolve_detail_purge = function(x) {

  #----------------------------------------------------------------------------
  # Reads the delete_detail argument into what it asks for, and when.
  #
  # Peak disk is set in the middle of a run, not at the end, so purging only at
  # the end cannot lower it. The two no-wealth trees are measurement passes: each
  # is read by the wealth recurrence that follows it, carries no totals, and is
  # the source of no distribution table except the payroll overlay a reform
  # changing employer payroll law takes off the mechanical one. Purging them the
  # moment their recurrence has read them takes a scenario from five detail trees
  # on disk to three.
  #
  # Accepts 'none' (or 0), 'all' (or 1), 'transient', or a comma-separated
  # combination, so 'transient,all' gives the lowest peak and the lowest final.
  #
  # Note that the eager purge is opt-in for a reason. The gains elasticity
  # harness reads a scenario's conventional_no_wealth detail after the run has
  # finished, so a calibration run has to keep it.
  #
  # Parameters:
  #   - x (str) : the delete_detail argument
  #
  # Returns: list of eager (purge each measurement tree once consumed) and final
  #          (purge every tree after post-processing).
  #----------------------------------------------------------------------------

  parts = x %>%
    as.character() %>%
    strsplit(',', fixed = TRUE) %>%
    unlist() %>%
    trimws()
  parts = parts[nzchar(parts)]

  parts[parts == '0'] = 'none'
  parts[parts == '1'] = 'all'

  known = c('none', 'all', 'transient')
  bad   = setdiff(parts, known)
  if (length(parts) == 0 || length(bad) > 0) {
    stop('delete_detail must be one of ', paste(known, collapse = ', '),
         ' (0 for none and 1 for all are also accepted), or a comma-separated ',
         'combination such as "transient,all". Got "', x, '".')
  }
  if ('none' %in% parts && length(parts) > 1) {
    stop('delete_detail "none" cannot be combined with ',
         paste(setdiff(parts, 'none'), collapse = ', '), '.')
  }

  list(eager = 'transient' %in% parts,
       final = 'all'       %in% parts)
}



purge_detail = function(passes = NULL, scenarios = NULL) {

  #----------------------------------------------------------------------------
  # Deletes output stored in /detail, which contains tax unit microdata detail
  # files.
  #
  # Every pass writes a detail tree, so the default set is read from PASS_SPECS
  # rather than listed here: a run that leaves the no-wealth or mechanical trees
  # behind keeps most of its detail on disk, which is what this is called to
  # reclaim.
  #
  # Scope the scenarios whenever the caller is one scenario's pass rather than
  # the end of the run. The default is every scenario in the runscript, which is
  # right for the end-of-run purge and wrong for anything running concurrently:
  # an unscoped call from inside a per-scenario phase deletes the detail that the
  # other scenarios' tasks are still reading.
  #
  # Parameters:
  #   - passes (str[])    : pass roots to purge, defaulting to every one
  #   - scenarios (str[]) : scenario IDs to purge, defaulting to all of them
  #
  # Returns: void
  #----------------------------------------------------------------------------

  if (is.null(passes)) {
    passes = c('static', map_chr(PASS_SPECS, 'root'))
  }
  if (is.null(scenarios)) {
    scenarios = globals$runscript$ID
  }

  for (scenario_id in scenarios) {
    for (pass in passes) {
      unlink(
        file.path(globals$output_root,
                  scenario_id,
                  pass,
                  "detail/*"
        )
      )
    }
  }
}


  
knit_series = function(counterfactual_ids, path) {
  
  #----------------------------------------------------------------------------
  # Reads in all variations of an output file across scenarios and combines them
  # into a single file.
  # 
  # Parameters: 
  #   - counterfactual_ids (str) : Non baseline scenarios
  #   - path (file.path) : Path from scenario's main directory in output to the
  #                        file to be knit.
  #
  # Returns: (df) combined files
  #----------------------------------------------------------------------------
  
  c('baseline', counterfactual_ids) %>%
    map(.f = ~ file.path(globals$output_root, .x, path) %>%
          fread() %>%
          tibble()) %>%
    bind_rows() %>%
    return()
}


