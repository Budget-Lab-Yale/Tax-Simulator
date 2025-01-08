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



purge_detail = function() {
  
  #----------------------------------------------------------------------------
  # Deletes all output stored in /detail, which contains tax unit microdata 
  # detail files.
  # 
  # Parameters: none
  #
  # Returns: void
  #----------------------------------------------------------------------------
  
  for (scenario_id in globals$runtime_args$ID) {
    for (behavior in c("static", "conventional")) {
      unlink(
        file.path(globals$output_root,
                  scenario_id,
                  behavior,
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


