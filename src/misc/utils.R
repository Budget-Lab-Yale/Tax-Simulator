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
  for(behavior in c("static", "conventional")){
    unlink(
      file.path(globals$output_root,
                scenario_id,
                behavior,
                "detail/*"
      )
    )
  }
}

  
  
  
