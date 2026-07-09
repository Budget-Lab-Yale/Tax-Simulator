#-------------------------------------------------------------------------
# helpers.R
#
# Shared data-layer readers and grouping helpers used by economy.R and the
# post-processing scripts (distribution, revenue, 1040, time burden,
# horizontal)
#-------------------------------------------------------------------------



read_static_detail = function(id, yr) {

  #----------------------------------------------------------------------------
  # Reads a scenario's static tax unit detail file for a given year.
  #
  # Parameters:
  #   - id (str) : scenario ID ('baseline' resolves to the baseline root)
  #   - yr (int) : year of detail file
  #
  # Returns: tibble of tax unit detail (df).
  #----------------------------------------------------------------------------

  root = if (id == 'baseline') globals$baseline_root else globals$output_root
  root %>%
    file.path(id, 'static/detail', paste0(yr, '.csv')) %>%
    fread() %>%
    tibble() %>%
    return()
}



read_vat_offset = function(id) {

  #----------------------------------------------------------------------------
  # Reads a scenario's static VAT price offset series.
  #
  # Parameters:
  #   - id (str) : scenario ID
  #
  # Returns: tibble of price level adjustment factors by year (df).
  #----------------------------------------------------------------------------

  globals$output_root %>%
    file.path(id, 'static/supplemental/vat_price_offset.csv') %>%
    read_csv(show_col_types = F) %>%
    return()
}



read_macro_spliced = function(root) {

  #----------------------------------------------------------------------------
  # Reads and row-binds the historical and projected series of a Macro-
  # Projections-style interface.
  #
  # Parameters:
  #   - root (str) : interface root containing historical.csv/projections.csv
  #
  # Returns: tibble of historical rows followed by projected rows (df).
  #----------------------------------------------------------------------------

  c('historical.csv', 'projections.csv') %>%
    map(.f = ~ read_csv(file.path(root, .x), show_col_types = F)) %>%
    bind_rows() %>%
    return()
}



interface_root = function(interface_name, scenario_id = globals$interface_paths$ID[1]) {

  #----------------------------------------------------------------------------
  # Returns the path of a dependency-interface for a given scenario row of
  # globals$interface_paths. Defaults to the first row's ID (the baseline
  # scenario's interface versions).
  #
  # Parameters:
  #   - interface_name (str) : dependent model name (e.g. 'Macro-Projections')
  #   - scenario_id (str)    : scenario ID row to look up
  #
  # Returns: interface path (str).
  #----------------------------------------------------------------------------

  globals$interface_paths %>%
    filter(ID == scenario_id, interface == interface_name) %>%
    pull(path)
}



add_rank_groups = function(df, rank_var, pctile_col, prefix = '') {

  #----------------------------------------------------------------------------
  # Adds percentile-rank grouping columns for a ranking variable: a percentile,
  # quintile labels, and top 10/5/1/0.1% cuts. Only the nonnegative population
  # is ranked -- negative values get NA rank (callers label that group, e.g.
  # 'Negative income'). When df is grouped, ranks are computed within each
  # existing group.
  #
  # Parameters:
  #   - df (df)          : microdata with `weight` and the ranking variable
  #   - rank_var (str)   : name of the variable to rank on
  #   - pctile_col (str) : name for the new percentile column
  #   - prefix (str)     : prefix for the quintile/top_* column names
  #
  # Returns: df arranged by rank_var (within groups) with rank columns
  #          appended (df).
  #----------------------------------------------------------------------------

  df %>%
    arrange(.data[[rank_var]], .by_group = T) %>%
    mutate(

      # Percentile among the nonnegative population
      .rank_val = .data[[rank_var]],
      .pctile   = cumsum(weight * (.rank_val >= 0)) / sum(weight * (.rank_val >= 0)),
      .pctile   = if_else(.rank_val < 0, NA, .pctile),

      # Quintiles and top shares
      "{pctile_col}"     := .pctile,
      "{prefix}quintile" := case_when(
        .pctile <= 0.2 ~ 'Quintile 1',
        .pctile <= 0.4 ~ 'Quintile 2',
        .pctile <= 0.6 ~ 'Quintile 3',
        .pctile <= 0.8 ~ 'Quintile 4',
        .pctile <= 1   ~ 'Quintile 5',
      ),
      "{prefix}top_10" := if_else(.pctile > 0.9,   'Top 10%',   NA),
      "{prefix}top_5"  := if_else(.pctile > 0.95,  'Top 5%',    NA),
      "{prefix}top_1"  := if_else(.pctile > 0.99,  'Top 1%',    NA),
      "{prefix}top_01"  := if_else(.pctile > 0.999,  'Top 0.1%',  NA),
      "{prefix}top_001" := if_else(.pctile > 0.9999, 'Top 0.01%', NA)
    ) %>%
    select(-.rank_val, -.pctile)
}
