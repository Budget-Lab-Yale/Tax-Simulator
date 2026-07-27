do_kg_dynamics = function(tax_units, baseline_mtrs, static_mtrs,
                          scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Reads the year's state file and applies the cell results to records. All of the
  # work is done in the pre-pass; this module only allocates.
  #
  # The pre-pass runs in run_bathtub_pass() in sequential mode and in
  # src/slurm/bathtub.R under SLURM. It solves each cell's realization choice, steps
  # the stock of unrealized gains forward, and writes one file per year to the
  # scenario's conventional supplemental folder. Each file carries the year's
  # treatment of gains at death and a table of cells, holding both the three
  # quantities the allocation needs and the diagnostics behind them.
  #
  # Not compatible with the simple elasticity modules under src/behavior/kg/.
  #
  # Returns: tax units with kg_lt adjusted. Gains at death enter as an expectation
  #          weighted by mortality, not by drawing decedents (df).
  #----------------------------------------------------------------------------

  year       = tax_units$year[1]
  state_path = kg_dyn_state_path(scenario_info, year)

  if (!file.exists(state_path)) {
    stop("kg_dynamics: missing precomputed state file at ", state_path,
         "\nThe bathtub pre-pass must run before the conventional pass. ",
         "In main.R sequential mode this happens automatically inside ",
         "do_scenario(); in SLURM mode it's Phase 2B (src/slurm/bathtub.R).")
  }

  state = readRDS(state_path)

  cpiu_by_year = kg_dyn_load_cpiu_levels(
    scenario_info$interface_paths$`Macro-Projections`,
    years = year
  )
  tax_units = kg_dyn_attach_record_attrs(tax_units,
                                         cpiu_by_year = cpiu_by_year)

  kg_dyn_apply_to_records(
    tax_units        = tax_units,
    cell_table       = state$cell_table,
    realize_by_asset = state$regime$realize
  )
}
