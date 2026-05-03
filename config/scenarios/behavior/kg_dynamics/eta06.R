do_kg_dynamics = function(tax_units, baseline_mtrs, static_mtrs,
                          scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Thin applier: reads the precomputed bathtub state for this year, attaches
  # per-record cohort attributes, and applies the rate / carryover / deemed
  # channels to records via kg_dyn_apply_to_records().
  #
  # The bathtub recurrence itself runs in run_bathtub_pass() (src/sim/run.R)
  # for main.R sequential mode and in src/slurm/bathtub.R for the SLURM
  # pipeline. Either path produces per-year state files at:
  #
  #   {scenario_output}/conventional/supplemental/kg_dynamics_state/{year}.rds
  #
  # Each state file is list(delta_prev, r_S, regime, baseline_t).
  #
  # NOT compatible with the legacy kg/*.R or carryover_basis/*.R modules
  # (they apply overlapping rate-elasticity / regime adjustments to kg_lt).
  # Combine only with non-kg behavior modules.
  #
  # Parameters:
  #   - tax_units (df)        : tibble of tax units with calculated variables.
  #                             Must contain value.*/basis.* (five wealth
  #                             classes), q_death1, q_death2, age1, age2,
  #                             filing_status, weight, kg_lt.
  #   - baseline_mtrs (df)    : unused in v1 (single-tau approximation)
  #   - static_mtrs (df)      : unused in v1
  #   - scenario_info (list)  : output of get_scenario_info(); supplies
  #                             output_path
  #   - indexes (df)          : unused
  #
  # Returns: tibble of tax units with adjusted kg_lt and added decedent_flag.
  #----------------------------------------------------------------------------

  year       = tax_units$year[1]
  state_path = file.path(scenario_info$output_path,
                          'conventional', 'supplemental',
                          'kg_dynamics_state',
                          paste0(year, '.rds'))

  if (!file.exists(state_path)) {
    stop("kg_dynamics: missing precomputed state file at ", state_path,
         "\nThe bathtub pre-pass must run before the conventional pass. ",
         "In main.R sequential mode this happens automatically inside ",
         "do_scenario(); in SLURM mode it's Phase 2B (src/slurm/bathtub.R).")
  }

  state = readRDS(state_path)

  tax_units = kg_dyn_attach_record_attrs(tax_units)

  kg_dyn_apply_to_records(
    tax_units        = tax_units,
    baseline_cells_t = state$baseline_t,
    r_S              = state$r_S,
    delta_prev       = state$delta_prev,
    regime           = state$regime,
    decedent_random  = tax_units$r.behavior1
  )
}
