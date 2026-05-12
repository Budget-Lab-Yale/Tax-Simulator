do_kg_dynamics = function(tax_units, baseline_mtrs, static_mtrs,
                          scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Pure allocator: reads the precomputed bathtub state for this year and
  # applies the cell-level rate / lock-in / deemed quantities to records via
  # kg_dyn_apply_to_records().
  #
  # The bathtub pre-pass runs in run_bathtub_pass() (src/sim/run.R) for
  # main.R sequential mode and in src/slurm/bathtub.R for the SLURM pipeline.
  # That pre-pass solves the representative-cell Bellman backward induction
  # on the extended age grid [18, 119] to produce a scenario realization
  # rate r_D_S(a, t), then runs the bathtub recurrence
  # (kg_dyn_step_recurrence) on the bathtub grid [18, 80] for dG evolution
  # and channel decomposition. Per-year state files are at:
  #
  #   {scenario_output}/conventional/supplemental/kg_dynamics_state/{year}.rds
  #
  # Each state file is list(regime, cell_table). The cell_table carries the
  # Bellman diagnostics (W_B, W_S, MC_B, MC_S, kappa, r_D_B, r_D_S) plus
  # the two-channel decomposition (lambda_I, r_V_B, r_V_S) and the applier
  # inputs (rate_factor, extra_R, deemed_factor). All math is in the bathtub;
  # this module does no recurrence work.
  #
  # NOT compatible with the legacy kg/*.R or carryover_basis/*.R modules.
  #
  # Returns: tibble of tax units with adjusted kg_lt and added decedent_flag.
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

  tax_units = kg_dyn_attach_record_attrs(tax_units)

  kg_dyn_apply_to_records(
    tax_units       = tax_units,
    cell_table      = state$cell_table,
    delta_realize   = state$regime$delta_realize,
    decedent_random = tax_units$r.behavior1
  )
}
