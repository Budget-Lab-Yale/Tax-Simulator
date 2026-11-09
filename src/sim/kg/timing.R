#-------------------------------------------------------------------------------
# timing.R
#
# Contains functions to retime realizations across nearby years
#-------------------------------------------------------------------------------


kg_dyn_build_planned_timing = function(baseline_cells, tau_S_mat, years,
                                       tau_B_mat = NULL,
                                       timeable_share = kg_dyn_active_timeable_share(),
                                       timing_window = kg_setting('timing_window'),
                                       ref_wedge     = kg_setting('timing_ref_wedge'),
                                       ages_bathtub = KG_DYN_AGE_MIN:
                                                      KG_DYN_AGE_MAX,
                                       tie_tol = 1e-12) {

  # Moves retimeable realizations toward whichever nearby year is cheapest.
  #
  # For each age and year, the retimeable share of that year's baseline
  # realizations compares the rate change across the window of years on either
  # side and looks for the lowest. Ties go to the nearest year, and then to the
  # earlier one. The share that actually moves is the rate saving over the
  # reference wedge, capped at 1; the rest stays put.
  #
  # The comparison is on the change in the rate rather than its level, so a
  # baseline run retimes nothing. Callers pass rates measured on law alone, which
  # makes the change exactly zero when the schedule faced during life is
  # unchanged.
  #
  # Returns: list of baseline and scenario retimeable realizations, and the
  #          difference between them.

  kg_dyn_validate_timing_params(timeable_share = timeable_share,
                                timing_window  = timing_window,
                                ref_wedge      = ref_wedge)

  ages_chr  = as.character(ages_bathtub)
  years_chr = as.character(years)
  n_ages    = length(ages_bathtub)
  n_years   = length(years)
  H         = as.integer(timing_window)

  R_B = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  for (t_chr in years_chr) {
    bt = baseline_cells[[t_chr]]
    R_B[, t_chr] = bt$R_B[match(ages_bathtub, bt$age)]
  }

  R_planned_B = timeable_share * R_B
  R_planned_S = matrix(0, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  timing_tau_mat = if (is.null(tau_B_mat)) tau_S_mat else tau_S_mat - tau_B_mat
  tau_bt = timing_tau_mat[ages_chr, years_chr, drop = FALSE]

  if (timeable_share == 0 || H == 0) {
    R_planned_S = R_planned_B
  } else {
    for (i in seq_len(n_ages)) {
      for (j in seq_len(n_years)) {
        amount = R_planned_B[i, j]
        if (amount == 0) next

        eligible = max(1, j - H):min(n_years, j + H)
        tau_vals = tau_bt[i, eligible]
        min_tau  = min(tau_vals, na.rm = TRUE)

        if (tau_bt[i, j] <= min_tau + tie_tol) {
          R_planned_S[i, j] = R_planned_S[i, j] + amount
        } else {
          candidates = eligible[tau_vals <= min_tau + tie_tol]
          distances  = abs(candidates - j)
          nearest    = candidates[distances == min(distances)]
          dest       = min(nearest)

          tax_saving = tau_bt[i, j] - tau_bt[i, dest]
          move_share = min(max(tax_saving / ref_wedge, 0), 1)
          moved      = amount * move_share

          R_planned_S[i, dest] = R_planned_S[i, dest] + moved
          R_planned_S[i, j]    = R_planned_S[i, j]    + (amount - moved)
        }
      }
    }
  }

  list(R_planned_B = R_planned_B,
       R_planned_S = R_planned_S,
       planned_timing_shift = R_planned_S - R_planned_B)
}



kg_dyn_build_scenario_rate = function(baseline_t, r_ordinary_S,
                                      R_planned_B_col, R_planned_S_col) {

  # Combines the long-run response with the retiming to give the scenario's
  # realization rate for one year.
  #
  # Returns: list of the rate, its unclipped value, which cells were clipped, and
  #          the retimed and long-run pieces it was built from.

  G_B = baseline_t$G_B
  r_B = baseline_t$r_B

  r_planned_B  = ifelse(G_B > 0, R_planned_B_col / G_B, 0)
  r_planned_S  = ifelse(G_B > 0, R_planned_S_col / G_B, 0)
  r_ordinary_B = r_B

  # The long-run response plus the net retiming. Under a permanent uniform rate
  # change no year is cheaper than another, so the retiming nets to zero and the
  # long-run response stands alone. At the baseline both pieces cancel and the
  # rate is the baseline rate.
  r_S_unclipped = r_ordinary_S + (r_planned_S - r_planned_B)
  r_S           = pmin(pmax(r_S_unclipped, 0), 1)

  list(r_S            = r_S,
       r_S_unclipped  = r_S_unclipped,
       timing_clipped = abs(r_S - r_S_unclipped) > 1e-12,
       r_planned_B    = r_planned_B,
       r_planned_S    = r_planned_S,
       r_ordinary_B   = r_ordinary_B,
       r_ordinary_S   = r_ordinary_S)
}



