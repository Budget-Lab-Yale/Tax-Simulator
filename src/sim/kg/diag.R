#-------------------------------------------------------------------------------
# diag.R
#
# Contains functions to write diagnostics on the gains model
#-------------------------------------------------------------------------------


kg_dyn_write_estate_exposure_diag = function(baseline_joined, reform_joined,
                                              scenario_info, year) {

  # Writes one file per year showing how estate exposure is spread across
  # records, by leg and by decile of gains held, with a summary row per leg. The
  # cell averages the model uses compress a skewed distribution, and this makes
  # that visible.
  #
  # Returns: invisible NULL (writes files as a side effect)

  leg_diag = function(joined, leg) {
    d = joined %>%
      filter(pmax(G_unit, 0) > 0) %>%
      mutate(e_rec = pmin(pmax(coalesce(mtr_estate_ded, 0), 0), 1),
             gw    = weight * G_unit)
    if (nrow(d) == 0) return(NULL)

    # Deciles of gains held, cut on cumulative population weight
    d = d %>%
      arrange(G_unit) %>%
      mutate(decile = pmin(floor(cumsum(weight) / sum(weight) * 10) + 1, 10))

    by_decile = d %>%
      group_by(decile) %>%
      summarise(e_gw_mean    = sum(gw * e_rec) / pmax(sum(gw), 1e-12),
                gain_dollars = sum(gw),
                n_records    = n(),
                .groups = 'drop') %>%
      mutate(group = paste0('gain_decile_', decile)) %>%
      select(-decile)

    all_row = d %>%
      summarise(
        e_gw_mean    = sum(gw * e_rec) / pmax(sum(gw), 1e-12),
        gain_dollars = sum(gw),
        n_records    = n(),
        share_zero   = sum(gw * (e_rec < 1e-6)) / pmax(sum(gw), 1e-12),
        share_top    = sum(gw * (e_rec >= 0.35)) / pmax(sum(gw), 1e-12),
        e_gw_age80   = {
          gw80 = gw * (age_cohort >= 80)
          if (sum(gw80) > 0) sum(gw80 * e_rec) / sum(gw80) else 0
        }) %>%
      mutate(group = 'all')

    bind_rows(by_decile, all_row) %>%
      mutate(leg = leg, year = year) %>%
      relocate(year, leg, group)
  }

  diag = bind_rows(leg_diag(baseline_joined, 'baseline'),
                   leg_diag(reform_joined,   'reform'))
  if (is.null(diag) || nrow(diag) == 0) return(invisible(NULL))

  out_dir = file.path(scenario_info$output_path, 'conventional',
                      'supplemental')
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(diag, file.path(out_dir,
                            paste0('kg_estate_exposure_diag_', year, '.csv')))
  invisible(NULL)
}



kg_dyn_wealth_law_active = function(tax_law) {

  # Reports whether the scenario's tax law levies a wealth tax in any year. Asking
  # about any year rather than this one keeps the detail file columns the same
  # across a phase-in. The baseline has a single zero bracket, so every scenario
  # without a wealth tax returns FALSE.
  #
  # Returns: TRUE if the scenario has a wealth tax (bool).

  rate_cols = grep('^wealth\\.rates[0-9]*$', names(tax_law), value = TRUE)
  if (length(rate_cols) == 0) return(FALSE)
  any(sapply(tax_law[rate_cols], function(x) any(!is.na(x) & x != 0)))
}



#-------------------------------------------------------------------------------
# Summarizing the pass
#-------------------------------------------------------------------------------

kg_dyn_build_summary = function(scenario_info) {

  # Reads every year's state file and writes two summaries: one row per year and
  # age, and a rollup by year carrying the weighted averages, the decomposition
  # into channels, and the elasticity the run implies. Does nothing for a scenario
  # that never ran the pass.
  #
  # Returns: invisible NULL (writes files as a side effect)

  state_dir = kg_dyn_state_dir(scenario_info)
  if (!dir.exists(state_dir)) return(invisible(NULL))

  years = scenario_info$years
  state_files = file.path(state_dir, paste0(years, '.rds'))
  if (!all(file.exists(state_files))) return(invisible(NULL))

  states = lapply(years, function(t) readRDS(file.path(state_dir, paste0(t, '.rds'))))
  names(states) = as.character(years)

  # Stamp the year's treatment of gains at death onto every row, which is
  # redundant but convenient to read.
  age_profile = bind_rows(lapply(seq_along(years), function(i) {
    s = states[[i]]
    codes = s$regime$codes
    s$cell_table %>%
      mutate(year                          = years[i],
             regime_equities               = codes$equities,
             regime_pass_throughs          = codes$pass_throughs,
             regime_primary_home           = codes$primary_home,
             regime_other_home             = codes$other_home,
             regime_re_fund                = codes$re_fund,
             theta                         = s$regime$theta,
             sec121_excl_single            = s$regime$sec121_excl_single,
             sec121_excl_married           = s$regime$sec121_excl_married) %>%
      relocate(year, age)
  }))

  age_profile %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_age_profile.csv'))

  # The year's treatment of gains at death, by asset class
  regime_df = bind_rows(lapply(seq_along(years), function(i) {
    r = states[[i]]$regime
    tibble(year                 = years[i],
           regime_equities      = r$codes$equities,
           regime_pass_throughs = r$codes$pass_throughs,
           regime_primary_home  = r$codes$primary_home,
           regime_other_home    = r$codes$other_home,
           regime_re_fund       = r$codes$re_fund,
           theta                = r$theta,
           sec121_excl_single   = r$sec121_excl_single,
           sec121_excl_married  = r$sec121_excl_married)
  }))

  # Weighted averages, defaulting to zero for the realization rates and NA for
  # everything else where the weights sum to zero.
  wmean = function(x, w, default = NA_real_) {
    s = sum(w)
    if (s > 0) sum(x * w) / s else default
  }

  yearly = age_profile %>%
    group_by(year) %>%
    summarise(
      G_B_total           = sum(G_B),
      R_B_total           = sum(R_B),
      dG_total            = sum(dG),
      G_B_equities_total          = sum(G_B_equities),
      G_B_pass_throughs_total     = sum(G_B_pass_throughs),
      G_B_primary_home_total      = sum(G_B_primary_home),
      G_B_other_home_total        = sum(G_B_other_home),
      G_B_re_fund_total           = sum(G_B_re_fund),
      G_B_primary_above_cap_total = sum(G_B_primary_above_cap),
      m_avg_gw            = wmean(m,            G_B),
      estate_2026_m_avg_dgw =
        wmean(estate_2026_m_avg_dgw, mG_record),
      p_char_extensive_avg_dgw = wmean(p_char_extensive, mG_record),
      p_char_intensive_avg_dgw = wmean(p_char_intensive, mG_record),
      p_char_avg_dgw      = wmean(p_char,       mG_record, default = 0),
      r_B_avg_gw          = wmean(r_B,          G_B, default = 0),
      r_S_avg_gw          = wmean(r_S,          G_B, default = 0),
      c_phi_avg_gw        = wmean(c_phi,         G_B, default = 0),
      delta_vanish_avg_gw  = wmean(delta_vanish,  G_B, default = 0),
      delta_route_avg_gw   = wmean(delta_route,   G_B, default = 0),
      delta_realize_avg_gw = wmean(delta_realize, G_B, default = 0),
      r_planned_B_avg_gw  = wmean(r_planned_B,  G_B),
      r_planned_S_avg_gw  = wmean(r_planned_S,  G_B),
      r_ordinary_B_avg_gw = wmean(r_ordinary_B, G_B),
      r_ordinary_S_avg_gw = wmean(r_ordinary_S, G_B),
      tau_B_avg_gw        = wmean(tau_B,        G_B),
      tau_S_avg_gw        = wmean(tau_S,        G_B),
      tau_B_avg_rw        = wmean(tau_B,        R_B),
      tau_S_avg_rw        = wmean(tau_S,        R_B),
      carry_h_avg_gw      = wmean(carry_h,      G_B, default = 0),
      W_B_avg_gw          = wmean(W_B,          G_B),
      W_S_avg_gw          = wmean(W_S,          G_B),
      MC_B_avg_gw         = wmean(MC_B,         G_B),
      MC_S_avg_gw         = wmean(MC_S,         G_B),
      kappa_avg_gw        = wmean(kappa,        G_B),
      rate_channel    = sum(R_B * (rate_factor - 1)),
      lockin_channel  = sum(extra_R),
      R_planned_B_total = sum(R_planned_B),
      R_planned_S_total = sum(R_planned_S),
      planned_timing_shift_total = sum(planned_timing_shift),
      timing_clipped_cells = sum(timing_clipped, na.rm = TRUE),
      inheritance_flow    = sum(delta_route   * taxable_death_stock),
      deemed_realized     = sum(delta_realize * taxable_death_stock),
      taxable_deemed_stock = sum(delta_realize * taxable_death_stock),
      decedent_stock      = sum(decedent_stock),
      terminal_char_stock = sum(terminal_char_stock),
      taxable_death_stock = sum(taxable_death_stock),
      .groups = 'drop'
    ) %>%
    left_join(regime_df, by = 'year') %>%
    mutate(
      R_S_total          = R_B_total + rate_channel + lockin_channel +
                           deemed_realized,
      dtau               = tau_S_avg_rw - tau_B_avg_rw,
      semi_elast_implied = if_else(R_B_total > 0 & R_S_total > 0 &
                                     abs(dtau) > 1e-10,
                                   log(R_S_total / R_B_total) / dtau,
                                   NA_real_)
    ) %>%
    select(year,
           regime_equities, regime_pass_throughs, regime_primary_home,
           regime_other_home, regime_re_fund,
           theta, sec121_excl_single, sec121_excl_married,
           c_phi_avg_gw,
           estate_2026_m_avg_dgw,
           p_char_extensive_avg_dgw, p_char_intensive_avg_dgw,
           p_char_avg_dgw,
           delta_vanish_avg_gw, delta_route_avg_gw, delta_realize_avg_gw,
           G_B_total, R_B_total, R_S_total, dG_total,
           G_B_equities_total, G_B_pass_throughs_total,
           G_B_primary_home_total, G_B_other_home_total,
           G_B_re_fund_total, G_B_primary_above_cap_total,
           m_avg_gw, r_B_avg_gw, r_S_avg_gw,
           r_planned_B_avg_gw, r_planned_S_avg_gw,
           r_ordinary_B_avg_gw, r_ordinary_S_avg_gw,
           tau_B_avg_gw, tau_S_avg_gw, tau_B_avg_rw, tau_S_avg_rw,
           carry_h_avg_gw,
           W_B_avg_gw, W_S_avg_gw, MC_B_avg_gw, MC_S_avg_gw, kappa_avg_gw,
           rate_channel, lockin_channel,
           R_planned_B_total, R_planned_S_total,
           planned_timing_shift_total, timing_clipped_cells,
           decedent_stock, terminal_char_stock, taxable_death_stock,
           inheritance_flow, deemed_realized, taxable_deemed_stock,
           semi_elast_implied)

  yearly %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_summary.csv'))

  invisible(NULL)
}
