#-------------------------------------------------------------------------------
# state.R
#
# State-file paths, scenario activation, and per-year regime resolution.
#-------------------------------------------------------------------------------


# Conventional bathtub state: thin wrappers over the shared cohort-state IO
# helpers (cohort_bathtub.R) with kg's subdir/pass fixed. Path is
# {output_path}/conventional/supplemental/kg_dynamics_state/{year}.rds.
kg_dyn_state_dir = function(scenario_info) {
  cohort_state_dir(scenario_info, 'kg_dynamics_state', 'conventional')
}

kg_dyn_state_path = function(scenario_info, year) {
  cohort_state_path(scenario_info, 'kg_dynamics_state', year, 'conventional')
}

# Mechanical (frozen-realization) state: consumed by the STATIC pass, so it
# lives under static/supplemental. One {year}.rds per year, same
# list(regime, cell_table) contract as the conventional bathtub state, plus
# inputs_cache.rds (baseline cells + slim per-record frames) reused by the
# full bathtub pass to avoid a second Tax-Data sweep.
kg_dyn_mech_state_dir = function(scenario_info) {
  cohort_state_dir(scenario_info, 'kg_dynamics_mech_state', 'static')
}

kg_dyn_mech_state_path = function(scenario_info, year) {
  cohort_state_path(scenario_info, 'kg_dynamics_mech_state', year, 'static')
}

kg_dyn_inputs_cache_path = function(scenario_info) {
  file.path(kg_dyn_mech_state_dir(scenario_info), 'inputs_cache.rds')
}

# Does this scenario's behavior set include any kg_dynamics module?
scenario_uses_kg_dynamics = function(scenario_info) {
  any(startsWith(scenario_info$behavior_modules %||% character(),
                 'kg_dynamics/'))
}



#-------------------------------------------------------------------------------
# Per-year regime resolution (shared by the conventional bathtub pass and the
# mechanical frozen pass)
#-------------------------------------------------------------------------------

kg_dyn_resolve_year_regime = function(tax_law, year, baseline_t,
                                       ages_bathtub = KG_DYN_AGE_MIN:
                                                      KG_DYN_AGE_MAX) {

  # Extracts the year's per-asset death-regime codes, bequest motive theta,
  # and §121 caps from the joined tax law, validates them, and builds the
  # cell-level regime mix. Returns list(regime, mix) where regime is the
  # metadata object persisted in state files (codes, theta, sec121 caps,
  # per-asset realize indicators) and mix is the output of
  # kg_dyn_build_regime_mix on the bathtub grid.

  tlt = tax_law %>% filter(year == !!year)
  if (nrow(tlt) == 0) {
    stop('kg_dynamics: tax_law has no rows for year ', year)
  }
  tlt_row = tlt %>% slice(1)

  regime_codes = list(
    equities      = as.numeric(tlt_row$`pref.kg_death_regime_equities`),
    pass_throughs = as.numeric(tlt_row$`pref.kg_death_regime_pass_throughs`),
    primary_home  = as.numeric(tlt_row$`pref.kg_death_regime_primary_home`),
    other_home    = as.numeric(tlt_row$`pref.kg_death_regime_other_home`),
    re_fund       = as.numeric(tlt_row$`pref.kg_death_regime_re_fund`)
  )
  theta = as.numeric(tlt_row$`pref.kg_bequest_motive`)
  if (length(theta) != 1 || !is.finite(theta) || theta < 0 || theta > 1) {
    stop(sprintf(
      paste0('kg_dynamics: pref.kg_bequest_motive must be a finite ',
             'scalar in [0, 1]; got %s for year %d. theta drives c_phi ',
             'under carryover and feeds the Bellman; out-of-range or NA ',
             'values silently produce nonsensical W/MC/kappa.'),
      format(theta), year))
  }

  sec121_by_fs = tlt %>%
    select(filing_status, `pref.kg_sec121_excl`) %>%
    distinct()
  sec121_single = sec121_by_fs %>%
    filter(filing_status == 1) %>% pull(`pref.kg_sec121_excl`)
  sec121_married = sec121_by_fs %>%
    filter(filing_status == 2) %>% pull(`pref.kg_sec121_excl`)
  if (length(sec121_single)  == 0) sec121_single  = NA_real_
  if (length(sec121_married) == 0) sec121_married = NA_real_

  mix = kg_dyn_build_regime_mix(regime_codes, theta, baseline_t, ages_bathtub)

  # Per-asset realize indicators (year-level scalars from the regime codes)
  realize_by_asset = lapply(KG_DYN_ASSET_CLASSES, function(k) {
    KG_DYN_REGIME_TRIPLET[[as.character(regime_codes[[k]])]]$realize
  })
  names(realize_by_asset) = KG_DYN_ASSET_CLASSES

  list(
    regime = list(
      codes               = regime_codes,
      theta               = theta,
      sec121_excl_single  = sec121_single[1],
      sec121_excl_married = sec121_married[1],
      realize             = realize_by_asset
    ),
    mix = mix
  )
}



