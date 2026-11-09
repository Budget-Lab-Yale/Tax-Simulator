#-------------------------------------------------------------------------------
# state.R
#
# Contains functions to locate state files and resolve the year's tax treatment
# of gains at death
#-------------------------------------------------------------------------------


# State written by the conventional pass, under the scenario's conventional
# supplemental folder, one file per year. These wrap the shared helpers in
# cohort_bathtub.R.
kg_dyn_state_dir = function(scenario_info) {
  cohort_state_dir(scenario_info, 'kg_dynamics_state', 'conventional')
}

kg_dyn_state_path = function(scenario_info, year) {
  cohort_state_path(scenario_info, 'kg_dynamics_state', year, 'conventional')
}

# State written by the frozen pass, which the static pass reads, so it lives
# under static instead. Same contents as above, plus a cache of the baseline
# cells and record frames that the conventional pass reuses rather than reading
# Tax-Data a second time.
kg_dyn_mech_state_dir = function(scenario_info) {
  cohort_state_dir(scenario_info, 'kg_dynamics_mech_state', 'static')
}

kg_dyn_mech_state_path = function(scenario_info, year) {
  cohort_state_path(scenario_info, 'kg_dynamics_mech_state', year, 'static')
}

kg_dyn_inputs_cache_path = function(scenario_info) {
  file.path(kg_dyn_mech_state_dir(scenario_info), 'inputs_cache.rds')
}

# Reports whether a scenario runs the gains model. Reads the behavior leg's
# kg_dynamics section rather than looking for a module name in a list, since the
# loader adds the module itself once the machinery is bound.
scenario_uses_kg_dynamics = function(scenario_info) {
  length(scenario_info$resolved_behavior$spec$kg_pieces %||% character()) > 0
}



#-------------------------------------------------------------------------------
# Tax treatment of gains at death, by year
#-------------------------------------------------------------------------------

kg_dyn_resolve_year_regime = function(tax_law, year, baseline_t,
                                       ages_bathtub = KG_DYN_AGE_MIN:
                                                      KG_DYN_AGE_MAX) {

  # Reads the year's treatment of gains at death out of tax law: the treatment by
  # asset class, the weight the holder puts on the heir's tax bill, the §121
  # exclusion caps, and the death-gain exclusion. Checks them and averages the
  # treatment to the cell.
  #
  # Returns: list of the year's treatment, which is written into the state files,
  #          and the cell-level mix of it.

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
             'scalar in [0, 1]; got %s for year %d. It sets how much of the ',
             "heir's tax bill the holder counts under carryover, and an ",
             'out-of-range value gives a meaningless realization choice.'),
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

  # The death-gain exclusion, per filing status, on top of §121
  death_excl_by_fs = tlt %>%
    select(filing_status, `pref.kg_death_gain_excl`) %>%
    distinct()
  death_excl_single = death_excl_by_fs %>%
    filter(filing_status == 1) %>% pull(`pref.kg_death_gain_excl`)
  death_excl_married = death_excl_by_fs %>%
    filter(filing_status == 2) %>% pull(`pref.kg_death_gain_excl`)
  if (length(death_excl_single)  == 0) death_excl_single  = NA_real_
  if (length(death_excl_married) == 0) death_excl_married = NA_real_

  mix = kg_dyn_build_regime_mix(regime_codes, theta, baseline_t, ages_bathtub)

  # Whether each asset class is realized at death
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
      death_excl_single   = death_excl_single[1],
      death_excl_married  = death_excl_married[1],
      realize             = realize_by_asset
    ),
    mix = mix
  )
}



