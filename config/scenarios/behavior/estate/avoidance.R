#-------------------------------------------------------------------------------
# ESTATE_AVOID_PROVENANCE
#
# Estate reporting response, split out of wealth/avoidance.R on 2026-07-16 so
# that estate-relevant scenarios can never run without it (the activation bug
# found in the 2026-07-16 methodology review: estate-only runscript rows never
# loaded wealth/avoidance, so estate-only scores had no reported-estate
# response and evasion never propagated into estates). This module owns the
# single estate_concealed_frac column read by calc_estate; it combines THREE
# drivers into one hidden ledger for the estate base:
#
#   (1) Wealth-tax concealment (R4): the concealed share of marketable /
#       closely-held assets, computed by wealth/avoidance and persisted on the
#       frame as wealth_c_pub / wealth_c_priv (0 when no wealth module runs).
#   (2) Income-tax evasion (R3 cross-base): the evaded share of a record's
#       closely-held income (from the evasion module's persisted evasion_g_*
#       response factors) pulls the matching share of its closely-held assets
#       out of the reported estate. Inert when no evasion module runs.
#   (3) The estate OWN-RATE response (estate-margins build part (b)).
#       Kopczuk-Slemrod (2001, "The Impact of the Estate Tax on Wealth
#       Accumulation and Avoidance Behavior", in Rethinking Estate and Gift
#       Taxation, Brookings; NOT their REStat death-timing paper "Dying to
#       Save Taxes") estimate an elasticity of REPORTED estates with respect
#       to the net-of-estate-tax rate of ~0.16 (pooled estimates ~0.10-0.22;
#       publish the band, not the point). Exact net-of-tax power form:
#
#         retained = ((1 - tau_S) / (1 - tau_B)) ^ ESTATE_REPORT_EPS
#         f_estate = 1 - retained
#
#       where tau_B / tau_S are the per-record UN-SWITCHED marginal estate
#       rates (mtr_estate) under the baseline and scenario legs, clamped to
#       [0, 1 - 1e-6]. Exact for large reforms; handles newly taxable records
#       (tau_B = 0 -> retained = (1 - tau_S)^eps).
#
# KEY DESIGN RULE — the own-rate response keys off the CHANGE in the estate
# price (tau_S vs tau_B), NEVER the level: baseline-level avoidance is already
# baked into the frozen valuation bridge (r, rho_pt, f_ded in
# estate_valuation_params.yaml, calibrated to SOI reported estates); a level
# response would double-count it. Unchanged estate law -> ratio = 1 -> exact
# no-op, so loading this module in every conventional run is free.
#
# NO CHI on the own-base response: the own base IS the estate — valuation
# gaming and concealment both reduce estate_base through the same single
# estate_concealed_frac column. The FULL f_estate applies; CHI stays reserved
# for cross-base propagation.
#
# Firewall (identical to the wealth concealment): reduces estate_base ONLY,
# via estate_concealed_frac; estate_distributable (the heir allocator's
# bequest ladder) and value.* stay invariant. The three drivers stack
# multiplicatively on the retained share (never > 100% hidden).
#
# Source caveats: KS's 0.16 bundles timing, avoidance, valuation, and some
# real accumulation; part of the observed reported-estate response IS
# charitable planning, which this reduced form partially absorbs.
# Denominator convention: estate_concealed_frac applies to reported_gross,
# while KS's elasticity is of the reported TAXABLE estate — a slight
# overstatement above the exemption, same convention as the wealth
# concealment, inside the published band (disclosed in the methodology memo).
#
# Magnitude reconciliation: at eps = 0.16 a 40% -> 55% top-rate hike gives
# f ~= 1 - (0.45/0.60)^0.16 ~= 4.5% of the top-bracket reported estate — an
# order of magnitude below the wealth-tax concealment response, as it should
# be: the estate tax is a once-at-death levy, the wealth tax recurs.
#
# ACTIVATION CONTRACT: wealth/avoidance hard-stops when this module is not
# later in the same behavior stack; evasion/debacker warns. The runscript lint
# (other/top_tax/tests/lint_estate_module.R) enforces it statically for the
# top_tax product runscripts.
#-------------------------------------------------------------------------------

ESTATE_AVOID_VERSION = '2026-07-16 standalone estate reporting module (split from wealth/avoidance)'
ESTATE_REPORT_EPS    = as.numeric(Sys.getenv('ESTATE_REPORT_EPS', unset = '0.16'))


do_estate = function(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Sets estate_concealed_frac — the fraction of reported gross estate hidden
  # from the estate tax — as the multiplicative union of wealth-tax
  # concealment, income-tax evasion, and the Kopczuk-Slemrod own-rate
  # reporting response (see ESTATE_AVOID_PROVENANCE above). calc_estate reads
  # the column and reduces estate_base; estate_distributable and value.* are
  # never touched (heirs inherit hidden assets in full).
  #
  # Must run AFTER evasion/ and wealth/ modules when they are present (it
  # reads their persisted per-record outputs); order-guarded below. With
  # neither present it reduces to the pure own-rate response, and with
  # unchanged estate law it is an exact no-op — so it is safe (and intended)
  # to load in every conventional run.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units with raw value.* columns,
  #                            plus (optionally) evasion_g_* factors persisted
  #                            by evasion/debacker and wealth_c_pub /
  #                            wealth_c_priv persisted by wealth/avoidance
  #   - baseline_mtrs (df)   : baseline MTRs; mtr_estate is LOAD-BEARING
  #                            (current law has a 40% top estate rate)
  #   - static_mtrs (df)     : static-counterfactual MTRs; must carry
  #                            mtr_estate (the un-switched marginal estate rate)
  #   - scenario_info (list) : get_scenario_info() object (ID + behavior_modules
  #                            for the order guards; output_path for diagnostics)
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with the estate_concealed_frac column set
  #          and the transient upstream factors (evasion_g_*, wealth_c_*)
  #          dropped. All other columns unchanged.
  #----------------------------------------------------------------------------

  modules = scenario_info$behavior_modules %||% character()

  # --- Order guards: upstream modules must already have run, since this module
  # reads their persisted per-record outputs.
  es_pos = which(startsWith(modules, 'estate/'))
  for (up in c('evasion/', 'wealth/')) {
    up_pos = which(startsWith(modules, up))
    if (length(up_pos) > 0 && length(es_pos) > 0 && min(up_pos) > min(es_pos)) {
      stop('do_estate(): when a ', up, ' module is present it must run BEFORE ',
           'estate/avoidance (this module reads its persisted per-record ',
           'outputs). Scenario "', scenario_info$ID, '" has behavior modules: ',
           paste(modules, collapse = ' '), '.')
    }
  }

  # --- Required-MTR guards. The own-rate response needs the un-switched
  # marginal estate rate on BOTH legs: the BASELINE estate MTR is genuinely
  # nonzero (current law has a 40% top rate), so the baseline join is
  # load-bearing. A missing baseline column usually means a PRE-BUILD baseline
  # vintage -- re-run the baseline static pass with current code and "estate"
  # registered in its mtr_vars.
  if (is.null(static_mtrs) || !('mtr_estate' %in% names(static_mtrs))) {
    stop('do_estate(): the estate own-rate response requires a registered ',
         'estate MTR (mtr_vars = "estate", mtr_types = "nextdollar") on the ',
         'scenario row. The runscript for scenario "', scenario_info$ID,
         '" does not provide mtr_estate in static MTRs.')
  }
  if (is.null(baseline_mtrs) || !('mtr_estate' %in% names(baseline_mtrs))) {
    stop('do_estate(): the estate own-rate response requires mtr_estate in ',
         'BASELINE MTRs (register "estate" in the baseline row\'s mtr_vars). ',
         'Missing there usually means a pre-build baseline vintage -- re-run ',
         'the baseline static pass with current code. Scenario: "',
         scenario_info$ID, '".')
  }

  message('do_estate(): applying estate reporting response (',
          ESTATE_AVOID_VERSION, '; estate_report_eps=', ESTATE_REPORT_EPS, ')')

  year = tax_units$year[1]

  # Upstream inputs: default the persisted factors when the upstream module did
  # not run this scenario, so each driver is inert but the code path is
  # unconditional.
  for (g in c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent')) {
    if (!(g %in% names(tax_units))) tax_units[[g]] = 1
  }
  for (g in c('wealth_c_pub', 'wealth_c_priv')) {
    if (!(g %in% names(tax_units))) tax_units[[g]] = 0
  }

  # Weight-free per-record NA-safe column sum helper
  sum_cols = function(d, cols) {
    m = as.matrix(d[, cols, drop = FALSE])
    m[is.na(m)] = 0
    rowSums(m)
  }

  df = tax_units %>%
    left_join(static_mtrs %>% select(id, year, mtr_estate_S = mtr_estate),
              by = c('id', 'year')) %>%
    left_join(baseline_mtrs %>% select(id, year, mtr_estate_B = mtr_estate),
              by = c('id', 'year')) %>%
    mutate(mtr_estate_S = replace_na(mtr_estate_S, 0),
           mtr_estate_B = replace_na(mtr_estate_B, 0))

  #--- Economic balance-sheet component sums (raw value.*, UNTOUCHED) ----------
  mkt   = sum_cols(df, WEALTH_MARKETABLE_COLS)
  clh   = sum_cols(df, WEALTH_CLOSELY_HELD_COLS)
  gross = mkt + clh

  #--- Driver 2: evaded income share (R3 cross-base link) ----------------------
  # Leg-weighted evaded income rate across the record's POSITIVE closely-held
  # flows, mapped from the persisted evasion response factors (evaded rate =
  # 1 - g). Reading the post-concealment legs is exact: wealth concealment
  # scales every closely-held leg by the same (1 - c_priv) on the same gates,
  # which cancels in the ratio.
  lp_schc = pmax(replace_na(df$sole_prop, 0), 0)
  lp_pt   = pmax(replace_na(df$part_active,   0), 0) +
            pmax(replace_na(df$part_passive,  0), 0) +
            pmax(replace_na(df$scorp_active,  0), 0) +
            pmax(replace_na(df$scorp_passive, 0), 0)
  lp_rent = pmax(replace_na(df$rent, 0), 0)
  leg_tot = lp_schc + lp_pt + lp_rent
  evaded  = ifelse(leg_tot > 0,
                   (lp_schc * (1 - df$evasion_g_schc) +
                    lp_pt   * (1 - df$evasion_g_pt) +
                    lp_rent * (1 - df$evasion_g_rent)) / leg_tot,
                   0)
  evaded  = pmax(pmin(evaded, 1), 0)

  #--- Drivers 1 + 2: concealment union -----------------------------------------
  # Wealth-tax concealment and income-tax evasion are two routes by which a
  # closely-held asset leaves the authority's sight. Combine them as a
  # multiplicative union: c_priv is hidden first, then evasion hides its share
  # of the remaining visible balance (no overlap double-counted). Legal
  # valuation avoidance (the non-concealment share of the wealth response)
  # remains visible to the estate, as intended.
  c_pub  = pmax(pmin(replace_na(df$wealth_c_pub,  0), 1), 0)
  c_priv = pmax(pmin(replace_na(df$wealth_c_priv, 0), 1), 0)
  estate_c_priv = c_priv + (1 - c_priv) * evaded
  estate_union  = ifelse(gross > 0,
                         (c_pub * mkt + estate_c_priv * clh) / gross, 0)

  #--- Driver 3: estate OWN-RATE response --------------------------------------
  # Exact KS net-of-tax power form on the CHANGE in the un-switched marginal
  # estate rate. Both legs clamped before the ratio; unchanged estate law ->
  # ratio = 1 -> retained = 1 -> exact no-op. Newly taxable records (tau_B = 0)
  # fall out of the same formula. A rate CUT gives retained > 1 -- previously
  # unreported estate surfaces (symmetric KS margin).
  tau_eS = pmin(pmax(df$mtr_estate_S, 0), 1 - 1e-6)
  tau_eB = pmin(pmax(df$mtr_estate_B, 0), 1 - 1e-6)
  retained_estate = ((1 - tau_eS) / (1 - tau_eB)) ^ ESTATE_REPORT_EPS
  f_estate = 1 - retained_estate

  # Stack multiplicatively on the RETAINED share: one hidden ledger, three
  # drivers, never > 100% hidden by construction (estate_union <= 1 and
  # retained_estate > 0). Floor at -1 so a pathological rate cut can never
  # more than double the reported base. Reduces (or, on cuts, boosts)
  # estate_base ONLY -- estate_distributable and value.* stay invariant.
  df$estate_concealed_frac = pmax(1 - (1 - estate_union) * retained_estate, -1)

  if (any(is.na(df$estate_concealed_frac))) {
    stop('do_estate(): NA introduced in estate_concealed_frac.')
  }

  #--- Diagnostics -------------------------------------------------------------
  w = df$weight
  diag = tibble(
    year                        = year,
    version                     = ESTATE_AVOID_VERSION,
    estate_report_eps           = ESTATE_REPORT_EPS,
    estate_union_wmean_grosspos = if (sum(w * (gross > 0)) > 0)
                                    sum(w * estate_union * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_hidden_from_evasion  = sum(w * (1 - c_priv) * evaded * clh),
    estate_mtr_B_wmean_grosspos = if (sum(w * (gross > 0)) > 0)
                                    sum(w * tau_eB * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_mtr_S_wmean_grosspos = if (sum(w * (gross > 0)) > 0)
                                    sum(w * tau_eS * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_own_rate_f_wmean     = if (sum(w * (gross > 0)) > 0)
                                    sum(w * f_estate * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_own_rate_hidden      = sum(w * f_estate * (1 - estate_union) * gross),
    estate_concealed_frac_wmean = if (sum(w * (gross > 0)) > 0)
                                    sum(w * df$estate_concealed_frac) /
                                    sum(w * (gross > 0)) else 0)

  diag_dir = file.path(scenario_info$output_path, 'conventional', 'supplemental')
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(diag, file.path(diag_dir, paste0('estate_avoidance_', year, '.csv')))

  #--- Return ------------------------------------------------------------------
  # Drop the joined MTR legs and the transient upstream factors; KEEP
  # estate_concealed_frac (read by calc_estate via the frame).
  df %>%
    select(-mtr_estate_S, -mtr_estate_B,
           -any_of(c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent',
                     'wealth_c_pub', 'wealth_c_priv'))) %>%
    return()
}
