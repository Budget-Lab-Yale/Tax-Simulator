#-------------------------------------------------------------------------------
# EVASION_PROVENANCE
#
# Rate-driven income tax noncompliance (evasion), per DeBacker, Heim &
# Yuskavage, "Marginal Tax Rates and Income Tax Noncompliance" (Treasury
# OTA / NTA Annual Meetings, November 2025; NRP random-audit data 2006-2017).
# DHY estimate the elasticity of noncompliance with respect to the net-of-tax
# rate as a COMPONENT of the ETI (their slide 8: ETI = e_avoidance +
# e_noncompliance), so the values below are applied here as net-of-tax-rate
# elasticities of REPORTED income — the authors' own usage in their OBBBA
# revenue illustration.
#
# Central values and sources (elasticity magnitudes; sign handled by the
# 'netoftax' form — reported income falls when the MTR rises):
#   - EVASION_E_SCHC = 0.046 : Schedule C/F (sole prop + farm). DHY pooled
#         cross-section, sole-proprietor subsample, federal MTR. Kansas DiD
#         Schedule C gives 0.09; bunching (EITC-kink sole props) 0.26 — both
#         alternative anchors for the env-var sweep, not centrals (the
#         bunching estimate is a low-income population).
#   - EVASION_E_PT   = 0.052 : partnership + S-corp income. DHY pooled
#         cross-section, partnership subsample, federal MTR.
#   - EVASION_E_RENT = 0.040 : rent (Schedule E ex pass-through). DHY Kansas
#         DiD Schedule E (not significant; weakest-identified value here).
#   - Wages, interest, dividends: NO response by design. Information reporting
#         makes these visible (DHY Table: nonzero audit-adjustment rates ~7%
#         for wages vs ~74% for sole-prop income); the tax-gap literature puts
#         wage misreporting near 1%.
#
# EVASION_TOPEND_MULT (default 1.0) is an underdetection sweep knob: NRP
# random audits underdetect sophisticated top-end evasion (offshore, tiered
# partnerships — Guyton, Langetieg, Reck, Risch & Zucman 2021), so
# detected-noncompliance elasticities are a FLOOR for the top tail. Sweep
# 1.5-2.0 for the high band.
#
# Omitted margins (accepted, revisit if they bind): overstated losses and
# itemized deductions (DHY find itemizer elasticities 0.069-0.23; deduction-
# side evasion is not modeled here — only positive income legs respond), and
# any enforcement-recovery offset (audit recoveries are outside the model).
#
# Role in the top-tax ETI decomposition: this module is the EVASION leg —
# income that LEAVES the tax system (a leak). It is distinct from the planned
# cross-base shifting parameter (a conservation flow into the kg gain state)
# and from modeled legal avoidance (realization, entity shifting, charity).
# Do NOT stack this module with any generic ETI adjustment: that would
# double-count the noncompliance component.
#-------------------------------------------------------------------------------

EVASION_VERSION     = '2026-07-07 DHY (NTA 2025) centrals, seeded from slides'
EVASION_E_SCHC      = as.numeric(Sys.getenv('EVASION_E_SCHC',      unset = '0.046'))
EVASION_E_PT        = as.numeric(Sys.getenv('EVASION_E_PT',        unset = '0.052'))
EVASION_E_RENT      = as.numeric(Sys.getenv('EVASION_E_RENT',      unset = '0.040'))
EVASION_TOPEND_MULT = as.numeric(Sys.getenv('EVASION_TOPEND_MULT', unset = '1'))
EVASION_MAX_ADJ     = 1


do_evasion = function(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Models rate-driven noncompliance: when the marginal tax rate on a
  # low-visibility income type rises, reported income of that type falls (and
  # vice versa), per the DHY elasticities documented in EVASION_PROVENANCE.
  # Applies a net-of-tax-rate response by income-visibility group:
  #
  #   - Schedule C/F group : sole_prop, farm — factor from mtr_sole_prop1
  #                          (the SECA-inclusive Schedule C rate; calc_mtrs
  #                          bumps sole_prop alongside sole_prop1)
  #   - Pass-through group : part_active, part_passive, scorp_active,
  #                          scorp_passive — factor from mtr_part_active
  #                          (calc_mtrs bumps part_se1 alongside, so SECA is in)
  #   - Rent               : rent — factor from mtr_rent
  #
  # Only POSITIVE income legs respond (underreporting shrinks reported income;
  # the overstated-loss/deduction margin is deliberately omitted — see
  # provenance block). Loss and Sec. 179 legs are untouched. SECA/NIIT
  # earner-split companions (sole_prop1/2, farm1/2, part_se1/2) co-scale with
  # their parent aggregate so the payroll frame stays consistent, mirroring
  # the WEALTH_CAP_FLOWS_SE_COMPANIONS convention. derive_vars() recombines
  # the raw legs into part/scorp/pt/sch_e inside do_taxes(), so scaling the
  # legs propagates.
  #
  # This is an "implement-any-logic" module (like wealth/avoidance.R), not an
  # apply_mtr_elasticity() call, because one shared group factor scales many
  # legs plus companions; the netoftax formula is identical to the helper's.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units with calculated variables
  #   - baseline_mtrs (df)   : year-id indexed tibble of MTRs under baseline;
  #                            must carry mtr_sole_prop1, mtr_part_active,
  #                            mtr_rent
  #   - static_mtrs (df)     : year-id indexed tibble of MTRs under the static
  #                            counterfactual scenario; same required columns
  #   - scenario_info (list) : get_scenario_info() object (ID used in errors)
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with reported low-visibility income legs
  #          adjusted for the noncompliance response.
  #----------------------------------------------------------------------------

  # Required MTRs must be registered. Fail loudly rather than silently skipping
  # the response (which would mislabel a static score as conventional).
  required = c('mtr_sole_prop1', 'mtr_part_active', 'mtr_rent')
  missing  = if (is.null(static_mtrs) || is.null(baseline_mtrs)) required else
             setdiff(required, intersect(names(static_mtrs), names(baseline_mtrs)))
  if (length(missing) > 0) {
    stop('do_evasion(): the evasion module requires registered MTRs for ',
         'sole_prop1, part_active, and rent (mtr_vars = "sole_prop1 ',
         'part_active rent", mtr_types = "nextdollar nextdollar nextdollar"). ',
         'The runscript for scenario "', scenario_info$ID, '" is missing: ',
         paste(missing, collapse = ', '), '.')
  }

  message('do_evasion(): applying noncompliance elasticities (', EVASION_VERSION,
          '; schc=', EVASION_E_SCHC, ', pt=', EVASION_E_PT,
          ', rent=', EVASION_E_RENT, ', topend_mult=', EVASION_TOPEND_MULT, ')')

  # Net-of-tax response factor, clamped at +/- EVASION_MAX_ADJ. NA in either
  # MTR frame (or a degenerate baseline rate of 1) means no response.
  response_factor = function(mtr, mtr_baseline, e) {
    pct_chg = e * EVASION_TOPEND_MULT * ((1 - mtr) / (1 - mtr_baseline) - 1)
    pct_chg = pmax(-EVASION_MAX_ADJ, pmin(pct_chg, EVASION_MAX_ADJ))
    if_else(is.na(pct_chg), 1, 1 + pct_chg)
  }

  tax_units %>%

    # Join MTRs under baseline and static counterfactual
    left_join(baseline_mtrs %>%
                select(id, year, all_of(required)) %>%
                rename_with(.cols = -c(id, year),
                            .fn   = ~ paste0(., '_baseline')),
              by = c('id', 'year')) %>%
    left_join(static_mtrs %>%
                select(id, year, all_of(required)),
              by = c('id', 'year')) %>%

    mutate(

      # Group response factors. PERSISTED as record columns (evasion_g_*) so the
      # wealth-avoidance module -- which runs AFTER this one in the pinned stack
      # -- can read each record's income-evasion outcome for the R3
      # evasion->wealth consistency link (an income evader under a wealth tax
      # should not report the assets whose income he hides). Not registered in
      # detail_vars, so they do not leak into the written detail.
      evasion_g_schc = response_factor(mtr_sole_prop1,  mtr_sole_prop1_baseline,  EVASION_E_SCHC),
      evasion_g_pt   = response_factor(mtr_part_active, mtr_part_active_baseline, EVASION_E_PT),
      evasion_g_rent = response_factor(mtr_rent,        mtr_rent_baseline,        EVASION_E_RENT),

      # Positive-income gates, evaluated BEFORE any leg is scaled so the
      # companions always ride with their parent
      .schc_pos  = !is.na(sole_prop)     & sole_prop     > 0,
      .farm_pos  = !is.na(farm)          & farm          > 0,
      .parta_pos = !is.na(part_active)   & part_active   > 0,
      .partp_pos = !is.na(part_passive)  & part_passive  > 0,
      .scorpa_pos = !is.na(scorp_active)  & scorp_active  > 0,
      .scorpp_pos = !is.na(scorp_passive) & scorp_passive > 0,
      .rent_pos  = !is.na(rent)          & rent          > 0,

      # Schedule C/F group (+ SECA earner-split companions)
      across(.cols = all_of(c('sole_prop', 'sole_prop1', 'sole_prop2')),
             .fns  = ~ if_else(.schc_pos, . * evasion_g_schc, .)),
      across(.cols = all_of(c('farm', 'farm1', 'farm2')),
             .fns  = ~ if_else(.farm_pos, . * evasion_g_schc, .)),

      # Partnership / S-corp group (+ partnership SE companions)
      across(.cols = all_of(c('part_active', 'part_se1', 'part_se2')),
             .fns  = ~ if_else(.parta_pos, . * evasion_g_pt, .)),
      part_passive  = if_else(.partp_pos,  part_passive  * evasion_g_pt, part_passive),
      scorp_active  = if_else(.scorpa_pos, scorp_active  * evasion_g_pt, scorp_active),
      scorp_passive = if_else(.scorpp_pos, scorp_passive * evasion_g_pt, scorp_passive),

      # Rent (rent_loss untouched)
      rent = if_else(.rent_pos, rent * evasion_g_rent, rent)
    ) %>%

    # Drop joined MTRs and the positive-income gate temps (evasion_g_* are kept
    # for the downstream wealth-avoidance consistency link); return full frame
    select(-all_of(c(required, paste0(required, '_baseline'),
                     '.schc_pos', '.farm_pos', '.parta_pos', '.partp_pos',
                     '.scorpa_pos', '.scorpp_pos', '.rent_pos'))) %>%
    return()
}
