#-------------------------------------------------------------------------------
# evasion/debacker.R
#
# Contains the income tax noncompliance response
#-------------------------------------------------------------------------------

# Assume that reported income falls as the marginal rate rises, because some of it
# is not reported at all. The elasticities are from DeBacker, Heim and Yuskavage,
# "Marginal Tax Rates and Income Tax Noncompliance" (National Tax Association
# meetings, November 2025), estimated on random audit data from 2006 to 2017. The
# findings are the authors' own and not Treasury's.
#
# Those authors treat noncompliance as one component of the elasticity of taxable
# income, alongside avoidance, and the values are used here the same way: as
# elasticities of reported income with respect to the net-of-tax rate. That is their
# own usage in their illustration of the 2025 act.
#
# Wages, interest and dividends do not respond, by design. Third parties report
# them, so they are visible: the authors find audit adjustments on about 7% of wages
# against about 74% of sole proprietor income, and the tax gap literature puts wage
# misreporting near 1%.
#
# Two margins are left out. Overstated losses and deductions, where the authors do
# find a response; only positive income legs move here. And any recovery through
# enforcement, which the model does not represent.
#
# This is income leaving the tax system altogether. It is distinct from income
# moving between bases, which entity shifting and conversion handle, and from legal
# avoidance. Do not run it alongside a general elasticity of taxable income, which
# would count noncompliance twice.

EVASION_VERSION     = '2026-07-07 DHY (NTA 2025) centrals, seeded from slides'
EVASION_MAX_ADJ     = 1
EVASION_NET_RATE_EPS = 1e-12

# The elasticities. Here because this module is the only thing that reads them. A
# variant is another file in this folder rather than a value in a config file, so
# the assumption is traceable to a file in git.

# Schedule C/F -- sole proprietors and farms. DHY pooled cross-section,
# sole-proprietor subsample, federal MTR. Alternative anchors, for a band rather
# than a central: 0.09 (Kansas difference-in-differences, Schedule C) and 0.26
# (bunching at EITC kinks, a low-income population).
EVASION_E_SCHC = 0.046

# Partnership and S-corp income. DHY pooled cross-section, partnership
# subsample, federal MTR.
EVASION_E_PT = 0.052

# Rent -- Schedule E excluding pass-through. DHY Kansas difference-in-
# differences. Not statistically significant, and the weakest-identified value
# in this module.
EVASION_E_RENT = 0.040

# A multiplier on all three, for evasion the audits do not detect. Random audits
# miss sophisticated arrangements at the top, such as offshore structures and tiered
# partnerships (Guyton, Langetieg, Reck, Risch and Zucman 2021), so an elasticity
# measured from detected noncompliance is a floor there. At 1 there is no adjustment;
# a high variant of this module would set 1.5 to 2.
EVASION_TOPEND_MULT = 1


evasion_response_factor = function(mtr, mtr_baseline, e) {

  # The elasticity is undefined where the baseline net-of-tax rate is zero. Treat
  # that, and any missing rate, as no response. Guard the
  # denominator before dividing, since an infinity would survive the fallback, hit
  # the clamp, and either zero or double reported income.
  net_base = 1 - mtr_baseline
  valid = is.finite(mtr) & is.finite(mtr_baseline) &
          abs(net_base) > EVASION_NET_RATE_EPS

  pct_chg = rep(NA_real_, length(net_base))
  pct_chg[valid] = e * EVASION_TOPEND_MULT *
                   ((1 - mtr[valid]) / net_base[valid] - 1)
  pct_chg = pmax(-EVASION_MAX_ADJ, pmin(pct_chg, EVASION_MAX_ADJ))
  if_else(is.na(pct_chg), 1, 1 + pct_chg)
}


do_evasion = function(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Reduces reported income where its marginal rate rises, in three groups by how
  # visible the income is: sole proprietorships and farms, pass-through income, and
  # rent. Each group responds to one representative rate, which includes
  # self-employment tax where that applies.
  #
  # Only positive legs respond, since underreporting shrinks reported income. Losses
  # and the expensing legs are untouched. The earner splits scale with their parent
  # aggregate, so the payroll base stays consistent. derive_vars() recombines the raw
  # legs inside do_taxes(), so scaling them carries through.
  #
  # This is written out rather than being a call to apply_mtr_elasticity(), because
  # one factor per group scales many legs at once. The formula is the same as the
  # helper's.
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

  # The R3 consistency contract -- evaded income must also leave the reported
  # wealth and estate bases, and the estate leg of that lives in
  # estate/avoidance, which reads the evasion_g_* factors persisted below -- is
  # now checked once at parse time for every scenario in the runscript
  # (behavior_validate_spec, src/sim/behavior.R) rather than warned about here
  # on every year of every pass.

  message('do_evasion(): applying noncompliance elasticities (', EVASION_VERSION,
          '; schc=', EVASION_E_SCHC,
          ', pt=',   EVASION_E_PT,
          ', rent=', EVASION_E_RENT,
          ', topend_mult=', EVASION_TOPEND_MULT, ')')

  # Net-of-tax response factor, clamped at +/- EVASION_MAX_ADJ. NA in either
  # MTR frame (or a degenerate baseline rate of 1) means no response.
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
      evasion_g_schc = evasion_response_factor(mtr_sole_prop1,  mtr_sole_prop1_baseline,  EVASION_E_SCHC),
      evasion_g_pt   = evasion_response_factor(mtr_part_active, mtr_part_active_baseline, EVASION_E_PT),
      evasion_g_rent = evasion_response_factor(mtr_rent,        mtr_rent_baseline,        EVASION_E_RENT),

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
