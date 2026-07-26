#-------------------------------------------------------------------------------
# WEALTH_AVOID_PROVENANCE
#
# Within-year wealth-tax avoidance / evasion elasticities. SEMI-elasticities of
# taxable wealth with respect to the marginal wealth-tax rate (the exp(mtr * e)
# form, == apply_mtr_elasticity's 'semi' type with a baseline MTR of 0). Two
# classes, mirroring the standalone Wealth-Tax-Simulator's do_avoidance():
#   - public_e  : publicly traded / liquid / marketable wealth
#   - private_e : closely-held business and other nonfinancial wealth
#
# CALIBRATION STATUS: AUTHOR-ACCEPTED 2026-07-08. These values were seeded
# verbatim from the standalone (config/scenarios/nickel_dime.R: public_e = -7,
# private_e = -17) and reviewed by the author, who accepts them as reasonable
# centrals (at a 3% marginal rate they imply ~19% / ~40% erosion of marketable /
# closely-held reported wealth). A sensitivity band around them is still good
# practice for published estimates. Bump WEALTH_AVOID_VERSION and this block if
# the values change.
#
# HIDDEN-LEDGER OVERLAY (2026-07-08, other/top_tax/hidden_ledger_design.md,
# rulings R1-R7; code audit other/top_tax/reporting_vs_real_audit.md). Splits
# the avoidance response into CONCEALMENT (the money leaves the tax authority's
# sight entirely -- its income flows and its estate value disappear from the
# reported bases too) and legal VALUATION gaming (assessed value lowballed but
# income still visibly received), and adds the reverse consistency link (income
# the evasion module hides pulls the matching closely-held assets out of the
# reported wealth base). The organizing object is a per-record hidden ledger:
# each reporting margin contributes hidden amounts, every REPORTED base (wealth,
# income, estate) reads from it, and every REAL base (value.*, bathtub, kg,
# heirs) never does.
#
#   R1  Marketable avoidance is 100% concealment (you cannot lowball an exchange
#       price)                                                  -> CHI_PUB  = 1.0
#   R2  Closely-held avoidance is 50% valuation / 50% concealment (discounts are
#       real and legal for private businesses)                  -> CHI_PRIV = 0.5
#   R3  The evasion elasticity applies to wealth and estate reporting too: the
#       evaded share of a record's closely-held income pulls the matching share
#       of its closely-held assets out of both reported stock bases (a
#       consistency rule, not a new elasticity)
#   R4  Concealed wealth also escapes the reported ESTATE at death -- carried
#       by estate/avoidance.R, which a stack containing this module is refused
#       without (checked at parse time), and which combines it with income
#       evasion and the KS own-rate response into the estate_concealed_frac
#       column read by calc_estate
#   R5  Homes keep the uniform 50/50 split (no home-specific chi)
#   R6  Capital gains are IN v1 as a reporting-QUANTITY overlay: reported kg_lt
#       scales by (1 - c_pub) AFTER the kg module sets realization behavior (the
#       GAIN is scaled, not the price; no basis change, no value.* change)
#   R7  the two chi shares are module constants (below), read once and stamped
#       into the diagnostics file
#
# The IRON RULE (from the audit): concealment touches ONLY tax-computation
# inputs -- reported income-flow columns (they feed do_taxes and nothing else),
# the materialized net_worth column (the documented isolation point calc_wealth
# reads), and -- through estate/avoidance.R -- the optional estate_concealed_frac
# input to calc_estate. It NEVER scales value.*, which every real-side channel
# reads as the true balance sheet.
#-------------------------------------------------------------------------------

WEALTH_AVOID_VERSION  = paste('2026-07-08 hidden-ledger (R1-R7);',
                              'elasticities author-accepted (seeded from standalone);',
                              '2026-07-16 estate response split out to estate/avoidance')

# --- The elasticities ----------------------------------------------------------
# Here rather than in config because this module is their only reader. A band is
# a copy of this file with different numbers, listed by a different behavior
# alternative.
#
# Semi-elasticities of REPORTED wealth with respect to the marginal wealth rate:
# the reported stock scales by exp(mtr_net_worth * e). Both were seeded verbatim
# from the standalone Wealth-Tax-Simulator and accepted by the author on
# 2026-07-08 as reasonable centrals -- they are NOT calibrated, and at a 3%
# marginal rate they imply roughly 19% and 40% erosion of reported marketable and
# closely-held wealth. The private-wealth value is the largest single behavioral
# magnitude in the model and the first thing a sensitivity ranking should reach
# for.
WEALTH_AVOID_PUBLIC_E  = -7    # marketable / publicly valued wealth
WEALTH_AVOID_PRIVATE_E = -17   # closely held, where valuation discretion is wider

# --- Concealment shares (R7) ---------------------------------------------------
# What fraction of the avoidance response is CONCEALMENT -- money that leaves the
# tax authority's sight, so its income flows and its estate value disappear from
# the reported bases too -- as opposed to legal valuation gaming, where the
# assessed value is lowballed but the income is still visibly received.
#
# 1.0 for marketable wealth: you cannot lowball an exchange price, so a
# marketable asset that vanishes from the reported balance sheet has genuinely
# vanished. 0.5 for closely held: discounts are real and legal for private
# businesses, so half the response is treated as valuation.
#
# Setting both to 0 reproduces pre-hidden-ledger behavior exactly -- reported
# net_worth still shrinks by the full avoidance response, but the flow, gain and
# estate concealment overlays vanish.
CHI_PUB  = 1.0
CHI_PRIV = 0.5


do_wealth = function(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Models within-year wealth-tax avoidance as a reduction in the REPORTED
  # (taxable) wealth base, and -- via the hidden ledger -- the matching
  # concealment of income flows, capital gains, and the taxable estate.
  #
  # (1) Reported net worth: two semi-elasticities (public_e / private_e) shrink
  #     the marketable / closely-held component sums of net worth; the R3
  #     evasion link further shaves the closely-held term by the record's evaded
  #     income share. Written back to the MATERIALIZED net_worth column ONLY;
  #     the raw value.* columns are left untouched (the isolation requirement),
  #     so estate liability and capital income -- which read value.* -- see the
  #     concealment only through the explicit reported-side channels below.
  # (2) Concealment fractions c_pub = CHI_PUB * f_pub, c_priv = CHI_PRIV * f_priv
  #     (f_* = 1 - exp(mtr_w * e), the same avoided fraction; 0 below the
  #     exemption since mtr_net_worth = 0 there).
  # (3) Flow scaling: concealed marketable wealth stops producing reported
  #     txbl_int / div_ord / div_pref (and, per R6, reported kg_lt); concealed
  #     closely-held wealth stops producing reported pass-through / rent income
  #     (SECA companions co-scaled, debacker pattern). Multiplicative,
  #     positive-leg gated. These columns feed only do_taxes.
  # (4) Estate propagation is DELEGATED: the concealment fractions are
  #     persisted as wealth_c_pub / wealth_c_priv record columns and consumed
  #     by estate/avoidance.R (required in the stack; checked at parse time),
  #     which combines them with income evasion and the KS own-rate response
  #     into the single estate_concealed_frac column read by calc_estate.
  #
  # This is an "implement-any-logic" module (like employment/bastian.R), not a
  # one-line apply_mtr_elasticity() call, because the dual-class response scales
  # two component sums plus many reported legs rather than a single registered
  # variable -- but it reads the static MTR through the same id/year-indexed
  # machinery.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units, pre tax calculation, with
  #                            the materialized net_worth column and raw value.*
  #   - baseline_mtrs (df)   : baseline MTRs (unused here -- the baseline
  #                            wealth MTR is 0 by construction; the estate
  #                            legs now live in estate/avoidance.R)
  #   - static_mtrs (df)     : static-counterfactual MTRs, must carry
  #                            mtr_net_worth (the statutory marginal wealth
  #                            rate)
  #   - scenario_info (list) : get_scenario_info() object (ID for messages;
  #                            output_path for diagnostics)
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with net_worth overwritten by the avoided
  #          (reported) base, reported income/gain legs concealed, and the
  #          concealment fractions persisted as wealth_c_pub / wealth_c_priv
  #          for estate/avoidance.R downstream. value.* and kg_lt_basis /
  #          kg_deemed_full unchanged.
  #----------------------------------------------------------------------------

  # Three guards used to sit here: that evasion runs before this module (R3 --
  # the link reads evasion's per-record factors), that the bathtub runs before
  # it (R6 -- the reported-gain overlay would otherwise be overwritten), and
  # that an estate module runs after it (the concealment has to reach the
  # reported estate). The first two are now guaranteed by the pinned family
  # order and the third is checked at parse time; both live in
  # src/sim/behavior.R.

  # --- Required-MTR guard. The wealth MTR must be registered for this module to
  # do anything. Fail loudly rather than silently skipping avoidance (which
  # would mislabel a static score as conventional).
  if (is.null(static_mtrs) || !('mtr_net_worth' %in% names(static_mtrs))) {
    stop('do_wealth(): the wealth-avoidance module requires a registered ',
         'net_worth MTR (mtr_vars = "net_worth", mtr_types = "nextdollar") so ',
         'it can read the statutory marginal wealth rate. The runscript for ',
         'scenario "', scenario_info$ID, '" does not provide mtr_net_worth.')
  }

  message('do_wealth(): applying wealth-avoidance + hidden-ledger concealment (',
          WEALTH_AVOID_VERSION,
          '; public_e=',  WEALTH_AVOID_PUBLIC_E,
          ', private_e=', WEALTH_AVOID_PRIVATE_E,
          '; chi_pub=',   CHI_PUB,
          ', chi_priv=',  CHI_PRIV, ')')

  year = tax_units$year[1]

  # Evasion-link inputs: default the persisted evasion response factors to 1 (no
  # evasion) when the evasion module did not run this scenario, so the R3 link
  # is inert but the code path is unconditional (harmless -- net_worth is read
  # only by calc_wealth).
  for (g in c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent')) {
    if (!(g %in% names(tax_units))) tax_units[[g]] = 1
  }

  # Weight-free per-record NA-safe column sum helper (value.* / debts are always
  # present in Tax-Data; replace_na guards the odd missing cell)
  sum_cols = function(d, cols) {
    m = as.matrix(d[, cols, drop = FALSE])
    m[is.na(m)] = 0
    rowSums(m)
  }

  df = tax_units %>%
    left_join(static_mtrs %>% select(id, year, mtr_net_worth),
              by = c('id', 'year')) %>%
    mutate(mtr_net_worth = replace_na(mtr_net_worth, 0))

  #--- Economic balance-sheet component sums (raw value.*, UNTOUCHED) ----------
  mkt   = sum_cols(df, WEALTH_MARKETABLE_COLS)
  clh   = sum_cols(df, WEALTH_CLOSELY_HELD_COLS)
  debts = sum_cols(df, WEALTH_DEBT_COLS)
  gross = mkt + clh

  #--- Avoided and concealed fractions ----------------------------------------
  # Avoided fraction of each class (semi-elasticity, baseline wealth MTR = 0).
  f_pub  = 1 - exp(df$mtr_net_worth * WEALTH_AVOID_PUBLIC_E)
  f_priv = 1 - exp(df$mtr_net_worth * WEALTH_AVOID_PRIVATE_E)
  # Concealed fraction: the concealment share (CHI) of the avoidance response.
  c_pub  = CHI_PUB  * f_pub
  c_priv = CHI_PRIV * f_priv
  # PERSISTED for estate/avoidance downstream (the R4 estate propagation);
  # dropped from the frame there, mirroring the evasion_g_* convention.
  df$wealth_c_pub  = c_pub
  df$wealth_c_priv = c_priv

  #--- R3 evasion->wealth link -------------------------------------------------
  # Leg-weighted evaded income rate across the record's POSITIVE closely-held
  # flows (parent legs only; companions are earner splits of the same income),
  # mapped to the persisted evasion response factors (evaded rate = 1 - g).
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

  #--- Reported net worth ------------------------------------------------------
  # FULL avoidance response (valuation + concealment) shrinks the reported base
  # exactly as before; the R3 link further shaves the reported CLOSELY-HELD
  # component by the evaded income share. Debts are not avoided (no incentive).
  df$net_worth = mkt * exp(df$mtr_net_worth * WEALTH_AVOID_PUBLIC_E) +
                 clh * exp(df$mtr_net_worth * WEALTH_AVOID_PRIVATE_E) * (1 - evaded) -
                 debts

  #--- Flow concealment --------------------------------------------------------
  # Concealed assets stop producing reported income. Multiplicative, positive-
  # leg gated; a concealer keeps a NON-positive leg (loss) intact. Retirement
  # distributions (txbl_ira_dist / txbl_pens_dist) are DELIBERATELY excluded:
  # value.dc / value.db sit in the marketable class for the avoidance elasticity,
  # but retirement accounts are third-party-reported and not realistically
  # concealable (design 4.2). We track the concealed dollars explicitly so the
  # conservation identity (reported + hidden = pre-avoidance, per leg) is a real
  # arithmetic cross-check, not a restatement of the scaling.

  # Records the concealed dollars for one leg, gated on `gate_leg` positivity,
  # and returns the leg net of them. `pre`/`gate_leg` let companions ride a
  # parent's gate (SECA earner splits).
  conceal_leg = function(pre, c, gate) {
    pre0 = replace_na(pre, 0)
    ifelse(gate, pre0 * (1 - c), pre)
  }
  hidden_leg = function(pre, c, gate) {
    pre0 = replace_na(pre, 0)
    ifelse(gate, pre0 * c, 0)
  }

  # Gates (parent positivity), evaluated before scaling
  g_int   = replace_na(df$txbl_int,      0) > 0
  g_dord  = replace_na(df$div_ord,       0) > 0
  g_dprf  = replace_na(df$div_pref,      0) > 0
  g_kg    = replace_na(df$kg_lt,         0) > 0
  g_parta = replace_na(df$part_active,   0) > 0
  g_partp = replace_na(df$part_passive,  0) > 0
  g_scrpa = replace_na(df$scorp_active,  0) > 0
  g_scrpp = replace_na(df$scorp_passive, 0) > 0
  g_sole  = replace_na(df$sole_prop,     0) > 0
  g_rent  = replace_na(df$rent,          0) > 0

  # Snapshot pre-concealment legs for the conservation assert / diagnostics
  pre = list(
    txbl_int = df$txbl_int, div_ord = df$div_ord, div_pref = df$div_pref,
    kg_lt = df$kg_lt,
    part_active = df$part_active, part_passive = df$part_passive,
    scorp_active = df$scorp_active, scorp_passive = df$scorp_passive,
    sole_prop = df$sole_prop, rent = df$rent,
    sole_prop1 = df$sole_prop1, sole_prop2 = df$sole_prop2,
    part_se1 = df$part_se1, part_se2 = df$part_se2)

  # Marketable-class concealment (c_pub): interest and dividends, plus the R6
  # reporting-quantity overlay on realized long-term gains (gain scaled, not
  # price; no basis change -- kg_lt_basis is untouched)
  hid_int  = hidden_leg(df$txbl_int, c_pub, g_int)
  hid_dord = hidden_leg(df$div_ord,  c_pub, g_dord)
  hid_dprf = hidden_leg(df$div_pref, c_pub, g_dprf)
  hid_kg   = hidden_leg(df$kg_lt,    c_pub, g_kg)
  df$txbl_int = conceal_leg(df$txbl_int, c_pub, g_int)
  df$div_ord  = conceal_leg(df$div_ord,  c_pub, g_dord)
  df$div_pref = conceal_leg(df$div_pref, c_pub, g_dprf)
  df$kg_lt    = conceal_leg(df$kg_lt,    c_pub, g_kg)

  # Closely-held-class concealment (c_priv): pass-through and rent income, with
  # SECA earner-split companions (sole_prop1/2, part_se1/2) riding their parent's
  # gate so the payroll frame stays consistent (debacker pattern)
  hid_parta = hidden_leg(df$part_active,   c_priv, g_parta)
  hid_partp = hidden_leg(df$part_passive,  c_priv, g_partp)
  hid_scrpa = hidden_leg(df$scorp_active,  c_priv, g_scrpa)
  hid_scrpp = hidden_leg(df$scorp_passive, c_priv, g_scrpp)
  hid_sole  = hidden_leg(df$sole_prop,     c_priv, g_sole)
  hid_rent  = hidden_leg(df$rent,          c_priv, g_rent)
  df$part_active   = conceal_leg(df$part_active,   c_priv, g_parta)
  df$part_passive  = conceal_leg(df$part_passive,  c_priv, g_partp)
  df$scorp_active  = conceal_leg(df$scorp_active,  c_priv, g_scrpa)
  df$scorp_passive = conceal_leg(df$scorp_passive, c_priv, g_scrpp)
  df$sole_prop     = conceal_leg(df$sole_prop,     c_priv, g_sole)
  df$rent          = conceal_leg(df$rent,          c_priv, g_rent)
  # Companions ride the parent gate
  df$sole_prop1 = conceal_leg(df$sole_prop1, c_priv, g_sole)
  df$sole_prop2 = conceal_leg(df$sole_prop2, c_priv, g_sole)
  df$part_se1   = conceal_leg(df$part_se1,   c_priv, g_parta)
  df$part_se2   = conceal_leg(df$part_se2,   c_priv, g_parta)

  #--- Conservation identity (hard assert) -------------------------------------
  # Per leg, reported (post) + hidden = pre-avoidance, recomputed independently
  # from the pre-snapshot and the concealment fraction. Catches wrong-column /
  # wrong-fraction mappings, gate inconsistencies, and NA leakage.
  legs = list(
    list(post = df$txbl_int,      hid = hid_int,   pre = pre$txbl_int,      c = c_pub,  gate = g_int),
    list(post = df$div_ord,       hid = hid_dord,  pre = pre$div_ord,       c = c_pub,  gate = g_dord),
    list(post = df$div_pref,      hid = hid_dprf,  pre = pre$div_pref,      c = c_pub,  gate = g_dprf),
    list(post = df$kg_lt,         hid = hid_kg,    pre = pre$kg_lt,         c = c_pub,  gate = g_kg),
    list(post = df$part_active,   hid = hid_parta, pre = pre$part_active,   c = c_priv, gate = g_parta),
    list(post = df$part_passive,  hid = hid_partp, pre = pre$part_passive,  c = c_priv, gate = g_partp),
    list(post = df$scorp_active,  hid = hid_scrpa, pre = pre$scorp_active,  c = c_priv, gate = g_scrpa),
    list(post = df$scorp_passive, hid = hid_scrpp, pre = pre$scorp_passive, c = c_priv, gate = g_scrpp),
    list(post = df$sole_prop,     hid = hid_sole,  pre = pre$sole_prop,     c = c_priv, gate = g_sole),
    list(post = df$rent,          hid = hid_rent,  pre = pre$rent,          c = c_priv, gate = g_rent))
  # RELATIVE per-record error with a $1 floor. Absolute float rounding scales
  # with the flow level — past-2040 nominal dollars (~1e11 per record at the
  # top) broke an absolute 1e-6 bar on pure double-precision noise in the
  # 30-yr batch (2026-07-10). Any real mapping/factor bug is O(1) relative,
  # so 1e-9 relative keeps the guard's full diagnostic power at any horizon.
  max_err = 0
  for (lg in legs) {
    pre0 = replace_na(lg$pre, 0)
    den  = pmax(abs(pre0), 1)
    # identity: post + hidden == pre (exact restatement, guards NA/arithmetic)
    max_err = max(max_err, max(abs((lg$post + lg$hid) - lg$pre) / den, na.rm = TRUE))
    # independent hidden recompute from the fraction (guards column/factor swaps)
    exp_hid = ifelse(lg$gate, pre0 * lg$c, 0)
    max_err = max(max_err, max(abs(lg$hid - exp_hid) / den, na.rm = TRUE))
  }
  if (!is.finite(max_err) || max_err > 1e-9) {
    stop('do_wealth(): hidden-ledger conservation identity failed (max leg ',
         'RELATIVE reconciliation error = ', format(max_err, scientific = TRUE),
         '); reported + hidden != pre-avoidance for at least one flow class.')
  }
  if (any(is.na(df$net_worth))) {
    stop('do_wealth(): NA introduced in net_worth.')
  }

  #--- Diagnostics -------------------------------------------------------------
  # One-row-per-year summary. NOTE: for s>0 wealth scenarios the conv-no-wealth
  # pass (2N) also runs this module and writes this file FIRST; the final
  # conventional pass (2C) overwrites it. 2N -> 2C ordering holds in both the
  # sequential (run_one_year) and SLURM pipelines, so the persisted file always
  # reflects the final conventional frame.
  w = df$weight
  reduced_clh_from_evasion = clh * exp(df$mtr_net_worth * WEALTH_AVOID_PRIVATE_E) * evaded
  diag = tibble(
    year                          = year,
    chi_pub                       = CHI_PUB,
    chi_priv                      = CHI_PRIV,
    public_e                      = WEALTH_AVOID_PUBLIC_E,
    private_e                     = WEALTH_AVOID_PRIVATE_E,
    version                       = WEALTH_AVOID_VERSION,
    n_records_pos_wealth_mtr      = sum(df$mtr_net_worth > 0),
    weighted_records_pos_mtr      = sum(w[df$mtr_net_worth > 0]),
    concealed_wealth_marketable   = sum(w * c_pub  * mkt),
    concealed_wealth_closely_held = sum(w * c_priv * clh),
    concealed_flow_txbl_int       = sum(w * hid_int),
    concealed_flow_div_ord        = sum(w * hid_dord),
    concealed_flow_div_pref       = sum(w * hid_dprf),
    concealed_flow_kg_lt          = sum(w * hid_kg),
    concealed_flow_part_active    = sum(w * hid_parta),
    concealed_flow_part_passive   = sum(w * hid_partp),
    concealed_flow_scorp_active   = sum(w * hid_scrpa),
    concealed_flow_scorp_passive  = sum(w * hid_scrpp),
    concealed_flow_sole_prop      = sum(w * hid_sole),
    concealed_flow_rent           = sum(w * hid_rent),
    evasion_link_clh_reduction    = sum(w * reduced_clh_from_evasion),
    conservation_max_leg_err      = max_err)

  diag_dir = file.path(scenario_info$output_path, 'conventional', 'supplemental')
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(diag, file.path(diag_dir, paste0('hidden_ledger_', year, '.csv')))

  #--- Return ------------------------------------------------------------------
  # Drop the joined MTR only. The evasion_g_* factors and the persisted
  # wealth_c_* fractions ride through to estate/avoidance (required later in
  # the stack), which consumes and drops them.
  df %>%
    select(-mtr_net_worth) %>%
    return()
}
