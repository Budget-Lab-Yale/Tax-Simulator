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
#   R4  Concealed wealth also escapes the reported ESTATE at death (via the
#       estate_concealed_frac column read by calc_estate)
#   R5  Homes keep the uniform 50/50 split (no home-specific chi)
#   R6  Capital gains are IN v1 as a reporting-QUANTITY overlay: reported kg_lt
#       scales by (1 - c_pub) AFTER the kg module sets realization behavior (the
#       GAIN is scaled, not the price; no basis change, no value.* change)
#   R7  chi wired as env knobs (below), read once, stamped into diagnostics
#
# The IRON RULE (from the audit): concealment touches ONLY tax-computation
# inputs -- reported income-flow columns (they feed do_taxes and nothing else),
# the materialized net_worth column (the documented isolation point calc_wealth
# reads), and the optional estate_concealed_frac input to calc_estate. It NEVER
# scales value.*, which every real-side channel reads as the true balance sheet.
#-------------------------------------------------------------------------------

WEALTH_AVOID_VERSION  = paste('2026-07-08 hidden-ledger (R1-R7);',
                              'elasticities author-accepted (seeded from standalone);',
                              '2026-07-12 + estate own-rate response (KS)')
WEALTH_AVOID_PUBLIC_E  = -7
WEALTH_AVOID_PRIVATE_E = -17

#-------------------------------------------------------------------------------
# ESTATE_AVOID_PROVENANCE
#
# Reported-estate OWN-RATE response (estate-margins build part (b), plan
# effervescent-plotting-wadler rev 3). Kopczuk-Slemrod (2001, "Dying to save
# taxes", REStat 83(2)) estimate an elasticity of REPORTED estates with
# respect to the net-of-estate-tax rate of ~0.16 (their preferred pooled
# estimates run ~0.10-0.22 across specifications; publish the band, not the
# point). The internally consistent finite-change functional form is the
# EXACT net-of-tax power form
#
#   retained = ((1 - tau_S) / (1 - tau_B)) ^ ESTATE_REPORT_EPS
#   f_estate = 1 - retained
#
# where tau_B / tau_S are the per-record UN-SWITCHED marginal estate rates
# (mtr_estate) under the baseline and scenario legs, both clamped to
# [0, 1 - 1e-6]. Exact for large reforms; handles newly taxable records
# (tau_B = 0 -> retained = (1 - tau_S)^eps) without applying a top-rate
# local derivative where the baseline rate is zero. (The earlier rev-2 local
# semi-elasticity -0.27 = -0.16/0.60 at the 40% top rate is RETIRED; this
# form replaces it and reproduces it to first order at the top rate.)
#
# KEY DESIGN RULE — the response keys off the CHANGE in the estate price
# (tau_S vs tau_B), NEVER the level: the LEVEL of baseline avoidance is
# already baked into the frozen valuation bridge (r, rho_pt, f_ded,
# calibrated to actual SOI reported estates in estate_valuation_params.yaml);
# a level response would double-count it. Baseline leg / unchanged estate
# law -> ratio = 1 -> exact no-op. A reform toggling ONLY
# estate.income_tax_ded cannot fire this response (mtr_estate is the
# un-switched base rate by construction).
#
# NO CHI on the own-base response: the own base IS the estate — valuation
# gaming and concealment both reduce estate_base through the same single
# estate_concealed_frac column, so CHI*f would scale KS down with the
# (1-CHI) share going nowhere. The FULL f_estate applies. CHI stays reserved
# for cross-base propagation, of which the estate own-rate response has none
# in v1 (realized at death; no during-life income-flow feedback). This is a
# deliberate asymmetry with the wealth response above.
#
# Firewall (identical to the wealth concealment): reduces estate_base ONLY,
# via estate_concealed_frac; estate_distributable (the heir allocator's
# bequest ladder) and value.* stay invariant. Stacks MULTIPLICATIVELY on the
# retained share with the existing wealth-concealment x income-evasion union
# (one hidden ledger, now THREE drivers, never > 100% hidden).
#
# Source caveats: KS's 0.16 bundles timing, avoidance, valuation, and some
# real accumulation; part of the observed reported-estate response IS
# charitable planning, which this reduced form partially absorbs (a real
# responsive-charity margin — deduction + heir resources + p_char moving
# together — is a possible follow-up build, deliberately NOT this one).
# Denominator convention: estate_concealed_frac applies to reported_gross,
# while KS's elasticity is of the reported TAXABLE estate — a slight
# overstatement above the exemption, same convention as the wealth
# concealment, inside the published band.
#
# Magnitude reconciliation: at eps = 0.16 a 40% -> 55% top-rate hike gives
# f ~= 1 - (0.45/0.60)^0.16 ~= 4.5% of the top-bracket reported estate — an
# order of magnitude below the wealth-tax concealment response (public -7 /
# private -17 imply ~19%/~40% at a 3% ANNUAL rate), as it should be: the
# estate tax is a once-at-death levy, the wealth tax recurs.
#-------------------------------------------------------------------------------
ESTATE_REPORT_EPS = as.numeric(Sys.getenv('ESTATE_REPORT_EPS', unset = '0.16'))

# Concealment shares (R7): the fraction of the avoidance RESPONSE that is
# concealment as opposed to legal valuation gaming. Env-overridable for band
# sweeps (sweep WEALTH_CHI_PRIV 0.25 / 0.5 / 0.75 for the interaction rows).
# CHI_PUB = CHI_PRIV = 0 reproduces pre-hidden-ledger behavior exactly: reported
# net_worth still shrinks by the FULL avoidance response, but the flow / kg /
# estate concealment overlays vanish (c_pub = c_priv = 0), and with evasion
# absent the evasion->wealth link is inert.
WEALTH_CHI_PUB  = as.numeric(Sys.getenv('WEALTH_CHI_PUB',  unset = '1.0'))
WEALTH_CHI_PRIV = as.numeric(Sys.getenv('WEALTH_CHI_PRIV', unset = '0.5'))


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
  # (4) Estate: estate_concealed_frac (wealth concealment union income-evasion
  #     concealment, divided by gross assets) rides the record into calc_estate,
  #     where it reduces estate_base but NOT estate_distributable (heirs inherit
  #     the hidden wealth unchanged; the income-tax-at-death deduction uses the
  #     exact same slot).
  # (5) Estate OWN-RATE response (ESTATE_AVOID_PROVENANCE above): the exact
  #     KS net-of-tax power form on the CHANGE in the un-switched marginal
  #     estate rate (mtr_estate, scenario vs baseline leg) yields f_estate,
  #     which stacks multiplicatively on the retained share with the union in
  #     (4) — one hidden ledger, three drivers. Full strength (no CHI);
  #     estate_base only; exact no-op when estate law is unchanged.
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
  #   - baseline_mtrs (df)   : baseline MTRs. mtr_net_worth is unused (the
  #                            baseline wealth MTR is 0 by construction -- no
  #                            wealth tax under current law) but mtr_estate is
  #                            LOAD-BEARING: current law has a 40% top estate
  #                            rate, so the baseline estate-MTR leg is genuinely
  #                            nonzero and the own-rate response keys off the
  #                            scenario-vs-baseline change
  #   - static_mtrs (df)     : static-counterfactual MTRs, must carry
  #                            mtr_net_worth (the statutory marginal wealth
  #                            rate) and mtr_estate (the un-switched marginal
  #                            estate rate)
  #   - scenario_info (list) : get_scenario_info() object (ID + behavior_modules
  #                            for the order guard; output_path for diagnostics)
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with net_worth overwritten by the avoided
  #          (reported) base, reported income/gain legs concealed, and the
  #          estate_concealed_frac column added for calc_estate. value.* and
  #          kg_lt_basis / kg_deemed_full unchanged.
  #----------------------------------------------------------------------------

  modules = scenario_info$behavior_modules %||% character()

  # --- Order guard (R3 / design 4.5): the evasion->wealth link reads evasion's
  # per-record response factors, so an evasion/ module -- when present -- must
  # run BEFORE wealth/avoidance. Mirrors the sigma.R pinned-order stop.
  ev_pos = which(startsWith(modules, 'evasion/'))
  wl_pos = which(startsWith(modules, 'wealth/'))
  if (length(ev_pos) > 0 && length(wl_pos) > 0 && min(ev_pos) > min(wl_pos)) {
    stop('do_wealth(): when an evasion/ module is present it must run BEFORE ',
         'wealth/avoidance (the R3 evasion->wealth consistency link reads ',
         "evasion's per-record response factors). Scenario \"",
         scenario_info$ID, '" has behavior modules: ',
         paste(modules, collapse = ' '), '.')
  }

  # --- Order guard (R6): kg_dynamics must run BEFORE wealth/avoidance. The R6
  # reporting-quantity overlay scales the REPORTED kg_lt after the kg module has
  # set realization behavior (kg overwrites kg_lt each pass); running avoidance
  # first would let kg clobber the concealment. Only transitively guaranteed via
  # the sigma/evasion guards when those modules are present, so enforce directly.
  kg_pos = which(startsWith(modules, 'kg_dynamics/'))
  if (length(kg_pos) > 0 && length(wl_pos) > 0 && min(kg_pos) > min(wl_pos)) {
    stop('do_wealth(): kg_dynamics must run BEFORE wealth/avoidance (the R6 ',
         'reported-kg_lt concealment overlay scales the realized gain the kg ',
         'module sets; running avoidance first would let kg overwrite the ',
         'concealment). Scenario "', scenario_info$ID, '" has behavior modules: ',
         paste(modules, collapse = ' '), '.')
  }

  # --- Required-MTR guard. The wealth MTR must be registered for this module to
  # do anything. Fail loudly rather than silently skipping avoidance (which
  # would mislabel a static score as conventional).
  if (is.null(static_mtrs) || !('mtr_net_worth' %in% names(static_mtrs))) {
    stop('do_wealth(): the wealth-avoidance module requires a registered ',
         'net_worth MTR (mtr_vars = "net_worth", mtr_types = "nextdollar") so ',
         'it can read the statutory marginal wealth rate. The runscript for ',
         'scenario "', scenario_info$ID, '" does not provide mtr_net_worth.')
  }

  # --- Required-MTR guard, estate legs. The own-rate response needs the
  # un-switched marginal estate rate on BOTH legs: unlike the wealth case the
  # BASELINE estate MTR is genuinely nonzero (current law has a 40% top rate),
  # so the baseline join is load-bearing. A missing baseline column usually
  # means a PRE-BUILD baseline vintage -- re-run the baseline static pass with
  # current code and "estate" registered in its mtr_vars.
  if (is.null(static_mtrs) || !('mtr_estate' %in% names(static_mtrs))) {
    stop('do_wealth(): the estate own-rate response requires a registered ',
         'estate MTR (mtr_vars = "estate", mtr_types = "nextdollar") on the ',
         'scenario row. The runscript for scenario "', scenario_info$ID,
         '" does not provide mtr_estate in static MTRs.')
  }
  if (is.null(baseline_mtrs) || !('mtr_estate' %in% names(baseline_mtrs))) {
    stop('do_wealth(): the estate own-rate response requires mtr_estate in ',
         'BASELINE MTRs (register "estate" in the baseline row\'s mtr_vars). ',
         'Missing there usually means a pre-build baseline vintage -- re-run ',
         'the baseline static pass with current code. Scenario: "',
         scenario_info$ID, '".')
  }

  message('do_wealth(): applying wealth-avoidance + hidden-ledger concealment (',
          WEALTH_AVOID_VERSION, '; public_e=', WEALTH_AVOID_PUBLIC_E,
          ', private_e=', WEALTH_AVOID_PRIVATE_E, '; CHI_PUB=', WEALTH_CHI_PUB,
          ', CHI_PRIV=', WEALTH_CHI_PRIV,
          '; estate_report_eps=', ESTATE_REPORT_EPS, ')')

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
    left_join(static_mtrs %>% select(id, year, mtr_net_worth,
                                     mtr_estate_S = mtr_estate),
              by = c('id', 'year')) %>%
    left_join(baseline_mtrs %>% select(id, year, mtr_estate_B = mtr_estate),
              by = c('id', 'year')) %>%
    mutate(mtr_net_worth = replace_na(mtr_net_worth, 0),
           mtr_estate_S  = replace_na(mtr_estate_S, 0),
           mtr_estate_B  = replace_na(mtr_estate_B, 0))

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
  c_pub  = WEALTH_CHI_PUB  * f_pub
  c_priv = WEALTH_CHI_PRIV * f_priv

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

  #--- Estate concealment (R4 + R3 cross-base extension) -----------------------
  # Wealth-tax concealment and income-tax evasion are two routes by which a
  # closely-held asset leaves the authority's sight. Combine them as a
  # multiplicative union: c_priv is hidden first, then evasion hides its share
  # of the remaining visible balance. This carries the R3 evasion->wealth link
  # through to the estate base without counting their overlap twice. Legal
  # valuation avoidance (f_priv - c_priv) remains visible to the estate, as
  # intended. The resulting fraction reduces estate_base but not
  # estate_distributable; heirs still receive hidden assets.
  estate_c_priv = c_priv + (1 - c_priv) * evaded
  estate_union  = ifelse(gross > 0,
                         (c_pub * mkt + estate_c_priv * clh) / gross, 0)

  #--- Estate OWN-RATE response (part (b); ESTATE_AVOID_PROVENANCE header) -----
  # Exact KS net-of-tax power form on the CHANGE in the un-switched marginal
  # estate rate. Both legs clamped to [0, 1 - 1e-6] before the ratio; unchanged
  # estate law -> ratio = 1 -> retained = 1 -> exact no-op (the baseline leg of
  # a conventional run never executes this module at all). Newly taxable
  # records (tau_B = 0) fall out of the same formula. A rate CUT gives
  # retained > 1, i.e. negative f_estate -- previously-unreported estate
  # surfaces (symmetric KS margin); the combined fraction below then boosts
  # estate_base. FULL response, no CHI (own-base; see provenance block).
  tau_eS = pmin(pmax(df$mtr_estate_S, 0), 1 - 1e-6)
  tau_eB = pmin(pmax(df$mtr_estate_B, 0), 1 - 1e-6)
  retained_estate = ((1 - tau_eS) / (1 - tau_eB)) ^ ESTATE_REPORT_EPS
  f_estate = 1 - retained_estate

  # Stack multiplicatively on the RETAINED share with the existing two-driver
  # union: one hidden ledger, three drivers (wealth concealment, income
  # evasion, estate own-rate), never > 100% hidden by construction
  # (estate_union <= 1 and retained_estate > 0). Floor at -1 so a pathological
  # rate cut can never more than double the reported base. Reduces (or, on
  # cuts, boosts) estate_base ONLY -- estate_distributable and value.* stay
  # invariant, same firewall as the wealth concealment.
  df$estate_concealed_frac = pmax(1 - (1 - estate_union) * retained_estate, -1)

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
  if (any(is.na(df$net_worth)) || any(is.na(df$estate_concealed_frac))) {
    stop('do_wealth(): NA introduced in net_worth or estate_concealed_frac.')
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
    chi_pub                       = WEALTH_CHI_PUB,
    chi_priv                      = WEALTH_CHI_PRIV,
    public_e                      = WEALTH_AVOID_PUBLIC_E,
    private_e                     = WEALTH_AVOID_PRIVATE_E,
    version                       = WEALTH_AVOID_VERSION,
    n_records_pos_wealth_mtr      = sum(df$mtr_net_worth > 0),
    weighted_records_pos_mtr      = sum(w[df$mtr_net_worth > 0]),
    concealed_wealth_marketable   = sum(w * c_pub  * mkt),
    concealed_wealth_closely_held = sum(w * c_priv * clh),
    estate_hidden_from_evasion    = sum(w * (1 - c_priv) * evaded * clh),
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
    estate_concealed_frac_wmean   = if (sum(w * (gross > 0)) > 0)
                                      sum(w * df$estate_concealed_frac) /
                                      sum(w * (gross > 0)) else 0,
    # Estate own-rate response (part b): parameter, per-leg gain in the
    # estate price, and the response's own contribution net of the union
    # drivers (f_estate is the multiplicative third driver)
    estate_report_eps             = ESTATE_REPORT_EPS,
    estate_mtr_B_wmean_grosspos   = if (sum(w * (gross > 0)) > 0)
                                      sum(w * tau_eB * (gross > 0)) /
                                      sum(w * (gross > 0)) else 0,
    estate_mtr_S_wmean_grosspos   = if (sum(w * (gross > 0)) > 0)
                                      sum(w * tau_eS * (gross > 0)) /
                                      sum(w * (gross > 0)) else 0,
    estate_own_rate_f_wmean       = if (sum(w * (gross > 0)) > 0)
                                      sum(w * f_estate * (gross > 0)) /
                                      sum(w * (gross > 0)) else 0,
    estate_own_rate_hidden        = sum(w * f_estate * (1 - estate_union) *
                                          gross),
    conservation_max_leg_err      = max_err)

  diag_dir = file.path(scenario_info$output_path, 'conventional', 'supplemental')
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(diag, file.path(diag_dir, paste0('hidden_ledger_', year, '.csv')))

  #--- Return ------------------------------------------------------------------
  # Drop the joined MTR and the transient evasion factors; KEEP
  # estate_concealed_frac (read by calc_estate via the frame -- do_taxes needs
  # no change, per the parse_calc_fn_input contract).
  df %>%
    select(-mtr_net_worth, -mtr_estate_S, -mtr_estate_B,
           -any_of(c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent'))) %>%
    return()
}
