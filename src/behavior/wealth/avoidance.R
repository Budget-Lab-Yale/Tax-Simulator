#-------------------------------------------------------------------------------
# wealth/avoidance.R
#
# Contains the wealth tax avoidance response
#-------------------------------------------------------------------------------

# Assume that reported wealth falls with the marginal wealth tax rate, at
# different elasticities for two classes of asset: marketable wealth, which is
# publicly traded and liquid, and closely held business and other nonfinancial
# wealth. The two values below imply that a 3% marginal rate erodes about 19% of
# reported marketable wealth and about 40% of reported closely held wealth.
#
# The response splits into two kinds. Concealment means the money leaves the tax
# authority's sight altogether, so its income and its estate value disappear from
# the reported bases too. Valuation gaming means the assessed value is lowballed
# while the income is still visibly received. Marketable wealth can only be
# concealed, since an exchange price cannot be argued down; closely held wealth is
# assumed to split evenly between the two, because private business discounts are
# real and legal. Homes take the same even split.
#
# The two kinds are tracked as a hidden amount per record. Every reported base
# reads from it. No real base ever does.
#
# There are two further consistency rules, neither of them a new elasticity.
# Income the evasion module hides pulls the matching closely held assets out of
# reported wealth. And concealed wealth also escapes the reported estate at death,
# which estate/avoidance.R carries; a scenario running this module without that one
# is refused before the run starts. Reported gains scale down by the concealed
# share of marketable wealth, after the gains model has set realizations, which
# scales the gain and not the price.
#
# The firewall matters: concealment touches only the inputs to the tax
# calculation. The reported income columns, which feed do_taxes and nothing else;
# the materialized net worth column, which is where the wealth tax reads; and,
# through the estate module, the concealed share the estate tax reads. It never
# scales the asset columns, which every other channel reads as the true balance
# sheet.
#-------------------------------------------------------------------------------

WEALTH_AVOID_VERSION  = paste('2026-07-08 hidden-ledger (R1-R7);',
                              'elasticities author-accepted (seeded from standalone);',
                              '2026-07-16 estate response split out to estate/avoidance')

# The elasticities. Here rather than in config because this module is the only
# thing that reads them. A band is a copy of this file with different numbers,
# listed by a different behavior alternative.
#
# Reported wealth scales by exp(mtr_net_worth * e). Both values came from the
# standalone wealth tax model and were accepted as reasonable centrals on
# 2026-07-08. Neither is calibrated. The closely held value is the largest
# behavioral magnitude in the model, and the first thing a sensitivity check
# should vary.
WEALTH_AVOID_PUBLIC_E  = -7    # marketable / publicly valued wealth
WEALTH_AVOID_PRIVATE_E = -17   # closely held, where valuation discretion is wider

# How much of the response is concealment rather than valuation gaming. Full
# concealment for marketable wealth, since an exchange price cannot be argued down,
# so a marketable asset that leaves the reported balance sheet has really gone.
# Half for closely held wealth, since private business discounts are real and legal.
#
# Setting both to zero leaves reported net worth shrinking by the full response
# while the income, gain and estate concealment do nothing.
CHI_PUB  = 1.0
CHI_PRIV = 0.5


do_wealth = function(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Reduces the reported wealth base, and with it the income, gains and estate that
  # concealed wealth would have shown.
  #
  # Reported net worth falls by the two elasticities applied to the marketable and
  # closely held components, with the closely held term shaved further by whatever
  # share of the record's income the evasion module hid. The result is written back
  # to the materialized net worth column only. The asset columns are left alone, so
  # the estate tax and capital income, which read those, see the concealment only
  # through the channels below.
  #
  # Concealed marketable wealth stops producing reported interest, dividends and
  # gains. Concealed closely held wealth stops producing reported pass-through and
  # rent income, with the earner splits scaled alongside. All of those columns feed
  # only the tax calculation.
  #
  # The estate is left to estate/avoidance.R, which a scenario running this module
  # must also run. The two concealed shares are put on the record for it, and it
  # combines them with income evasion and the estate's own-rate response.
  #
  # This is written out rather than being a single call to apply_mtr_elasticity(),
  # because the response scales two component sums and many reported income legs
  # rather than one registered variable. It reads the marginal rate through the same
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

  # Ordering is handled in src/sim/behavior.R: evasion runs before this module,
  # since the link below reads what it hid; the gains model runs before it, or the
  # scaling of reported gains would be overwritten; and the estate module runs
  # after, so the concealment reaches the reported estate.

  # The wealth marginal rate must be registered for this module to do anything.
  # Stop rather than skip the response silently, which would label a static score
  # as conventional.
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

  # Default the evasion factors to no evasion where that module did not run, so the
  # link below does nothing but the code path stays the same.
  for (g in c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent')) {
    if (!(g %in% names(tax_units))) tax_units[[g]] = 1
  }

  # Sums columns per record, treating a missing cell as zero
  sum_cols = function(d, cols) {
    m = as.matrix(d[, cols, drop = FALSE])
    m[is.na(m)] = 0
    rowSums(m)
  }

  df = tax_units %>%
    left_join(static_mtrs %>% select(id, year, mtr_net_worth),
              by = c('id', 'year')) %>%
    mutate(mtr_net_worth = replace_na(mtr_net_worth, 0))

  # Component sums off the raw balance sheet, which is left alone
  mkt   = sum_cols(df, WEALTH_MARKETABLE_COLS)
  clh   = sum_cols(df, WEALTH_CLOSELY_HELD_COLS)
  debts = sum_cols(df, WEALTH_DEBT_COLS)
  gross = mkt + clh

  # The avoided share of each class
  f_pub  = 1 - exp(df$mtr_net_worth * WEALTH_AVOID_PUBLIC_E)
  f_priv = 1 - exp(df$mtr_net_worth * WEALTH_AVOID_PRIVATE_E)
  # And the part of it that is concealment rather than valuation
  c_pub  = CHI_PUB  * f_pub
  c_priv = CHI_PRIV * f_priv
  # Put on the record for the estate module, which consumes and drops them
  df$wealth_c_pub  = c_pub
  df$wealth_c_priv = c_priv

  # How much of the record's closely held income the evasion module hid, weighted
  # across its positive legs. Only the parent legs count; the companions are earner
  # splits of the same income.
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

  # Reported net worth. The whole response, both valuation and concealment, shrinks
  # the base, and the closely held part shrinks further by the evaded income share.
  # Debts are not avoided, there being no reason to.
  df$net_worth = mkt * exp(df$mtr_net_worth * WEALTH_AVOID_PUBLIC_E) +
                 clh * exp(df$mtr_net_worth * WEALTH_AVOID_PRIVATE_E) * (1 - evaded) -
                 debts

  # Concealed assets stop producing reported income. Only positive legs are scaled,
  # so a concealer keeps a loss intact.
  #
  # Retirement distributions are deliberately excluded. Retirement balances sit in
  # the marketable class for the avoidance elasticity, but the accounts are reported
  # by third parties and cannot realistically be concealed.
  #
  # The concealed dollars are tracked explicitly rather than inferred, so that
  # checking reported plus hidden against the original is real arithmetic rather
  # than a restatement of the scaling.

  # Records the concealed dollars for one leg and returns the leg net of them,
  # doing nothing where the gating leg is not positive. The gate argument lets an
  # earner split follow its parent leg.
  conceal_leg = function(pre, c, gate) {
    pre0 = replace_na(pre, 0)
    ifelse(gate, pre0 * (1 - c), pre)
  }
  hidden_leg = function(pre, c, gate) {
    pre0 = replace_na(pre, 0)
    ifelse(gate, pre0 * c, 0)
  }

  # Which legs are positive, before anything is scaled
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

  # Keep the original legs, for the check below and the diagnostics
  pre = list(
    txbl_int = df$txbl_int, div_ord = df$div_ord, div_pref = df$div_pref,
    kg_lt = df$kg_lt,
    part_active = df$part_active, part_passive = df$part_passive,
    scorp_active = df$scorp_active, scorp_passive = df$scorp_passive,
    sole_prop = df$sole_prop, rent = df$rent,
    sole_prop1 = df$sole_prop1, sole_prop2 = df$sole_prop2,
    part_se1 = df$part_se1, part_se2 = df$part_se2)

  # Marketable wealth: interest, dividends, and realized long-term gains. The gain
  # is scaled and the basis is not.
  hid_int  = hidden_leg(df$txbl_int, c_pub, g_int)
  hid_dord = hidden_leg(df$div_ord,  c_pub, g_dord)
  hid_dprf = hidden_leg(df$div_pref, c_pub, g_dprf)
  hid_kg   = hidden_leg(df$kg_lt,    c_pub, g_kg)
  df$txbl_int = conceal_leg(df$txbl_int, c_pub, g_int)
  df$div_ord  = conceal_leg(df$div_ord,  c_pub, g_dord)
  df$div_pref = conceal_leg(df$div_pref, c_pub, g_dprf)
  df$kg_lt    = conceal_leg(df$kg_lt,    c_pub, g_kg)

  # Closely held wealth: pass-through and rent income, with the earner splits
  # following their parent legs so the payroll base stays consistent
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
  # The earner splits follow their parents
  df$sole_prop1 = conceal_leg(df$sole_prop1, c_priv, g_sole)
  df$sole_prop2 = conceal_leg(df$sole_prop2, c_priv, g_sole)
  df$part_se1   = conceal_leg(df$part_se1,   c_priv, g_parta)
  df$part_se2   = conceal_leg(df$part_se2,   c_priv, g_parta)

  # Check, leg by leg, that what is reported plus what is hidden equals what was
  # there before, recomputing the hidden part independently. This catches a leg
  # mapped to the wrong fraction, an inconsistent gate, and missing values.
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
  # Compare relative error, with a floor of a dollar. Rounding error scales with
  # the size of the flow, and past 2040 the largest records run to $100bn, which
  # broke an absolute tolerance on rounding alone. A real error here is order one
  # in relative terms, so a relative tolerance loses nothing.
  max_err = 0
  for (lg in legs) {
    pre0 = replace_na(lg$pre, 0)
    den  = pmax(abs(pre0), 1)
    # Reported plus hidden against the original
    max_err = max(max_err, max(abs((lg$post + lg$hid) - lg$pre) / den, na.rm = TRUE))
    # And the hidden part recomputed from the fraction, which catches a swap
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

  # One row per year. Note that under the wealth channel the pass that measures the
  # tax change also runs this module and writes this file first; the final
  # conventional pass then overwrites it. That order holds in both the sequential
  # and the SLURM pipelines, so the file always reflects the final frame.
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

  # Drop only the joined marginal rate. The evasion factors and the concealed
  # shares carry through to the estate module, which consumes and drops them.
  df %>%
    select(-mtr_net_worth) %>%
    return()
}
