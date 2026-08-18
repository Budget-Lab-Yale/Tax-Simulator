#-------------------------------------------------------------
# Function to calculate state tax before credits (rate
# schedule plus tax-benefit recapture where applicable,
# continuous NY-style or stepped CT-style)
#-------------------------------------------------------------

# Set return variables for function
return_vars$calc_st_tax = c('st_tax_pre_credit')


calc_st_tax = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates state tax before credits: the rate/bracket schedule applied to
  # state taxable income, plus the NY-style tax-benefit recapture
  # (supplemental tax) where st_ord.recapture_agi_start is finite.
  #
  # Recapture implements the IT-201 worksheet identity (verified against the
  # published 2017-2025 worksheets; see plan research notes): with B the
  # lower bound of the taxpayer's taxable-income bracket, m its rate, m_prev
  # the rate below, T() the schedule tax, and S0 the recapture AGI trigger:
  #   RB  = m_prev*B - T(B)                    if B > S0, else 0
  #   phi = clamp((st_agi - max(B, S0)) / width, 0, 1)
  #   tax = T(TI) + RB + (m*TI - T(TI) - RB) * phi
  # Once st_agi >= B + width, tax = m*TI (all lower-bracket benefit
  # recaptured). Units with st_agi above the top bracket (the 25M rule,
  # 2022+) pay the top rate flat with no phase-in; the 2021-only $50k
  # phase above $25M is approximated by the same flat rule (negligible).
  #
  # Independently, CT-style STEPPED recapture segments (CT-1040 TCS phase-out
  # add-back and tax recapture tables) add, for each encoded segment s with
  # st_agi above its start:
  #   min(ceil((st_agi - start_s) / incr_s) * amount_s, max_s)
  # The segment vectors (st_ord.step_recap_*) are filing-status mapped and
  # absent for states without the feature (gated on step_recap_start1).
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_tax_pre_credit (dbl) : state tax before credits
  #----------------------------------------------------------------------------

  req_vars = c(
    'st_agi',                    # (dbl) state income base
    'st_txbl_inc',               # (dbl) state taxable income
    'st_bus_excess',             # (dbl) business income above the carve-out cap
    'kg_pref',                   # (dbl) net capital gain (alternative-rate base)
    'kg_st',                     # (dbl) short-term capital gains (separate-rate base)
    'wages1',                    # (dbl) primary wages (spouse tax adjustment)
    'wages2',                    # (dbl) secondary wages (spouse tax adjustment)
    'filing_status',             # (int) 1 single, 2 MFJ, 3 MFS, 4 HoH
    'age1',                      # (int) age of primary filer
    'age2',                      # (int) age of secondary filer (NA if none)
    'blind1',                    # (bool) whether primary filer is blind
    'blind2',                    # (bool) whether secondary filer is blind
    'st_ord.rates[]',            # (dbl) state marginal rates
    'st_ord.brackets[]',         # (dbl) state bracket lower bounds
    'st_ord.kg_alt_rate',        # (dbl) alternative max rate on net capital gain (HI; Inf = none)
    'st_ord.kg_alt_floor',       # (dbl) ordinary-income floor for the alternative rate (mapped)
    'st_ord.recapture_agi_start', # (dbl) recapture trigger (Inf = none)
    'st_ord.recapture_width',    # (dbl) recapture phase-in width
    'st_ord.sta_max',            # (dbl) spouse tax adjustment cap (VA; 0 = none)
    'st_ord.combined_sep',       # (int) combined-return separate filing (KY)
    'st_ord.combined_sep_std_share', # (dbl) share of the mapped std deduction per column (1 = per-person std, KY; 0.5 = joint is twice the column amount, DE)
    'st_ord.combined_split',     # (int) pooled deductions, taxable income split by income share (MO)
    'st_ord.combined_split_round', # (dbl) rounding increment for the income shares (MO 0.01)
    'st_ord.bus_rate',           # (dbl) flat rate on carve-out excess (OH 3%)
    'st_ord.st_gains_rate',      # (dbl) separate rate on short-term capital gains (MA)
    'st_itemizing',              # (bool) state itemization election (calc_st_ded)
    'st_ded',                    # (dbl) state deduction taken (calc_st_ded)
    'st_std_ded',                # (dbl) state standard deduction (calc_st_ded)
    'st_exempt.personal_amount', # (dbl) per-taxpayer exemption (STA feeder)
    'st_exempt.aged_addl',       # (dbl) aged exemption add-on (STA feeder)
    'st_exempt.blind_addl'       # (dbl) blind exemption add-on (STA feeder)
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Business carve-out split (OH): taxable income already nets exemptions
  # against the whole base, so capping the carve-out at taxable income
  # implements the ORC 5747.02(A)(4)(b) rule that unused exemptions offset
  # business income; the schedule applies to the nonbusiness remainder and
  # the excess is taxed flat at bus_rate below
  tax_unit %<>%
    mutate(
      st_txbl_bus    = pmin(st_bus_excess, st_txbl_inc),
      st_txbl_nonbus = st_txbl_inc - st_txbl_bus
    )

  # Schedule tax on (nonbusiness) taxable income
  tax_unit %<>%
    bind_cols(
      integrate_rates_brackets(
        df              = .,
        n_brackets      = NULL,
        prefix_brackets = 'st_ord.brackets',
        prefix_rates    = 'st_ord.rates',
        y               = 'st_txbl_nonbus',
        output_name     = 'st_tax_sched',
        by_bracket      = F
      )
    )

  # Recapture via bracket matrices
  br = st_family_matrix(tax_unit, 'st_ord.brackets', require_sentinel = FALSE)
  rt = st_family_matrix(tax_unit, 'st_ord.rates', elements = 1:ncol(br),
                        require_sentinel = FALSE)
  n_br = ncol(br)

  # Published base-amount schedule (OH 5747.02; MO-1040 tax chart), where
  # encoded: tax = base_j + rate_j x (y - bracket_j) with the statutory base
  # amounts transcribed as published. This preserves form discontinuities
  # that a smooth marginal schedule cannot represent -- OH's zero-bracket
  # cliff and 2025 $100,000 jump, and the MO chart's whole-dollar base
  # amounts (2023: $180 at $7,242 where continuous integration gives
  # $181.05)
  base_amt = st_family_matrix(tax_unit, 'st_ord.base_amounts')

  # Schedule tax at an arbitrary income vector. A state with fewer brackets
  # than the widest state in the law slice carries trailing NA bracket
  # columns; the NA upper bound would silently drop the top bracket's tax,
  # so treat it as Inf (the NA brackets' own terms still drop via na.rm).
  # Rows of a base-amount state take the published schedule; every other row
  # (including every row of a mixed law slice whose state has no base-amount
  # family) keeps the smooth integration unchanged
  sched_tax_at = function(y) {
    upper = cbind(br[, -1, drop = F], Inf)
    upper[is.na(upper)] = Inf
    smooth = rowSums(rt * pmax(0, pmin(y, upper) - br), na.rm = T)
    if (is.null(base_amt)) {
      return(smooth)
    }
    jy = st_band_index_lower(y, br)
    published = st_pick_slot(base_amt, jy) +
                st_pick_slot(rt, jy) * (y - st_pick_slot(br, jy))
    if_else(is.na(published), smooth, published)
  }

  # Stepped recapture segments (CT-style), zero when not encoded: for each
  # segment, per-step add-back of st_agi above the segment start, capped at
  # the segment maximum
  step_recap = rep(0, nrow(tax_unit))
  s_strt = st_family_matrix(tax_unit, 'st_ord.step_recap_start')
  if (!is.null(s_strt)) {
    seg    = 1:ncol(s_strt)
    s_incr = st_family_matrix(tax_unit, 'st_ord.step_recap_incr',   seg, F)
    s_amt  = st_family_matrix(tax_unit, 'st_ord.step_recap_amount', seg, F)
    s_max  = st_family_matrix(tax_unit, 'st_ord.step_recap_max',    seg, F)
    step_recap = rowSums(
      pmin(st_step_reduction(tax_unit$st_agi, s_strt, s_incr, s_amt), s_max),
      na.rm = T
    )
  }

  ti = tax_unit$st_txbl_nonbus
  j  = st_band_index_lower(ti, br)             # taxpayer's bracket index
  m      = st_pick_slot(rt, j)
  B      = st_pick_slot(br, j)
  m_prev = st_pick_slot(rt, pmax(1, j - 1))

  # Apply the published base-amount schedule to taxable income itself. Kept
  # as an explicit override of the integrate_rates_brackets result (rather
  # than a sched_tax_at call) so that rows of a mixed law slice whose state
  # has no base-amount family keep that function's value untouched
  if (!is.null(base_amt)) {
    sched_published = st_pick_slot(base_amt, j) + m * (ti - B)
    tax_unit$st_tax_sched = if_else(is.na(sched_published),
                                    tax_unit$st_tax_sched, sched_published)
  }

  # Alternative maximum rate on net capital gain (HI 235-51(f), Tax on
  # Capital Gains Worksheet): ordinary-taxed income is the GREATER of
  # (TI - net capital gain) and the filing-status floor; the remainder is
  # taxed at kg_alt_rate, and the unit pays the smaller of that and the
  # regular schedule tax. kg_pref is the IRC 1222 net capital gain
  # (qualified dividends excluded), matching the worksheet's smaller-of
  # LTCG/total-gain construction. Assumes no recapture/base-amount/
  # combined-separate machinery in the same state (params_schema.yaml)
  if (any(is.finite(tax_unit$st_ord.kg_alt_rate))) {
    kg_alt_ord = pmin(ti, pmax(ti - pmax(0, tax_unit$kg_pref),
                               tax_unit$st_ord.kg_alt_floor))
    kg_alt_tax = sched_tax_at(kg_alt_ord) +
                 tax_unit$st_ord.kg_alt_rate * (ti - kg_alt_ord)
    tax_unit$st_tax_sched = if_else(is.finite(tax_unit$st_ord.kg_alt_rate),
                                    pmin(tax_unit$st_tax_sched, kg_alt_tax),
                                    tax_unit$st_tax_sched)
  }

  tax_unit %>%
    mutate(
      recap_S0  = st_ord.recapture_agi_start,
      recap_on  = is.finite(recap_S0) & st_agi > recap_S0,
      recap_RB  = if_else(B > recap_S0, pmax(0, m_prev * B - sched_tax_at(B)), 0),
      recap_phi = pmin(1, pmax(0, (st_agi - pmax(B, recap_S0)) /
                                   st_ord.recapture_width)),
      flat_top  = recap_on & st_agi > br[, n_br] & br[, n_br] >= 5e6,

      # Spouse tax adjustment (VA Form 760 Line 17 worksheet): MFJ couples
      # recompute the schedule tax as if each spouse's share of taxable
      # income were taxed separately, capped at sta_max. Each spouse's
      # separate VAGI is own wages plus half of non-wage VAGI (joint asset
      # ownership unobserved; documented approximation), less own personal
      # and aged/blind exemptions; both nets must be positive. The notional
      # split is capped at 50/50 per the worksheet's min/max against half of
      # joint taxable income. Booklet tax-table rounding (the published $259
      # vs the continuous $257.50 maximum) is a documented known-difference
      sta_other = (st_agi - wages1 - wages2) / 2,
      sta_pe1   = st_exempt.personal_amount +
                  st_exempt.aged_addl * (age1 >= 65) +
                  st_exempt.blind_addl * coalesce(blind1, 0),
      sta_pe2   = st_exempt.personal_amount +
                  st_exempt.aged_addl * (!is.na(age2) & age2 >= 65) +
                  st_exempt.blind_addl * (!is.na(blind2) & blind2),
      sta_net1  = wages1 + sta_other - sta_pe1,
      sta_net2  = wages2 + sta_other - sta_pe2,
      sta_low   = pmin(sta_net1, sta_net2),
      sta_high  = st_txbl_inc - sta_low,
      st_sta    = if_else(
        st_ord.sta_max > 0 & filing_status == 2 &
          sta_net1 > 0 & sta_net2 > 0,
        pmin(st_ord.sta_max,
             pmax(0, st_tax_sched -
                     sched_tax_at(pmin(sta_low,  st_txbl_inc / 2)) -
                     sched_tax_at(pmax(sta_high, st_txbl_inc / 2)))),
        0
      ),

      st_tax_pre_credit = case_when(
        flat_top ~ rt[, n_br] * st_txbl_nonbus,
        recap_on ~ st_tax_sched + recap_RB +
                   pmax(0, m * st_txbl_nonbus - st_tax_sched - recap_RB) * recap_phi,
        TRUE     ~ st_tax_sched
      ) + step_recap - st_sta +

        # Flat tax on business income above the carve-out cap (OH 3%)
        st_ord.bus_rate * st_txbl_bus +

        # Separate rate on SHORT-TERM capital gains (MA Part A: 12% through
        # TY2022, 8.5% from TY2023). Those gains are held out of the ordinary
        # base by st_agi.ob_st_gains_share, so this is the whole tax on them,
        # not a supplement. Losses give no relief here -- Massachusetts nets
        # them under its own regime, which is a documented known difference
        st_ord.st_gains_rate * pmax(0, kg_st),

      # Married filing separately on a combined return (KY Form 740 filing
      # status 2): each spouse's column applies the schedule to own income
      # less own deduction and exemptions, floored at zero per column, and
      # the couple takes the lower of joint and combined tax. Column income
      # is own wages plus half of non-wage state AGI (asset ownership
      # unobserved; VA STA precedent, documented approximation). Itemized
      # deductions divide by each spouse's income share (Form 740 Schedule
      # A: "based on the percentage of each spouse's income to total
      # income"); each column takes combined_sep_std_share of the mapped
      # standard deduction -- the whole amount where the state's std is per
      # person (KY), half where the joint amount is twice the per-column
      # amount (DE). Where only one spouse is aged/blind the split of the
      # add-ons between columns is approximate, though their total is right.
      # Assumes no recapture, base-amount, or business carve-out machinery
      # in combined_sep states (see params_schema.yaml)
      cs_share1 = wages1 + sta_other,
      cs_share2 = wages2 + sta_other,
      cs_item_shr1 = if_else(st_agi > 0, pmax(0, pmin(1, cs_share1 / st_agi)), 0.5),
      cs_ded1 = if_else(st_itemizing, st_ded * cs_item_shr1,
                        st_std_ded * st_ord.combined_sep_std_share),
      cs_ded2 = if_else(st_itemizing, st_ded * (1 - cs_item_shr1),
                        st_std_ded * st_ord.combined_sep_std_share),
      cs_tax  = sched_tax_at(pmax(0, cs_share1 - cs_ded1 - sta_pe1)) +
                sched_tax_at(pmax(0, cs_share2 - cs_ded2 - sta_pe2)),
      st_tax_pre_credit = if_else(
        st_ord.combined_sep == 1 & filing_status == 2,
        pmin(st_tax_pre_credit, cs_tax),
        st_tax_pre_credit
      ),

      # Married filing COMBINED (MO-1040 filing status 2), a different
      # construction from KY's combined-separate columns above: Missouri
      # pools every deduction at the return level (Line 25), subtracts them
      # from combined Missouri AGI (Line 26), then splits the resulting
      # TAXABLE income between spouses by their shares of Missouri AGI
      # (Lines 7Y/7S x Line 26 = Lines 27Y/27S) and runs the same schedule on
      # each share (Lines 30Y/30S, "A separate tax must be computed for you
      # and your spouse"). There is no better-of election -- the combined
      # return is mandatory for couples filing jointly federally.
      #
      # Each spouse's Missouri AGI is own wages plus half of non-wage state
      # AGI (asset ownership unobserved; the VA STA / KY combined-separate
      # convention). The form rounds the shares to whole percent and requires
      # them to sum to 100%, so share2 is the complement of the rounded
      # share1; where one spouse's income is negative and the other's is
      # positive the form assigns 0%/100%, which the clamp reproduces
      csp_share1 = if_else(st_agi > 0,
                           pmin(1, pmax(0, cs_share1 / st_agi)),
                           0.5),
      # Rounded with floor(x + 0.5), NOT R's round(): the form rounds a half
      # UP ("97.5 percent would be shown as 98 percent") while round() would
      # send it to the even neighbour, 98 here but 96 at 96.5
      csp_share1 = if_else(is.finite(st_ord.combined_split_round) &
                             st_ord.combined_split_round > 0,
                           floor(csp_share1 / st_ord.combined_split_round + 0.5) *
                             st_ord.combined_split_round,
                           csp_share1),
      csp_tax = sched_tax_at(st_txbl_inc * csp_share1) +
                sched_tax_at(st_txbl_inc * (1 - csp_share1)),
      st_tax_pre_credit = if_else(
        st_ord.combined_split == 1 & filing_status == 2,
        csp_tax,
        st_tax_pre_credit
      )
    ) %>%
    select(all_of(return_vars$calc_st_tax)) %>%
    return()
}
