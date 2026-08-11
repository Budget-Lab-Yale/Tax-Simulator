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
    'wages1',                    # (dbl) primary wages (spouse tax adjustment)
    'wages2',                    # (dbl) secondary wages (spouse tax adjustment)
    'filing_status',             # (int) 1 single, 2 MFJ, 3 MFS, 4 HoH
    'age1',                      # (int) age of primary filer
    'age2',                      # (int) age of secondary filer (NA if none)
    'blind1',                    # (bool) whether primary filer is blind
    'blind2',                    # (bool) whether secondary filer is blind
    'st_ord.rates[]',            # (dbl) state marginal rates
    'st_ord.brackets[]',         # (dbl) state bracket lower bounds
    'st_ord.recapture_agi_start', # (dbl) recapture trigger (Inf = none)
    'st_ord.recapture_width',    # (dbl) recapture phase-in width
    'st_ord.sta_max',            # (dbl) spouse tax adjustment cap (VA; 0 = none)
    'st_ord.combined_sep',       # (int) combined-return separate filing (KY)
    'st_ord.bus_rate',           # (dbl) flat rate on carve-out excess (OH 3%)
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

  # Schedule tax at an arbitrary income vector. A state with fewer brackets
  # than the widest state in the law slice carries trailing NA bracket
  # columns; the NA upper bound would silently drop the top bracket's tax,
  # so treat it as Inf (the NA brackets' own terms still drop via na.rm)
  sched_tax_at = function(y) {
    upper = cbind(br[, -1, drop = F], Inf)
    upper[is.na(upper)] = Inf
    rowSums(rt * pmax(0, pmin(y, upper) - br), na.rm = T)
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

  # Published base-amount schedule (OH 5747.02): where the base_amounts
  # family is encoded, tax = base_j + rate_j x (TI - bracket_j) with the
  # statutory base amounts transcribed as published. This preserves the
  # form's discontinuities (the zero-bracket cliff and the 2025 $100,000
  # jump) that a smooth marginal schedule cannot represent
  base_amt = st_family_matrix(tax_unit, 'st_ord.base_amounts')
  if (!is.null(base_amt)) {
    sched_published = st_pick_slot(base_amt, j) + m * (ti - B)
    tax_unit$st_tax_sched = if_else(is.na(sched_published),
                                    tax_unit$st_tax_sched, sched_published)
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
        st_ord.bus_rate * st_txbl_bus,

      # Married filing separately on a combined return (KY Form 740 filing
      # status 2): each spouse's column applies the schedule to own income
      # less own deduction and exemptions, floored at zero per column, and
      # the couple takes the lower of joint and combined tax. Column income
      # is own wages plus half of non-wage state AGI (asset ownership
      # unobserved; VA STA precedent, documented approximation). Itemized
      # deductions divide by each spouse's income share (Form 740 Schedule
      # A: "based on the percentage of each spouse's income to total
      # income"); the standard deduction is one full amount per column.
      # Assumes no recapture, base-amount, or business carve-out machinery
      # in combined_sep states (see params_schema.yaml)
      cs_share1 = wages1 + sta_other,
      cs_share2 = wages2 + sta_other,
      cs_item_shr1 = if_else(st_agi > 0, pmax(0, pmin(1, cs_share1 / st_agi)), 0.5),
      cs_ded1 = if_else(st_itemizing, st_ded * cs_item_shr1,        st_std_ded),
      cs_ded2 = if_else(st_itemizing, st_ded * (1 - cs_item_shr1),  st_std_ded),
      cs_tax  = sched_tax_at(pmax(0, cs_share1 - cs_ded1 - sta_pe1)) +
                sched_tax_at(pmax(0, cs_share2 - cs_ded2 - sta_pe2)),
      st_tax_pre_credit = if_else(
        st_ord.combined_sep == 1 & filing_status == 2,
        pmin(st_tax_pre_credit, cs_tax),
        st_tax_pre_credit
      )
    ) %>%
    select(all_of(return_vars$calc_st_tax)) %>%
    return()
}
