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
    'st_exempt.personal_amount', # (dbl) per-taxpayer exemption (STA feeder)
    'st_exempt.aged_addl',       # (dbl) aged exemption add-on (STA feeder)
    'st_exempt.blind_addl'       # (dbl) blind exemption add-on (STA feeder)
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Schedule tax on taxable income
  tax_unit %<>%
    bind_cols(
      integrate_rates_brackets(
        df              = .,
        n_brackets      = NULL,
        prefix_brackets = 'st_ord.brackets',
        prefix_rates    = 'st_ord.rates',
        y               = 'st_txbl_inc',
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

  ti = tax_unit$st_txbl_inc
  j  = st_band_index_lower(ti, br)             # taxpayer's bracket index
  m      = st_pick_slot(rt, j)
  B      = st_pick_slot(br, j)
  m_prev = st_pick_slot(rt, pmax(1, j - 1))

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
        flat_top ~ rt[, n_br] * st_txbl_inc,
        recap_on ~ st_tax_sched + recap_RB +
                   pmax(0, m * st_txbl_inc - st_tax_sched - recap_RB) * recap_phi,
        TRUE     ~ st_tax_sched
      ) + step_recap - st_sta
    ) %>%
    select(all_of(return_vars$calc_st_tax)) %>%
    return()
}
