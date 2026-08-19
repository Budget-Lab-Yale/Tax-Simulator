#---------------------------------------------------------------------------
# Child/dependent care credit family (called by calc_st_credits): flat
# federal-CDCTC matches (GA/KY-style) and the NY styles (share table of the
# federal credit; own-rate credit on capped expenses).
#---------------------------------------------------------------------------

# Law parameters this family reads (assembled into calc_st_credits req_vars)
st_credits_care_req_vars = c(
  'st_credits.cdctc_match',
  'st_credits.cdctc_refundable',
  'st_credits.cdctc_style',
  'st_credits.cdctc_rate_max',
  'st_credits.cdctc_rate_floor',
  'st_credits.cdctc_rate_po_per_1k',
  'st_credits.cdctc_rate_po_start',
  'st_credits.cdctc_rate_po_step',
  'st_credits.cdctc_expense_ei_limit',
  'st_credits.cdctc_share_income_base',
  'st_credits.cdctc_cap_amount',
  'st_credits.cdctc_cap_thresh',
  'st_credits.cdctc_cap_po_rate',
  'st_credits.cdctc_cap_per_return',      # (int) the cap is per return, not per child (LA $25)
  'st_credits.cdctc_style_switch_agi'     # (dbl) above this federal AGI, use style 1 (LA $25,000)
)


st_credits_care = function(tax_unit) {

  #----------------------------------------------------------------------------
  # Calculates the care-credit family on a parsed tax unit tibble.
  #
  # Returns: list of per-row vectors --
  #   - st_cdctc (dbl) : state child/dependent care credit
  #----------------------------------------------------------------------------

  n = nrow(tax_unit)

  #-------------------------------
  # CDCTC share table (NY style 1)
  #-------------------------------

  cdctc_ny_share = rep(0, n)
  b_c = st_family_matrix(tax_unit, 'st_credits.cdctc_share_agi_bounds', 1:6)
  if (!is.null(b_c)) {
    s0 = st_family_matrix(tax_unit, 'st_credits.cdctc_share_start', 1:6, F)
    s1 = st_family_matrix(tax_unit, 'st_credits.cdctc_share_end',   1:6, F)
    cdctc_share_income = st_income_base(
      tax_unit, tax_unit$st_credits.cdctc_share_income_base
    )
    cdctc_ny_share = st_band_interp(cdctc_share_income, b_c, s0, s1)
  }

  #------------------------------------------------
  # CDCTC expense caps (NY style 2), by care-kid count
  #------------------------------------------------

  n_care_v = st_n_dep_in(tax_unit, 0, 12)
  cdctc_cap_vec = rep(0, n)
  caps = st_family_matrix(tax_unit, 'st_credits.cdctc_expense_caps', 1:5,
                          require_sentinel = FALSE)
  if (!is.null(caps)) {
    cdctc_cap_vec = coalesce(
      st_pick_slot(caps, pmin(pmax(n_care_v, 1), 5)), 0
    )
  }

  # Own-rate slide: continuous per-$1,000 (NY) or, where a step is encoded,
  # a stepped reduction of cdctc_rate_po_per_1k per step or fraction thereof
  # (HI Schedule X: 0.01 per $5,000 band of Hawaii AGI over $25,000)
  cdctc_rate_red = if_else(
    is.finite(tax_unit$st_credits.cdctc_rate_po_step),
    st_step_reduction(tax_unit$st_agi,
                      tax_unit$st_credits.cdctc_rate_po_start,
                      tax_unit$st_credits.cdctc_rate_po_step,
                      tax_unit$st_credits.cdctc_rate_po_per_1k),
    tax_unit$st_credits.cdctc_rate_po_per_1k *
      pmax(0, tax_unit$st_agi -
              tax_unit$st_credits.cdctc_rate_po_start) / 1000
  )
  cdctc_rate2 = pmax(tax_unit$st_credits.cdctc_rate_floor,
                     tax_unit$st_credits.cdctc_rate_max - cdctc_rate_red)

  # Style-2 expenses capped at each spouse's earned income where flagged
  # (the federal 2441 rule, carried by HI Schedule X; NY 2026 encoded
  # without it, so the default leaves it off)
  cdctc_ei_cap = if_else(
    tax_unit$st_credits.cdctc_expense_ei_limit == 1,
    pmax(0, if_else(tax_unit$filing_status == 2,
                    pmin(tax_unit$ei1, tax_unit$ei2), tax_unit$ei1)),
    Inf
  )
  # Louisiana runs BOTH computations in one credit, split at a federal-AGI
  # line (R.S. 47:297.4): at or below $25,000 the state computes the credit
  # from its own worksheet -- expenses, the earned-income limit and a sliding
  # decimal, halved -- and refunds it; above the line the credit is a share
  # of the FEDERAL credit and is nonrefundable. Encoded as a switch on the
  # style already in force rather than a third style, since both
  # computations are here. .inf = no switch
  cdctc_style_v = if_else(
    is.finite(tax_unit$st_credits.cdctc_style_switch_agi) &
      tax_unit$agi > tax_unit$st_credits.cdctc_style_switch_agi,
    1, tax_unit$st_credits.cdctc_style
  )
  cdctc_ny = case_when(
    cdctc_style_v == 1 ~
      cdctc_ny_share * (tax_unit$cdctc_nonref + tax_unit$cdctc_ref),
    cdctc_style_v == 2 & n_care_v > 0 ~
      cdctc_rate2 * pmin(tax_unit$care_exp, cdctc_cap_vec, cdctc_ei_cap),
    TRUE ~ 0
  )
  st_cdctc = tax_unit$st_credits.cdctc_match *
               (tax_unit$cdctc_nonref + tax_unit$cdctc_ref) + cdctc_ny

  # Income-capped variant (MN M1CD): above the threshold, the credit is
  # limited to cap_amount per qualifying person (up to two) less po_rate
  # times the excess AGI (a cliff at the threshold, as the form computes).
  # Louisiana's version of the same cap is a flat $25 per RETURN however many
  # children there are, hence the per-return flag
  cdctc_cap_units = if_else(tax_unit$st_credits.cdctc_cap_per_return == 1,
                            1, pmin(2, n_care_v))
  st_cdctc = if_else(
    is.finite(tax_unit$st_credits.cdctc_cap_thresh) &
      tax_unit$agi > tax_unit$st_credits.cdctc_cap_thresh,
    pmin(st_cdctc,
         pmax(0, tax_unit$st_credits.cdctc_cap_amount * cdctc_cap_units -
                 tax_unit$st_credits.cdctc_cap_po_rate *
                   (tax_unit$agi - tax_unit$st_credits.cdctc_cap_thresh))),
    st_cdctc
  )

  list(st_cdctc = st_cdctc)
}
