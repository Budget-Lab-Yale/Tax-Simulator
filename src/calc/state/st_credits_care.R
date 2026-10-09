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
  'st_credits.cdctc_fed_base',
  'st_credits.cdctc_base_liab_limit',
  'st_credits.cdctc_fed_base_switch_income',
  'st_credits.cdctc_lowinc_rate',
  'st_credits.cdctc_lowinc_agi_limit',
  'st_credits.cdctc_lowinc_agi_limit_joint',
  'st_credits.cdctc_lowinc_qual_share',
  'st_credits.cdctc_ref_qual_share',
  'st_credits.cdctc_rate_max',
  'st_credits.cdctc_rate_floor',
  'st_credits.cdctc_rate_po_per_1k',
  'st_credits.cdctc_rate_po_start',
  'st_credits.cdctc_rate_po_step',
  'st_credits.cdctc_expense_ei_limit',
  'st_credits.cdctc_share_income_base',
  'st_credits.cdctc_rate_income_base',    # (int) st_income_base enum for the style-2 rate slide (default 2, state AGI)
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
  # (HI Schedule X: 0.01 per $5,000 band of Hawaii AGI over $25,000). The
  # slide runs on state AGI by default; AR TY2021 runs it on federal AGI
  # because the credit it shares is a recomputed federal one
  cdctc_rate_income = st_income_base(tax_unit, tax_unit$st_credits.cdctc_rate_income_base)
  cdctc_rate_red = if_else(
    is.finite(tax_unit$st_credits.cdctc_rate_po_step),
    st_step_reduction(cdctc_rate_income,
                      tax_unit$st_credits.cdctc_rate_po_start,
                      tax_unit$st_credits.cdctc_rate_po_step,
                      tax_unit$st_credits.cdctc_rate_po_per_1k),
    tax_unit$st_credits.cdctc_rate_po_per_1k *
      pmax(0, cdctc_rate_income -
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
  # The federal credit a share-based state credit starts from
  # (cdctc_fed_base):
  #   0 = as claimed (Form 2441 line 11, capped at federal tax; default)
  #   1 = the tentative federal credit before that cap (2441 line 9/9c;
  #       cdctc_potential), which pays families with no federal tax
  #   2 = the state's own recomputation of the federal formula: capped
  #       expenses times the cdctc_rate_* decimal (CA FTB 3506 line 8, NY
  #       IT-216 line 11 with its own caps, WI 2024+ Schedule WI-2441, and
  #       the pre-ARPA TY2021 recomputations of CA/NY/KY)
  # cdctc_base_liab_limit = 1 caps the base at federal tax less the foreign
  # tax credit (KY Form 2441-K line 10, TY2021). A tiered credit uses the
  # chosen base only at or below cdctc_fed_base_switch_income (measured on
  # the share-table income base) and the claimed credit above it (NE Form
  # 2441N: at or below $29,000 federal AGI; OH: below $20,000)
  # A state credit recomputed from expenses shares the federal take-up draw
  # (cdctc_takeup; 90% calibrated in cdctc.R), as the claimed and tentative
  # bases already do -- JI 2026-10-08
  cdctc_claimed = tax_unit$cdctc_nonref + tax_unit$cdctc_ref
  cdctc_own = if_else(n_care_v > 0,
                      cdctc_rate2 * pmin(tax_unit$care_exp, cdctc_cap_vec,
                                         cdctc_ei_cap) * tax_unit$cdctc_takeup,
                      0)
  cdctc_fed = case_when(
    tax_unit$st_credits.cdctc_fed_base == 1 ~ tax_unit$cdctc_potential,
    tax_unit$st_credits.cdctc_fed_base == 2 ~ cdctc_own,
    TRUE ~ cdctc_claimed
  )
  cdctc_fed = if_else(tax_unit$st_credits.cdctc_base_liab_limit == 1,
                      pmin(cdctc_fed, pmax(0, tax_unit$liab_bc - tax_unit$ftc)),
                      cdctc_fed)
  cdctc_fed = if_else(
    st_income_base(tax_unit, tax_unit$st_credits.cdctc_share_income_base) >
      tax_unit$st_credits.cdctc_fed_base_switch_income,
    cdctc_claimed, cdctc_fed
  )
  cdctc_ny = case_when(
    cdctc_style_v == 1 ~
      cdctc_ny_share * cdctc_fed,
    cdctc_style_v == 2 ~ cdctc_own,
    TRUE ~ 0
  )
  st_cdctc = tax_unit$st_credits.cdctc_match * cdctc_fed + cdctc_ny

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

  # Low-income refundable alternative (VT 32 V.S.A. 5828c through TY2021,
  # IN-112 Part II): cdctc_lowinc_rate of the same federal base, prorated by
  # the share of care paid to qualifying (accredited) providers, refundable,
  # for federal AGI at or below the limit -- "instead of" the regular credit,
  # so the unit keeps whichever pays more. Provider status is unobserved:
  # cdctc_lowinc_qual_share is an assumption, documented per state
  lowinc_amt = tax_unit$st_credits.cdctc_lowinc_rate * cdctc_fed *
               tax_unit$st_credits.cdctc_lowinc_qual_share
  lowinc_limit = if_else(tax_unit$filing_status == 2,
                         tax_unit$st_credits.cdctc_lowinc_agi_limit_joint,
                         tax_unit$st_credits.cdctc_lowinc_agi_limit)
  regular_benefit = if_else(tax_unit$st_credits.cdctc_refundable == 1,
                            st_cdctc,
                            pmin(st_cdctc, pmax(0, tax_unit$st_tax_pre_credit)))
  st_cdctc_lowinc = tax_unit$st_credits.cdctc_lowinc_rate > 0 &
                    tax_unit$agi <= lowinc_limit &
                    lowinc_amt > regular_benefit
  st_cdctc = if_else(st_cdctc_lowinc, lowinc_amt, st_cdctc)

  list(st_cdctc = st_cdctc, st_cdctc_lowinc = st_cdctc_lowinc)
}
