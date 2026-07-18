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
  'st_credits.cdctc_rate_po_start'
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
    cdctc_ny_share = st_band_interp(tax_unit$st_agi, b_c, s0, s1)
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

  cdctc_rate2 = pmax(tax_unit$st_credits.cdctc_rate_floor,
                     tax_unit$st_credits.cdctc_rate_max -
                     tax_unit$st_credits.cdctc_rate_po_per_1k *
                     pmax(0, tax_unit$st_agi -
                             tax_unit$st_credits.cdctc_rate_po_start) / 1000)
  cdctc_ny = case_when(
    tax_unit$st_credits.cdctc_style == 1 ~
      cdctc_ny_share * (tax_unit$cdctc_nonref + tax_unit$cdctc_ref),
    tax_unit$st_credits.cdctc_style == 2 & n_care_v > 0 ~
      cdctc_rate2 * pmin(tax_unit$care_exp, cdctc_cap_vec),
    TRUE ~ 0
  )
  st_cdctc = tax_unit$st_credits.cdctc_match *
               (tax_unit$cdctc_nonref + tax_unit$cdctc_ref) + cdctc_ny

  list(st_cdctc = st_cdctc)
}
