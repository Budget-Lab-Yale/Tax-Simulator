#---------------------------------------------------------------------------
# Child / dependent credit family (called by calc_st_credits): IL
# percent-of-EITC, NY Empire State child credit styles 1/2, CO tiered CTC
# styles 1/2, the CO Family Affordability credit, and the AZ dependent
# credit.
#---------------------------------------------------------------------------

# Law parameters this family reads (assembled into calc_st_credits req_vars)
st_credits_child_req_vars = c(
  'st_credits.dep_credit_style',
  'st_credits.dep_credit_young_amount',
  'st_credits.dep_credit_other_amount',
  'st_credits.dep_credit_po_thresh',
  'st_credits.dep_credit_po_per_1k',
  'st_credits.ctc_style',
  'st_credits.ctc_match_share',
  'st_credits.ctc_fed_base_per_child',
  'st_credits.ctc_min_per_child',
  'st_credits.ctc_min_child_age',
  'st_credits.ctc_max_child_age',
  'st_credits.ctc_young_age_limit',
  'st_credits.ctc_young_amount',
  'st_credits.ctc_old_amount',
  'st_credits.ctc_po_thresh',
  'st_credits.ctc_po_rate',
  'st_credits.ctc_pct_of_eitc',
  'st_credits.fatc_young_amount',
  'st_credits.fatc_old_amount',
  'st_credits.fatc_young_age_limit',
  'st_credits.fatc_max_child_age',
  'st_credits.fatc_po_start',
  'st_credits.fatc_po_zero'
)


st_credits_child = function(tax_unit, st_eitc) {

  #----------------------------------------------------------------------------
  # Calculates the child/dependent credit family on a parsed tax unit
  # tibble. Structural gates: CO's tiered CTC machinery is gated on
  # st_credits.ctc_tier1_bound being present/non-NA; NY's ESCC machinery on
  # ctc_style >= 1 with tier bounds ABSENT. Both share the ctc_style column
  # but are mutually exclusive by state config.
  #
  # Parameters:
  #   - tax_unit (df)   : parsed tax unit tibble (see calc_st_credits)
  #   - st_eitc (dbl[]) : chosen state EITC (st_credits_earned; IL input)
  #
  # Returns: list of per-row vectors --
  #   - st_ctc (dbl)        : state child credits (incl. CO FATC)
  #   - st_dep_credit (dbl) : AZ-style dependent credit
  #----------------------------------------------------------------------------

  n   = nrow(tax_unit)
  agi = tax_unit$agi

  co_tier = rep(0L, n)
  if ('st_credits.ctc_tier1_bound' %in% colnames(tax_unit) &&
      any(!is.na(tax_unit$st_credits.ctc_tier1_bound))) {
    co_tier = case_when(
      agi <= tax_unit$st_credits.ctc_tier1_bound ~ 1L,
      agi <= tax_unit$st_credits.ctc_tier2_bound ~ 2L,
      agi <= tax_unit$st_credits.ctc_tier3_bound ~ 3L,
      TRUE                                       ~ 0L
    )
    co_tier[is.na(co_tier)] = 0L
  }

  # case_when() evaluates all branches eagerly, so this must return zeros
  # (not error) when the tier columns are absent from this state's law slice
  pick_tier = function(prefix) {
    m   = st_family_matrix(tax_unit, prefix, 1:3, require_sentinel = FALSE)
    out = rep(0, n)
    if (is.null(m)) {
      return(out)
    }
    ok = co_tier > 0
    out[ok] = m[cbind(which(ok), co_tier[ok])]
    out
  }

  # IL: percent of the state EITC when any child is under 12
  ctc_il = tax_unit$st_credits.ctc_pct_of_eitc * st_eitc *
           (!is.na(tax_unit$dep_age1) & tax_unit$dep_age1 < 12)

  # NY ESCC (gated: styles with NO tier bounds). Style 1: match share of
  # the pre-TCJA federal CTC replica ($/child base, $50-per-$1,000
  # phase-out with excess rounded UP), with the per-child minimum when
  # income is under the threshold. Style 2 (2025+): flat per-child
  # amounts phased at ctc_po_rate per $1,000 (excess rounded DOWN)
  ny_gate  = (is.na(tax_unit$st_credits.ctc_tier1_bound)) &
             (tax_unit$st_credits.ctc_style >= 1)
  n_qual   = st_n_dep_in(tax_unit, tax_unit$st_credits.ctc_min_child_age,
                         tax_unit$st_credits.ctc_max_child_age)
  n_young  = st_n_dep_in(tax_unit, 0, tax_unit$st_credits.ctc_young_age_limit)
  n_old    = st_n_dep_in(tax_unit, tax_unit$st_credits.ctc_young_age_limit + 1,
                         tax_unit$st_credits.ctc_max_child_age)
  ctc_po_up   = st_step_reduction(agi, tax_unit$st_credits.ctc_po_thresh, 1000,
                                  tax_unit$st_credits.ctc_po_rate * 1000)
  ctc_po_down = st_step_reduction(agi, tax_unit$st_credits.ctc_po_thresh, 1000,
                                  tax_unit$st_credits.ctc_po_rate * 1000,
                                  round_up = FALSE)
  ctc_ny = case_when(
    ny_gate & tax_unit$st_credits.ctc_style == 1 ~
      pmax(tax_unit$st_credits.ctc_match_share *
             pmax(0, tax_unit$st_credits.ctc_fed_base_per_child * n_qual -
                     ctc_po_up),
           tax_unit$st_credits.ctc_min_per_child * n_qual *
             (agi <= tax_unit$st_credits.ctc_po_thresh)),
    ny_gate & tax_unit$st_credits.ctc_style == 2 ~
      pmax(0, tax_unit$st_credits.ctc_young_amount * n_young +
              tax_unit$st_credits.ctc_old_amount   * n_old - ctc_po_down),
    TRUE ~ 0
  )

  # CO CTC (gated on tier bounds): style 1 = tier share of the federal
  # credit attributed to under-6 children; style 2 = flat tier amount
  # per under-6 child
  n_u6  = st_n_dep_in(tax_unit, 0, tax_unit$st_credits.ctc_max_child_age)
  n_u17 = st_n_dep_in(tax_unit, 0, 16)
  ctc_co = case_when(
    co_tier > 0 & tax_unit$st_credits.ctc_style == 1 ~
      pick_tier('st_credits.ctc_tier_shares') *
      (tax_unit$ctc_nonref + tax_unit$ctc_ref) * n_u6 / pmax(1, n_u17),
    co_tier > 0 & tax_unit$st_credits.ctc_style == 2 ~
      pick_tier('st_credits.ctc_tier_amounts') * n_u6,
    TRUE ~ 0
  )

  # CO Family Affordability credit: per-child amounts with a linear
  # phase-out between fatc_po_start and fatc_po_zero
  n_fatc_young = st_n_dep_in(tax_unit, 0,
                             tax_unit$st_credits.fatc_young_age_limit)
  n_fatc_old   = st_n_dep_in(tax_unit,
                             tax_unit$st_credits.fatc_young_age_limit + 1,
                             tax_unit$st_credits.fatc_max_child_age)
  fatc_factor  = if_else(
    tax_unit$st_credits.fatc_po_zero > tax_unit$st_credits.fatc_po_start,
    pmin(1, pmax(0, (tax_unit$st_credits.fatc_po_zero - agi) /
                    (tax_unit$st_credits.fatc_po_zero -
                     tax_unit$st_credits.fatc_po_start))),
    1
  )
  fatc = (tax_unit$st_credits.fatc_young_amount * n_fatc_young +
          tax_unit$st_credits.fatc_old_amount   * n_fatc_old) * fatc_factor

  st_ctc = ctc_il + ctc_ny + ctc_co + fatc

  # Dependent credit: separate qualifying-child and other-dependent
  # amounts, then a common percentage-point reduction for each $1,000 of
  # AGI above the state threshold (Arizona Form 140 Table V).
  dep_credit_base = tax_unit$st_credits.dep_credit_young_amount *
                      tax_unit$n_dep_ctc +
                    tax_unit$st_credits.dep_credit_other_amount *
                      pmax(0, tax_unit$n_dep - tax_unit$n_dep_ctc)
  dep_credit_factor = pmax(
    0,
    1 - st_step_reduction(agi, tax_unit$st_credits.dep_credit_po_thresh, 1000,
                          tax_unit$st_credits.dep_credit_po_per_1k)
  )
  st_dep_credit = if_else(tax_unit$st_credits.dep_credit_style == 1,
                          dep_credit_base * dep_credit_factor, 0)

  list(
    st_ctc        = st_ctc,
    st_dep_credit = st_dep_credit
  )
}
