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
  'st_credits.ctc_po_base',
  'st_credits.ctc_pct_of_eitc',
  'st_credits.fatc_young_amount',
  'st_credits.fatc_old_amount',
  'st_credits.fatc_young_age_limit',
  'st_credits.fatc_max_child_age',
  'st_credits.fatc_po_start',
  'st_credits.fatc_po_zero',
  'st_credits.cwfc_style',
  'st_credits.cwfc_ctc_amount',
  'st_credits.cwfc_ctc_max_age',
  'st_credits.cwfc_wfc_rate',
  'st_credits.cwfc_wfc_earned_cap',
  'st_credits.cwfc_po_rate',
  'st_credits.cwfc_po_rate_older_only',
  'st_credits.cwfc_po_thresh'
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

  # AGI-tiered per-child credit (CO DR 0104CN; NC 2017 G.S. 105-153.10; and
  # the NM/NJ tables). The bound family is ctc_tier{n}_bound -- the index sits
  # MID-name, so st_family_matrix (which appends the index) cannot discover it
  # and the columns are found by regex here instead. The tier COUNT comes from
  # the columns present, so a state may declare as many tiers as its table has
  # (CO 3, NC 3, NM 7, NJ 6).
  #
  # Semantics, preserved exactly from the original three-branch case_when:
  # tier j is the first whose bound is at or above AGI (closed upper), and AGI
  # above the LAST bound means INELIGIBLE (tier 0) rather than "clamp into the
  # top tier". st_band_index_upper is deliberately NOT used here -- it clamps,
  # which is right for KY's family-size table and would silently hand every
  # high-income CO filer the bottom-tier amount. A state whose table has no
  # eligibility ceiling says so by setting its last bound to Inf (NM's seventh
  # tier is "over $350,000").
  #
  # The per-row non-NA bound count matters: law slices bound across states pad
  # absent tiers with NA, so a 3-tier state sitting in a frame widened to 7
  # columns by another state must still be ineligible above ITS third bound,
  # not selected into a nonexistent fourth tier.
  co_tier = rep(0L, n)
  tier_bound_cols = str_subset(colnames(tax_unit),
                               '^st_credits\\.ctc_tier[0-9]+_bound$')
  if (length(tier_bound_cols) > 0 &&
      any(!is.na(tax_unit$st_credits.ctc_tier1_bound))) {
    n_tiers   = max(as.integer(str_extract(tier_bound_cols, '[0-9]+')))
    want_cols = paste0('st_credits.ctc_tier', 1:n_tiers, '_bound')
    if (!all(want_cols %in% colnames(tax_unit))) {
      stop('ctc_tier bounds are not contiguous: expected ',
           paste(want_cols, collapse = ' '))
    }
    bounds = as.matrix(tax_unit[want_cols])

    # A gap inside a state's own ladder (tier1 and tier3 declared, tier2 not)
    # would shift every tier above the gap onto the wrong amount, so fail
    # loudly rather than compute a plausible wrong number
    col_used = colSums(!is.na(bounds)) > 0
    if (any(diff(as.integer(col_used)) > 0)) {
      stop('ctc_tier bounds have an interior gap: ',
           paste(want_cols[!col_used], collapse = ' '), ' unset while a ',
           'higher tier is set')
    }

    n_bounds = rowSums(!is.na(bounds))
    passed   = rowSums(bounds < agi, na.rm = TRUE)
    co_tier  = if_else(n_bounds == 0 | passed >= n_bounds,
                       0L, as.integer(passed) + 1L)
    co_tier[is.na(co_tier)] = 0L
  }

  # Must return zeros (not error) where the tier columns are absent from this
  # state's law slice: case_when() below evaluates all branches eagerly, and
  # the shares/amounts families are alternatives -- a state on ctc_style 2
  # legitimately leaves shares entirely NA (and vice versa), so neither an
  # absent family nor one shorter than the selected tier can raise here.
  # `found` carries whether a value was actually located, which the style-aware
  # check below turns into the loud failure.
  pick_tier = function(prefix) {
    m   = st_family_matrix(tax_unit, prefix, NULL, require_sentinel = FALSE)
    out = rep(0, n)
    if (is.null(m)) {
      return(structure(out, found = rep(FALSE, n)))
    }
    ok  = co_tier > 0 & co_tier <= ncol(m)
    idx = cbind(which(ok), co_tier[ok])
    out[ok] = coalesce(m[idx], 0)
    structure(out, found = replace(rep(FALSE, n), which(ok), !is.na(m[idx])))
  }

  tier_shares  = pick_tier('st_credits.ctc_tier_shares')
  tier_amounts = pick_tier('st_credits.ctc_tier_amounts')

  # A state that declares a bound for tier t but no value for tier t in the
  # family its own ctc_style reads would silently credit zero -- the exact
  # mismatch the parameter-name validator cannot see, since both names are
  # legal members of their families. Fail on it instead
  tier_value_missing = co_tier > 0 & (
    (tax_unit$st_credits.ctc_style == 1 & !attr(tier_shares,  'found')) |
    (tax_unit$st_credits.ctc_style == 2 & !attr(tier_amounts, 'found'))
  )
  if (any(tier_value_missing)) {
    stop(sprintf(paste('tiered CTC selects tier %s but the ctc_tier value',
                       'family for ctc_style %s has no value there'),
                 paste(sort(unique(co_tier[tier_value_missing])),
                       collapse = '/'),
                 paste(sort(unique(
                   tax_unit$st_credits.ctc_style[tier_value_missing])),
                   collapse = '/')))
  }

  # IL: percent of the state EITC when any child is under 12
  ctc_il = tax_unit$st_credits.ctc_pct_of_eitc * st_eitc *
           (!is.na(tax_unit$dep_age1) & tax_unit$dep_age1 < 12)

  # NY ESCC (gated: styles with NO tier bounds). Style 1: match share of
  # the pre-TCJA federal CTC replica ($/child base, $50-per-$1,000
  # phase-out with excess rounded UP), with the per-child minimum when
  # income is under the threshold. Style 2 (2025+): flat per-child
  # amounts phased at ctc_po_rate per $1,000 (excess rounded DOWN).
  # Style 3 (UT 59-10-1047): flat per-child amount for children in the
  # [min_child_age, max_child_age] band, phased CONTINUOUSLY at
  # ctc_po_rate per dollar of the enum income base over the threshold
  ny_gate  = (is.na(tax_unit$st_credits.ctc_tier1_bound)) &
             (tax_unit$st_credits.ctc_style >= 1)
  n_qual   = st_n_dep_in(tax_unit, tax_unit$st_credits.ctc_min_child_age,
                         tax_unit$st_credits.ctc_max_child_age)
  n_young  = st_n_dep_in(tax_unit, 0, tax_unit$st_credits.ctc_young_age_limit)
  n_old    = st_n_dep_in(tax_unit, tax_unit$st_credits.ctc_young_age_limit + 1,
                         tax_unit$st_credits.ctc_max_child_age)
  ctc_po_income = st_income_base(tax_unit, tax_unit$st_credits.ctc_po_base)
  ctc_po_up   = st_step_reduction(ctc_po_income,
                                  tax_unit$st_credits.ctc_po_thresh, 1000,
                                  tax_unit$st_credits.ctc_po_rate * 1000)
  ctc_po_down = st_step_reduction(ctc_po_income,
                                  tax_unit$st_credits.ctc_po_thresh, 1000,
                                  tax_unit$st_credits.ctc_po_rate * 1000,
                                  round_up = FALSE)
  ctc_po_cont = tax_unit$st_credits.ctc_po_rate *
                pmax(0, ctc_po_income - tax_unit$st_credits.ctc_po_thresh)
  ctc_ny = case_when(
    ny_gate & tax_unit$st_credits.ctc_style == 1 ~
      pmax(tax_unit$st_credits.ctc_match_share *
             pmax(0, tax_unit$st_credits.ctc_fed_base_per_child * n_qual -
                     ctc_po_up),
           tax_unit$st_credits.ctc_min_per_child * n_qual *
             (ctc_po_income <= tax_unit$st_credits.ctc_po_thresh)),
    ny_gate & tax_unit$st_credits.ctc_style == 2 ~
      pmax(0, tax_unit$st_credits.ctc_young_amount * n_young +
              tax_unit$st_credits.ctc_old_amount   * n_old - ctc_po_down),
    ny_gate & tax_unit$st_credits.ctc_style == 3 ~
      pmax(0, tax_unit$st_credits.ctc_young_amount * n_qual - ctc_po_cont),
    TRUE ~ 0
  )

  # CO CTC (gated on tier bounds): style 1 = tier share of the federal
  # credit attributed to under-6 children; style 2 = flat tier amount
  # per under-6 child
  n_u6  = st_n_dep_in(tax_unit, 0, tax_unit$st_credits.ctc_max_child_age)
  n_u17 = st_n_dep_in(tax_unit, 0, 16)
  ctc_co = case_when(
    co_tier > 0 & tax_unit$st_credits.ctc_style == 1 ~
      as.numeric(tier_shares) *
      (tax_unit$ctc_nonref + tax_unit$ctc_ref) * n_u6 / pmax(1, n_u17),
    co_tier > 0 & tax_unit$st_credits.ctc_style == 2 ~
      as.numeric(tier_amounts) * n_u6,
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

  # Combined child + working-family credit (MN Schedule M1CWFC, 2023+):
  # ctc_amount per child under 18 (no child limit -- dependent slots cap
  # tracked children at three, a documented data limit), plus wfc_rate on
  # earned income up to the cap, plus a fixed amount keyed by the count of
  # qualifying OLDER children (proxied by dependents aged max_age+1 to 23;
  # student/disabled status unobserved), less ONE joint phase-out of
  # po_rate (po_rate_older_only when no under-18 children) on the greater
  # of earned income or AGI over the threshold. Refundable via
  # ctc_refundable; MFS and dependent filers ineligible
  cwfc = rep(0, n)
  if (any(tax_unit$st_credits.cwfc_style == 1)) {
    n_cwfc_young = st_n_dep_in(tax_unit, 0, tax_unit$st_credits.cwfc_ctc_max_age)
    n_cwfc_older = pmin(3L, st_n_dep_in(tax_unit,
                                        tax_unit$st_credits.cwfc_ctc_max_age + 1,
                                        23))
    older_amts = st_family_matrix(tax_unit, 'st_credits.cwfc_older_amounts',
                                  1:3, require_sentinel = FALSE)
    cwfc_older_amt = rep(0, n)
    if (!is.null(older_amts)) {
      ok = n_cwfc_older > 0 & !is.na(older_amts[, 1])
      cwfc_older_amt[ok] = older_amts[cbind(which(ok), n_cwfc_older[ok])]
    }
    cwfc_earned = pmax(0, tax_unit$ei1) + pmax(0, tax_unit$ei2)
    cwfc_base = tax_unit$st_credits.cwfc_ctc_amount * n_cwfc_young +
      tax_unit$st_credits.cwfc_wfc_rate *
        pmin(cwfc_earned, tax_unit$st_credits.cwfc_wfc_earned_cap) +
      cwfc_older_amt
    # M1CWFC line 13 (verified against the 2024 form): the reduced rate
    # applies only with an older-child amount (line 5) and NO young-child
    # amount (line 8); childless units take the general rate
    cwfc_po_rate = if_else(n_cwfc_young == 0 & n_cwfc_older > 0,
                           tax_unit$st_credits.cwfc_po_rate_older_only,
                           tax_unit$st_credits.cwfc_po_rate)
    cwfc = if_else(
      tax_unit$st_credits.cwfc_style == 1 & tax_unit$dep_status != 1 &
        tax_unit$filing_status != 3,
      pmax(0, cwfc_base - cwfc_po_rate *
                pmax(0, pmax(cwfc_earned, agi) -
                        tax_unit$st_credits.cwfc_po_thresh)),
      0
    )
  }

  st_ctc = ctc_il + ctc_ny + ctc_co + fatc + cwfc

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
