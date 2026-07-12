#-------------------------------------------
# Function to calculate state tax credits
#-------------------------------------------

# Set return variables for function
return_vars$calc_st_credits = c('st_hh_credit', 'st_eitc', 'st_ctc',
                                'st_cdctc', 'st_credits_nonref',
                                'st_credits_ref')


calc_st_credits = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates state credits: household credit (NY table lookup), EITC match
  # (with the NY minus-household-credit mechanic), state child credits (IL
  # percent-of-EITC; NY Empire State child credit styles 1/2; CO tiered
  # styles 1/2 plus the Family Affordability credit), CDCTC (flat match or
  # NY styles), and the IL property tax credit.
  #
  # Structural gates: CO's tiered CTC machinery is gated on
  # st_credits.ctc_tier1_bound being present/non-NA; NY's ESCC machinery on
  # ctc_style >= 1 with tier bounds ABSENT. Both share the ctc_style column
  # but are mutually exclusive by state config. Child counts come from the
  # up-to-three tracked dependent ages (dep_age1-3), consistent with the
  # federal CTC calculator.
  #
  # v1 approximations (documented known-differences):
  #  - NY ESCC style 1 folds the pre-TCJA ACTC into full refundability
  #  - CO CTC style 1 attributes the federal credit to under-6 children
  #    proportionally by child count
  #  - CO FATC's stepped phase-out is approximated linearly
  #  - MFS household credit uses own (not combined) AGI
  #  - NY college tuition and IL K-12 credits are data-limited (not computed)
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_hh_credit (dbl)      : household credit (nonrefundable)
  #   - st_eitc (dbl)           : state EITC
  #   - st_ctc (dbl)            : state child credits (incl. CO FATC)
  #   - st_cdctc (dbl)          : state child/dependent care credit
  #   - st_credits_nonref (dbl) : total nonrefundable credits
  #   - st_credits_ref (dbl)    : total refundable credits
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',               # (dbl)  federal AGI
    'st_agi',            # (dbl)  state income base
    'st_tax_pre_credit', # (dbl)  state tax before credits
    'eitc',              # (dbl)  federal EITC
    'ctc_nonref',        # (dbl)  federal CTC, nonrefundable portion
    'ctc_ref',           # (dbl)  federal CTC, refundable portion
    'cdctc_nonref',      # (dbl)  federal CDCTC, nonrefundable portion
    'cdctc_ref',         # (dbl)  federal CDCTC, refundable portion
    'care_exp',          # (dbl)  eligible dependent care expenses
    'salt_prop',         # (dbl)  state/local real estate taxes paid
    'dep_age1',          # (int)  age of youngest dependent (NA if none)
    'dep_age2',          # (int)  age of second dependent (NA if none)
    'dep_age3',          # (int)  age of third dependent (NA if none)
    'n_dep',             # (int)  number of dependents
    'filing_status',     # (int)  filing status (1 single, 2 MFJ, 3 MFS, 4 HoH)
    'dep_status',        # (bool) whether filer is a dependent

    # State tax law (scalar; vector table params accessed by column name)
    'st_credits.eitc_match',
    'st_credits.eitc_less_household_credit',
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
    'st_credits.cdctc_match',
    'st_credits.cdctc_style',
    'st_credits.cdctc_rate_max',
    'st_credits.cdctc_rate_floor',
    'st_credits.cdctc_rate_po_per_1k',
    'st_credits.cdctc_rate_po_start',
    'st_credits.prop_tax_credit_rate',
    'st_credits.credit_agi_limit',
    'st_credits.fatc_young_amount',
    'st_credits.fatc_old_amount',
    'st_credits.fatc_young_age_limit',
    'st_credits.fatc_max_child_age',
    'st_credits.fatc_po_start',
    'st_credits.fatc_po_zero',
    'st_credits.hh_mfs_half'
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  n   = nrow(tax_unit)
  cn  = colnames(tax_unit)
  agi = tax_unit$agi

  # Count tracked dependents with ages in [lo, hi] (vectors allowed)
  n_dep_in = function(lo, hi) {
    (!is.na(tax_unit$dep_age1) & tax_unit$dep_age1 >= lo & tax_unit$dep_age1 <= hi) +
    (!is.na(tax_unit$dep_age2) & tax_unit$dep_age2 >= lo & tax_unit$dep_age2 <= hi) +
    (!is.na(tax_unit$dep_age3) & tax_unit$dep_age3 >= lo & tax_unit$dep_age3 <= hi)
  }

  #----------------------------
  # Household credit (NY-style)
  #----------------------------

  st_hh_credit = rep(0, n)

  hh_single_cols = paste0('st_credits.hh_agi_bounds_single', 1:7)
  if (all(hh_single_cols %in% cn) &&
      any(!is.na(tax_unit$st_credits.hh_agi_bounds_single1))) {

    ub_s  = as.matrix(tax_unit[paste0('st_credits.hh_agi_bounds_single', 2:7)])
    amt_s = as.matrix(tax_unit[paste0('st_credits.hh_amount_single', 1:6)])
    lb_s  = cbind(-Inf, ub_s[, -6, drop = F])
    hh_s  = rowSums(amt_s * (agi > lb_s & agi <= ub_s), na.rm = T)

    ub_o   = as.matrix(tax_unit[paste0('st_credits.hh_agi_bounds_other', 2:9)])
    base_o = as.matrix(tax_unit[paste0('st_credits.hh_base_other', 1:8)])
    incr_o = as.matrix(tax_unit[paste0('st_credits.hh_incr_other', 1:8)])
    lb_o   = cbind(-Inf, ub_o[, -8, drop = F])
    n_ex   = 1 + (tax_unit$filing_status == 2) + tax_unit$n_dep
    hh_o   = rowSums((base_o + incr_o * (n_ex - 1)) * (agi > lb_o & agi <= ub_o),
                     na.rm = T)

    st_hh_credit = case_when(
      tax_unit$dep_status == 1     ~ 0,
      tax_unit$filing_status == 1  ~ hh_s,
      tax_unit$filing_status == 3  ~ hh_o * if_else(tax_unit$st_credits.hh_mfs_half == 1,
                                                    0.5, 1),
      TRUE                         ~ hh_o
    )
  }

  #-------------------------------
  # CDCTC share table (NY style 1)
  #-------------------------------

  cdctc_ny_share = rep(0, n)
  cdctc_anchor_cols = paste0('st_credits.cdctc_share_agi_bounds', 1:6)
  if (all(cdctc_anchor_cols %in% cn) &&
      any(!is.na(tax_unit$st_credits.cdctc_share_agi_bounds1))) {

    b_c = as.matrix(tax_unit[paste0('st_credits.cdctc_share_agi_bounds', 1:6)])
    s0  = as.matrix(tax_unit[paste0('st_credits.cdctc_share_start', 1:6)])
    s1  = as.matrix(tax_unit[paste0('st_credits.cdctc_share_end', 1:6)])
    ub  = cbind(b_c[, -1, drop = F], Inf)
    y   = tax_unit$st_agi
    w   = ub - b_c
    frac = ifelse(is.finite(w) & w > 0, (y - b_c) / w, 0)
    seg  = (y >= b_c & y < ub)
    cdctc_ny_share = rowSums((s0 + (s1 - s0) * pmin(1, pmax(0, frac))) * seg,
                             na.rm = T)
  }

  #---------------------------------
  # CO tiered child credit machinery
  #---------------------------------

  #------------------------------------------------
  # CDCTC expense caps (NY style 2), by care-kid count
  #------------------------------------------------

  n_care_v = n_dep_in(0, 12)
  cdctc_cap_vec = rep(0, n)
  cap_cols = paste0('st_credits.cdctc_expense_caps', 1:5)
  if (all(cap_cols %in% cn)) {
    caps = as.matrix(tax_unit[cap_cols])
    cdctc_cap_vec = caps[cbind(1:n, pmin(pmax(n_care_v, 1), 5))]
    cdctc_cap_vec[is.na(cdctc_cap_vec)] = 0
  }

  co_tier = rep(0L, n)
  if ('st_credits.ctc_tier1_bound' %in% cn &&
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
    cols = paste0(prefix, 1:3)
    out  = rep(0, n)
    if (!all(cols %in% cn)) {
      return(out)
    }
    m  = as.matrix(tax_unit[cols])
    ok = co_tier > 0
    out[ok] = m[cbind(which(ok), co_tier[ok])]
    out
  }

  tax_unit %>%
    mutate(

      st_hh_credit = st_hh_credit,

      # State EITC: match on the federal credit, less the household credit
      # (capped at remaining tax) where flagged (NY IT-215 lines 13-16)
      st_eitc = pmax(0, st_credits.eitc_match * eitc -
                        st_credits.eitc_less_household_credit *
                        pmin(st_hh_credit, pmax(0, st_tax_pre_credit))),

      #--------------------
      # State child credits
      #--------------------

      # IL: percent of the state EITC when any child is under 12
      ctc_il = st_credits.ctc_pct_of_eitc * st_eitc *
               (!is.na(dep_age1) & dep_age1 < 12),

      # NY ESCC (gated: styles with NO tier bounds). Style 1: match share of
      # the pre-TCJA federal CTC replica ($/child base, $50-per-$1,000
      # phase-out with excess rounded UP), with the per-child minimum when
      # income is under the threshold. Style 2 (2025+): flat per-child
      # amounts phased at ctc_po_rate per $1,000 (excess rounded DOWN)
      ny_gate  = (is.na(st_credits.ctc_tier1_bound)) & (st_credits.ctc_style >= 1),
      n_qual   = n_dep_in(st_credits.ctc_min_child_age, st_credits.ctc_max_child_age),
      n_young  = n_dep_in(0, st_credits.ctc_young_age_limit),
      n_old    = n_dep_in(st_credits.ctc_young_age_limit + 1,
                          st_credits.ctc_max_child_age),
      ctc_po_up   = st_credits.ctc_po_rate * 1000 *
                    ceiling(pmax(0, agi - st_credits.ctc_po_thresh) / 1000),
      ctc_po_down = st_credits.ctc_po_rate * 1000 *
                    floor(pmax(0, agi - st_credits.ctc_po_thresh) / 1000),
      ctc_ny = case_when(
        ny_gate & st_credits.ctc_style == 1 ~
          pmax(st_credits.ctc_match_share *
                 pmax(0, st_credits.ctc_fed_base_per_child * n_qual - ctc_po_up),
               st_credits.ctc_min_per_child * n_qual *
                 (agi <= st_credits.ctc_po_thresh)),
        ny_gate & st_credits.ctc_style == 2 ~
          pmax(0, st_credits.ctc_young_amount * n_young +
                  st_credits.ctc_old_amount   * n_old - ctc_po_down),
        TRUE ~ 0
      ),

      # CO CTC (gated on tier bounds): style 1 = tier share of the federal
      # credit attributed to under-6 children; style 2 = flat tier amount
      # per under-6 child
      n_u6  = n_dep_in(0, st_credits.ctc_max_child_age),
      n_u17 = n_dep_in(0, 16),
      ctc_co = case_when(
        co_tier > 0 & st_credits.ctc_style == 1 ~
          pick_tier('st_credits.ctc_tier_shares') * (ctc_nonref + ctc_ref) *
          n_u6 / pmax(1, n_u17),
        co_tier > 0 & st_credits.ctc_style == 2 ~
          pick_tier('st_credits.ctc_tier_amounts') * n_u6,
        TRUE ~ 0
      ),

      # CO Family Affordability credit: per-child amounts with a linear
      # phase-out between fatc_po_start and fatc_po_zero
      n_fatc_young = n_dep_in(0, st_credits.fatc_young_age_limit),
      n_fatc_old   = n_dep_in(st_credits.fatc_young_age_limit + 1,
                              st_credits.fatc_max_child_age),
      fatc_factor  = if_else(st_credits.fatc_po_zero > st_credits.fatc_po_start,
                             pmin(1, pmax(0, (st_credits.fatc_po_zero - agi) /
                                             (st_credits.fatc_po_zero -
                                              st_credits.fatc_po_start))),
                             1),
      fatc = (st_credits.fatc_young_amount * n_fatc_young +
              st_credits.fatc_old_amount   * n_fatc_old) * fatc_factor,

      st_ctc = ctc_il + ctc_ny + ctc_co + fatc,

      #-------
      # CDCTC
      #-------

      n_care = n_care_v,
      cdctc_rate2 = pmax(st_credits.cdctc_rate_floor,
                         st_credits.cdctc_rate_max -
                         st_credits.cdctc_rate_po_per_1k *
                         pmax(0, st_agi - st_credits.cdctc_rate_po_start) / 1000),
      cdctc_ny = case_when(
        st_credits.cdctc_style == 1 ~ cdctc_ny_share * (cdctc_nonref + cdctc_ref),
        st_credits.cdctc_style == 2 & n_care > 0 ~
          cdctc_rate2 * pmin(care_exp, cdctc_cap_vec),
        TRUE ~ 0
      ),
      st_cdctc = st_credits.cdctc_match * (cdctc_nonref + cdctc_ref) + cdctc_ny,

      # Property tax credit (IL): rate times property taxes, denied above the
      # AGI limit
      prop_credit = st_credits.prop_tax_credit_rate * salt_prop *
                    (agi <= st_credits.credit_agi_limit),

      #------------
      # Aggregation
      #------------

      st_credits_nonref = st_hh_credit + prop_credit,
      st_credits_ref    = st_eitc + st_ctc + st_cdctc
    ) %>%
    select(all_of(return_vars$calc_st_credits)) %>%
    return()
}
