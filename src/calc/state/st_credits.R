#-------------------------------------------
# Function to calculate state tax credits
#-------------------------------------------

# Set return variables for function
return_vars$calc_st_credits = c('st_hh_credit', 'st_eitc', 'st_ctc',
                                'st_dep_credit', 'st_cdctc', 'st_family_credit',
                                'st_exempt_credit', 'st_earned_credit', 'st_yctc',
                                'st_credits_nonref', 'st_credits_ref')


lookup_state_credit_table = function(income, n_children, credit_tables,
                                     table_id) {

  #----------------------------------------------------------------------------
  # Looks up an independent state credit with inclusive income bands and
  # capped child-count columns. Missing ranges intentionally return zero,
  # which supports published tables that omit their zero-credit tails.
  #----------------------------------------------------------------------------

  amount = rep(0, length(income))
  if (is.null(credit_tables) || nrow(credit_tables) == 0) {
    return(amount)
  }

  schedule = credit_tables[credit_tables$credit_id == table_id, , drop = FALSE]
  if (nrow(schedule) == 0) {
    return(amount)
  }

  child_count = pmin(
    max(schedule$child_count),
    pmax(min(schedule$child_count), coalesce(n_children, 0L))
  )
  for (child_slot in unique(child_count)) {
    rows = which(child_count == child_slot)
    bands = schedule[schedule$child_count == child_slot, , drop = FALSE]
    bands = bands[order(bands$income_lower), , drop = FALSE]
    index = findInterval(income[rows], bands$income_lower)
    valid = index > 0
    valid[valid] = income[rows][valid] <= bands$income_upper[index[valid]]
    amount[rows[valid]] = bands$amount[index[valid]]
  }

  return(amount)
}



calc_st_credits = function(tax_unit, fill_missings = F, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # Calculates state credits: household credit (NY table lookup), EITC match
  # (with the NY minus-household-credit mechanic), state child credits (IL
  # percent-of-EITC; NY Empire State child credit styles 1/2; CO tiered
  # styles 1/2 plus the Family Affordability credit), CDCTC (flat match or
  # NY styles), and the IL property tax credit. It also supports a family-size
  # percentage-of-tax credit, exemption credits, and independent earned-income
  # credits whose schedule is not a federal-EITC match. Dense schedules are
  # supplied through credit_tables instead of state-specific code.
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
  #   - st_yctc (dbl)           : state young-child credit
  #   - st_cdctc (dbl)          : state child/dependent care credit
  #   - st_credits_nonref (dbl) : total nonrefundable credits
  #   - st_credits_ref (dbl)    : total refundable credits
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',               # (dbl)  federal AGI
    'st_agi',            # (dbl)  state income base
    'st_additions',      # (dbl)  additions to the federal AGI base
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
    'n_dep_ctc',         # (int)  federal CTC-qualifying dependent count
    'n_dep_eitc',        # (int)  federal EITC-qualifying dependent count
    'filing_status',     # (int)  filing status (1 single, 2 MFJ, 3 MFS, 4 HoH)
    'dep_status',        # (bool) whether filer is a dependent
    'age1',              # (int)  age of primary filer
    'age2',              # (int)  age of secondary filer (NA if none)
    'blind1',            # (bool) whether primary filer is blind
    'blind2',            # (bool) whether secondary filer is blind
    'ei1',               # (dbl)  primary earned income for independent EITCs
    'ei2',               # (dbl)  secondary earned income for independent EITCs
    'wages1',            # (dbl)  primary wages for zero-income child credits
    'wages2',            # (dbl)  secondary wages for zero-income child credits
    'sole_prop',         # (dbl)  Schedule C income/loss
    'sch_e',             # (dbl)  Schedule E income/loss
    'farm',              # (dbl)  Schedule F income/loss

    # State tax law (scalar; vector table params accessed by column name)
    'st_credits.eitc_match',
    'st_credits.eitc_refundable',
    'st_credits.eitc_less_household_credit',
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
    'st_credits.cdctc_match',
    'st_credits.cdctc_refundable',
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
    'st_credits.hh_mfs_half',
    'st_credits.family_credit_style',
    'st_credits.exempt_credit_style',
    'st_credits.exempt_credit_personal',
    'st_credits.exempt_credit_aged',
    'st_credits.exempt_credit_blind',
    'st_credits.exempt_credit_dep',
    'st_credits.exempt_credit_po_thresh',
    'st_credits.exempt_credit_po_width',
    'st_credits.exempt_credit_po_per_step',
    'st_credits.earned_credit_style',
    'st_credits.earned_credit_age_min',
    'st_credits.earned_credit_agi_limit',
    'st_credits.earned_credit_earned_limit',
    'st_credits.earned_credit_round',
    'st_credits.earned_credit_refundable',
    'st_credits.young_child_credit_style',
    'st_credits.young_child_credit_amount',
    'st_credits.young_child_credit_max_age',
    'st_credits.young_child_credit_phaseout_start',
    'st_credits.young_child_credit_phaseout_per_100',
    'st_credits.young_child_credit_zero_income_enabled',
    'st_credits.young_child_credit_zero_income_wage_limit',
    'st_credits.young_child_credit_zero_income_loss_limit',
    'st_credits.young_child_credit_zero_income_agi_limit'
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  n   = nrow(tax_unit)
  cn  = colnames(tax_unit)
  agi = tax_unit$agi

  # A state can provide four family-size columns (one through four-or-more)
  # of income bounds and preliminary-tax reduction shares. This keeps the
  # Kentucky-style credit a parameterized table, not a state module.
  family_credit_rate = rep(0, n)
  family_size = pmin(4L, 1L + (tax_unit$filing_status == 2) + tax_unit$n_dep)
  family_income = tax_unit$agi + tax_unit$st_additions
  for (f in 1:4) {
    bound_cols = paste0('st_credits.family_credit_f', f, '_bounds', 1:11)
    rate_cols  = paste0('st_credits.family_credit_f', f, '_rates', 1:11)
    if (all(c(bound_cols, rate_cols) %in% cn) &&
        any(!is.na(tax_unit[[bound_cols[1]]]))) {
      bounds = as.matrix(tax_unit[bound_cols])
      rates  = as.matrix(tax_unit[rate_cols])
      row    = which(family_size == f)
      if (length(row) > 0) {
        index = rowSums(family_income[row] > bounds[row, , drop = FALSE]) + 1L
        index = pmin(index, ncol(rates))
        family_credit_rate[row] = rates[cbind(row, index)]
      }
    }
  }

  # Generic independent earned-income credit. Style 1 uses the existing
  # triangular schedule; style 2 uses a dense row-based table for schedules
  # such as CalEITC.
  pick_earned_param = function(prefix) {
    cols = paste0('st_credits.', prefix, 1:4)
    out = rep(0, n)
    if (!all(cols %in% cn)) {
      return(out)
    }
    values = as.matrix(tax_unit[cols])
    slot = pmin(4L, 1L + tax_unit$n_dep_eitc)
    values[cbind(seq_len(n), slot)]
  }
  earned_income_raw = tax_unit$ei1 + tax_unit$ei2
  earned_income = pmax(0, earned_income_raw)
  earned_credit_curve = function(income) {
    phasein_rate  = pick_earned_param('earned_credit_phasein_rate')
    maximum       = pick_earned_param('earned_credit_max')
    phaseout_at   = pick_earned_param('earned_credit_phaseout_start')
    phaseout_rate = pick_earned_param('earned_credit_phaseout_rate')
    if_else(income > phaseout_at,
            pmax(0, maximum - (income - phaseout_at) * phaseout_rate),
            pmin(maximum, income * phasein_rate))
  }
  earned_credit_earned = earned_credit_curve(earned_income)
  earned_credit_agi    = earned_credit_curve(pmax(0, agi))
  earned_credit_age_ok = tax_unit$n_dep_eitc > 0 |
                         tax_unit$age1 >= tax_unit$st_credits.earned_credit_age_min |
                         (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) &
                          tax_unit$age2 >= tax_unit$st_credits.earned_credit_age_min)
  # Dependents of another taxpayer are ineligible for the independent
  # earned-income credit, mirroring the federal EITC (eitc.R) and the state
  # exempt/household/WFTC credits; ei1/ei2 are never dependent-zeroed upstream.
  earned_credit_eligible = tax_unit$dep_status != 1 & earned_income > 0 &
    earned_income < tax_unit$st_credits.earned_credit_earned_limit &
    agi < tax_unit$st_credits.earned_credit_agi_limit & earned_credit_age_ok
  earned_credit_table_earned = lookup_state_credit_table(
    earned_income, tax_unit$n_dep_eitc, credit_tables,
    'independent_earned_income'
  )
  earned_credit_table_agi = lookup_state_credit_table(
    pmax(0, agi), tax_unit$n_dep_eitc, credit_tables,
    'independent_earned_income'
  )
  earned_credit_table = if_else(
    pmax(0, agi) > pick_earned_param('earned_credit_agi_safe_harbor'),
    pmin(earned_credit_table_earned, earned_credit_table_agi),
    earned_credit_table_earned
  )
  st_earned_credit = case_when(
    tax_unit$st_credits.earned_credit_style == 1 & earned_credit_eligible ~
      pmin(earned_credit_earned, earned_credit_agi),
    tax_unit$st_credits.earned_credit_style == 2 & earned_credit_eligible ~
      earned_credit_table,
    TRUE ~ 0
  )
  st_earned_credit = if_else(tax_unit$st_credits.earned_credit_round == 1,
                             round(st_earned_credit), st_earned_credit)

  # Exemption credits are a credit (rather than an income exemption) in
  # California. The common per-credit phaseout is generic and applies to
  # personal, aged, blind, and dependent credits separately.
  n_taxpayers = 1 + (tax_unit$filing_status == 2)
  n_aged = (tax_unit$age1 >= 65) +
           (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) & tax_unit$age2 >= 65)
  n_blind = coalesce(tax_unit$blind1, 0) +
            (tax_unit$filing_status == 2 & coalesce(tax_unit$blind2, 0))
  credit_reduction = ceiling(pmax(0, agi - tax_unit$st_credits.exempt_credit_po_thresh) /
                               tax_unit$st_credits.exempt_credit_po_width) *
                     tax_unit$st_credits.exempt_credit_po_per_step
  taxpayer_credit = (tax_unit$dep_status != 1) * (
    n_taxpayers * pmax(0, tax_unit$st_credits.exempt_credit_personal - credit_reduction) +
    n_aged * pmax(0, tax_unit$st_credits.exempt_credit_aged - credit_reduction) +
    n_blind * pmax(0, tax_unit$st_credits.exempt_credit_blind - credit_reduction)
  )
  dependent_credit = tax_unit$n_dep *
                     pmax(0, tax_unit$st_credits.exempt_credit_dep - credit_reduction)
  st_exempt_credit = if_else(tax_unit$st_credits.exempt_credit_style == 1,
                             taxpayer_credit + dependent_credit, 0)

  # Count tracked dependents with ages in [lo, hi] (vectors allowed)
  n_dep_in = function(lo, hi) {
    (!is.na(tax_unit$dep_age1) & tax_unit$dep_age1 >= lo & tax_unit$dep_age1 <= hi) +
    (!is.na(tax_unit$dep_age2) & tax_unit$dep_age2 >= lo & tax_unit$dep_age2 <= hi) +
    (!is.na(tax_unit$dep_age3) & tax_unit$dep_age3 >= lo & tax_unit$dep_age3 <= hi)
  }

  # Refundable young-child credits can depend on an independent earned-income
  # credit. The zero-income path uses the available current-year loss proxy;
  # state data contracts can supply a more complete loss measure later.
  n_young_child = n_dep_in(0, tax_unit$st_credits.young_child_credit_max_age)
  yctc_wages = tax_unit$wages1 + tax_unit$wages2
  yctc_current_loss = pmax(0, -tax_unit$sole_prop) +
                      pmax(0, -tax_unit$sch_e) +
                      pmax(0, -tax_unit$farm)
  yctc_zero_income_eligible =
    tax_unit$st_credits.young_child_credit_zero_income_enabled == 1 &
    earned_income_raw <= 0 & tax_unit$n_dep_eitc > 0 &
    agi < tax_unit$st_credits.young_child_credit_zero_income_agi_limit &
    yctc_wages <= tax_unit$st_credits.young_child_credit_zero_income_wage_limit &
    yctc_current_loss <= tax_unit$st_credits.young_child_credit_zero_income_loss_limit
  yctc_unrounded = tax_unit$st_credits.young_child_credit_amount -
    pmax(0, earned_income -
           tax_unit$st_credits.young_child_credit_phaseout_start) / 100 *
    tax_unit$st_credits.young_child_credit_phaseout_per_100
  yctc_amount = case_when(
    yctc_unrounded > 0 & yctc_unrounded < 1 ~ 1,
    yctc_unrounded >= 1 ~ floor(yctc_unrounded + 0.5),
    TRUE ~ 0
  )
  st_yctc = if_else(
    tax_unit$st_credits.young_child_credit_style == 1 & n_young_child > 0 &
      (st_earned_credit > 0 | yctc_zero_income_eligible),
    yctc_amount, 0
  )

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
      st_exempt_credit = st_exempt_credit,
      st_earned_credit = st_earned_credit,
      st_yctc = st_yctc,

      # State EITC: match on the federal credit, less the household credit
      # (capped at remaining tax) where flagged (NY IT-215 lines 13-16)
      st_eitc = pmax(0, st_credits.eitc_match * eitc -
                        st_credits.eitc_less_household_credit *
                        pmin(st_hh_credit, pmax(0, st_tax_pre_credit))),

      # Family-size credit: a table-selected percentage of preliminary tax.
      st_family_credit = if_else(st_credits.family_credit_style == 1,
                                 st_tax_pre_credit * family_credit_rate, 0),

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

      # Dependent credit: separate qualifying-child and other-dependent
      # amounts, then a common percentage-point reduction for each $1,000 of
      # AGI above the state threshold (Arizona Form 140 Table V).
      dep_credit_base = st_credits.dep_credit_young_amount * n_dep_ctc +
                        st_credits.dep_credit_other_amount * pmax(0, n_dep - n_dep_ctc),
      dep_credit_factor = pmax(
        0,
        1 - st_credits.dep_credit_po_per_1k *
          ceiling(pmax(0, agi - st_credits.dep_credit_po_thresh) / 1000)
      ),
      st_dep_credit = if_else(st_credits.dep_credit_style == 1,
                              dep_credit_base * dep_credit_factor, 0),

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

      st_credits_nonref = st_hh_credit + prop_credit + st_dep_credit +
                          st_family_credit + st_exempt_credit +
                          st_eitc * (1 - st_credits.eitc_refundable) +
                          st_earned_credit * (1 - st_credits.earned_credit_refundable) +
                          st_cdctc * (1 - st_credits.cdctc_refundable),
      st_credits_ref    = st_eitc * st_credits.eitc_refundable + st_ctc +
                          st_earned_credit * st_credits.earned_credit_refundable +
                          st_yctc +
                          st_cdctc * st_credits.cdctc_refundable
    ) %>%
    select(all_of(return_vars$calc_st_credits)) %>%
    return()
}
