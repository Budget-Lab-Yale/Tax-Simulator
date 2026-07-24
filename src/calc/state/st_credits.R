#-------------------------------------------
# Function to calculate state tax credits
#-------------------------------------------

# Set return variables for function
return_vars$calc_st_credits = c('st_hh_credit', 'st_eitc', 'st_ctc',
                                'st_dep_credit', 'st_cdctc', 'st_family_credit',
                                'st_exempt_credit', 'st_earned_credit', 'st_yctc',
                                'st_pct_credit', 'st_cli', 'st_ded_credit',
                                'st_age_credit', 'st_retire_credit',
                                'st_senior_credit', 'st_jfc',
                                'st_forgive_credit', 'st_percap_credit',
                                'st_marriage_credit', 'st_twoearner_credit',
                                'st_item_credit',
                                'st_credits_nonref', 'st_credits_ref')


calc_st_credits = function(tax_unit, fill_missings = F, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # Calculates state credits, orchestrating the five credit-family modules
  # (2026-07-17 review item #6):
  #   - st_credits_household.R : NY household credit, CA exemption credits,
  #                              UT taxpayer tax credit (credit in lieu of
  #                              deductions), CT Table E rate, KY family-size
  #                              rate, IL/CT property tax credit
  #   - st_credits_earned.R    : EITC matches (incl. the VA option choice and
  #                              CLI), CalEITC-style independent credits,
  #                              young-child credits
  #   - st_credits_child.R     : IL/NY/CO/UT child credits, CO FATC, AZ
  #                              dependent credit
  #   - st_credits_care.R      : CDCTC match and NY care-credit styles
  #   - st_credits_senior.R    : UT retirement/SS credits, OH banded
  #                              retirement + senior credits
  # Dense schedules are supplied through credit_tables instead of
  # state-specific code. Cross-family dependencies flow through arguments:
  # the household credit feeds the NY EITC offset, and the chosen state
  # EITC feeds the IL child credit.
  #
  # Two credits depend on REMAINING tax under the state's credit ordering
  # and are computed here after the family modules: the OH joint filing
  # credit (5747.05(E): banded share of tax net of the credits that precede
  # it under 5747.98) and the OH 2017-18 EITC limitation (50% of the same
  # remaining-tax quantity above the income threshold). For all other
  # nonrefundable credits, ordering does not change final liability.
  #
  # v1 approximations (documented known-differences):
  #  - NY ESCC style 1 folds the pre-TCJA ACTC into full refundability
  #  - CO CTC style 1 attributes the federal credit to under-6 children
  #    proportionally by child count
  #  - CO FATC's stepped phase-out is approximated linearly
  #  - MFS household credit uses own (not combined) AGI
  #  - NY college tuition and IL K-12 credits are data-limited (not computed)
  #  - the OH JFC qualifying-income test uses each spouse's earned income
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #   - credit_tables (df)   : dense schedules (see build_state_credit_tables)
  #
  # Returns: dataframe of following variables:
  #   - st_hh_credit (dbl)      : household credit (nonrefundable)
  #   - st_eitc (dbl)           : state EITC
  #   - st_ctc (dbl)            : state child credits (incl. CO FATC)
  #   - st_dep_credit (dbl)     : dependent credit (AZ-style)
  #   - st_cdctc (dbl)          : state child/dependent care credit
  #   - st_family_credit (dbl)  : family-size percentage-of-tax credit
  #   - st_exempt_credit (dbl)  : exemption credits (CA/OH-style)
  #   - st_earned_credit (dbl)  : independent earned-income credit
  #   - st_yctc (dbl)           : state young-child credit
  #   - st_pct_credit (dbl)     : percentage-of-tax credit (CT Table E)
  #   - st_cli (dbl)            : credit for low-income individuals (VA)
  #   - st_ded_credit (dbl)     : credit in lieu of deductions (UT)
  #   - st_age_credit (dbl)     : retirement/SS credit (UT)
  #   - st_retire_credit (dbl)  : banded retirement income credit (OH)
  #   - st_senior_credit (dbl)  : senior citizen credit (OH)
  #   - st_jfc (dbl)            : joint filing credit (OH)
  #   - st_forgive_credit (dbl) : poverty-based forgiveness credit (PA)
  #   - st_percap_credit (dbl)  : per-person credit (ID grocery credit)
  #   - st_marriage_credit (dbl): two-earner marriage credit (MN)
  #   - st_twoearner_credit (dbl): married couple credit (WI)
  #   - st_item_credit (dbl)    : itemized-deduction credit (WI)
  #   - st_credits_nonref (dbl) : total nonrefundable credits
  #   - st_credits_ref (dbl)    : total refundable credits
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',               # (dbl)  federal AGI
    'exempt_int',        # (dbl)  tax-exempt interest (PA forgiveness income)
    'alimony',           # (dbl)  alimony received (PA forgiveness income)
    'st_agi',            # (dbl)  state income base
    'st_txbl_inc',       # (dbl)  state taxable income (MN marriage credit)
    'st_additions',      # (dbl)  additions to the federal AGI base
    'st_bid',            # (dbl)  business carve-out deduction (OH MAGI addback)
    'st_exempt',         # (dbl)  state exemption allowance (means-test bases)
    'st_tax_pre_credit', # (dbl)  state tax before credits
    'st_age_package_taken', # (int) aged package claimed under exclusivity (calc_st_agi)
    'eitc',              # (dbl)  federal EITC
    'ctc_nonref',        # (dbl)  federal CTC, nonrefundable portion
    'ctc_ref',           # (dbl)  federal CTC, refundable portion
    'cdctc_nonref',      # (dbl)  federal CDCTC, nonrefundable portion
    'cdctc_ref',         # (dbl)  federal CDCTC, refundable portion
    'care_exp',          # (dbl)  eligible dependent care expenses
    'salt_prop',         # (dbl)  state/local real estate taxes paid
    'salt_pers',         # (dbl)  state/local personal property taxes paid
    'salt_item_ded',     # (dbl)  federal SALT deduction, capped (UT ded credit)
    'item_ded',          # (dbl)  federal itemized deductions post-limitation
    'std_ded',           # (dbl)  federal standard deduction (UT ded credit)
    'itemizing',         # (bool) whether unit itemizes federally
    'txbl_ss',           # (dbl)  taxable Social Security benefits (UT SS credit)
    'txbl_pens_dist',    # (dbl)  taxable pension distributions (OH retirement credit)
    'txbl_ira_dist',     # (dbl)  taxable IRA distributions (OH retirement credit)
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

    # State tax law (scalar; vector table params accessed by column name),
    # declared per credit-family module
    st_credits_household_req_vars,
    st_credits_earned_req_vars,
    st_credits_child_req_vars,
    st_credits_care_req_vars,
    st_credits_senior_req_vars,

    # Ordered-credit parameters read here (JFC and the EITC liability cap)
    'st_credits.jfc_cap',
    'st_credits.jfc_min_each_income',
    'st_credits.jfc_income_base',
    'st_credits.jfc_magi_limit',
    'st_credits.jfc_magi_limit_base',
    'st_credits.eitc_liab_cap_thresh',
    'st_credits.eitc_liab_cap_share',
    'st_credits.eitc_liab_cap_base',
    'st_credits.ctc_refundable',
    'st_credits.percap_refundable',
    'st_credits.mc_style',
    'st_credits.mc_min_lesser_income',
    'st_credits.mc_min_joint_txbl',
    'st_credits.mc_max',
    'st_credits.mc_share_offset',
    'st_credits.twoearner_rate',
    'st_credits.twoearner_max',
    'st_credits.item_credit_rate',
    'st_credits.item_credit_incl_medical',
    'st_credits.item_credit_incl_mortgage',
    'st_credits.item_credit_incl_investment',
    'st_credits.item_credit_incl_charity',
    'st_credits.item_credit_incl_casualty',
    'st_std_ded',        # (dbl) state standard deduction (WI itemized credit)
    'med_item_ded',      # (dbl) federal deductible medical (WI itemized credit)
    'mort_int_item_ded', # (dbl) federal deductible mortgage interest
    'inv_int_item_ded',  # (dbl) federal deductible investment interest
    'char_item_ded',     # (dbl) federal deductible charitable
    'casualty_item_ded'  # (dbl) federal deductible casualty losses
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Credit-family modules (cross-family inputs passed explicitly)
  hh     = st_credits_household(tax_unit, credit_tables)
  earn   = st_credits_earned(tax_unit, hh$st_hh_credit, credit_tables)
  child  = st_credits_child(tax_unit, earn$st_eitc)
  care   = st_credits_care(tax_unit)
  senior = st_credits_senior(tax_unit)

  # Percentage-of-tax credits and aggregation
  st_family_credit = if_else(tax_unit$st_credits.family_credit_style == 1,
                             tax_unit$st_tax_pre_credit * hh$family_credit_rate,
                             0)
  st_pct_credit = hh$pct_credit_rate * pmax(0, tax_unit$st_tax_pre_credit)

  # Remaining tax after the credits that precede the JFC/EITC in the OH
  # ordering (5747.98: retirement -> senior -> CDCTC -> exemption credit)
  cdctc_nonref_part = care$st_cdctc *
                      (1 - tax_unit$st_credits.cdctc_refundable)
  remaining_pre_jfc = pmax(
    0,
    tax_unit$st_tax_pre_credit - senior$st_retire_credit -
      senior$st_senior_credit - cdctc_nonref_part - hh$st_exempt_credit
  )

  # Joint filing credit (OH 5747.05(E)): a banded share of remaining tax,
  # capped per return; MFJ only, each spouse needing the minimum qualifying
  # income (earned-income proxy), and denied above the modified-income limit
  st_jfc = rep(0, nrow(tax_unit))
  jfc_ub = st_family_matrix(tax_unit, 'st_credits.jfc_bounds')
  if (!is.null(jfc_ub)) {
    jfc_rates  = st_family_matrix(tax_unit, 'st_credits.jfc_rates',
                                  1:ncol(jfc_ub), require_sentinel = FALSE)
    jfc_income = st_income_base(tax_unit, tax_unit$st_credits.jfc_income_base)
    jfc_magi   = st_income_base(tax_unit,
                                tax_unit$st_credits.jfc_magi_limit_base)
    jfc_rate   = st_band_value(jfc_income, jfc_ub, jfc_rates)
    jfc_elig   = tax_unit$filing_status == 2 &
                 tax_unit$ei1 >= tax_unit$st_credits.jfc_min_each_income &
                 tax_unit$ei2 >= tax_unit$st_credits.jfc_min_each_income &
                 jfc_magi < tax_unit$st_credits.jfc_magi_limit
    st_jfc = if_else(jfc_elig & !is.na(jfc_rate),
                     pmin(tax_unit$st_credits.jfc_cap,
                          jfc_rate * remaining_pre_jfc),
                     0)
  }

  # Two-earner marriage credit (MN Schedule M1MA, 290.0892): the joint
  # MFJ-schedule tax on state taxable income, less the SINGLE-schedule tax
  # on each spouse's imputed share -- the lesser earner's share is their
  # earned income less the share offset (half the MFJ standard deduction;
  # plus one exemption pre-2019), remainder to the other spouse. Both
  # eligibility floors must be met; result floored at zero and capped at
  # the published maximum. Earned income proxies the M1MA lines 1-5
  # concept (taxable pension/SS elements unobserved; known-difference).
  # The single-schedule brackets come in as the mc_single_brackets family
  # (the unit's own st_ord.brackets are its filing-status-mapped MFJ
  # schedule); rates are shared across statuses. Nonrefundable
  st_marriage_credit = rep(0, nrow(tax_unit))
  mc_br = st_family_matrix(tax_unit, 'st_credits.mc_single_brackets')
  if (!is.null(mc_br)) {
    mc_rt = st_family_matrix(tax_unit, 'st_ord.rates', 1:ncol(mc_br),
                             require_sentinel = FALSE)
    jt_br = st_family_matrix(tax_unit, 'st_ord.brackets', 1:ncol(mc_br),
                             require_sentinel = FALSE)
    mc_sched = function(y, br) {
      upper = cbind(br[, -1, drop = FALSE], Inf)
      upper[is.na(upper)] = Inf
      rowSums(mc_rt * pmax(0, pmin(y, upper) - br), na.rm = TRUE)
    }
    mc_ei_lo  = pmin(pmax(0, tax_unit$ei1), pmax(0, tax_unit$ei2))
    mc_share1 = pmax(0, pmin(mc_ei_lo - tax_unit$st_credits.mc_share_offset,
                             tax_unit$st_txbl_inc))
    mc_share2 = tax_unit$st_txbl_inc - mc_share1
    mc_elig   = tax_unit$st_credits.mc_style == 1 &
                tax_unit$filing_status == 2 &
                mc_ei_lo >= tax_unit$st_credits.mc_min_lesser_income &
                tax_unit$st_txbl_inc >= tax_unit$st_credits.mc_min_joint_txbl
    st_marriage_credit = if_else(
      mc_elig & !is.na(mc_br[, 1]),
      pmin(tax_unit$st_credits.mc_max,
           pmax(0, mc_sched(tax_unit$st_txbl_inc, jt_br) -
                   mc_sched(mc_share1, mc_br) -
                   mc_sched(mc_share2, mc_br))),
      0
    )
  }

  # Two-earner credit (WI married couple credit, 71.07(6)): rate times the
  # lesser-earning spouse's earned income, capped; MFJ only, nonrefundable
  st_twoearner_credit = if_else(
    tax_unit$filing_status == 2 & tax_unit$st_credits.twoearner_rate > 0,
    pmin(tax_unit$st_credits.twoearner_max,
         tax_unit$st_credits.twoearner_rate *
           pmin(pmax(0, tax_unit$ei1), pmax(0, tax_unit$ei2))),
    0
  )

  # Itemized-deduction credit (WI 71.07(5)): rate times the excess of the
  # selected federal-style components (no taxes) over the state standard
  # deduction; nonrefundable. Component amounts use the federal-floor
  # definitions (documented approximation)
  st_item_credit = tax_unit$st_credits.item_credit_rate * pmax(
    0,
    tax_unit$st_credits.item_credit_incl_medical * tax_unit$med_item_ded +
      tax_unit$st_credits.item_credit_incl_mortgage * tax_unit$mort_int_item_ded +
      tax_unit$st_credits.item_credit_incl_investment * tax_unit$inv_int_item_ded +
      tax_unit$st_credits.item_credit_incl_charity * tax_unit$char_item_ded +
      tax_unit$st_credits.item_credit_incl_casualty * tax_unit$casualty_item_ded -
      tax_unit$st_std_ded
  )

  # EITC liability-share limitation (OH 2017-18: above the income threshold,
  # the credit cannot exceed eitc_liab_cap_share of remaining pre-JFC tax)
  eitc_liab_income = st_income_base(tax_unit,
                                    tax_unit$st_credits.eitc_liab_cap_base)
  st_eitc = if_else(
    is.finite(tax_unit$st_credits.eitc_liab_cap_thresh) &
      eitc_liab_income > tax_unit$st_credits.eitc_liab_cap_thresh,
    pmin(earn$st_eitc,
         tax_unit$st_credits.eitc_liab_cap_share * remaining_pre_jfc),
    earn$st_eitc
  )

  tibble(
    st_hh_credit     = hh$st_hh_credit,
    st_eitc          = st_eitc,
    st_ctc           = child$st_ctc,
    st_dep_credit    = child$st_dep_credit,
    st_cdctc         = care$st_cdctc,
    st_family_credit = st_family_credit,
    st_exempt_credit = hh$st_exempt_credit,
    st_earned_credit = earn$st_earned_credit,
    st_yctc          = earn$st_yctc,
    st_pct_credit    = st_pct_credit,
    st_cli           = earn$st_cli,
    st_ded_credit    = hh$st_ded_credit,
    st_age_credit    = senior$st_age_credit,
    st_retire_credit = senior$st_retire_credit,
    st_senior_credit = senior$st_senior_credit,
    st_jfc           = st_jfc,
    st_forgive_credit = earn$st_forgive_credit,
    st_percap_credit  = hh$st_percap_credit,
    st_marriage_credit = st_marriage_credit,
    st_twoearner_credit = st_twoearner_credit,
    st_item_credit    = st_item_credit,

    st_credits_nonref = hh$st_hh_credit + hh$prop_credit + child$st_dep_credit +
                        st_family_credit + hh$st_exempt_credit + st_pct_credit +
                        earn$st_cli + hh$st_ded_credit + senior$st_age_credit +
                        senior$st_retire_credit + senior$st_senior_credit +
                        st_jfc + earn$st_forgive_credit + st_marriage_credit +
                        st_twoearner_credit + st_item_credit +
                        hh$st_percap_credit *
                          (1 - tax_unit$st_credits.percap_refundable) +
                        st_eitc * (1 - earn$st_eitc_ref_share) +
                        child$st_ctc *
                          (1 - tax_unit$st_credits.ctc_refundable) +
                        earn$st_earned_credit *
                          (1 - tax_unit$st_credits.earned_credit_refundable) +
                        care$st_cdctc *
                          (1 - tax_unit$st_credits.cdctc_refundable),
    st_credits_ref    = st_eitc * earn$st_eitc_ref_share +
                        child$st_ctc * tax_unit$st_credits.ctc_refundable +
                        earn$st_earned_credit *
                          tax_unit$st_credits.earned_credit_refundable +
                        earn$st_yctc +
                        care$st_cdctc * tax_unit$st_credits.cdctc_refundable +
                        hh$st_percap_credit *
                          tax_unit$st_credits.percap_refundable
  ) %>%
    select(all_of(return_vars$calc_st_credits)) %>%
    return()
}
