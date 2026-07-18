#---------------------------------------------------------------------------
# Retirement-age credit family (called by calc_st_credits): the UT
# retirement credit (per-person amount for a frozen birth cohort) and
# Social Security benefits credit (rate times taxable SS), which are
# mutually exclusive at the return level in Utah; and the OH banded
# retirement income credit and per-return senior citizen credit, both
# gated on a means-test income limit.
#---------------------------------------------------------------------------

# Law parameters this family reads (assembled into calc_st_credits req_vars)
st_credits_senior_req_vars = c(
  'st_credits.age_credit_amount',
  'st_credits.age_credit_min_age',
  'st_credits.age_credit_po_thresh',
  'st_credits.age_credit_po_rate',
  'st_credits.age_credit_income_base',
  'st_credits.ss_credit_rate',
  'st_credits.ss_credit_po_thresh',
  'st_credits.ss_credit_po_rate',
  'st_credits.ss_credit_income_base',
  'st_credits.ss_age_credit_exclusive',
  'st_credits.retire_credit_income_limit',
  'st_credits.retire_credit_income_base',
  'st_credits.senior_credit_amount',
  'st_credits.senior_credit_min_age',
  'st_credits.senior_credit_income_limit',
  'st_credits.senior_credit_income_base'
)


st_credits_senior = function(tax_unit) {

  #----------------------------------------------------------------------------
  # Calculates the retirement-age credit family on a parsed tax unit tibble
  # (columns guaranteed by calc_st_credits). All four credits are
  # nonrefundable.
  #
  # Returns: list of per-row vectors --
  #   - st_age_credit (dbl)    : UT-style retirement or SS credit (the larger
  #                              where the state makes them exclusive)
  #   - st_retire_credit (dbl) : OH-style banded retirement income credit
  #   - st_senior_credit (dbl) : OH-style per-return senior citizen credit
  #----------------------------------------------------------------------------

  n = nrow(tax_unit)

  # Per-person retirement-age credit (UT 59-10-1019): amount per filer at or
  # above the minimum age (year-keyed in config to encode the frozen
  # born-on-or-before-1952 cohort), reduced po_rate per dollar of the enum
  # income base over the filing-status threshold
  n_age_elig = (tax_unit$age1 >= tax_unit$st_credits.age_credit_min_age) +
               (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) &
                tax_unit$age2 >= tax_unit$st_credits.age_credit_min_age)
  age_credit_income = st_income_base(tax_unit,
                                     tax_unit$st_credits.age_credit_income_base)
  age_credit = pmax(
    0,
    tax_unit$st_credits.age_credit_amount * n_age_elig -
      tax_unit$st_credits.age_credit_po_rate *
      pmax(0, age_credit_income - tax_unit$st_credits.age_credit_po_thresh)
  )

  # Social Security benefits credit (UT 59-10-1042): the year's tax rate
  # times taxable SS benefits, reduced po_rate per dollar of the enum income
  # base over the filing-status threshold
  ss_credit_income = st_income_base(tax_unit,
                                    tax_unit$st_credits.ss_credit_income_base)
  ss_credit = pmax(
    0,
    tax_unit$st_credits.ss_credit_rate * pmax(0, tax_unit$txbl_ss) -
      tax_unit$st_credits.ss_credit_po_rate *
      pmax(0, ss_credit_income - tax_unit$st_credits.ss_credit_po_thresh)
  )

  # Return-level exclusivity (UT booklets: a return claiming the retirement
  # credit may not claim the SS credit): the unit takes the larger. Both
  # credits are nonrefundable, so the raw comparison equals the benefit
  # comparison whenever tax is positive
  st_age_credit = if_else(tax_unit$st_credits.ss_age_credit_exclusive == 1,
                          pmax(age_credit, ss_credit),
                          age_credit + ss_credit)

  # Banded retirement income credit (OH 5747.055(B)): a fixed amount by
  # band of qualifying retirement income (pensions + IRA distributions in
  # the state base), denied at or above the means-test income limit.
  # (lower, upper] band semantics with zero below the first band
  st_retire_credit = rep(0, n)
  rc_ub = st_family_matrix(tax_unit, 'st_credits.retire_credit_bounds')
  if (!is.null(rc_ub)) {
    rc_amt = st_family_matrix(tax_unit, 'st_credits.retire_credit_amounts',
                              1:ncol(rc_ub), require_sentinel = FALSE)
    retire_income = pmax(0, tax_unit$txbl_pens_dist + tax_unit$txbl_ira_dist)
    rc_test_income = st_income_base(
      tax_unit, tax_unit$st_credits.retire_credit_income_base
    )
    st_retire_credit = st_band_value(retire_income, rc_ub, rc_amt) *
      (rc_test_income < tax_unit$st_credits.retire_credit_income_limit)
    st_retire_credit[is.na(st_retire_credit)] = 0
  }

  # Per-return senior citizen credit (OH 5747.055(F)): flat amount when
  # either filer is at or above the minimum age, denied at or above the
  # means-test income limit
  senior_age_ok = tax_unit$age1 >= tax_unit$st_credits.senior_credit_min_age |
                  (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) &
                   tax_unit$age2 >= tax_unit$st_credits.senior_credit_min_age)
  senior_test_income = st_income_base(
    tax_unit, tax_unit$st_credits.senior_credit_income_base
  )
  st_senior_credit = tax_unit$st_credits.senior_credit_amount *
    senior_age_ok *
    (senior_test_income < tax_unit$st_credits.senior_credit_income_limit)

  list(
    st_age_credit    = st_age_credit,
    st_retire_credit = st_retire_credit,
    st_senior_credit = st_senior_credit
  )
}
