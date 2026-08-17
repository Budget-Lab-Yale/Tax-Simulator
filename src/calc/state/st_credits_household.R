#---------------------------------------------------------------------------
# Household / percentage-of-tax credit family (called by calc_st_credits):
# NY household credit, CA exemption credits, CT Table E percentage-of-tax
# rate, KY family-size credit rate, and the IL/CT property tax credit.
#---------------------------------------------------------------------------

# Law parameters this family reads (assembled into calc_st_credits req_vars)
st_credits_household_req_vars = c(
  'st_credits.hh_mfs_half',
  'st_credits.family_credit_style',
  'st_credits.family_credit_income_base',
  'st_credits.exempt_credit_style',
  'st_credits.exempt_credit_personal',
  'st_credits.exempt_credit_aged',
  'st_credits.exempt_credit_blind',
  'st_credits.exempt_credit_dep',
  'st_credits.exempt_credit_po_thresh',
  'st_credits.exempt_credit_po_width',
  'st_credits.exempt_credit_po_per_step',
  'st_credits.exempt_credit_po_base',
  'st_credits.ded_credit_rate',
  'st_credits.ded_credit_exempt_taxpayer',
  'st_credits.ded_credit_exempt_dep',
  'st_credits.ded_credit_dep_age0_extra',
  'st_credits.ded_credit_salt_cap',
  'st_credits.ded_credit_po_rate',
  'st_credits.ded_credit_po_thresh',
  'st_credits.ded_credit_po_base',
  'st_credits.prop_tax_credit_rate',
  'st_credits.prop_tax_credit_rate_cap',
  'st_credits.credit_agi_limit',
  'st_credits.prop_tax_credit_max',
  'st_credits.prop_tax_credit_po_thresh',
  'st_credits.prop_tax_credit_po_step',
  'st_credits.prop_tax_credit_po_rate',
  'st_credits.prop_tax_credit_restrict_aged_dep',
  'st_credits.percap_amount',
  'st_credits.percap_aged_addl',
  'st_credits.percap_table_income_base',
  'st_credits.stfc_income_base',
  'st_credits.stfc_add_nontax_ss',
  'st_credits.stfc_add_exempt_int'
)


st_credits_household = function(tax_unit, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # Calculates the household-structure credit family on a parsed tax unit
  # tibble (columns guaranteed by calc_st_credits).
  #
  # Parameters:
  #   - tax_unit (df)      : parsed tax unit tibble
  #   - credit_tables (df) : dense schedules (see build_state_credit_tables)
  #
  # Returns: list of per-row vectors --
  #   - st_hh_credit (dbl)       : NY-style household credit
  #   - st_exempt_credit (dbl)   : CA-style exemption credits
  #   - st_ded_credit (dbl)      : UT-style credit in lieu of deductions
  #   - family_credit_rate (dbl) : KY-style table rate (share of tax)
  #   - pct_credit_rate (dbl)    : CT Table E rate (share of tax)
  #   - prop_credit (dbl)        : IL/CT property tax credit
  #   - st_percap_credit (dbl)   : per-person credit (ID grocery credit)
  #----------------------------------------------------------------------------

  n   = nrow(tax_unit)
  agi = tax_unit$agi

  # Family-size credit rate (KY Schedule ITC Table C): dense table keyed by
  # family size (one through four-or-more), income rounded to whole dollars
  # per the form before the lookup. States without the table get zero
  family_size = pmin(4L, 1L + (tax_unit$filing_status == 2) + tax_unit$n_dep)
  family_income = st_income_base(
    tax_unit, tax_unit$st_credits.family_credit_income_base
  )
  family_credit_rate = lookup_state_credit_table(
    floor(family_income + 0.5), family_size, credit_tables,
    'family_size_tax_credit'
  )

  # Exemption credits are a credit (rather than an income exemption) in
  # California. The common per-credit phaseout is generic and applies to
  # personal, aged, blind, and dependent credits separately. The phase-out
  # income base is configurable (CA: federal AGI; OH $20 credit: the
  # means-test base less exemptions, with a one-step cliff)
  n_taxpayers = 1 + (tax_unit$filing_status == 2)
  n_aged = (tax_unit$age1 >= 65) +
           (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) & tax_unit$age2 >= 65)
  n_blind = coalesce(tax_unit$blind1, 0) +
            (tax_unit$filing_status == 2 & coalesce(tax_unit$blind2, 0))
  exempt_credit_po_income = st_income_base(
    tax_unit, tax_unit$st_credits.exempt_credit_po_base
  )
  credit_reduction = st_step_reduction(
    exempt_credit_po_income, tax_unit$st_credits.exempt_credit_po_thresh,
    tax_unit$st_credits.exempt_credit_po_width,
    tax_unit$st_credits.exempt_credit_po_per_step
  )
  taxpayer_credit = (tax_unit$dep_status != 1) * (
    n_taxpayers * pmax(0, tax_unit$st_credits.exempt_credit_personal - credit_reduction) +
    n_aged * pmax(0, tax_unit$st_credits.exempt_credit_aged - credit_reduction) +
    n_blind * pmax(0, tax_unit$st_credits.exempt_credit_blind - credit_reduction)
  )
  dependent_credit = tax_unit$n_dep *
                     pmax(0, tax_unit$st_credits.exempt_credit_dep - credit_reduction)
  st_exempt_credit = if_else(tax_unit$st_credits.exempt_credit_style == 1,
                             taxpayer_credit + dependent_credit, 0)

  #-------------------------------------------------------------------
  # Credit in lieu of deductions/exemptions (UT taxpayer tax credit,
  # 59-10-1018): rate times [per-taxpayer + per-dependent exemption
  # amounts plus the federal standard deduction or (itemized deductions
  # less the state/local income-tax component, capped)], reduced
  # ded_credit_po_rate per dollar of the enum income base over the
  # filing-status threshold. Dependents born during the year count
  # twice where flagged (UT 2023+; proxied by dependents age 0)
  #-------------------------------------------------------------------

  ded_credit_salt = pmin(
    pmax(0, tax_unit$salt_item_ded - tax_unit$salt_prop - tax_unit$salt_pers),
    tax_unit$st_credits.ded_credit_salt_cap
  )
  ded_credit_fed_ded = if_else(tax_unit$itemizing == 1,
                               pmax(0, tax_unit$item_ded - ded_credit_salt),
                               tax_unit$std_ded)
  ded_credit_n_dep = tax_unit$n_dep +
    tax_unit$st_credits.ded_credit_dep_age0_extra * st_n_dep_in(tax_unit, 0, 0)
  ded_credit_exempt =
    (tax_unit$dep_status != 1) * n_taxpayers *
      tax_unit$st_credits.ded_credit_exempt_taxpayer +
    ded_credit_n_dep * tax_unit$st_credits.ded_credit_exempt_dep
  ded_credit_po_income = st_income_base(
    tax_unit, tax_unit$st_credits.ded_credit_po_base
  )
  st_ded_credit = pmax(
    0,
    tax_unit$st_credits.ded_credit_rate *
      (ded_credit_exempt + ded_credit_fed_ded) -
    tax_unit$st_credits.ded_credit_po_rate *
      pmax(0, ded_credit_po_income - tax_unit$st_credits.ded_credit_po_thresh)
  )

  #----------------------------
  # Household credit (NY-style)
  #----------------------------

  st_hh_credit = rep(0, n)

  hh_bounds_s = st_family_matrix(tax_unit, 'st_credits.hh_agi_bounds_single',
                                 1:7)
  if (!is.null(hh_bounds_s)) {

    ub_s  = hh_bounds_s[, 2:7, drop = F]
    amt_s = st_family_matrix(tax_unit, 'st_credits.hh_amount_single', 1:6, F)
    hh_s  = st_band_value(agi, ub_s, amt_s)

    ub_o   = st_family_matrix(tax_unit, 'st_credits.hh_agi_bounds_other',
                              2:9, F)
    base_o = st_family_matrix(tax_unit, 'st_credits.hh_base_other', 1:8, F)
    incr_o = st_family_matrix(tax_unit, 'st_credits.hh_incr_other', 1:8, F)
    n_ex   = 1 + (tax_unit$filing_status == 2) + tax_unit$n_dep
    hh_o   = st_band_value(agi, ub_o, base_o + incr_o * (n_ex - 1))

    st_hh_credit = case_when(
      tax_unit$dep_status == 1     ~ 0,
      tax_unit$filing_status == 1  ~ hh_s,
      tax_unit$filing_status == 3  ~ hh_o * if_else(tax_unit$st_credits.hh_mfs_half == 1,
                                                    0.5, 1),
      TRUE                         ~ hh_o
    )
  }

  #--------------------------------------------------
  # Percentage-of-tax personal credit (CT Table E)
  #--------------------------------------------------

  # A filing-status-keyed dense table of state-AGI bands and credit rates:
  # the rate applies to the whole of tax before credits (schedule plus
  # add-back/recapture). Zero below the first band (where the exemption
  # exhausts the base anyway) and above the last. Income rounded to whole
  # dollars per the form before the lookup
  pct_credit_rate = lookup_state_credit_table(
    floor(tax_unit$st_agi + 0.5), rep(0L, n), credit_tables,
    'pct_of_tax_credit', filing_status = tax_unit$filing_status
  )

  # Property tax credit, two generic styles (mutually exclusive by
  # config): IL rate-style (rate times property taxes, denied above the
  # AGI limit) and CT capped-style (min(property tax, max credit) reduced
  # po_rate per po_step, or fraction, of state AGI over the threshold;
  # optionally restricted to aged-65+/with-dependent filers, 2017-2021).
  # Both are limited to observed salt_prop (known-difference: property
  # taxes of federal non-itemizers are underobserved)
  prop_credit_ct_eligible =
    tax_unit$st_credits.prop_tax_credit_restrict_aged_dep == 0 |
    tax_unit$age1 >= 65 |
    (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) & tax_unit$age2 >= 65) |
    tax_unit$n_dep > 0
  prop_credit_ct_factor = pmax(
    0,
    1 - st_step_reduction(tax_unit$st_agi,
                          tax_unit$st_credits.prop_tax_credit_po_thresh,
                          tax_unit$st_credits.prop_tax_credit_po_step,
                          tax_unit$st_credits.prop_tax_credit_po_rate)
  )
  prop_credit = pmin(tax_unit$st_credits.prop_tax_credit_rate * tax_unit$salt_prop,
                     tax_unit$st_credits.prop_tax_credit_rate_cap) *
                (agi <= tax_unit$st_credits.credit_agi_limit) +
                pmin(tax_unit$salt_prop, tax_unit$st_credits.prop_tax_credit_max) *
                prop_credit_ct_factor * prop_credit_ct_eligible

  #--------------------------------------------------
  # Per-person credit (ID grocery credit, 63-3024A)
  #--------------------------------------------------

  # Flat amount per taxpayer and dependent plus an aged add-on per 65+
  # filer. Dependent filers are ineligible (their amount is claimed on the
  # claiming return via n_dep). Part-year residence and the SNAP-months
  # proration are unobserved (documented known-differences); refundability
  # is split in calc_st_credits via percap_refundable.
  #
  # A state whose per-person amount is a published income-banded table
  # (HI Form N-311 food/excise credit: per-exemption amount by federal-AGI
  # band, filing-status keyed) carries it in credit_tables.csv under
  # credit_id percap_credit; the lookup returns zero outside the bands
  # (which encodes the eligibility ceiling) and zero for states without
  # rows. The banded amount counts PERSONS -- self, spouse, dependents --
  # so the aged add-on does not multiply it (HI counts people, not
  # exemptions; the flat percap_aged_addl remains ID-only)
  percap_table_income = st_income_base(
    tax_unit, tax_unit$st_credits.percap_table_income_base
  )
  percap_table_amt = lookup_state_credit_table(
    floor(percap_table_income + 0.5), rep(0L, n), credit_tables,
    'percap_credit', filing_status = tax_unit$filing_status
  )
  st_percap_credit = (tax_unit$dep_status != 1) * (
    (tax_unit$st_credits.percap_amount + percap_table_amt) *
      (n_taxpayers + tax_unit$n_dep) +
    tax_unit$st_credits.percap_aged_addl * n_aged
  )

  #--------------------------------------------------------------------
  # Household base credit from a dense table (ME sales tax fairness
  # credit, 36 M.R.S. 5213-A)
  #--------------------------------------------------------------------

  # Per-return amount keyed by filing status and capped dependent count,
  # with the stepped phase-out transcribed into the table's income bands
  # (credit_tables.csv, credit_id stfc). MFS ineligibility is encoded by
  # omitting filing-status-3 rows; dependent filers are ineligible. The
  # income concept is the enum base plus optional add-backs for
  # NONTAXABLE Social Security and exempt interest, approximating ME's
  # broad "total income" (loss and above-the-line add-backs are
  # documented approximations)
  stfc_income = st_income_base(tax_unit, tax_unit$st_credits.stfc_income_base) +
    tax_unit$st_credits.stfc_add_nontax_ss *
      pmax(0, tax_unit$gross_ss - tax_unit$txbl_ss) +
    tax_unit$st_credits.stfc_add_exempt_int * tax_unit$exempt_int
  st_stfc = (tax_unit$dep_status != 1) * lookup_state_credit_table(
    floor(stfc_income + 0.5), pmin(tax_unit$n_dep, 3L), credit_tables,
    'stfc', filing_status = tax_unit$filing_status
  )

  list(
    st_hh_credit       = st_hh_credit,
    st_exempt_credit   = st_exempt_credit,
    st_ded_credit      = st_ded_credit,
    family_credit_rate = family_credit_rate,
    pct_credit_rate    = pct_credit_rate,
    prop_credit        = prop_credit,
    st_percap_credit   = st_percap_credit,
    st_stfc            = st_stfc
  )
}
