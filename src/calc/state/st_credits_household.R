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
  'st_credits.family_credit_child_max_age',
  'st_credits.family_credit_mfs_combined',
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
  'st_credits.percap_income_limit',
  'st_credits.percap_income_limit_elig',
  'st_credits.percap_elig_age',
  'st_credits.percap_elig_child_age',
  'st_credits.percap_elig_disabled',
  'st_credits.percap_dep_max_age',
  'st_credits.percap_add_nontax_ss',
  'st_credits.percap_add_exempt_int',
  'st_credits.percap_add_hh_resources',
  'st_credits.percap_after_eitc',
  'st_credits.percap_add_spouse_mfs',
  'st_credits.lictr',
  'st_credits.lictr_income_base',
  'st_credits.lictr_add_nontax_ss',
  'st_credits.lictr_add_exempt_int',
  'st_credits.lictr_add_hh_resources',
  'st_credits.lictr_unnet_losses',
  'st_credits.lictr_add_nol',
  'st_credits.lictr_aged_exemptions',
  'st_credits.lictr_blind_exemptions',
  'st_credits.lictr_mfs_share',
  'st_credits.stfc_income_base',
  'st_credits.stfc_add_nontax_ss',
  'st_credits.stfc_add_exempt_int',
  'st_credits.stfc_unnet_losses'
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
  # Dependents counted by age from the three observed slots (shared by the
  # family-size and per-person credits); Inf = every dependent
  dep_ages = cbind(tax_unit$dep_age1, tax_unit$dep_age2, tax_unit$dep_age3)
  n_dep_under = function(max_age) {
    if_else(is.infinite(max_age), as.numeric(tax_unit$n_dep),
            rowSums(dep_ages < max_age, na.rm = TRUE))
  }
  # KRS 141.066(1)(b),(e): family size counts qualifying CHILDREN (under 19,
  # or under 24 if a student -> KY 24), not every dependent; (1)(c),(4): a
  # married-separate filer living with the spouse counts the spouse and
  # adds the spouse's federal AGI -- read from the spouse_agi_mfs optional
  # data column, gated on has_spouse_agi_mfs because zero is a real value,
  # and switched off by living_apart (S43). Own income floors at zero (MGI
  # worksheet line (a))
  mfs_combined = tax_unit$st_credits.family_credit_mfs_combined == 1 &
                 tax_unit$filing_status == 3 & tax_unit$has_spouse_agi_mfs &
                 !coalesce(tax_unit$living_apart, FALSE)
  family_size = pmin(4L, 1L + (tax_unit$filing_status == 2 | mfs_combined) +
                       n_dep_under(tax_unit$st_credits.family_credit_child_max_age))
  family_income = pmax(0, st_income_base(
    tax_unit, tax_unit$st_credits.family_credit_income_base
  )) + mfs_combined * pmax(0, tax_unit$spouse_agi_mfs)
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
  #
  # Eligibility gate and income ceilings (KS food sales tax credit, OK sales
  # tax relief credit; params_schema.yaml percap_* comment). The income
  # concept adds nontaxable Social Security, exempt interest and the
  # hh_resources_inc optional data column to the enum base where a state
  # says so (OK's "gross household income"); dependents count only under
  # percap_dep_max_age, from the observed age slots
  percap_table_income = st_income_base(
    tax_unit, tax_unit$st_credits.percap_table_income_base
  )
  percap_income = percap_table_income +
    (tax_unit$filing_status == 3) * tax_unit$st_credits.percap_add_spouse_mfs *
      pmax(0, tax_unit$spouse_agi_mfs) +
    tax_unit$st_credits.percap_add_nontax_ss *
      pmax(0, tax_unit$gross_ss - tax_unit$txbl_ss) +
    tax_unit$st_credits.percap_add_exempt_int * tax_unit$exempt_int +
    tax_unit$st_credits.percap_add_hh_resources * tax_unit$hh_resources_inc
  percap_table_amt = lookup_state_credit_table(
    floor(percap_table_income + 0.5), rep(0L, n), credit_tables,
    'percap_credit', filing_status = tax_unit$filing_status
  )
  percap_dep_count = n_dep_under(tax_unit$st_credits.percap_dep_max_age)
  percap_elig =
    tax_unit$age1 >= tax_unit$st_credits.percap_elig_age |
    (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) &
       tax_unit$age2 >= tax_unit$st_credits.percap_elig_age) |
    n_dep_under(tax_unit$st_credits.percap_elig_child_age) > 0 |
    (tax_unit$st_credits.percap_elig_disabled == 1 &
       (coalesce(tax_unit$disabled1, FALSE) | coalesce(tax_unit$disabled2, FALSE)))
  percap_limit = if_else(percap_elig, tax_unit$st_credits.percap_income_limit_elig,
                                      tax_unit$st_credits.percap_income_limit)
  st_percap_credit = (tax_unit$dep_status != 1) * (percap_income <= percap_limit) * (
    (tax_unit$st_credits.percap_amount + percap_table_amt) *
      (n_taxpayers + percap_dep_count) +
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
  # Losses and above-the-line adjustments that a "total income" concept adds
  # back to federal AGI (ME 5213-A/5219-KK total income; NM 7-2-2(L)
  # "undiminished by losses"): each loss (the capital loss only to the $3,000
  # federal AGI actually nets) and the above-the-line deductions. S-corp
  # losses and above_ded are read optionally so synthetic test units without
  # them still compute
  opt_col = function(name) {
    if (name %in% colnames(tax_unit)) coalesce(tax_unit[[name]], 0) else rep(0, n)
  }
  unnetted_losses = pmax(0, -tax_unit$sole_prop) + pmax(0, -tax_unit$farm) +
    pmin(3000, pmax(0, -(tax_unit$kg_lt + tax_unit$kg_st))) +
    pmax(0, tax_unit$rent_loss) + pmax(0, tax_unit$part_active_loss) +
    pmax(0, tax_unit$part_passive_loss) +
    pmax(0, opt_col('scorp_active_loss')) +
    pmax(0, opt_col('scorp_passive_loss')) +
    pmax(0, opt_col('above_ded'))
  stfc_income = st_income_base(tax_unit, tax_unit$st_credits.stfc_income_base) +
    tax_unit$st_credits.stfc_add_nontax_ss *
      pmax(0, tax_unit$gross_ss - tax_unit$txbl_ss) +
    tax_unit$st_credits.stfc_add_exempt_int * tax_unit$exempt_int +
    tax_unit$st_credits.stfc_unnet_losses * unnetted_losses
  st_stfc = (tax_unit$dep_status != 1) * lookup_state_credit_table(
    floor(stfc_income + 0.5), pmin(tax_unit$n_dep, 3L), credit_tables,
    'stfc', filing_status = tax_unit$filing_status
  )

  #--------------------------------------------------------------------
  # Low income comprehensive tax rebate from a dense table (NM PIT-RC
  # Table 1, NMSA 7-2-14; params_schema.yaml lictr_* comment)
  #--------------------------------------------------------------------

  # Modified gross income: every household member's income from whatever
  # source, undiminished by losses, taxable or not. The observable part is
  # the enum base plus nontaxable Social Security, exempt interest and the
  # positive part of each loss; public assistance, child support, gifts
  # and the like come from the hh_resources_inc optional data column (S43;
  # zero until published, which OVERSTATES eligibility for transfer
  # recipients -- documented per state). The exemption key counts
  # taxpayers, dependents and the per-65+/per-blind add-ons; the lookup
  # caps the key at the table's "6 or more" column and returns zero above
  # the top band. Negative MGI uses the first row (7-2-14(E))
  lictr_income = st_income_base(tax_unit, tax_unit$st_credits.lictr_income_base) +
    tax_unit$st_credits.lictr_add_nontax_ss *
      pmax(0, tax_unit$gross_ss - tax_unit$txbl_ss) +
    tax_unit$st_credits.lictr_add_exempt_int * tax_unit$exempt_int +
    tax_unit$st_credits.lictr_add_hh_resources * tax_unit$hh_resources_inc +
    # "undiminished by losses" and with no deductions (shared add-back above);
    # a net operating loss carried into the year (negative other income in
    # the PUF) is a deduction, not income, so it is added back too. Form 4797
    # ordinary losses (other_gains) are un-netted here, outside the shared
    # helper, so the ME STFC income (validated without them) is unchanged
    tax_unit$st_credits.lictr_unnet_losses *
      (unnetted_losses + pmax(0, -opt_col('other_gains'))) +
    tax_unit$st_credits.lictr_add_nol * pmax(0, -tax_unit$other_inc)
  lictr_key = as.integer(n_taxpayers + tax_unit$n_dep +
                         tax_unit$st_credits.lictr_aged_exemptions * n_aged +
                         tax_unit$st_credits.lictr_blind_exemptions * n_blind)
  st_lictr = tax_unit$st_credits.lictr * (tax_unit$dep_status != 1) *
    if_else(tax_unit$filing_status == 3, tax_unit$st_credits.lictr_mfs_share, 1) *
    lookup_state_credit_table(floor(pmax(0, lictr_income) + 0.5), lictr_key,
                              credit_tables, 'lictr')

  list(
    st_hh_credit       = st_hh_credit,
    st_exempt_credit   = st_exempt_credit,
    st_ded_credit      = st_ded_credit,
    family_credit_rate = family_credit_rate,
    pct_credit_rate    = pct_credit_rate,
    prop_credit        = prop_credit,
    st_percap_credit   = st_percap_credit,
    st_stfc            = st_stfc,
    st_lictr           = st_lictr
  )
}
