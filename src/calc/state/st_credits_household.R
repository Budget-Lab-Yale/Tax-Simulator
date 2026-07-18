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
  'st_credits.prop_tax_credit_rate',
  'st_credits.credit_agi_limit',
  'st_credits.prop_tax_credit_max',
  'st_credits.prop_tax_credit_po_thresh',
  'st_credits.prop_tax_credit_po_step',
  'st_credits.prop_tax_credit_po_rate',
  'st_credits.prop_tax_credit_restrict_aged_dep'
)


st_credits_household = function(tax_unit) {

  #----------------------------------------------------------------------------
  # Calculates the household-structure credit family on a parsed tax unit
  # tibble (columns guaranteed by calc_st_credits).
  #
  # Returns: list of per-row vectors --
  #   - st_hh_credit (dbl)       : NY-style household credit
  #   - st_exempt_credit (dbl)   : CA-style exemption credits
  #   - family_credit_rate (dbl) : KY-style table rate (share of tax)
  #   - pct_credit_rate (dbl)    : CT Table E rate (share of tax)
  #   - prop_credit (dbl)        : IL/CT property tax credit
  #----------------------------------------------------------------------------

  n   = nrow(tax_unit)
  agi = tax_unit$agi

  # A state can provide four family-size columns (one through four-or-more)
  # of income bounds and preliminary-tax reduction shares. This keeps the
  # Kentucky-style credit a parameterized table, not a state module.
  family_credit_rate = rep(0, n)
  family_size = pmin(4L, 1L + (tax_unit$filing_status == 2) + tax_unit$n_dep)
  family_income = st_income_base(
    tax_unit, tax_unit$st_credits.family_credit_income_base
  )
  for (f in 1:4) {
    bounds = st_family_matrix(
      tax_unit, paste0('st_credits.family_credit_f', f, '_bounds'), 1:11
    )
    rates = st_family_matrix(
      tax_unit, paste0('st_credits.family_credit_f', f, '_rates'), 1:11,
      require_sentinel = FALSE
    )
    if (!is.null(bounds) && !is.null(rates)) {
      row = which(family_size == f)
      if (length(row) > 0) {
        index = st_band_index_upper(family_income[row],
                                    bounds[row, , drop = FALSE])
        family_credit_rate[row] = rates[cbind(row, index)]
      }
    }
  }

  # Exemption credits are a credit (rather than an income exemption) in
  # California. The common per-credit phaseout is generic and applies to
  # personal, aged, blind, and dependent credits separately.
  n_taxpayers = 1 + (tax_unit$filing_status == 2)
  n_aged = (tax_unit$age1 >= 65) +
           (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) & tax_unit$age2 >= 65)
  n_blind = coalesce(tax_unit$blind1, 0) +
            (tax_unit$filing_status == 2 & coalesce(tax_unit$blind2, 0))
  credit_reduction = st_step_reduction(
    agi, tax_unit$st_credits.exempt_credit_po_thresh,
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

  # A filing-status-mapped step table of state-AGI bands and credit rates:
  # the rate applies to the whole of tax before credits (schedule plus
  # add-back/recapture). Zero below the first bound (where the exemption
  # exhausts the base anyway) and above the last. Bounds are one longer
  # than rates; unused tail rows pad with repeated bounds and zero rates.
  pct_credit_rate = rep(0, n)
  pct_r = st_family_matrix(tax_unit, 'st_credits.pct_credit_rates',
                           require_sentinel = FALSE)
  if (!is.null(pct_r) &&
      any(!is.na(tax_unit$st_credits.pct_credit_agi_bounds1))) {
    n_pct = ncol(pct_r)
    pct_b = st_family_matrix(tax_unit, 'st_credits.pct_credit_agi_bounds',
                             1:(n_pct + 1), require_sentinel = FALSE)
    pct_credit_rate = st_band_value(
      tax_unit$st_agi,
      upper  = pct_b[, 2:(n_pct + 1), drop = FALSE],
      values = pct_r,
      lower  = pct_b[, 1:n_pct, drop = FALSE]
    )
  }

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
  prop_credit = tax_unit$st_credits.prop_tax_credit_rate * tax_unit$salt_prop *
                (agi <= tax_unit$st_credits.credit_agi_limit) +
                pmin(tax_unit$salt_prop, tax_unit$st_credits.prop_tax_credit_max) *
                prop_credit_ct_factor * prop_credit_ct_eligible

  list(
    st_hh_credit       = st_hh_credit,
    st_exempt_credit   = st_exempt_credit,
    family_credit_rate = family_credit_rate,
    pct_credit_rate    = pct_credit_rate,
    prop_credit        = prop_credit
  )
}
