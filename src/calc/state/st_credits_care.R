#---------------------------------------------------------------------------
# Child/dependent care credit family (called by calc_st_credits): flat
# federal-CDCTC matches (GA/KY-style), the NY styles (share table of the
# federal credit; own-rate credit on capped expenses) and the MD style
# (stepped-rate share of the federal credit).
#---------------------------------------------------------------------------

# Law parameters this family reads (assembled into calc_st_credits req_vars)
st_credits_care_req_vars = c(
  'st_credits.cdctc_match',
  'st_credits.cdctc_refundable',
  'st_credits.cdctc_style',
  'st_credits.cdctc_fed_base',
  'st_credits.cdctc_base_liab_limit',
  'st_credits.cdctc_fed_base_switch_income',
  'st_credits.cdctc_lowinc_rate',
  'st_credits.cdctc_lowinc_agi_limit',
  'st_credits.cdctc_lowinc_agi_limit_joint',
  'st_credits.cdctc_lowinc_qual_share',
  'st_credits.cdctc_lowinc_expense_base', # (int) 1 = low-income credit on capped expenses, not the federal credit (CO)
  'st_credits.cdctc_lowinc_cap1',         # (dbl) low-income cap, one qualifying child
  'st_credits.cdctc_lowinc_cap2',         # (dbl) low-income cap, two or more
  'st_credits.cdctc_lowinc_no_fed_tax',   # (int) 1 = only where federal tax before credits is zero (CO)
  'st_credits.cdctc_ref_qual_share',
  'st_credits.cdctc_rate_max',
  'st_credits.cdctc_rate_floor',
  'st_credits.cdctc_rate_po_per_1k',
  'st_credits.cdctc_rate_po_start',
  'st_credits.cdctc_rate_po_step',
  'st_credits.cdctc_expense_ei_limit',
  'st_credits.cdctc_share_income_base',
  'st_credits.cdctc_rate_income_base',    # (int) st_income_base enum for the style-2 rate slide (default 2, state AGI)
  'st_credits.cdctc_cap_amount',
  'st_credits.cdctc_cap_thresh',
  'st_credits.cdctc_cap_po_rate',
  'st_credits.cdctc_cap_per_return',      # (int) the cap is per return, not per child (LA $25)
  'st_credits.cdctc_style_switch_agi',    # (dbl) above this federal AGI, use style 1 (LA $25,000)
  'st_credits.care_credit',               # (int) care credit on (MA 2021-22 DCTC/HDTC)
  'st_credits.care_credit_per_dep_cap',   # (dbl) expense cap per qualifying child
  'st_credits.care_credit_dep_limit',     # (int) qualifying-child count cap
  'st_credits.care_credit_age_limit',     # (int) qualifying child age <= this
  'st_credits.care_credit_hh_member_amt', # (dbl) flat amount per household member
  'st_credits.care_credit_hh_member_age_limit', # (int) member age <= this
  'st_credits.care_credit_hh_aged_min',   # (int) or dependent age >= this
  'st_credits.care_credit_hh_limit',      # (int) member count cap
  'st_credits.care_credit_mfs_ineligible',# (int) married filing separately barred
  'st_credits.care_credit_refundable',    # (int) refundable
  'st_credits.care_fpg_style',            # (int) poverty-banded care credit on (OR WFHDC)
  'st_credits.care_fpg_exp_cap1',         # (dbl) expense cap, one qualifying child
  'st_credits.care_fpg_exp_cap2',         # (dbl) expense cap, two or more
  'st_credits.care_fpg_child_age_limit',  # (int) qualifying child year-end age <= this
  'st_credits.care_fpg_age_cut1',         # (int) Jan-1 age below this: table column 1
  'st_credits.care_fpg_age_cut2',         # (int) Jan-1 age below this: column 2 (else 3)
  'st_credits.care_fpg_income_max_state'  # (int) 1 = income is the greater of federal and state AGI
)


st_credits_care = function(tax_unit, credit_tables = NULL) {

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
    cdctc_share_income = st_income_base(
      tax_unit, tax_unit$st_credits.cdctc_share_income_base
    )
    cdctc_ny_share = st_band_interp(cdctc_share_income, b_c, s0, s1)
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

  # Own-rate slide: continuous per-$1,000 (NY) or, where a step is encoded,
  # a stepped reduction of cdctc_rate_po_per_1k per step or fraction thereof
  # (HI Schedule X: 0.01 per $5,000 band of Hawaii AGI over $25,000). The
  # slide runs on state AGI by default; AR TY2021 runs it on federal AGI
  # because the credit it shares is a recomputed federal one
  cdctc_rate_income = st_income_base(tax_unit, tax_unit$st_credits.cdctc_rate_income_base)
  cdctc_rate_red = if_else(
    is.finite(tax_unit$st_credits.cdctc_rate_po_step),
    st_step_reduction(cdctc_rate_income,
                      tax_unit$st_credits.cdctc_rate_po_start,
                      tax_unit$st_credits.cdctc_rate_po_step,
                      tax_unit$st_credits.cdctc_rate_po_per_1k),
    tax_unit$st_credits.cdctc_rate_po_per_1k *
      pmax(0, cdctc_rate_income -
              tax_unit$st_credits.cdctc_rate_po_start) / 1000
  )
  cdctc_rate2 = pmax(tax_unit$st_credits.cdctc_rate_floor,
                     tax_unit$st_credits.cdctc_rate_max - cdctc_rate_red)

  # Style-2 expenses capped at each spouse's earned income where flagged
  # (the federal 2441 rule, carried by HI Schedule X; NY 2026 encoded
  # without it, so the default leaves it off)
  cdctc_ei_cap = if_else(
    tax_unit$st_credits.cdctc_expense_ei_limit == 1,
    pmax(0, if_else(tax_unit$filing_status == 2,
                    pmin(tax_unit$ei1, tax_unit$ei2), tax_unit$ei1)),
    Inf
  )
  # Louisiana runs BOTH computations in one credit, split at a federal-AGI
  # line (R.S. 47:297.4): at or below $25,000 the state computes the credit
  # from its own worksheet -- expenses, the earned-income limit and a sliding
  # decimal, halved -- and refunds it; above the line the credit is a share
  # of the FEDERAL credit and is nonrefundable. Encoded as a switch on the
  # style already in force rather than a third style, since both
  # computations are here. .inf = no switch
  cdctc_style_v = if_else(
    is.finite(tax_unit$st_credits.cdctc_style_switch_agi) &
      tax_unit$agi > tax_unit$st_credits.cdctc_style_switch_agi,
    1, tax_unit$st_credits.cdctc_style
  )
  # The federal credit a share-based state credit starts from
  # (cdctc_fed_base):
  #   0 = as claimed (Form 2441 line 11, capped at federal tax; default)
  #   1 = the tentative federal credit before that cap (2441 line 9/9c;
  #       cdctc_potential), which pays families with no federal tax
  #   2 = the state's own recomputation of the federal formula: capped
  #       expenses times the cdctc_rate_* decimal (CA FTB 3506 line 8, NY
  #       IT-216 line 11 with its own caps, WI 2024+ Schedule WI-2441, and
  #       the pre-ARPA TY2021 recomputations of CA/NY/KY)
  # cdctc_base_liab_limit = 1 caps the base at federal tax less the foreign
  # tax credit (KY Form 2441-K line 10, TY2021). A tiered credit uses the
  # chosen base only at or below cdctc_fed_base_switch_income (measured on
  # the share-table income base) and the claimed credit above it (NE Form
  # 2441N: at or below $29,000 federal AGI; OH: below $20,000)
  # A state credit recomputed from expenses shares the federal take-up draw
  # (cdctc_takeup; 90% calibrated in cdctc.R), as the claimed and tentative
  # bases already do -- JI 2026-10-08
  cdctc_claimed = tax_unit$cdctc_nonref + tax_unit$cdctc_ref
  cdctc_own = if_else(n_care_v > 0,
                      cdctc_rate2 * pmin(tax_unit$care_exp, cdctc_cap_vec,
                                         cdctc_ei_cap) * tax_unit$cdctc_takeup,
                      0)
  cdctc_fed = case_when(
    tax_unit$st_credits.cdctc_fed_base == 1 ~ tax_unit$cdctc_potential,
    tax_unit$st_credits.cdctc_fed_base == 2 ~ cdctc_own,
    TRUE ~ cdctc_claimed
  )
  cdctc_fed = if_else(tax_unit$st_credits.cdctc_base_liab_limit == 1,
                      pmin(cdctc_fed, pmax(0, tax_unit$liab_bc - tax_unit$ftc)),
                      cdctc_fed)
  cdctc_fed = if_else(
    st_income_base(tax_unit, tax_unit$st_credits.cdctc_share_income_base) >
      tax_unit$st_credits.cdctc_fed_base_switch_income,
    cdctc_claimed, cdctc_fed
  )
  # Style 3: a share of the federal credit at the stepped own-rate slide --
  # cdctc_rate_max less cdctc_rate_po_per_1k per step (or fraction) of income
  # over cdctc_rate_po_start. MD Tax-Gen 10-716(c)-(d), Form 502CR Part B:
  # 32% of the federal credit, reduced 1% of itself (0.0032) per $2,000
  # ($3,000 joint) of federal AGI over $30,000 ($50,000); TY2017-18, 32.5%
  # less 10% of itself per $1,000 over $41,000. The style-1 share table
  # cannot hold a 40-step chart, and the rate slide already is one
  cdctc_ny = case_when(
    cdctc_style_v == 1 ~
      cdctc_ny_share * cdctc_fed,
    cdctc_style_v == 2 ~ cdctc_own,
    cdctc_style_v == 3 ~ cdctc_rate2 * cdctc_fed,
    TRUE ~ 0
  )
  st_cdctc = tax_unit$st_credits.cdctc_match * cdctc_fed + cdctc_ny

  # Income-capped variant (MN M1CD): above the threshold, the credit is
  # limited to cap_amount per qualifying person (up to two) less po_rate
  # times the excess AGI (a cliff at the threshold, as the form computes).
  # Louisiana's version of the same cap is a flat $25 per RETURN however many
  # children there are, hence the per-return flag
  cdctc_cap_units = if_else(tax_unit$st_credits.cdctc_cap_per_return == 1,
                            1, pmin(2, n_care_v))
  st_cdctc = if_else(
    is.finite(tax_unit$st_credits.cdctc_cap_thresh) &
      tax_unit$agi > tax_unit$st_credits.cdctc_cap_thresh,
    pmin(st_cdctc,
         pmax(0, tax_unit$st_credits.cdctc_cap_amount * cdctc_cap_units -
                 tax_unit$st_credits.cdctc_cap_po_rate *
                   (tax_unit$agi - tax_unit$st_credits.cdctc_cap_thresh))),
    st_cdctc
  )

  # Low-income refundable alternative (VT 32 V.S.A. 5828c through TY2021,
  # IN-112 Part II): cdctc_lowinc_rate of the same federal base, prorated by
  # the share of care paid to qualifying (accredited) providers, refundable,
  # for federal AGI at or below the limit -- "instead of" the regular credit,
  # so the unit keeps whichever pays more. Provider status is unobserved:
  # cdctc_lowinc_qual_share is an assumption, documented per state
  # Expense-based variant (CO 39-22-119.5, DR 0347 Part IV): the rate times
  # the smallest of care expenses and each earner's earned income (line 3),
  # capped by the qualifying-child count (Table A), never for a separate
  # filer, and only where federal tax before credits is zero (line 6)
  lowinc_exp = pmin(tax_unit$care_exp, pmax(0, tax_unit$ei1),
                    if_else(tax_unit$filing_status == 2,
                            pmax(0, tax_unit$ei2), Inf)) * tax_unit$cdctc_takeup
  lowinc_cap = if_else(n_care_v >= 2, tax_unit$st_credits.cdctc_lowinc_cap2,
                       tax_unit$st_credits.cdctc_lowinc_cap1)
  lowinc_amt = if_else(
    tax_unit$st_credits.cdctc_lowinc_expense_base == 1,
    if_else(n_care_v > 0 & tax_unit$filing_status != 3,
            pmin(tax_unit$st_credits.cdctc_lowinc_rate * lowinc_exp, lowinc_cap), 0),
    tax_unit$st_credits.cdctc_lowinc_rate * cdctc_fed *
      tax_unit$st_credits.cdctc_lowinc_qual_share
  )
  lowinc_amt = if_else(tax_unit$st_credits.cdctc_lowinc_no_fed_tax == 1 &
                         tax_unit$liab_bc > 0, 0, lowinc_amt)
  lowinc_limit = if_else(tax_unit$filing_status == 2,
                         tax_unit$st_credits.cdctc_lowinc_agi_limit_joint,
                         tax_unit$st_credits.cdctc_lowinc_agi_limit)
  regular_benefit = if_else(tax_unit$st_credits.cdctc_refundable == 1,
                            st_cdctc,
                            pmin(st_cdctc, pmax(0, tax_unit$st_tax_pre_credit)))
  st_cdctc_lowinc = tax_unit$st_credits.cdctc_lowinc_rate > 0 &
                    tax_unit$agi <= lowinc_limit &
                    lowinc_amt > regular_benefit
  st_cdctc = if_else(st_cdctc_lowinc, lowinc_amt, st_cdctc)

  #------------------------------------------------------------------------
  # Care credit (MA TY2021-22): the credit-side twin of the st_ded.R care
  # deduction block. Dependent Care Tax Credit = employment-related expenses
  # capped at per_dep_cap per qualifying individual (at most dep_limit) and
  # by the lesser earner's earned income (Form 2441 lines 4-5); Household
  # Dependent Tax Credit = hh_member_amt per member under the age limit, or
  # aged >= hh_aged_min, or disabled, at most hh_limit. The two are
  # exclusive per return ("you may claim an amount in line 46 only if there
  # is no entry in line 45"), so the unit takes the larger. Disabled
  # qualifying individuals come from the dep_disabled_n optional data
  # column (zero until published, S43); 65+ dependents from the age slots
  #------------------------------------------------------------------------
  dep_age = cbind(tax_unit$dep_age1, tax_unit$dep_age2, tax_unit$dep_age3)
  care_credit_n_qual = pmin(
    rowSums(dep_age <= tax_unit$st_credits.care_credit_age_limit, na.rm = TRUE) +
      tax_unit$dep_disabled_n,
    tax_unit$st_credits.care_credit_dep_limit)
  care_credit_ei_limit = pmax(0, if_else(tax_unit$filing_status == 2,
                                         pmin(tax_unit$ei1, tax_unit$ei2),
                                         tax_unit$ei1))
  care_credit_hh_n = pmin(
    rowSums(dep_age <= tax_unit$st_credits.care_credit_hh_member_age_limit |
            dep_age >= tax_unit$st_credits.care_credit_hh_aged_min, na.rm = TRUE) +
      tax_unit$dep_disabled_n,
    tax_unit$st_credits.care_credit_hh_limit)
  st_care_credit = tax_unit$st_credits.care_credit * (tax_unit$dep_status != 1) *
    pmax(pmin(tax_unit$care_exp,
              care_credit_n_qual * tax_unit$st_credits.care_credit_per_dep_cap,
              care_credit_ei_limit),
         care_credit_hh_n * tax_unit$st_credits.care_credit_hh_member_amt) *
    (1 - tax_unit$st_credits.care_credit_mfs_ineligible * (tax_unit$filing_status == 3))

  #------------------------------------------------------------------------
  # Poverty-banded care credit (OR Working Family Household and Dependent
  # Care credit, ORS 315.264; Schedule OR-WFHDC and Publication
  # OR-WFHDC-TB). A percentage of qualified expenses -- capped at
  # care_fpg_exp_cap1 for one qualifying child, cap2 for two or more, and at
  # the lesser earner's earned income -- read from the state's care_fpg_rate
  # table. The table's income key is the band ceiling(10 x income / poverty
  # guideline) for the household size (poverty_guideline table; taxpayers
  # plus dependents, 8 or more uses 8); its key concept is the column for
  # the youngest qualifying child's age on January 1 of the tax year
  # (year-end age less one). Disabled qualifying individuals (the table's
  # columns 4-5) are not observed and not modeled. Shares the federal
  # take-up draw like the other own-computation care credits
  #------------------------------------------------------------------------
  # (the per-unit age limit recycles down each column of the age matrix)
  fpg_age_obs  = dep_age
  fpg_age_obs[is.na(dep_age) |
                dep_age > tax_unit$st_credits.care_fpg_child_age_limit] = Inf
  fpg_n_qual   = rowSums(is.finite(fpg_age_obs))
  fpg_jan1_age = pmax(0, pmin(fpg_age_obs[, 1], fpg_age_obs[, 2], fpg_age_obs[, 3]) - 1)
  fpg_column   = case_when(
    fpg_jan1_age < tax_unit$st_credits.care_fpg_age_cut1 ~ 1L,
    fpg_jan1_age < tax_unit$st_credits.care_fpg_age_cut2 ~ 2L,
    TRUE ~ 3L)
  fpg_hh_size  = 1L + (tax_unit$filing_status == 2) + as.integer(tax_unit$n_dep)
  fpg_guideline = lookup_state_credit_table(rep(0, nrow(tax_unit)), fpg_hh_size,
                                            credit_tables, 'poverty_guideline')
  fpg_income   = if_else(tax_unit$st_credits.care_fpg_income_max_state == 1,
                         pmax(tax_unit$agi, tax_unit$st_agi), tax_unit$agi)
  fpg_band     = if_else(fpg_guideline > 0,
                         ceiling(10 * pmax(0, fpg_income) / fpg_guideline), Inf)
  fpg_rate     = lookup_state_credit_table(fpg_band, fpg_column, credit_tables,
                                           'care_fpg_rate')
  fpg_exp_cap  = if_else(fpg_n_qual >= 2, tax_unit$st_credits.care_fpg_exp_cap2,
                         tax_unit$st_credits.care_fpg_exp_cap1)
  st_care_fpg  = (tax_unit$st_credits.care_fpg_style == 1) *
    (tax_unit$dep_status != 1) * (fpg_n_qual > 0) * fpg_rate *
    pmin(tax_unit$care_exp, fpg_exp_cap, care_credit_ei_limit) *
    tax_unit$cdctc_takeup
  st_care_credit = st_care_credit + st_care_fpg

  list(st_cdctc = st_cdctc, st_cdctc_lowinc = st_cdctc_lowinc,
       st_care_credit = st_care_credit)
}
