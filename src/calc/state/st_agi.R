#----------------------------------------------------
# Function to calculate state AGI (state income base)
#----------------------------------------------------

# Set return variables for function
return_vars$calc_st_agi = c('st_additions', 'st_subtractions', 'st_retirement_excl',
                            'st_agi', 'st_age_package_taken',
                            'st_age_package_forgone')


calc_st_agi = function(tax_unit, fill_missings = F, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # Calculates the state income base: federal starting point (AGI or taxable
  # income per st_agi.start_point) plus state additions minus state
  # subtractions.
  #
  # Documented v1 approximations (plan known-differences):
  #  - own-state share of tax-exempt interest is unobserved; states exempting
  #    own-state bonds add back (1 - OWN_STATE_MUNI_SHARE) of exempt_int
  #  - US-obligation share of taxable interest is unobserved; sub_us_int is
  #    carried as a flag but no subtraction is taken (share unknown)
  #  - SS/pension age tests use unit-level approximation: primary age for SS
  #    flags, per-spouse caps summed for the pension exclusion
  #  - government vs private pension split unobserved; all pensions treated
  #    as private (understates NY subtraction)
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_additions (dbl)    : total state additions to the federal base
  #   - st_subtractions (dbl) : total state subtractions
  #   - st_agi (dbl)          : state income base after modifications
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',            # (dbl)  federal Adjusted Gross Income (post-federal calc)
    'txbl_inc',       # (dbl)  federal taxable income (post-federal calc)
    'exempt_int',     # (dbl)  tax-exempt interest income
    'state_ref',      # (dbl)  taxable refunds of state/local taxes
    'txbl_ss',        # (dbl)  taxable Social Security benefits (federal)
    'gross_ss',       # (dbl)  gross Social Security benefits received
    'txbl_pens_dist', # (dbl)  taxable pension distributions
    'txbl_ira_dist',  # (dbl)  taxable IRA distributions
    'wages1',         # (dbl)  primary filer wages
    'wages2',         # (dbl)  secondary filer wages
    'ui',             # (dbl)  unemployment benefits (in federal AGI)
    'eitc',           # (dbl)  federal EITC (post-federal calc)
    'blind1',         # (bool) whether primary filer is blind
    'blind2',         # (bool) whether secondary filer is blind
    'sole_prop',      # (dbl)  sole proprietorship income or loss
    'part_active',    # (dbl)  active partnership income or loss
    'scorp',          # (dbl)  S-corporation income or loss
    'farm',           # (dbl)  farm income or loss
    'txbl_int',       # (dbl)  taxable interest income
    'div_ord',        # (dbl)  ordinary dividends
    'div_pref',       # (dbl)  qualified dividends
    'kg_lt',          # (dbl)  long-term capital gains
    'kg_st',          # (dbl)  short-term capital gains
    'rent',           # (dbl)  rental income or loss
    'part_passive',   # (dbl)  passive partnership income or loss
    'other_inc',      # (dbl)  other taxable income
    'ot_ded',         # (dbl)  federal overtime deduction (post-federal calc)
    'char_cash',      # (dbl)  cash charitable contributions
    'char_noncash',   # (dbl)  non-cash charitable contributions
    'itemizing',      # (bool) whether unit itemizes on the federal return
    'age1',           # (int)  age of primary filer
    'age2',           # (int)  age of secondary filer (NA if none)
    'filing_status',  # (int)  filing status (1 single, 2 MFJ, 3 MFS, 4 HoH)

    # State tax law
    'st_agi.start_point',           # (int) 0 own base, 1 fed AGI, 2 fed taxable income
    'st_agi.add_exempt_int',        # (int) whether exempt interest is added back
    'st_agi.own_state_exempt',      # (int) whether own-state bonds stay exempt
    'st_agi.sub_state_ref',         # (int) whether state refunds are subtracted
    'st_agi.ss_sub_share',          # (dbl) share of taxable SS subtracted (flat)
    'st_agi.ss_full_sub_65plus',    # (int) full SS subtraction at 65+ (CO-style)
    'st_agi.ss_full_sub_5564',      # (int) full SS subtraction at 55-64 under AGI limit
    'st_agi.ss_5564_agi_limit',     # (dbl) AGI limit for the 55-64 SS subtraction
    'st_agi.ss_full_sub_allages',   # (int) full SS subtraction at any age under AGI limit
    'st_agi.ss_allages_agi_limit',  # (dbl) AGI limit for the all-ages SS subtraction
    'st_agi.ss_taxable_gross_cap_share', # (dbl) cap on taxable SS as share of gross (CT 0.25)
    'st_agi.pension_sub_share',     # (dbl) share of pension income subtracted (CT-style)
    'st_agi.ira_sub_share',         # (dbl) share of IRA distributions subtracted (CT-style)
    'st_agi.pension_excl_under65',  # (dbl) per-person pension exclusion cap, under 65
    'st_agi.pension_excl_65plus',   # (dbl) per-person pension exclusion cap, 65+
    'st_agi.pension_excl_min_age',  # (dbl) minimum age for the pension exclusion
    'st_agi.pension_cap_incl_ss',   # (int) whether taxable SS counts within the cap
    'st_agi.retirement_excl_style', # (int) 1 = per-person earned/unearned exclusion
    'st_agi.retirement_excl_min_age', # (dbl) minimum age for broad exclusion
    'st_agi.retirement_excl_under65', # (dbl) per-person cap below 65
    'st_agi.retirement_excl_65plus',  # (dbl) per-person cap at 65+
    'st_agi.retirement_excl_earned_cap', # (dbl) per-person portion usable for earned income
    'st_agi.sub_char_nonitem_floor', # (dbl) floor for non-itemizer charitable sub
    'st_agi.add_overtime_ded',      # (int) whether the federal OT deduction is added back
    'st_agi.cap_gains_excl_share',  # (dbl) share of net LT capital gain excluded
    'st_agi.div_excl_share',        # (dbl) share of qualified dividends excluded
    'st_agi.age_ded_amount',        # (dbl) per-person aged deduction (VA-style)
    'st_agi.age_ded_min_age',       # (dbl) minimum age for the aged deduction
    'st_agi.age_ded_no_test_min_age', # (dbl) age at/above which no income test applies
    'st_agi.age_ded_po_thresh',     # (dbl) income threshold for $1-per-$1 reduction
    'st_agi.age_ded_po_base',       # (int) reduction income base (st_income_base enum)
    'st_agi.retire_sub_factor_income_base', # (int) factor-table income base (enum)
    'st_agi.age_excl_eitc',         # (int) age package and EITC/CLI mutually exclusive (VA)
    'st_agi.sub_ui_share',          # (dbl) share of unemployment benefits subtracted
    'st_exempt.aged_addl',          # (dbl) aged exemption add-on (exclusivity choice)
    'st_exempt.blind_addl',         # (dbl) blind exemption add-on (exclusivity choice)
    'st_credits.eitc_match',        # (dbl) state EITC match (exclusivity choice)
    'st_credits.eitc_match_alt'     # (dbl) alternative EITC match (exclusivity choice)
  )

  # Assumed share of tax-exempt interest from own-state bonds for states that
  # exempt them (unobserved in the PUF; known-difference)
  OWN_STATE_MUNI_SHARE = 0.75

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Top marginal schedule rate, used only to approximate the value of the
  # aged deduction + aged/blind exemption package when a state makes it
  # mutually exclusive with the EITC/CLI (VA; documented approximation)
  rate_cols = str_subset(colnames(tax_unit), '^st_ord\\.rates[0-9]*$')
  st_top_rate = if (length(rate_cols) > 0) {
    reduce(map(rate_cols, ~ coalesce(tax_unit[[.x]], 0)), pmax)
  } else {
    rep(0, nrow(tax_unit))
  }

  # CT-style share-based pension/IRA subtraction factor: a filing-status-
  # keyed dense table (credit_tables id retirement_subtraction_factor)
  # covering both the 2017-2023 eligibility cliff and the 2024+ published
  # phase-out table, with income rounded to whole dollars per the worksheet
  # before the lookup. Defaults to 1 where a state carries no table (a
  # zero factor inside the table is real; absence is not)
  retire_factor = rep(1, nrow(tax_unit))
  if (!is.null(credit_tables) &&
      any(credit_tables$credit_id == 'retirement_subtraction_factor')) {
    rf_income = st_income_base(
      tax_unit, tax_unit$st_agi.retire_sub_factor_income_base
    )
    retire_factor = lookup_state_credit_table(
      floor(rf_income + 0.5), rep(0L, nrow(tax_unit)), credit_tables,
      'retirement_subtraction_factor', filing_status = tax_unit$filing_status
    )
  }

  # Age-deduction reduction income base per the uniform enum (VA: 5 = AFAGI,
  # federal AGI less taxable Social Security)
  st_age_po_income_v = st_income_base(tax_unit,
                                      tax_unit$st_agi.age_ded_po_base)

  tax_unit %>%
    mutate(

      # Starting point
      st_start = if_else(st_agi.start_point == 2, txbl_inc, agi),

      # Additions: tax-exempt interest (own-state carve-out approximated) and
      # the federal overtime deduction where added back (CO 2026+; only
      # applies to a taxable-income start, where the deduction reduced it)
      st_add_muni = st_agi.add_exempt_int * exempt_int *
                    if_else(st_agi.own_state_exempt == 1, 1 - OWN_STATE_MUNI_SHARE, 1),
      st_add_ot   = st_agi.add_overtime_ded * ot_ded,
      st_additions = st_add_muni + st_add_ot,

      # Subtraction: state refunds included in the federal base
      st_sub_ref = st_agi.sub_state_ref * state_ref,

      # Subtraction: Social Security. Full-subtraction share is the greater of
      # the flat share (IL/NY = 1) and the CO-style age-conditional full
      # subtraction; primary age proxies the unit
      ss_age_full   = (st_agi.ss_full_sub_65plus == 1 & age1 >= 65) |
                      (st_agi.ss_full_sub_5564 == 1 & age1 >= 55 & age1 < 65 &
                       agi <= st_agi.ss_5564_agi_limit) |
                      (st_agi.ss_full_sub_allages == 1 &
                       agi <= st_agi.ss_allages_agi_limit),
      ss_full_share = pmax(st_agi.ss_sub_share, as.integer(ss_age_full)),

      # CT-style cap on taxable SS as a share of gross benefits: above the
      # full-subtraction AGI limit, the CT-1040 SS Benefit Adjustment
      # Worksheet taxes at most 25% of benefits, i.e. subtracts
      # max(0, taxable SS - 0.25 x gross). The worksheet's min(gross, excess
      # over federal base) is approximated by gross: above the CT AGI limits
      # the federal excess-over-base is essentially always larger than gross
      # benefits (documented known-difference)
      ss_cap_extra = if_else(
        is.finite(st_agi.ss_taxable_gross_cap_share),
        pmax(0, txbl_ss - st_agi.ss_taxable_gross_cap_share * gross_ss),
        0
      ),
      st_sub_ss_full = pmax(txbl_ss * ss_full_share, ss_cap_extra),

      # Subtraction: pension/IRA exclusion. Per-person caps summed across
      # qualifying spouses. Where SS shares the cap (CO): fully-subtracted SS
      # reduces the cap dollar-for-dollar; otherwise SS claims cap room first
      pens_inc  = txbl_pens_dist + txbl_ira_dist,
      cap1      = if_else(age1 >= st_agi.pension_excl_min_age,
                          if_else(age1 >= 65, st_agi.pension_excl_65plus,
                                              st_agi.pension_excl_under65), 0),
      cap2      = if_else(filing_status == 2 & !is.na(age2) &
                          age2 >= st_agi.pension_excl_min_age,
                          if_else(age2 >= 65, st_agi.pension_excl_65plus,
                                              st_agi.pension_excl_under65), 0),
      pens_cap  = case_when(
        st_agi.pension_cap_incl_ss == 0 ~ cap1 + cap2,
        ss_full_share >= 1              ~ pmax(0, cap1 + cap2 - txbl_ss),
        TRUE                            ~ cap1 + cap2
      ),
      st_sub_ss_cap = if_else(st_agi.pension_cap_incl_ss == 1 & ss_full_share < 1,
                              pmin(txbl_ss * (1 - ss_full_share), pens_cap),
                              0),
      st_sub_pens = pmin(pens_inc, pmax(0, pens_cap - st_sub_ss_cap)),
      st_sub_ss   = st_sub_ss_full + st_sub_ss_cap,

      # CT-style share-based pension/annuity and IRA subtraction: statutory
      # phase-in shares times the AGI-banded factor (the 2019-2023 cliff and
      # the 2024+ published phase-out table; retire_factor computed above).
      # Military/railroad/teacher pensions have separate CT subtractions but
      # are unobservable subsets of pension income (known-difference)
      st_sub_retire_share = retire_factor *
        (st_agi.pension_sub_share * pmax(0, txbl_pens_dist) +
         st_agi.ira_sub_share     * pmax(0, txbl_ira_dist)),

      # Broad retirement exclusion (GA-style): each eligible spouse may use a
      # limited amount against own earned income first and then against
      # retirement-type unearned income. Jointly held non-wage income is split
      # equally because ownership is not observed in the PUF.
      st_retir_n = 1 + (filing_status == 2),
      st_retir_other_earned = sole_prop + part_active + scorp + farm,
      st_retir_unearned = txbl_int + div_ord + div_pref + kg_lt + kg_st +
                          rent + part_passive + txbl_pens_dist + txbl_ira_dist +
                          other_inc,
      st_retir_cap1 = if_else(age1 >= st_agi.retirement_excl_min_age,
                               if_else(age1 >= 65,
                                       st_agi.retirement_excl_65plus,
                                       st_agi.retirement_excl_under65), 0),
      st_retir_cap2 = if_else(filing_status == 2 & !is.na(age2) &
                               age2 >= st_agi.retirement_excl_min_age,
                               if_else(age2 >= 65,
                                       st_agi.retirement_excl_65plus,
                                       st_agi.retirement_excl_under65), 0),
      st_retir_earned1 = pmax(0, wages1 + st_retir_other_earned / st_retir_n),
      st_retir_earned2 = pmax(0, wages2 + st_retir_other_earned / st_retir_n),
      st_retir_earned_take1 = pmin(st_retir_cap1,
                                   pmin(st_agi.retirement_excl_earned_cap,
                                        st_retir_earned1)),
      st_retir_earned_take2 = pmin(st_retir_cap2,
                                   pmin(st_agi.retirement_excl_earned_cap,
                                        st_retir_earned2)),
      st_retir_unearned_each = pmax(0, st_retir_unearned / st_retir_n),
      st_retirement_excl = if_else(
        st_agi.retirement_excl_style == 1,
        st_retir_earned_take1 + st_retir_earned_take2 +
          pmin(pmax(0, st_retir_cap1 - st_retir_earned_take1),
               st_retir_unearned_each) +
          pmin(pmax(0, st_retir_cap2 - st_retir_earned_take2),
               st_retir_unearned_each),
        0
      ),

      # Subtraction: flat per-person aged deduction with dollar-for-dollar
      # income-based reduction (VA-style). Each spouse at/above the minimum
      # age contributes the per-person amount; persons at/above the no-test
      # age (VA: born on or before 1/1/1939, encoded as a year-keyed age)
      # keep the full amount regardless of income. The remainder is reduced
      # $1 for each $1 the phase-out income base (federal AGI, or federal AGI
      # less taxable Social Security) exceeds the filing-status threshold.
      # MFS combined-spouse income is unobserved (own income used;
      # known-difference)
      st_age_q1  = age1 >= st_agi.age_ded_min_age,
      st_age_q2  = filing_status == 2 & !is.na(age2) &
                   age2 >= st_agi.age_ded_min_age,
      st_age_gf1 = st_age_q1 & age1 >= st_agi.age_ded_no_test_min_age,
      st_age_gf2 = st_age_q2 & age2 >= st_agi.age_ded_no_test_min_age,
      st_age_po_income = st_age_po_income_v,
      st_sub_age_pot = st_agi.age_ded_amount * (st_age_gf1 + st_age_gf2) +
                       pmax(0, st_agi.age_ded_amount *
                               ((st_age_q1 & !st_age_gf1) +
                                (st_age_q2 & !st_age_gf2)) -
                               pmax(0, st_age_po_income -
                                       st_agi.age_ded_po_thresh)),

      # Age-package vs EITC/CLI mutual exclusivity (VA Form 760 Line 4 /
      # Schedule ADJ Line 17 rules): a return claiming the aged deduction or
      # any aged/blind exemption add-on may not claim the CLI or state EITC,
      # household-wide. The unit takes whichever side is worth more,
      # approximating the package's value at the top schedule rate and the
      # EITC side at the best available match rate (documented
      # approximation; both uncapped by liability here). Downstream:
      # st_exempt zeroes the add-ons when forgone; st_credits zeroes the
      # EITC/CLI when taken
      st_age_addl_pot = st_exempt.aged_addl *
                          ((age1 >= 65) +
                           (filing_status == 2 & !is.na(age2) & age2 >= 65)) +
                        st_exempt.blind_addl *
                          (coalesce(blind1, 0) + (!is.na(blind2) & blind2)),
      st_age_package_exists = (st_sub_age_pot + st_age_addl_pot) > 0,
      st_age_package_forgone = as.integer(
        st_agi.age_excl_eitc == 1 & st_age_package_exists & eitc > 0 &
          pmax(st_credits.eitc_match, st_credits.eitc_match_alt) * eitc >
            st_top_rate * (st_sub_age_pot + st_age_addl_pot)
      ),
      st_age_package_taken = as.integer(
        st_agi.age_excl_eitc == 1 & st_age_package_exists &
          st_age_package_forgone == 0
      ),
      st_sub_age = st_sub_age_pot * (1 - st_age_package_forgone),

      # Subtraction: unemployment benefits included in the federal base (VA)
      st_sub_ui = st_agi.sub_ui_share * pmax(0, ui),

      # Subtraction: charitable contributions for federal non-itemizers in
      # excess of the floor (CO)
      st_sub_char = if_else(itemizing != 1 & is.finite(st_agi.sub_char_nonitem_floor),
                            pmax(0, char_cash + char_noncash -
                                    st_agi.sub_char_nonitem_floor),
                            0),

      # Subtraction: partial exclusion of net long-term capital gain (ND 40%,
      # SC 44%) and qualified dividends (ND 40%). The gain base is net LT gain
      # in excess of net ST loss (the smaller of net LT gain and net LT gain
      # less net ST loss), matching the ND/SC worksheets.
      st_cap_gain_base = pmax(0, kg_lt + pmin(0, kg_st)),
      st_sub_capgain = st_agi.cap_gains_excl_share * st_cap_gain_base +
                       st_agi.div_excl_share * pmax(0, div_pref),

      st_subtractions = st_sub_ref + st_sub_ss + st_sub_pens + st_sub_char +
                        st_retirement_excl + st_sub_capgain +
                        st_sub_retire_share + st_sub_age + st_sub_ui,

      # State income base
      st_agi = st_start + st_additions - st_subtractions
    ) %>%
    select(all_of(return_vars$calc_st_agi)) %>%
    return()
}
