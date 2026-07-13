#----------------------------------------------------
# Function to calculate state AGI (state income base)
#----------------------------------------------------

# Set return variables for function
return_vars$calc_st_agi = c('st_additions', 'st_subtractions', 'st_retirement_excl',
                            'st_agi')


calc_st_agi = function(tax_unit, fill_missings = F) {

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
    'txbl_pens_dist', # (dbl)  taxable pension distributions
    'txbl_ira_dist',  # (dbl)  taxable IRA distributions
    'wages1',         # (dbl)  primary filer wages
    'wages2',         # (dbl)  secondary filer wages
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
    'st_agi.add_overtime_ded'       # (int) whether the federal OT deduction is added back
  )

  # Assumed share of tax-exempt interest from own-state bonds for states that
  # exempt them (unobserved in the PUF; known-difference)
  OWN_STATE_MUNI_SHARE = 0.75

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
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
                       agi <= st_agi.ss_5564_agi_limit),
      ss_full_share = pmax(st_agi.ss_sub_share, as.integer(ss_age_full)),
      st_sub_ss_full = txbl_ss * ss_full_share,

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

      # Subtraction: charitable contributions for federal non-itemizers in
      # excess of the floor (CO)
      st_sub_char = if_else(itemizing != 1 & is.finite(st_agi.sub_char_nonitem_floor),
                            pmax(0, char_cash + char_noncash -
                                    st_agi.sub_char_nonitem_floor),
                            0),

      st_subtractions = st_sub_ref + st_sub_ss + st_sub_pens + st_sub_char +
                        st_retirement_excl,

      # State income base
      st_agi = st_start + st_additions - st_subtractions
    ) %>%
    select(all_of(return_vars$calc_st_agi)) %>%
    return()
}
