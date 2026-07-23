#---------------------------------------------------------------------------
# Earned-income / low-income credit family (called by calc_st_credits):
# federal-EITC matches (with the NY minus-household-credit mechanic and the
# VA nonrefundable-vs-refundable option choice), independent earned-income
# credits (CalEITC-style curves and dense tables), refundable young-child
# credits, the VA CLI, the VA age-package exclusivity gate, and the
# poverty-based forgiveness credit (PA Schedule SP).
#---------------------------------------------------------------------------

# Law parameters this family reads (assembled into calc_st_credits req_vars)
st_credits_earned_req_vars = c(
  'st_credits.eitc_match',
  'st_credits.eitc_refundable',
  'st_credits.eitc_match_alt',
  'st_credits.eitc_refundable_alt',
  'st_credits.eitc_less_household_credit',
  'st_credits.eitc_wage_cap',
  'st_credits.eitc_child_bonus',
  'st_credits.cli_amount',
  'st_credits.cli_poverty_addl',
  'st_credits.eitc_cli_excl_age_package',
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
  'st_credits.young_child_credit_zero_income_agi_limit',
  'st_credits.forgive_style',
  'st_credits.forgive_base',
  'st_credits.forgive_dep_amount',
  'st_credits.forgive_step',
  'st_credits.forgive_step_share',
  'st_credits.forgive_income_base',
  'st_credits.forgive_add_exempt_int',
  'st_credits.forgive_add_alimony'
)


st_credits_earned = function(tax_unit, st_hh_credit, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # Calculates the earned-income credit family on a parsed tax unit tibble.
  # The household credit feeds the NY EITC offset; the exclusive CLI/EITC
  # choice and the age-package gate resolve here.
  #
  # Parameters:
  #   - tax_unit (df)        : parsed tax unit tibble (see calc_st_credits)
  #   - st_hh_credit (dbl[]) : household credit (st_credits_household)
  #   - credit_tables (df)   : dense schedules (see build_state_credit_tables)
  #
  # Returns: list of per-row vectors --
  #   - st_earned_credit (dbl)  : independent earned-income credit
  #   - st_yctc (dbl)           : refundable young-child credit
  #   - st_eitc (dbl)           : state EITC (chosen option)
  #   - st_eitc_ref_share (dbl) : refundability of the chosen option (0/1)
  #   - st_cli (dbl)            : credit for low-income individuals (VA)
  #   - st_forgive_credit (dbl) : poverty-based forgiveness credit (PA)
  #----------------------------------------------------------------------------

  n   = nrow(tax_unit)
  agi = tax_unit$agi

  # Generic independent earned-income credit. Style 1 uses the existing
  # triangular schedule; style 2 uses a dense row-based table for schedules
  # such as CalEITC.
  pick_earned_param = function(prefix) {
    values = st_family_matrix(tax_unit, paste0('st_credits.', prefix), 1:4,
                              require_sentinel = FALSE)
    if (is.null(values)) {
      return(rep(0, n))
    }
    st_pick_slot(values, pmin(4L, 1L + tax_unit$n_dep_eitc))
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
  # MFS filers are barred unless the state opts them in (earned_credit_mfs_
  # eligible == 1), mirroring the federal EITC's MFS treatment.
  earned_credit_eligible = tax_unit$dep_status != 1 &
    (tax_unit$filing_status != 3 |
       tax_unit$st_credits.earned_credit_mfs_eligible == 1) &
    earned_income > 0 &
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

  # Refundable young-child credits can depend on an independent earned-income
  # credit. The zero-income path uses the available current-year loss proxy;
  # state data contracts can supply a more complete loss measure later.
  n_young_child = st_n_dep_in(tax_unit, 0,
                              tax_unit$st_credits.young_child_credit_max_age)
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
  # FTB conditions the YCTC on CalEITC ELIGIBILITY, not on a strictly positive
  # computed CalEITC amount, so a filer at the very top of the phase-out band
  # (credit rounds to zero) with a young child still qualifies.
  st_yctc = if_else(
    tax_unit$st_credits.young_child_credit_style == 1 & n_young_child > 0 &
      (earned_credit_eligible | yctc_zero_income_eligible),
    yctc_amount, 0
  )

  #--------------------------------------------------------
  # CLI poverty guideline (VA): dense table keyed by family size
  # (credit_tables id cli_poverty_guideline; HHS guidelines publish sizes
  # 1-8 plus a per-additional-person increment, which stays in YAML as
  # cli_poverty_addl). A state without the table gets -Inf (ineligible)
  #--------------------------------------------------------

  cli_fam = 1 + (tax_unit$filing_status == 2) + tax_unit$n_dep
  cli_base = lookup_state_credit_table(rep(0, n), cli_fam, credit_tables,
                                       'cli_poverty_guideline')
  cli_guideline = if_else(
    cli_base > 0,
    cli_base + pmax(0, cli_fam - 8) * tax_unit$st_credits.cli_poverty_addl,
    -Inf
  )

  # State EITC: match on the federal credit, less the household credit
  # (capped at remaining tax) where flagged (NY IT-215 lines 13-16),
  # plus a flat per-return bonus for filers with a federal qualifying
  # child (CT Schedule CT-EITC line 15a, 2025+), capped at W-2 wages
  # where flagged (UT 59-10-1044 2023+ "earn income in Utah reported on
  # a W-2"; total wages proxy Utah-source wages -- known-difference)
  st_eitc_main = pmax(0, tax_unit$st_credits.eitc_match * tax_unit$eitc -
                         tax_unit$st_credits.eitc_less_household_credit *
                         pmin(st_hh_credit, pmax(0, tax_unit$st_tax_pre_credit))) +
                 tax_unit$st_credits.eitc_child_bonus *
                   (tax_unit$eitc > 0 & tax_unit$n_dep_eitc > 0)
  st_eitc_main = if_else(
    tax_unit$st_credits.eitc_wage_cap == 1,
    pmin(st_eitc_main, pmax(0, tax_unit$wages1 + tax_unit$wages2)),
    st_eitc_main
  )

  # Alternative state EITC option (VA: taxpayer claims the greater of a
  # nonrefundable match or, 2022+, a lower refundable match). Realized
  # benefit of a nonrefundable credit is capped at pre-credit tax; the
  # unit takes whichever option yields the larger benefit, keeping the
  # main option on ties. Per-unit refundability follows the chosen option
  st_eitc_alt_amt = tax_unit$st_credits.eitc_match_alt * tax_unit$eitc
  st_eitc_benefit_main = if_else(tax_unit$st_credits.eitc_refundable == 1,
                                 st_eitc_main,
                                 pmin(st_eitc_main,
                                      pmax(0, tax_unit$st_tax_pre_credit)))
  st_eitc_benefit_alt  = if_else(tax_unit$st_credits.eitc_refundable_alt == 1,
                                 st_eitc_alt_amt,
                                 pmin(st_eitc_alt_amt,
                                      pmax(0, tax_unit$st_tax_pre_credit)))
  st_eitc_use_alt = tax_unit$st_credits.eitc_match_alt > 0 &
                    st_eitc_benefit_alt > st_eitc_benefit_main
  st_eitc = if_else(st_eitc_use_alt, st_eitc_alt_amt, st_eitc_main)
  st_eitc_ref_share = if_else(st_eitc_use_alt,
                              tax_unit$st_credits.eitc_refundable_alt,
                              tax_unit$st_credits.eitc_refundable)

  # Credit for low-income individuals (VA Schedule ADJ Lines 10-17): a
  # flat amount per personal + dependent exemption (65+/blind add-ons
  # excluded) for families with VAGI at or below the poverty guideline
  # for their family size. Nonrefundable, and exclusive with the state
  # EITC options -- the household claims whichever benefit is larger.
  # Dependent income in family VAGI is unobserved (known-difference);
  # dependent filers are ineligible
  st_cli_amt = if_else(
    tax_unit$st_credits.cli_amount > 0 & tax_unit$dep_status != 1 &
      tax_unit$st_agi <= cli_guideline,
    tax_unit$st_credits.cli_amount *
      (1 + (tax_unit$filing_status == 2) + tax_unit$n_dep),
    0
  )
  st_cli_benefit = pmin(st_cli_amt, pmax(0, tax_unit$st_tax_pre_credit))
  st_eitc_benefit_chosen = if_else(st_eitc_use_alt, st_eitc_benefit_alt,
                                   st_eitc_benefit_main)
  st_cli  = if_else(st_cli_benefit > st_eitc_benefit_chosen, st_cli_amt, 0)
  st_eitc = if_else(st_cli > 0, 0, st_eitc)

  # Age-package exclusivity (VA): a household claiming the aged
  # deduction or aged/blind exemption add-ons may claim neither the CLI
  # nor the state EITC (choice made in calc_st_agi)
  st_age_excl = tax_unit$st_credits.eitc_cli_excl_age_package == 1 &
                tax_unit$st_age_package_taken == 1
  st_cli  = st_cli * !st_age_excl
  st_eitc = st_eitc * !st_age_excl

  # Poverty-based forgiveness credit (PA Schedule SP): a share of
  # pre-credit tax, 100% at or below the family-size eligibility-income
  # limit (per-return base, filing-status mapped, plus a per-dependent
  # amount), dropping forgive_step_share for each forgive_step -- or
  # fraction thereof -- above it (PA: 10pp per $250). Eligibility income
  # is the enum base plus configured shares of tax-exempt interest and
  # alimony received -- the observable slice of the form's nontaxable
  # additions; gifts, support received, and inheritances are unobserved,
  # and MFS units use own rather than combined-spouse income (documented
  # known-differences).
  # Dependent filers are ineligible (a dependent's forgiveness follows the
  # parents' eligibility, unobservable across units). Nonrefundable
  forgive_income = st_income_base(
    tax_unit, tax_unit$st_credits.forgive_income_base
  ) + tax_unit$st_credits.forgive_add_exempt_int * tax_unit$exempt_int +
      tax_unit$st_credits.forgive_add_alimony * tax_unit$alimony
  forgive_limit = tax_unit$st_credits.forgive_base +
                  tax_unit$st_credits.forgive_dep_amount * tax_unit$n_dep
  forgive_share = pmax(
    0, 1 - st_step_reduction(forgive_income, forgive_limit,
                             tax_unit$st_credits.forgive_step,
                             tax_unit$st_credits.forgive_step_share)
  )
  st_forgive_credit = if_else(
    tax_unit$st_credits.forgive_style == 1 & tax_unit$dep_status != 1,
    forgive_share * pmax(0, tax_unit$st_tax_pre_credit),
    0
  )

  list(
    st_earned_credit  = st_earned_credit,
    st_yctc           = st_yctc,
    st_eitc           = st_eitc,
    st_eitc_ref_share = st_eitc_ref_share,
    st_cli            = st_cli,
    st_forgive_credit = st_forgive_credit
  )
}
