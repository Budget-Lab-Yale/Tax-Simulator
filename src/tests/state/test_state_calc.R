#---------------------------------------------------------------
# Form-worksheet unit tests for the state calculator (Phase 3)
#
# Each case is a hand-computed return from the state's published
# forms/worksheets (arithmetic documented inline), run through
# do_state_taxes() on a synthetic post-federal tax unit joined to
# the parsed baseline state law.
#
# Defines functions only (sourced by main.R's recursive walk).
# Run manually after sourcing ./src (see test_state_tax_law.R).
#---------------------------------------------------------------


test_state_calc = function() {

  #----------------------------------------------------------------------------
  # Runs all state calculator form-worksheet tests.
  #
  # Returns: TRUE invisibly if all tests pass (throws otherwise).
  #----------------------------------------------------------------------------

  law = build_state_tax_law(
    states  = c('IL', 'CO', 'NY', 'NH', 'TN', 'WA', 'AZ', 'GA', 'NC',
                'IN', 'KY', 'MI', 'CA'),
    years   = 2017:2035,
    indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
              mutate(growth = 0.025)
  )
  credit_tables = attr(law, 'credit_tables')

  run_case = function(st, yr, unit_overrides, expect, tol = 0.01, label = '') {

    unit = st_test_unit(unit_overrides)
    law_row = law %>%
      filter(state == st, year == yr,
             filing_status == unit$filing_status) %>%
      select(-state, -year, -filing_status)
    stopifnot('law row missing' = nrow(law_row) == 1)

    result = unit %>%
      bind_cols(law_row) %>%
      do_state_taxes(
        credit_tables = state_credit_tables_for_year(credit_tables, st, yr)
      )

    for (v in names(expect)) {
      got = result[[v]][1]
      if (abs(got - expect[[v]]) > tol) {
        stop(sprintf('%s [%s %s]: %s = %.4f, expected %.4f',
                     label, st, yr, v, got, expect[[v]]))
      }
    }
    invisible(TRUE)
  }

  #--------------------------------------------------------------------------
  # Illinois (IL-1040)
  #--------------------------------------------------------------------------

  # IL-1: 2024 single, AGI 50,000. Exemption 2,775 -> net 47,225 x 4.95%
  run_case('IL', 2024, list(agi = 50000),
           expect = list(st_agi = 50000, st_exempt = 2775,
                         liab_st_iit = 47225 * 0.0495,
                         liab_st_individual_net = 47225 * 0.0495),
           label = 'IL-1 basic single')

  # IL-2: 2017 blended rate. (100,000 - 2,175) x 4.3549%
  run_case('IL', 2017, list(agi = 100000),
           expect = list(liab_st_iit = 97825 * 0.043549),
           label = 'IL-2 2017 blended rate')

  # IL-3: 2023 MFJ, AGI 100,000 incl. SS 20,000 + pensions 30,000 (both
  # fully subtracted, Line 5); 2 deps; ages 67/65. Exemptions: 4 x 2,425
  # + 2 x 1,000 aged = 11,700. Base 50,000 - 11,700 = 38,300 x 4.95% =
  # 1,895.85. Property tax credit 5% x 8,000 = 400 (nonref). IL EITC 20%
  # x 3,000 = 600 (ref). Liab = 1,895.85 - 400 - 600 = 895.85
  run_case('IL', 2023,
           list(agi = 100000, filing_status = 2, age1 = 67, age2 = 65,
                txbl_ss = 20000, txbl_pens_dist = 30000, n_dep = 2,
                dep_age1 = 8, dep_age2 = 10, salt_prop = 8000, eitc = 3000),
           expect = list(st_agi = 50000, st_exempt = 11700,
                         st_eitc = 600, liab_st_iit = 895.85),
           label = 'IL-3 retirement/exemptions/credits')

  # IL-4: 2024 single, AGI 300,000: exemption AND property credit denied
  # (cliff at 250k). 300,000 x 4.95% = 14,850
  run_case('IL', 2024,
           list(agi = 300000, salt_prop = 8000),
           expect = list(st_exempt = 0, liab_st_iit = 14850),
           label = 'IL-4 high-income cliffs')

  #--------------------------------------------------------------------------
  # New York (IT-201)
  #--------------------------------------------------------------------------

  # NY-1: 2025 MFJ, AGI 200,000, standard deduction. TI = 183,950.
  # Schedule: 686 + 290.25 + 225.75 + 5.5%x133,650 + 6%x22,400 = 9,896.75.
  # Recapture WS2: RB = 5.5%x161,550 - T(161,550) = 8,885.25 - 8,552.75 =
  # 332.50; IB = 6%x183,950 - 9,896.75 - 332.50 = 807.75; phi = 38,450 /
  # 50,000 = 0.769. Tax = 9,896.75 + 332.50 + 807.75x0.769 = 10,850.41
  run_case('NY', 2025,
           list(agi = 200000, filing_status = 2),
           expect = list(st_ded = 16050, st_txbl_inc = 183950,
                         st_tax_pre_credit = 9896.75 + 332.5 + 807.75 * 0.769),
           tol = 0.02, label = 'NY-1 recapture phase-in')

  # NY-2: 2024 MFJ, AGI 20,000, 2 deps (ages 5, 8), federal EITC 5,000.
  # TI = 20,000 - 16,050 std - 2x1,000 dep exemptions = 1,950 -> T = 78.
  # Household credit (Table 2, 7k-20k row, 4 exemptions): 60 + 15x3 = 105.
  # NYS EITC = 30%x5,000 - min(105, 78) = 1,422 (IT-215 line 15 caps the
  # household-credit reduction at remaining tax). ESCC (style 1, 2023+ ages
  # 0-16): 33% x (1,000x2) = 660 (no phase-out; > $100x2 floor).
  # Liab = max(0, 78-105) - (1,422+660) = -2,082
  run_case('NY', 2024,
           list(agi = 20000, filing_status = 2, n_dep = 2,
                dep_age1 = 5, dep_age2 = 8, eitc = 5000),
           expect = list(st_txbl_inc = 1950, st_hh_credit = 105,
                         st_eitc = 1422, st_ctc = 660,
                         liab_st_iit = -2082),
           label = 'NY-2 household credit / EITC / ESCC')

  # NY-3: 2024 single itemizer, AGI 2,000,000. Federal pre-limitation
  # itemized 150,000 (SALT capped 10,000; property 60,000; charitable
  # 100,000). NY base = 150,000 - 10,000 + 60,000 = 200,000; Pease and
  # 615(f) apply but NYAGI > 1M -> charitable-only: 50% x 100,000 = 50,000.
  # TI = 1,950,000. T = 155,604.45; recapture: NYAGI - 1,077,550 > 50,000
  # -> fully recaptured: tax = 9.65% x 1,950,000 = 188,175
  run_case('NY', 2024,
           list(agi = 2000000, itemizing = TRUE, item_ded = 150000,
                item_ded_ex_limits = 150000, salt_item_ded = 10000,
                salt_prop = 60000, salt_inc_sales = 130000,
                char_item_ded = 100000, std_ded = 14600),
           expect = list(st_item_ded = 50000, st_txbl_inc = 1950000,
                         st_tax_pre_credit = 0.0965 * 1950000,
                         liab_st_iit = 188175),
           label = 'NY-3 charitable-only limitation + full recapture')

  # NY-4: 2026 ESCC style 2: MFJ AGI 113,000, deps ages 2 and 6.
  # 1,000 + 500 = 1,500 minus 16.50 x floor(3,000/1,000) = 49.50 -> 1,450.50
  run_case('NY', 2026,
           list(agi = 113000, filing_status = 2, n_dep = 2,
                dep_age1 = 2, dep_age2 = 6),
           expect = list(st_ctc = 1500 - 16.5 * 3),
           label = 'NY-4 2026 decoupled ESCC')

  #--------------------------------------------------------------------------
  # Colorado (DR 0104)
  #--------------------------------------------------------------------------

  # CO-1: 2024 single, fed taxable income 45,400, non-itemizer with 1,000
  # cash charity: subtraction 500. (45,400 - 500) x 4.25% = 1,908.25
  run_case('CO', 2024,
           list(agi = 60000, txbl_inc = 45400, char_cash = 1000),
           expect = list(st_agi = 44900, liab_st_iit = 44900 * 0.0425),
           label = 'CO-1 charitable subtraction + 2024 TABOR rate')

  # CO-2: 2023 MFJ itemizer, AGI 500,000, fed taxable 460,000, itemized
  # 40,000 (SALT 10,000 = 3,000 income + 7,000 property), fed std 27,700.
  # Line 2 addback: min(3,000, 12,300) = 3,000. Line 4 (Prop FF): 40,000 -
  # 16,000 - 3,000 = 21,000. Base = 460,000 + 24,000 = 484,000 x 4.4% = 21,296
  run_case('CO', 2023,
           list(agi = 500000, txbl_inc = 460000, filing_status = 2,
                itemizing = TRUE, item_ded = 40000, salt_item_ded = 10000,
                salt_prop = 7000, std_ded = 27700),
           expect = list(st_addback = 24000, liab_st_iit = 484000 * 0.044),
           label = 'CO-2 SALT addback + Prop FF cap')

  # CO-3: SS/pension, single age 70, fed taxable 50,000 incl. SS 20,000 +
  # pensions 30,000.
  #  2023: full SS sub + pension min(30,000, 24,000-20,000) -> base 26,000
  #        x 4.4% = 1,144
  #  2020: SS inside the 24,000 cap: 20,000 SS + 4,000 pension -> base
  #        26,000 x 4.55% = 1,183
  run_case('CO', 2023,
           list(agi = 55000, txbl_inc = 50000, age1 = 70,
                txbl_ss = 20000, txbl_pens_dist = 30000),
           expect = list(st_agi = 26000, liab_st_iit = 26000 * 0.044),
           label = 'CO-3a post-2022 SS')
  run_case('CO', 2020,
           list(agi = 55000, txbl_inc = 50000, age1 = 70,
                txbl_ss = 20000, txbl_pens_dist = 30000),
           expect = list(st_agi = 26000, liab_st_iit = 26000 * 0.0455),
           label = 'CO-3b pre-2022 SS inside cap')

  # CO-4: 2024 MFJ, AGI 30,000, fed taxable 8,800, deps ages 3 and 7,
  # federal EITC 6,000. Tax = 8,800 x 4.25% = 374. EITC = 50% x 6,000 =
  # 3,000. CTC (style 2, tier 1 joint <= 35,000): 1,200 x 1 under-6.
  # FATC: (3,200 + 2,400) x (95,000-30,000)/(95,000-25,000) = 5,200.
  # Liab = 374 - (3,000 + 1,200 + 5,200) = -9,026
  run_case('CO', 2024,
           list(agi = 30000, txbl_inc = 8800, filing_status = 2, n_dep = 2,
                dep_age1 = 3, dep_age2 = 7, eitc = 6000),
           expect = list(st_eitc = 3000, st_ctc = 1200 + 5200,
                         liab_st_iit = 8800 * 0.0425 - 9400),
           label = 'CO-4 EITC/CTC/FATC')

  #--------------------------------------------------------------------------
  # First broad-IIT rollout states
  #--------------------------------------------------------------------------

  # AZ-1: 2025 single, AGI 100,000. Standard deduction is 15,750, leaving
  # 84,250 at the 2.5% flat rate.
  run_case('AZ', 2025, list(agi = 100000),
           expect = list(st_ded = 15750, st_txbl_inc = 84250,
                         liab_st_iit = 84250 * 0.025),
           label = 'AZ-1 2025 flat rate and standard deduction')

  # AZ-2: a non-itemizer adds 34% of $1,000 charitable contributions to the
  # 2025 standard deduction: 15,750 + 340.
  run_case('AZ', 2025, list(agi = 100000, char_cash = 1000),
           expect = list(st_std_char_add = 340, st_ded = 16090,
                         liab_st_iit = (100000 - 16090) * 0.025),
           label = 'AZ-2 charitable standard deduction add-on')

  # AZ-3: two dependents, one under 17, at AGI 201,000. The $125 credit is
  # reduced five percentage points for the first $1,000 over the threshold.
  run_case('AZ', 2025,
           list(agi = 201000, n_dep = 2, n_dep_ctc = 1),
           expect = list(st_dep_credit = 118.75,
                         liab_st_iit = (201000 - 15750) * 0.025 - 118.75),
           label = 'AZ-3 dependent-credit phaseout')

  # GA-1: age-66 single with $5,000 wages and $60,000 pension income. The
  # $65,000 exclusion takes earned income first, then pension income. Taxable
  # income is 100,000 - 65,000 - 12,000 = 23,000.
  run_case('GA', 2025,
           list(agi = 100000, age1 = 66, wages1 = 5000,
                txbl_pens_dist = 60000),
           expect = list(st_retirement_excl = 65000, st_agi = 35000,
                         st_ded = 12000, liab_st_iit = 23000 * 0.0519),
           label = 'GA-1 retirement exclusion ordering')

  # GA-2: the 2025 child/dependent-care credit equals 50% of the $300 federal
  # credit but is nonrefundable.  It lowers, rather than exceeds, $415.20 tax.
  run_case('GA', 2025,
           list(agi = 20000, cdctc_nonref = 300),
           expect = list(st_cdctc = 150, st_credits_nonref = 150,
                         liab_st_iit = 8000 * 0.0519 - 150),
           label = 'GA-2 nonrefundable care credit')

  # GA-3: the pre-2024 standard deduction adds $1,300 each for age and
  # blindness.  The $2,700 personal exemption remains in 2023.
  run_case('GA', 2023, list(agi = 50000, age1 = 66, blind1 = 1),
           expect = list(st_std_ded = 8000, st_exempt = 2700,
                         liab_st_iit = 2087.25),
           label = 'GA-3 2023 age and blind standard deduction')

  # NC-1: at exactly $30,000 AGI in 2025, two qualifying children receive the
  # $2,500 table amount each. TI = 30,000 - 12,750 - 5,000 = 12,250.
  run_case('NC', 2025, list(agi = 30000, n_dep_ctc = 2),
           expect = list(st_child_ded = 5000, st_txbl_inc = 12250,
                         liab_st_iit = 12250 * 0.0425),
           label = 'NC-1 child deduction boundary')

  # NC-2: state itemization is independent.  Even without federal itemizing,
  # allowed components are 1,000 medical + 10,000 mortgage + 3,000 charity +
  # 10,000 of the 12,000 property tax amount.
  run_case('NC', 2025,
           list(agi = 50000, med_item_ded = 1000, mort_int_item_ded = 10000,
                char_item_ded = 3000, salt_prop = 12000),
           expect = list(st_item_ded = 24000, st_ded = 24000,
                         liab_st_iit = 26000 * 0.0425),
           label = 'NC-2 independent component itemization')

  # NC-3: 2026 enacted flat rate is 3.99% (later revenue-trigger reductions
  # are not assumed in this baseline).
  run_case('NC', 2026, list(agi = 50000),
           expect = list(liab_st_iit = (50000 - 12750) * 0.0399),
           label = 'NC-3 enacted 2026 rate')

  #--------------------------------------------------------------------------
  # Second broad-IIT rollout states
  #--------------------------------------------------------------------------

  # IN-1: 2025 single, AGI $50,000, one $1,000 personal exemption, and a
  # $1,000 federal EITC. Indiana's 10% refundable match is $100.
  run_case('IN', 2025, list(agi = 50000, eitc = 1000),
           expect = list(st_exempt = 1000, st_eitc = 100,
                         liab_st_iit = 49000 * 0.03 - 100),
           label = 'IN-1 exemptions and refundable EITC')

  # KY-1: 2025 single at MGI $16,000 is in the 90% family-size-credit band.
  # Tax is ($16,000 - $3,270) x 4% = $509.20; the credit is $458.28.
  run_case('KY', 2025, list(agi = 16000),
           expect = list(st_family_credit = 509.2 * 0.9,
                         liab_st_iit = 509.2 * 0.1),
           label = 'KY-1 family-size percentage-of-tax credit')

  # MI-1: 2025 single with a $5,800 personal exemption and $1,000 federal
  # EITC. Michigan's refundable EITC is 30% of the federal amount.
  run_case('MI', 2025, list(agi = 50000, eitc = 1000),
           expect = list(st_exempt = 5800, st_eitc = 300,
                         liab_st_iit = (50000 - 5800) * 0.0425 - 300),
           label = 'MI-1 exemption and refundable EITC')

  # CA-1: the $9,825 row of FTB 3514's 2025 EITC table gives a two-child
  # credit of $3,339. Exemption credits (153 + 2 x 475) are nonrefundable,
  # so the broad-IIT liability is the refundable CalEITC only. Neither child
  # is under six, isolating CalEITC from the separately tested YCTC.
  run_case('CA', 2025,
           list(agi = 9825, ei1 = 9825, n_dep = 2, n_dep_eitc = 2,
                dep_age1 = 7, dep_age2 = 8),
           expect = list(st_exempt_credit = 153 + 2 * 475,
                         st_earned_credit = 3339, liab_st_iit = -3339),
           label = 'CA-1 exemption credits and independent CalEITC')

  # CA-2: historical scalar parameters use the 2018 Form 540 amounts, not
  # a carry-forward from 2017.
  run_case('CA', 2018, list(agi = 0, n_dep = 1),
           expect = list(st_std_ded = 4401, st_exempt_credit = 118 + 367),
           label = 'CA-2 2018 standard deduction and exemption credits')

  # CA-3: Schedule CA subtracts federally taxable Social Security and state
  # income-tax refunds from the federal AGI starting point.
  run_case('CA', 2025, list(agi = 2000, txbl_ss = 1200, state_ref = 500),
           expect = list(st_subtractions = 1700, st_agi = 300),
           label = 'CA-3 Social Security and state-refund subtractions')

  # CA-4: Above the one-child $6,998 safe harbor, Form 3514 requires both
  # the earned-income ($2,016) and AGI ($1,162) lookups, using the lower one.
  run_case('CA', 2025,
           list(agi = 10000, ei1 = 7000, n_dep = 1, n_dep_eitc = 1,
                dep_age1 = 8),
           expect = list(st_earned_credit = 1162),
           label = 'CA-4 CalEITC federal-AGI second lookup')

  # CA-5: YCTC is a $1,189 per-return credit for a CalEITC claimant with a
  # qualifying child under six. At $30,000 earnings the linear worksheet
  # reduction gives $629.9675, which Form 3514 rounds to $630.
  run_case('CA', 2025,
           list(agi = 30000, ei1 = 30000, n_dep = 1, n_dep_eitc = 1,
                dep_age1 = 3),
           expect = list(st_yctc = 630),
           label = 'CA-5 YCTC annual phaseout')

  # CA-6: Beginning in 2022, a claimant with zero/negative earned income can
  # receive YCTC when the FTB wage and current-year-loss gates are satisfied.
  run_case('CA', 2022,
           list(agi = 0, n_dep = 1, n_dep_eitc = 1, dep_age1 = 3),
           expect = list(st_earned_credit = 0, st_yctc = 1083,
                         liab_st_iit = -1083),
           label = 'CA-6 YCTC zero-income expansion')

  # CA-7: the state aggregate path must retain the table attribute and pass
  # the selected schedule into the per-state calculator.
  ca_law = law %>% filter(state == 'CA')
  attr(ca_law, 'credit_tables') = credit_tables
  ca_total = get_state_totals(
    tax_units_calc = st_test_unit(list(
      id = 1, year = 2025, agi = 9825, ei1 = 9825, n_dep = 2,
      n_dep_eitc = 2, dep_age1 = 7, dep_age2 = 8
    )),
    state_tax_law = ca_law,
    state_weights = tibble(id = 1, state = 'CA', weight = 1),
    yr = 2025
  )
  stopifnot(
    'CA-7 aggregate table integration failed' =
      ca_total %>%
      filter(variable == 'st_earned_credit') %>%
      pull(value) == 3339
  )

  # CA-8: a dependent with $3,000 of earned income receives the smaller of
  # the regular $5,706 deduction and max($1,350, $3,000 + $450) = $3,450.
  run_case('CA', 2025, list(agi = 10000, dep_status = 1, ei1 = 3000),
           expect = list(st_std_ded = 3450, st_txbl_inc = 6550),
           label = 'CA-8 dependent standard deduction worksheet')

  # CA-9: the Schedule CA limitation protects $20,000 medical, $10,000
  # investment interest, and $5,000 casualty deductions. The reduction is
  # the lesser of 80% of $160,000 unprotected deductions and 6% of the
  # $47,797 excess over the $252,203 single threshold: $2,867.82.
  run_case('CA', 2025,
           list(agi = 300000, med_item_ded = 20000,
                inv_int_item_ded = 10000, casualty_item_ded = 5000,
                mort_int_item_ded = 100000, char_item_ded = 50000,
                salt_prop = 10000),
           expect = list(st_item_ded = 195000 - 0.06 * (300000 - 252203)),
           label = 'CA-9 protected itemized deduction limitation')

  # CA-10: Form 540 line 62 applies 1% to taxable income over $1 million,
  # after the regular $5,706 standard deduction. The threshold itself owes no
  # surcharge; this return has $1,004,294 of taxable income and owes $42.94.
  run_case('CA', 2025, list(agi = 1010000),
           expect = list(st_taxable_income_surtax = 42.94),
           label = 'CA-10 Behavioral Health Services Tax')

  #--------------------------------------------------------------------------
  # Narrow and partial-IIT jurisdictions
  #--------------------------------------------------------------------------

  # NH-1: TY2024 single, $4,800 interest/dividends. $2,400 exemption leaves
  # $2,400 x 3% = $72. The broad-IIT component remains zero.
  run_case('NH', 2024, list(txbl_int = 3000, div_ord = 1800),
           expect = list(liab_st_iit = 0, liab_st_narrow_iit = 72,
                         liab_st_individual_net = 72),
           label = 'NH-1 2024 interest/dividends tax')

  # NH-2: age-70 filer gets an additional $1,200 exemption: $1,200 x 3% = $36.
  run_case('NH', 2024, list(txbl_int = 3000, div_ord = 1800, age1 = 70),
           expect = list(liab_st_narrow_iit = 36),
           label = 'NH-2 age exemption')

  # NH-3: full repeal for taxable years beginning in 2025.
  run_case('NH', 2025, list(txbl_int = 3000, div_ord = 1800),
           expect = list(liab_st_narrow_iit = 0, liab_st_individual_net = 0),
           label = 'NH-3 repeal')

  # TN-1: TY2017 single, ($3,000 - $1,250) x 4% = $70.
  run_case('TN', 2017, list(txbl_int = 3000),
           expect = list(liab_st_narrow_iit = 70),
           label = 'TN-1 2017 Hall tax')

  # TN-2: TY2020 MFJ, ($6,000 - $2,500) x 1% = $35.
  run_case('TN', 2020, list(filing_status = 2, txbl_int = 6000),
           expect = list(liab_st_narrow_iit = 35),
           label = 'TN-2 2020 phase-down')

  # TN-2b: for a jointly filed return with one blind spouse, the unavailable
  # ownership split is approximated as one-half of the Hall-tax base.
  run_case('TN', 2020,
           list(filing_status = 2, txbl_int = 6000, blind1 = 1),
           expect = list(liab_st_narrow_iit = 17.5),
           label = 'TN-2b joint blind-spouse allocation proxy')

  # TN-3: age-65 low-income exemption applies to a $36,000 all-source-income
  # proxy. TY2021 onward is fully repealed in any case.
  run_case('TN', 2019, list(age1 = 67, agi = 36000, txbl_int = 3000),
           expect = list(liab_st_narrow_iit = 0),
           label = 'TN-3 age-65 low-income exemption')

  # TN-3b: joint return where only the SECONDARY filer is 65+ and combined
  # income ($50,000) is under the $68,000 joint limit. Either spouse's age
  # qualifies (Hall Income Tax Manual), so the return is fully exempt. Under
  # the pre-fix single-filer age test this wrongly owed (6,000-2,500) x 2%.
  run_case('TN', 2019,
           list(filing_status = 2, age1 = 60, age2 = 67, agi = 50000,
                txbl_int = 6000),
           expect = list(liab_st_narrow_iit = 0),
           label = 'TN-3b joint age-65 exemption via secondary filer')

  # TN-3c: same joint return but combined income ($70,000) exceeds the
  # $68,000 joint limit, so the age-65 exemption does NOT apply and the Hall
  # tax is owed: (6,000 - 2,500) x 2% = $70. Confirms the income test binds
  # on the secondary-filer path.
  run_case('TN', 2019,
           list(filing_status = 2, age1 = 60, age2 = 67, agi = 70000,
                txbl_int = 6000),
           expect = list(liab_st_narrow_iit = 70),
           label = 'TN-3c joint age-65 exemption denied above income limit')

  # TN-3d: joint return with a 100-year-old secondary filer (age-100
  # exemption begins TY2018). Income is above the age-65 limit, isolating the
  # age-100 rule. Either spouse at 100+ exempts the return; pre-fix this
  # tested only the primary filer and wrongly owed (6,000-2,500) x 3%.
  run_case('TN', 2018,
           list(filing_status = 2, age1 = 50, age2 = 100, agi = 200000,
                txbl_int = 6000),
           expect = list(liab_st_narrow_iit = 0),
           label = 'TN-3d joint age-100 exemption via secondary filer')

  run_case('TN', 2021, list(txbl_int = 3000),
           expect = list(liab_st_narrow_iit = 0),
           label = 'TN-4 repeal')

  # WA-1: TY2024, $400,000 long-term gains less $270,000 deduction leaves
  # $130,000 x 7% = $9,100.
  run_case('WA', 2024, list(kg_lt = 400000),
           expect = list(liab_st_iit = 0, liab_st_ltcg_excise = 9100,
                         liab_st_individual_net = 9100),
           label = 'WA-1 capital gains excise')

  # WA-2: TY2025 base tax is $1,222,000 x 7%; surcharge is $222,000 x 2.9%.
  run_case('WA', 2025, list(kg_lt = 1500000),
           expect = list(liab_st_ltcg_excise = 91978),
           label = 'WA-2 2025 capital gains surcharge')

  # WA-3: TY2022, one-child WFTC maximum is $600. At income $40,000, the
  # reduction is ($40,000 - $38,492) x 12% = $180.96, so the rounded refund
  # is $419 and net fiscal liability is -$419.
  run_case('WA', 2022,
           list(agi = 40000, ei1 = 40000, n_dep_eitc = 1),
           expect = list(st_refund_wftc = 419, liab_st_individual_net = -419),
           label = 'WA-3 WFTC phaseout')

  # WA-4: TY2025 childless filer: $335 - ($396 x $335 / $2,500) = $281.94,
  # rounded to $282. A claimant over age 64 is not eligible without a child.
  run_case('WA', 2025, list(agi = 17000, ei1 = 17000),
           expect = list(st_refund_wftc = 282),
           label = 'WA-4 childless WFTC')
  run_case('WA', 2025, list(agi = 17000, ei1 = 17000, age1 = 65),
           expect = list(st_refund_wftc = 0),
           label = 'WA-5 childless age limit')

  #--------------------------------------------------------------------------
  # Structural smoke test: a coarse grid of units through every broad-IIT
  # baseline state and several years must produce finite, non-NA results
  #--------------------------------------------------------------------------

  grid = expand_grid(
    filing_status = c(1, 2, 3, 4),
    agi_level     = c(-5000, 0, 15000, 60000, 250000, 1500000, 30000000),
    age1          = c(30, 68),
    n_dep         = c(0, 2)
  ) %>%
    pmap_dfr(function(filing_status, agi_level, age1, n_dep) {
      st_test_unit(list(
        filing_status = filing_status, agi = agi_level,
        txbl_inc = pmax(0, agi_level - 15000), age1 = age1,
        age2 = if (filing_status == 2) age1 else NA_integer_,
        n_dep = n_dep,
        dep_age1 = if (n_dep > 0) 3L else NA_integer_,
        dep_age2 = if (n_dep > 1) 9L else NA_integer_,
        eitc = if (agi_level > 0 & agi_level < 40000 & n_dep > 0) 4000 else 0,
        txbl_ss = if (age1 >= 65) 10000 else 0,
        salt_prop = if (agi_level >= 60000) 6000 else 0,
        itemizing = agi_level >= 250000,
        item_ded = if (agi_level >= 250000) 50000 else 0,
        item_ded_ex_limits = if (agi_level >= 250000) 50000 else 0,
        salt_item_ded = if (agi_level >= 250000) 10000 else 0,
        char_item_ded = if (agi_level >= 250000) 20000 else 0,
        std_ded = 14600
      ))
    })

  for (st in c('IL', 'CO', 'NY', 'AZ', 'GA', 'NC', 'IN', 'KY', 'MI', 'CA')) {
    for (yr in c(2017, 2021, 2024, 2026, 2030)) {
      law_slice = law %>%
        filter(state == st, year == yr) %>%
        select(-state, -year)
      out = grid %>%
        left_join(law_slice, by = 'filing_status') %>%
        do_state_taxes()
      stopifnot(
        'smoke: NA liability'  = !anyNA(out$liab_st_iit),
        'smoke: infinite liab' = all(is.finite(out$liab_st_iit)),
        'smoke: NA filer flag' = !anyNA(out$st_filer)
      )
    }
  }
  message('test_state_calc smoke grid: PASSED (', nrow(grid), ' units x 10 states x 5 years)')

  # Subset-states regression: a law table built WITHOUT a given state lacks
  # that state's feature columns entirely (not just NA cells); the calculator
  # must handle both shapes (real-run failure: IL+NY slice missing CO's
  # ctc_tier_shares columns)
  for (subset_states in list(c('IL', 'NY'), 'CO', 'NY', c('AZ', 'NC'))) {
    law_sub = build_state_tax_law(
      states  = subset_states,
      years   = 2024,
      indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
                mutate(growth = 0.025)
    )
    for (st in subset_states) {
      out = grid %>%
        left_join(law_sub %>%
                    filter(state == st) %>%
                    select(-state, -year),
                  by = 'filing_status') %>%
        do_state_taxes()
      stopifnot('subset-states: NA liability' = !anyNA(out$liab_st_iit))
    }
  }
  message('test_state_calc subset-states regression: PASSED')

  # No-broad-IIT zero stubs: these jurisdictions are safe to include in
  # states = all because they generate no state IIT liability and no state
  # filer flag across the model window. NH/TN/WA are intentionally excluded:
  # they have special non-broad taxes or credits that need custom modeling.
  no_broad_zero_states = c('AK', 'FL', 'NV', 'SD', 'TX', 'WY')
  law_zero = build_state_tax_law(
    states  = no_broad_zero_states,
    years   = 2017:2035,
    indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
              mutate(growth = 0.025)
  )
  for (st in no_broad_zero_states) {
    for (yr in c(2017, 2021, 2024, 2026, 2030)) {
      out = grid %>%
        left_join(law_zero %>%
                    filter(state == st, year == yr) %>%
                    select(-state, -year),
                  by = 'filing_status') %>%
        do_state_taxes()
      stopifnot(
        'no-broad-IIT stub: nonzero liability' = all(out$liab_st_iit == 0),
        'no-broad-IIT stub: state filer flagged' = !any(out$st_filer)
      )
    }
  }
  message('test_state_calc no-broad-IIT zero stubs: PASSED')

  message('test_state_calc: ALL TESTS PASSED')
  invisible(TRUE)
}



st_test_unit = function(overrides = list()) {

  #----------------------------------------------------------------------------
  # Builds a one-row synthetic post-federal tax unit with neutral defaults
  # for every variable the state calculator requires; overrides applied on
  # top.
  #
  # Parameters:
  #   - overrides (list) : named values replacing the defaults
  #
  # Returns: one-row tibble (df).
  #----------------------------------------------------------------------------

  # 0/1 numerics for flag variables, mirroring the production microdata
  # (the calculator must accept both; regression for a real-data failure)
  unit = list(
    filing_status = 1, filer = 1, dep_status = 0,
    age1 = 40, age2 = NA_integer_, blind1 = 0, blind2 = NA_real_,
    n_dep = 0, n_dep_ctc = 0, dep_age1 = NA_integer_, dep_age2 = NA_integer_,
    dep_age3 = NA_integer_,
    agi = 0, txbl_inc = 0, itemizing = 0,
    exempt_int = 0, state_ref = 0, gross_ss = 0, txbl_ss = 0,
    txbl_int = 0, div_ord = 0, div_pref = 0, kg_lt = 0, kg_st = 0,
    txbl_kg = 0, wages1 = 0, wages2 = 0, sole_prop = 0, part_active = 0,
    part_passive = 0, scorp = 0, farm = 0, rent = 0, other_inc = 0,
    sch_e = 0, part_scorp = 0, ei1 = 0, ei2 = 0, n_dep_eitc = 0,
    txbl_pens_dist = 0,
    txbl_ira_dist = 0, ot_ded = 0, char_cash = 0, char_noncash = 0,
    item_ded = 0, item_ded_ex_limits = 0, salt_item_ded = 0,
    salt_inc_sales = 0, salt_prop = 0, salt_pers = 0,
    med_item_ded = 0, mort_int_item_ded = 0, inv_int_item_ded = 0,
    casualty_item_ded = 0, char_item_ded = 0, misc_item_ded = 0,
    other_item_ded = 0, std_ded = 0,
    eitc = 0, ctc_nonref = 0, ctc_ref = 0, cdctc_nonref = 0,
    cdctc_ref = 0, care_exp = 0
  )
  for (v in names(overrides)) {
    unit[[v]] = overrides[[v]]
  }
  as_tibble(unit)
}
