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
                'IN', 'KY', 'MI', 'CA', 'ND', 'SC', 'CT', 'VA', 'UT', 'OH',
                'PA', 'ID', 'MN', 'MD', 'WI'),
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

  # NC-4: taxable Social Security fully deducted (G.S. 105-153.5(b)(6);
  # regression for the 2026-07-23 survey finding). Single 68, AGI 50,000
  # incl. taxable SS 20,000: NC AGI 30,000 - std 12,750 = 17,250 x 4.5%
  run_case('NC', 2024,
           list(agi = 50000, age1 = 68, txbl_ss = 20000, gross_ss = 24000),
           expect = list(st_agi = 30000, liab_st_iit = 17250 * 0.045),
           label = 'NC-4 Social Security deduction')

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

  # MI-2: 2019 Tier 1 (born before 1946 = age 74+): single 75, pension
  # 60,000 capped at the Form 4884 maximum 52,808; senior investment cap
  # (11,771) fully absorbed by the retirement subtraction. AGI 65,000 ->
  # MI base 12,192; exemption 4,400 -> tax 4.25% x 7,792
  run_case('MI', 2019,
           list(agi = 65000, age1 = 75, txbl_pens_dist = 60000,
                txbl_int = 5000),
           expect = list(st_agi = 65000 - 52808,
                         liab_st_iit = (65000 - 52808 - 4400) * 0.0425),
           label = 'MI-2 Tier 1 pension cap')

  # MI-3: 2019 Tier 2 (born 1946-1952 = ages 67-73): single 68, pension
  # 30,000 capped at 20,000 (the 67+ Michigan Standard Deduction amount,
  # modeled as a pension-only subtraction)
  run_case('MI', 2019, list(agi = 40000, age1 = 68, txbl_pens_dist = 30000),
           expect = list(st_agi = 20000),
           label = 'MI-3 Tier 2 $20k cap')

  # MI-4: 2019 Tier 3 (born after 1952, under 67): no retirement
  # subtraction at all
  run_case('MI', 2019, list(agi = 40000, age1 = 60, txbl_pens_dist = 20000),
           expect = list(st_agi = 40000),
           label = 'MI-4 Tier 3 no subtraction')

  # MI-5: 2024 phase-in (50%, born 1946-1962 = ages 62-78; older spouse
  # controls per return): MFJ 66/64, pensions 80,000 capped at the printed
  # joint maximum 64,040. Exemptions 2 x 5,600
  run_case('MI', 2024,
           list(agi = 100000, filing_status = 2, age1 = 66, age2 = 64,
                txbl_pens_dist = 80000),
           expect = list(st_agi = 100000 - 64040,
                         liab_st_iit = (100000 - 64040 - 11200) * 0.0425),
           label = 'MI-5 2024 phase-in, older-spouse joint cap')

  # MI-6: 2026 fully phased in (everyone 59+): single 60, pension 70,000
  # capped at the carried 2025 maximum 65,897
  run_case('MI', 2026, list(agi = 75000, age1 = 60, txbl_pens_dist = 70000),
           expect = list(st_agi = 75000 - 65897),
           label = 'MI-6 2026 full restoration')

  # MI-7: 2024 senior investment income subtraction (born before 1946 =
  # age 79+): MFJ 80/78, pension 20,000 (fully subtracted, Tier 1 joint
  # cap 128,080), investment income 10,000 int + 8,000 div + 20,000 LTCG;
  # cap 28,548 less the 20,000 retirement subtraction -> 8,548
  run_case('MI', 2024,
           list(agi = 60000, filing_status = 2, age1 = 80, age2 = 78,
                wages1 = 2000, txbl_pens_dist = 20000, txbl_int = 10000,
                div_ord = 8000, kg_lt = 20000),
           expect = list(st_agi = 60000 - 20000 - 8548),
           label = 'MI-7 senior investment income subtraction')

  # MI-8: Tier 2 Michigan Standard Deduction against ALL income: 2019
  # single 68 (born 1951), wages 40,000, NO pension -> full 20,000 off the
  # base (Schedule 1 line 23); taxable = 20,000 - 4,400 exemption
  run_case('MI', 2019, list(agi = 40000, age1 = 68, wages1 = 40000),
           expect = list(st_agi = 20000,
                         liab_st_iit = (20000 - 4400) * 0.0425),
           label = 'MI-8 Tier 2 standard deduction, no pension')

  # MI-9: Tier 3 netted standard deduction: 2021 single 68 (born 1953,
  # below the no-net age 69), wages 30,000 + taxable SS 8,000. Worksheet 2:
  # 20,000 - 8,000 SS - 4,900 exemption = 7,100, claimed alongside the
  # normal SS subtraction and exemption. Base = 38,000 - 8,000 - 7,100
  run_case('MI', 2021,
           list(agi = 38000, age1 = 68, wages1 = 30000, txbl_ss = 8000,
                gross_ss = 10000),
           expect = list(st_agi = 38000 - 8000 - 7100,
                         liab_st_iit = (22900 - 4900) * 0.0425),
           label = 'MI-9 Tier 3 netted standard deduction')

  # MI-10: Tier 1 (born before 1946) is INELIGIBLE for the standard
  # deduction: 2019 single 75, wages 50,000, pension 1,000 -> only the
  # 1,000 pension subtraction (guards the std-deduction overgrant)
  run_case('MI', 2019,
           list(agi = 51000, age1 = 75, wages1 = 50000,
                txbl_pens_dist = 1000),
           expect = list(st_agi = 50000),
           label = 'MI-10 Tier 1 std-deduction ineligibility')

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

  # CA-8b: a dependent of another taxpayer is ineligible for the refundable
  # CalEITC (and hence YCTC), mirroring the federal EITC. Pre-fix this filer
  # wrongly received a childless CalEITC and a negative (refund) liability;
  # post-fix they owe the ordinary tax on $6,550 taxable income (1% = $65.50)
  # with no refundable credit.
  run_case('CA', 2025, list(agi = 10000, dep_status = 1, ei1 = 3000),
           expect = list(st_earned_credit = 0, st_yctc = 0,
                         liab_st_individual_net = 65.5),
           label = 'CA-8b dependent filer excluded from CalEITC')

  # CA-8c: a married-filing-separately filer is barred from the refundable
  # CalEITC (earned_credit_mfs_eligible = 0), mirroring the federal EITC.
  run_case('CA', 2025, list(filing_status = 3, agi = 10000, ei1 = 3000),
           expect = list(st_earned_credit = 0),
           label = 'CA-8c MFS filer barred from CalEITC')

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
  # North Dakota (Form ND-1) -- federal-taxable-income start, graduated rates
  #--------------------------------------------------------------------------

  # ND-1: TY2020 single, ND taxable income $50,000. Pre-reform 5-bracket
  # schedule: 1.10% x 40,125 + 2.04% x (50,000 - 40,125) = 441.375 + 201.45.
  run_case('ND', 2020, list(agi = 50000, txbl_inc = 50000),
           expect = list(st_agi = 50000, liab_st_iit = 642.825),
           label = 'ND-1 2020 single pre-reform schedule')

  # ND-2: TY2023 single, $100,000. HB1158 three-tier: 0% to 44,725, then
  # 1.95% x (100,000 - 44,725) = 1,077.8625. Tests the zero-tax bottom bracket.
  run_case('ND', 2023, list(agi = 100000, txbl_inc = 100000),
           expect = list(liab_st_iit = 1077.8625),
           label = 'ND-2 2023 single HB1158 three-tier')

  # ND-3: TY2023 single with $10,000 net long-term gain. 40% exclusion drops
  # the base to 96,000: 1.95% x (96,000 - 44,725) = 999.8625.
  run_case('ND', 2023, list(agi = 100000, txbl_inc = 100000, kg_lt = 10000),
           expect = list(st_agi = 96000, liab_st_iit = 999.8625),
           label = 'ND-3 40% net long-term capital gain exclusion')

  # ND-4: TY2024 single with $5,000 qualified dividends. 40% exclusion drops
  # the base to 58,000: 1.95% x (58,000 - 47,150) = 211.575.
  run_case('ND', 2024, list(agi = 60000, txbl_inc = 60000, div_pref = 5000),
           expect = list(st_agi = 58000, liab_st_iit = 211.575),
           label = 'ND-4 40% qualified dividend exclusion')

  # ND-5: TY2019 age-40 single with $3,000 taxable SS and federal AGI $45,000
  # (<= $50,000 cap): the SS exemption applies at ANY age, base 42,000.
  # 1.10% x 39,450 + 2.04% x (42,000 - 39,450) = 433.95 + 52.02.
  run_case('ND', 2019,
           list(agi = 45000, txbl_inc = 45000, txbl_ss = 3000, age1 = 40),
           expect = list(st_agi = 42000, liab_st_iit = 485.97),
           label = 'ND-5 all-ages AGI-capped SS exemption (2019)')

  # ND-5b: same filer with AGI $60,000 (> cap): no SS subtraction, base 45,000.
  run_case('ND', 2019,
           list(agi = 60000, txbl_inc = 45000, txbl_ss = 3000, age1 = 40),
           expect = list(st_agi = 45000, liab_st_iit = 547.17),
           label = 'ND-5b SS exemption denied above AGI cap')

  # ND-6: TY2020 MFS uses its own schedule (not single/2). $50,000:
  # 1.10% x 33,525 + 2.04% x (50,000 - 33,525) = 368.775 + 336.09.
  run_case('ND', 2020, list(filing_status = 3, agi = 50000, txbl_inc = 50000),
           expect = list(liab_st_iit = 704.865),
           label = 'ND-6 2020 MFS distinct schedule')

  # ND-7: TY2020 MFJ, $100,000: 1.10% x 67,050 + 2.04% x (100,000 - 67,050)
  # = 737.55 + 672.18.
  run_case('ND', 2020, list(filing_status = 2, agi = 100000, txbl_inc = 100000),
           expect = list(liab_st_iit = 1409.73),
           label = 'ND-7 2020 MFJ schedule')

  #--------------------------------------------------------------------------
  # South Carolina (SC1040) -- federal-taxable-income start, one schedule for
  # all filing statuses
  #--------------------------------------------------------------------------

  # SC-1: TY2021 pre-reform six-bracket schedule, taxable income $50,000.
  # 3/4/5/6% on each $3,110 band + 7% x (50,000 - 15,550) = 559.80 + 2,411.50.
  run_case('SC', 2021, list(agi = 50000, txbl_inc = 50000),
           expect = list(liab_st_iit = 2971.3),
           label = 'SC-1 2021 pre-reform six-bracket schedule')

  # SC-2: TY2024 reform three-bracket, $60,000. 3% x (17,350 - 3,460) + 6.2% x
  # (60,000 - 17,350) = 6.2% x 60,000 - 659 (exact published constant) = 3,061.
  run_case('SC', 2024, list(agi = 60000, txbl_inc = 60000),
           expect = list(liab_st_iit = 3061.0),
           label = 'SC-2 2024 reform schedule (exact constant C)')

  # SC-3: TY2024 with $20,000 net long-term gain, 44% deduction -> base 51,200:
  # 6.2% x 51,200 - 659 = 2,515.40.
  run_case('SC', 2024, list(agi = 60000, txbl_inc = 60000, kg_lt = 20000),
           expect = list(st_agi = 51200, liab_st_iit = 2515.4),
           label = 'SC-3 44% net capital gain deduction')

  # SC-4: SS fully exempt. TY2024, $40,000 base incl. $10,000 taxable SS ->
  # base 30,000: 6.2% x 30,000 - 659 = 1,201.
  run_case('SC', 2024,
           list(agi = 40000, txbl_inc = 40000, txbl_ss = 10000, age1 = 67),
           expect = list(st_agi = 30000, liab_st_iit = 1201.0),
           label = 'SC-4 full Social Security exemption')

  # SC-5: retirement-income deduction. Age 67 gets the $10,000 cap; $8,000 of
  # pension is fully deducted -> base 32,000: 6.2% x 32,000 - 659 = 1,325.
  run_case('SC', 2024,
           list(agi = 40000, txbl_inc = 40000, txbl_pens_dist = 8000, age1 = 67),
           expect = list(st_agi = 32000, liab_st_iit = 1325.0),
           label = 'SC-5 age-65 retirement deduction ($10k cap)')

  # SC-5b: under-65 filer gets only the $3,000 retirement cap -> base 37,000.
  run_case('SC', 2024,
           list(agi = 40000, txbl_inc = 40000, txbl_pens_dist = 8000, age1 = 50),
           expect = list(st_agi = 37000),
           label = 'SC-5b under-65 retirement deduction ($3k cap)')

  # SC-6: nonrefundable SC EITC at 125% of the $1,000 federal EITC (TY2023).
  # Tax before credits on $60,000 = 6.4% x 60,000 - 670 = 3,170; less $1,250.
  run_case('SC', 2023, list(agi = 60000, txbl_inc = 60000, eitc = 1000),
           expect = list(st_eitc = 1250, liab_st_iit = 1920.0),
           label = 'SC-6 nonrefundable SC EITC (125% of federal)')

  #--------------------------------------------------------------------------
  # Connecticut (CT-1040) -- federal-AGI start, graduated rates with stepped
  # Table C add-back / Table D recapture, Table A exemption phase-out,
  # Table E percentage-of-tax credit (all keyed to CT AGI)
  #--------------------------------------------------------------------------

  # CT-1: 2024 single, AGI 40,000. Exemption 15,000 - 10 x 1,000 = 5,000
  # (Table A band (39,000, 40,000]); TI 35,000. Tax = 2% x 10,000 + 4.5% x
  # 25,000 = 1,325. No add-back/recapture (< 56,500). Table E: 40,000 in
  # (33,300, 60,000] -> 10% -> 132.50. Liab = 1,325 - 132.50
  run_case('CT', 2024, list(agi = 40000),
           expect = list(st_agi = 40000, st_exempt = 5000,
                         st_txbl_inc = 35000, st_tax_pre_credit = 1325,
                         st_pct_credit = 132.5, liab_st_iit = 1192.5),
           label = 'CT-1 2024 exemption phase-out + Table E')

  # CT-2: 2025 single, AGI 250,000. Exemption 0; TI 250,000. Schedule:
  # 10,750 + 6.5% x 50,000 = 14,000. Table C: ceil(193,500/5,000) x 25 = 975
  # -> capped 250. Tier 0: ceil(145,000/5,000) x 25 = 725 -> capped 250.
  # Tier 1: ceil(50,000/5,000) x 90 = 900. No Table E above 64,500.
  # Tax = 14,000 + 250 + 250 + 900 = 15,400
  run_case('CT', 2025, list(agi = 250000),
           expect = list(st_exempt = 0, st_tax_pre_credit = 15400,
                         st_pct_credit = 0, liab_st_iit = 15400),
           label = 'CT-2 2025 stepped add-back + two recapture tiers')

  # CT-3: 2017 MFJ, AGI 110,000. Exemption 0 (> 71,000); TI 110,000. Old
  # rates: 600 + 5% x 80,000 + 5.5% x 10,000 = 5,150. Table C: ceil(9,500/
  # 5,000) x 40 = 80 (tier 0 inert pre-2024). No Table E above 100,500
  run_case('CT', 2017, list(agi = 110000, filing_status = 2),
           expect = list(st_tax_pre_credit = 5230, liab_st_iit = 5230),
           label = 'CT-3 2017 rates + Table C add-back')

  # CT-4: 2025 MFJ retirees (68/66), AGI 110,000 = SS 20,000 taxable (30,000
  # gross) + pensions 40,000 + IRA 20,000 + interest 30,000. Above the SS
  # limit: sub = 20,000 - 25% x 30,000 = 12,500. Phase-out factor at
  # 110,000 (MFJ band 110,000-114,999) = 0.55: pension 40,000 x 1.0 + IRA
  # 20,000 x 0.75 = 55,000 x 0.55 = 30,250. CT AGI = 67,250; exemption
  # 24,000 - 20 x 1,000 = 4,000; TI 63,250. Tax = 400 + 4.5% x 43,250 =
  # 2,346.25. Table E: 67,250 in (52,000, 96,000] -> 10% -> 234.625.
  # Property tax credit: min(6,000, 300) x 1.0 (67,250 < 70,500) = 300.
  # Liab = 2,346.25 - 234.625 - 300 = 1,811.625
  run_case('CT', 2025,
           list(agi = 110000, filing_status = 2, age1 = 68, age2 = 66,
                gross_ss = 30000, txbl_ss = 20000, txbl_pens_dist = 40000,
                txbl_ira_dist = 20000, txbl_int = 30000, salt_prop = 6000),
           expect = list(st_agi = 67250, st_exempt = 4000,
                         st_tax_pre_credit = 2346.25,
                         liab_st_iit = 1811.625),
           label = 'CT-4 SS cap + pension/IRA phase-out + property tax credit')

  # CT-5: 2023 MFJ, AGI 26,000, 2 kids, federal EITC 6,000. Full exemption
  # 24,000 -> TI 2,000; tax 3% x 2,000 = 60. Table E: 26,000 in (24,000,
  # 30,000] -> 75% -> 45. CT EITC = 40% x 6,000 = 2,400 (refundable; no
  # child bonus until 2025). Liab = (60 - 45) - 2,400 = -2,385
  run_case('CT', 2023,
           list(agi = 26000, filing_status = 2, n_dep = 2, n_dep_eitc = 2,
                dep_age1 = 5, dep_age2 = 8, eitc = 6000),
           expect = list(st_exempt = 24000, st_tax_pre_credit = 60,
                         st_eitc = 2400, liab_st_iit = -2385),
           label = 'CT-5 low-income EITC + 75% Table E')

  # CT-6: 2025 MFS, AGI 60,000. Exemption 0 (> 35,000); single-schedule tax
  # 200 + 4.5% x 40,000 + 5.5% x 10,000 = 2,550. Table C MFS: ceil(9,750/
  # 2,500) x 25 = 100. No Table E above 52,500. Liab = 2,650
  run_case('CT', 2025, list(agi = 60000, filing_status = 3),
           expect = list(st_tax_pre_credit = 2650, liab_st_iit = 2650),
           label = 'CT-6 MFS Table C increments')

  # CT-7: 2025 single, AGI 20,000, 1 kid, federal EITC 3,000. Exemption
  # 15,000 -> TI 5,000; tax 2% x 5,000 = 100. Table E: 20,000 in (19,800,
  # 20,300] -> 60% -> 60. CT EITC = 40% x 3,000 + 250 child bonus = 1,450.
  # Liab = (100 - 60) - 1,450 = -1,410
  run_case('CT', 2025,
           list(agi = 20000, n_dep = 1, n_dep_eitc = 1, dep_age1 = 5,
                eitc = 3000),
           expect = list(st_eitc = 1450, liab_st_iit = -1410),
           label = 'CT-7 2025 EITC child bonus')

  # CT-8: fractional CT AGI rounds to whole dollars before the Table E
  # lookup (the booklet's own instruction; adopted with the dense-table
  # migration, review item #7). AGI 19,800.40 rounds to 19,800 -> band
  # (19,300, 19,800] -> 65% (the old continuous-band encoding put
  # 19,800.40 in the 60% band). Exemption 15,000; TI = 4,800.40; tax =
  # 2% x 4,800.40 = 96.008; credit 65% -> liab = 96.008 x 0.35 = 33.6028
  run_case('CT', 2024, list(agi = 19800.40),
           expect = list(st_tax_pre_credit = 96.008,
                         st_pct_credit = 96.008 * 0.65,
                         liab_st_iit = 96.008 * 0.35),
           tol = 0.001, label = 'CT-8 Table E whole-dollar rounding')

  #--------------------------------------------------------------------------
  # Virginia (Form 760)
  #--------------------------------------------------------------------------

  # VA-1: 2024 single, AGI 50,000, federal standard deduction. VAGI 50,000;
  # std 8,500; exemption 930. TI = 40,570. Tax = 720 + 5.75% x 23,570 =
  # 2,075.275
  run_case('VA', 2024, list(agi = 50000),
           expect = list(st_agi = 50000, st_ded = 8500, st_exempt = 930,
                         st_txbl_inc = 40570, liab_st_iit = 2075.275),
           label = 'VA-1 basic single')

  # VA-2: 2024 single, VAGI 11,900 < $11,950 filing threshold -> tax is $0
  # outright (Form 760 Line 9 cliff) and the unit is not a filer
  run_case('VA', 2024, list(agi = 11900),
           expect = list(liab_st_iit = 0, st_filer = 0),
           label = 'VA-2 no-tax cliff')

  # VA-3: 2024 single, AGI 14,000 (above threshold, below the $15,060
  # poverty guideline for family size 1). TI = 14,000 - 8,500 - 930 =
  # 4,570 -> tax 60 + 3% x 1,570 = 107.10. CLI = $300 x 1 exemption
  # (nonrefundable) wipes the tax
  run_case('VA', 2024, list(agi = 14000),
           expect = list(st_cli = 300, liab_st_iit = 0),
           label = 'VA-3 CLI')

  # VA-4: 2024 HoH (files as single in VA), AGI 25,000, 2 deps, federal
  # EITC 6,000. Std 8,500 (single amount); exemptions 3 x 930 = 2,790.
  # TI = 13,710 -> tax 120 + 5% x 8,710 = 555.50. Choice: 20% nonref
  # (benefit 555.50) vs 15% refundable (900) -> refundable 900. CLI (300 x
  # 3 = 900 nonref, family of 3 under the 25,820 guideline) benefit 555.50
  # loses. Liab = 555.50 - 900 = -344.50
  run_case('VA', 2024,
           list(filing_status = 4, agi = 25000, n_dep = 2, dep_age1 = 8,
                dep_age2 = 10, eitc = 6000),
           expect = list(st_ded = 8500, st_eitc = 900, st_cli = 0,
                         liab_st_iit = -344.50),
           label = 'VA-4 refundable EITC option')

  # VA-5: 2022 single, AGI 30,000, 1 dep, federal EITC 1,500. TI = 30,000
  # - 8,000 - 1,860 = 20,140 -> tax 720 + 5.75% x 3,140 = 900.55. Choice:
  # 20% nonref (300) vs 15% refundable (225) -> nonrefundable 300.
  # Liab = 600.55
  run_case('VA', 2022,
           list(agi = 30000, n_dep = 1, dep_age1 = 6, eitc = 1500),
           expect = list(st_eitc = 300, liab_st_iit = 600.55),
           label = 'VA-5 nonrefundable EITC wins')

  # VA-6: 2025 MFJ both 70 (income-tested window), AGI 90,000 incl. 10,000
  # taxable SS, no wages. AFAGI = 80,000 -> joint excess 5,000; age
  # deduction pool 24,000 - 5,000 = 19,000. VAGI = 90,000 - 10,000 SS -
  # 19,000 = 61,000. Std 17,500; exemptions 2 x 930 + 2 x 800 = 3,460.
  # TI = 40,040 -> schedule tax 2,044.80. STA: each spouse nets (61,000/2
  # - 1,730) = 28,770 > half TI 20,020 -> two pieces at 20,020 -> 2 x
  # 893.65 = 1,787.30; STA = min(259, 257.50). Liab = 1,787.30
  run_case('VA', 2025,
           list(filing_status = 2, agi = 90000, age1 = 70, age2 = 70,
                txbl_ss = 10000, txbl_pens_dist = 60000),
           expect = list(st_agi = 61000, st_exempt = 3460,
                         st_txbl_inc = 40040, st_tax_pre_credit = 1787.30,
                         liab_st_iit = 1787.30),
           label = 'VA-6 age deduction phase-out + STA')

  # VA-7: 2020 single age 82 (born 1938, on/before 1/1/1939): flat 12,000
  # age deduction with NO income test at AGI 100,000. VAGI 88,000; std
  # 4,500; exemptions 930 + 800 = 1,730. TI = 81,770 -> tax = 720 +
  # 5.75% x 64,770 = 4,444.275
  run_case('VA', 2020, list(agi = 100000, age1 = 82),
           expect = list(st_agi = 88000, liab_st_iit = 4444.275),
           label = 'VA-7 grandfathered age deduction')

  # VA-8: same as VA-7 but age 70 (income-tested): AFAGI 100,000 excess
  # 50,000 wipes the 12,000 -> no age deduction. TI = 100,000 - 4,500 -
  # 1,730 = 93,770 -> tax = 720 + 5.75% x 76,770 = 5,134.275
  run_case('VA', 2020, list(agi = 100000, age1 = 70),
           expect = list(st_agi = 100000, liab_st_iit = 5134.275),
           label = 'VA-8 age deduction phased to zero')

  # VA-9: 2024 single, AGI 400,000, itemizing federally (coupled): VA
  # itemized = mortgage 20,000 + charity 10,000 + property tax 15,000
  # (uncapped; income taxes excluded via addback). VA Pease: 3% x
  # (400,000 - 323,650) = 2,290.50 (< 80% of 45,000) -> 42,709.50.
  # TI = 400,000 - 42,709.50 - 930 = 356,360.50 -> tax = 720 + 5.75% x
  # 339,360.50 = 20,233.23
  run_case('VA', 2024,
           list(agi = 400000, itemizing = 1, mort_int_item_ded = 20000,
                char_item_ded = 10000, salt_prop = 15000,
                salt_inc_sales = 30000, item_ded = 65000,
                item_ded_ex_limits = 65000, salt_item_ded = 10000),
           expect = list(st_item_ded = 42709.50, liab_st_iit = 20233.229),
           label = 'VA-9 itemized + VA Pease')

  # VA-10: 2023 MFJ, AGI 80,000, wages 40,000 each, 2 care-age deps,
  # care expenses 8,000: deduction capped at 6,000 (2 x 3,000). Std
  # 16,000; exemptions 4 x 930 = 3,720. TI = 54,280 -> schedule tax
  # 2,863.60. STA: each nets 40,000 - 930 = 39,070 > half TI 27,140 ->
  # 2 x T(27,140) = 2 x 1,303.05 -> STA 257.50. Liab = 2,606.10
  run_case('VA', 2023,
           list(filing_status = 2, agi = 80000, wages1 = 40000,
                wages2 = 40000, ei1 = 40000, ei2 = 40000, n_dep = 2,
                dep_age1 = 3, dep_age2 = 6, care_exp = 8000),
           expect = list(st_ded = 22000, st_tax_pre_credit = 2606.10,
                         liab_st_iit = 2606.10),
           label = 'VA-10 care expense deduction + STA')

  # VA-11: age package vs EITC exclusivity. 2024 MFJ (67/60), AGI 22,000,
  # 1 dep, federal EITC 4,000. Package value approx 5.75% x (12,000 +
  # 800) = 736 < best match 20% x 4,000 = 800 -> package forgone: no age
  # deduction, no aged add-on (exemptions 3 x 930 = 2,790). VAGI 22,000 <
  # 23,900 -> tax floored to 0; refundable 15% option (600) beats the
  # nonref 20% (benefit 0). Liab = -600
  run_case('VA', 2024,
           list(filing_status = 2, agi = 22000, age1 = 67, age2 = 60,
                n_dep = 1, dep_age1 = 4, eitc = 4000),
           expect = list(st_age_package_forgone = 1, st_exempt = 2790,
                         st_eitc = 600, liab_st_iit = -600),
           label = 'VA-11 exclusivity: EITC side')

  # VA-12: exclusivity, age side. 2024 single 67, AGI 40,000, federal
  # EITC 500: package value 736 > 20% x 500 = 100 -> package kept, EITC
  # and CLI denied. VAGI = 28,000; TI = 28,000 - 8,500 - 1,730 = 17,770
  # -> tax = 720 + 5.75% x 770 = 764.275
  run_case('VA', 2024, list(agi = 40000, age1 = 67, eitc = 500),
           expect = list(st_age_package_taken = 1, st_eitc = 0,
                         st_agi = 28000, liab_st_iit = 764.275),
           label = 'VA-12 exclusivity: age side')

  # VA-13: 2021 single, AGI 30,000 incl. 5,000 unemployment benefits:
  # fully subtracted. VAGI 25,000; std 4,500; exemption 930. TI = 19,570
  # -> tax = 720 + 5.75% x 2,570 = 867.775
  run_case('VA', 2021, list(agi = 30000, ui = 5000),
           expect = list(st_agi = 25000, liab_st_iit = 867.775),
           label = 'VA-13 UI subtraction')

  #--------------------------------------------------------------------------
  # Utah (TC-40)
  #--------------------------------------------------------------------------

  # UT-1: 2024 single, AGI 60,000, standard deduction 14,600. Taxable =
  # 60,000 -> tax = 2,730. Taxpayer credit: 6% x 14,600 = 876, phase-out
  # .013 x (60,000 - 17,652) = 550.524 -> 325.476.
  # Liab = 2,730 - 325.476 = 2,404.524
  run_case('UT', 2024, list(agi = 60000, std_ded = 14600),
           expect = list(st_agi = 60000, st_ded_credit = 325.476,
                         liab_st_iit = 2404.524),
           label = 'UT-1 taxpayer credit phase-out')

  # UT-2: 2024 MFJ, AGI 40,000, deps ages 2 and 5, std 29,200. Taxpayer
  # credit: 6% x (29,200 + 2 x 2,046) = 1,997.52 minus .013 x (40,000 -
  # 35,304) = 61.048 -> 1,936.472. CTC (2024 ages 1-3): one qualifying
  # child x 1,000, MAGI 40,000 < 54,000 -> 1,000 NONREFUNDABLE.
  # Tax = 1,820 < credits -> liab 0 (not negative)
  run_case('UT', 2024,
           list(agi = 40000, filing_status = 2, n_dep = 2, n_dep_ctc = 2,
                dep_age1 = 2, dep_age2 = 5, std_ded = 29200),
           expect = list(st_ded_credit = 1936.472, st_ctc = 1000,
                         liab_st_iit = 0),
           label = 'UT-2 CTC age band + nonrefundability')

  # UT-3: 2025 single age 75, AGI 40,000 incl. taxable SS 10,000 (NOT
  # subtracted from the base), std 15,000. SS credit = 4.5% x 10,000 = 450
  # (no phase-out below 54,000) beats retirement credit 450 - .025 x
  # (40,000 - 25,000) = 75. Taxpayer credit = 900 - .013 x (40,000 -
  # 18,213) = 616.769. Liab = 1,800 - 450 - 616.769 = 733.231
  run_case('UT', 2025,
           list(agi = 40000, age1 = 75, txbl_ss = 10000, gross_ss = 12000,
                std_ded = 15000),
           expect = list(st_agi = 40000, st_age_credit = 450,
                         liab_st_iit = 733.231),
           label = 'UT-3 SS credit vs retirement credit')

  # UT-4: 2023 MFJ itemizer, AGI 80,000; itemized 30,000 with SALT 10,000
  # of which property 4,000 -> income-tax component 6,000 removed. Newborn
  # (age 0) counts twice: 2 x 1,941 = 3,882. Credit = 6% x (24,000 + 3,882)
  # = 1,672.92 minus .013 x (80,000 - 33,484) = 604.708 -> 1,068.212.
  # No CTC in 2023. Liab = 3,720 - 1,068.212 = 2,651.788
  run_case('UT', 2023,
           list(agi = 80000, filing_status = 2, itemizing = 1,
                item_ded = 30000, item_ded_ex_limits = 30000,
                salt_item_ded = 10000, salt_prop = 4000,
                salt_inc_sales = 6000, n_dep = 1, n_dep_ctc = 1,
                dep_age1 = 0, std_ded = 27700),
           expect = list(st_ded_credit = 1068.212, st_ctc = 0,
                         liab_st_iit = 2651.788),
           label = 'UT-4 itemizer SALT removal + newborn double exemption')

  # UT-5: 2023 EITC W-2 wage cap. HoH, one EITC child, federal EITC 3,000.
  # (a) SE-only (wages 0): UT EITC = min(20% x 3,000, 0) = 0.
  # (b) wage earner: min(600, 15,000) = 600
  run_case('UT', 2023,
           list(agi = 15000, filing_status = 4, n_dep = 1, dep_age1 = 8,
                n_dep_eitc = 1, eitc = 3000, sole_prop = 15000, ei1 = 15000,
                std_ded = 20800),
           expect = list(st_eitc = 0),
           label = 'UT-5a EITC wage cap: SE-only')
  run_case('UT', 2023,
           list(agi = 15000, filing_status = 4, n_dep = 1, dep_age1 = 8,
                n_dep_eitc = 1, eitc = 3000, wages1 = 15000, ei1 = 15000,
                std_ded = 20800),
           expect = list(st_eitc = 600, liab_st_iit = 0),
           label = 'UT-5b EITC wage cap: wage earner')

  # UT-6: 2017 pre-TCJA exemption: MFJ, AGI 50,000, 2 deps, std 12,700.
  # Exemption = 4 x 3,038 = 12,152 (taxpayer+spouse+deps). Credit = 6% x
  # 24,852 = 1,491.12 minus .013 x (50,000 - 27,956) = 286.572 ->
  # 1,204.548. Liab = 2,500 - 1,204.548 = 1,295.452
  run_case('UT', 2017,
           list(agi = 50000, filing_status = 2, n_dep = 2, dep_age1 = 8,
                dep_age2 = 10, std_ded = 12700),
           expect = list(st_ded_credit = 1204.548, liab_st_iit = 1295.452),
           label = 'UT-6 2017 taxpayer/spouse/dep exemption')

  # UT-7: frozen retirement-credit cohort (born on/before 12/31/1952):
  # 2024 single age 72 (born 1952) gets 450; age 71 (born 1953) gets 0
  run_case('UT', 2024, list(agi = 20000, age1 = 72, std_ded = 14600),
           expect = list(st_age_credit = 450),
           label = 'UT-7a retirement credit cohort: eligible at 72')
  run_case('UT', 2024, list(agi = 20000, age1 = 71, std_ded = 14600),
           expect = list(st_age_credit = 0),
           label = 'UT-7b retirement credit cohort: ineligible at 71')

  #--------------------------------------------------------------------------
  # Ohio (IT 1040)
  #--------------------------------------------------------------------------

  # OH-1: 2024 single, OAGI 60,000. Exemption tier 2 (40-80k) = 2,150 ->
  # TI = 57,850 -> tax = 360.69 + 2.75% x 31,800 = 1,235.19. $20 credit
  # denied (base 57,850 > 30,000). Liab = 1,235.19
  run_case('OH', 2024, list(agi = 60000),
           expect = list(st_agi = 60000, st_exempt = 2150,
                         liab_st_iit = 360.69 + 0.0275 * 31800),
           label = 'OH-1 base-amount schedule')

  # OH-2: zero-bracket cliff, 2024 single. OAGI 28,000: TI = 25,600 <=
  # 26,050 -> tax 0. OAGI 30,000: TI = 27,600 -> tax = 360.69 + 2.75% x
  # 1,550 = 403.315; $20 credit allowed (27,600 < 30,000) -> liab 383.315
  run_case('OH', 2024, list(agi = 28000),
           expect = list(liab_st_iit = 0, st_filer = 1),
           label = 'OH-2a zero-bracket: no tax')
  run_case('OH', 2024, list(agi = 30000),
           expect = list(st_exempt_credit = 20,
                         liab_st_iit = 360.69 + 0.0275 * 1550 - 20),
           label = 'OH-2b zero-bracket cliff + $20 credit')

  # OH-3: 2025 internal discontinuity at 100,000. Single OAGI 105,000:
  # exemption 1,900 -> TI = 103,100 -> tax = 2,394.32 (statutory base, not
  # the 2,375.63 continuation) + 3.125% x 3,100 = 2,491.195
  run_case('OH', 2025, list(agi = 105000),
           expect = list(liab_st_iit = 2394.32 + 0.03125 * 3100),
           label = 'OH-3 2025 statutory base jump at 100k')

  # OH-4: 2024 MFJ, both 67. Wages 30,000 + 20,000, pensions 6,000,
  # taxable SS 15,000 (fully subtracted): OAGI = 56,000. Exemptions
  # 2 x 2,150 = 4,300 -> TI = 51,700 -> tax = 360.69 + 2.75% x 25,650 =
  # 1,066.065. Retirement credit 130 (income 6,000 in the 5-8k band;
  # 51,700 < 100k), senior 50. $20 credit denied. JFC: remaining =
  # 886.065, tier 50-75k -> 10% = 88.6065 (< 650 cap).
  # Liab = 886.065 - 88.6065 = 797.4585
  run_case('OH', 2024,
           list(agi = 71000, filing_status = 2, age1 = 67, age2 = 67,
                wages1 = 30000, wages2 = 20000, ei1 = 30000, ei2 = 20000,
                txbl_ss = 15000, gross_ss = 18000, txbl_pens_dist = 6000),
           expect = list(st_agi = 56000, st_retire_credit = 130,
                         st_senior_credit = 50, st_jfc = 88.6065,
                         liab_st_iit = 797.4585),
           label = 'OH-4 retirement/senior/JFC ordering')

  # OH-5: business income deduction. 2024 single, Schedule C 400,000:
  # BID = 250,000 -> OAGI = 150,000; excess 150,000. Exemption: MAGI =
  # 400,000 -> 1,900 -> TI = 148,100, all business (nonbusiness income is
  # zero, exemptions offset business per 5747.02(A)(4)(b)).
  # Tax = 3% x 148,100 = 4,443; no credits (MAGI-based tests fail)
  run_case('OH', 2024, list(agi = 400000, sole_prop = 400000, ei1 = 400000),
           expect = list(st_agi = 150000, st_bid = 250000,
                         st_bus_excess = 150000,
                         liab_st_iit = 0.03 * 148100),
           label = 'OH-5 BID carve-out + flat 3%')

  # OH-6: 2018 EITC limitation. Single, 2 deps, OAGI 30,000, federal EITC
  # 5,000. Exemptions 3 x 2,350 = 7,050 -> TI = 22,950 -> tax = 323.41 +
  # 2.969% x 1,200 = 359.038. $20 credit: 22,950 < 30,000 -> 60. Tax base
  # 22,950 > 20,000 -> EITC = min(10% x 5,000, 50% x (359.038 - 60)) =
  # 149.519. Liab = 359.038 - 60 - 149.519 = 149.519
  run_case('OH', 2018,
           list(agi = 30000, n_dep = 2, dep_age1 = 8, dep_age2 = 10,
                n_dep_eitc = 2, eitc = 5000, filing_status = 4),
           expect = list(st_exempt_credit = 60, st_eitc = 149.519,
                         liab_st_iit = 149.519),
           label = 'OH-6 2018 EITC 50%-limitation')

  # OH-7: 2021 post-HB 110 top rate. Single OAGI 300,000: exemption 1,900
  # -> TI = 298,100 -> tax = 3,123.05 + 3.99% x 187,450 = 10,602.305
  run_case('OH', 2021, list(agi = 300000),
           expect = list(liab_st_iit = 3123.05 + 0.0399 * 187450),
           label = 'OH-7 2021 top-bracket elimination')

  # OH-8: 2026 flat schedule. Single OAGI 50,000: exemption 2,150 ->
  # TI = 47,850 -> tax = 332 + 2.75% x 21,800 = 931.50
  run_case('OH', 2026, list(agi = 50000),
           expect = list(liab_st_iit = 332 + 0.0275 * 21800),
           label = 'OH-8 2026 flat 2.75%')

  # OH-9: 2017 CDCTC tiers (OAGI base pre-2019). HoH, 1 dep, OAGI 30,000,
  # federal CDCTC 1,200: 25% tier (20k <= OAGI < 40k) -> 300. Exemptions
  # 2 x 2,300 = 4,600 -> TI = 25,400 -> tax = 317.48 + 2.969% x 4,050 =
  # 437.725. $20 credit: 25,400 < 30,000 -> 40.
  # Liab = 437.725 - 300 - 40 = 97.725
  run_case('OH', 2017,
           list(agi = 30000, filing_status = 4, n_dep = 1, dep_age1 = 4,
                care_exp = 3000, cdctc_nonref = 1200),
           expect = list(st_cdctc = 300, st_exempt_credit = 40,
                         liab_st_iit = 97.725),
           label = 'OH-9 2017 CDCTC tiers + $20 credit')

  #--------------------------------------------------------------------------
  # Pennsylvania (PA-40) -- own-base state: eight income classes, class-level
  # loss floors, flat 3.07%, Tax Forgiveness (Schedule SP)
  #--------------------------------------------------------------------------

  # PA-1: 2024 single, wages 50,000. Own base = 50,000 x 3.07% = 1,535
  run_case('PA', 2024, list(wages1 = 50000),
           expect = list(st_agi = 50000, liab_st_iit = 1535),
           label = 'PA-1 basic flat rate')

  # PA-2: 2024 single, class-level loss floors. Wages 60,000; business loss
  # -20,000 (floored); rents +5,000; capital loss -3,000 (floored).
  # Base = 65,000 x 3.07% = 1,995.50
  run_case('PA', 2024,
           list(wages1 = 60000, sole_prop = -20000, rent = 5000, kg_lt = -3000),
           expect = list(st_agi = 65000, liab_st_iit = 65000 * 0.0307),
           label = 'PA-2 no cross-class loss offset')

  # PA-3: 2024 MFJ retirees. Pensions 40,000, taxable SS 20,000, UI 5,000 all
  # PA-exempt; wages 10,000 -> base 10,000, tax 307. Eligibility income
  # 10,000 <= 13,000 (married, 0 dep) -> 100% forgiveness -> liab 0
  run_case('PA', 2024,
           list(filing_status = 2, age1 = 68, age2 = 66, wages1 = 10000,
                txbl_pens_dist = 40000, txbl_ss = 20000, gross_ss = 25000,
                ui = 5000),
           expect = list(st_agi = 10000, st_forgive_credit = 307,
                         liab_st_iit = 0),
           label = 'PA-3 exclusions + full forgiveness')

  # PA-4: 2024 single, wages 7,000. Tax 214.90. Excess over 6,500 = 500 ->
  # 2 steps of 250 -> 80% forgiveness (SP Table 1: 7,000 is in the 80%
  # column). Credit 171.92; liab 42.98
  run_case('PA', 2024, list(wages1 = 7000),
           expect = list(st_forgive_credit = 214.90 * 0.8,
                         liab_st_iit = 214.90 * 0.2),
           label = 'PA-4 forgiveness step-down')

  # PA-5: 2024 MFJ, 2 deps, wages 30,000, exempt interest 2,000. Base =
  # 30,000 + 25% x 2,000 (other-state muni share) = 30,500, tax 936.35.
  # Eligibility income = 30,500 + 75% x 2,000 = 32,000 = Table 2 100% limit
  # (13,000 + 2 x 9,500) -> full forgiveness
  run_case('PA', 2024,
           list(filing_status = 2, age2 = 40, n_dep = 2, dep_age1 = 5,
                dep_age2 = 9, wages1 = 30000, exempt_int = 2000),
           expect = list(st_agi = 30500, liab_st_iit = 0),
           label = 'PA-5 forgiveness at exact limit w/ exempt interest')

  # PA-6: CDCTC enhancement. MFJ, 1 dep, wages 40,000, federal CDCTC 600:
  # tax 1,228; no forgiveness (40,000 > 22,500 + 2,250). 2023+: 100% match
  # -> refundable 600 -> liab 628. 2022: 30% match -> 180 -> liab 1,048
  run_case('PA', 2023,
           list(filing_status = 2, age2 = 40, n_dep = 1, dep_age1 = 4,
                wages1 = 25000, wages2 = 15000, care_exp = 3000,
                cdctc_nonref = 600),
           expect = list(st_cdctc = 600, liab_st_iit = 1228 - 600),
           label = 'PA-6a 2023 CDCTC 100%')
  run_case('PA', 2022,
           list(filing_status = 2, age2 = 40, n_dep = 1, dep_age1 = 4,
                wages1 = 25000, wages2 = 15000, care_exp = 3000,
                cdctc_nonref = 600),
           expect = list(st_cdctc = 180, liab_st_iit = 1228 - 180),
           label = 'PA-6b 2022 CDCTC 30%')

  # PA-7: Working Pennsylvanians Tax Credit (TY2025+): 10% of federal EITC,
  # refundable. Single, 1 dep, wages 20,000, federal EITC 2,000: tax 614, no
  # forgiveness (20,000 > 16,000 + 2,250). 2025: liab = 614 - 200 = 414;
  # 2024: no WPTC -> 614
  run_case('PA', 2025,
           list(n_dep = 1, n_dep_eitc = 1, dep_age1 = 8, wages1 = 20000,
                ei1 = 20000, eitc = 2000),
           expect = list(st_eitc = 200, liab_st_iit = 614 - 200),
           label = 'PA-7a 2025 WPTC')
  run_case('PA', 2024,
           list(n_dep = 1, n_dep_eitc = 1, dep_age1 = 8, wages1 = 20000,
                ei1 = 20000, eitc = 2000),
           expect = list(st_eitc = 0, liab_st_iit = 614),
           label = 'PA-7b 2024 no WPTC')

  #--------------------------------------------------------------------------
  # Idaho (Form 40 / 39R) -- federal-taxable start, SALT addback, four rate
  # regimes incl. the 2023+ indexed-zero-bracket flat tax, grocery credit,
  # $205 CTC, $10 Permanent Building Fund excise
  #--------------------------------------------------------------------------

  # ID-1: 2017 single, federal taxable income 50,000 (7-bracket schedule):
  # schedule tax at 11,043 = 563.198; + 7.4% x 38,957 = 3,446.016. Grocery
  # credit 100 (refundable); PBF +10. Liab = 3,356.016
  run_case('ID', 2017, list(txbl_inc = 50000),
           expect = list(st_percap_credit = 100,
                         liab_st_iit = 3446.016 - 100 + 10),
           label = 'ID-1 2017 seven brackets + grocery + PBF')

  # ID-2: 2020 MFJ, federal taxable 30,000 incl. taxable SS 10,000 (fully
  # subtracted) -> ID taxable 20,000. 2020 married schedule: 35.28 + 98.00 +
  # 113.68 + 145.04 + 176.40 + 6.625% x 4,320 = 854.60. Grocery 2 x 100;
  # PBF 10 -> liab 664.60
  run_case('ID', 2020,
           list(filing_status = 2, age2 = 40, txbl_inc = 30000,
                txbl_ss = 10000, gross_ss = 12000),
           expect = list(st_agi = 20000, liab_st_iit = 854.60 - 200 + 10),
           label = 'ID-2 2020 SS subtraction + married schedule')

  # ID-3: 2024 single itemizer, federal taxable 100,000; capped SALT 10,000
  # of which property 4,000 -> income-tax addback 6,000 (< itemized-over-
  # standard 10,400). Taxable 106,000; flat tax = 5.695% x (106,000 - 4,673).
  # Grocery 120; PBF 10
  run_case('ID', 2024,
           list(txbl_inc = 100000, itemizing = 1, item_ded = 25000,
                item_ded_ex_limits = 25000, salt_item_ded = 10000,
                salt_prop = 4000, salt_inc_sales = 8000, std_ded = 14600),
           expect = list(st_addback = 6000,
                         liab_st_iit = 0.05695 * 101327 - 120 + 10),
           label = 'ID-3 SALT addback + 2024 flat tax')

  # ID-4: 2023 MFJ, 2 kids (5, 8), federal taxable 60,000; care expenses
  # 8,000 capped at 6,000 deduction (2 x 3,000, under lesser earner 20,000).
  # Taxable 54,000; tax = 5.8% x (54,000 - 8,978) = 2,611.276. CTC 2 x 205
  # nonref; grocery 4 x 120; PBF 10 -> liab 1,731.276
  run_case('ID', 2023,
           list(filing_status = 2, age2 = 40, txbl_inc = 60000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 5, dep_age2 = 8, care_exp = 8000,
                ei1 = 40000, ei2 = 20000),
           expect = list(st_ctc = 410, st_percap_credit = 480,
                         liab_st_iit = 0.058 * 45022 - 410 - 480 + 10),
           label = 'ID-4 care deduction + CTC + family grocery')

  # ID-5: 2025 single age 70, federal taxable 40,000 incl. taxable SS
  # 15,000 -> ID taxable 25,000; tax = 5.3% x (25,000 - 4,811) = 1,070.017.
  # Grocery 155 flat (aged add-on eliminated 2025); PBF 10
  run_case('ID', 2025,
           list(age1 = 70, txbl_inc = 40000, txbl_ss = 15000,
                gross_ss = 18000),
           expect = list(st_percap_credit = 155,
                         liab_st_iit = 0.053 * 20189 - 155 + 10),
           label = 'ID-5 2025 flat 5.3% + $155 grocery')

  # ID-6: 2024 single legally blind, federal taxable 3,000 (under the 4,673
  # zero bracket): tax 0; grocery 120 refundable; PBF exempt (blind) ->
  # liab -120
  run_case('ID', 2024, list(blind1 = 1, txbl_inc = 3000),
           expect = list(liab_st_iit = -120),
           label = 'ID-6 refundable grocery + blind PBF exemption')

  # ID-7: 2021 HoH uses the MARRIED schedule (Idaho tax-table convention).
  # Federal taxable 30,000, 1 kid (10): married 2021 schedule tax = 31.76 +
  # 196.85 + 142.92 + 174.68 + 6.5% x 14,122 = 1,464.14. CTC 205; grocery
  # 2 x 100; PBF 10 -> liab 1,069.14
  run_case('ID', 2021,
           list(filing_status = 4, txbl_inc = 30000, n_dep = 1,
                n_dep_ctc = 1, dep_age1 = 10),
           expect = list(liab_st_iit = 1464.14 - 205 - 200 + 10),
           label = 'ID-7 2021 HoH on married schedule + CTC')

  #--------------------------------------------------------------------------
  # Minnesota (Form M1) -- fed-taxable start 2017 / FAGI 2018+, MN standard
  # deduction with the high-income limitation, dependent exemptions, dual
  # SS regimes, WFC/M1CWFC, marriage credit, M1CD cap, NIIT
  #--------------------------------------------------------------------------

  # MN-1: 2024 single, FAGI 50,000 wages. Std 14,575 -> taxable 35,425;
  # tax = 5.35% x 31,690 + 6.80% x 3,735
  run_case('MN', 2024, list(agi = 50000, wages1 = 50000, ei1 = 50000),
           expect = list(st_ded = 14575,
                         liab_st_iit = 31690 * 0.0535 + 3735 * 0.068),
           label = 'MN-1 2024 basic')

  # MN-2: 2017 federal-taxable start + SALT addback. Single itemizer,
  # federal taxable 60,000, SALT income component 8,000 (< item - std) ->
  # MN taxable 68,000; tax = 5.35% x 25,390 + 7.05% x 42,610
  run_case('MN', 2017,
           list(agi = 80000, txbl_inc = 60000, itemizing = 1,
                item_ded = 20000, item_ded_ex_limits = 20000,
                salt_item_ded = 8000, salt_inc_sales = 8000, std_ded = 6350),
           expect = list(st_addback = 8000,
                         liab_st_iit = 25390 * 0.0535 + 42610 * 0.0705),
           label = 'MN-2 2017 taxable-income start + SALT addback')

  # MN-3: 2018 nonconformity stack: TCJA FAGI + MN pre-TCJA deductions.
  # MFJ, FAGI 100,000, 2 deps: std 13,000 + exemptions 4 x 4,150 = 16,600
  # -> taxable 70,400; tax = 5.35% x 37,850 + 7.05% x 32,550
  run_case('MN', 2018,
           list(agi = 100000, filing_status = 2, age2 = 40, n_dep = 2,
                dep_age1 = 5, dep_age2 = 9, wages1 = 100000, ei1 = 100000),
           expect = list(st_ded = 13000, st_exempt = 16600,
                         liab_st_iit = 37850 * 0.0535 + 32550 * 0.0705),
           label = 'MN-3 2018 pre-TCJA stack on FAGI')

  # MN-4: 2021 sliding SS subtraction: MFJ both 70, AGI 60,000 incl.
  # taxable SS 20,000 (gross 25,000). Provisional = 40,000 + 12,500 =
  # 52,500 < 80,270 -> full max 5,290 subtracted. Std 25,050 + 2 x 1,300
  # aged; taxable = 54,710 - 27,650 = 27,060 x 5.35%
  run_case('MN', 2021,
           list(agi = 60000, filing_status = 2, age1 = 70, age2 = 70,
                txbl_ss = 20000, gross_ss = 25000),
           expect = list(st_agi = 60000 - 5290,
                         liab_st_iit = 27060 * 0.0535),
           label = 'MN-4 sliding SS subtraction + aged std add-ons')

  # MN-5: 2024 simplified SS with stepped phase-out: single 68, AGI 90,000
  # incl. taxable SS 30,000. Excess 7,810 over 82,190 -> 2 steps -> 80%
  # share = 24,000 (beats the frozen sliding method's 2,910). Std 14,575 +
  # 1,950 aged; taxable = 66,000 - 16,525 = 49,475
  run_case('MN', 2024,
           list(agi = 90000, age1 = 68, txbl_ss = 30000, gross_ss = 35000),
           expect = list(st_agi = 66000,
                         liab_st_iit = 31690 * 0.0535 + 17785 * 0.068),
           label = 'MN-5 simplified SS stepped phase-out')

  # MN-6: 2023 two-tier deduction limitation + exemption phase-out. MFJ
  # wages/AGI 340,000, 2 deps. Std reduction = 3% x 84,320 + 10% x 35,030
  # = 6,032.60 (< 80% cap) -> std 21,617.40. Exemptions 9,600 reduced 8%
  # (4 steps x 2%) -> 8,832. Taxable 309,550.60
  run_case('MN', 2023,
           list(agi = 340000, filing_status = 2, age2 = 40, n_dep = 2,
                dep_age1 = 5, dep_age2 = 9, wages1 = 340000, ei1 = 340000),
           expect = list(st_ded = 27650 - 6032.6, st_exempt = 8832,
                         liab_st_iit = 43950 * 0.0535 + 130660 * 0.068 +
                                       130360 * 0.0785 + 4580.6 * 0.0985),
           label = 'MN-6 two-tier limitation + exemption phase-out')

  # MN-7: 2021 Working Family Credit: single, 2 kids, earned = AGI =
  # 15,000: 11% phase-in -> 1,650 (< max 2,213, below phase-out).
  # Deductions/exemptions zero the tax; refundable credit nets -1,650
  run_case('MN', 2021,
           list(agi = 15000, wages1 = 15000, ei1 = 15000, n_dep = 2,
                n_dep_eitc = 2, dep_age1 = 8, dep_age2 = 10),
           expect = list(st_earned_credit = 1650, liab_st_iit = -1650),
           label = 'MN-7 WFC triangular schedule')

  # MN-8: 2024 M1CWFC: MFJ, 2 kids under 18, earned = AGI = 50,000:
  # 2 x 1,750 + 4% x 9,220 = 3,868.80, less 12% x (50,000 - 36,880) =
  # 1,574.40 -> 2,294.40 refundable. Taxable = 50,000 - 29,150 - 10,100
  # = 10,750 -> tax 575.125
  run_case('MN', 2024,
           list(agi = 50000, filing_status = 2, age2 = 40, n_dep = 2,
                n_dep_ctc = 2, n_dep_eitc = 2, dep_age1 = 5, dep_age2 = 8,
                wages1 = 30000, wages2 = 20000, ei1 = 30000, ei2 = 20000),
           expect = list(st_ctc = 2294.4,
                         liab_st_iit = 10750 * 0.0535 - 2294.4),
           label = 'MN-8 M1CWFC combined credit')

  # MN-9: 2022 marriage credit: MFJ earned 60,000/40,000, AGI 100,000.
  # Taxable 74,200; share1 = 40,000 - 12,900 = 27,100 (single tax
  # 1,449.85), share2 = 47,100 (single tax 2,795.64); joint tax 4,450.375
  # -> credit 204.885, nonrefundable
  run_case('MN', 2022,
           list(agi = 100000, filing_status = 2, age2 = 40,
                wages1 = 60000, wages2 = 40000, ei1 = 60000, ei2 = 40000),
           expect = list(st_marriage_credit = 204.885,
                         liab_st_iit = 4450.375 - 204.885),
           label = 'MN-9 marriage credit')

  # MN-10: 2024 NIIT + flat 80% deduction cut: single, AGI 2.1M (NII =
  # 1.65M -> 1% x 650,000 = 6,500); AGI > 1,053,750 -> std = 20% x 14,575
  run_case('MN', 2024,
           list(agi = 2100000, wages1 = 450000, ei1 = 450000,
                kg_lt = 1500000, div_ord = 100000, txbl_int = 50000),
           expect = list(st_ded = 0.20 * 14575,
                         liab_st_iit = 31690 * 0.0535 + 72400 * 0.068 +
                                       89150 * 0.0785 + 1903845 * 0.0985 +
                                       6500),
           label = 'MN-10 NIIT + flat 80% limitation')

  # MN-11: 2023 dependent care cap: MFJ AGI 70,000, federal CDCTC 1,200:
  # cap = 1,200 - 5% x 10,790 = 660.50. M1CWFC fully phased out at this
  # income (3,850 - 4,200 < 0). Taxable = 70,000 - 27,650 - 9,600
  run_case('MN', 2023,
           list(agi = 70000, filing_status = 2, age2 = 40, n_dep = 2,
                n_dep_ctc = 2, n_dep_eitc = 2, dep_age1 = 5, dep_age2 = 8,
                wages1 = 40000, wages2 = 30000, ei1 = 40000, ei2 = 30000,
                care_exp = 6000, cdctc_nonref = 1200),
           expect = list(st_cdctc = 660.5, st_ctc = 0,
                         liab_st_iit = 32750 * 0.0535 - 660.5),
           label = 'MN-11 dependent care cap')

  #--------------------------------------------------------------------------
  # Maryland (Form 502, state lines only) -- 15%-of-AGI std deduction,
  # banded exemptions, pension exclusion less gross SS, EITC options,
  # CTC, senior credit, 2025 brackets + capital-gains surtax
  #--------------------------------------------------------------------------

  # MD-1: 2024 single, FAGI 50,000. Std = clamp(7,500; 1,800-2,700) =
  # 2,700; exemption 3,200 -> taxable 44,100; tax = 90 + 4.75% x 41,100
  run_case('MD', 2024, list(agi = 50000, wages1 = 50000, ei1 = 50000),
           expect = list(st_ded = 2700, st_exempt = 3200,
                         liab_st_iit = 90 + 0.0475 * 41100),
           label = 'MD-1 percent std deduction')

  # MD-2: 2019 pension exclusion less GROSS SS: single 70, pension 30,000,
  # gross SS 20,000 (taxable 10,000), FAGI 40,000. Exclusion = min(30,000,
  # 31,100 - 20,000) = 11,100; MD AGI = 18,900; std = 2,250 (max);
  # exemptions 3,200 + 1,000 aged -> taxable 12,450
  run_case('MD', 2019,
           list(agi = 40000, age1 = 70, txbl_pens_dist = 30000,
                txbl_ss = 10000, gross_ss = 20000),
           expect = list(st_agi = 18900,
                         liab_st_iit = 90 + 0.0475 * 9450),
           label = 'MD-2 pension exclusion with gross-SS offset')

  # MD-3: 2023 exemption bands: MFJ FAGI 160,000, 2 deps -> $1,600 per
  # exemption (150-175k band) x 4 = 6,400; std max 5,150; taxable 148,450
  run_case('MD', 2023,
           list(agi = 160000, filing_status = 2, age2 = 40, n_dep = 2,
                dep_age1 = 8, dep_age2 = 10, wages1 = 160000, ei1 = 160000),
           expect = list(st_exempt = 6400, st_ded = 5150,
                         liab_st_iit = 90 + 0.0475 * 145450),
           label = 'MD-3 banded exemption phase-down')

  # MD-4: 2021 childless EITC (18A.1): single 30, federal EIC 1,000 ->
  # 100% capped at 530, refundable. FAGI 15,000: taxable 9,550, tax
  # 401.125 -> liab -128.875
  run_case('MD', 2021,
           list(agi = 15000, age1 = 30, wages1 = 15000, ei1 = 15000,
                eitc = 1000),
           expect = list(st_eitc = 530,
                         liab_st_iit = (90 + 0.0475 * 6550) - 530),
           label = 'MD-4 childless EITC capped')

  # MD-5: 2023 EITC option choice: HoH, 2 kids, federal EIC 6,000, FAGI
  # 25,000. Tax 500.875; alt (45% refundable = 2,700) beats the capped
  # nonrefundable benefit -> refundable 2,700. CTC zero (FAGI > 15,000)
  run_case('MD', 2023,
           list(agi = 25000, filing_status = 4, n_dep = 2, n_dep_eitc = 2,
                n_dep_ctc = 2, dep_age1 = 3, dep_age2 = 5, wages1 = 25000,
                ei1 = 25000, eitc = 6000),
           expect = list(st_eitc = 2700, st_ctc = 0,
                         liab_st_iit = (90 + 0.0475 * 8650) - 2700),
           label = 'MD-5 refundable EITC option + CTC cliff')

  # MD-6: 2023 CTC eligible: single, 1 child age 2, FAGI 12,000 <= 15,000
  # -> $500 refundable; EITC alt 45% x 3,000 = 1,350 beats min(tax, 1,500)
  run_case('MD', 2023,
           list(agi = 12000, n_dep = 1, n_dep_eitc = 1, n_dep_ctc = 1,
                dep_age1 = 2, wages1 = 12000, ei1 = 12000, eitc = 3000),
           expect = list(st_ctc = 500, st_eitc = 1350,
                         liab_st_iit = (90 + 0.0475 * 800) - 1350 - 500),
           label = 'MD-6 CTC under-6')

  # MD-7: 2022 senior credit, one-65+ joint tier: MFJ 70/62, FAGI 80,000
  # -> $1,000 (not 1,750); std 4,850, exemptions 6,400 + 1,000 aged
  run_case('MD', 2022,
           list(agi = 80000, filing_status = 2, age1 = 70, age2 = 62,
                wages1 = 80000, ei1 = 80000),
           expect = list(st_senior_credit = 1000,
                         liab_st_iit = (90 + 0.0475 * 64750) - 1000),
           label = 'MD-7 senior credit one-65 tier')

  # MD-8: 2025 new brackets + 2% capital-gains surtax: single FAGI 1.5M
  # (300k wages + 1.2M LTCG); flat std 3,350; exemptions 0. Tax at
  # 1,496,650 = 58,385 + 6.5% x 496,650; surtax 2% x 1.2M
  run_case('MD', 2025,
           list(agi = 1500000, wages1 = 300000, ei1 = 300000,
                kg_lt = 1200000),
           expect = list(st_ded = 3350,
                         liab_st_iit = 58385 + 0.065 * 496650 +
                                       0.02 * 1200000),
           label = 'MD-8 2025 brackets + capital-gains surtax')

  # MD-9: 2019 two-income subtraction + itemized: MFJ 60k/40k wages,
  # itemizers with 20,000 of MD-allowed components (mortgage 8,000 +
  # charity 6,000 + property 6,000); MD AGI = 100,000 - 1,200; itemized
  # beats std 4,550; taxable 72,400
  run_case('MD', 2019,
           list(agi = 100000, filing_status = 2, age2 = 40, wages1 = 60000,
                wages2 = 40000, ei1 = 60000, ei2 = 40000, itemizing = 1,
                item_ded = 24000, item_ded_ex_limits = 24000,
                salt_item_ded = 10000, salt_prop = 6000,
                salt_inc_sales = 7000, mort_int_item_ded = 8000,
                char_item_ded = 6000, std_ded = 24400),
           expect = list(st_agi = 98800, st_ded = 20000,
                         liab_st_iit = 90 + 0.0475 * 69400),
           label = 'MD-9 two-income subtraction + itemized')

  #--------------------------------------------------------------------------
  # Wisconsin (Form 1) -- sliding standard deduction (HoH single-floor),
  # 30% LTCG exclusion, itemized-deduction credit, married couple credit,
  # child-keyed EITC, school property tax credit, $5k retirement exclusion
  #--------------------------------------------------------------------------

  # WI-1: 2024 single, WI income 50,000. SD = 13,230 - 12% x 30,930 =
  # 9,518.40; exemption 700 -> taxable 39,781.60
  run_case('WI', 2024, list(agi = 50000, wages1 = 50000, ei1 = 50000),
           expect = list(st_ded = 13230 - 0.12 * 30930,
                         liab_st_iit = 14320 * 0.035 + 14320 * 0.044 +
                                       (39781.6 - 28640) * 0.053),
           label = 'WI-1 sliding standard deduction')

  # WI-2: 2024 HoH floors at the single schedule: WI income 80,000 -> HoH
  # slide 3,371.61 < single slide 5,918.40 -> SD 5,918.40; exemptions
  # 2 x 700; single rate schedule
  run_case('WI', 2024,
           list(agi = 80000, filing_status = 4, n_dep = 1, dep_age1 = 10,
                wages1 = 80000, ei1 = 80000),
           expect = list(st_ded = 13230 - 0.12 * 60930,
                         liab_st_iit = 14320 * 0.035 + 14320 * 0.044 +
                                       (80000 - 5918.4 - 1400 - 28640) * 0.053),
           label = 'WI-2 HoH single-schedule floor')

  # WI-3: 2022 30% LTCG exclusion: 40,000 wages + 20,000 LTCG -> exclusion
  # 6,000; WI income 54,000; SD 7,348.80; taxable 45,951.20
  run_case('WI', 2022,
           list(agi = 60000, wages1 = 40000, ei1 = 40000, kg_lt = 20000),
           expect = list(st_agi = 54000,
                         liab_st_iit = 12760 * 0.0354 + 12760 * 0.0465 +
                                       (45951.2 - 25520) * 0.053),
           label = 'WI-3 30% capital gain exclusion')

  # WI-4: 2024 itemized-deduction credit + married couple credit: MFJ
  # 60k/30k wages, charitable 8,000 + mortgage 9,000. SD 12,132.71;
  # credit = 5% x (17,000 - 12,132.71); married couple = min(3% x 30,000,
  # 480) = 480
  run_case('WI', 2024,
           list(agi = 90000, filing_status = 2, age2 = 40, wages1 = 60000,
                wages2 = 30000, ei1 = 60000, ei2 = 30000,
                char_item_ded = 8000, mort_int_item_ded = 9000),
           expect = list(st_item_credit = 0.05 * (17000 - (24490 - 0.19778 * 62480)),
                         st_twoearner_credit = 480,
                         liab_st_iit = 19090 * 0.035 + 19100 * 0.044 +
                                       (90000 - (24490 - 0.19778 * 62480) -
                                        1400 - 38190) * 0.053 -
                                       0.05 * (17000 - (24490 - 0.19778 * 62480)) -
                                       480),
           label = 'WI-4 itemized credit + married couple credit')

  # WI-5: 2021 EITC by child count: HoH, 2 kids, federal EIC 5,000 ->
  # 11% = 550 refundable. SD (HoH slide binds) 13,603.17; taxable
  # 4,296.83 at 3.54%
  run_case('WI', 2021,
           list(agi = 20000, filing_status = 4, n_dep = 2, n_dep_eitc = 2,
                dep_age1 = 6, dep_age2 = 9, wages1 = 20000, ei1 = 20000,
                eitc = 5000),
           expect = list(st_eitc = 550,
                         liab_st_iit = (20000 - (14470 - 0.22515 * 3850) -
                                        2100) * 0.0354 - 550),
           label = 'WI-5 child-keyed EITC')

  # WI-6: 2019 one-time rates + school property tax credit: single WI
  # income 40,000, property taxes 3,000 -> credit min(360, 300) = 300;
  # SD 7,939.20; taxable 31,360.80 at 3.86/5.04/6.27
  run_case('WI', 2019,
           list(agi = 40000, wages1 = 40000, ei1 = 40000, salt_prop = 3000),
           expect = list(liab_st_iit = 11760 * 0.0386 + 11760 * 0.0504 +
                                       (31360.8 - 23520) * 0.0627 - 300),
           label = 'WI-6 2019 rates + school property tax credit')

  # WI-7: 2019 $5,000 retirement exclusion at 65+ under the FAGI cliff:
  # single 70, pension 14,000 (FAGI < 15,000) -> WI income 9,000; SD
  # above income -> zero tax
  run_case('WI', 2019,
           list(agi = 14000, age1 = 70, txbl_pens_dist = 14000),
           expect = list(st_agi = 9000, liab_st_iit = 0),
           label = 'WI-7 retirement exclusion cliff')

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

  for (st in c('IL', 'CO', 'NY', 'AZ', 'GA', 'NC', 'IN', 'KY', 'MI', 'CA', 'ND',
               'SC', 'CT', 'VA', 'UT', 'OH', 'PA', 'ID', 'MN', 'MD', 'WI')) {
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
  message('test_state_calc smoke grid: PASSED (', nrow(grid), ' units x 21 states x 5 years)')

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
    part_passive = 0, scorp = 0, farm = 0, rent = 0, other_gains = 0,
    alimony = 0, other_inc = 0,
    sch_e = 0, part_scorp = 0, ei1 = 0, ei2 = 0, n_dep_eitc = 0,
    txbl_pens_dist = 0,
    txbl_ira_dist = 0, ot_ded = 0, char_cash = 0, char_noncash = 0,
    item_ded = 0, item_ded_ex_limits = 0, salt_item_ded = 0,
    salt_inc_sales = 0, salt_prop = 0, salt_pers = 0,
    med_item_ded = 0, mort_int_item_ded = 0, inv_int_item_ded = 0,
    casualty_item_ded = 0, char_item_ded = 0, misc_item_ded = 0,
    other_item_ded = 0, std_ded = 0,
    eitc = 0, ctc_nonref = 0, ctc_ref = 0, cdctc_nonref = 0,
    cdctc_ref = 0, care_exp = 0, ui = 0
  )
  for (v in names(overrides)) {
    unit[[v]] = overrides[[v]]
  }
  as_tibble(unit)
}
