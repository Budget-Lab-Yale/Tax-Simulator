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
                'PA', 'ID', 'MN', 'MD', 'WI', 'KS', 'DE', 'RI', 'WV', 'NM',
                'VT', 'OK', 'DC', 'NE', 'HI', 'ME'),
    years   = 2017:2035,
    indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
              mutate(growth = 0.025)
  )
  credit_tables = attr(law, 'credit_tables')

  # Credit-family output columns for the worksheet-coverage layer (code
  # review 2026-07-17 item #9a): run_case records which of these each
  # state's hand-computed cases actually move
  coverage_outputs = c(
    'st_hh_credit', 'st_eitc', 'st_ctc', 'st_dep_credit', 'st_cdctc',
    'st_family_credit', 'st_exempt_credit', 'st_earned_credit', 'st_yctc',
    'st_pct_credit', 'st_cli', 'st_ded_credit', 'st_age_credit',
    'st_retire_credit', 'st_senior_credit', 'st_jfc', 'st_forgive_credit',
    'st_percap_credit', 'st_marriage_credit', 'st_twoearner_credit',
    'st_item_credit', 'st_stfc'
  )
  case_exercised = new.env()
  case_exercised$sets = list()

  # law_overrides sets named law parameters on top of the state's own row. It
  # exists to exercise GENERIC machinery that no encoded state uses yet: a new
  # parameter is only proved neutral by the rest of the suite, and proving it
  # WORKS otherwise has to wait for the first state that consumes it. Every
  # overridden name must be a legal parameter name (scalar or vector-family
  # member), so a typo or a renamed parameter fails loudly rather than
  # silently testing nothing.
  run_case = function(st, yr, unit_overrides, expect, tol = 0.01, label = '',
                      law_overrides = list()) {

    unit = st_test_unit(unit_overrides)
    law_row = law %>%
      filter(state == st, year == yr,
             filing_status == unit$filing_status) %>%
      select(-state, -year, -filing_status)
    stopifnot('law row missing' = nrow(law_row) == 1)

    if (length(law_overrides) > 0) {
      # Validate against the schema registry rather than the law row: a
      # parameter no state encodes yet is legitimately absent from the row
      # (ensure_st_params supplies it downstream), but a name that is not in
      # the registry at all is a typo or a rename and must fail loudly
      registry = st_param_name_registry()
      unknown  = keep(names(law_overrides),
                      ~ !(.x %in% registry$scalars) &&
                        !any(str_detect(.x, registry$families)))
      if (length(unknown) > 0) {
        stop(sprintf('%s: law_overrides names not in params_schema: %s',
                     label, paste(unknown, collapse = ' ')))
      }
      for (p in names(law_overrides)) {
        law_row[[p]] = law_overrides[[p]]
      }
    }

    result = unit %>%
      bind_cols(law_row) %>%
      do_state_taxes(
        credit_tables = state_credit_tables_for_year(credit_tables, st, yr)
      )

    nz = coverage_outputs[map_lgl(coverage_outputs,
                                  ~ isTRUE(abs(result[[.x]][1]) > 1e-9))]
    case_exercised$sets[[st]] = union(case_exercised$sets[[st]], nz)

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

  # IL-5: 2025 child tax credit (P.A. 103-0592): 40% of the IL EITC with a
  # qualifying child under 12. Federal EITC 3,000 -> IL EITC 20% = 600 ->
  # IL CTC 0.40 x 600 = 240
  run_case('IL', 2025,
           list(agi = 20000, n_dep = 1, dep_age1 = 5, eitc = 3000),
           expect = list(st_eitc = 600, st_ctc = 240),
           label = 'IL-5 child credit as share of EITC')

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

  # NY-5: 2024 dependent-care credit (IT-216 Worksheet 1 share table): at
  # NYAGI 45,000 the share segment [40,000, 50,000) is flat at 1.00, so the
  # NY credit equals the federal credit (500)
  run_case('NY', 2024,
           list(agi = 45000, n_dep = 1, dep_age1 = 4, cdctc_nonref = 500,
                care_exp = 2500),
           expect = list(st_cdctc = 500),
           label = 'NY-5 dependent-care share table')

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

  # GA-4: the married-filing-joint personal exemption is $7,400 TOTAL
  # (O.C.G.A. 48-7-26, HB 386 of 2012, through the 2024 elimination).
  # 2019 MFJ wages 60,000/40,000: taxable 100,000 - 6,000 - 7,400 =
  # 86,600 -> 340 + 5.75% x 76,600 = 4,744.50. TAXSIM agrees exactly
  run_case('GA', 2019,
           list(agi = 100000, filing_status = 2, age2 = 40,
                wages1 = 60000, wages2 = 40000, ei1 = 60000, ei2 = 40000),
           expect = list(st_exempt = 7400, liab_st_iit = 4744.50),
           label = 'GA-4 $7,400 joint exemption')

  # GA-5: the HB 593 standard deduction increase (4,600 -> 5,400 single) is
  # effective for taxable years beginning on or after 1/1/2022 -- TY2021
  # keeps the 2018 amounts. Taxable 50,000 - 4,600 - 2,700 = 42,700 ->
  # 230 + 5.75% x 35,700 = 2,282.75. PE 1.775.7 agrees on the 2021 vintage.
  run_case('GA', 2021, list(agi = 50000),
           expect = list(st_std_ded = 4600, liab_st_iit = 2282.75),
           label = 'GA-5 2021 pre-HB593 standard deduction')

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

  # NC-5: TY2017 credit for children (105-153.10, repealed TY2018+): MFJ
  # AGI 50,000 (above the $40k additional-credit band, below the $100k
  # cutoff) with two CTC-qualifying children -> $100 x 2. Taxable
  # 50,000 - 17,500 = 32,500 x 5.499% = 1,787.175 - 200
  run_case('NC', 2017,
           list(agi = 50000, filing_status = 2, age2 = 40, n_dep = 2,
                dep_age1 = 5, dep_age2 = 8, std_ded = 12700),
           expect = list(st_ctc = 200, liab_st_iit = 1587.175),
           label = 'NC-5 2017 child credit tiers')

  # NC-5b: the 2018+ standard deduction stays at the 2017 amounts (the
  # S.L. 2018-5 increase is effective TY2019). MFJ 2018: std 17,500
  run_case('NC', 2018,
           list(agi = 50000, filing_status = 2, age2 = 40, std_ded = 24000),
           expect = list(st_std_ded = 17500,
                         liab_st_iit = (50000 - 17500) * 0.05499),
           label = 'NC-5b 2018 pre-SB99 standard deduction')

  #--------------------------------------------------------------------------
  # Second broad-IIT rollout states
  #--------------------------------------------------------------------------

  # IN-1: 2025 single, AGI $50,000, one $1,000 personal exemption, and a
  # $1,000 federal EITC. Indiana's 10% refundable match is $100.
  run_case('IN', 2025, list(agi = 50000, eitc = 1000),
           expect = list(st_exempt = 1000, st_eitc = 100,
                         liab_st_iit = 49000 * 0.03 - 100),
           label = 'IN-1 exemptions and refundable EITC')

  # IN-2: dependent-child exemption stack (IC 6-3-1-3.5(a)): $1,000 per
  # exemption (2 taxpayers + 2 dependents) + $1,500 additional per
  # dependent child = 7,000. 2019 MFJ AGI 100,000 ->
  # (100,000 - 7,000) x 3.23% = 3,003.90. TAXSIM agrees exactly
  run_case('IN', 2019,
           list(agi = 100000, filing_status = 2, age2 = 40,
                wages1 = 60000, wages2 = 40000, ei1 = 60000, ei2 = 40000,
                n_dep = 2, dep_age1 = 8, dep_age2 = 10),
           expect = list(st_exempt = 7000,
                         liab_st_iit = 93000 * 0.0323),
           label = 'IN-2 dependent-child exemption stack')

  # IN-3: the $1,000 65+ exemption is universal (only the extra $500
  # under $40,000 AGI is income-tested and remains omitted): 2019 single
  # 70, AGI 50,000 -> (50,000 - 2,000) x 3.23% = 1,550.40. TAXSIM agrees
  run_case('IN', 2019, list(agi = 50000, age1 = 70),
           expect = list(st_exempt = 2000,
                         liab_st_iit = 48000 * 0.0323),
           label = 'IN-3 universal aged exemption')

  # KY-1: 2025 single at MGI $16,000 is in the 90% family-size-credit band.
  # Tax is ($16,000 - $3,270) x 4% = $509.20; the credit is $458.28.
  run_case('KY', 2025, list(agi = 16000),
           expect = list(st_family_credit = 509.2 * 0.9,
                         liab_st_iit = 509.2 * 0.1),
           label = 'KY-1 family-size percentage-of-tax credit')

  # KY-2: 2017 graduated schedule (2/3/4/5/5.8/6%), single, AGI 50,000.
  # Std ded 2,480 -> taxable 47,520 -> $280 + 5.8% x 39,520 = 2,572.16;
  # less the $10 Section B personal credit. TAXSIM-35 reproduces this
  # exactly (siitax 2,562.16)
  run_case('KY', 2017, list(agi = 50000),
           expect = list(st_exempt_credit = 10,
                         liab_st_iit = 2572.16 - 10),
           label = 'KY-2 2017 graduated schedule')

  # KY-3: 2017 top bracket: single, AGI 100,000 -> taxable 97,520 ->
  # $4,166 + 6% x 22,520 = 5,517.20, less $10
  run_case('KY', 2017, list(agi = 100000),
           expect = list(liab_st_iit = 5517.2 - 10),
           label = 'KY-3 2017 6% top bracket')

  # KY-4: 2017 married filing separately on a combined return: wages
  # 40,000/30,000. Each column takes its own 2,480 std ded on the
  # graduated schedule: tax(37,520) + tax(27,520) = 1,992.16 + 1,412.16 =
  # 3,404.32 < joint tax(67,520) = 3,732.16. Less 2 x $10 credits.
  # (TAXSIM shows 3,096.64 here: it deducts twice the 2017 std ded per
  # spouse -- a pre-registered TAXSIM difference, not our target)
  run_case('KY', 2017,
           list(filing_status = 2, age2 = 40, agi = 70000,
                wages1 = 40000, wages2 = 30000),
           expect = list(st_exempt_credit = 20,
                         liab_st_iit = 3404.32 - 20),
           label = 'KY-4 2017 combined-return split')

  # KY-5: 2019 standard deduction vintage (2,590, DOR announcement):
  # single, AGI 30,000 -> (30,000 - 2,590) x 5% = 1,370.50
  run_case('KY', 2019, list(agi = 30000),
           expect = list(st_std_ded = 2590, liab_st_iit = 1370.50),
           label = 'KY-5 2019 std deduction vintage')

  # KY-6: 2020 combined return, flat rate: wages 30,000/20,000, std
  # 2,650 per column: (27,350 + 17,350) x 5% = 2,235.00 < joint
  # (50,000 - 2,650) x 5% = 2,367.50. TAXSIM agrees (2 x std)
  run_case('KY', 2020,
           list(filing_status = 2, age2 = 40, agi = 50000,
                wages1 = 30000, wages2 = 20000),
           expect = list(liab_st_iit = 2235.00),
           label = 'KY-6 combined return two-earner 2x std')

  # KY-7: 2021 one-earner couple: the zero-income spouse's column floors
  # at zero, so the second std ded (2,690) is wasted and combined equals
  # joint: (50,000 - 2,690) x 5% = 2,365.50. (TAXSIM gives the couple
  # both std deductions unconditionally -- pre-registered difference)
  run_case('KY', 2021,
           list(filing_status = 2, age2 = 40, agi = 50000, wages1 = 50000),
           expect = list(liab_st_iit = 2365.50),
           label = 'KY-7 one-earner column floor')

  # KY-8: 2018 pension exclusion: single, AGI 60,000 incl. 40,000
  # pension capped at 31,110 -> state AGI 28,890; (28,890 - 2,530) x 5%
  run_case('KY', 2018, list(agi = 60000, txbl_pens_dist = 40000),
           expect = list(st_agi = 28890,
                         liab_st_iit = (28890 - 2530) * 0.05),
           label = 'KY-8 pension exclusion cap')

  # KY-9: 2023 aged credit ($40, Schedule ITC Section B, unchanged from
  # the 2017 four-box system): single 70, AGI 30,000 ->
  # (30,000 - 2,980) x 4.5% = 1,215.90 - 40
  run_case('KY', 2023, list(agi = 30000, age1 = 70),
           expect = list(st_exempt_credit = 40,
                         liab_st_iit = 1215.90 - 40),
           label = 'KY-9 aged $40 credit')

  # KY-10: 2024 CDCTC match: single, AGI 40,000, federal credit 600 ->
  # (40,000 - 3,160) x 4% = 1,473.60 less 20% x 600 = 120
  run_case('KY', 2024, list(agi = 40000, cdctc_nonref = 600),
           expect = list(st_cdctc = 120, liab_st_iit = 1473.60 - 120),
           label = 'KY-10 CDCTC 20% match')

  # KY-11: 2017 combined-return itemizers: wages 60,000/40,000, federal
  # itemized 20,000 divided by income share (Schedule A rule): columns
  # (60,000 - 12,000) and (40,000 - 8,000) -> 2,600 + 1,672 = 4,272 <
  # joint tax(80,000) = 4,466. Less 2 x $10 credits
  run_case('KY', 2017,
           list(filing_status = 2, age2 = 40, agi = 100000,
                wages1 = 60000, wages2 = 40000, itemizing = 1,
                item_ded = 20000, item_ded_ex_limits = 20000),
           expect = list(st_ded = 20000,
                         liab_st_iit = 4272 - 20),
           label = 'KY-11 combined-return itemized split')

  # MI-1: 2025 single with a $5,800 personal exemption and $1,000 federal
  # EITC. Michigan's refundable EITC is 30% of the federal amount.
  run_case('MI', 2025, list(agi = 50000, eitc = 1000),
           expect = list(st_exempt = 5800, st_eitc = 300,
                         liab_st_iit = (50000 - 5800) * 0.0425 - 300),
           label = 'MI-1 exemption and refundable EITC')

  # MI-2: 2019 Tier 1 (born before 1946 = age 74+): single 75, pension
  # 60,000 capped at the Form 4884 maximum 52,808; senior investment cap
  # (11,771) fully absorbed by the retirement subtraction. AGI 65,000 less
  # the cap and the 15% US-obligation share of 5,000 interest (750);
  # exemption 4,400
  run_case('MI', 2019,
           list(agi = 65000, age1 = 75, txbl_pens_dist = 60000,
                txbl_int = 5000),
           expect = list(st_agi = 65000 - 52808 - 750,
                         liab_st_iit = (65000 - 52808 - 750 - 4400) * 0.0425),
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
  # cap 28,548 less the 20,000 retirement subtraction -> 8,548 (still
  # binding). The 15% US-obligation share of interest (1,500) also comes out
  run_case('MI', 2024,
           list(agi = 60000, filing_status = 2, age1 = 80, age2 = 78,
                wages1 = 2000, txbl_pens_dist = 20000, txbl_int = 10000,
                div_ord = 8000, kg_lt = 20000),
           expect = list(st_agi = 60000 - 20000 - 8548 - 1500),
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

  # CA-11: California's itemization election is independent of the federal
  # one, so a federal standard-deduction taker itemizes for CA from the
  # as-if-itemizing amounts (production shape: as-claimed components zeroed
  # by do_taxes.R, _potential preserved). 2023 MFJ, AGI 500,000: base
  # 8,000 mortgage + 4,000 charity + 6,000 uncapped property tax = 18,000;
  # limitation = lesser of 6% x (500,000 - 474,075) = 1,555.50 and 80% x
  # 18,000 -> itemized 16,444.50 beats the 10,726 standard deduction.
  # Schedule Y tax on 483,555.50 = 38,276.3615; exemption credits 2 x 144
  # phase out by $6 x ceil(25,925/2,500) = 66 each -> 156.
  run_case('CA', 2023,
           list(agi = 500000, filing_status = 2, itemizing = 0,
                mort_int_item_ded_potential = 8000,
                char_item_ded_potential = 4000, salt_prop = 6000),
           expect = list(st_item_ded = 16444.50, st_txbl_inc = 483555.50,
                         liab_st_iit = 38120.36),
           label = 'CA-11 state-only itemization for a federal std-taker')

  # CA-12: unemployment compensation is not taxable in California (Schedule
  # CA Section B line 7 subtraction; R&TC does not conform to IRC 85).
  # 2019 single, AGI 30,000 including 4,000 UI: CA AGI 26,000, standard
  # deduction 4,537, Schedule X tax on 21,463 = 352.77, exemption credit 122.
  run_case('CA', 2019,
           list(agi = 30000, ui = 4000),
           expect = list(st_agi = 26000, st_txbl_inc = 21463,
                         liab_st_iit = 230.77),
           label = 'CA-12 unemployment compensation subtraction')

  # CA-13: fractional income at a table-bin edge. FTB 3514 amounts are
  # entered in whole dollars, so 18,350.63 rounds to 18,351 and lands in
  # the childless 18,351-18,400 bin ($90); the unrounded value falls in the
  # one-dollar crack between bins and wrongly returned zero (2026-08-15).
  run_case('CA', 2019,
           list(agi = 18350.63, wages1 = 18350.63, ei1 = 18350.63),
           expect = list(st_earned_credit = 90),
           label = 'CA-13 CalEITC table lookup rounds income to the bin')

  # CA-14: California does not conform to IRC 223, so the federal HSA
  # deduction is reversed on Schedule CA (column C addition). 2019 single,
  # AGI 50,000 + 3,000 HSA: CA AGI 53,000, std 4,537, Schedule X tax on
  # 48,463 = 1,797.03, exemption credit 122.
  run_case('CA', 2019,
           list(agi = 50000, hsa_contr = 3000),
           expect = list(st_agi = 53000, liab_st_iit = 1675.03),
           label = 'CA-14 HSA deduction added back')

  # CA-15: US-obligation interest is exempt (31 U.S.C. 3124); the model
  # subtracts US_OBLIGATION_INT_SHARE (0.15) of taxable interest. 2019
  # single, AGI 50,000 including 10,000 taxable interest: subtraction
  # 1,500, CA AGI 48,500, Schedule X tax on 43,963 = 1,472.83, credit 122.
  run_case('CA', 2019,
           list(agi = 50000, txbl_int = 10000),
           expect = list(st_agi = 48500, liab_st_iit = 1350.83),
           label = 'CA-15 US-obligation interest share subtracted')

  # CA-16: FTB 3514 Worksheet 1 bars the CalEITC when investment income
  # exceeds the year ceiling ($3,828 in 2019). One-child filer, earned
  # 10,000, ordinary dividends 4,000: credit denied.
  run_case('CA', 2019,
           list(agi = 14000, wages1 = 10000, ei1 = 10000, n_dep = 1,
                n_dep_eitc = 1, div_ord = 4000),
           expect = list(st_earned_credit = 0),
           label = 'CA-16 CalEITC investment-income ceiling')

  # CA-17: FTB 3506 child/dependent care credit: 43% of the federal CDCTC
  # in the $40,001-70,000 federal-AGI tier, nonrefundable. 2023 single,
  # AGI 67,868, federal credit 600: st_cdctc = 258. Schedule X tax on
  # 62,505 (std 5,363) = 2,541.80, less exemption credits (144 personal +
  # 446 dependent) and the 258 care credit.
  run_case('CA', 2023,
           list(agi = 67868, n_dep = 1, dep_age1 = 6, care_exp = 5000,
                cdctc_nonref = 600),
           expect = list(st_cdctc = 258, liab_st_iit = 1693.80),
           label = 'CA-17 CDCTC 43% tier, nonrefundable')

  # CA-17b: tier edges -- 34% through exactly $100,000 of federal AGI,
  # zero above.
  run_case('CA', 2023,
           list(agi = 100000, n_dep = 1, dep_age1 = 6, care_exp = 5000,
                cdctc_nonref = 600),
           expect = list(st_cdctc = 204),
           label = 'CA-17b CDCTC 34% tier upper edge')
  run_case('CA', 2023,
           list(agi = 100001, n_dep = 1, dep_age1 = 6, care_exp = 5000,
                cdctc_nonref = 600),
           expect = list(st_cdctc = 0),
           label = 'CA-17c CDCTC zero above $100,000')

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

  # ND-8: HB 1515 resident tax relief credit ($350/taxpayer, TY2021-22
  # only). Single, taxable 60,000: 1.10% x 40,525 + 2.04% x 19,475 =
  # 843.06 - 350 = 493.06. Nonrefundable: at taxable 17,450 the 191.95
  # of tax floors at zero. Absent in 2020 and 2023 (ND-7 pins 2020)
  run_case('ND', 2021, list(agi = 60000, txbl_inc = 60000),
           expect = list(st_exempt_credit = 350,
                         liab_st_iit = 0.011 * 40525 + 0.0204 * 19475 - 350),
           label = 'ND-8 2021 relief credit')
  run_case('ND', 2022, list(agi = 20000, txbl_inc = 17450),
           expect = list(liab_st_iit = 0),
           label = 'ND-8b relief credit nonrefundable floor')

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

  # SC-4: SS fully exempt. TY2024, $40,000 base incl. $10,000 taxable SS,
  # filer 67: SS subtraction 10,000 + age-65 deduction 15,000 (no
  # retirement deduction claimed, so no (A) offset) -> base 15,000, now in
  # the 3% middle bracket: 3% x (15,000 - 3,460) = 346.20. (Updated
  # 2026-08-11 when the 12-6-1170(B) aged deduction was encoded; the SS
  # subtraction itself is unchanged)
  run_case('SC', 2024,
           list(agi = 40000, txbl_inc = 40000, txbl_ss = 10000, age1 = 67),
           expect = list(st_agi = 15000, liab_st_iit = 346.20),
           label = 'SC-4 full Social Security exemption')

  # SC-5: retirement-income deduction. Age 67: $8,000 pension fully
  # deducted under the $10,000 (A) cap, plus the aged deduction (B) =
  # 15,000 - 8,000 = 7,000 -> base 25,000: 6.2% x 25,000 - 659 = 891.
  # (Updated 2026-08-11 with the 12-6-1170(B) encoding)
  run_case('SC', 2024,
           list(agi = 40000, txbl_inc = 40000, txbl_pens_dist = 8000, age1 = 67),
           expect = list(st_agi = 25000, liab_st_iit = 891.0),
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

  # SC-8: age-65 deduction (12-6-1170(B)): single 70, wages 20,000 +
  # pension 20,000, federal taxable 26,150. Retirement deduction (A) =
  # min(20,000, 10,000 cap); aged deduction (B) = 15,000 - 10,000 = 5,000;
  # SC income 26,150 - 15,000 = 11,150. TAXSIM within $3.20 on this case
  run_case('SC', 2019,
           list(age1 = 70, agi = 40000, txbl_inc = 26150, wages1 = 20000,
                ei1 = 20000, txbl_pens_dist = 20000),
           expect = list(st_agi = 11150),
           label = 'SC-8 age-65 deduction net of retirement deduction')

  # SC-9: both-65+ couple: pensions 40,000, (A) = 2 x 10,000; (B) =
  # 2 x 15,000 - 20,000 = 10,000 -> subtractions 30,000
  run_case('SC', 2019,
           list(filing_status = 2, age1 = 70, age2 = 70, agi = 60000,
                txbl_inc = 50000, txbl_pens_dist = 40000),
           expect = list(st_agi = 20000),
           label = 'SC-9 joint age-65 deductions with (A) offset')

  # SC-10: Two Wage Earner Credit (12-6-3330, Act 266 phase-in): 2019 cap
  # 36,667 -> credit 0.7% x 36,667 = 256.67 (lower earner 40,000 exceeds
  # the cap). TAXSIM produces exactly this value
  run_case('SC', 2019,
           list(filing_status = 2, age2 = 40, agi = 100000,
                txbl_inc = 75600, wages1 = 60000, wages2 = 40000,
                ei1 = 60000, ei2 = 40000),
           expect = list(st_twoearner_credit = 256.67),
           label = 'SC-10 two wage earner credit at the 2019 cap')

  # SC-11: TWEC below the cap: 2024 lower earner 20,000 -> 0.7% x 20,000
  run_case('SC', 2024,
           list(filing_status = 2, age2 = 40, agi = 80000,
                txbl_inc = 50000, wages1 = 60000, wages2 = 20000,
                ei1 = 60000, ei2 = 20000),
           expect = list(st_twoearner_credit = 140),
           label = 'SC-11 two wage earner credit below the cap')

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
  # 20,000 x 0.75 = 55,000 x 0.55 = 30,250. US-obligation share of interest
  # (15% x 30,000 = 4,500) also comes out. CT AGI = 62,750; exemption
  # 24,000 - 15 x 1,000 = 9,000; TI 53,750. Tax = 400 + 4.5% x 33,750 =
  # 1,918.75. Table E: 62,750 in (52,000, 96,000] -> 10% -> 191.875.
  # Property tax credit: min(6,000, 300) x 1.0 (62,750 < 70,500) = 300.
  # Liab = 1,918.75 - 191.875 - 300 = 1,426.875
  run_case('CT', 2025,
           list(agi = 110000, filing_status = 2, age1 = 68, age2 = 66,
                gross_ss = 30000, txbl_ss = 20000, txbl_pens_dist = 40000,
                txbl_ira_dist = 20000, txbl_int = 30000, salt_prop = 6000),
           expect = list(st_agi = 62750, st_exempt = 9000,
                         st_tax_pre_credit = 1918.75,
                         liab_st_iit = 1426.875),
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
  # 1.65M -> 1% x 650,000 = 6,500); AGI > 1,053,750 -> std = 20% x 14,575.
  # The 15% US-obligation share of 50,000 interest (7,500) comes off the
  # base, shrinking the top-bracket slice
  run_case('MN', 2024,
           list(agi = 2100000, wages1 = 450000, ei1 = 450000,
                kg_lt = 1500000, div_ord = 100000, txbl_int = 50000),
           expect = list(st_ded = 0.20 * 14575,
                         liab_st_iit = 31690 * 0.0535 + 72400 * 0.068 +
                                       89150 * 0.0785 + 1896345 * 0.0985 +
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

  # MN-12: M1CWFC childless phase-out at the GENERAL 12% rate (2024 form
  # line 13: 9% only with older children and no young children --
  # verified from the published schedule 2026-08-11). Single, wages
  # 33,000: WFC 4% x 9,220 = 368.80 less 12% x (33,000 - 31,090) =
  # 229.20 -> 139.60
  run_case('MN', 2024,
           list(agi = 33000, txbl_inc = 18400, wages1 = 33000, ei1 = 33000),
           expect = list(st_ctc = 139.60),
           label = 'MN-12 childless M1CWFC 12% phase-out')

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

  # MD-10: dependent-care expense subtraction (Form 502 line 9, encoded
  # via the VA care_exp_ded machinery): MFJ 30k/30k wages, 2 qualifying
  # deps (3, 6), 4,000 of care expenses -> full 4,000 allowed (cap 6,000).
  # MD AGI = 60,000 - 1,200 two-income sub = 58,800; std = 15% capped at
  # 4,550; exemptions 3,200 x 4; taxable 58,800 - 4,550 - 4,000 - 12,800
  # = 37,450 -> 90 + 4.75% x 34,450
  run_case('MD', 2019,
           list(agi = 60000, filing_status = 2, age2 = 40, wages1 = 30000,
                wages2 = 30000, ei1 = 30000, ei2 = 30000, n_dep = 2,
                dep_age1 = 3, dep_age2 = 6, care_exp = 4000,
                std_ded = 24400),
           expect = list(st_agi = 58800, st_ded = 8550,
                         liab_st_iit = 90 + 0.0475 * 34450),
           label = 'MD-10 dependent-care expense subtraction')

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

  # WI-8: 2022 dependent-care credit (2021 Act 58): 50% of the federal
  # CDCTC, nonrefundable. Federal credit 600 -> WI 300
  run_case('WI', 2022,
           list(agi = 40000, wages1 = 40000, ei1 = 40000, n_dep = 1,
                dep_age1 = 4, cdctc_nonref = 600, care_exp = 3000),
           expect = list(st_cdctc = 300),
           label = 'WI-8 dependent-care 50% match')

  # WI-9: net capital loss addback (71.05(10)(c)): the federal return
  # deducts the full $3,000 net loss; WI allows $500/year through 2022,
  # so $2,500 is added back on Schedule I. Single, wages 55,000,
  # kg_lt -3,000 -> FAGI 52,000, WI income 54,500. TAXSIM applies the
  # same add-back (probe-verified 2026-08-11)
  run_case('WI', 2019,
           list(agi = 52000, age1 = 30, wages1 = 55000, ei1 = 55000,
                kg_lt = -3000),
           expect = list(st_additions = 2500, st_agi = 54500),
           label = 'WI-9 $500 capital loss limit addback')

  #--------------------------------------------------------------------------
  # Kansas (K-40) -- SB 30 2017 retroactive schedule, low-income zero-tax
  # cliff via base_amounts, SB 1 2024 restructure, SS $75k cliff, 17% EITC
  #--------------------------------------------------------------------------

  # KS-1: 2017 SB 30 retroactive schedule, single top bracket. TI =
  # 40,000 - 3,000 - 2,250 = 34,750 -> 1,170 + 5.2% x 4,750 = 1,417
  # (2017 worksheet: 5.2% x 34,750 - 390 = 1,417)
  run_case('KS', 2017, list(agi = 40000),
           expect = list(st_agi = 40000, st_txbl_inc = 34750,
                         liab_st_iit = 1417.00),
           label = 'KS-1 2017 SB 30 retroactive schedule')

  # KS-2/KS-2b: the 2017 MFJ zero-tax cliff at $12,500 taxable -- the
  # booklet table gives 0 through 12,500, then taxes the FULL amount
  # (362.50 + 2.9% x 100 = 2.9% x 12,600)
  run_case('KS', 2017, list(filing_status = 2, age2 = 40, agi = 24400),
           expect = list(st_txbl_inc = 12400, liab_st_iit = 0),
           label = 'KS-2 2017 MFJ below the $12,500 zero-tax cliff')
  run_case('KS', 2017, list(filing_status = 2, age2 = 40, agi = 24600),
           expect = list(st_txbl_inc = 12600, liab_st_iit = 365.40),
           label = 'KS-2b 2017 MFJ cliff: full-amount tax at $12,600')

  # KS-3: 2024 SB 1: two brackets + new std/exemptions + 50% CDCTC. TI =
  # 100,000 - 8,240 - (18,320 + 2 x 2,320) = 68,800 -> 2,392 + 5.58% x
  # 22,800 = 3,664.24; less 50% x 1,200 care credit
  run_case('KS', 2024,
           list(filing_status = 2, age1 = 40, age2 = 38, agi = 100000,
                n_dep = 2, dep_age1 = 8, dep_age2 = 10, cdctc_nonref = 1200),
           expect = list(st_txbl_inc = 68800, st_cdctc = 600,
                         liab_st_iit = 3064.24),
           label = 'KS-3 2024 SB 1 two-bracket + new std/exemptions + 50% CDCTC')

  # KS-4/KS-4b: 2021 HoH SS $75k cliff pair + 17% refundable EITC.
  # Under: AGI 25,000 -> subtract taxable SS 5,000; TI = 20,000 - 6,000 -
  # (4,500 HoH base+addl + 2 x 2,250) = 5,000 -> 77.50 + 3.1% x 2,500 =
  # 155; EITC 17% x 5,000 = 850 refundable -> -695.
  # Over: AGI 76,000 -> no subtraction; TI = 61,000 -> 1,252.50 + 5.7% x
  # 31,000 = 3,019.50
  run_case('KS', 2021,
           list(filing_status = 4, agi = 25000, txbl_ss = 5000,
                gross_ss = 6000, n_dep = 2, dep_age1 = 8, dep_age2 = 10,
                ei1 = 20000, wages1 = 20000, eitc = 5000),
           expect = list(st_agi = 20000, st_txbl_inc = 5000, st_eitc = 850,
                         liab_st_iit = -695.00),
           label = 'KS-4 2021 HoH: SS subtraction under the cliff + 17% EITC')
  run_case('KS', 2021,
           list(filing_status = 4, agi = 76000, txbl_ss = 5000,
                gross_ss = 6000, n_dep = 2, dep_age1 = 8, dep_age2 = 10),
           expect = list(st_agi = 76000, liab_st_iit = 3019.50),
           label = 'KS-4b SS subtraction denied above $75,000 (cliff)')

  # KS-5: 2018 SB 30 itemized phase-in shares: federal itemizer with
  # mortgage 10,000 / property tax 4,000 / charity 2,000 -> KS itemized =
  # 0.5 x 10,000 + 0.5 x 4,000 + 2,000 = 9,000 (beats std 3,000). TI =
  # 60,000 - 9,000 - 2,250 = 48,750 -> 1,252.50 + 5.7% x 18,750 = 2,321.25
  run_case('KS', 2018,
           list(agi = 60000, itemizing = 1, item_ded = 26000,
                item_ded_ex_limits = 26000, mort_int_item_ded = 10000,
                salt_prop = 4000, char_item_ded = 2000, salt_item_ded = 10000,
                salt_inc_sales = 6000, std_ded = 12000),
           expect = list(st_ded = 9000, liab_st_iit = 2321.25),
           label = 'KS-5 2018 itemized component phase-in')

  #--------------------------------------------------------------------------
  # Delaware (PIT-RES) -- one schedule for every filing status, $110
  # per-exemption CREDIT, two-tier age-60 retirement exclusion, combined
  # separate filing, 20%-nonrefundable/4.5%-refundable EITC election
  #--------------------------------------------------------------------------

  # DE-1: 2024 single, AGI 50,000, two dependents. Std 3,250 -> taxable
  # 46,750 in the 5.55% band: 1,001 + 5.55% x 21,750 = 2,208.125; less
  # three $110 personal credits
  run_case('DE', 2024, list(agi = 50000, n_dep = 2, dep_age1 = 8,
                            dep_age2 = 10),
           expect = list(st_agi = 50000, st_std_ded = 3250,
                         st_exempt_credit = 330, liab_st_iit = 1878.125),
           label = 'DE-1 graduated schedule and $110 personal credits')

  # DE-2: 2024 single age 67, AGI 35,000 = pension 30,000 + interest 4,000 +
  # dividends 1,000. The age-60 exclusion covers pension PLUS eligible
  # retirement income, capped 12,500; the 15% US-obligation share of
  # interest (600) also comes out -> st_agi 21,900; std 3,250 + 2,500
  # (65+) = 5,750; taxable 16,150 -> 261 + 4.8% x 6,150 = 556.20; less $110
  # personal + $110 age-60 credits
  run_case('DE', 2024,
           list(agi = 35000, age1 = 67, txbl_pens_dist = 30000,
                txbl_int = 4000, div_ord = 1000),
           expect = list(st_retirement_excl = 12500, st_agi = 21900,
                         st_std_ded = 5750, st_exempt_credit = 110,
                         st_age_credit = 110, liab_st_iit = 336.20),
           label = 'DE-2 age-60 retirement exclusion and 65+ standard deduction')

  # DE-3: 2024 single, one child, wages 6,000, federal EITC 2,040. Taxable
  # 2,750 -> 2.2% x 750 = 16.50 of tax. The 20% nonrefundable option is
  # worth min(408, 16.50) = 16.50; the 4.5% refundable option is 91.80 and
  # wins. Personal credits (220) exhaust the tax
  run_case('DE', 2024,
           list(agi = 6000, wages1 = 6000, ei1 = 6000, n_dep = 1,
                dep_age1 = 5, eitc = 2040),
           expect = list(st_eitc = 91.80, st_exempt_credit = 220,
                         liab_st_iit = -91.80),
           label = 'DE-3 EITC election: 4.5% refundable beats 20% nonrefundable')

  # DE-4: 2024 two-earner MFJ, wages 60,000 each. Joint: taxable 113,500 ->
  # 2,943.50 + 6.6% x 53,500 = 6,474.50. Combined separate (one schedule for
  # all statuses makes this valuable): each column 60,000 - 3,250 = 56,750 ->
  # 1,001 + 5.55% x 31,750 = 2,763.125, x2 = 5,526.25, which is lower; less
  # $220 of personal credits. Pins combined_sep_std_share = 0.5
  run_case('DE', 2024,
           list(filing_status = 2, age2 = 40, agi = 120000,
                wages1 = 60000, wages2 = 60000, ei1 = 60000, ei2 = 60000),
           expect = list(liab_st_iit = 5306.25),
           label = 'DE-4 married filing combined separate (per-column std)')

  # DE-5: 2024 single, one dependent, AGI 40,000, federal care credit 1,000.
  # DE child care credit = 50% x 1,000 = 500, nonrefundable. Taxable
  # 36,750 -> 1,001 + 5.55% x 11,750 = 1,653.125; less 500 care credit and
  # two $110 personal credits
  run_case('DE', 2024,
           list(agi = 40000, n_dep = 1, dep_age1 = 4, care_exp = 3000,
                cdctc_nonref = 1000),
           expect = list(st_cdctc = 500, st_exempt_credit = 220,
                         liab_st_iit = 933.125),
           label = 'DE-5 50% child and dependent care credit')

  #--------------------------------------------------------------------------
  # Rhode Island (RI-1040) -- one schedule for every filing status, own
  # indexed std/exemption BOTH phased out on a stepped 20%-per-increment
  # schedule, full-retirement-age + AGI-capped SS and pension modifications
  #--------------------------------------------------------------------------

  # RI-1: 2024 single, wages 90,000. Std 10,550 + exemption 4,950 ->
  # taxable 74,500, inside the first bracket: 3.75% x 74,500
  run_case('RI', 2024, list(agi = 90000, wages1 = 90000, ei1 = 90000),
           expect = list(st_std_ded = 10550, st_exempt = 4950,
                         st_txbl_inc = 74500, liab_st_iit = 74500 * 0.0375),
           label = 'RI-1 2024 single: schedule, own std and exemption')

  # RI-2: the stepped phase-out of BOTH the std deduction and the exemptions.
  # 2024 MFJ, 2 dependents (4 exemptions), AGI 260,000. Excess over 246,450
  # = 13,550; 13,550/7,050 = 1.922 -> ceil = 2 steps -> share 0.60.
  # Std 21,150 x 0.6 = 12,690; exemptions 19,800 x 0.6 = 11,880; taxable
  # 235,430 -> 7,587.88 + 5.99% x 59,380 = 11,144.74
  run_case('RI', 2024,
           list(filing_status = 2, age2 = 40, agi = 260000, n_dep = 2,
                dep_age1 = 8, dep_age2 = 12, wages1 = 260000, ei1 = 260000),
           expect = list(st_std_ded = 12690, st_exempt = 11880,
                         st_txbl_inc = 235430, liab_st_iit = 11144.74),
           label = 'RI-2 2024 stepped std and exemption phase-out (2 steps)')

  # RI-2b: past the cliff. AGI 275,000 -> excess 28,550 > 4 x 7,050 = 28,200
  # -> ceil(4.05) = 5 steps -> share 0; BOTH amounts zero out, so taxable
  # income is the whole 275,000. Expectation written as the continuous
  # schedule rather than the published "Pay" constants, which are rounded to
  # the cent (7,587.88 for 7,587.875) and so differ by half a cent
  run_case('RI', 2024,
           list(filing_status = 2, age2 = 40, agi = 275000, n_dep = 2,
                dep_age1 = 8, dep_age2 = 12, wages1 = 275000, ei1 = 275000),
           expect = list(st_std_ded = 0, st_exempt = 0,
                         liab_st_iit = 0.0375 * 77450 +
                                       0.0475 * (176050 - 77450) +
                                       0.0599 * (275000 - 176050)),
           label = 'RI-2b phase-out cliff: std and exemptions both zero')

  # RI-3: 2024 single age 70, AGI 60,000 = pension 22,000 + IRA 18,000 +
  # taxable SS 12,000 + wages 8,000. SS modification: age 70 >= 66 and AGI
  # 60,000 <= 104,200 -> subtract all 12,000. Pension modification: cap
  # 20,000 on pensions ONLY (IRA excluded) -> min(22,000, 20,000) = 20,000.
  # st_agi 28,000; std 10,550; exemption 4,950; taxable 12,500 -> 3.75%
  run_case('RI', 2024,
           list(agi = 60000, age1 = 70, txbl_pens_dist = 22000,
                txbl_ira_dist = 18000, txbl_ss = 12000, gross_ss = 14000,
                wages1 = 8000, ei1 = 8000),
           expect = list(st_agi = 28000, st_txbl_inc = 12500,
                         liab_st_iit = 12500 * 0.0375),
           label = 'RI-3 2024 SS + pension modifications (IRA excluded)')

  # RI-4: 16% refundable EITC (the rate rose from 15% in TY2024). 2024 HoH,
  # two dependents, wages 30,000, federal EITC 4,000. Std 15,850 +
  # exemptions 3 x 4,950 = 14,850 exceed income -> zero tax; the credit
  # pays out in full
  run_case('RI', 2024,
           list(filing_status = 4, agi = 30000, wages1 = 30000, ei1 = 30000,
                n_dep = 2, dep_age1 = 8, dep_age2 = 12, eitc = 4000),
           expect = list(st_txbl_inc = 0, st_eitc = 640,
                         liab_st_iit = -640),
           label = 'RI-4 2024 16% refundable EITC')

  # RI-5: 25% nonrefundable child and dependent care credit. 2024 MFJ, two
  # dependents, AGI 80,000, federal care credit 1,200 -> RI 300. Taxable
  # 80,000 - 21,150 - 19,800 = 39,050 -> 3.75% x 39,050 = 1,464.375
  run_case('RI', 2024,
           list(filing_status = 2, age2 = 40, agi = 80000, wages1 = 50000,
                wages2 = 30000, ei1 = 50000, ei2 = 30000, n_dep = 2,
                dep_age1 = 4, dep_age2 = 7, care_exp = 6000,
                cdctc_nonref = 1200),
           expect = list(st_cdctc = 300, st_txbl_inc = 39050,
                         liab_st_iit = 39050 * 0.0375 - 300),
           label = 'RI-5 2024 25% nonrefundable care credit')

  #--------------------------------------------------------------------------
  # West Virginia (IT-140) -- NO standard deduction and NO itemized deduction,
  # flat $2,000 exemptions, five brackets with an exact half-bracket MFS
  # mirror, two-track SS phase-in, $8,000 senior modification NETTED by the SS
  # subtraction, and an FPG-keyed percentage-of-tax Family Tax Credit
  #--------------------------------------------------------------------------

  # WV-1: 2019 five-bracket schedule, exemptions only, no deduction of any
  # kind. MFJ 2 dependents -> 4 exemptions x 2,000 = 8,000. Taxable
  # 120,000 - 8,000 = 112,000, top band: 2,775 + 6.5% x 52,000 = 6,155
  run_case('WV', 2019,
           list(filing_status = 2, age2 = 40, agi = 120000, n_dep = 2,
                dep_age1 = 8, dep_age2 = 10, wages1 = 120000, ei1 = 120000),
           expect = list(st_agi = 120000, st_ded = 0, st_exempt = 8000,
                         st_txbl_inc = 112000, liab_st_iit = 6155.00),
           label = 'WV-1 2019 five-bracket schedule, exemptions only')

  # WV-1b: Rate Schedule II. 2019 MFS -> ONE exemption (box (b) is MFJ-only).
  # Taxable 40,000 - 2,000 = 38,000 on the halved ladder:
  # 1,387.50 + 6.5% x 8,000 = 1,907.50
  run_case('WV', 2019,
           list(filing_status = 3, agi = 40000, wages1 = 40000, ei1 = 40000),
           expect = list(st_exempt = 2000, st_txbl_inc = 38000,
                         liab_st_iit = 1907.50),
           label = 'WV-1b 2019 MFS half-bracket schedule, single exemption')

  # WV-2: 2023 HB 2526 rates (a 21.25% cut) with the 100% SS subtraction below
  # the AGI limit. Single age 63 (no senior modification), FAGI 45,000 =
  # wages 20,000 + pension 12,000 + taxable SS 13,000. 45,000 <= 50,000 ->
  # subtract all 13,000; st_agi 32,000 - 2,000 = 30,000 taxable ->
  # 708.50 + 3.54% x 5,000 = 885.50
  run_case('WV', 2023,
           list(agi = 45000, age1 = 63, wages1 = 20000, ei1 = 20000,
                txbl_pens_dist = 12000, txbl_ss = 13000, gross_ss = 15000),
           expect = list(st_agi = 32000, st_txbl_inc = 30000,
                         liab_st_iit = 885.50),
           label = 'WV-2 2023 HB 2526 rates + 100% SS below the limit')

  # WV-2b: the $50,000 cliff. Same unit at FAGI 52,000 -> no subtraction in
  # 2023 (the above-limit track starts TY2024). Taxable 50,000 ->
  # 1,239.50 + 4.72% x 10,000 = 1,711.50
  run_case('WV', 2023,
           list(agi = 52000, age1 = 63, wages1 = 27000, ei1 = 27000,
                txbl_pens_dist = 12000, txbl_ss = 13000, gross_ss = 15000),
           expect = list(st_agi = 52000, st_txbl_inc = 50000,
                         liab_st_iit = 1711.50),
           label = 'WV-2b SS subtraction denied above $50,000 (cliff)')

  # WV-3: TY2025 SB 2033 rates (a further 6% cut, LEGISLATED not certified)
  # plus the HB 4880 second SS track. MFJ ages 62/60, FAGI 130,000 = wages
  # 100,000 + taxable SS 30,000. Above the $100,000 limit, so the 2025
  # above-limit share applies: 65% x 30,000 = 19,500. st_agi 110,500 - 4,000
  # = 106,500 taxable -> 2,053.50 + 4.82% x 46,500 = 4,294.80
  run_case('WV', 2025,
           list(filing_status = 2, age1 = 62, age2 = 60, agi = 130000,
                wages1 = 100000, ei1 = 100000, txbl_ss = 30000,
                gross_ss = 34000),
           expect = list(st_agi = 110500, st_txbl_inc = 106500,
                         liab_st_iit = 4294.80),
           label = 'WV-3 2025 SB 2033 rates + 65% above-limit SS track')

  # WV-4: the Family Tax Credit. 2024 HoH, 2 dependents -> family size 3,
  # whose 2024 guideline is 15,060 + 2 x 5,380 = 25,820. Modified AGI 26,500
  # is 680 above -> ceiling(680/300) = 3 steps -> 70% (the published table row
  # 26,420-26,720 reads 70%). Taxable 26,500 - 6,000 = 20,500 ->
  # 236 + 3.15% x 10,500 = 566.75; credit 70% x 566.75 = 396.725
  run_case('WV', 2024,
           list(filing_status = 4, agi = 26500, wages1 = 26500, ei1 = 26500,
                n_dep = 2, dep_age1 = 8, dep_age2 = 10),
           expect = list(st_exempt = 6000, st_txbl_inc = 20500,
                         st_forgive_credit = 396.725,
                         liab_st_iit = 170.025),
           label = 'WV-4 2024 Family Tax Credit at 70% (3 steps above FPG)')

  # WV-5: the $8,000 senior citizen modification in a year with NO Social
  # Security subtraction, so the offset is inert. 2019 MFJ ages 70/68,
  # FAGI 60,000 = pension 30,000 + IRA 20,000 + interest 10,000 ->
  # modification 8,000 x 2 = 16,000 plus the 15% US-obligation share of
  # interest (1,500): st_agi 42,500; taxable 38,500 -> 900 + 4.5% x 13,500
  run_case('WV', 2019,
           list(filing_status = 2, age1 = 70, age2 = 68, agi = 60000,
                txbl_pens_dist = 30000, txbl_ira_dist = 20000,
                txbl_int = 10000),
           expect = list(st_agi = 42500, st_txbl_inc = 38500,
                         liab_st_iit = 1507.50),
           label = 'WV-5 2019 senior citizen modification, 8,000 x 2')

  # WV-5b: the SS NETTING that age_ded_less_ss_sub exists for. Same couple in
  # 2023 with taxable SS 20,000. Schedule M line 47 box (d) nets each spouse's
  # SS subtraction out of that spouse's $8,000, so the modification is ZERO
  # here: st_agi = 60,000 - 20,000 (SS) - 0 - 1,500 (15% US-obligation share
  # of interest) = 38,500; taxable 34,500 -> 708.50 + 3.54% x 9,500 =
  # 1,044.80. Without the SS offset the model once returned roughly half
  # the true liability
  run_case('WV', 2023,
           list(filing_status = 2, age1 = 68, age2 = 68, agi = 60000,
                txbl_pens_dist = 20000, txbl_ira_dist = 10000,
                txbl_int = 10000, txbl_ss = 20000, gross_ss = 23000),
           expect = list(st_agi = 38500, st_txbl_inc = 34500,
                         liab_st_iit = 1044.80),
           label = 'WV-5b senior modification netted by the SS subtraction')

  # WV-6: dependent filer -- the $500 allowance in place of exemptions, and
  # the Family Tax Credit barred ("Individuals who file their income tax
  # return with zero exemptions cannot claim the credit"). 2023 wages 12,000,
  # above the $10,000 low-income-exclusion cliff so the case is exact.
  # Taxable 12,000 - 500 = 11,500 -> 236 + 3.15% x 1,500 = 283.25
  run_case('WV', 2023,
           list(agi = 12000, dep_status = 1, wages1 = 12000, ei1 = 12000),
           expect = list(st_ded = 500, st_exempt = 0, st_txbl_inc = 11500,
                         st_forgive_credit = 0, liab_st_iit = 283.25),
           label = 'WV-6 2023 dependent filer: $500 allowance, credit barred')

  # WV-7: the 50% child and dependent care credit, which FIRST APPEARS in
  # TY2024 (no care line exists in the 2017-2023 booklets). MFJ, 2 dependents,
  # AGI 80,000, federal care credit 1,200 -> WV 600, nonrefundable. Taxable
  # 80,000 - 8,000 = 72,000 -> 2,183.50 + 5.12% x 12,000 = 2,797.90; the
  # Family Tax Credit is zero at this income
  run_case('WV', 2024,
           list(filing_status = 2, age2 = 40, agi = 80000, wages1 = 50000,
                wages2 = 30000, ei1 = 50000, ei2 = 30000, n_dep = 2,
                dep_age1 = 4, dep_age2 = 7, care_exp = 6000,
                cdctc_nonref = 1200),
           expect = list(st_cdctc = 600, st_txbl_inc = 72000,
                         liab_st_iit = 2797.90 - 600),
           label = 'WV-7 2024 50% care credit (first year it exists)')

  # WV-8/WV-8b: the AGI-GATED PARTIAL Social Security share (TY2021 = 65% at
  # or below $100,000 joint, nothing above). MFJ 68/66 with taxable SS 20,000.
  # Below the limit: SS subtraction 0.65 x 20,000 = 13,000, and the senior
  # modification is 8,000 x 2 less that 13,000 = 3,000, so st_agi = 80,000 -
  # 13,000 - 3,000 = 64,000; taxable 60,000 -> 2,775 exactly at the top band.
  # Above the limit: no SS subtraction in 2021 (the above-limit track starts
  # TY2024), so the senior modification is the full 16,000 -> st_agi 104,000,
  # taxable 100,000 -> 2,775 + 6.5% x 40,000 = 5,375. Together these pin
  # ss_allages_sub_share AND its interaction with age_ded_less_ss_sub
  run_case('WV', 2021,
           list(filing_status = 2, age1 = 68, age2 = 66, agi = 80000,
                wages1 = 60000, ei1 = 60000, txbl_ss = 20000,
                gross_ss = 23000),
           expect = list(st_agi = 64000, st_txbl_inc = 60000,
                         liab_st_iit = 2775.00),
           label = 'WV-8 2021 gated 65% SS share below the AGI limit')
  run_case('WV', 2021,
           list(filing_status = 2, age1 = 68, age2 = 66, agi = 120000,
                wages1 = 100000, ei1 = 100000, txbl_ss = 20000,
                gross_ss = 23000),
           expect = list(st_agi = 104000, st_txbl_inc = 100000,
                         liab_st_iit = 5375.00),
           label = 'WV-8b 2021 no SS subtraction above the AGI limit')

  #--------------------------------------------------------------------------
  # NEW MEXICO (PIT-1, PIT-ADJ, PIT-RC)
  #
  # Every case pins st_ded.std_amount (and the aged add-on where it bites) to
  # the REAL federal standard deduction for that year. NM's ded.yaml mirrors
  # the federal std.yaml including its indexation, and this suite's synthetic
  # 2.5% index leaves indexed federal parameters at their anchor values, so the
  # harness would otherwise compute against $12,000 where TY2019 was $12,200.
  # Pinning keeps the arithmetic form-true; test_nm_std_mirrors_federal() in
  # test_state_tax_law.R is what guarantees the mirror itself tracks federal.
  #--------------------------------------------------------------------------

  # NM-1 TY2019 single, FAGI 28,000, no dependents. Low- and middle-income
  # exemption on the slope: 2,500 - 0.15 x (28,000 - 20,000) = 1,300 for the
  # one exemption. Taxable 28,000 - 12,200 - 1,300 = 14,500, on the four-bracket
  # 2017-2020 schedule: 5,500 x 1.7% = 93.50, 5,500 x 3.2% = 176.00,
  # 3,500 x 4.7% = 164.50
  run_case('NM', 2019,
           list(agi = 28000, wages1 = 28000, ei1 = 28000, std_ded = 12200),
           expect = list(st_exempt = 1300, st_txbl_inc = 14500,
                         liab_st_iit = 434.00),
           law_overrides = list(st_ded.std_amount = 12200),
           label = 'NM-1 2019 four-bracket schedule + LMI exemption slope')

  # NM-1b the LMI exemption reaches exactly zero at the published single-filer
  # limit of $36,667 (0.15 x 16,667 = 2,500.05), which is why the limit needs
  # no separate parameter. Taxable 36,667 - 12,200 = 24,467
  run_case('NM', 2019,
           list(agi = 36667, wages1 = 36667, ei1 = 36667, std_ded = 12200),
           expect = list(st_exempt = 0),
           law_overrides = list(st_ded.std_amount = 12200),
           label = 'NM-1b LMI exemption zero at the published AGI limit')

  # NM-2 TY2023 MFJ, two children aged 5 and 8, FAGI 45,000, federal EITC
  # 3,049.08. Four exemptions x 2,500 = 10,000 gross LMI, reduced
  # 0.10 x (45,000 - 30,000) = 1,500 per exemption -> 4,000 left. Dependent
  # deduction 4,000 x (2 - 1) = 4,000 (the count_offset). Taxable
  # 45,000 - 27,700 - 4,000 - 4,000 = 9,300 -> 8,000 x 1.7% + 1,300 x 3.2%
  # = 177.60. Child credit at tier 2 (25,001-50,000): 400 x 2 = 800.
  # WFTC 25% x 3,049.08 = 762.27. Both refundable, so
  # 177.60 - 800 - 762.27 = -1,384.67
  run_case('NM', 2023,
           list(filing_status = 2, age2 = 38, agi = 45000, wages1 = 30000,
                ei1 = 30000, wages2 = 15000, ei2 = 15000, n_dep = 2,
                n_dep_ctc = 2, n_dep_eitc = 2, dep_age1 = 5, dep_age2 = 8,
                eitc = 3049.08, std_ded = 27700),
           expect = list(st_exempt = 4000, st_child_ded = 4000,
                         st_txbl_inc = 9300, st_ctc = 800, st_eitc = 762.27,
                         liab_st_iit = -1384.67),
           law_overrides = list(st_ded.std_amount = 27700),
           label = 'NM-2 2023 dependent deduction, tier-2 child credit, WFTC')

  # NM-2b the same return with a THIRD child, which is where count_offset
  # earns its keep: the deduction is 4,000 x (3 - 1) = 8,000, not 12,000.
  # Taxable 45,000 - 27,700 - 4,000 (LMI is now 5 x 2,500 = 12,500 gross less
  # 1,500 x 5 = 7,500, so 5,000) - 8,000 = 4,300; the child credit reaches
  # only three children because three dependent age slots are tracked
  run_case('NM', 2023,
           list(filing_status = 2, age2 = 38, agi = 45000, wages1 = 30000,
                ei1 = 30000, wages2 = 15000, ei2 = 15000, n_dep = 3,
                n_dep_ctc = 3, n_dep_eitc = 3, dep_age1 = 5, dep_age2 = 8,
                dep_age3 = 11, eitc = 3049.08, std_ded = 27700),
           expect = list(st_exempt = 5000, st_child_ded = 8000,
                         st_txbl_inc = 4300, st_ctc = 1200),
           law_overrides = list(st_ded.std_amount = 27700),
           label = 'NM-2b count_offset excludes only the FIRST dependent')

  # NM-2c a single filer with the same two children gets NO dependent
  # deduction: 7-2-39 is limited to joint, surviving-spouse and head-of-
  # household returns, encoded through the amounts mapper
  run_case('NM', 2023,
           list(agi = 45000, wages1 = 45000, ei1 = 45000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 5, dep_age2 = 8, std_ded = 13850),
           expect = list(st_child_ded = 0),
           law_overrides = list(st_ded.std_amount = 13850),
           label = 'NM-2c dependent deduction denied to single filers')

  # NM-3 TY2025 MFJ both aged 70, FAGI 72,000 including 22,000 of federally
  # taxable Social Security. THE CASE THAT JUSTIFIES start_point 1. The SS
  # exemption is a cliff and 72,000 <= 150,000, so all 22,000 comes out ->
  # st_agi 50,000. The aged $8,000-per-person exemption is already exhausted
  # (16,000 - (72,000 - 35,000) < 0). LMI is zero (0.10 x 42,000 > 2,500).
  # Federal deduction 31,500 + 2 x 1,600 aged = 34,700, so taxable 15,300 on
  # the HB 252 six-bracket schedule: 8,000 x 1.5% + 7,300 x 3.2% = 353.60.
  # txbl_inc is supplied as 25,300 -- federal taxable income AFTER the OBBBA
  # senior deduction of 2 x 6,000 -- to show it is not read.
  nm3_unit = list(filing_status = 2, age1 = 70, age2 = 70, agi = 72000,
                  txbl_inc = 25300, wages1 = 50000, ei1 = 50000,
                  txbl_ss = 22000, gross_ss = 26000, std_ded = 34700)
  run_case('NM', 2025, nm3_unit,
           expect = list(st_agi = 50000, st_exempt = 0, st_txbl_inc = 15300,
                         liab_st_iit = 353.60),
           law_overrides = list(st_ded.std_amount = 31500,
                                st_ded.std_aged_addl = 1600),
           label = 'NM-3 2025 six-bracket schedule, SS cliff, aged exhausted')

  # NM-3b the same return read from federal TAXABLE income instead, which is
  # what a start_point 2 encoding would do: 25,300 - 22,000 = 3,300 of NM
  # taxable income and 49.50 of tax. That is a $304 (86%) understatement on one
  # return, and it is the OBBBA senior deduction plus QBI leaking in. This case
  # exists so the decision cannot be silently reversed
  run_case('NM', 2025, nm3_unit,
           expect = list(st_agi = 3300, st_txbl_inc = 3300,
                         liab_st_iit = 49.50),
           law_overrides = list(st_agi.start_point = 2,
                                st_ded.std_amount = 0,
                                st_ded.std_aged_addl = 0,
                                st_ded.item_allowed = 0),
           label = 'NM-3b start_point 2 would leak the OBBBA senior deduction')

  # NM-4 TY2018 single with a 50,000 long-term gain and 100,000 of wages: the
  # net capital gains deduction is the GREATER of 50% of the gain (25,000) and
  # the flat 1,000, so 25,000. st_agi 150,000 - 25,000 = 125,000; LMI zero;
  # taxable 125,000 - 12,000 = 113,000 on the four-bracket schedule, whose top
  # rate is 4.9% (the 5.9% bracket does NOT exist before 2021 -- PolicyEngine
  # applies it from 2008 and overstates NM tax here):
  # 93.50 + 176.00 + 235.00 + 0.049 x (113,000 - 16,000) = 5,257.50
  run_case('NM', 2018,
           list(agi = 150000, wages1 = 100000, ei1 = 100000, kg_lt = 50000,
                txbl_kg = 50000, std_ded = 12000),
           expect = list(st_agi = 125000, st_txbl_inc = 113000,
                         liab_st_iit = 5257.50),
           law_overrides = list(st_ded.std_amount = 12000),
           label = 'NM-4 2018 50% capital gains deduction, pre-2021 top rate')

  # NM-4b TY2025 the percentage leg is gone (it survives only for sales of a
  # New Mexico business, unobservable) and the flat floor is 2,500, so a
  # 50,000 gain now yields a 2,500 deduction rather than 20,000
  run_case('NM', 2025,
           list(agi = 150000, wages1 = 100000, ei1 = 100000, kg_lt = 50000,
                txbl_kg = 50000, std_ded = 15000),
           expect = list(st_agi = 147500),
           law_overrides = list(st_ded.std_amount = 15000),
           label = 'NM-4b 2025 flat $2,500 capital gains floor replaces the share')

  # NM-5 TY2022 single aged 68, FAGI 24,000 with 6,000 of taxable SS. Both
  # senior provisions bite at once: the SS cliff exempts all 6,000, and the
  # aged deduction pays 8,000 - (24,000 - 20,500) = 4,500. st_agi
  # 24,000 - 6,000 - 4,500 = 13,500. LMI 2,500 - 0.15 x 4,000 = 1,900.
  # Taxable 13,500 - 12,950 - 1,900 floors at zero
  run_case('NM', 2022,
           list(age1 = 68, agi = 24000, wages1 = 18000, ei1 = 18000,
                txbl_ss = 6000, gross_ss = 7000, std_ded = 12950),
           expect = list(st_agi = 13500, st_exempt = 1900, st_txbl_inc = 0,
                         liab_st_iit = 0),
           law_overrides = list(st_ded.std_amount = 12950),
           label = 'NM-5 2022 SS cliff and aged deduction ramp together')

  # NM-6 TY2024 MFJ, FAGI 260,000, two children: the SIXTH tier of the child
  # credit (200,001-350,000) pays 51 per child, which the pre-generalization
  # three-tier selector would have paid as zero. The LMI exemption is long gone
  # at this income but the dependent deduction is not income-tested, so taxable
  # is 260,000 - 29,200 - 4,000 = 226,800 -- below the 5.9% bracket that begins
  # at 315,000, so the top of the schedule here is 4.9%:
  # 136.00 + 256.00 + 376.00 + 0.049 x (226,800 - 24,000) = 10,705.20,
  # less 102 of child credit
  run_case('NM', 2024,
           list(filing_status = 2, age2 = 40, agi = 260000, wages1 = 260000,
                ei1 = 260000, n_dep = 2, n_dep_ctc = 2, dep_age1 = 5,
                dep_age2 = 8, std_ded = 29200),
           expect = list(st_exempt = 0, st_child_ded = 4000,
                         st_txbl_inc = 226800, st_ctc = 102,
                         liab_st_iit = 10603.20),
           law_overrides = list(st_ded.std_amount = 29200),
           label = 'NM-6 2024 sixth child-credit tier (was zero at 3 tiers)')

  # NM-6b the seventh tier is UNBOUNDED ("over $350,000"), so a 400,000 return
  # still collects 25 per child. This is the semantic that separates NM from
  # Colorado, whose credit ends above its last bound
  run_case('NM', 2024,
           list(filing_status = 2, age2 = 40, agi = 400000, wages1 = 400000,
                ei1 = 400000, n_dep = 2, n_dep_ctc = 2, dep_age1 = 5,
                dep_age2 = 8, std_ded = 29200),
           expect = list(st_ctc = 50),
           law_overrides = list(st_ded.std_amount = 29200),
           label = 'NM-6b seventh child-credit tier stays open above 350,000')

  #--------------------------------------------------------------------------
  # VERMONT (Form IN-111, Schedules IN-112 / IN-153)
  #
  # Vermont publishes its own standard deduction and personal exemption from
  # TY2018, so unlike New Mexico these cases need no federal pinning -- except
  # in 2017, whose base is federal taxable income and whose IN-155 addback is
  # measured against the federal standard deduction.
  #--------------------------------------------------------------------------

  # VT-1 TY2017 single, FAGI 120,000, itemized 21,000 of which 6,000 is state
  # income tax, 1,000 of out-of-state municipal interest. Federal taxable income
  # 120,000 - 21,000 - 4,050 exemption = 94,950 is the base. Additions: the muni
  # addback at the model's 25% out-of-state convention = 250, and the IN-155
  # first term = min(6,000 state income tax, 21,000 - 6,350 federal standard)
  # = 6,000. VT taxable 101,200 on the 2017 FIVE-bracket schedule:
  # 37,900 x 3.55% = 1,345.45, 53,950 x 6.80% = 3,668.60,
  # 9,350 x 7.80% = 729.30 -> 5,743.35. The printed 2017 Schedule X gives
  # 5,743.30; the $0.05 is the schedule's whole-dollar base rounding
  run_case('VT', 2017,
           list(agi = 120000, txbl_inc = 94950, wages1 = 120000, ei1 = 120000,
                itemizing = 1, exempt_int = 1000, item_ded = 21000,
                item_ded_ex_limits = 21000, salt_item_ded = 6000,
                salt_inc_sales = 6000, std_ded = 6350),
           expect = list(st_txbl_inc = 101200, liab_st_iit = 5743.35),
           label = 'VT-1 2017 taxable-income base + IN-155 SALT addback')

  # VT-2 TY2019 MFJ aged 67 and 64, one dependent, FAGI 58,000 including 8,000
  # of federally taxable Social Security. AGI is under the 60,000 joint
  # threshold so the SS exemption is FULL -> st_agi 50,000. Standard deduction
  # 12,300 plus ONE aged box (only the 67-year-old qualifies) = 13,300;
  # exemptions 3 x 4,250 = 12,750. Taxable 23,950 x 3.35% = 802.33
  run_case('VT', 2019,
           list(filing_status = 2, age1 = 67, age2 = 64, agi = 58000,
                wages1 = 50000, ei1 = 50000, txbl_ss = 8000, gross_ss = 9000,
                n_dep = 1, dep_age1 = 10),
           expect = list(st_agi = 50000, st_ded = 13300, st_exempt = 12750,
                         st_txbl_inc = 23950, liab_st_iit = 802.33),
           label = 'VT-2 2019 full SS exemption below the joint threshold')

  # VT-2b the phase-out band: TY2023 single aged 68, AGI 52,000 with 10,000 of
  # taxable SS. The single threshold is 50,000, so the excess is 2,000 = 20
  # whole $100 steps, leaving an exempt share of 1 - 0.20 = 0.80 -> 8,000
  # subtracted. st_agi 44,000; deduction 7,000 + 1,150 aged = 8,150; exemption
  # 4,850. Taxable 31,000 x 3.35% = 1,038.50
  run_case('VT', 2023,
           list(age1 = 68, agi = 52000, wages1 = 42000, ei1 = 42000,
                txbl_ss = 10000, gross_ss = 11500),
           expect = list(st_agi = 44000, st_txbl_inc = 31000,
                         liab_st_iit = 1038.50),
           label = 'VT-2b 2023 SS exemption at 80% inside the phase-out band')

  # VT-2c above the band the exemption is gone entirely: the same return at AGI
  # 62,000 is more than 10,000 over the 50,000 threshold, so no Social Security
  # comes out and st_agi equals AGI
  run_case('VT', 2023,
           list(age1 = 68, agi = 62000, wages1 = 52000, ei1 = 52000,
                txbl_ss = 10000, gross_ss = 11500),
           expect = list(st_agi = 62000),
           label = 'VT-2c SS exemption exhausted 10,000 above the threshold')

  # VT-3 TY2023 head of household, two dependents aged 8 and 10, wages 24,000
  # plus an 8,000 long-term gain, federal EITC 4,200, charitable contributions
  # 2,000. THE ARCHETYPAL VERMONT CASE: both provisions that had no parameter
  # until 2026-08-12 bind here.
  #   capital gains: max(share 0, min(flat 5,000, gain 8,000)) = 5,000, then
  #     capped at 40% of federal taxable income (32,000 - 20,800 = 11,200), so
  #     4,480 -- the CEILING binds, which is why a flat parameter alone was not
  #     enough
  #   st_agi 32,000 - 4,480 = 27,520; deduction 10,550; exemptions 3 x 4,850
  #     = 14,550 -> taxable 2,420 x 3.35% = 81.07 of tax
  #   charitable credit 5% x 2,000 = 100, nonrefundable, so it absorbs the
  #     81.07 and no more
  #   EITC 38% x 4,200 = 1,596, refundable
  # Total -1,596.00. The dependents are too old for Vermont's child credit
  # (age 5 or under in 2023), which is what leaves the EITC alone on the line
  run_case('VT', 2023,
           list(filing_status = 4, agi = 32000, txbl_inc = 11200,
                wages1 = 24000, ei1 = 24000, kg_lt = 8000, txbl_kg = 8000,
                n_dep = 2, n_dep_eitc = 2, dep_age1 = 8, dep_age2 = 10,
                eitc = 4200, char_cash = 2000, std_ded = 20800),
           expect = list(st_agi = 27520, st_txbl_inc = 2420,
                         st_char_credit = 100, st_eitc = 1596,
                         liab_st_iit = -1596.00),
           label = 'VT-3 2023 capital gains ceiling + charitable credit')

  # VT-4 TY2023 MFJ, two children aged 3 and 4, AGI 145,000: the child credit
  # with its PER-CHILD phase-out. The reduction is 20 whole $1,000 steps x $20
  # = 400 against EACH child's 1,000, so 2 x 600 = 1,200 -- not the 1,600 that
  # reducing the aggregate credit once would give
  run_case('VT', 2023,
           list(filing_status = 2, age2 = 40, agi = 145000, wages1 = 145000,
                ei1 = 145000, n_dep = 2, n_dep_ctc = 2, dep_age1 = 3,
                dep_age2 = 4),
           expect = list(st_ctc = 1200),
           label = 'VT-4 2023 child credit phased out per child')

  # VT-4b at AGI 185,000 the reduction is 1,200, more than one child's 1,000,
  # so the per-child floor zeroes the credit -- matching the statute's full
  # phase-out at 175,000. Reducing the aggregate would still pay 800
  run_case('VT', 2023,
           list(filing_status = 2, age2 = 40, agi = 185000, wages1 = 185000,
                ei1 = 185000, n_dep = 2, n_dep_ctc = 2, dep_age1 = 3,
                dep_age2 = 4),
           expect = list(st_ctc = 0),
           label = 'VT-4b child credit fully phased out at 175,000')

  # VT-4c "or fraction thereof": AGI 145,500 is a partial 21st step, counted
  # whole, so the reduction is 420 and each child keeps 580
  run_case('VT', 2023,
           list(filing_status = 2, age2 = 40, agi = 145500, wages1 = 145500,
                ei1 = 145500, n_dep = 2, n_dep_ctc = 2, dep_age1 = 3,
                dep_age2 = 4),
           expect = list(st_ctc = 1160),
           label = 'VT-4c child credit counts a partial step whole')

  # VT-5 TY2025 the childless EITC match becomes 100% of the federal credit
  # (Act 71) while filers with children stay at 38%. Same 4,200 federal credit,
  # no qualifying children -> 4,200 rather than 1,596
  run_case('VT', 2025,
           list(agi = 20000, wages1 = 20000, ei1 = 20000, eitc = 4200),
           expect = list(st_eitc = 4200),
           label = 'VT-5 2025 childless EITC match at 100%')

  # VT-5b a filer WITH children stays on the 38% match in the same year, which
  # is what the child-count-keyed family is for
  run_case('VT', 2025,
           list(agi = 20000, wages1 = 20000, ei1 = 20000, n_dep = 1,
                n_dep_eitc = 1, dep_age1 = 10, eitc = 4200),
           expect = list(st_eitc = 1596),
           label = 'VT-5b 2025 match stays 38% with a qualifying child')

  # VT-6 TY2019 the care credit is 24% of the federal credit and NONREFUNDABLE;
  # TY2022 turns the same credit into 72% refundable. Single with one dependent
  # and 20,000 of AGI: deduction 6,150, exemptions 2 x 4,250 = 8,500 (the
  # dependent counts), so taxable 5,350 and tax 179.23. The 144 credit is
  # nonrefundable and can only offset, leaving 35.23
  run_case('VT', 2019,
           list(agi = 20000, wages1 = 20000, ei1 = 20000, n_dep = 1,
                dep_age1 = 4, care_exp = 3000, cdctc_nonref = 600),
           expect = list(st_cdctc = 144, st_exempt = 8500,
                         st_txbl_inc = 5350, liab_st_iit = 35.23),
           label = 'VT-6 2019 care credit at 24% nonrefundable')

  # VT-6b the same return in TY2022: 72% of 600 = 432, REFUNDABLE. Deduction
  # 6,500 and exemptions 2 x 4,500 = 9,000 leave 4,500 taxable and 150.75 of
  # tax. TY2022 is also the first year of the child credit and this dependent is
  # 4, so a further 1,000 lands unphased at this income:
  # 150.75 - 432 - 1,000 = -1,281.25. Act 138 turned both credits at once,
  # which is why the pair of them shows up together
  run_case('VT', 2022,
           list(agi = 20000, wages1 = 20000, ei1 = 20000, n_dep = 1,
                n_dep_ctc = 1, dep_age1 = 4, care_exp = 3000,
                cdctc_nonref = 600),
           expect = list(st_cdctc = 432, st_ctc = 1000, st_txbl_inc = 4500,
                         liab_st_iit = -1281.25),
           label = 'VT-6b 2022 care credit at 72% refundable, plus the new CTC')

  #--------------------------------------------------------------------------
  # OKLAHOMA (Form 511, Schedules 511-A/B/D/F/G)
  #
  # No Form 511 packet prints a bracket schedule in any year, so the schedule
  # in ord.yaml was RECOVERED from the $50-range tax table and the printed
  # "over $100,000" constants. OK-1 and OK-2 are the acceptance test for that
  # recovery: they put taxable income at exactly $100,000 and check the
  # cumulative tax against the printed constant, for every vintage and both
  # rate columns.
  #--------------------------------------------------------------------------

  # OK-1 the single column across all three vintages. AGI 107,350 less the
  # frozen 6,350 standard deduction and one 1,000 exemption is exactly 100,000
  # of taxable income, where the printed constants are $4,812 (2017-2021),
  # $4,562 (2022-2025) and -- for the not-yet-published 2026 schedule -- an
  # expected $4,285
  ok_single_100k = list(agi = 107350, wages1 = 107350, ei1 = 107350)
  run_case('OK', 2017, ok_single_100k,
           expect = list(st_txbl_inc = 100000, liab_st_iit = 4811.50),
           label = 'OK-1 2017 single schedule reproduces the $4,812 constant')
  run_case('OK', 2022, ok_single_100k,
           expect = list(liab_st_iit = 4561.50),
           label = 'OK-1b 2022 rate cut reproduces the $4,562 constant')
  run_case('OK', 2026, ok_single_100k,
           expect = list(liab_st_iit = 4285.25),
           label = 'OK-1c 2026 HB 2764 schedule hits the expected $4,285')

  # OK-2 the married column, which is where the risk is. AGI 114,700 less the
  # 12,700 deduction and two exemptions is 100,000 of taxable income. TY2024
  # must differ from TY2023 by $22 -- HB 1040X moved the married top-bracket
  # start from 12,200 to 14,400 by changing ONE number, with no rate change and
  # nothing visible in the single column. This pair is what stops that vintage
  # from being simplified away
  ok_mfj_100k = list(filing_status = 2, age2 = 40, agi = 114700,
                     wages1 = 114700, ei1 = 114700)
  run_case('OK', 2017, ok_mfj_100k,
           expect = list(st_txbl_inc = 100000, liab_st_iit = 4645.00),
           label = 'OK-2 2017 married schedule reproduces the $4,645 constant')
  run_case('OK', 2023, ok_mfj_100k,
           expect = list(liab_st_iit = 4395.00),
           label = 'OK-2b 2023 married constant $4,395 (top bracket 12,200)')
  run_case('OK', 2024, ok_mfj_100k,
           expect = list(liab_st_iit = 4373.00),
           label = 'OK-2c 2024 HB 1040X moves the married top bracket to 14,400')

  # OK-3 head of household takes the MARRIED rate column while keeping only one
  # personal exemption. AGI 110,350 less the 9,350 HoH deduction, one personal
  # and one dependent exemption leaves 99,000, taxed on the married schedule:
  # 307.00 cumulative at 14,400 plus 4.75% of 84,600 = 4,325.50
  run_case('OK', 2024,
           list(filing_status = 4, agi = 110350, wages1 = 110350,
                ei1 = 110350, n_dep = 1, dep_age1 = 10),
           expect = list(st_exempt = 2000, st_txbl_inc = 99000,
                         liab_st_iit = 4325.50),
           label = 'OK-3 head of household on the married rate column')

  # OK-4 the age-65 special exemption, encoded as a subtraction because it
  # carries a hard federal-AGI cliff no exemption add-on can express. A single
  # 66-year-old at AGI 14,000 is under the 15,000 limit, so the full 1,000
  # comes out: st_agi 13,000, taxable 13,000 - 6,350 - 1,000 = 5,650
  run_case('OK', 2024,
           list(age1 = 66, agi = 14000, wages1 = 14000, ei1 = 14000),
           expect = list(st_agi = 13000, st_txbl_inc = 5650),
           label = 'OK-4 age-65 exemption allowed under the AGI limit')

  # OK-4b at AGI 16,000 the statute allows nothing at all. The calculator ramps
  # the 1,000 down dollar for dollar above the limit rather than cliffing, so it
  # reaches zero exactly here and is form-exact outside a 1,000-wide AGI band
  run_case('OK', 2024,
           list(age1 = 66, agi = 16000, wages1 = 16000, ei1 = 16000),
           expect = list(st_agi = 16000),
           label = 'OK-4b age-65 exemption gone 1,000 above the limit')

  # OK-5 THE GREATER-OF. Oklahoma grants the larger of 20% of the federal child
  # care credit and 5% of the federal child tax credit, never both. MFJ at AGI
  # 60,000 with a 4,000 federal CTC and a 1,200 federal CDCC: the care leg
  # (240) beats the child leg (200), so the child leg is zeroed
  run_case('OK', 2019,
           list(filing_status = 2, age2 = 38, agi = 60000, wages1 = 40000,
                ei1 = 40000, wages2 = 20000, ei2 = 20000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 5, dep_age2 = 8, care_exp = 6000,
                ctc_nonref = 4000, cdctc_nonref = 1200),
           expect = list(st_ctc = 0, st_cdctc = 240),
           label = 'OK-5 greater-of picks the 20% care credit')

  # OK-5b the same family with no care expenses: the child leg is all there is,
  # and it survives at 200. Encoding only the care leg -- the fallback before
  # the greater-of machinery existed -- would have paid this family nothing
  run_case('OK', 2019,
           list(filing_status = 2, age2 = 38, agi = 60000, wages1 = 40000,
                ei1 = 40000, wages2 = 20000, ei2 = 20000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 5, dep_age2 = 8, ctc_nonref = 4000),
           expect = list(st_ctc = 200, st_cdctc = 0),
           label = 'OK-5b greater-of keeps the 5% child credit alone')

  # OK-5c above 100,000 of federal AGI the whole credit is denied -- a cliff,
  # not a phase-out. Both legs must go to zero, which is why the child leg uses
  # a single-tier ladder and the care leg a zero cap above the threshold
  run_case('OK', 2019,
           list(filing_status = 2, age2 = 38, agi = 120000, wages1 = 120000,
                ei1 = 120000, n_dep = 2, n_dep_ctc = 2, dep_age1 = 5,
                dep_age2 = 8, care_exp = 6000, ctc_nonref = 4000,
                cdctc_nonref = 1200),
           expect = list(st_ctc = 0, st_cdctc = 0),
           label = 'OK-5c both credit legs denied above the 100,000 cliff')

  # OK-6 the $17,000 itemized cap with charity and medical EXEMPT from it.
  # Components: 3,000 medical, 20,000 mortgage, 5,000 charity, 6,000 property
  # tax and 4,000 income tax, with the federal SALT deduction capped at 10,000.
  # Oklahoma's base replaces capped SALT with uncapped property tax and drops
  # the income-tax component: 38,000 - 10,000 + 6,000 = 34,000. The cap then
  # applies to everything except the 8,000 of charity and medical:
  # min(17,000, 34,000 - 8,000) + 8,000 = 25,000. Taxable 80,000 - 25,000
  # - 1,000 = 54,000, taxed 171.50 + 5% x 46,800 = 2,511.50
  ok_itemizer = list(agi = 80000, wages1 = 80000, ei1 = 80000, itemizing = 1,
                     item_ded = 38000, item_ded_ex_limits = 38000,
                     salt_item_ded = 10000, salt_prop = 6000,
                     salt_inc_sales = 4000, med_item_ded = 3000,
                     char_item_ded = 5000, mort_int_item_ded = 20000,
                     std_ded = 12200)
  run_case('OK', 2019, ok_itemizer,
           expect = list(st_ded = 25000, st_txbl_inc = 54000,
                         liab_st_iit = 2511.50),
           label = 'OK-6 $17,000 itemized cap, charity and medical exempt')

  # OK-6b TY2017 had no cap, so the same return deducts the full 34,000 and is
  # exact: taxable 45,000, tax 171.50 + 5% x 37,800 = 2,061.50
  run_case('OK', 2017, ok_itemizer,
           expect = list(st_ded = 34000, st_txbl_inc = 45000,
                         liab_st_iit = 2061.50),
           label = 'OK-6b no itemized cap existed in 2017')

  # OK-7 the EITC changes character in 2022. A single filer at AGI 7,000 owes
  # nothing (7,000 - 6,350 - 1,000 floors at zero), so the 5% of a 500 federal
  # credit is worth nothing in 2019 and pays out in 2022
  ok_eitc_unit = list(agi = 7000, wages1 = 7000, ei1 = 7000, eitc = 500)
  run_case('OK', 2019, ok_eitc_unit,
           expect = list(st_eitc = 25, st_txbl_inc = 0, liab_st_iit = 0),
           label = 'OK-7 2019 EITC nonrefundable, worth nothing at zero tax')
  run_case('OK', 2022, ok_eitc_unit,
           expect = list(st_eitc = 25, liab_st_iit = -25),
           label = 'OK-7b 2022 HB 2962 restored refundability')

  # OK-8 Social Security comes out in full, with no age or income test. A
  # single 70-year-old at AGI 40,000 including 15,000 of taxable benefits keeps
  # 25,000 of base; the age-65 exemption is long gone at this income
  run_case('OK', 2024,
           list(age1 = 70, agi = 40000, wages1 = 25000, ei1 = 25000,
                txbl_ss = 15000, gross_ss = 17000),
           expect = list(st_agi = 25000, st_txbl_inc = 17650),
           label = 'OK-8 full Social Security subtraction, unconditional')

  # OK-9 the retirement exclusion is $10,000 PER PERSON, and the calculator
  # pools the two caps at unit level. A couple where one spouse holds all
  # 30,000 of pension income therefore excludes 20,000 where Oklahoma allows
  # 10,000 -- the documented over-exclusion, pinned here so it cannot drift
  # silently
  run_case('OK', 2024,
           list(filing_status = 2, age1 = 70, age2 = 68, agi = 80000,
                wages1 = 50000, ei1 = 50000, txbl_pens_dist = 30000),
           expect = list(st_agi = 60000),
           label = 'OK-9 retirement exclusion caps pooled across spouses')

  #--------------------------------------------------------------------------
  # DISTRICT OF COLUMBIA (D-40, Schedule S Calculations F / G / G-1 / J)
  #
  # DC runs ONE graduated schedule for every filing status, so the acceptance
  # test for the rate encoding is the published BASE AMOUNT at each bracket
  # knot: 28,150 at 350,000 under the 2017-2021 schedule and 42,775 at 500,000
  # under the 2022 restructure. Both reproduce exactly from the marginal rates,
  # which is why DC needs no base_amounts family.
  #--------------------------------------------------------------------------

  # DC-1 TY2019 single, AGI 60,000, standard deduction 12,200 (federal
  # conformity years), no exemptions after TCJA. Taxable 47,800:
  # 10,000 x 4% + 30,000 x 6% + 7,800 x 6.5% = 400 + 1,800 + 507 = 2,707
  run_case('DC', 2019,
           list(agi = 60000, wages1 = 60000, ei1 = 60000),
           expect = list(st_exempt = 0, st_txbl_inc = 47800,
                         liab_st_iit = 2707.00),
           label = 'DC-1 2019 single, federal-conformity standard deduction')

  # DC-2 the published base amount at the top knot of the 2022 schedule. AGI
  # 512,950 less the 12,950 deduction is exactly 500,000 of taxable income,
  # where the booklet prints a base of 42,775
  run_case('DC', 2022,
           list(agi = 512950, wages1 = 512950, ei1 = 512950),
           expect = list(st_txbl_inc = 500000, liab_st_iit = 42775.00),
           label = 'DC-2 2022 schedule reproduces the published 42,775 base')

  # DC-2b the same check on the 2017-2021 schedule, whose top knot is 350,000
  # with a published base of 28,150. The two schedules share their four lowest
  # bands; only the top end was restructured
  run_case('DC', 2021,
           list(agi = 362550, wages1 = 362550, ei1 = 362550),
           expect = list(st_txbl_inc = 350000, liab_st_iit = 28150.00),
           label = 'DC-2b 2021 schedule reproduces the published 28,150 base')

  # DC-3 the TY2017 exemption and its stepped phase-out: 2% of the allowance
  # per $2,500 (or fraction) of federal AGI over 150,000. At AGI 200,000 the
  # excess is 20 whole steps, so 40% is removed and 1,775 x 0.6 = 1,065 remains
  run_case('DC', 2017,
           list(agi = 200000, wages1 = 200000, ei1 = 200000),
           expect = list(st_exempt = 1065),
           label = 'DC-3 2017 exemption phased 40% at AGI 200,000')

  # DC-3b at 275,000 the 50 steps remove 100%, which is why the published
  # eligibility limit needs no separate parameter
  run_case('DC', 2017,
           list(agi = 275000, wages1 = 275000, ei1 = 275000),
           expect = list(st_exempt = 0),
           label = 'DC-3b 2017 exemption zeroes exactly at 275,000')

  # DC-3c a head of household gets an EXTRA exemption in 2017 -- two at 1,775
  # for the filer plus one for the dependent. Taxable 50,000 - 7,800 - 5,325
  run_case('DC', 2017,
           list(filing_status = 4, agi = 50000, wages1 = 50000, ei1 = 50000,
                n_dep = 1, dep_age1 = 10),
           expect = list(st_exempt = 5325, st_txbl_inc = 36875),
           label = 'DC-3c 2017 head-of-household extra exemption')

  # DC-4 THE CHILDLESS-WORKER CREDIT, which is an independent DC formula rather
  # than a match: 7.65% of earned income up to 649, reduced by 8.48% of the
  # excess over 23,288. At 25,000 of earned income that is
  # 649 - 0.0848 x 1,712 = 503.82
  run_case('DC', 2025,
           list(age1 = 40, agi = 25000, wages1 = 25000, ei1 = 25000),
           expect = list(st_earned_credit = 503.82),
           label = 'DC-4 2025 childless EITC on the DC formula')

  # DC-4b the age ceiling. A 66-year-old childless filer is ineligible for the
  # federal childless credit and for DC's, which the calculator could not
  # express until earned_credit_age_max landed
  run_case('DC', 2025,
           list(age1 = 66, agi = 8483, wages1 = 8483, ei1 = 8483),
           expect = list(st_earned_credit = 0),
           label = 'DC-4b childless credit denied above age 64')

  # DC-5 a filer WITH children takes the match and nothing from the independent
  # credit. This pair is the reason the match is loaded through the
  # child-count-keyed family with slot 1 zeroed: calc_st_credits SUMS the two,
  # so a scalar match would pay both to every childless recipient
  run_case('DC', 2024,
           list(agi = 20000, wages1 = 20000, ei1 = 20000, n_dep = 2,
                n_dep_eitc = 2, dep_age1 = 5, dep_age2 = 8, eitc = 5000),
           expect = list(st_eitc = 3500, st_earned_credit = 0),
           label = 'DC-5 2024 match at 70% for filers with children')

  # DC-5b TY2025 takes the match to 100% of the federal credit
  run_case('DC', 2025,
           list(agi = 20000, wages1 = 20000, ei1 = 20000, n_dep = 2,
                n_dep_eitc = 2, dep_age1 = 5, dep_age2 = 8, eitc = 5000),
           expect = list(st_eitc = 5000, st_earned_credit = 0),
           label = 'DC-5b 2025 match at 100%')

  # DC-6 the TY2025 dependent-filer decoupling, which the booklet settles: a
  # flat 15,000 where the federal worksheet would have given
  # min(15,000, max(1,300, earned + 450)). A dependent with 20,000 of earned
  # income keeps 5,000 of taxable income
  run_case('DC', 2025,
           list(dep_status = 1, agi = 20000, wages1 = 20000, ei1 = 20000),
           expect = list(st_ded = 15000, st_txbl_inc = 5000),
           label = 'DC-6 2025 dependent filer takes the flat 15,000')

  # DC-6b TY2024 still runs the federal worksheet, which caps at that year's
  # 14,600 standard deduction rather than the earned-income figure
  run_case('DC', 2024,
           list(dep_status = 1, agi = 20000, wages1 = 20000, ei1 = 20000),
           expect = list(st_ded = 14600, st_txbl_inc = 5400),
           label = 'DC-6b 2024 dependent filer on the federal worksheet')

  # DC-7 Social Security comes out in full at any age with no income test, and
  # DC has no pension exclusion of any kind
  run_case('DC', 2024,
           list(age1 = 70, agi = 50000, wages1 = 30000, ei1 = 30000,
                txbl_ss = 20000, gross_ss = 23000),
           expect = list(st_agi = 30000),
           label = 'DC-7 full Social Security subtraction at any age')

  # DC-8 the DC-specific itemized limitation: non-protected components reduced
  # by 5% of AGI over 200,000. Components are 5,000 medical + 30,000 mortgage
  # + 10,000 charity + 12,000 real property = 57,000, with income tax stripped.
  # Medical is protected, so 52,000 is exposed and the reduction is
  # 5% x 100,000 = 5,000, leaving 52,000. Taxable 248,000 gives
  # 400 + 1,800 + 1,300 + 8.5% x 188,000 = 19,480
  run_case('DC', 2019,
           list(agi = 300000, wages1 = 300000, ei1 = 300000, itemizing = 1,
                item_ded = 57000, item_ded_ex_limits = 57000,
                salt_item_ded = 10000, salt_prop = 12000,
                salt_inc_sales = 15000, med_item_ded = 5000,
                char_item_ded = 10000, mort_int_item_ded = 30000,
                std_ded = 12200),
           expect = list(st_ded = 52000, st_txbl_inc = 248000,
                         liab_st_iit = 19480.00),
           label = 'DC-8 2019 itemized limitation, 5% of AGI over 200,000')

  # DC-9 the care credit is 32% of the federal section 21 credit and
  # nonrefundable. Single with one young dependent at AGI 40,000: taxable
  # 27,800 gives 400 + 6% x 17,800 = 1,468 of tax, which absorbs the 192
  run_case('DC', 2019,
           list(agi = 40000, wages1 = 40000, ei1 = 40000, n_dep = 1,
                dep_age1 = 4, care_exp = 3000, cdctc_nonref = 600),
           expect = list(st_cdctc = 192, st_txbl_inc = 27800,
                         liab_st_iit = 1276.00),
           label = 'DC-9 2019 care credit at 32%, nonrefundable')

  # DC-10 state income tax refunds taxed federally come back out of the DC
  # base ("Taxable refunds, credits or offsets of state and local income
  # tax", Line 8 of the 2017 D-40, Line 9 of the 2020 D-40). Added 2026-08-15:
  # the missing subtraction was 76-86% of the fed-aligned state-AGI-stage
  # cross-model mismatches in every TAXSIM-window year
  run_case('DC', 2019,
           list(agi = 61500, wages1 = 60000, ei1 = 60000, state_ref = 1500),
           expect = list(st_agi = 60000, st_txbl_inc = 47800,
                         liab_st_iit = 2707.00),
           label = 'DC-10 2019 state income tax refund subtracted')

  #--------------------------------------------------------------------------
  # NEBRASKA (Form 1040N, Schedules I/II, Form 2441N)
  #--------------------------------------------------------------------------

  # NE-1 TY2017 single, AGI 50,000. Standard deduction 6,350 leaves 43,650 of
  # taxable income on the four-bracket schedule, less the $132 per-exemption
  # CREDIT (Nebraska's answer to a personal exemption is a credit, not an
  # allowance). Independently validated against the printed 2017 tax table: the
  # 43,860-43,960 row reads $2,147 and this schedule gives $2,147.45 at the
  # 43,910 midpoint
  run_case('NE', 2017,
           list(agi = 50000, wages1 = 50000, ei1 = 50000),
           expect = list(st_exempt = 0, st_txbl_inc = 43650,
                         st_percap_credit = 132, liab_st_iit = 1997.68),
           label = 'NE-1 2017 four-bracket schedule + per-exemption credit')

  # NE-2 THE 2017-ONLY ADDITIONAL TAX, a graduated-rate-benefit recapture keyed
  # to federal AGI. Above full phase-in the whole return is taxed at the top
  # rate, so the recapture is the constant 6.84% x B - T(B) for any taxable
  # income in the top bracket -- the published maximum of $855.99 for a single
  # filer. The pair isolates it: the same return with the trigger disabled pays
  # exactly that much less (the cent is floating-point, against a published
  # figure that is itself rounded)
  ne_high = list(agi = 600000, wages1 = 600000, ei1 = 600000)
  run_case('NE', 2017, ne_high,
           expect = list(st_txbl_inc = 593650, liab_st_iit = 40473.66),
           label = 'NE-2 2017 Additional Tax fully phased in')
  run_case('NE', 2017, ne_high,
           expect = list(liab_st_iit = 39617.68),
           law_overrides = list(st_ord.recapture_agi_start = Inf),
           label = 'NE-2b same return without the recapture: 855.98 less')

  # NE-3 TY2024 MFJ both aged 68, 28,000 of taxable Social Security and 120,000
  # of pension. The SS ramp reached 100% in TY2024 (LB 754), so all 28,000 comes
  # out; the standard deduction is 16,700 plus two aged boxes at 1,600 = 19,900,
  # matching the printed chart; and the top rate is 5.84%
  ne_retired = list(filing_status = 2, age1 = 68, age2 = 68, agi = 148000,
                    txbl_ss = 28000, gross_ss = 32000, txbl_pens_dist = 120000)
  run_case('NE', 2024, ne_retired,
           expect = list(st_agi = 120000, st_ded = 19900, st_txbl_inc = 100100,
                         liab_st_iit = 4105.32),
           label = 'NE-3 2024 full SS exclusion, two aged boxes, 5.84% top rate')

  # NE-3b the same couple under TY2022 law, where the percentage rule was only
  # 40% and the top rate still 6.84%: 11,200 of Social Security comes out,
  # taxable 119,300, tax 6,255.85 less two $146 credits
  run_case('NE', 2022, ne_retired,
           expect = list(st_agi = 136800, st_txbl_inc = 119300,
                         liab_st_iit = 5963.85),
           label = 'NE-3b 2022 SS at 40% and the pre-LB 754 top rate')

  # NE-4 THE SOCIAL SECURITY GREATER-OF, and the genuine cliff it creates. The
  # statute grants the better of a percentage rule and a full exclusion at or
  # below an AGI threshold ($61,760 joint in TY2022). At AGI 61,000 the
  # threshold rule wins and ALL 20,000 of benefits come out; at 65,000 only the
  # 40% percentage survives. A $4,000 increase in AGI raises Nebraska tax by
  # $561.60 -- that is the law, not an artifact
  run_case('NE', 2022,
           list(filing_status = 2, age1 = 68, age2 = 66, agi = 61000,
                wages1 = 41000, ei1 = 41000, txbl_ss = 20000, gross_ss = 23000),
           expect = list(st_agi = 41000, liab_st_iit = 460.82),
           label = 'NE-4 2022 SS threshold rule wins below the limit')
  run_case('NE', 2022,
           list(filing_status = 2, age1 = 68, age2 = 66, agi = 65000,
                wages1 = 45000, ei1 = 45000, txbl_ss = 20000, gross_ss = 23000),
           expect = list(st_agi = 57000, liab_st_iit = 1022.42),
           label = 'NE-4b 2022 only the 40% rule survives above the limit')

  # NE-5 the care credit spans two regimes on one share table: a decimal that
  # runs 1.00 at or below 22,000 down to 0.30 at 29,000, then the flat 25%
  # match above. The middle band is LINEARIZED (the form steps 10 points per
  # $1,000), so 25,500 gives 0.65 where the form gives 0.70 -- at most half a
  # step, which is the documented cost of fitting eight steps into the six-slot
  # share family
  ne_care = function(agi) {
    list(agi = agi, wages1 = agi, ei1 = agi, n_dep = 1, dep_age1 = 4,
         care_exp = 3000, cdctc_nonref = 1000)
  }
  run_case('NE', 2019, ne_care(20000),
           expect = list(st_cdctc = 1000),
           label = 'NE-5 care credit at 100% below 22,000')
  run_case('NE', 2019, ne_care(35000),
           expect = list(st_cdctc = 250),
           label = 'NE-5b care credit at the 25% match above 29,000')

  # NE-6 the per-exemption credit is NONREFUNDABLE. A single filer at AGI
  # 13,000 in TY2024 has 4,650 of taxable income and 122.27 of tax, which the
  # $166 credit can only zero -- not turn negative
  run_case('NE', 2024,
           list(agi = 13000, wages1 = 13000, ei1 = 13000),
           expect = list(st_txbl_inc = 4650, st_percap_credit = 166,
                         liab_st_iit = 0),
           label = 'NE-6 per-exemption credit stops at zero, not below')

  # NE-6b the same filer with a dependent and a federal EITC: the two credits
  # behave differently on the same return. Two $166 exemption credits absorb the
  # 192.47 of tax and stop there, while the 10% EITC match is REFUNDABLE and
  # takes the return to -300
  run_case('NE', 2024,
           list(agi = 15000, wages1 = 15000, ei1 = 15000, n_dep = 1,
                n_dep_eitc = 1, dep_age1 = 6, eitc = 3000),
           expect = list(st_percap_credit = 332, st_eitc = 300,
                         liab_st_iit = -300),
           label = 'NE-6b 10% EITC refundable against a nonrefundable credit')

  #--------------------------------------------------------------------------
  # Hawaii (N-11)
  #--------------------------------------------------------------------------

  # HI-1: 2018 single, wages 60,000, standard deduction. TI = 60,000 - 2,200
  # - 1,144 = 56,656 -> 3,213.60 + 8.25% x 8,656 = 3,927.72 (continuous
  # schedule; the printed base rounds to 3,214). Food credit denied
  # (single gate $30,000)
  run_case('HI', 2018, list(agi = 60000, wages1 = 60000, ei1 = 60000),
           expect = list(st_agi = 60000, liab_st_iit = 3927.72),
           label = 'HI-1 2018 twelve-bracket schedule, standard deduction')

  # HI-2: 2017 nine-bracket year, MFJ 66/64, wages 40,000 + pension 30,000 +
  # taxable SS 10,000 (FAGI 80,000). Pension and SS fully subtracted -> HI
  # AGI 40,000. Exemptions 2 x 1,144 + one aged extra = 3,432; std 4,400;
  # TI = 32,168 -> 1,363.20 + 6.8% x 3,368 = 1,592.22 (continuous). Food
  # credit: FAGI
  # 80,000 over the $50,000 gate -> 0
  run_case('HI', 2017,
           list(agi = 80000, filing_status = 2, age1 = 66, age2 = 64,
                wages1 = 24000, wages2 = 16000, ei1 = 24000, ei2 = 16000,
                txbl_pens_dist = 30000, txbl_ss = 10000, gross_ss = 12000),
           expect = list(st_agi = 40000, st_exempt = 3432,
                         liab_st_iit = 1592.22),
           label = 'HI-2 2017 schedule, pension/SS exclusions, aged exemption')

  # HI-3: 2024 single, wages 100,000 + net LTCG 100,000, standard deduction.
  # TI = 194,456. Regular tax 13,878.60 + 10% x 19,456 = 15,824.20. Alternative
  # (Tax on Capital Gains Worksheet): ordinary part max(TI - 100,000,
  # 24,000) = 94,456 -> 3,213.60 + 8.25% x 46,456 = 7,046.22; plus 7.25% x
  # 100,000 = 7,250 -> 14,296.22, the smaller -> tax (continuous schedule)
  run_case('HI', 2024,
           list(agi = 200000, wages1 = 100000, ei1 = 100000, kg_lt = 100000,
                kg_pref = 100000, txbl_kg = 100000),
           expect = list(st_tax_pre_credit = 14296.22,
                         liab_st_iit = 14296.22),
           label = 'HI-3 7.25% alternative capital-gains tax')

  # HI-3b: the worksheet's filing-status floor binds. 2024 single, wages
  # 20,000 + gain 30,000: TI = 44,456; ordinary part floored at 24,000
  # (schedule tax 1,353.60), alternative-rate part = 20,456 x 7.25% =
  # 1,483.06 -> 2,836.66 versus regular 2,933.62 (continuous)
  run_case('HI', 2024,
           list(agi = 50000, wages1 = 20000, ei1 = 20000, kg_lt = 30000,
                kg_pref = 30000, txbl_kg = 30000),
           expect = list(liab_st_iit = 2836.66),
           label = 'HI-3b alternative tax with the $24,000 ordinary floor')

  # HI-4: 2023 single, one dependent, wages 18,000, federal EITC 3,000. TI
  # = 18,000 - 2,200 - 2,288 = 13,512 -> 374.40 + 6.4% x 3,912 = 624.77
  # (continuous). HI
  # EITC 40% x 3,000 = 1,200 REFUNDABLE (Act 114/163); food credit: FAGI
  # 18,000 in the 15,000-19,999 band of the doubled 2023 table -> $200 x 2
  # persons = 400. Liability 624.77 - 1,600 = -975.23
  run_case('HI', 2023,
           list(agi = 18000, wages1 = 18000, ei1 = 18000, n_dep = 1,
                n_dep_eitc = 1, dep_age1 = 4, eitc = 3000),
           expect = list(st_eitc = 1200, st_percap_credit = 400,
                         liab_st_iit = -975.23),
           label = 'HI-4 refundable 40% EITC + doubled food credit')

  # HI-4b: the same unit in 2019 -- the 20% NONREFUNDABLE vintage and the
  # original food table. EITC 600 can only offset; food credit $70 x 2 =
  # 140 stays refundable. Tax 624.77 - 600 = 24.77, less 140 -> -115.23
  run_case('HI', 2019,
           list(agi = 18000, wages1 = 18000, ei1 = 18000, n_dep = 1,
                n_dep_eitc = 1, dep_age1 = 4, eitc = 3000),
           expect = list(st_eitc = 600, st_percap_credit = 140,
                         liab_st_iit = -115.23),
           label = 'HI-4b nonrefundable 20% EITC vintage')

  # HI-5: 2023 MFJ, two care-age dependents, wages 60,000/30,000, care
  # expenses 12,000. Rate floor 15% (HI AGI over 50,000); Act 163 cap
  # 20,000 for two, earned-income cap 30,000 -> credit 15% x 12,000 =
  # 1,800 refundable. TI = 90,000 - 4,400 - 4,576 = 81,024 -> 4,531.20 +
  # 7.9% x 9,024 = 5,244.10 (continuous); liability 3,444.10
  run_case('HI', 2023,
           list(agi = 90000, filing_status = 2, age2 = 40, wages1 = 60000,
                wages2 = 30000, ei1 = 60000, ei2 = 30000, n_dep = 2,
                dep_age1 = 3, dep_age2 = 5, care_exp = 12000),
           expect = list(st_cdctc = 1800, liab_st_iit = 3444.10),
           label = 'HI-5 refundable care credit at the 15% floor, Act 163 caps')

  # HI-5b: the sliding rate one band in. 2019 single, HI AGI 27,500 -> rate
  # 25% - 1 x 1% = 24%; cap 2,400 (one qualifying person) -> credit 576.
  # TI = 23,012 -> 1,008 + 7.2% x 3,812 = 1,282.46; food credit 55 x 2 =
  # 110; liability 1,282.46 - 576 - 110 = 596.46
  run_case('HI', 2019,
           list(agi = 27500, wages1 = 27500, ei1 = 27500, n_dep = 1,
                dep_age1 = 2, care_exp = 3000),
           expect = list(st_cdctc = 576, st_percap_credit = 110,
                         liab_st_iit = 596.46),
           label = 'HI-5b care-credit rate slide, pre-2023 expense cap')

  # HI-6: 2024 MFJ itemizer at FAGI 250,000: the state-income-tax deduction
  # is DENIED (over the $200,000 Worksheet A-2 threshold) and the overall
  # limitation bites. Base = 45,000 - 10,000 SALT + 8,000 property =
  # 43,000; reduction min(3% x (250,000 - 166,800), 80% x 43,000) = 2,496
  # -> 40,504. TI = 250,000 - 40,504 - 2,288 = 207,208 -> 6,427.20 + 8.25%
  # x 111,208 = 15,601.86 (continuous)
  run_case('HI', 2024,
           list(agi = 250000, filing_status = 2, age2 = 40, wages1 = 150000,
                wages2 = 100000, ei1 = 150000, ei2 = 100000, itemizing = 1,
                item_ded = 45000, item_ded_ex_limits = 45000,
                salt_item_ded = 10000, salt_inc_sales = 20000,
                salt_prop = 8000, mort_int_item_ded = 25000,
                char_item_ded = 10000, char_cash = 10000),
           expect = list(st_item_ded = 40504, liab_st_iit = 15601.86),
           label = 'HI-6 SALT disallowance over $200k + overall limitation')

  # HI-6b: the same deductions at FAGI 150,000 -- BELOW both the SALT
  # threshold and the $166,800 limitation, so income taxes stay deductible:
  # 43,000 + 20,000 = 63,000. TI = 84,712 -> 4,531.20 + 7.9% x 12,712 =
  # 5,535.45 (continuous)
  run_case('HI', 2024,
           list(agi = 150000, filing_status = 2, age2 = 40, wages1 = 90000,
                wages2 = 60000, ei1 = 90000, ei2 = 60000, itemizing = 1,
                item_ded = 45000, item_ded_ex_limits = 45000,
                salt_item_ded = 10000, salt_inc_sales = 20000,
                salt_prop = 8000, mort_int_item_ded = 25000,
                char_item_ded = 10000, char_cash = 10000),
           expect = list(st_item_ded = 63000, liab_st_iit = 5535.45),
           label = 'HI-6b state income tax deductible under the threshold')

  # HI-7: 2025 -- the FIRST Act 46 bracket step (printed in the 2025
  # booklet) with the 2024 standard deduction. Single wages 80,000: TI =
  # 74,456 -> 2,539.20 + 7.6% x 26,456 = 4,549.86 (continuous)
  run_case('HI', 2025, list(agi = 80000, wages1 = 80000, ei1 = 80000),
           expect = list(liab_st_iit = 4549.86),
           label = 'HI-7 Act 46 TY2025 bracket step')

  # HI-8: 2027 -- the second Act 46 bracket step plus the TY2026 standard-
  # deduction step ($8,000 single). TI = 80,000 - 8,000 - 1,144 = 70,856
  # -> 2,203.20 + 7.2% x 22,856 = 3,848.83 (continuous; enacted future law)
  run_case('HI', 2027, list(agi = 80000, wages1 = 80000, ei1 = 80000),
           expect = list(liab_st_iit = 3848.83),
           label = 'HI-8 Act 46 TY2027 schedule + TY2026 standard deduction')

  #--------------------------------------------------------------------------
  # Maine (1040ME)
  #--------------------------------------------------------------------------

  # ME-1: 2024 single, wages 50,000, standard deduction (= federal 14,600),
  # exemption 5,000. TI = 30,400 -> 1,510.90 + 6.75% x 4,350 = 1,804.53
  # (continuous schedule; the printed base rounds to 1,511)
  run_case('ME', 2024, list(agi = 50000, wages1 = 50000, ei1 = 50000),
           expect = list(st_agi = 50000, liab_st_iit = 1804.53),
           label = 'ME-1 2024 basic single')

  # ME-2: 2017 regime -- the exemption is the FEDERAL amount including
  # dependents (4 x 4,050 = 16,200) and the std is Maine's own 23,200. MFJ
  # wages 100,000, two dependents: TI = 60,600 -> 2,450.50 + 6.75% x
  # 18,350 = 3,689.13 (continuous; no DETC in 2017; STFC zero here)
  run_case('ME', 2017,
           list(agi = 100000, filing_status = 2, age2 = 40, wages1 = 60000,
                wages2 = 40000, ei1 = 60000, ei2 = 40000, n_dep = 2,
                dep_age1 = 5, dep_age2 = 9),
           expect = list(st_exempt = 16200, liab_st_iit = 3689.13),
           label = 'ME-2 2017 federal-style exemptions incl. dependents')

  # ME-3: the deduction phase-out. 2024 single, wages 120,000: excess over
  # 97,150 = 22,850 -> ratio 0.304667 -> std 14,600 x 0.695333 = 10,151.87.
  # TI = 104,848.13 -> 3,910.53 + 7.15% x 43,248.13 = 7,002.77 (continuous)
  run_case('ME', 2024, list(agi = 120000, wages1 = 120000, ei1 = 120000),
           expect = list(st_std_ded = 10151.87, liab_st_iit = 7002.77),
           label = 'ME-3 linear deduction phase-out on the standard deduction')

  # ME-4: itemized cap + phase-out interaction, in the worksheet's order
  # (cap FIRST, then the whole-deduction phase-out). 2024 MFJ, FAGI
  # 250,000, federal itemized 52,000 of which medical 4,000 / SALT 10,000
  # capped (income 15,000, property 12,000) / mortgage 30,000 / charity
  # 8,000. Maine base = 52,000 - 10,000 + 12,000 = 54,000; cap 35,250 with
  # medical exempt -> 39,250; phase-out (250,000 - 194,300)/150,000 =
  # 0.371333 -> 24,675.17 (the phased standard, 18,357.07, loses the
  # best-of election). TI = 215,324.83 -> 7,824.43 + 7.15% x 92,074.83 =
  # 14,407.78 (continuous)
  run_case('ME', 2024,
           list(agi = 250000, filing_status = 2, age2 = 40, wages1 = 150000,
                wages2 = 100000, ei1 = 150000, ei2 = 100000, itemizing = 1,
                item_ded = 52000, item_ded_ex_limits = 52000,
                salt_item_ded = 10000, salt_inc_sales = 15000,
                salt_prop = 12000, med_item_ded = 4000,
                mort_int_item_ded = 30000, char_item_ded = 8000,
                char_cash = 8000),
           expect = list(st_item_ded = 24675.17, liab_st_iit = 14407.78),
           label = 'ME-4 itemized cap (medical exempt) then phase-out')

  # ME-5: 2024 pension deduction under the NEW $45,864 per-person cap (the
  # maximum-SS-benefit rule -- NOT the superseded $35,000), reduced by
  # GROSS SS received. MFJ both 68: pensions 60,000 + IRA 10,000 (IRAs
  # eligible) + taxable SS 20,000 of 30,000 gross. Caps 91,728 - 30,000 =
  # 61,728 -> deduction 61,728; SS fully subtracted -> ME AGI 8,272; std
  # 29,200 + 2 x 1,550 aged = 32,300 -> TI 0. STFC income = FAGI + nontax
  # SS = 100,000 -> 0
  run_case('ME', 2024,
           list(agi = 90000, filing_status = 2, age1 = 68, age2 = 68,
                txbl_pens_dist = 60000, txbl_ira_dist = 10000,
                gross_ss = 30000, txbl_ss = 20000),
           expect = list(st_agi = 8272, liab_st_iit = 0),
           label = 'ME-5 SS-max pension cap less gross SS, aged std add-ons')

  # ME-6: the TY2025 pension-deduction phase-out (P.L. 2025 c. 388 Pt. H).
  # Single 70, pension 60,000 + wages 100,000: capped deduction 48,216 x
  # (1 - 35,000/100,000) = 31,340.40 -> ME AGI 128,659.60. Deduction
  # phase-out: excess 28,659.60/75,000 -> std 17,000 x 0.617867 =
  # 10,503.73; exemption 5,150. TI = 113,005.87 -> 4,028.28 + 7.15% x
  # 49,555.87 = 7,571.52 (continuous)
  run_case('ME', 2025,
           list(agi = 160000, age1 = 70, wages1 = 100000, ei1 = 100000,
                txbl_pens_dist = 60000),
           expect = list(st_agi = 128659.60, liab_st_iit = 7571.52),
           label = 'ME-6 2025 pension phase-out + phased aged std deduction')

  # ME-7: the 2022 credit stack. HoH, two children (4/8), wages 25,000,
  # federal EITC 5,000, care credit base 800. TI = 25,000 - 19,400 - 4,450
  # = 1,150 -> tax 66.70. ME EITC 25% (2+ kids) = 1,250 refundable; DETC
  # 300 x 2 = 600 NONREFUNDABLE (zeroes the tax); care credit 25% x 800 =
  # 200, within the $500 refundable cap; STFC HoH/2 deps at income 25,000
  # = 210 (table). Liability = 0 - 1,250 - 200 - 210 = -1,660
  run_case('ME', 2022,
           list(agi = 25000, filing_status = 4, wages1 = 25000, ei1 = 25000,
                n_dep = 2, n_dep_eitc = 2, n_dep_ctc = 2, dep_age1 = 4,
                dep_age2 = 8, eitc = 5000, cdctc_nonref = 800,
                care_exp = 3000),
           expect = list(st_eitc = 1250, st_ctc = 600, st_cdctc = 200,
                         st_stfc = 210, liab_st_iit = -1660),
           label = 'ME-7 EITC/DETC/care/STFC stack, 2022 vintages')

  # ME-8: the TY2025 DETC restructure -- $610 under-6 + $305 for 6+, with
  # the new $20-per-$500 phase-out over $150,000 (MFJ). Children 3 and 9,
  # wages 160,000: credit 915 - 20 x 20 steps = 515, refundable. TI =
  # 160,000 - 30,000 - 10,300 = 119,700 -> 3,108.80 + 6.75% x 66,100 =
  # 7,570.55 (continuous); liability 7,055.55
  run_case('ME', 2025,
           list(agi = 160000, filing_status = 2, age2 = 40, wages1 = 100000,
                wages2 = 60000, ei1 = 100000, ei2 = 60000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 3, dep_age2 = 9),
           expect = list(st_ctc = 515, liab_st_iit = 7055.55),
           label = 'ME-8 2025 DETC: doubled under-6 amount, $500-step phase-out')

  # ME-9: STFC alone on a childless single, 2024 -- income 26,000 sits in
  # the 25,751-26,250 band ($10-per-$500 decrements from the $150 base) =
  # 120, refundable. TI = 6,400 -> tax 371.20; liability 251.20
  run_case('ME', 2024, list(agi = 26000, wages1 = 26000, ei1 = 26000),
           expect = list(st_stfc = 120, liab_st_iit = 251.20),
           label = 'ME-9 sales tax fairness credit, childless single')

  #--------------------------------------------------------------------------
  # GENERIC MACHINERY CASES
  #
  # These prove parameters that no encoded state consumes yet. Each was added
  # for a batch-C state whose research showed no existing parameter could
  # express its provision, and each is exercised here by overriding the named
  # parameter on a HOST state's law row (see run_case's law_overrides). The
  # rest of the suite proves the additions are neutral at their defaults;
  # these prove they actually work, so the first state to use one is not also
  # the first test of it. Retire a case once its own state's tests cover it.
  #--------------------------------------------------------------------------

  # MACH-1: flat-dollar capital gains exclusion, capped at the eligible gain
  # (VT 32 V.S.A. 5811(21)(B)(ii): a flat $5,000). Host IL 2024, whose own
  # subtractions are zero for this unit, so the exclusion is isolated:
  # min(5,000, gain 3,000) = 3,000
  run_case('IL', 2024, list(agi = 60000, kg_lt = 3000, txbl_inc = 50000),
           expect = list(st_subtractions = 3000),
           law_overrides = list(st_agi.cap_gains_excl_flat = 5000),
           label = 'MACH-1 flat capital-gains exclusion, capped at the gain')

  # MACH-2: the GREATER of the flat amount and the share, then the
  # federal-taxable-income CEILING (VT IN-153 Part III: 40% of federal
  # taxable income). Gain 20,000 -> max(40% x 20,000 = 8,000,
  # min(5,000, 20,000) = 5,000) = 8,000, then capped at 40% x 15,000 = 6,000
  run_case('IL', 2024, list(agi = 60000, kg_lt = 20000, txbl_inc = 15000),
           expect = list(st_subtractions = 6000),
           law_overrides = list(st_agi.cap_gains_excl_flat = 5000,
                                st_agi.cap_gains_excl_share = 0.40,
                                st_agi.cap_gains_excl_txbl_share = 0.40),
           label = 'MACH-2 greater-of exclusion under the taxable-income ceiling')

  # MACH-3: charitable contribution CREDIT on capped contributions, available
  # without itemizing (VT 5822(d)(3): 5% of the first $20,000, max $1,000).
  # 30,000 of contributions -> 5% x 20,000 = 1,000
  run_case('IL', 2024, list(agi = 60000, char_cash = 30000),
           expect = list(st_char_credit = 1000),
           law_overrides = list(st_credits.char_credit_rate = 0.05,
                                st_credits.char_credit_base_cap = 20000),
           label = 'MACH-3 charitable credit on capped contributions')

  # MACH-4/MACH-4b: flat-dollar itemized cap with exempt components
  # (OK 68 O.S. 2358(E)(3)(b): $17,000, charity and medical exempt). Host
  # MD 2019, whose itemized total for this unit is medical 10,000 + mortgage
  # 20,000 + charity 30,000 + property min(15,000, 10,000) = 70,000.
  # With medical and charity exempt: exempt 40,000, remainder 30,000 capped to
  # 17,000 -> 57,000. With nothing exempt: min(17,000, 70,000) = 17,000
  md_item_unit = list(agi = 200000, itemizing = 1, item_ded = 70000,
                      item_ded_ex_limits = 70000, med_item_ded = 10000,
                      mort_int_item_ded = 20000, char_item_ded = 30000,
                      salt_item_ded = 10000, salt_inc_sales = 25000,
                      salt_prop = 15000, std_ded = 24400)
  run_case('MD', 2019, md_item_unit,
           expect = list(st_item_ded = 57000),
           law_overrides = list(st_ded.item_flat_cap = 17000,
                                st_ded.item_flat_cap_excl_medical = 1,
                                st_ded.item_flat_cap_excl_charity = 1),
           label = 'MACH-4 flat itemized cap with medical and charity exempt')
  run_case('MD', 2019, md_item_unit,
           expect = list(st_item_ded = 17000),
           law_overrides = list(st_ded.item_flat_cap = 17000),
           label = 'MACH-4b flat itemized cap with nothing exempt')

  # MACH-5: per-dependent deduction excluding the FIRST dependent
  # (NM 7-2-39: "Subtract 1 from total dependents"). Host NC 2024, whose
  # table gives 2,500 per child at 50,000 of joint AGI: 3 dependents pay
  # 7,500 without the offset and 5,000 with it
  run_case('NC', 2024,
           list(filing_status = 2, age2 = 40, agi = 50000, n_dep = 3,
                n_dep_ctc = 3, dep_age1 = 4, dep_age2 = 8, dep_age3 = 10),
           expect = list(st_child_ded = 5000),
           law_overrides = list(st_child_ded.count_offset = 1),
           label = 'MACH-5 child deduction excluding the first dependent')

  # MACH-6: upper age bound on the childless earned-income credit (DC's
  # worksheet: "at least age 25, but not age 65"). Host CA 2024, where a
  # childless 70-year-old with 10,000 of wages otherwise receives $202 of
  # CalEITC; the ceiling makes them ineligible
  run_case('CA', 2024, list(agi = 10000, age1 = 70, wages1 = 10000,
                            ei1 = 10000),
           expect = list(st_earned_credit = 0),
           law_overrides = list(st_credits.earned_credit_age_max = 64),
           label = 'MACH-6 childless earned-credit age ceiling')

  # MACH-7: a SEVEN-tier AGI-tiered child credit (NM 7-2-18.34 publishes seven
  # bands where the calculator used to hard-code three). Host NC 2017, whose
  # own ladder is replaced here by NM's TY2023 table. AGI 120,000 lands in the
  # fifth band (100,001-200,000) at $75 per child, x 2 children = 150.
  # Before the n-tier generalization a fourth bound was accepted by the
  # parameter-name validator and then silently ignored, so this unit would
  # have received 0
  nm_tiers = list(st_credits.ctc_tier1_bound = 25000,
                  st_credits.ctc_tier2_bound = 50000,
                  st_credits.ctc_tier3_bound = 75000,
                  st_credits.ctc_tier4_bound = 100000,
                  st_credits.ctc_tier5_bound = 200000,
                  st_credits.ctc_tier6_bound = 350000,
                  st_credits.ctc_tier7_bound = Inf,
                  st_credits.ctc_tier_amounts1 = 600,
                  st_credits.ctc_tier_amounts2 = 400,
                  st_credits.ctc_tier_amounts3 = 200,
                  st_credits.ctc_tier_amounts4 = 100,
                  st_credits.ctc_tier_amounts5 = 75,
                  st_credits.ctc_tier_amounts6 = 50,
                  st_credits.ctc_tier_amounts7 = 25)
  run_case('NC', 2017,
           list(filing_status = 2, age2 = 40, agi = 120000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 5, dep_age2 = 8, std_ded = 12700),
           expect = list(st_ctc = 150),
           law_overrides = nm_tiers,
           label = 'MACH-7 seven-tier child credit selects the fifth band')

  # MACH-7b: the top tier is unbounded (NM's seventh band is "over $350,000"),
  # so a $2,000,000 unit still receives $25 per child rather than dropping to
  # zero the way a state with a finite top bound does. This is the semantic
  # that stops st_band_index_upper from being usable here: CO must fall to
  # zero above its last bound, NM must not
  run_case('NC', 2017,
           list(filing_status = 2, age2 = 40, agi = 2000000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 5, dep_age2 = 8, std_ded = 12700),
           expect = list(st_ctc = 50),
           law_overrides = nm_tiers,
           label = 'MACH-7b unbounded top tier stays eligible')

  # MACH-7c: the same host with only THREE tiers declared must still fall to
  # zero above its third bound -- the case that a naive tier count would break
  # by selecting a fourth tier out of a frame widened by another state
  run_case('NC', 2017,
           list(filing_status = 2, age2 = 40, agi = 120000, n_dep = 2,
                n_dep_ctc = 2, dep_age1 = 5, dep_age2 = 8, std_ded = 12700),
           expect = list(st_ctc = 0),
           law_overrides = list(st_credits.ctc_tier1_bound = 25000,
                                st_credits.ctc_tier2_bound = 50000,
                                st_credits.ctc_tier3_bound = 75000,
                                st_credits.ctc_tier_amounts1 = 600,
                                st_credits.ctc_tier_amounts2 = 400,
                                st_credits.ctc_tier_amounts3 = 200),
           label = 'MACH-7c three declared tiers stay ineligible above the top')

  # MACH-8: per-child phase-out (VT's CTC pays $1,000 per child age 5 or under,
  # reduced by $20 per $1,000 of AGI over $125,000, the reduction applied to
  # EACH child's amount -- source_packets/vt.md). Host NY 2025, the style-2
  # state, with VT's table substituted. MFJ, two children aged 2 and 4, AGI
  # $145,000: excess 20,000 -> 20 steps x $20 = $400 reduction, so per-child
  # gives 2 x (1000 - 400) = 1,200 where reducing the aggregate once gives
  # 2,000 - 400 = 1,600
  vt_ctc = list(st_credits.ctc_style = 2,
                st_credits.ctc_tier1_bound = NA_real_,
                st_credits.ctc_young_age_limit = 5,
                st_credits.ctc_young_amount = 1000,
                st_credits.ctc_old_amount = 0,
                st_credits.ctc_max_child_age = 16,
                st_credits.ctc_po_thresh = 125000,
                st_credits.ctc_po_rate = 0.02,
                st_credits.ctc_po_base = 1,
                st_credits.ctc_po_per_child = 1)
  vt_unit = function(agi) {
    list(filing_status = 2, age2 = 40, agi = agi, wages1 = agi, ei1 = agi,
         n_dep = 2, n_dep_ctc = 2, dep_age1 = 2, dep_age2 = 4)
  }
  run_case('NY', 2025, vt_unit(145000),
           expect = list(st_ctc = 1200),
           law_overrides = vt_ctc,
           label = 'MACH-8 per-child phase-out reduces each child separately')

  # MACH-8b: the floor is what makes the distinction matter. At AGI $185,000 the
  # reduction is 60 x $20 = $1,200, exceeding one child's $1,000, so per-child
  # zeroes the credit -- matching the statute's full phase-out at $175,000.
  # Reducing the aggregate would still pay 2,000 - 1,200 = $800 to a filer VT
  # has phased out entirely
  run_case('NY', 2025, vt_unit(185000),
           expect = list(st_ctc = 0),
           law_overrides = vt_ctc,
           label = 'MACH-8b per-child phase-out floors at zero per child')

  # MACH-8c: VT reduces "$20 for each $1,000, OR FRACTION THEREOF" while NY 2025
  # rounds the style-2 excess down, so the two need separate control. AGI
  # $145,500 is a partial 21st step: rounding up gives 21 x $20 = $420 and
  # 2 x (1000 - 420) = 1,160; MACH-8's exact-multiple case cannot see the
  # difference, which is why this one uses a fraction
  run_case('NY', 2025, vt_unit(145500),
           expect = list(st_ctc = 1160),
           law_overrides = c(vt_ctc, list(st_credits.ctc_po_round_up = 1)),
           label = 'MACH-8c stepped phase-out counts a partial step whole')

  # MACH-8d: the same return with the default rounding stays on 20 whole steps
  # ($400), so the flag is shown changing the answer rather than merely parsing
  run_case('NY', 2025, vt_unit(145500),
           expect = list(st_ctc = 1200),
           law_overrides = vt_ctc,
           label = 'MACH-8d default rounding drops the partial step')

  # MACH-9: child and care credits as ALTERNATIVES (OK 68 O.S. 2357.43 grants
  # the greater of 20% of the federal CDCC and 5% of the federal CTC). Same unit
  # in both directions, so the election is shown choosing each leg in turn.
  # NY 2025's style-2 CTC pays 1,330 against a 1,200 care credit, so the care
  # leg is zeroed
  greater_of_unit = list(filing_status = 2, age2 = 40, agi = 40000,
                         wages1 = 25000, ei1 = 25000, wages2 = 15000,
                         ei2 = 15000, n_dep = 2, n_dep_ctc = 2, dep_age1 = 2,
                         dep_age2 = 4, care_exp = 6000, cdctc_nonref = 1200,
                         cdctc_ref = 0, ctc_nonref = 4000, ctc_ref = 0)
  run_case('NY', 2025, greater_of_unit,
           expect = list(st_ctc = 1330, st_cdctc = 0),
           law_overrides = list(st_credits.ctc_cdctc_greater_of = 1),
           label = 'MACH-9 greater-of keeps the larger child credit')

  # MACH-9b: the same unit under NY 2017, whose style-1 CTC pays only 330, so
  # the election runs the other way and the child leg is zeroed. Both legs stay
  # reported as claimed, which is what lets the downstream refundable /
  # nonrefundable split stay ignorant of the election
  run_case('NY', 2017, greater_of_unit,
           expect = list(st_ctc = 0, st_cdctc = 1200),
           law_overrides = list(st_credits.ctc_cdctc_greater_of = 1),
           label = 'MACH-9b greater-of keeps the larger care credit')

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
        # Earner split and care expenses so two-earner machinery (JFC,
        # marriage/two-earner credits, combined_sep) and CDCTC families
        # activate in the coverage layer below
        wages1 = pmax(0, agi_level) * if (filing_status == 2) 0.6 else 1,
        wages2 = pmax(0, agi_level) * if (filing_status == 2) 0.4 else 0,
        ei1    = pmax(0, agi_level) * if (filing_status == 2) 0.6 else 1,
        ei2    = pmax(0, agi_level) * if (filing_status == 2) 0.4 else 0,
        cdctc_nonref = if (n_dep > 0 & agi_level > 0) 400 else 0,
        care_exp     = if (n_dep > 0 & agi_level > 0) 3000 else 0,
        std_ded = 14600
      ))
    })

  # NH/TN (narrow investment taxes) and WA (LTCG excise + WFTC) are swept
  # with everyone else; their structural assertion runs on the net
  # individual liability, which routes through their special programs
  smoke_states = c('IL', 'CO', 'NY', 'AZ', 'GA', 'NC', 'IN', 'KY', 'MI',
                   'CA', 'ND', 'SC', 'CT', 'VA', 'UT', 'OH', 'PA', 'ID',
                   'MN', 'MD', 'WI', 'NH', 'TN', 'WA', 'KS', 'DE', 'RI',
                   'WV', 'NM', 'VT', 'OK', 'DC', 'NE', 'HI', 'ME')
  smoke_active = list()
  for (st in smoke_states) {
    active = character()
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
        'smoke: NA net liab'   = !anyNA(out$liab_st_individual_net),
        'smoke: infinite net'  = all(is.finite(out$liab_st_individual_net)),
        'smoke: NA filer flag' = !anyNA(out$st_filer)
      )
      active = union(active, coverage_outputs[
        map_lgl(coverage_outputs, ~ any(abs(out[[.x]]) > 1e-9, na.rm = T))
      ])
    }
    smoke_active[[st]] = active
  }
  message('test_state_calc smoke grid: PASSED (', nrow(grid), ' units x ',
          length(smoke_states), ' states x 5 years)')

  #--------------------------------------------------------------------------
  # Worksheet coverage (review item #9a): any credit family the smoke grid
  # activates for a state must also be exercised (nonzero) by at least one
  # of that state's hand-computed worksheet cases. Waivers name pairs where
  # the family is pinned some other way or synthetic inputs cannot reach
  # it; each carries a reason and the list should shrink over time.
  #--------------------------------------------------------------------------
  coverage_waivers = list()
  coverage_gaps = character()
  for (st in smoke_states) {
    exercised = c(case_exercised$sets[[st]], coverage_waivers[[st]])
    missing = setdiff(smoke_active[[st]], exercised)
    if (length(missing) > 0) {
      coverage_gaps = c(coverage_gaps,
                        paste0(st, ': ', paste(missing, collapse = ' ')))
    }
  }
  if (length(coverage_gaps) > 0) {
    stop('worksheet coverage gaps (smoke-active credit families with no ',
         'exercising hand-computed case):\n  ',
         paste(coverage_gaps, collapse = '\n  '))
  }
  message('test_state_calc worksheet coverage: PASSED')

  #--------------------------------------------------------------------------
  # Continuity sweep (review item #9b): pre-refundable-transfer liability
  # for a single wage-only filer must move by no more than a per-state
  # jump allowance per $500 of AGI. The default allowance covers the
  # steepest legitimate combined slope (bracket rate + credit phase-out,
  # e.g. MN's 5.35% + 9% childless WFC phase-out); states with published
  # discontinuities carry documented allowances. Anything larger is a
  # mis-encoded bracket bound, band edge, or cliff.
  #--------------------------------------------------------------------------
  sweep_step = 500
  sweep_agis = seq(0, 300000, by = sweep_step)
  sweep_grid = map_dfr(sweep_agis, function(x) {
    st_test_unit(list(agi = x, txbl_inc = pmax(0, x - 14600),
                      age1 = 30, wages1 = x, ei1 = x, std_ded = 14600))
  })
  sweep_allow = c(
    IL = 175,   # exemption disallowance cliff at $250k (2,775 x 4.95% + step)
    NY = 350,   # recapture segment entry at the 215,400 bracket: +327.45
                #   verified worksheet-true 2026-08-11 (supplemental-tax
                #   incremental benefit 1,830.90 x 0.162 phase + rate step;
                #   our endpoints match the IT-201 computation exactly and
                #   PE 1.775.7 concurs within cents)
    KY = 160,   # Table C family-size band edges at low MGI
    CT = 200,   # Table A exemption steps + Table D stepped recapture
                #   ($122.50/segment observed, pinned by CT-8)
    VA = 320,   # no-tax-below cliff (full tax owed at the VAGI threshold)
    RI = 225,   # stepped std-deduction AND exemption phase-out (R.I.G.L.
                #   44-30-2.6: 20% of each per increment of modified AGI over
                #   one shared threshold, so both drop together at every
                #   boundary): 0.20 x (10,550 + 4,950) = 3,100 of base at
                #   5.99% = 185.69 in 2024, plus the marginal step; four such
                #   boundaries plus the zero-out cliff. Pinned by RI-2/RI-2b
    KS = 175,   # statutory low-income zero-tax cliff through 2023 (K.S.A.
                #   79-32,110: "not over $X: 0%", next band taxes the FULL
                #   amount; single $145 at TI 5,000 in 2017, $77.50 at
                #   2,500 in 2018-23; booklet-table-verified; KS-2/2b)
    OH = 400,   # zero-bracket base-amount cliff (statutory; tax owed on
                #   the first taxed dollar from 2019, OH-3)
    MD = 130,   # banded exemption steps at $100k/$125k/$150k
    HI = 160    # N-311 food-credit band edges (published cliffs: the single
                #   table ends at $40,000 in the 2023-2027 vintage, dropping
                #   $110 for the one-person sweep unit, plus the bracket
                #   slope; the 2017 vintage's largest edge is $55 at $30,000)
  )
  sweep_default = sweep_step * 0.20 + 5
  for (st in smoke_states) {
    allow = max(sweep_default, sweep_allow[st], na.rm = T)
    for (yr in c(2017, 2024)) {
      law_slice = law %>%
        filter(state == st, year == yr) %>%
        select(-state, -year)
      out = sweep_grid %>%
        left_join(law_slice, by = 'filing_status') %>%
        do_state_taxes(
          credit_tables = state_credit_tables_for_year(credit_tables, st, yr)
        )
      liab = out$liab_st_iit + out$liab_st_narrow_iit + out$liab_st_ltcg_excise
      jumps = abs(diff(liab))
      if (any(jumps > allow)) {
        b = which.max(jumps)
        stop(sprintf(
          'continuity: %s %s jumps %.2f (> allowance %.0f) at AGI %d -> %d',
          st, yr, jumps[b], allow, sweep_agis[b], sweep_agis[b + 1]))
      }
    }
  }
  message('test_state_calc continuity sweep: PASSED (',
          length(sweep_agis), ' points x ', length(smoke_states),
          ' states x 2 years)')

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
    txbl_kg = 0, kg_pref = 0, wages1 = 0, wages2 = 0, sole_prop = 0, part_active = 0,
    part_passive = 0, scorp = 0, farm = 0, rent = 0, other_gains = 0,
    alimony = 0, other_inc = 0,
    sch_e = 0, part_scorp = 0, ei1 = 0, ei2 = 0, n_dep_eitc = 0,
    txbl_pens_dist = 0,
    txbl_ira_dist = 0, ot_ded = 0, hsa_contr = 0,
    char_cash = 0, char_noncash = 0,
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
  # As-if-itemizing amounts mirror the plain component overrides unless a
  # case sets the _potential name itself (production: do_taxes.R preserves
  # these before zeroing the as-claimed columns for non-itemizers)
  for (v in c('item_ded', 'item_ded_ex_limits', 'salt_item_ded',
              'med_item_ded', 'mort_int_item_ded', 'inv_int_item_ded',
              'casualty_item_ded', 'char_item_ded', 'misc_item_ded',
              'other_item_ded')) {
    pot = paste0(v, '_potential')
    if (is.null(unit[[pot]])) {
      unit[[pot]] = unit[[v]]
    }
  }
  as_tibble(unit)
}
