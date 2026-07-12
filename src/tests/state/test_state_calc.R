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
    states  = c('IL', 'CO', 'NY'),
    years   = 2017:2035,
    indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
              mutate(growth = 0.025)
  )

  run_case = function(st, yr, unit_overrides, expect, tol = 0.01, label = '') {

    unit = st_test_unit(unit_overrides)
    law_row = law %>%
      filter(state == st, year == yr,
             filing_status == unit$filing_status) %>%
      select(-state, -year, -filing_status)
    stopifnot('law row missing' = nrow(law_row) == 1)

    result = unit %>%
      bind_cols(law_row) %>%
      do_state_taxes()

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
                         liab_st_iit = 47225 * 0.0495),
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
  # Structural smoke test: a coarse grid of units through every pilot state
  # and several years must produce finite, non-NA results
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

  for (st in c('IL', 'CO', 'NY')) {
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
  message('test_state_calc smoke grid: PASSED (', nrow(grid), ' units x 3 states x 5 years)')

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

  unit = list(
    filing_status = 1, filer = TRUE, dep_status = FALSE,
    age1 = 40, age2 = NA_integer_, blind1 = FALSE, blind2 = NA,
    n_dep = 0, dep_age1 = NA_integer_, dep_age2 = NA_integer_,
    dep_age3 = NA_integer_,
    agi = 0, txbl_inc = 0, itemizing = FALSE,
    exempt_int = 0, state_ref = 0, txbl_ss = 0, txbl_pens_dist = 0,
    txbl_ira_dist = 0, ot_ded = 0, char_cash = 0, char_noncash = 0,
    item_ded = 0, item_ded_ex_limits = 0, salt_item_ded = 0,
    salt_inc_sales = 0, salt_prop = 0, salt_pers = 0,
    med_item_ded = 0, inv_int_item_ded = 0, casualty_item_ded = 0,
    char_item_ded = 0, std_ded = 0,
    eitc = 0, ctc_nonref = 0, ctc_ref = 0, cdctc_nonref = 0,
    cdctc_ref = 0, care_exp = 0
  )
  for (v in names(overrides)) {
    unit[[v]] = overrides[[v]]
  }
  as_tibble(unit)
}
