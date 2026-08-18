#---------------------------------------------------------------
# Function to calculate state deductions and deduction addbacks
#---------------------------------------------------------------

# Set return variables for function
return_vars$calc_st_ded = c('st_item_ded', 'st_std_ded', 'st_std_char_add',
                            'st_itemizing', 'st_ded', 'st_addback',
                            'st_fed_tax_ded', 'st_retire_exempt')


calc_st_ded = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates the state deduction (standard vs itemized under the state's
  # coupling rule, with state itemized adjustments and limitations) and any
  # deduction ADDBACKS for federal-taxable-income-start states (CO's state
  # income tax addback and high-income deduction addback).
  #
  # State itemized base (v1, documented known-difference): federal
  # pre-limitation itemized total with the capped SALT component replaced by
  # uncapped property/personal-property taxes (income and sales taxes
  # excluded where salt_addback = 1). Pre-TCJA-only components (misc 2%-floor
  # deductions) are data-limited and not reconstructed.
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_item_ded (dbl)  : state itemized deduction after limitations
  #   - st_std_ded (dbl)   : state standard deduction
  #   - st_itemizing (bool): whether the unit itemizes at the state level
  #   - st_ded (dbl)       : state deduction amount (subtracted from base)
  #   - st_addback (dbl)   : deduction addbacks (added to base; CO-style)
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',                # (dbl)  federal AGI
    'st_agi',             # (dbl)  state income base (calc_st_agi)
    'filing_status',      # (int)  1 single, 2 MFJ, 3 MFS, 4 HoH
    'itemizing',          # (bool) whether unit itemizes federally
    'dep_status',         # (bool) whether filer is a dependent

    # Federal liability inputs for the federal income tax deduction
    'liab_bc',            # (dbl)  federal tax before credits, incl. AMT and excess APTC
    'nonref',             # (dbl)  federal nonrefundable credits (limited to liab_bc)
    'eitc',               # (dbl)  federal earned income credit
    'ed_ref',             # (dbl)  refundable education credit (AOC)
    'net_ptc',            # (dbl)  net premium tax credit
    'ctc_ref',            # (dbl)  additional (refundable) child tax credit
    'liab_niit',          # (dbl)  net investment income tax
    'liab_pr_ee',         # (dbl)  employee share of payroll taxes (MO itemized add-on)
    'liab_seca',          # (dbl)  self-employment tax (MO itemized add-on)

    # Retirement exemption inputs (states taking it as a deduction, not an
    # AGI subtraction)
    'txbl_ss',            # (dbl)  federally taxable Social Security benefits
    'txbl_pens_dist',     # (dbl)  taxable pension distributions
    'txbl_ira_dist',      # (dbl)  taxable IRA distributions
    'item_ded',           # (dbl)  federal itemized deductions as claimed (addback base)
    'salt_item_ded',      # (dbl)  federal SALT deduction as claimed (addback base)
    'salt_inc_sales',     # (dbl)  state/local income-or-sales taxes paid (post-workaround)
    'salt_prop',          # (dbl)  state/local real estate taxes paid
    'salt_pers',          # (dbl)  state/local personal property taxes paid

    # As-if-itemizing Schedule A amounts (preserved by do_taxes.R before its
    # non-itemizer zeroing): the state itemized base must use what the unit
    # COULD claim, because independent-election states (CA/AZ/NY 2018+/...)
    # let federal standard-deduction takers itemize on the state return.
    # Coupled and federal-gated states are unaffected -- their election
    # requires federal itemizing, where potential == as-claimed
    'item_ded_potential',           # (dbl) federal itemized, post-limitation
    'item_ded_ex_limits_potential', # (dbl) federal itemized, pre-limitation
    'salt_item_ded_potential',      # (dbl) federal SALT deduction (capped)
    'med_item_ded_potential',       # (dbl) deductible medical expenses
    'mort_int_item_ded_potential',  # (dbl) deductible mortgage interest
    'inv_int_item_ded_potential',   # (dbl) deductible investment interest
    'casualty_item_ded_potential',  # (dbl) deductible casualty losses
    'char_item_ded_potential',      # (dbl) deductible charitable contributions
    'misc_item_ded_potential',      # (dbl) miscellaneous itemized deductions
    'other_item_ded_potential',     # (dbl) other federal itemized deductions
    'char_cash',          # (dbl)  cash charitable contributions
    'char_noncash',       # (dbl)  non-cash charitable contributions
    'age1',               # (int)  age of primary filer
    'age2',               # (int)  age of secondary filer
    'blind1',             # (bool) whether primary filer is blind
    'blind2',             # (bool) whether secondary filer is blind
    'std_ded',            # (dbl)  federal standard deduction
    'ei1',                # (dbl)  primary earned income
    'ei2',                # (dbl)  secondary earned income
    'dep_age1',           # (int)  age of youngest dependent (NA if none)
    'dep_age2',           # (int)  age of second-youngest dependent (NA if none)
    'dep_age3',           # (int)  age of oldest dependent (NA if none)
    'care_exp',           # (dbl)  eligible dependent care expenses

    # State tax law
    'st_ded.std_equals_federal', # (int) adopt the federal standard deduction outright (MO)
    'st_ded.std_amount',      # (dbl) state standard deduction (filing-status mapped)
    'st_ded.std_dependent',   # (dbl) standard deduction for dependent filers
    'st_ded.std_dependent_style', # (int) 1 = floor/earned-income/cap worksheet
    'st_ded.std_dependent_floor', # (dbl) minimum dependent standard deduction
    'st_ded.std_dependent_earned_add', # (dbl) addition to dependent earned income
    'st_ded.std_aged_addl',   # (dbl) extra standard deduction per age-65+ filer
    'st_ded.std_blind_addl',  # (dbl) extra standard deduction per blind filer
    'st_ded.std_char_share',  # (dbl) charitable share added to standard deduction
    'st_ded.std_char_floor',  # (dbl) charitable floor for the standard add-on
    'st_ded.item_allowed',    # (int) whether state itemized deductions exist
    'st_ded.item_coupling',   # (int) 0 independent, 1 must match federal
    'st_ded.item_fed_gate',   # (int) federal itemizers only, best-of election (MD)
    'st_ded.salt_addback',    # (int) whether state income tax is excluded/added back
    'st_ded.salt_addback_agi_thresh', # (dbl) FAGI at/above which the exclusion applies (HI; -Inf = always)
    'st_ded.item_component_style', # (int) 1 = select components; 2 = federal amount
    'st_ded.item_include_medical',
    'st_ded.item_include_mortgage',
    'st_ded.item_include_investment',
    'st_ded.item_include_charity',
    'st_ded.item_include_casualty',
    'st_ded.item_include_misc',
    'st_ded.item_include_other',
    'st_ded.item_include_prop_tax',
    'st_ded.item_include_pers_tax',
    'st_ded.item_include_income_sales_tax',
    'st_ded.item_prop_tax_cap',
    'st_ded.pease',           # (int) whether a pre-TCJA Pease limitation applies
    'st_ded.pease_thresh',    # (dbl) Pease AGI threshold (filing-status mapped)
    'st_ded.pease_thresh2',   # (dbl) second-tier threshold (MN 2023+)
    'st_ded.pease_rate2',     # (dbl) second-tier rate (MN 0.10)
    'st_ded.pease_flat_thresh', # (dbl) AGI above which the flat 80% cut applies
    'st_ded.pease_incl_std',  # (int) limitation also reduces the standard deduction
    'st_ded.pease_agi_base',  # (int) limitation income base (st_income_base enum; HI = state AGI)
    'st_ded.std_po_thresh',   # (dbl) sliding std deduction phase-out start (WI)
    'st_ded.std_po_rate',     # (dbl) reduction per dollar above the threshold
    'st_ded.std_po_base',     # (int) phase-out income base (st_income_base enum)
    'st_ded.std_amount2',     # (dbl) second sliding pair maximum (WI HoH floor)
    'st_ded.std_po_rate2',    # (dbl) second sliding pair rate
    'st_ded.std_po_step',     # (dbl) stepped phase-out increment (RI/AL; .inf = sliding)
    'st_ded.std_po_share_per_step', # (dbl) share of the std lost per step (RI 0.20)
    'st_ded.std_po_amount_per_step', # (dbl) DOLLARS of std lost per step (AL)
    'st_ded.std_po_floor',    # (dbl) minimum the stepped deduction falls to (AL)
    'st_ded.item_flat_cap',   # (dbl) flat-dollar itemized cap (OK $17,000; .inf = none)
    'st_ded.item_flat_cap_excl_medical', # (int) medical exempt from the flat cap (OK)
    'st_ded.item_flat_cap_excl_charity', # (int) charity exempt from the flat cap (OK)
    'st_ded.std_pct_rate',    # (dbl) percent-of-income standard deduction rate (MD)
    'st_ded.std_pct_min',     # (dbl) minimum (filing-status mapped)
    'st_ded.std_pct_max',     # (dbl) maximum (filing-status mapped)
    'st_ded.item_limit_style', # (int) 1 = protected-component limitation
    'st_ded.item_limit_agi_base', # (int) limitation income base (st_income_base enum)
    'st_ded.item_limit_thresh', # (dbl) limitation threshold
    'st_ded.item_limit_rate', # (dbl) reduction rate above threshold
    'st_ded.item_limit_max_nonprotected_share', # (dbl) maximum reduction share
    'st_ded.item_limit_protect_medical',
    'st_ded.item_limit_protect_investment',
    'st_ded.item_limit_protect_casualty',
    'st_ded.item_limit_protect_other',
    'st_ded.item_limit_po_thresh',   # (dbl) NY 615(f) phase start (state AGI)
    'st_ded.item_limit_po_width',    # (dbl) NY 615(f) phase width
    'st_ded.item_limit_share1',      # (dbl) first-tier reduction share
    'st_ded.item_limit_tier2_thresh', # (dbl) second-tier phase start
    'st_ded.item_limit_tier2_width',  # (dbl) second-tier phase width
    'st_ded.item_limit_share2',      # (dbl) second-tier reduction share
    'st_ded.char_only_thresh1',      # (dbl) charitable-only tier 1 (state AGI)
    'st_ded.char_only_share1',       # (dbl) charitable share retained, tier 1
    'st_ded.char_only_thresh2',      # (dbl) charitable-only tier 2
    'st_ded.char_only_share2',       # (dbl) charitable share retained, tier 2
    'st_ded.addback_cap_thresh',     # (dbl) high-income addback AGI threshold
    'st_ded.addback_cap',            # (dbl) allowed federal deduction cap
    'st_ded.addback_incl_std',       # (int) whether standard deduction is subject
    'st_ded.care_exp_ded',           # (int) whether care expenses are deductible (VA)
    'st_ded.care_exp_ded_per_dep_cap', # (dbl) per-qualifying-dependent expense cap
    'st_ded.care_exp_ded_dep_limit', # (int) maximum number of qualifying dependents
    'st_ded.care_exp_ded_age_limit', # (int) maximum dependent age to qualify
    'st_ded.item_add_payroll',       # (int) payroll/SE taxes added to the state itemized base (MO)
    'st_ded.retire_exempt_ss',       # (int) taxable Social Security exempt as a DEDUCTION (MO)
    'st_ded.retire_exempt_ss_min_age', # (int) minimum age for the SS exemption (MO 62)
    'st_ded.retire_exempt_ss_limit', # (dbl) income limit, reduced $1-for-$1 above (mapped)
    'st_ded.retire_exempt_priv_cap', # (dbl) per-person private pension exemption (MO $6,000)
    'st_ded.retire_exempt_priv_limit', # (dbl) private pension income limit (mapped)
    'st_ded.retire_exempt_less_ss',  # (int) the limit income measure nets taxable SS (MO)
    'st_ded.fed_tax_ded',            # (int) whether federal income tax is deductible (MO/OR/AL)
    'st_ded.fed_tax_ded_add_niit',   # (int) net investment income tax added to the base (AL)
    'st_ded.fed_tax_ded_less_eitc',  # (int) earned income credit reduces the base (MO/AL)
    'st_ded.fed_tax_ded_less_ctc_ref', # (int) additional child tax credit reduces the base (AL, NOT MO)
    'st_ded.fed_tax_ded_less_ed_ref', # (int) refundable education credit reduces the base (MO/AL)
    'st_ded.fed_tax_ded_less_ptc',   # (int) net premium tax credit reduces the base (MO, NOT AL)
    'st_ded.fed_tax_ded_cap',        # (dbl) cap on the deduction (filing-status mapped)
    'st_ded.fed_tax_ded_band_base'   # (int) share-band income base (st_income_base enum)
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Itemized-limitation income base per the uniform enum (st_income_base)
  item_limit_agi_v = st_income_base(tax_unit,
                                    tax_unit$st_ded.item_limit_agi_base)

  # Sliding standard deduction phase-out income base (WI: state AGI)
  std_po_income_v = st_income_base(tax_unit, tax_unit$st_ded.std_po_base)

  # Pease-style limitation income base (default federal AGI; HI computes its
  # worksheet on Hawaii AGI)
  pease_income_v = st_income_base(tax_unit, tax_unit$st_ded.pease_agi_base)

  # Federal income tax deduction: income-banded SHARE of the deductible base
  # (MO's AGI-tiered percentage). Absent the family the share is 1, leaving
  # states with an uncapped-by-income deduction (AL) or a flat cap alone
  # unaffected. Bands are (lower, upper] on the enum income base; the top
  # band's own value carries the zero tail explicitly rather than relying on
  # st_band_value's outside-the-table zero, so a band table that stops short
  # of infinity is a transcription error rather than a silent zero
  fed_tax_share_v = rep(1, nrow(tax_unit))
  ftd_ub = st_family_matrix(tax_unit, 'st_ded.fed_tax_ded_band_upper')
  if (!is.null(ftd_ub)) {
    ftd_share = st_family_matrix(tax_unit, 'st_ded.fed_tax_ded_band_share',
                                 1:ncol(ftd_ub), require_sentinel = FALSE)
    ftd_income = st_income_base(tax_unit, tax_unit$st_ded.fed_tax_ded_band_base)
    has_ftd_band = !is.na(ftd_ub[, 1])
    fed_tax_share_v = if_else(has_ftd_band,
                              st_band_value(ftd_income, ftd_ub, ftd_share),
                              fed_tax_share_v)
  }

  tax_unit %>%
    mutate(

      #------------------------------------------------
      # State deduction (AGI-start states: IL, NY, ...)
      #------------------------------------------------

      # Income-based limitation amount shared by the itemized and (where
      # flagged) standard deductions: 3% of the enum income base above the
      # threshold, plus a second-tier rate above thresh2 (MN 2023+ two-tier
      # structure); the 80% cap and the flat-80% override are applied at
      # each use. The base defaults to federal AGI (MN); HI computes its
      # worksheet on Hawaii AGI (pease_agi_base = 2)
      pease_income = pease_income_v,
      pease_income_red = 0.03 * pmax(0, pmin(pease_income, st_ded.pease_thresh2) -
                                        st_ded.pease_thresh) +
                         st_ded.pease_rate2 * pmax(0, pease_income - st_ded.pease_thresh2),

      n_taxpayers_ded = 1 + (filing_status == 2),
      n_std_aged = (age1 >= 65) + (filing_status == 2 & !is.na(age2) & age2 >= 65),
      n_std_blind = coalesce(blind1, 0) +
                    (filing_status == 2 & coalesce(blind2, 0)),
      st_std_char_add = st_ded.std_char_share *
                        pmax(0, char_cash + char_noncash - st_ded.std_char_floor),
      st_std_dep = case_when(
        st_ded.std_dependent_style == 1 ~ pmin(
          st_ded.std_amount,
          pmax(st_ded.std_dependent_floor,
               pmax(0, ei1) + if_else(filing_status == 2, pmax(0, ei2), 0) +
                 st_ded.std_dependent_earned_add)
        ),
        TRUE ~ st_ded.std_dependent
      ),
      st_std_ded = if_else(dep_status == 1, st_std_dep, st_ded.std_amount) +
                   n_std_aged * st_ded.std_aged_addl +
                   n_std_blind * st_ded.std_blind_addl +
                   st_std_char_add,

      # Percent-of-income standard deduction (MD 10-217: 15% of state AGI
      # bounded by filing-status min/max, 2017-2024; the 2025 flat amounts
      # revert to std_amount with the rate zeroed)
      st_std_ded = if_else(
        st_ded.std_pct_rate > 0,
        pmin(st_ded.std_pct_max,
             pmax(st_ded.std_pct_min,
                  st_ded.std_pct_rate * pmax(0, st_agi))) +
          n_std_aged * st_ded.std_aged_addl +
          n_std_blind * st_ded.std_blind_addl,
        st_std_ded
      ),

      # High-income limitation applied to the standard deduction itself
      # (MN 290.0123 subd. 5, same tiers as the itemized limitation;
      # 80% maximum reduction, flat 80% above the flat threshold)
      st_std_ded = if_else(
        st_ded.pease == 1 & st_ded.pease_incl_std == 1,
        if_else(pease_income > st_ded.pease_flat_thresh,
                0.20 * st_std_ded,
                pmax(0.20 * st_std_ded, st_std_ded - pease_income_red)),
        st_std_ded
      ),

      # Standard-deduction phase-out above the enum income base threshold,
      # in two mutually exclusive shapes selected by whether a step is
      # encoded:
      #   STEPPED, SHARE (RI-1040 Standard Deduction Worksheet):
      #     share_per_step of the deduction is removed per increment, or
      #     fraction thereof, of income over the threshold -- so the
      #     applicable share falls 0.8/0.6/0.4/0.2 and reaches zero once the
      #     excess passes 1/share_per_step steps. Same construction as
      #     st_exempt's po_share_per_step (CT/MN), including the ceiling()
      #     rounding
      #   STEPPED, DOLLARS (AL Form 40 standard deduction chart): a fixed
      #     amount_per_step comes off per step of income over the threshold,
      #     down to a FLOOR the deduction never falls below. Alabama's chart
      #     is 21 rows -- one flat maximum, nineteen steps, and a floor row
      #     -- and the threshold is encoded as the last flat dollar so that
      #     the first step lands on the chart's first stepped row
      #   SLIDING (WI 71.05(22)): reduced std_po_rate per dollar over the
      #     threshold, to zero. Where a second (max, rate) pair is encoded
      #     the deduction is the larger of the two slides (WI HoH floors at
      #     the single-filer schedule)
      st_std_ded = if_else(
        is.finite(st_ded.std_po_step) & st_ded.std_po_amount_per_step > 0,
        pmax(st_ded.std_po_floor,
             st_std_ded - st_step_reduction(
               std_po_income_v, st_ded.std_po_thresh, st_ded.std_po_step,
               st_ded.std_po_amount_per_step
             )),
        st_std_ded
      ),
      st_std_ded = if_else(
        is.finite(st_ded.std_po_step) & st_ded.std_po_amount_per_step == 0,
        pmax(0, st_std_ded * (1 - pmin(1, st_step_reduction(
          std_po_income_v, st_ded.std_po_thresh, st_ded.std_po_step,
          st_ded.std_po_share_per_step
        )))),
        pmax(
          pmax(0, st_std_ded - st_ded.std_po_rate *
                  pmax(0, std_po_income_v - st_ded.std_po_thresh)),
          pmax(0, st_ded.std_amount2 - st_ded.std_po_rate2 *
                  pmax(0, std_po_income_v - st_ded.std_po_thresh))
        )
      ),

      # States that adopt the FEDERAL standard deduction by reference rather
      # than publishing their own (MO: RSMo 143.131.2, MO-1040 line 14 "enter
      # the standard deduction amount for your filing status"). Taking the
      # unit's own federal amount carries the aged and blind add-ons and the
      # dependent-filer limitation automatically, and cannot drift from
      # federal law the way a transcribed copy would. Applied last, so it
      # replaces rather than stacks with the amounts above
      st_std_ded = if_else(st_ded.std_equals_federal == 1, std_ded, st_std_ded),

      # State itemized base: pre-limitation federal itemized, SALT component
      # replaced by uncapped property taxes (income/sales excluded where
      # added back). The exclusion is unconditional by default; a state
      # that denies the income/sales-tax deduction only above an income
      # threshold (HI Worksheet A-2, federal AGI) encodes the threshold
      st_item_default =
        item_ded_ex_limits_potential - salt_item_ded_potential +
        salt_prop + salt_pers +
        if_else(st_ded.salt_addback == 1 &
                  agi >= st_ded.salt_addback_agi_thresh, 0, salt_inc_sales),
      st_item_components =
        st_ded.item_include_medical * med_item_ded_potential +
        st_ded.item_include_mortgage * mort_int_item_ded_potential +
        st_ded.item_include_investment * inv_int_item_ded_potential +
        st_ded.item_include_charity * char_item_ded_potential +
        st_ded.item_include_casualty * casualty_item_ded_potential +
        st_ded.item_include_misc * misc_item_ded_potential +
        st_ded.item_include_other * other_item_ded_potential +
        st_ded.item_include_prop_tax * pmin(salt_prop, st_ded.item_prop_tax_cap) +
        st_ded.item_include_pers_tax * salt_pers +
        st_ded.item_include_income_sales_tax * salt_inc_sales,
      st_item_base = if_else(
        st_ded.item_allowed == 1,
        case_when(
          st_ded.item_component_style == 1 ~ st_item_components,
          st_ded.item_component_style == 2 ~ item_ded_potential,
          TRUE ~ st_item_default
        ),
        0
      ),

      # Employee payroll and self-employment taxes added to the itemized
      # base (MO-A Part 2 lines 2-7: Social Security tax withheld -- capped
      # on the form at the year's OASDI maximum, which liab_pr_ee already
      # respects -- plus Medicare tax, railroad retirement Tier I/II, and
      # self-employment tax, entered per spouse). Railroad retirement
      # contributions are not modeled separately and fall in with wage FICA
      st_item_base = st_item_base +
                     st_ded.item_add_payroll * (liab_pr_ee + liab_seca) *
                     (st_ded.item_allowed == 1),

      # Pre-TCJA Pease limitation (state-indexed thresholds; medical,
      # investment interest, and casualty are protected), extended with the
      # MN 2023+ second tier and flat-80% override via pease_income_red
      pease_nonprot = pmax(0, st_item_base - med_item_ded_potential -
                              inv_int_item_ded_potential -
                              casualty_item_ded_potential),
      pease_red     = if_else(st_ded.pease == 1,
                              if_else(pease_income > st_ded.pease_flat_thresh,
                                      0.80 * pease_nonprot,
                                      pmin(pease_income_red,
                                           0.80 * pease_nonprot)),
                              0),
      st_item_lim   = pmax(0, st_item_base - pease_red),

      # Protected-component limitation (California-style): apply the smaller
      # of the income-based reduction and a share of unprotected deductions.
      item_limit_agi = item_limit_agi_v,
      item_limit_protected =
        st_ded.item_limit_protect_medical *
          st_ded.item_include_medical * med_item_ded_potential +
        st_ded.item_limit_protect_investment *
          st_ded.item_include_investment * inv_int_item_ded_potential +
        st_ded.item_limit_protect_casualty *
          st_ded.item_include_casualty * casualty_item_ded_potential +
        st_ded.item_limit_protect_other *
          st_ded.item_include_other * other_item_ded_potential,
      item_limit_nonprotected = pmax(0, st_item_lim - item_limit_protected),
      item_limit_red = if_else(
        st_ded.item_limit_style == 1,
        pmin(st_ded.item_limit_max_nonprotected_share * item_limit_nonprotected,
             st_ded.item_limit_rate *
               pmax(0, item_limit_agi - st_ded.item_limit_thresh)),
        0
      ),
      st_item_lim = pmax(0, st_item_lim - item_limit_red),

      # FLAT-DOLLAR cap on the itemized deduction (OK 68 O.S. 2358(E)(3)(b):
      # "Oklahoma itemized deductions are limited to, and may not exceed,
      # $17,000. Charitable contributions and medical expenses are not subject
      # to the $17,000 limit"; ME Schedule 2 line 5, medical exempt). Applied
      # BEFORE the share-based phase-out below because that is the ME
      # worksheet order (the cap sits at Schedule 2 line 5; the line-17
      # phase-out then reduces the capped total) -- behavior-preserving for
      # OK (no share-based phase-out) and NY (no flat cap). The exempt
      # components are held out, the remainder is capped, and the two are
      # recombined -- so the result never exceeds the pre-cap total.
      # Default .inf = no cap
      item_flat_cap_exempt = pmin(
        st_ded.item_flat_cap_excl_medical *
          st_ded.item_include_medical * med_item_ded_potential +
        st_ded.item_flat_cap_excl_charity *
          st_ded.item_include_charity * char_item_ded_potential,
        st_item_lim
      ),
      st_item_lim = if_else(
        is.finite(st_ded.item_flat_cap),
        pmin(st_ded.item_flat_cap,
             pmax(0, st_item_lim - item_flat_cap_exempt)) + item_flat_cap_exempt,
        st_item_lim
      ),

      # High-income itemized limitation (NY 615(f); ME's whole-deduction
      # phase-out with share1 = 1): first-tier share phased over the width
      # above the threshold, second tier likewise
      lim_phi1 = pmin(1, pmax(0, (st_agi - st_ded.item_limit_po_thresh) /
                                  st_ded.item_limit_po_width)),
      lim_phi2 = pmin(1, pmax(0, (st_agi - st_ded.item_limit_tier2_thresh) /
                                  st_ded.item_limit_tier2_width)),
      st_item_lim = st_item_lim * (1 - st_ded.item_limit_share1 * lim_phi1 -
                                       st_ded.item_limit_share2 * lim_phi2),

      # Charitable-only tiers (NY 615(g)): above the thresholds the deduction
      # is a share of charitable contributions only
      st_item_ded = case_when(
        st_agi > st_ded.char_only_thresh2 ~
          st_ded.char_only_share2 * char_item_ded_potential,
        st_agi > st_ded.char_only_thresh1 ~
          st_ded.char_only_share1 * char_item_ded_potential,
        TRUE                              ~ st_item_lim
      ),

      # Election: independent choice takes the larger; coupled follows the
      # federal election
      st_itemizing = case_when(
        st_ded.item_allowed == 0 ~ FALSE,
        # MD: only federal itemizers MAY itemize, but they take the better
        # of the state standard and itemized deductions
        st_ded.item_fed_gate == 1 ~ itemizing == 1 & st_item_ded > st_std_ded,
        st_ded.item_coupling == 1 ~ itemizing == 1,
        TRUE                      ~ st_item_ded > st_std_ded
      ),

      # Dependent-care expense deduction (VA-style): expenses on which the
      # federal CDCTC could be based, using state-side caps so a federal cap
      # change (e.g. ARPA 2021) flows through only if the state conforms.
      # Qualifying dependents counted by age (disabled dependents/spouses are
      # unobserved; known-difference), expenses limited per federal mechanics
      # to the lesser earner's earned income
      st_care_n_qual = pmin(
        (!is.na(dep_age1) & dep_age1 <= st_ded.care_exp_ded_age_limit) +
        (!is.na(dep_age2) & dep_age2 <= st_ded.care_exp_ded_age_limit) +
        (!is.na(dep_age3) & dep_age3 <= st_ded.care_exp_ded_age_limit),
        st_ded.care_exp_ded_dep_limit
      ),
      st_care_ei_limit = pmax(0, if_else(filing_status == 2,
                                         pmin(ei1, ei2), ei1)),
      st_care_exp_ded = st_ded.care_exp_ded *
        pmin(care_exp,
             st_care_n_qual * st_ded.care_exp_ded_per_dep_cap,
             st_care_ei_limit),

      # Federal income tax deduction (MO-1040 lines 9-13; OR-40 federal tax
      # liability subtraction; AL Form 40 full deductibility). The deductible
      # base is federal tax after nonrefundable credits but before other
      # taxes -- 1040 line 22, which is liab_bc (tax plus AMT plus excess
      # APTC repayment) less the nonrefundable credits actually used. AMT is
      # therefore IN the base, matching MO (subtracted on the "tax from
      # federal return" line, added back on "other federal tax") and OR
      # (worksheet line 1 is 1040 line 22 outright). Self-employment,
      # household-employment, and tips FICA taxes are excluded by both
      # states, and none of them enter liab_bc.
      #
      # Each state names its OWN list of refundable credits that reduce the
      # base, and the lists genuinely differ -- so each is a separate flag
      # rather than one blanket switch. Missouri subtracts the earned income
      # credit, refundable education credit and net premium tax credit but
      # NOT the additional child tax credit; Alabama subtracts the earned
      # income credit, the additional child tax credit and the refundable
      # education credit but NOT the net premium tax credit. Both omissions
      # are deliberate: neither appears anywhere in that state's worksheet.
      # Alabama also adds the net investment income tax back, because it
      # rides Schedule 2 Part II and so never reached 1040 line 22.
      #
      # Other add-backs some forms carry (retirement-plan penalty taxes,
      # recapture taxes, Form 2439 credits, the foreign tax credit line
      # Missouri adds as if a tax) are not modeled; see each state's
      # known differences
      st_fed_tax_base = pmax(0,
        liab_bc - nonref +
          st_ded.fed_tax_ded_add_niit * liab_niit -
          st_ded.fed_tax_ded_less_eitc    * eitc -
          st_ded.fed_tax_ded_less_ctc_ref * ctc_ref -
          st_ded.fed_tax_ded_less_ed_ref  * ed_ref -
          st_ded.fed_tax_ded_less_ptc     * net_ptc),
      st_fed_tax_ded = st_ded.fed_tax_ded *
                       pmin(st_ded.fed_tax_ded_cap,
                            fed_tax_share_v * st_fed_tax_base),

      # Retirement exemption taken as a DEDUCTION rather than an AGI
      # subtraction (MO-1040 line 8, from MO-A Part 3). Placement matters:
      # Missouri AGI (line 6) is struck BEFORE this exemption, and line 6 is
      # what the federal-tax-deduction percentage bands key on, so running
      # it through calc_st_agi would feed the bands the wrong income.
      #
      # Both pieces are reduced DOLLAR-FOR-DOLLAR by income above their own
      # limit (MO-A Part 3 sections A-C: "subtract line 5 from line 8; if
      # line 5 is greater, enter $0"), where the limit income is Missouri
      # AGI less taxable Social Security
      retire_limit_income = st_agi - st_ded.retire_exempt_less_ss * txbl_ss,

      # Social Security / SS disability: 100% of federally taxable benefits,
      # for filers at or above the minimum age. Benefits are allocated
      # between spouses on the form by each spouse's share of benefits
      # received, which is unobserved, so the share of the couple's taxable
      # benefits that qualifies is prorated by how many spouses meet the age
      # test. SSDI carries no age requirement but disability status is
      # unobserved [understates the exemption for under-62 SSDI recipients]
      n_retire_age = (age1 >= st_ded.retire_exempt_ss_min_age) +
                     (filing_status == 2 & !is.na(age2) &
                        age2 >= st_ded.retire_exempt_ss_min_age),
      st_retire_ss_exempt = st_ded.retire_exempt_ss *
        pmax(0, pmax(0, txbl_ss) * (n_retire_age / pmax(1, n_taxpayers_ded)) -
                pmax(0, retire_limit_income - st_ded.retire_exempt_ss_limit)),

      # Private pension/annuity/IRA exemption, per person, against the
      # observable taxable pension and IRA distribution pool
      st_retire_priv_exempt = pmax(
        0,
        pmin(pmax(0, txbl_pens_dist + txbl_ira_dist),
             n_taxpayers_ded * st_ded.retire_exempt_priv_cap) -
          pmax(0, retire_limit_income - st_ded.retire_exempt_priv_limit)
      ),
      st_retire_exempt = st_retire_ss_exempt + st_retire_priv_exempt,

      st_ded = if_else(st_itemizing, st_item_ded, st_std_ded) +
               st_care_exp_ded + st_fed_tax_ded + st_retire_exempt,

      #--------------------------------------------------------
      # Deduction addbacks (taxable-income-start states: CO...)
      #--------------------------------------------------------

      # State income tax addback for federal itemizers: income-tax component
      # of the (capped) SALT deduction, limited to the itemized-over-standard
      # excess
      salt_inc_component = pmax(0, salt_item_ded - salt_prop - salt_pers),
      st_addback_salt = if_else(st_ded.salt_addback == 1 & itemizing == 1 &
                                  st_ded.item_allowed == 0,
                                pmin(salt_inc_component,
                                     pmax(0, item_ded - std_ded)),
                                0),

      # High-income federal deduction addback (CO three regimes): federal
      # deduction claimed in excess of the cap, net of state income tax
      # already added back
      fed_ded_claimed = if_else(itemizing == 1, item_ded,
                                std_ded * st_ded.addback_incl_std),
      st_addback_cap  = if_else(agi > st_ded.addback_cap_thresh &
                                  (itemizing == 1 | st_ded.addback_incl_std == 1),
                                pmax(0, fed_ded_claimed - st_ded.addback_cap -
                                        st_addback_salt),
                                0),

      st_addback = st_addback_salt + st_addback_cap
    ) %>%
    select(all_of(return_vars$calc_st_ded)) %>%
    return()
}
