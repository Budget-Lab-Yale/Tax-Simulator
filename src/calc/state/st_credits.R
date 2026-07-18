#-------------------------------------------
# Function to calculate state tax credits
#-------------------------------------------

# Set return variables for function
return_vars$calc_st_credits = c('st_hh_credit', 'st_eitc', 'st_ctc',
                                'st_dep_credit', 'st_cdctc', 'st_family_credit',
                                'st_exempt_credit', 'st_earned_credit', 'st_yctc',
                                'st_pct_credit', 'st_cli',
                                'st_credits_nonref', 'st_credits_ref')


calc_st_credits = function(tax_unit, fill_missings = F, credit_tables = NULL) {

  #----------------------------------------------------------------------------
  # Calculates state credits, orchestrating the four credit-family modules
  # (2026-07-17 review item #6):
  #   - st_credits_household.R : NY household credit, CA exemption credits,
  #                              CT Table E rate, KY family-size rate, IL/CT
  #                              property tax credit
  #   - st_credits_earned.R    : EITC matches (incl. the VA option choice and
  #                              CLI), CalEITC-style independent credits,
  #                              young-child credits
  #   - st_credits_child.R     : IL/NY/CO child credits, CO FATC, AZ
  #                              dependent credit
  #   - st_credits_care.R      : CDCTC match and NY care-credit styles
  # Dense schedules are supplied through credit_tables instead of
  # state-specific code. Cross-family dependencies flow through arguments:
  # the household credit feeds the NY EITC offset, and the chosen state
  # EITC feeds the IL child credit.
  #
  # v1 approximations (documented known-differences):
  #  - NY ESCC style 1 folds the pre-TCJA ACTC into full refundability
  #  - CO CTC style 1 attributes the federal credit to under-6 children
  #    proportionally by child count
  #  - CO FATC's stepped phase-out is approximated linearly
  #  - MFS household credit uses own (not combined) AGI
  #  - NY college tuition and IL K-12 credits are data-limited (not computed)
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #   - credit_tables (df)   : dense schedules (see build_state_credit_tables)
  #
  # Returns: dataframe of following variables:
  #   - st_hh_credit (dbl)      : household credit (nonrefundable)
  #   - st_eitc (dbl)           : state EITC
  #   - st_ctc (dbl)            : state child credits (incl. CO FATC)
  #   - st_dep_credit (dbl)     : dependent credit (AZ-style)
  #   - st_cdctc (dbl)          : state child/dependent care credit
  #   - st_family_credit (dbl)  : family-size percentage-of-tax credit
  #   - st_exempt_credit (dbl)  : exemption credits (CA-style)
  #   - st_earned_credit (dbl)  : independent earned-income credit
  #   - st_yctc (dbl)           : state young-child credit
  #   - st_pct_credit (dbl)     : percentage-of-tax credit (CT Table E)
  #   - st_cli (dbl)            : credit for low-income individuals (VA)
  #   - st_credits_nonref (dbl) : total nonrefundable credits
  #   - st_credits_ref (dbl)    : total refundable credits
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',               # (dbl)  federal AGI
    'st_agi',            # (dbl)  state income base
    'st_additions',      # (dbl)  additions to the federal AGI base
    'st_tax_pre_credit', # (dbl)  state tax before credits
    'st_age_package_taken', # (int) aged package claimed under exclusivity (calc_st_agi)
    'eitc',              # (dbl)  federal EITC
    'ctc_nonref',        # (dbl)  federal CTC, nonrefundable portion
    'ctc_ref',           # (dbl)  federal CTC, refundable portion
    'cdctc_nonref',      # (dbl)  federal CDCTC, nonrefundable portion
    'cdctc_ref',         # (dbl)  federal CDCTC, refundable portion
    'care_exp',          # (dbl)  eligible dependent care expenses
    'salt_prop',         # (dbl)  state/local real estate taxes paid
    'dep_age1',          # (int)  age of youngest dependent (NA if none)
    'dep_age2',          # (int)  age of second dependent (NA if none)
    'dep_age3',          # (int)  age of third dependent (NA if none)
    'n_dep',             # (int)  number of dependents
    'n_dep_ctc',         # (int)  federal CTC-qualifying dependent count
    'n_dep_eitc',        # (int)  federal EITC-qualifying dependent count
    'filing_status',     # (int)  filing status (1 single, 2 MFJ, 3 MFS, 4 HoH)
    'dep_status',        # (bool) whether filer is a dependent
    'age1',              # (int)  age of primary filer
    'age2',              # (int)  age of secondary filer (NA if none)
    'blind1',            # (bool) whether primary filer is blind
    'blind2',            # (bool) whether secondary filer is blind
    'ei1',               # (dbl)  primary earned income for independent EITCs
    'ei2',               # (dbl)  secondary earned income for independent EITCs
    'wages1',            # (dbl)  primary wages for zero-income child credits
    'wages2',            # (dbl)  secondary wages for zero-income child credits
    'sole_prop',         # (dbl)  Schedule C income/loss
    'sch_e',             # (dbl)  Schedule E income/loss
    'farm',              # (dbl)  Schedule F income/loss

    # State tax law (scalar; vector table params accessed by column name),
    # declared per credit-family module
    st_credits_household_req_vars,
    st_credits_earned_req_vars,
    st_credits_child_req_vars,
    st_credits_care_req_vars
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Credit-family modules (cross-family inputs passed explicitly)
  hh    = st_credits_household(tax_unit)
  earn  = st_credits_earned(tax_unit, hh$st_hh_credit, credit_tables)
  child = st_credits_child(tax_unit, earn$st_eitc)
  care  = st_credits_care(tax_unit)

  # Percentage-of-tax credits and aggregation
  st_family_credit = if_else(tax_unit$st_credits.family_credit_style == 1,
                             tax_unit$st_tax_pre_credit * hh$family_credit_rate,
                             0)
  st_pct_credit = hh$pct_credit_rate * pmax(0, tax_unit$st_tax_pre_credit)

  tibble(
    st_hh_credit     = hh$st_hh_credit,
    st_eitc          = earn$st_eitc,
    st_ctc           = child$st_ctc,
    st_dep_credit    = child$st_dep_credit,
    st_cdctc         = care$st_cdctc,
    st_family_credit = st_family_credit,
    st_exempt_credit = hh$st_exempt_credit,
    st_earned_credit = earn$st_earned_credit,
    st_yctc          = earn$st_yctc,
    st_pct_credit    = st_pct_credit,
    st_cli           = earn$st_cli,

    st_credits_nonref = hh$st_hh_credit + hh$prop_credit + child$st_dep_credit +
                        st_family_credit + hh$st_exempt_credit + st_pct_credit +
                        earn$st_cli +
                        earn$st_eitc * (1 - earn$st_eitc_ref_share) +
                        earn$st_earned_credit *
                          (1 - tax_unit$st_credits.earned_credit_refundable) +
                        care$st_cdctc *
                          (1 - tax_unit$st_credits.cdctc_refundable),
    st_credits_ref    = earn$st_eitc * earn$st_eitc_ref_share + child$st_ctc +
                        earn$st_earned_credit *
                          tax_unit$st_credits.earned_credit_refundable +
                        earn$st_yctc +
                        care$st_cdctc * tax_unit$st_credits.cdctc_refundable
  ) %>%
    select(all_of(return_vars$calc_st_credits)) %>%
    return()
}
