#---------------------------------------------------------------
# Function to calculate state deductions and deduction addbacks
#---------------------------------------------------------------

# Set return variables for function
return_vars$calc_st_ded = c('st_item_ded', 'st_std_ded', 'st_std_char_add',
                            'st_itemizing', 'st_ded', 'st_addback')


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
    'item_ded',           # (dbl)  federal itemized deductions post-limitation
    'item_ded_ex_limits', # (dbl)  federal itemized deductions pre-limitation
    'mort_int_item_ded',  # (dbl)  federal deductible mortgage interest
    'salt_item_ded',      # (dbl)  federal SALT deduction (capped)
    'salt_inc_sales',     # (dbl)  state/local income-or-sales taxes paid (post-workaround)
    'salt_prop',          # (dbl)  state/local real estate taxes paid
    'salt_pers',          # (dbl)  state/local personal property taxes paid
    'med_item_ded',       # (dbl)  federal deductible medical expenses
    'inv_int_item_ded',   # (dbl)  federal deductible investment interest
    'casualty_item_ded',  # (dbl)  federal deductible casualty losses
    'char_item_ded',      # (dbl)  federal deductible charitable contributions
    'misc_item_ded',      # (dbl)  federal miscellaneous itemized deductions
    'other_item_ded',     # (dbl)  other federal itemized deductions
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
    'st_ded.salt_addback',    # (int) whether state income tax is excluded/added back
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
    'st_ded.care_exp_ded_age_limit'  # (int) maximum dependent age to qualify
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Itemized-limitation income base per the uniform enum (st_income_base)
  item_limit_agi_v = st_income_base(tax_unit,
                                    tax_unit$st_ded.item_limit_agi_base)

  tax_unit %>%
    mutate(

      #------------------------------------------------
      # State deduction (AGI-start states: IL, NY, ...)
      #------------------------------------------------

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

      # State itemized base: pre-limitation federal itemized, SALT component
      # replaced by uncapped property taxes (income/sales excluded where
      # added back)
      st_item_default =
        item_ded_ex_limits - salt_item_ded + salt_prop + salt_pers +
        if_else(st_ded.salt_addback == 1, 0, salt_inc_sales),
      st_item_components =
        st_ded.item_include_medical * med_item_ded +
        st_ded.item_include_mortgage * mort_int_item_ded +
        st_ded.item_include_investment * inv_int_item_ded +
        st_ded.item_include_charity * char_item_ded +
        st_ded.item_include_casualty * casualty_item_ded +
        st_ded.item_include_misc * misc_item_ded +
        st_ded.item_include_other * other_item_ded +
        st_ded.item_include_prop_tax * pmin(salt_prop, st_ded.item_prop_tax_cap) +
        st_ded.item_include_pers_tax * salt_pers +
        st_ded.item_include_income_sales_tax * salt_inc_sales,
      st_item_base = if_else(
        st_ded.item_allowed == 1,
        case_when(
          st_ded.item_component_style == 1 ~ st_item_components,
          st_ded.item_component_style == 2 ~ item_ded,
          TRUE ~ st_item_default
        ),
        0
      ),

      # Pre-TCJA Pease limitation (state-indexed thresholds; medical,
      # investment interest, and casualty are protected)
      pease_nonprot = pmax(0, st_item_base - med_item_ded - inv_int_item_ded -
                              casualty_item_ded),
      pease_red     = if_else(st_ded.pease == 1,
                              pmin(0.03 * pmax(0, agi - st_ded.pease_thresh),
                                   0.80 * pease_nonprot),
                              0),
      st_item_lim   = pmax(0, st_item_base - pease_red),

      # Protected-component limitation (California-style): apply the smaller
      # of the income-based reduction and a share of unprotected deductions.
      item_limit_agi = item_limit_agi_v,
      item_limit_protected =
        st_ded.item_limit_protect_medical *
          st_ded.item_include_medical * med_item_ded +
        st_ded.item_limit_protect_investment *
          st_ded.item_include_investment * inv_int_item_ded +
        st_ded.item_limit_protect_casualty *
          st_ded.item_include_casualty * casualty_item_ded +
        st_ded.item_limit_protect_other *
          st_ded.item_include_other * other_item_ded,
      item_limit_nonprotected = pmax(0, st_item_lim - item_limit_protected),
      item_limit_red = if_else(
        st_ded.item_limit_style == 1,
        pmin(st_ded.item_limit_max_nonprotected_share * item_limit_nonprotected,
             st_ded.item_limit_rate *
               pmax(0, item_limit_agi - st_ded.item_limit_thresh)),
        0
      ),
      st_item_lim = pmax(0, st_item_lim - item_limit_red),

      # High-income itemized limitation (NY 615(f)): first-tier share phased
      # over the width above the threshold, second tier likewise
      lim_phi1 = pmin(1, pmax(0, (st_agi - st_ded.item_limit_po_thresh) /
                                  st_ded.item_limit_po_width)),
      lim_phi2 = pmin(1, pmax(0, (st_agi - st_ded.item_limit_tier2_thresh) /
                                  st_ded.item_limit_tier2_width)),
      st_item_lim = st_item_lim * (1 - st_ded.item_limit_share1 * lim_phi1 -
                                       st_ded.item_limit_share2 * lim_phi2),

      # Charitable-only tiers (NY 615(g)): above the thresholds the deduction
      # is a share of charitable contributions only
      st_item_ded = case_when(
        st_agi > st_ded.char_only_thresh2 ~ st_ded.char_only_share2 * char_item_ded,
        st_agi > st_ded.char_only_thresh1 ~ st_ded.char_only_share1 * char_item_ded,
        TRUE                              ~ st_item_lim
      ),

      # Election: independent choice takes the larger; coupled follows the
      # federal election
      st_itemizing = case_when(
        st_ded.item_allowed == 0 ~ FALSE,
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

      st_ded = if_else(st_itemizing, st_item_ded, st_std_ded) +
               st_care_exp_ded,

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
