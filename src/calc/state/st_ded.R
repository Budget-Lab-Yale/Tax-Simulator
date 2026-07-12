#---------------------------------------------------------------
# Function to calculate state deductions and deduction addbacks
#---------------------------------------------------------------

# Set return variables for function
return_vars$calc_st_ded = c('st_item_ded', 'st_std_ded', 'st_itemizing',
                            'st_ded', 'st_addback')


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
    'itemizing',          # (bool) whether unit itemizes federally
    'dep_status',         # (bool) whether filer is a dependent
    'item_ded',           # (dbl)  federal itemized deductions post-limitation
    'item_ded_ex_limits', # (dbl)  federal itemized deductions pre-limitation
    'salt_item_ded',      # (dbl)  federal SALT deduction (capped)
    'salt_inc_sales',     # (dbl)  state/local income-or-sales taxes paid (post-workaround)
    'salt_prop',          # (dbl)  state/local real estate taxes paid
    'salt_pers',          # (dbl)  state/local personal property taxes paid
    'med_item_ded',       # (dbl)  federal deductible medical expenses
    'inv_int_item_ded',   # (dbl)  federal deductible investment interest
    'casualty_item_ded',  # (dbl)  federal deductible casualty losses
    'char_item_ded',      # (dbl)  federal deductible charitable contributions
    'std_ded',            # (dbl)  federal standard deduction

    # State tax law
    'st_ded.std_amount',      # (dbl) state standard deduction (filing-status mapped)
    'st_ded.std_dependent',   # (dbl) standard deduction for dependent filers
    'st_ded.item_allowed',    # (int) whether state itemized deductions exist
    'st_ded.item_coupling',   # (int) 0 independent, 1 must match federal
    'st_ded.salt_addback',    # (int) whether state income tax is excluded/added back
    'st_ded.pease',           # (int) whether a pre-TCJA Pease limitation applies
    'st_ded.pease_thresh',    # (dbl) Pease AGI threshold (filing-status mapped)
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
    'st_ded.addback_incl_std'        # (int) whether standard deduction is subject
  )

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      #------------------------------------------------
      # State deduction (AGI-start states: IL, NY, ...)
      #------------------------------------------------

      st_std_ded = if_else(dep_status, st_ded.std_dependent, st_ded.std_amount),

      # State itemized base: pre-limitation federal itemized, SALT component
      # replaced by uncapped property taxes (income/sales excluded where
      # added back)
      st_item_base = if_else(
        st_ded.item_allowed == 1,
        item_ded_ex_limits - salt_item_ded + salt_prop + salt_pers +
          if_else(st_ded.salt_addback == 1, 0, salt_inc_sales),
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
        st_ded.item_coupling == 1 ~ itemizing,
        TRUE                      ~ st_item_ded > st_std_ded
      ),
      st_ded = if_else(st_itemizing, st_item_ded, st_std_ded),

      #--------------------------------------------------------
      # Deduction addbacks (taxable-income-start states: CO...)
      #--------------------------------------------------------

      # State income tax addback for federal itemizers: income-tax component
      # of the (capped) SALT deduction, limited to the itemized-over-standard
      # excess
      salt_inc_component = pmax(0, salt_item_ded - salt_prop - salt_pers),
      st_addback_salt = if_else(st_ded.salt_addback == 1 & itemizing &
                                  st_ded.item_allowed == 0,
                                pmin(salt_inc_component,
                                     pmax(0, item_ded - std_ded)),
                                0),

      # High-income federal deduction addback (CO three regimes): federal
      # deduction claimed in excess of the cap, net of state income tax
      # already added back
      fed_ded_claimed = if_else(itemizing, item_ded,
                                std_ded * st_ded.addback_incl_std),
      st_addback_cap  = if_else(agi > st_ded.addback_cap_thresh &
                                  (itemizing | st_ded.addback_incl_std == 1),
                                pmax(0, fed_ded_claimed - st_ded.addback_cap -
                                        st_addback_salt),
                                0),

      st_addback = st_addback_salt + st_addback_cap
    ) %>%
    select(all_of(return_vars$calc_st_ded)) %>%
    return()
}
