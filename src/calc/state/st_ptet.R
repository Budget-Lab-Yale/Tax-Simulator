#---------------------------------------------------------------------
# Function to calculate the state side of pass-through entity taxes
#---------------------------------------------------------------------

# Set return variables for function
return_vars$calc_st_ptet = c('st_ptet', 'st_ptet_addback', 'st_ptet_credit')


st_ptet_active = function(tax_unit) {

  #----------------------------------------------------------------------------
  # Whether the state PTET treatment applies: only under ptet.mode 1
  # (src/calc/ptet.R). Under the legacy federal workaround the state
  # calculator is unchanged.
  #
  # Parameters:
  #   - tax_unit (df) : tibble of tax units
  #
  # Returns: TRUE if the state PTET treatment applies (bool).
  #----------------------------------------------------------------------------

  'ptet.mode' %in% names(tax_unit) && all(tax_unit$ptet.mode == PTET_MODE_REPORTED)
}



calc_st_ptet = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Attributes a pass-through entity tax (PTET) election to each record in
  # this state and applies the state's treatment of it (design:
  # research/state_tax/ptet/ptet_methodology.md sections 3 to 5; decision
  # S48). The record's pass-through income is the REPORTED amount: if its
  # entity elected here, the income is already net of the PTET, so
  #
  #   income before the PTET = reported / (1 - rate)
  #   PTET                   = rate / (1 - rate) x max(0, reported)
  #
  # by entity type, at the state's top PTET rate, where a PTET is in effect.
  #
  # Election. The record elects when its federal gain exceeds
  # ptet.gain_threshold and its r.salt_workaround draw is below
  # ptet.takeup_{part,scorp}. The gain is what not electing would cost
  # federally: income higher by the PTET, less whatever of the state tax the
  # owner would then pay directly (the state's relief) fits under the SALT
  # cap, at the ordinary bracket rate on federal taxable income. The existing
  # item.salt_workaround_allowed_* switches turn the election off where a
  # scenario sets them to zero. Approximations: the bracket rate ignores the
  # AMT, the QBI deduction and preferential rates; a standard-deduction filer
  # is assumed to stay one.
  #
  # State treatment where the record elects:
  #   - credit state (st_ptet.method 1): a credit of credit_share x PTET;
  #     where owners must add the entity's deduction back (st_ptet.addback 1)
  #     state income rises by the PTET, to income before it
  #   - exclusion state (st_ptet.method 2): the owner excludes the reported
  #     pass-through income the entity paid on; no credit
  #
  # Not modeled: PTET paid to another state and the resident credit for it,
  # graduated entity schedules, local entity taxes, carryforward of a
  # nonrefundable credit.
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_ptet (dbl)         : this state's entity tax on the owner's share
  #                             (collected from the entity, outside
  #                             liab_st_iit)
  #   - st_ptet_addback (dbl) : net addition to state income (negative in an
  #                             exclusion state)
  #   - st_ptet_credit (dbl)  : owner's credit for the entity tax
  #----------------------------------------------------------------------------

  if (!st_ptet_active(tax_unit)) {
    return(tibble(st_ptet = rep(0, nrow(tax_unit)), st_ptet_addback = 0,
                  st_ptet_credit = 0))
  }

  req_vars = c(

    # Tax unit attributes
    'part',              # (dbl) reported net partnership income
    'scorp',             # (dbl) reported net S-corporation income
    'sstb_part',         # (int) whether the partnership is an SSTB; NA = no partnership income
    'sstb_scorp',        # (int) whether the S corporation is an SSTB; NA = no S-corporation income
    'agi',               # (dbl) federal AGI
    'txbl_inc',          # (dbl) federal taxable income
    'itemizing',         # (int) whether the unit itemizes federally
    'salt_inc_sales',    # (dbl) reported state/local income or sales taxes paid
    'salt_prop',         # (dbl) state/local real estate taxes paid
    'salt_pers',         # (dbl) state/local personal property taxes paid
    'r.salt_workaround', # (dbl) the record's draw for the election

    # Federal tax law attributes
    'ptet.gain_threshold',   # (dbl) federal gain above which an owner elects
    'ptet.takeup_part',      # (dbl) share of partnership owners with a gain who elect
    'ptet.takeup_scorp',     # (dbl) share of S-corporation owners with a gain who elect
    'item.salt_limit',       # (dbl) maximum deductible state and local taxes
    'item.salt_floor',       # (dbl) minimum value of the SALT limit
    'item.salt_floor_thresh', # (dbl) income above which the limit phases down
    'item.salt_po_rate',     # (dbl) rate at which the limit phases down
    'item.salt_workaround_allowed_part',     # (dbl) 0 = no partnership PTET
    'item.salt_workaround_allowed_sstb',     # (dbl) 0 = no PTET for SSTBs
    'item.salt_workaround_allowed_non_sstb', # (dbl) 0 = no PTET for other businesses

    # State tax law attributes
    'st_ptet.available',     # (int) whether a PTET is in effect
    'st_ptet.method',        # (int) owner relief: 0 none, 1 credit, 2 exclusion
    'st_ptet.rate',          # (dbl) entity rate (partnerships)
    'st_ptet.rate_scorp',    # (dbl) S-corporation rate; NA = same as rate
    'st_ptet.credit_share',  # (dbl) credit as a share of the entity tax
    'st_ptet.addback'        # (int) whether owners add back the entity's deduction
  )

  # Ordinary bracket rate on the last dollar of federal taxable income
  bracket_cols  = names(tax_unit) %>% str_subset('^ord\\.brackets\\d+$')
  if (length(bracket_cols) == 0) {
    stop('calc_st_ptet(): federal ord.brackets columns are missing')
  }
  marginal_rate = rep(0, nrow(tax_unit))
  for (i in seq_along(bracket_cols)) {
    above = tax_unit$txbl_inc > tax_unit[[paste0('ord.brackets', i)]]
    marginal_rate = if_else(above, tax_unit[[paste0('ord.rates', i)]], marginal_rate)
  }

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      # PTET if the record's entity elected here, by entity type
      in_effect   = st_ptet.available == 1 & st_ptet.method != 0,
      rate_part   = st_ptet.rate,
      rate_scorp  = coalesce(st_ptet.rate_scorp, st_ptet.rate),
      potential_part  = in_effect * rate_part  / (1 - rate_part)  * pmax(0, part),
      potential_scorp = in_effect * rate_scorp / (1 - rate_scorp) * pmax(0, scorp),
      potential       = potential_part + potential_scorp,

      # Federal gain from electing. Not electing, the owner pays the state's
      # relief directly as state income tax; an itemizer deducts what fits
      # under the SALT cap (calc_item_ded())
      relief    = if_else(st_ptet.method == 1, st_ptet.credit_share, 1) * potential,
      salt_paid = salt_inc_sales + salt_prop + salt_pers,
      salt_cap  = pmax(item.salt_floor,
                       item.salt_limit - item.salt_po_rate *
                         pmax(0, agi - item.salt_floor_thresh)),
      ded_regained = if_else(itemizing == 1,
                             pmin(relief, pmax(0, salt_cap - salt_paid)),
                             0),
      ptet_gain = marginal_rate * (potential - ded_regained),

      # Election. A missing SSTB flag means no income of that type
      allowed_part  = !is.na(sstb_part) &
                      item.salt_workaround_allowed_part > 0 &
                      if_else(coalesce(sstb_part, 0) == 1,
                              item.salt_workaround_allowed_sstb > 0,
                              item.salt_workaround_allowed_non_sstb > 0),
      allowed_scorp = !is.na(sstb_scorp) &
                      if_else(coalesce(sstb_scorp, 0) == 1,
                              item.salt_workaround_allowed_sstb > 0,
                              item.salt_workaround_allowed_non_sstb > 0),
      gains       = ptet_gain > ptet.gain_threshold,
      elect_part  = gains & allowed_part  & potential_part  > 0 &
                    r.salt_workaround < ptet.takeup_part,
      elect_scorp = gains & allowed_scorp & potential_scorp > 0 &
                    r.salt_workaround < ptet.takeup_scorp,

      # This state's entity tax on the owner's share
      st_ptet = elect_part * potential_part + elect_scorp * potential_scorp,

      # State income: up by the PTET where owners add it back; down by the
      # reported income the entity paid on in an exclusion state
      st_ptet_addback = case_when(
        st_ptet.method == 1 & st_ptet.addback == 1 ~ st_ptet,
        st_ptet.method == 2 ~ -(elect_part * pmax(0, part) +
                                elect_scorp * pmax(0, scorp)),
        TRUE ~ 0
      ),

      st_ptet_credit = (st_ptet.method == 1) * st_ptet.credit_share * st_ptet
    ) %>%
    select(all_of(return_vars$calc_st_ptet)) %>%
    return()
}
