#-------------------------------------------------------------
# Function to calculate state tax before credits (rate
# schedule plus tax-benefit recapture where applicable,
# continuous NY-style or stepped CT-style)
#-------------------------------------------------------------

# Set return variables for function
return_vars$calc_st_tax = c('st_tax_pre_credit')


calc_st_tax = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates state tax before credits: the rate/bracket schedule applied to
  # state taxable income, plus the NY-style tax-benefit recapture
  # (supplemental tax) where st_ord.recapture_agi_start is finite.
  #
  # Recapture implements the IT-201 worksheet identity (verified against the
  # published 2017-2025 worksheets; see plan research notes): with B the
  # lower bound of the taxpayer's taxable-income bracket, m its rate, m_prev
  # the rate below, T() the schedule tax, and S0 the recapture AGI trigger:
  #   RB  = m_prev*B - T(B)                    if B > S0, else 0
  #   phi = clamp((st_agi - max(B, S0)) / width, 0, 1)
  #   tax = T(TI) + RB + (m*TI - T(TI) - RB) * phi
  # Once st_agi >= B + width, tax = m*TI (all lower-bracket benefit
  # recaptured). Units with st_agi above the top bracket (the 25M rule,
  # 2022+) pay the top rate flat with no phase-in; the 2021-only $50k
  # phase above $25M is approximated by the same flat rule (negligible).
  #
  # Independently, CT-style STEPPED recapture segments (CT-1040 TCS phase-out
  # add-back and tax recapture tables) add, for each encoded segment s with
  # st_agi above its start:
  #   min(ceil((st_agi - start_s) / incr_s) * amount_s, max_s)
  # The segment vectors (st_ord.step_recap_*) are filing-status mapped and
  # absent for states without the feature (gated on step_recap_start1).
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_tax_pre_credit (dbl) : state tax before credits
  #----------------------------------------------------------------------------

  req_vars = c(
    'st_agi',                    # (dbl) state income base
    'st_txbl_inc',               # (dbl) state taxable income
    'st_ord.rates[]',            # (dbl) state marginal rates
    'st_ord.brackets[]',         # (dbl) state bracket lower bounds
    'st_ord.recapture_agi_start', # (dbl) recapture trigger (Inf = none)
    'st_ord.recapture_width'     # (dbl) recapture phase-in width
  )

  tax_unit %<>%
    parse_calc_fn_input(req_vars, fill_missings)

  # Schedule tax on taxable income
  tax_unit %<>%
    bind_cols(
      integrate_rates_brackets(
        df              = .,
        n_brackets      = NULL,
        prefix_brackets = 'st_ord.brackets',
        prefix_rates    = 'st_ord.rates',
        y               = 'st_txbl_inc',
        output_name     = 'st_tax_sched',
        by_bracket      = F
      )
    )

  # Recapture via bracket matrices
  bracket_cols = str_subset(colnames(tax_unit), '^st_ord\\.brackets[0-9]+$')
  n_br = max(as.integer(str_extract(bracket_cols, '[0-9]+$')))
  br   = as.matrix(tax_unit[paste0('st_ord.brackets', 1:n_br)])
  rt   = as.matrix(tax_unit[paste0('st_ord.rates',    1:n_br)])

  # Schedule tax at an arbitrary income vector
  sched_tax_at = function(y) {
    upper = cbind(br[, -1, drop = F], Inf)
    rowSums(rt * pmax(0, pmin(y, upper) - br), na.rm = T)
  }

  # Stepped recapture segments (CT-style), zero when not encoded
  step_recap = rep(0, nrow(tax_unit))
  step_start_cols = str_subset(colnames(tax_unit),
                               '^st_ord\\.step_recap_start[0-9]+$')
  if (length(step_start_cols) > 0 &&
      any(!is.na(tax_unit[[step_start_cols[1]]]))) {
    n_seg  = max(as.integer(str_extract(step_start_cols, '[0-9]+$')))
    s_strt = as.matrix(tax_unit[paste0('st_ord.step_recap_start',  1:n_seg)])
    s_incr = as.matrix(tax_unit[paste0('st_ord.step_recap_incr',   1:n_seg)])
    s_amt  = as.matrix(tax_unit[paste0('st_ord.step_recap_amount', 1:n_seg)])
    s_max  = as.matrix(tax_unit[paste0('st_ord.step_recap_max',    1:n_seg)])
    excess = pmax(0, tax_unit$st_agi - s_strt)
    step_recap = rowSums(pmin(ceiling(excess / s_incr) * s_amt, s_max),
                         na.rm = T)
  }

  ti = tax_unit$st_txbl_inc
  i  = seq_along(ti)
  j  = pmax(1, rowSums(br <= ti, na.rm = T))   # taxpayer's bracket index
  m      = rt[cbind(i, j)]
  B      = br[cbind(i, j)]
  m_prev = rt[cbind(i, pmax(1, j - 1))]

  tax_unit %>%
    mutate(
      recap_S0  = st_ord.recapture_agi_start,
      recap_on  = is.finite(recap_S0) & st_agi > recap_S0,
      recap_RB  = if_else(B > recap_S0, pmax(0, m_prev * B - sched_tax_at(B)), 0),
      recap_phi = pmin(1, pmax(0, (st_agi - pmax(B, recap_S0)) /
                                   st_ord.recapture_width)),
      flat_top  = recap_on & st_agi > br[, n_br] & br[, n_br] >= 5e6,

      st_tax_pre_credit = case_when(
        flat_top ~ rt[, n_br] * st_txbl_inc,
        recap_on ~ st_tax_sched + recap_RB +
                   pmax(0, m * st_txbl_inc - st_tax_sched - recap_RB) * recap_phi,
        TRUE     ~ st_tax_sched
      ) + step_recap
    ) %>%
    select(all_of(return_vars$calc_st_tax)) %>%
    return()
}
