#---------------------------------------------------------------------------
# st_split.R
#
# The MARRIED-SEPARATE ELECTION: a generic minimum-liability pass for states
# that let a couple filing jointly for federal purposes compute state tax in
# two per-spouse columns and keep whichever result is cheaper.
#
# Four encoded states need it, and they need the same thing: the WHOLE state
# pipeline rerun per spouse, not just the rate schedule. st_ord.combined_sep
# (KY, DE, IA) reruns the ladder alone, which is enough where the only
# per-spouse quantity is the standard deduction. It is not enough where a
# state computes the deduction, exemption or federal-tax deduction on each
# spouse's own income:
#
#   - MT filing status 2a: a percentage-of-own-AGI standard deduction with its
#     own floor and cap, a per-spouse pension exemption whose phase-out runs
#     on own federal AGI, and a per-spouse $5,000 federal-tax deduction cap,
#     all against a ladder whose bounds do not vary by filing status
#   - AL separate returns: the ladder is split-neutral, but the standard
#     deduction slide and the dependent exemption tiers key on each spouse's
#     own Alabama AGI
#   - AR filing status 4: the schedule applied once per column with a
#     per-spouse standard deduction, itemized deductions pooled then prorated
#   - MS combined returns: two columns, so a two-earner couple shelters two
#     zero brackets
#
# NOT in scope here, because it is a different mechanism: an ALTERNATIVE
# COMPUTATION replacing the schedule rather than splitting the unit. That
# covers AR's five Low Income Tax Tables (used instead of the schedule AND
# instead of any deduction) and the WI Act 15 retirement election.
#---------------------------------------------------------------------------


# Inputs whose ownership is UNOBSERVED, divided evenly between the columns.
# This is the VA spouse-tax-adjustment / KY combined-return convention, but
# applied at the INPUT level rather than to state AGI, so that each column's
# own means tests see the right income. Listed explicitly rather than by
# exclusion: a federal variable added to the state calculator later must be
# classified deliberately, not swept into a half by default.
ST_SPLIT_HALVE = c(
  # Income
  'txbl_int', 'exempt_int', 'div_ord', 'div_pref', 'kg_lt', 'kg_st',
  'kg_pref', 'txbl_kg', 'other_gains', 'sole_prop', 'part_active',
  'part_passive', 'part_scorp', 'scorp', 'rent', 'farm', 'sch_e',
  'txbl_ira_dist', 'txbl_pens_dist', 'gross_ss', 'txbl_ss', 'ui',
  'alimony', 'other_inc', 'state_ref',
  # Above-the-line and below-the-line federal amounts
  'hsa_contr', 'ot_ded', 'std_ded', 'char_cash', 'char_noncash', 'care_exp',
  # Itemized deductions, as claimed and as-if. Halved rather than prorated by
  # income share: Montana's rule is that a deduction "attributable to one
  # spouse must be claimed by that spouse", which is unobservable, so an even
  # division is as defensible as any. Arkansas status 4 differs -- it pools
  # itemized deductions and prorates them by AGI share -- so wiring AR will
  # need that as a per-state option
  'item_ded', 'item_ded_potential', 'item_ded_ex_limits_potential',
  'med_item_ded_potential', 'mort_int_item_ded_potential',
  'inv_int_item_ded_potential', 'casualty_item_ded_potential',
  'char_item_ded_potential', 'misc_item_ded_potential',
  'other_item_ded_potential', 'salt_item_ded', 'salt_item_ded_potential',
  'salt_prop', 'salt_pers', 'salt_inc_sales',
  # Federal tax and credit quantities, which feed the federal-tax deduction
  'liab_bc', 'nonref', 'liab_niit', 'liab_pr_ee', 'liab_seca', 'excess_ptc',
  'eitc', 'ctc_nonref', 'ctc_ref', 'cdctc_nonref', 'cdctc_ref', 'ed_ref',
  'net_ptc'
)

# Inputs split WAGE-ANCHORED: own wages plus half of the joint remainder.
# Federal AGI and federal taxable income already contain each spouse's own
# wages, so halving them outright would hand half of one spouse's earnings to
# the other and misplace every means test keyed on them -- MT's pension
# phase-out reads own federal AGI directly.
ST_SPLIT_WAGE_ANCHORED = c('agi', 'txbl_inc')


st_split_spouse_unit = function(tax_unit, spouse, law_mfs) {

  #----------------------------------------------------------------------------
  # Builds the per-spouse half-unit for one column of a married-separate
  # election. Filing status becomes married-filing-separately, the chosen
  # spouse's person-indexed values move into the primary slots, the secondary
  # slots are emptied, jointly-held amounts are halved, and federal AGI and
  # taxable income are rebuilt as own wages plus half the non-wage remainder.
  #
  # The LAW columns are replaced wholesale with the married-separate row.
  # This is the step that cannot be skipped: the filing-status mapper is
  # resolved when the law is joined to the unit, so relabelling
  # filing_status here would leave every mapped parameter -- Montana's
  # standard-deduction floor and cap, its federal-tax deduction cap -- still
  # holding the JOINT value, and each column would silently claim a joint-size
  # deduction. Everything on tax_unit prefixed st_ at this point is a law
  # parameter, since the state outputs do not exist until the pipeline runs.
  #
  # The dependent block rides WHOLE with the first column. The forms let a
  # couple allocate dependents between columns but not divide them, and the
  # allocation that minimizes tax is its own optimization, documented as an
  # approximation per state rather than solved here.
  #
  # Parameters:
  #   - tax_unit (df) : units with law columns joined and params backfilled
  #   - spouse (int)  : 1 or 2, which spouse's column to build
  #   - law_mfs (df)  : one row of the state-year law at filing status 3
  #
  # Returns: tibble of the same shape as tax_unit (df).
  #----------------------------------------------------------------------------

  stopifnot(spouse %in% c(1, 2))
  col = tax_unit

  # law_mfs is the RAW law row, so a parameter the state does not encode sits
  # there as NA. Swapping those in would undo the backfill the joint unit
  # already had, so the column is re-backfilled: a parameter absent from the
  # married-separate row must fall back to its schema default, not to the
  # joint value it happened to be resolved to
  law_cols = intersect(names(law_mfs), names(col))
  for (v in law_cols) {
    col[[v]] = law_mfs[[v]][1]
  }
  col %<>% ensure_st_params()

  own_wages = if (spouse == 1) tax_unit$wages1 else tax_unit$wages2
  own_ei    = if (spouse == 1) tax_unit$ei1    else tax_unit$ei2
  own_age   = if (spouse == 1) tax_unit$age1   else tax_unit$age2
  own_blind = if (spouse == 1) tax_unit$blind1 else tax_unit$blind2

  joint_wages = coalesce(tax_unit$wages1, 0) + coalesce(tax_unit$wages2, 0)
  for (v in intersect(ST_SPLIT_WAGE_ANCHORED, names(col))) {
    col[[v]] = coalesce(own_wages, 0) + (col[[v]] - joint_wages) / 2
  }

  for (v in intersect(ST_SPLIT_HALVE, names(col))) {
    col[[v]] = col[[v]] / 2
  }

  col$filing_status = 3L
  col$wages1 = own_wages
  col$ei1    = own_ei
  col$age1   = own_age
  col$blind1 = own_blind
  col$wages2 = 0
  col$ei2    = 0
  col$age2   = NA_real_
  col$blind2 = NA

  if (spouse == 2) {
    for (v in intersect(c('n_dep', 'n_dep_ctc', 'n_dep_eitc'), names(col))) {
      col[[v]] = 0L
    }
    for (v in intersect(c('dep_age1', 'dep_age2', 'dep_age3'), names(col))) {
      col[[v]] = NA_real_
    }
  }

  return(col)
}


st_split_election = function(tax_units, joint, credit_tables = NULL,
                             law_mfs = NULL) {

  #----------------------------------------------------------------------------
  # Runs the whole state pipeline again on each spouse's half-unit, sums the
  # two columns, and gives each electing couple whichever total is lower.
  #
  # Only a married unit under an electing law can benefit, so the two extra
  # passes run on THAT SUBSET of rows rather than the whole frame. This matters
  # because a mixed law slice carries every state at once: gating the passes on
  # the slice would triple the cost of every run containing one electing state.
  #
  # DOLLAR outputs come from the winning basis -- summed across columns where
  # the split wins. Logical and INTEGER outputs stay on the joint basis, since
  # they are return-level concepts (whether the unit files, whether it
  # itemized, whether an exclusivity package was taken) that splitting the
  # income does not redefine, and summing a pair of 0/1 flags would produce a
  # meaningless 2. Hence the selection below is on is.double(), not
  # is.numeric(): the integer flags st_age_package_taken/_forgone are numeric.
  #
  # Parameters:
  #   - tax_units (df)     : units with law columns joined and params filled
  #   - joint (df)         : the joint-basis result from st_pipeline()
  #   - credit_tables (df) : dense credit schedules
  #
  # Returns: tibble of state-calculated variables (df), same shape as joint.
  #----------------------------------------------------------------------------

  electing = which(tax_units$st_ord.split_election == 1 &
                     tax_units$filing_status == 2)
  if (length(electing) == 0) {
    return(joint)
  }

  # Fail loudly rather than silently computing the columns on joint-resolved
  # parameters, which would understate the couple's tax
  if (is.null(law_mfs) || nrow(law_mfs) != 1) {
    stop('st_ord.split_election is on but law_mfs was not supplied as one ',
         'married-separate law row; pass it through do_state_taxes()')
  }

  sub  = tax_units[electing, ]
  col1 = st_pipeline(st_split_spouse_unit(sub, 1, law_mfs), credit_tables)
  col2 = st_pipeline(st_split_spouse_unit(sub, 2, law_mfs), credit_tables)

  wins = coalesce(
    (col1$liab_st_iit + col2$liab_st_iit) < joint$liab_st_iit[electing],
    FALSE
  )
  rows = electing[wins]
  if (length(rows) == 0) {
    return(joint)
  }

  for (v in names(joint)[map_lgl(joint, is.double)]) {
    joint[[v]][rows] = (col1[[v]] + col2[[v]])[wins]
  }

  return(joint)
}
