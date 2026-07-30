#-------------------------------------------------------------------------------
# wealth_dynamics.R
#
# Contains functions to simulate saving responses to tax changes and their
# effect on the estate tax base
#-------------------------------------------------------------------------------

# Assume that a share s of any increase in taxes paid during life is financed
# out of wealth rather than consumption. The shortfall compounds over time and
# reduces the estate at death. s is set by the scenario's financing profile.
# Conventional runs only.

# Accounting is done by cohort rather than by tax unit. Cells are age by net
# worth percentile within age, stepped forward one year at a time by
# run_wealth_bathtub_pass(), then applied back to the records in each cell by
# wealth_dyn_apply_to_records(). Cohort helpers are shared with the capital
# gains bathtub and live in src/sim/cohort_bathtub.R.

# Notes and calibration: other/wealth_dynamics/



#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------

# Cohort age grid, top-coded at 80 to match the capital gains bathtub and the
# Tax-Data age1 top code. Declared separately from the capital gains constants.
WEALTH_DYN_AGE_MIN = 18L
WEALTH_DYN_AGE_MAX = 80L

WEALTH_DYN_SPEC_VERSION = 1L

# Proportional bump used to measure the capital income bundle MTR through the
# calculator.
WEALTH_DYN_MTR_BUMP = 0.01

# Minimum capital income, as a share of gross assets, for a record to get a
# measured bundle MTR. Below it there is no meaningful margin and the MTR is 0.
WEALTH_DYN_F_FLOOR = 1e-4

# Cap on capital income as a share of assets: a one-time gain in a small cell
# can otherwise push this far above a plausible yearly return.
WEALTH_DYN_Y_MAX = 1.0

# A record's bundle rate is measured by bumping its capital income 1% and
# dividing the change in tax by the change in income. The statute has discrete
# steps in it: the child credit phases out in $1,000 blocks at 5%, so a record
# whose bump carries it across a block boundary pays $50 on a bump that may be a
# dollar wide, and reports a rate in the tens. The floor above does not catch
# this, since the record can have real capital income and still straddle a step.
#
# A marginal rate on capital income lies in the unit interval. Records measuring
# outside it are dropped from the cell average, numerator and denominator
# together, rather than averaged in: the cells are thin enough at the top that
# one such record sets the rate for the whole cell.
WEALTH_DYN_MTR_RANGE = c(0, 1)

# Floor for the growth factor G. Tax feedback damps growth but cannot turn it
# negative. Binds only in the sparse cells described above.
WEALTH_DYN_G_FLOOR = 1e-6

# Numerical floor for denominators (cell net worth sums, gross assets).
WEALTH_DYN_EPS = 1e-8

# The capital income flows the channel touches, and their weights. The MTR bump
# and the applier haircut scale exactly these columns by these weights; if they
# diverge, the measured MTR no longer matches the erosion.
#
# Pure capital flows carry weight 1: interest, dividends, the four taxed gain
# classes, and the rental and estate/trust pairs. Each loss leg is scaled with
# its income leg so the net scales proportionally. rent/rent_loss and
# estate/estate_loss are raw Tax-Data columns, which derive_vars() recombines
# into sch_e inside do_taxes(), so scaling the raw legs carries through.
WEALTH_CAP_FLOWS_PURE = c(
  'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
  'kg_st', 'kg_lt', 'kg_1250', 'kg_collect',
  'rent', 'rent_loss', 'estate', 'estate_loss'
)

# Basis is scaled with kg_lt so that the taxable gain scales proportionally.
# Scale kg_lt_basis rather than kg_lt_infl_adj, which derive_vars() recomputes;
# scaling the latter has no effect and can drive kg_lt_adj negative under
# indexed-basis law.
WEALTH_CAP_FLOWS_PURE_BASIS = c('kg_lt_basis')

# Pass-through flows carry weight 0.2, the capital share of business income
# under the model's 20/80 capital-labor split. This is the raw disaggregated
# business income list from economy.R. derive_vars() recombines these into
# part/scorp/pt/sch_e inside do_taxes(), so scaling the raw legs carries
# through. Loss legs are scaled with their income legs.
WEALTH_CAP_FLOWS_PT = c(
  'sole_prop', 'part_active', 'part_passive', 'part_active_loss',
  'part_passive_loss', 'part_179', 'scorp_active', 'scorp_passive',
  'scorp_active_loss', 'scorp_passive_loss', 'scorp_179', 'farm'
)
# Weight on pass-through flows; see config/scenarios/economy/default/wealth.yaml
wealth_cap_flows_pt_weight = function() economy_param('wealth', 'cap_flows_pt_weight')

# Earner-split companions of the pass-through aggregates, scaled by the same
# 0.2 weight so the SECA and NIIT active-passive split stays consistent. Note
# that the bundle MTR is income tax only, so the SECA response of this slice is
# omitted; it is under 1% at the top of the distribution.
WEALTH_CAP_FLOWS_SE_COMPANIONS = c(
  'part_se1', 'part_se2', 'sole_prop1', 'sole_prop2', 'farm1', 'farm2'
)

# Retirement distributions, scaled with the balance sheet haircut. The haircut
# marks down retirement balances, so proportional draws from those balances must
# scale too, or faster drawdown undoes the erosion. These are not part of
# WEALTH_CAP_FLOWS: a distribution moves money between pockets rather than
# generating income, so it enters neither the bundle MTR nor the forcing, and
# its tax consequence arrives only through do_taxes. gross_pens_dist is scaled
# along with the taxable legs so that expanded income and the derived
# distributional metrics stay consistent.
#
# Note that the kernel's tax drag prices only tax forgone on capital income, so
# the ordinary income relief on eroded distributions is left out.
WEALTH_RET_DIST_FLOWS = c(
  'txbl_ira_dist', 'txbl_pens_dist', 'gross_pens_dist'
)


#-------------------------------------------------------------------------------
# Scenario gate and params
#-------------------------------------------------------------------------------

scenario_uses_wealth_dynamics = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reports whether the channel runs for a scenario, based on its financing
  # profile rather than its behavior column. Active if any cell's saving share
  # is positive, so a profile of all zeros is dormant and skips the second
  # simulation pass. The default profile is calibrated and nonzero, so a
  # scenario that says nothing runs with the channel on; set financing_profile
  # to none to opt out.
  #
  # Returns: TRUE if the channel is active for this scenario (bool). Errors on a
  #          missing or malformed profile.
  #----------------------------------------------------------------------------

  isTRUE(wealth_dyn_resolve_profile(scenario_info)$active)
}



wealth_dyn_load_params = function() {

  #----------------------------------------------------------------------------
  # Reads the operational parameters from the economy leg's wealth channel. The
  # saving share s and the transition matrix M are not among them; they come
  # from the scenario's financing profile, via wealth_dyn_resolve_profile().
  #
  # Returns: list of n_pctiles, fmax, and r_total.
  #----------------------------------------------------------------------------

  list(
    n_pctiles = as.integer(economy_param('wealth', 'n_pctiles')),
    fmax      = as.numeric(economy_param('wealth', 'fmax')),
    r_total   = list(source         = 'macro_gdp_per_capita',
                     additive_delta = as.numeric(
                       economy_param('wealth', 'r_total_additive_delta')))
  )
}



#-------------------------------------------------------------------------------
# Financing profile: the saving share s by age and percentile, and the
# percentile transition matrix M
#
# A profile is a folder under config/calibrations/wealth_profiles/ holding two
# files:
#   - s.csv : one row per cell, with columns age, nw_pctile, and s. Covers every
#             cell of the age by percentile grid exactly once, with s in [0, 1].
#   - M.csv : the within-age percentile transition, one n_pctiles by n_pctiles
#             matrix applied at every age, raked to doubly stochastic on load.
#             M.rds is also accepted, either as a matrix or as a list by age. If
#             no M file is present the transition is the identity.
#
# The economy value wealth.financing_profile names the profile, and takes one of
# three forms:
#   'none' or 'off'  -> the channel is off, s = 0 everywhere
#   'flat:<s>'       -> a constant s at every cell, with identity transition
#   <folder name>    -> that profile folder
#
# The resolved profile is cached per process.
#-------------------------------------------------------------------------------

WEALTH_PROFILES_ROOT   = './config/calibrations/wealth_profiles'
WEALTH_DEFAULT_PROFILE = 'default'

# Cache of resolved profiles, keyed by profile and grid. Each SLURM worker is a
# separate process and builds its own.
.wealth_profile_cache = new.env(parent = emptyenv())


wealth_dyn_profile_spec = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Determines which profile source a scenario resolves to, without building any
  # matrices. See the note above for the three forms the value can take.
  #
  # Returns: list of kind ('off', 'folder', or 'scalar') and value.
  #----------------------------------------------------------------------------

  wf = trimws(as.character(economy_param('wealth', 'financing_profile'))[1])

  if (is.na(wf) || !nzchar(wf)) {
    return(list(kind = 'folder', value = WEALTH_DEFAULT_PROFILE))
  }
  if (tolower(wf) %in% c('none', 'off')) {
    return(list(kind = 'off', value = NA))
  }
  if (startsWith(wf, 'flat:')) {
    s = suppressWarnings(as.numeric(str_remove(wf, '^flat:')))
    if (is.na(s)) {
      stop("economy.wealth.financing_profile = '", wf, "' -- the flat shorthand ",
           'is flat:<number>, e.g. flat:0.5')
    }
    return(list(kind = 'scalar', value = s))
  }
  list(kind = 'folder', value = wf)
}



wealth_dyn_load_s_csv = function(path, ages, n_bins) {

  #----------------------------------------------------------------------------
  # Reads a profile's s.csv into a saving share matrix of age by percentile.
  # Checks the required columns, the range of s, the range of age and percentile,
  # duplicate cells, and coverage of every cell. An incomplete grid is an error
  # rather than a fill, since it would otherwise zero out some cells' response
  # without saying so.
  #
  # Returns: matrix of saving shares, ages on rows and percentiles on columns.
  #----------------------------------------------------------------------------

  d = path %>% fread() %>% tibble()
  need = c('age', 'nw_pctile', 's')
  missing = setdiff(need, names(d))
  if (length(missing) > 0) {
    stop("wealth_dynamics profile s.csv (", path, ") is missing column(s): ",
         paste(missing, collapse = ', '), '. Required: age, nw_pctile, s.')
  }
  d = d %>% transmute(age       = as.integer(age),
                      nw_pctile = as.integer(nw_pctile),
                      s         = as.numeric(s))

  if (anyNA(d$age) || anyNA(d$nw_pctile) || anyNA(d$s)) {
    stop('wealth_dynamics profile s.csv (', path, ') has non-numeric/NA entries.')
  }
  if (any(d$s < 0 | d$s > 1)) {
    bad = which(d$s < 0 | d$s > 1)[1]
    stop(sprintf(paste0('wealth_dynamics profile s.csv (%s): s must be in [0, 1] ',
                        '(s = 1 - MPC). First offending cell: age %d, pctile %d, ',
                        's = %s.'),
                 path, d$age[bad], d$nw_pctile[bad], d$s[bad]))
  }
  if (any(d$age < min(ages) | d$age > max(ages))) {
    stop(sprintf('wealth_dynamics profile s.csv (%s): age outside [%d, %d].',
                 path, min(ages), max(ages)))
  }
  if (any(d$nw_pctile < 1 | d$nw_pctile > n_bins)) {
    stop(sprintf('wealth_dynamics profile s.csv (%s): nw_pctile outside [1, %d].',
                 path, n_bins))
  }
  if (anyDuplicated(d[c('age', 'nw_pctile')])) {
    stop('wealth_dynamics profile s.csv (', path,
         ') has duplicate (age, nw_pctile) rows.')
  }
  if (nrow(d) != length(ages) * n_bins) {
    stop(sprintf(paste0('wealth_dynamics profile s.csv (%s) must cover all %d ',
                        '(age x pctile) cells exactly once; found %d rows.'),
                 path, length(ages) * n_bins, nrow(d)))
  }

  s_mat = matrix(NA_real_, length(ages), n_bins, dimnames = list(ages, NULL))
  s_mat[cbind(match(d$age, ages), d$nw_pctile)] = d$s
  if (anyNA(s_mat)) {
    stop('wealth_dynamics profile s.csv (', path,
         ') does not cover every (age, pctile) cell.')
  }
  s_mat
}



wealth_dyn_load_M = function(dir, n_bins) {

  #----------------------------------------------------------------------------
  # Reads a profile's transition matrix M. Prefers M.csv, a headerless numeric
  # grid, and falls back to M.rds, either a matrix or a list by age. An absent M
  # file means the identity, or full persistence. Matrices are raked to doubly
  # stochastic on the way in.
  #
  # Returns: transition matrix, or a list of them by age.
  #----------------------------------------------------------------------------

  csv = file.path(dir, 'M.csv')
  rds = file.path(dir, 'M.rds')

  if (file.exists(csv)) {
    M = as.matrix(fread(csv, header = FALSE))
    storage.mode(M) = 'double'
    if (nrow(M) != n_bins || ncol(M) != n_bins) {
      stop(sprintf('wealth_dynamics profile M.csv (%s) must be %d x %d; got %d x %d.',
                   csv, n_bins, n_bins, nrow(M), ncol(M)))
    }
    if (any(M < 0, na.rm = TRUE) || anyNA(M)) {
      stop('wealth_dynamics profile M.csv (', csv,
           ') has negative or NA entries.')
    }
    return(sinkhorn_rake(M))
  }

  if (file.exists(rds)) {
    M = readRDS(rds)
    if (is.list(M) && !is.matrix(M)) return(lapply(M, sinkhorn_rake))
    if (nrow(M) != n_bins || ncol(M) != n_bins) {
      stop(sprintf('wealth_dynamics profile M.rds (%s) must be %d x %d.',
                   rds, n_bins, n_bins))
    }
    return(sinkhorn_rake(M))
  }

  diag(n_bins)                          # no M file, so full persistence
}



wealth_dyn_load_profile_folder = function(name, ages, n_bins) {

  # Reads a profile folder's s.csv and M file into a saving share matrix and a
  # transition matrix.
  dir = file.path(WEALTH_PROFILES_ROOT, name)
  if (!dir.exists(dir)) {
    stop(sprintf(paste0("wealth_dynamics: financing profile '%s' not found at %s. ",
                        'Point economy.wealth.financing_profile at a folder ',
                        "under %s, use the flat:<s> shorthand, or ship the '%s' ",
                        'profile.'),
                 name, dir, WEALTH_PROFILES_ROOT, WEALTH_DEFAULT_PROFILE))
  }
  s_path = file.path(dir, 's.csv')
  if (!file.exists(s_path)) {
    stop("wealth_dynamics: financing profile '", name, "' has no s.csv (", s_path, ').')
  }
  list(s_mat  = wealth_dyn_load_s_csv(s_path, ages, n_bins),
       M      = wealth_dyn_load_M(dir, n_bins),
       source = sprintf('profile %s', name))
}



wealth_dyn_resolve_profile = function(scenario_info, params = NULL,
                                      ages = NULL, n_bins = NULL) {

  #----------------------------------------------------------------------------
  # Resolves a scenario to its financing profile, following the precedence in
  # wealth_dyn_profile_spec(). The flat and off forms are built in memory; a
  # folder name is read from disk. Results are cached.
  #
  # Also attaches active (whether any cell saves), ages, n_bins, source, and a
  # fingerprint used for logging and state stamping. The fingerprint is not a
  # provenance check, since profiles are meant to vary across scenarios.
  #
  # Returns: list of s_mat and M, plus the fields above.
  #----------------------------------------------------------------------------

  if (is.null(params)) params = wealth_dyn_load_params()
  if (is.null(ages))   ages   = WEALTH_DYN_AGE_MIN:WEALTH_DYN_AGE_MAX
  if (is.null(n_bins)) n_bins = params$n_pctiles

  spec = wealth_dyn_profile_spec(scenario_info)
  key  = paste(spec$kind, spec$value, min(ages), max(ages), n_bins, sep = '|')
  hit  = .wealth_profile_cache[[key]]
  if (!is.null(hit)) return(hit)

  zero_mat = function() matrix(0, length(ages), n_bins, dimnames = list(ages, NULL))
  prof = switch(
    spec$kind,
    'off'    = list(s_mat = zero_mat(), M = diag(n_bins),
                    source = 'off (forced dormant)'),
    'scalar' = {
      if (spec$value < 0 || spec$value > 1)
        stop(sprintf(paste0('wealth_dynamics: scalar saving share s = %s is ',
                            'outside [0, 1] (s = 1 - MPC). Use a profile folder ',
                            'for bracket-varying s, or fix the runscript.'),
                     spec$value))
      list(s_mat = matrix(spec$value, length(ages), n_bins,
                          dimnames = list(ages, NULL)),
           M = diag(n_bins),
           source = sprintf('scalar s = %s', spec$value))
    },
    'folder' = wealth_dyn_load_profile_folder(spec$value, ages, n_bins),
    stop('wealth_dynamics: unknown profile kind ', spec$kind))

  prof$ages   = ages
  prof$n_bins = n_bins
  prof$active = any(prof$s_mat > 0)
  prof$fingerprint = list(
    s_mean        = mean(prof$s_mat),
    s_min         = min(prof$s_mat),
    s_max         = max(prof$s_mat),
    s_sd          = stats::sd(as.numeric(prof$s_mat)),
    M_is_identity = is.matrix(prof$M) &&
                    isTRUE(all.equal(unname(prof$M), diag(n_bins))))

  .wealth_profile_cache[[key]] = prof
  prof
}



#-------------------------------------------------------------------------------
# Refusal gate
#-------------------------------------------------------------------------------

wealth_dyn_check_run_compat = function(scenario_info, vat_price_offset) {

  #----------------------------------------------------------------------------
  # Checks the preconditions for the pre-pass and the applier. Cell state is in
  # raw wealth dollars while the tax delta is in adjusted dollars, and the cells
  # are sparser than the capital gains bathtub's, so the channel takes the shared
  # guard for raw-dollar channels. Staleness of the profile is checked at parse
  # time instead.
  #
  # Returns: invisibly TRUE; stops on violation.
  #----------------------------------------------------------------------------

  check_raw_data_channel_compat('wealth_dynamics (s > 0)', scenario_info,
                                vat_price_offset)
  invisible(TRUE)
}



#-------------------------------------------------------------------------------
# Cohort key and capital income helpers
#-------------------------------------------------------------------------------

wealth_dyn_age_cohort = function(tax_units) {

  #----------------------------------------------------------------------------
  # Assigns each record its cell's age. Joint records take the older of the two
  # ages, the both-die event their mortality rate already reflects, applied
  # before the top code. Matches the capital gains bathtub and distribution.R.
  # The pre-pass and the applier have to compute this the same way.
  #
  # Returns: integer vector of cohort ages.
  #----------------------------------------------------------------------------

  a = if_else(tax_units$filing_status == 2,
              pmax(tax_units$age1, tax_units$age2, na.rm = TRUE),
              tax_units$age1)
  pmax(WEALTH_DYN_AGE_MIN, pmin(WEALTH_DYN_AGE_MAX, a))
}



wealth_dyn_safe_col = function(df, col) {

  # Returns a column with NAs set to 0, or a vector of 0s if it is absent.
  if (col %in% names(df)) replace_na(df[[col]], 0) else rep(0, nrow(df))
}



wealth_dyn_capital_total = function(df) {

  #----------------------------------------------------------------------------
  # Calculates each record's taxable capital income, as the weighted sum of the
  # flows above. Pure capital flows enter at weight 1, with rental and
  # estate/trust income net of losses; pass-through income enters net at weight
  # 0.2. Missing columns are treated as 0.
  #
  # Returns: numeric vector of capital income in dollars.
  #----------------------------------------------------------------------------

  g = function(col) wealth_dyn_safe_col(df, col)

  f_pure = g('txbl_int') + g('exempt_int') + g('div_ord') + g('div_pref') +
           g('kg_st') + g('kg_lt') + g('kg_1250') + g('kg_collect') +
           (g('rent')   - g('rent_loss')) +
           (g('estate') - g('estate_loss'))

  f_pt_net = g('sole_prop') +
             (g('part_active') + g('part_passive') -
              g('part_active_loss') - g('part_passive_loss') - g('part_179')) +
             (g('scorp_active') + g('scorp_passive') -
              g('scorp_active_loss') - g('scorp_passive_loss') - g('scorp_179')) +
             g('farm')

  f_pure + wealth_cap_flows_pt_weight() * f_pt_net
}



wealth_dyn_economic_gross = function(df) {

  # Sums the asset columns to gross assets, the same quantity calc_estate()
  # forms inline.
  rowSums(cols_matrix(df, ESTATE_ASSET_COLS))
}

cols_matrix = function(df, cols) {
  # Returns the requested columns as a numeric matrix, with NAs set to 0 and
  # absent columns dropped. Always has one row per record.
  present = intersect(cols, names(df))
  if (length(present) == 0) return(matrix(0, nrow(df), 1))
  m = as.matrix(df[present])
  m[is.na(m)] = 0
  m
}



calc_cap_bundle_mtr = function(tax_units, actual_liab_iit, baseline_pr_er,
                               vars_1040, vars_payroll) {

  #----------------------------------------------------------------------------
  # Calculates each record's marginal income tax rate on its capital income as a
  # whole. Scales the record's capital flows in the same proportions the haircut
  # scales them, recalculates taxes, and differences:
  #
  #   tau = (liab_iit_net(scaled) - liab_iit_net) / (bump * capital income)
  #
  # Measuring the rate through the calculator rather than assigning it from
  # statutory rates means reforms to QBI, the municipal bond exclusion or the
  # gain rates are picked up without further work, as are the effects capital
  # income has elsewhere on the return through Social Security taxability, AGI
  # phaseouts and the NIIT. Income tax only, so the SECA response on
  # pass-through income is left out.
  #
  # Must run on the same frame the tax delta is measured on, before deemed
  # realizations are folded in, and over the whole frame rather than a subset,
  # since random numbers are assigned by position.
  #
  # Parameters:
  #   - tax_units (df)          : tax units before do_taxes
  #   - actual_liab_iit (dbl[]) : liab_iit_net on the unscaled frame
  #   - baseline_pr_er (df)     : baseline employer-side payroll liabilities
  #   - vars_1040 (str[])       : names of 1040 variables to return
  #   - vars_payroll (str[])    : names of payroll tax variables to return
  #
  # Returns: tibble of mtr_cap_bundle and cap_bundle_F, one row per record in
  #          the order of tax_units (df).
  #----------------------------------------------------------------------------

  eps   = WEALTH_DYN_MTR_BUMP
  F     = wealth_dyn_capital_total(tax_units)
  gross = wealth_dyn_economic_gross(tax_units)

  pt_cols = intersect(c(WEALTH_CAP_FLOWS_PT, WEALTH_CAP_FLOWS_SE_COMPANIONS),
                      names(tax_units))
  pure_cols = intersect(c(WEALTH_CAP_FLOWS_PURE, WEALTH_CAP_FLOWS_PURE_BASIS),
                        names(tax_units))

  bumped = tax_units %>%
    mutate(across(all_of(pure_cols), ~ . * (1 + eps)),
           across(all_of(pt_cols),   ~ . * (1 + wealth_cap_flows_pt_weight() * eps)))

  taxed_bumped = bumped %>%
    do_taxes(baseline_pr_er   = baseline_pr_er,
             vars_1040        = vars_1040,
             vars_payroll     = vars_payroll,
             calc_estate_flag = FALSE,
             calc_wealth_flag = FALSE)

  stopifnot(identical(taxed_bumped$id, tax_units$id))

  dT  = taxed_bumped$liab_iit_net - actual_liab_iit
  # Records with too little capital income to measure against, and those with a
  # net capital loss, get no rate.
  ok  = F >= pmax(WEALTH_DYN_F_FLOOR * gross, WEALTH_DYN_EPS)
  mtr = if_else(ok, dT / (eps * F), 0)

  tibble(mtr_cap_bundle = mtr, cap_bundle_F = F)
}



#-------------------------------------------------------------------------------
# Wealth growth rate
#-------------------------------------------------------------------------------

wealth_dyn_read_rtotal = function(scenario_info, params) {

  #----------------------------------------------------------------------------
  # Calculates the growth rate of wealth by year, taken to be growth in nominal
  # GDP per capita:
  #
  #   r_total = (gdp_t / gdp_t-1) / (pop_t / pop_t-1) - 1
  #
  # Nominal, to match the nominal wealth stock. Population is the sum of the
  # married and unmarried tax unit counts by age. Note that gdp_c is the
  # consumption component of GDP, not GDP per capita, and is not what is wanted
  # here.
  #
  # Historical and projected series are spliced rather than reading projections
  # alone. Projections start in the first projection year, so the growth rate of
  # that year, and of any earlier lead-in year, has no prior year to difference
  # against. Splicing gives the boundary year a real predecessor, as the CPI and
  # Treasury rate loaders in src/sim/kg/ also do.
  #
  # Returns: named numeric vector of growth rates by year.
  #----------------------------------------------------------------------------

  macro_root = scenario_info$interface_paths$`Macro-Projections`
  read_gdp_pop = function(f) {
    raw = read_csv(file.path(macro_root, f), show_col_types = FALSE)
    pop_cols = grep('^(unmarried|married)_[0-9]+$', names(raw), value = TRUE)
    if (length(pop_cols) == 0) {
      stop('wealth_dynamics: no unmarried_*/married_* population columns in ',
           'Macro-Projections ', f, '; cannot form GDP per capita.')
    }
    raw %>% transmute(year, gdp,
                      pop = rowSums(across(all_of(pop_cols), ~ replace_na(., 0))))
  }

  # Splice historical ahead of projections so that every requested year has a
  # prior year to difference against. Where the two series overlap, distinct()
  # keeps the historical row.
  macro = bind_rows(read_gdp_pop('historical.csv'),
                    read_gdp_pop('projections.csv')) %>%
    distinct(year, .keep_all = TRUE) %>%
    arrange(year) %>%
    mutate(r = (gdp / lag(gdp)) / (pop / lag(pop)) - 1)

  delta = params$r_total$additive_delta %||% 0
  yrs   = scenario_info$years
  r     = macro$r[match(yrs, macro$year)] + delta

  if (anyNA(r)) {
    stop('wealth_dynamics: nominal GDP/capita growth missing for year(s) ',
         paste(yrs[is.na(r)], collapse = ', '),
         ' in spliced Macro-Projections historical.csv + projections.csv. ',
         'r_total(t) is undefined.')
  }
  setNames(r, as.character(yrs))
}



#-------------------------------------------------------------------------------
# Detail files for the pass measuring the tax change
#-------------------------------------------------------------------------------

wealth_dyn_convnw_detail_dir = function(scenario_info, leg = 'conventional') {
  # A separate output root, so this intermediate pass never overwrites the final
  # detail that distribution and receipts read. No totals or receipts are written
  # for it. The mechanical and conventional rungs each measure their own forcing,
  # so each has its own no-wealth tree.
  file.path(scenario_info$output_path, paste0(leg, '_no_wealth'), 'detail')
}

wealth_dyn_convnw_detail_path = function(scenario_info, year,
                                         leg = 'conventional') {
  file.path(wealth_dyn_convnw_detail_dir(scenario_info, leg), paste0(year, '.csv'))
}



#-------------------------------------------------------------------------------
# Pre-pass: accumulating the wealth shortfall by cell
#-------------------------------------------------------------------------------

run_wealth_bathtub_pass = function(scenario_info, tax_law,
                                   vat_price_offset = NULL,
                                   leg = c('conventional', 'mechanical'),
                                   purge_transient = FALSE) {

  #----------------------------------------------------------------------------
  # Runs the wealth shortfall recurrence for one scenario, one year at a time.
  # For each year, reads the scenario's detail from the pass that excludes this
  # channel along with the baseline static detail, assigns cells, and forms the
  # forcing each cell faces:
  #
  #   F = change in during-life tax - change in outside income
  #     = change in (income tax + payroll - deemed + wealth tax)
  #         - corp_dY_exog - pr_dY_exog
  #
  # The two income terms are shocks to income from outside the tax system: the
  # corporate channel's cut to dividends, interest and rent, and the employer
  # payroll adjustment's shave to wages. Each is 0 for a scenario without the
  # provision that produces it, leaving the forcing as the tax change alone. The
  # baseline leg carries no such shock.
  # A gain realized during life or a retirement distribution is not outside
  # income: the household already loses those resources through the balance
  # sheet, so only their tax consequence enters here.
  #
  # The shortfall then accumulates over cells as
  #
  #   P(t) = G(t) * [P(t-1), aged and moved across percentiles] + s * F(t)
  #
  # where growth is damped by the tax the eroded wealth no longer generates,
  #
  #   G(t) = (1 + r_total(t)) - s * (tau * y + tau_w)
  #
  # with y the cell's capital income per dollar of assets, tau its marginal rate
  # on that income, and tau_w its marginal wealth tax rate. There is no survival
  # term: deaths enter only at aggregation, through each record's mortality rate.
  #
  # Writes the shortfall and the percentile cutoffs by year to the scenario's
  # {leg}/supplemental/wealth_dynamics_state/. Requires that both this scenario's
  # no-wealth detail for the leg and the baseline static detail already exist.
  #
  # Parameters:
  #   - scenario_info (list)  : output of get_scenario_info()
  #   - tax_law (df)          : joined tax law tibble; see build_tax_law()
  #   - vat_price_offset (df) : VAT price offset tibble
  #   - leg (str)             : which rung's forcing to measure, 'conventional'
  #                             (default) or 'mechanical'. The mechanical rung
  #                             measures the forcing with behavior off, so its
  #                             drawdown compounds the static tax change.
  #   - purge_transient (bool): delete the leg's no-wealth detail once this pass
  #                             has read it. Nothing downstream of here reads it,
  #                             so this is what keeps peak disk at three detail
  #                             trees a scenario rather than five. Off by default:
  #                             the gains elasticity harness reads a scenario's
  #                             conventional_no_wealth detail after the run
  #
  # Returns: invisibly NULL.
  #----------------------------------------------------------------------------

  leg = match.arg(leg)

  wealth_dyn_check_run_compat(scenario_info, vat_price_offset)

  params  = wealth_dyn_load_params()
  ages    = WEALTH_DYN_AGE_MIN:WEALTH_DYN_AGE_MAX
  n_ages  = length(ages)
  n_bins  = params$n_pctiles

  # Get the saving share by cell and the percentile transition
  profile = wealth_dyn_resolve_profile(scenario_info, params, ages, n_bins)
  s_mat   = profile$s_mat
  M       = profile$M
  message(sprintf('wealth_dynamics: financing profile = %s (s in [%.3f, %.3f], mean %.3f; M %s)',
                  profile$source, profile$fingerprint$s_min, profile$fingerprint$s_max,
                  profile$fingerprint$s_mean,
                  if (isTRUE(profile$fingerprint$M_is_identity)) 'identity' else 'non-identity'))

  A       = build_aging_matrix(ages)
  r_total = wealth_dyn_read_rtotal(scenario_info, params)
  years   = scenario_info$years

  has_baseline = !is.null(globals$baseline_root)

  # Cell shortfall in dollars. Zero before the first reform year, where scenario
  # law and baseline law agree and the tax change is zero.
  P = matrix(0, n_ages, n_bins, dimnames = list(ages, NULL))

  for (t in years) {

    scen = wealth_dyn_read_convnw_detail(scenario_info, t, leg)
    base = wealth_dyn_read_baseline_detail(t, has_baseline)

    # Drop dependent returns, matching the population distribution.R forms the
    # during-life tax over. The applier drops them too, so the records that are
    # forced and the records that are drained are the same.
    scen = scen %>% filter(dep_status == 0)

    # Assign cells, ranking on positive raw net worth. Rank on net_worth_raw
    # rather than net_worth, which a behavior module such as wealth avoidance
    # may have overwritten. The applier ranks on the raw stock, so the pre-pass
    # has to as well, or the cells disagree and the shortfall is not conserved.
    scen$age_cohort = wealth_dyn_age_cohort(scen)
    cutoffs = compute_within_age_cutoffs(scen$net_worth_raw, scen$weight,
                                         scen$age_cohort, ages, n_bins,
                                         positive_only = TRUE)
    scen$bin = assign_within_age_bin(scen$net_worth_raw, scen$age_cohort, cutoffs,
                                     n_bins, positive_only = TRUE)

    # Forcing per record: the change in during-life tax, less the change in
    # income from outside the tax system. During-life tax is income tax plus
    # payroll less deemed realizations, plus the wealth tax. The outside income
    # shock is negative for a tax increase, which makes the forcing positive:
    # the household draws down wealth to hold consumption up against the income
    # it has lost, net of the tax it no longer owes on it.
    scen = scen %>%
      mutate(liab_iit_pr_scen = liab_iit_net + liab_pr - coalesce(liab_deemed, 0)) %>%
      left_join(base %>% select(id, liab_iit_pr_base, liab_wealth_base),
                by = 'id') %>%
      mutate(liab_iit_pr_base = coalesce(liab_iit_pr_base, 0),
             liab_wealth_base = coalesce(liab_wealth_base, 0),
             dT0 = weight * ((liab_iit_pr_scen + liab_wealth) -
                             (liab_iit_pr_base + liab_wealth_base) -
                             coalesce(corp_dY_exog, 0) -
                             coalesce(pr_dY_exog, 0)))

    # Aggregate to cells, dropping records with no cell (zero or negative net
    # worth).
    cells = scen %>%
      filter(!is.na(bin)) %>%
      mutate(bundle_is_rate = !is.na(mtr_cap_bundle) &
                              mtr_cap_bundle >= WEALTH_DYN_MTR_RANGE[1] &
                              mtr_cap_bundle <= WEALTH_DYN_MTR_RANGE[2]) %>%
      group_by(age_cohort, bin) %>%
      summarise(
        dT0_cell = sum(dT0,                                     na.rm = TRUE),
        F_signed = sum(weight * cap_bundle_F,                   na.rm = TRUE),
        F_pos    = sum(weight * pmax(cap_bundle_F, 0) * bundle_is_rate,
                       na.rm = TRUE),
        Fmtr_pos = sum(weight * pmax(cap_bundle_F, 0) * bundle_is_rate *
                         mtr_cap_bundle, na.rm = TRUE),
        gross    = sum(weight * economic_gross,                 na.rm = TRUE),
        nw_pos   = sum(weight * pmax(net_worth_raw, 0),         na.rm = TRUE),
        nwmtr    = sum(weight * pmax(net_worth_raw, 0) * mtr_net_worth, na.rm = TRUE),
        .groups  = 'drop') %>%
      mutate(
        # Capital income per dollar of assets, capped as described above; the
        # income-weighted marginal rate on capital income; and the net
        # worth-weighted marginal wealth tax rate.
        y    = pmin(pmax(if_else(gross > WEALTH_DYN_EPS, F_signed / gross, 0), 0),
                    WEALTH_DYN_Y_MAX),
        tau  = if_else(F_pos  > WEALTH_DYN_EPS, Fmtr_pos / F_pos,  0),
        tau_w= if_else(nw_pos > WEALTH_DYN_EPS, nwmtr    / nw_pos, 0))

    # Scatter the cell quantities into matrices of age by percentile.
    dT0_mat = matrix(0, n_ages, n_bins, dimnames = list(ages, NULL))
    y_mat   = matrix(0, n_ages, n_bins)
    tau_mat = matrix(0, n_ages, n_bins)
    tw_mat  = matrix(0, n_ages, n_bins)
    ri = match(cells$age_cohort, ages)
    ci = cells$bin
    idx = cbind(ri, ci)
    dT0_mat[idx] = cells$dT0_cell
    y_mat[idx]   = cells$y
    tau_mat[idx] = cells$tau
    tw_mat[idx]  = cells$tau_w

    # Growth factor. The income tax term is a rate on income, so it is converted
    # to a rate on wealth by the yield; the wealth tax is already a rate on
    # wealth and enters directly. Both terms are the tax forgone as wealth
    # erodes, which cannot be negative, so both are floored at zero. Without
    # that, a cell whose marginal rate is negative through a refundable credit
    # or a phase-in would grow faster than the economy.
    rt = unname(r_total[as.character(t)])
    G  = (1 + rt) - s_mat * (pmax(tau_mat * y_mat, 0) + pmax(tw_mat, 0))
    # A non-finite growth factor means an input was mis-scaled.
    if (any(!is.finite(G))) {
      bad = which(!is.finite(G), arr.ind = TRUE)
      stop(sprintf(paste0('wealth_dynamics: non-finite kernel G in %d cell(s) ',
                          'for year %d (e.g. age %s, pctile %d). Mis-scaled ',
                          'tau/y/tau_w input.'),
                   nrow(bad), t, ages[bad[1, 1]], bad[1, 2]))
    }
    # Hold G inside its plausible range. Given the cap on y and the floors
    # above, this binds only in sparse low net worth cells with a large one-time
    # realization, which hold too little of the shortfall to matter. Warn rather
    # than clamp silently: a large count would mean something is mis-scaled.
    n_clamp = sum(G < WEALTH_DYN_G_FLOOR)
    if (n_clamp > 0) {
      warning(sprintf(paste0('wealth_dynamics: clamped kernel G to >= %.0e in ',
                            '%d/%d cell(s), year %d (min raw G=%.4f). Expected ',
                            'only in sparse low-net-worth lumpy-realization ',
                            'cells (negligible dollar impact); a large count ',
                            'would indicate a scaling problem.'),
                    WEALTH_DYN_G_FLOOR, n_clamp, length(G), t, min(G)))
    }
    G = pmin(pmax(G, WEALTH_DYN_G_FLOOR), 1 + rt)

    # Step forward: the shortfall carried in from last year, aged and moved
    # across percentiles, grows by G, and this year's saving out of the tax
    # change enters at face value, since it is assumed to happen at year end.
    P = cohort_recurrence_step(P_prev = P, growth = G, inflow = s_mat * dT0_mat,
                               A = A, M_by_age = M)

    write_cohort_state(
      state = list(P            = P,
                   cutoffs      = cutoffs,
                   ages         = ages,
                   n_bins       = n_bins,
                   year         = t,
                   r_total      = rt,
                   # s is the flat saving share where there is one, and NA
                   # otherwise. Kept for diagnostics that predate s_mat.
                   s_mat        = s_mat,
                   s            = if (isTRUE(all.equal(min(s_mat), max(s_mat))))
                                    s_mat[1] else NA_real_,
                   profile_src  = profile$source,
                   profile_fp   = profile$fingerprint,
                   spec_version = WEALTH_DYN_SPEC_VERSION,
                   # Diagnostics, used to check the recurrence by hand
                   diag         = list(y = y_mat, tau = tau_mat, tau_w = tw_mat,
                                       G = G, dT0 = dT0_mat)),
      scenario_info = scenario_info,
      subdir        = 'wealth_dynamics_state',
      year          = t,
      pass          = leg)
  }

  # The forcing has been read for every year and written into the state files, so
  # the leg's no-wealth detail has no remaining reader
  if (isTRUE(purge_transient)) {
    root = paste0(leg, '_no_wealth')
    purge_detail(passes = root)
    message(sprintf('wealth_dynamics: purged %s detail for %s, read in full.',
                    root, scenario_info$ID))
  }

  invisible(NULL)
}



wealth_dyn_read_convnw_detail = function(scenario_info, year,
                                          leg = 'conventional') {

  # Reads the columns the pre-pass needs from one year of the scenario's
  # no-wealth detail. Errors on a missing file or missing columns, since an old
  # or partial file would zero out the forcing without saying so.
  path = wealth_dyn_convnw_detail_path(scenario_info, year, leg)
  if (!file.exists(path)) {
    stop('wealth_dynamics pre-pass: ', leg, ' no-wealth detail missing: ', path,
         ' (was the no-wealth phase run for this scenario-year?)')
  }
  need = c('id', 'weight', 'dep_status', 'filing_status', 'age1', 'age2',
           'net_worth', 'net_worth_raw', 'liab_iit_net', 'liab_pr', 'liab_wealth',
           'estate_m', 'economic_gross', 'cap_bundle_F', 'mtr_cap_bundle',
           'mtr_net_worth')
  optional = c('liab_deemed', 'corp_dY_exog', 'pr_dY_exog')

  # Read the header first, then only the 18 columns of about 98 that are needed.
  # Detail files run to 150MB, so parsing the rest is wasted time.
  have = names(fread(path, nrows = 0))
  missing = setdiff(need, have)
  if (length(missing) > 0) {
    stop('wealth_dynamics pre-pass: ', leg, ' no-wealth detail ', path,
         ' is missing required column(s): ', paste(missing, collapse = ', '))
  }
  d = path %>%
    fread(select = c(need, intersect(optional, have))) %>%
    tibble()
  if (!('liab_deemed' %in% names(d))) d$liab_deemed = 0
  # The corporate channel's shock to outside income. Absent for scenarios
  # without a corporate provision, and for detail written before the channel
  # existed; zero leaves the forcing as the tax change alone.
  if (!('corp_dY_exog' %in% names(d))) d$corp_dY_exog = 0
  # The employer payroll wage adjustment's shave to cash wages, under the same
  # contract. Absent for scenarios not touching employer payroll law.
  if (!('pr_dY_exog' %in% names(d)))   d$pr_dY_exog = 0
  d
}



wealth_dyn_read_baseline_detail = function(year, has_baseline) {

  # Reads during-life tax from the baseline static detail, for the baseline side
  # of the forcing. The baseline runs with no behavior and no wealth channel, so
  # its static and no-wealth detail are the same thing. Deemed realizations and
  # the wealth tax default to zero.
  if (!has_baseline) {
    stop('wealth_dynamics pre-pass: no baseline_root available, but the tax ',
         'change is measured as scenario less baseline. Supply a baseline.')
  }
  path = globals$baseline_root %>%
    file.path('baseline/static/detail', paste0(year, '.csv'))
  if (!file.exists(path)) {
    stop('wealth_dynamics pre-pass: baseline static detail missing: ', path)
  }
  need     = c('id', 'liab_iit_net', 'liab_pr')
  optional = c('liab_deemed', 'liab_wealth')

  # Header first, then the handful of columns needed
  have = names(fread(path, nrows = 0))
  missing = setdiff(need, have)
  if (length(missing) > 0) {
    stop('wealth_dynamics pre-pass: baseline detail ', path,
         ' is missing required column(s): ', paste(missing, collapse = ', '))
  }
  d = path %>%
    fread(select = c(need, intersect(optional, have))) %>%
    tibble()
  if (!('liab_deemed' %in% names(d))) d$liab_deemed = 0
  if (!('liab_wealth' %in% names(d))) d$liab_wealth = 0
  d %>%
    transmute(id,
              liab_iit_pr_base = liab_iit_net + liab_pr - coalesce(liab_deemed, 0),
              liab_wealth_base = liab_wealth)
}



#-------------------------------------------------------------------------------
# Applier: draining the shortfall out of records
#-------------------------------------------------------------------------------

wealth_dyn_apply_to_records = function(tax_units, state, params = NULL,
                                       rank_value = NULL) {

  #----------------------------------------------------------------------------
  # Takes each record's share of its cell's accumulated shortfall out of its
  # wealth, before the behavior modules and do_taxes run. The share is
  # proportional to net worth:
  #
  #   allocated  = P * net worth / cell total weighted positive net worth
  #   f          = allocated / gross assets, clamped to plus or minus fmax
  #
  # Assets are then scaled by (1 - f). Every asset column is scaled by the same
  # factor, so that the whole balance sheet shrinks and the pass-through share
  # and its frozen valuation discount are left where they are. Capital income
  # flows and basis scale the same way, pass-through flows and their earner
  # splits at the 0.2 weight, and retirement distributions with the balances
  # they are drawn from. Net worth is then recomputed, leaving debts alone.
  #
  # The eroded assets feed calc_estate, lowering the estate base, and calc_wealth,
  # which reprices the wealth tax on the smaller stock. Records with no cell, at
  # zero or negative net worth, are left alone.
  #
  # Parameters:
  #   - tax_units (df)    : conventional-pass tax units, before behavior
  #   - state (list)      : the year's saved shortfall and percentile cutoffs
  #   - params (list)     : wealth parameters; loaded if NULL
  #   - rank_value (dbl[]): net worth to rank on, defaulting to the frame's own.
  #                         A caller that has already transformed the frame, as
  #                         the corporate markdown does, passes the raw stock so
  #                         that records land in the cells the pre-pass gave
  #                         them. The allocation denominator still uses the live
  #                         frame, which does not affect conservation.
  #
  # Returns: tax units with eroded assets, flows and basis, recomputed net
  #          worth, and the columns nw_pctile, D_alloc and wealth_haircut (df).
  #----------------------------------------------------------------------------

  if (is.null(params)) params = wealth_dyn_load_params()
  fmax   = params$fmax
  n_bins = state$n_bins
  ages   = state$ages
  P      = state$P

  n = nrow(tax_units)
  age_cohort = wealth_dyn_age_cohort(tax_units)
  # Rank on raw net worth. The applier runs before the behavior modules, so the
  # frame's own net worth is still the raw stock the pre-pass ranked on, unless
  # the caller has passed the raw stock explicitly.
  if (is.null(rank_value)) rank_value = tax_units$net_worth
  stopifnot(length(rank_value) == n)
  bin = assign_within_age_bin(rank_value, age_cohort, state$cutoffs,
                              n_bins, positive_only = TRUE)
  # Drop dependent returns, matching the pre-pass.
  if ('dep_status' %in% names(tax_units)) {
    bin[tax_units$dep_status != 0] = NA_integer_
  }

  gross = wealth_dyn_economic_gross(tax_units)
  nw_pos = pmax(tax_units$net_worth, 0)

  # Each record's cell shortfall, and its cell's total weighted net worth.
  P_i        = rep(0, n)
  cell_denom = rep(0, n)
  has_cell   = !is.na(bin)
  if (any(has_cell)) {
    ri = match(age_cohort[has_cell], ages)
    ci = bin[has_cell]
    P_i[has_cell] = P[cbind(ri, ci)]
    # Weighted positive net worth by cell
    denom_tbl = tibble(age_cohort = age_cohort[has_cell],
                       bin        = ci,
                       wnw        = tax_units$weight[has_cell] * nw_pos[has_cell]) %>%
      group_by(age_cohort, bin) %>%
      summarise(denom = sum(wnw, na.rm = TRUE), .groups = 'drop')
    key = tibble(age_cohort = age_cohort[has_cell], bin = ci) %>%
      left_join(denom_tbl, by = c('age_cohort', 'bin'))
    cell_denom[has_cell] = key$denom
  }

  # Allocate in proportion to net worth, guarding against an empty cell or a
  # record with no assets.
  D_alloc = if_else(cell_denom > WEALTH_DYN_EPS & has_cell,
                    P_i * nw_pos / cell_denom, 0)
  f = if_else(gross > WEALTH_DYN_EPS, D_alloc / gross, 0)
  f = pmax(-fmax, pmin(fmax, f))

  pure_cols = intersect(c(WEALTH_CAP_FLOWS_PURE, WEALTH_CAP_FLOWS_PURE_BASIS),
                        names(tax_units))
  pt_cols   = intersect(c(WEALTH_CAP_FLOWS_PT, WEALTH_CAP_FLOWS_SE_COMPANIONS),
                        names(tax_units))
  ret_cols  = intersect(WEALTH_RET_DIST_FLOWS, names(tax_units))
  asset_cols = intersect(ESTATE_ASSET_COLS, names(tax_units))

  f_pure = 1 - f
  f_pt   = 1 - wealth_cap_flows_pt_weight() * f

  # Debts are left alone, so take them off the original frame and subtract them
  # back after the assets are eroded.
  debts = rowSums(cols_matrix(tax_units, WEALTH_DEBT_COLS))

  out = tax_units %>%
    mutate(
      across(all_of(asset_cols), ~ . * (1 - f)),
      across(all_of(pure_cols),  ~ . * f_pure),
      across(all_of(ret_cols),   ~ . * f_pure),
      across(all_of(pt_cols),    ~ . * f_pt)) %>%
    # Recompute net worth off the eroded balance sheet, as run_one_year() and
    # the wealth avoidance module also do, so that calc_wealth and calc_estate
    # both price off the smaller stock.
    mutate(
      net_worth      = rowSums(across(all_of(asset_cols), ~ replace_na(., 0))) - debts,
      # Diagnostics, written to non-baseline detail only
      nw_pctile      = bin,
      D_alloc        = D_alloc,
      wealth_haircut = f)

  out
}
