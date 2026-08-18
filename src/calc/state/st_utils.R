#---------------------------------------------------------------------------
# st_utils.R
#
# Shared primitives for the state calculators (2026-07-17 review item #3):
# one way to collect a vector-family parameter matrix, and explicit-boundary
# band/step lookups. Boundary semantics (> vs >=, ceiling vs floor) are
# POLICY choices -- each helper makes them explicit so a new state picks a
# semantic instead of re-coding an idiom (the historical off-by-one class).
#---------------------------------------------------------------------------


st_param_schema = local({

  #----------------------------------------------------------------------------
  # Reads and caches params_schema.yaml, the single source of truth for the
  # state law parameter contract (2026-07-17 review item #4): legal names,
  # neutral defaults, vector-family patterns, and feature-gate sentinels.
  # Cached per session because ensure_st_params() runs per state-year.
  # Schema conventions: a bare value is the default (a {default: x} mapping
  # is also legal); .inf/-.inf for infinities; null means NA.
  #
  # Returns: list with defaults (named num[]), sentinels (str[]), and
  #          registry (list of scalars str[] and families regex str[]).
  #----------------------------------------------------------------------------

  cache = NULL
  function(path = './config/scenarios/tax_law_state/params_schema.yaml') {
    if (!is.null(cache)) {
      return(cache)
    }
    raw = read_yaml(path)

    defaults = map_dbl(raw$scalars, function(x) {
      value = if (is.list(x)) x$default else x
      if (is.null(value)) NA_real_ else as.numeric(value)
    })
    families = map_chr(raw$families, 'pattern')

    if (length(defaults) == 0 || is.null(names(defaults)) ||
        anyDuplicated(names(defaults)) || length(families) == 0) {
      stop('params_schema.yaml is malformed: scalars must be uniquely ',
           'named and at least one family pattern must be present')
    }

    cache <<- list(
      defaults  = defaults,
      sentinels = unlist(raw$sentinels),
      registry  = list(
        scalars  = c(names(defaults), unlist(raw$extra_scalars)),
        families = families
      )
    )
    cache
  }
})



st_param_defaults = function() {

  #----------------------------------------------------------------------------
  # Neutral no-feature default for every optional scalar state law
  # parameter, from params_schema.yaml. Doubles as the registry of legal
  # scalar names (see st_param_name_registry()).
  #
  # Returns: named numeric vector of defaults (num[]).
  #----------------------------------------------------------------------------

  st_param_schema()$defaults
}



st_param_vector_sentinels = function() {

  #----------------------------------------------------------------------------
  # First-element column names that gate vector-family features, from
  # params_schema.yaml. ensure_st_params() adds an NA sentinel when a
  # state's law lacks the family entirely.
  #
  # Returns: character vector of column names (str[]).
  #----------------------------------------------------------------------------

  st_param_schema()$sentinels
}



st_param_name_registry = function() {

  #----------------------------------------------------------------------------
  # The complete set of state law parameter names the calculators read
  # (exact scalar names plus vector-family regex patterns), from
  # params_schema.yaml. Consumed by validate_state_param_names() to reject
  # unknown/misspelled YAML parameters at load time (review items #1/#2).
  #
  # Returns: list with scalars (str[]) and families (regex str[]).
  #----------------------------------------------------------------------------

  st_param_schema()$registry
}



st_family_matrix = function(tax_unit, prefix, elements = NULL,
                            require_sentinel = TRUE) {

  #----------------------------------------------------------------------------
  # Collects a vector-family parameter (columns prefix1..prefixN) as a
  # numeric matrix, gating the feature in one place. Absent columns (a law
  # slice without the state that uses the family) return NULL; where
  # require_sentinel is TRUE, an all-NA first element (the ensure_st_params
  # sentinel for feature-not-present) also returns NULL. Companion matrices
  # of an already-gated anchor should pass require_sentinel = FALSE with the
  # anchor's element range.
  #
  # Parameters:
  #   - tax_unit (df)          : tibble with law columns joined
  #   - prefix (str)           : full column prefix, e.g. 'st_ord.rates'
  #   - elements (int[])       : element indices to collect; NULL discovers
  #                              1:max from the columns present
  #   - require_sentinel (bool): NULL result when the first collected
  #                              element is NA for every row
  #
  # Returns: numeric matrix (rows = tax units), or NULL when the family is
  #          absent/ungated (matrix | NULL).
  #----------------------------------------------------------------------------

  if (is.null(elements)) {
    pattern = paste0('^', str_replace_all(prefix, fixed('.'), '\\.'), '[0-9]+$')
    found = str_subset(colnames(tax_unit), pattern)
    if (length(found) == 0) {
      return(NULL)
    }
    elements = 1:max(as.integer(str_extract(found, '[0-9]+$')))
  }

  cols = paste0(prefix, elements)
  if (!all(cols %in% colnames(tax_unit))) {
    return(NULL)
  }

  m = as.matrix(tax_unit[cols])
  if (require_sentinel && all(is.na(m[, 1]))) {
    return(NULL)
  }
  return(m)
}



st_band_value = function(x, upper, values, lower = NULL) {

  #----------------------------------------------------------------------------
  # Value of the band containing x in a table of upper bounds, with
  # (lower, upper] semantics: the band is selected where lower < x <= upper,
  # and x OUTSIDE the table (at or below the first lower bound, or above the
  # last upper bound) returns ZERO -- published credit tables omit their
  # zero tails. Lower bounds default to the preceding upper bound with a
  # -Inf lead (contiguous bands open at the bottom).
  #
  # Parameters:
  #   - x (dbl[])       : income measure
  #   - upper (matrix)  : per-row band upper bounds
  #   - values (matrix) : per-row band values, same band count as upper
  #   - lower (matrix)  : per-row band lower bounds; NULL = contiguous
  #
  # Returns: per-row band value, zero outside the table (dbl[]).
  #----------------------------------------------------------------------------

  if (is.null(lower)) {
    lower = cbind(-Inf, upper[, -ncol(upper), drop = FALSE])
  }
  rowSums(values * (x > lower & x <= upper), na.rm = TRUE)
}



st_band_index_lower = function(x, lower_bounds) {

  #----------------------------------------------------------------------------
  # Index of the band containing x in a table of LOWER bounds, with
  # closed-lower semantics (band j applies where bounds[j] <= x <
  # bounds[j+1]). x below the first bound clamps INTO band 1; x above the
  # last bound stays in the top band (schedule-style tables cover the whole
  # line). NA bounds (narrower state in a mixed law slice) are ignored.
  #
  # Returns: per-row band index in [1, ncol] (int[]).
  #----------------------------------------------------------------------------

  pmax(1L, pmin(rowSums(lower_bounds <= x, na.rm = TRUE),
                ncol(lower_bounds)))
}



st_band_index_upper = function(x, upper_bounds) {

  #----------------------------------------------------------------------------
  # Index of the band containing x in a table of UPPER bounds, with
  # closed-upper semantics (band j applies where bounds[j-1] < x <=
  # bounds[j]). x above the last bound clamps into the top band (KY family
  # credit table semantics).
  #
  # Returns: per-row band index in [1, ncol] (int[]).
  #----------------------------------------------------------------------------

  pmin(rowSums(upper_bounds < x, na.rm = TRUE) + 1L, ncol(upper_bounds))
}



st_pick_slot = function(m, slot) {

  #----------------------------------------------------------------------------
  # Per-row element pick from a family matrix: row i takes column slot[i].
  # Used for count-keyed families (child-count bins, family size, CO tiers,
  # band indexes from st_band_index_*).
  #
  # Returns: per-row picked value, NA where the slot cell is NA (dbl[]).
  #----------------------------------------------------------------------------

  m[cbind(seq_len(nrow(m)), slot)]
}



st_step_reduction = function(x, thresh, step, per_step, round_up = TRUE) {

  #----------------------------------------------------------------------------
  # Stepped phase-out reduction: per_step for each step (or fraction
  # thereof, when round_up) of x above thresh; zero at or below the
  # threshold. round_up = TRUE rounds a partial step UP to a full step
  # (IL/CT/AZ-style worksheets); FALSE rounds DOWN (NY ESCC style 2). The
  # choice is policy semantics -- take it from the form. Accepts vectors or
  # matrices (per-segment recapture tables) for x/thresh/step/per_step.
  #
  # Returns: reduction amount(s), same shape as the widest input (dbl).
  #----------------------------------------------------------------------------

  steps = pmax(0, x - thresh) / step
  steps = if (round_up) ceiling(steps) else floor(steps)
  steps * per_step
}



st_income_base = function(tax_unit, code) {

  #----------------------------------------------------------------------------
  # Resolves a phase-out/table income base from the uniform enum (2026-07-17
  # review item #8), so every banded feature declares its base in config
  # instead of hard-coding one:
  #   1 = federal AGI
  #   2 = state AGI (only available downstream of calc_st_agi)
  #   3 = federal AGI plus state additions (KY family credit; UT MAGI proxy)
  #   4 = earned income (ei1 + ei2, floored at zero)
  #   5 = federal AGI less taxable Social Security (VA AFAGI)
  #   6 = state AGI less exemptions (OH 2017-18 "income tax base";
  #       downstream of calc_st_exempt)
  #   7 = state AGI plus the business carve-out deduction addback (OH MAGI,
  #       ORC 5747.01(JJ))
  #   8 = modified state AGI (7) less exemptions (OH 2019+ means tests)
  #   9 = state TAXABLE income (only available downstream of calc_st_txbl;
  #       NJ keys its child credit tiers on it)
  # An unavailable base (e.g. state AGI requested inside calc_st_agi) or an
  # unknown code resolves to NA, which downstream comparisons surface.
  #
  # Parameters:
  #   - tax_unit (df) : parsed tax unit tibble
  #   - code (int[])  : per-row base selector (a law column or constant)
  #
  # Returns: per-row income base (dbl[]).
  #----------------------------------------------------------------------------

  n = nrow(tax_unit)
  col_or_na = function(name) {
    if (name %in% colnames(tax_unit)) tax_unit[[name]] else rep(NA_real_, n)
  }
  case_when(
    code == 1 ~ tax_unit$agi,
    code == 2 ~ col_or_na('st_agi'),
    code == 3 ~ tax_unit$agi + col_or_na('st_additions'),
    code == 4 ~ pmax(0, col_or_na('ei1')) + pmax(0, col_or_na('ei2')),
    code == 5 ~ tax_unit$agi - col_or_na('txbl_ss'),
    code == 6 ~ col_or_na('st_agi') - col_or_na('st_exempt'),
    code == 7 ~ col_or_na('st_agi') + col_or_na('st_bid'),
    code == 8 ~ col_or_na('st_agi') + col_or_na('st_bid') - col_or_na('st_exempt'),
    code == 9 ~ col_or_na('st_txbl_inc'),
    TRUE      ~ NA_real_
  )
}



st_n_dep_in = function(tax_unit, lo, hi) {

  #----------------------------------------------------------------------------
  # Counts tracked dependents with ages in [lo, hi] (vectors allowed),
  # from the up-to-three dependent age slots -- consistent with the federal
  # CTC calculator's dependent handling.
  #
  # Returns: per-row dependent count (int[]).
  #----------------------------------------------------------------------------

  (!is.na(tax_unit$dep_age1) &
     tax_unit$dep_age1 >= lo & tax_unit$dep_age1 <= hi) +
  (!is.na(tax_unit$dep_age2) &
     tax_unit$dep_age2 >= lo & tax_unit$dep_age2 <= hi) +
  (!is.na(tax_unit$dep_age3) &
     tax_unit$dep_age3 >= lo & tax_unit$dep_age3 <= hi)
}



lookup_state_credit_table = function(income, key, credit_tables, table_id,
                                     filing_status = NULL) {

  #----------------------------------------------------------------------------
  # Looks up a dense state schedule with inclusive income bands keyed by a
  # generalized concept (child count for CalEITC, family size for the VA
  # poverty guideline and KY Table C, none for CT Table E) and optionally
  # by filing status (rows with filing_status 0 apply to every status).
  # Missing ranges intentionally return zero, which supports published
  # tables that omit their zero-value tails. Callers migrating a
  # whole-dollar published table should round income first (the forms'
  # own instruction) -- see the 2026-07-17 review item #7 notes.
  #
  # Parameters:
  #   - income (dbl[])        : income measure (round if the form does)
  #   - key (int[])           : key-concept value per unit
  #   - credit_tables (df)    : year- and state-filtered schedule rows
  #   - table_id (str)        : credit_id to look up
  #   - filing_status (int[]) : per-unit filing status; NULL when the
  #                             table is not status-keyed
  #
  # Returns: per-unit table value, zero outside the table (dbl[]).
  #----------------------------------------------------------------------------

  value = rep(0, length(income))
  if (is.null(credit_tables) || nrow(credit_tables) == 0) {
    return(value)
  }

  schedule = credit_tables[credit_tables$credit_id == table_id, , drop = FALSE]
  if (nrow(schedule) == 0) {
    return(value)
  }
  if (is.null(filing_status)) {
    filing_status = rep(0L, length(income))
  }

  for (fs_slot in unique(schedule$filing_status)) {
    sched_fs = schedule[schedule$filing_status == fs_slot, , drop = FALSE]
    in_fs = if (fs_slot == 0) rep(TRUE, length(income))
            else filing_status == fs_slot

    # Cap at the top key value; do NOT floor up to the lowest present key.
    # A key below the schedule's minimum bin (e.g. a childless filer when a
    # table omits its zero-child rows) finds no matching band and returns
    # zero, consistent with the omitted-tail semantics above.
    key_fs = pmin(max(sched_fs$key_concept), coalesce(key, 0L))
    for (key_slot in unique(key_fs[in_fs])) {
      rows = which(in_fs & key_fs == key_slot)
      bands = sched_fs[sched_fs$key_concept == key_slot, , drop = FALSE]
      bands = bands[order(bands$income_lower), , drop = FALSE]
      index = findInterval(income[rows], bands$income_lower)
      valid = index > 0
      valid[valid] = income[rows][valid] <= bands$income_upper[index[valid]]
      value[rows[valid]] = bands$value[index[valid]]
    }
  }

  return(value)
}



st_band_interp = function(x, anchors, start_values, end_values) {

  #----------------------------------------------------------------------------
  # Piecewise-linear interpolation across anchor bands (NY CDCTC share
  # table): within [anchors[j], anchors[j+1]) the value moves linearly from
  # start_values[j] to end_values[j]; the top band (infinite width) holds
  # its start value. Closed-lower / open-upper band membership.
  #
  # Parameters:
  #   - x (dbl[])             : income measure
  #   - anchors (matrix)      : per-row band lower bounds
  #   - start_values (matrix) : value at the band's lower bound
  #   - end_values (matrix)   : value at the band's upper bound
  #
  # Returns: per-row interpolated value, zero outside the table (dbl[]).
  #----------------------------------------------------------------------------

  upper = cbind(anchors[, -1, drop = FALSE], Inf)
  width = upper - anchors
  frac  = ifelse(is.finite(width) & width > 0, (x - anchors) / width, 0)
  seg   = (x >= anchors & x < upper)
  rowSums((start_values + (end_values - start_values) *
             pmin(1, pmax(0, frac))) * seg,
          na.rm = TRUE)
}
