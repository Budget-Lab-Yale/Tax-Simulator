#---------------------------------------------------------------------------
# st_utils.R
#
# Shared primitives for the state calculators (2026-07-17 review item #3):
# one way to collect a vector-family parameter matrix, and explicit-boundary
# band/step lookups. Boundary semantics (> vs >=, ceiling vs floor) are
# POLICY choices -- each helper makes them explicit so a new state picks a
# semantic instead of re-coding an idiom (the historical off-by-one class).
#---------------------------------------------------------------------------


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
