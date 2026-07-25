#-------------------------------------------------------------------------------
# cohort_bathtub.R
#
# Generic helpers for cohort-level "bathtub" recurrences over (age x within-age
# percentile) cells. These are deliberately model-agnostic: the wealth-dynamics
# channel (src/sim/wealth_dynamics.R) consumes them now, and the kg_dynamics
# bathtub (src/sim/kg/) can be pointed at them later. Nothing here
# knows about wealth, gains, taxes, or estates -- only ages, percentiles,
# weights, transition matrices, and per-year state IO.
#
# The pattern (mirrors kg_dynamics): cell-level state, a deterministic age-shift
# composed with a stochastic within-age percentile transition, a forward
# recurrence, and per-year .rds state. See run_wealth_bathtub_pass() for the
# concrete recurrence that wires these together.
#-------------------------------------------------------------------------------



build_aging_matrix = function(ages) {

  #----------------------------------------------------------------------------
  # Deterministic age-shift operator on a contiguous integer age grid.
  # A[a, h] = 1 if h = a + 1; the top age self-loops (A[a_max, a_max] = 1) so
  # the topcode pool accumulates. Formerly duplicated as kg's private
  # kg_dyn_build_aging_matrix() (now removed); this is the shared primitive
  # both the wealth and kg channels call.
  #
  # The shift is applied as crossprod(A, X) (i.e. t(A) %*% X): row a of the
  # result gathers from row a-1 of X, with the top age also gathering itself.
  #
  # Parameters:
  #   - ages (int[]) : contiguous, increasing age grid (e.g. 18:80)
  #
  # Returns: an (n x n) 0/1 matrix with dimnames = ages.
  #----------------------------------------------------------------------------

  stopifnot(length(ages) >= 1, all(diff(ages) == 1))
  n = length(ages)
  A = matrix(0, n, n, dimnames = list(ages, ages))
  for (i in seq_len(n - 1)) A[i, i + 1] = 1
  A[n, n] = 1
  A
}



weighted_quantile_cuts = function(values, weights, probs) {

  #----------------------------------------------------------------------------
  # Weighted quantile cut points. Returns the values at which the cumulative
  # weight fraction (over the supplied, already-subsetted population) first
  # reaches each probability in `probs`. Used to draw equal-weighted-headcount
  # percentile boundaries within an age cohort.
  #
  # Convention: cut[k] = smallest value v such that
  #   sum(weights[values <= v]) / sum(weights) >= probs[k].
  # A record is then placed in bin findInterval(value, cuts) + 1, so a record
  # exactly at a boundary lands in the lower bin (left-closed intervals),
  # matching findInterval's default (rightmost.closed = FALSE).
  #
  # Parameters:
  #   - values  (dbl[]) : ranking values (e.g. net worth), positive population only
  #   - weights (dbl[]) : record weights, same length as values
  #   - probs   (dbl[]) : increasing probabilities in (0, 1)
  #
  # Returns: numeric vector of length(probs) cut points (monotone nondecreasing).
  #          Returns rep(Inf, length(probs)) when the population is empty.
  #----------------------------------------------------------------------------

  if (length(values) == 0 || sum(weights) <= 0) {
    return(rep(Inf, length(probs)))
  }
  ord = order(values)
  v   = values[ord]
  w   = weights[ord]
  cum = cumsum(w) / sum(w)
  # For each prob, first value whose cumulative fraction reaches it.
  idx = findInterval(probs, cum, left.open = TRUE) + 1L
  idx = pmin(idx, length(v))
  v[idx]
}



compute_within_age_cutoffs = function(value, weight, age_cohort,
                                      ages, n_bins, positive_only = TRUE) {

  #----------------------------------------------------------------------------
  # Per-age within-cohort percentile cut points (the n_bins - 1 interior
  # boundaries that split each age cohort into n_bins equal-weighted-headcount
  # bins, ranked by `value`).
  #
  # When positive_only = TRUE (the wealth convention, plan D17), records with
  # value <= 0 are EXCLUDED from the ranking: they fall in no cell. NOTE this
  # deliberately diverges from distribution.R's by-wealth view, which keeps
  # value == 0 (it uses value >= 0 / value < 0). A zero-net-worth record has no
  # stock to draw down and leaves no taxable estate, so excluding it is the
  # correct treatment for the saving-financing channel and keeps conservation
  # exact over the positive-NW population.
  #
  # Storing cut points (rather than bin assignments) lets the pre-pass and the
  # applier assign IDENTICAL percentiles from the same boundaries -- the records
  # are the same un-eroded cross-section in both, so the cells line up.
  #
  # Parameters:
  #   - value         (dbl[]) : ranking value per record (e.g. net worth)
  #   - weight        (dbl[]) : record weight
  #   - age_cohort    (int[]) : topcoded age cohort per record (already keyed)
  #   - ages          (int[]) : the age grid
  #   - n_bins        (int)   : number of equal-headcount percentile bins
  #   - positive_only (bool)  : drop value <= 0 from the ranking
  #
  # Returns: a named list keyed by as.character(age); each element a numeric
  #          vector of length (n_bins - 1) of interior cut points (or
  #          rep(Inf, n_bins - 1) for an empty/degenerate cohort).
  #----------------------------------------------------------------------------

  probs = (seq_len(n_bins - 1)) / n_bins
  out   = vector('list', length(ages))
  names(out) = as.character(ages)

  keep_pop = if (positive_only) (value > 0) else rep(TRUE, length(value))

  for (a in ages) {
    sel = which(age_cohort == a & keep_pop & is.finite(value) & weight > 0)
    out[[as.character(a)]] = weighted_quantile_cuts(value[sel], weight[sel], probs)
  }
  out
}



assign_within_age_bin = function(value, age_cohort, cutoffs,
                                 n_bins, positive_only = TRUE) {

  #----------------------------------------------------------------------------
  # Assign each record to its within-age percentile bin [1, n_bins] using the
  # cut points from compute_within_age_cutoffs(). Records with value <= 0 (under
  # positive_only) or an unknown age cohort get NA (no cell).
  #
  # The pre-pass and the applier both call this with the SAME cutoffs, so a
  # record lands in the same cell in both -- a hard requirement for the haircut
  # to drain the exact deficit that record's cell accrued.
  #
  # Parameters:
  #   - value         (dbl[]) : ranking value per record
  #   - age_cohort    (int[]) : topcoded age cohort per record
  #   - cutoffs       (list)  : output of compute_within_age_cutoffs()
  #   - n_bins        (int)   : number of bins
  #   - positive_only (bool)  : value <= 0 -> NA (no cell)
  #
  # Returns: integer vector of bin indices in [1, n_bins], NA where no cell.
  #----------------------------------------------------------------------------

  bin = rep(NA_integer_, length(value))
  in_pop = if (positive_only) (value > 0) else rep(TRUE, length(value))
  in_pop = in_pop & is.finite(value)

  ages_chr = names(cutoffs)
  for (a_chr in ages_chr) {
    a   = as.integer(a_chr)
    sel = which(age_cohort == a & in_pop)
    if (length(sel) == 0) next
    cuts     = cutoffs[[a_chr]]
    bin[sel] = pmin(findInterval(value[sel], cuts) + 1L, n_bins)
  }
  bin
}



sinkhorn_rake = function(M, max_iter = 1000, tol = 1e-10) {

  #----------------------------------------------------------------------------
  # Rake a nonnegative square matrix to doubly-stochastic (rows AND columns sum
  # to 1) via Sinkhorn iteration. The recurrence operates in percentile-index
  # space where each percentile holds an equal weighted headcount, so the
  # uniform headcount marginal must be preserved across the re-gridding -- a
  # row-stochastic transition alone conserves dollar mass, but doubly-stochastic
  # also conserves the equal-headcount marginal. A properly-defined within-age
  # percentile transition matrix already (approximately) satisfies this; the
  # rake makes it exact and is a no-op (to tolerance) on the identity and on the
  # uniform 1/n matrix.
  #
  # Parameters:
  #   - M        (matrix) : nonnegative n x n matrix
  #   - max_iter (int)    : iteration cap
  #   - tol      (dbl)    : convergence tolerance on row/col sum deviation
  #
  # Returns: doubly-stochastic n x n matrix (same dimnames).
  #----------------------------------------------------------------------------

  stopifnot(nrow(M) == ncol(M), all(M >= 0))
  n  = nrow(M)
  W  = M
  # Guard fully-empty rows/cols (degenerate input): seed with uniform.
  if (any(rowSums(W) == 0) || any(colSums(W) == 0)) {
    W = W + 1e-12
  }
  for (i in seq_len(max_iter)) {
    W = W / rowSums(W)
    W = sweep(W, 2, colSums(W), '/')
    if (max(abs(rowSums(W) - 1)) < tol && max(abs(colSums(W) - 1)) < tol) break
  }
  # Final row normalization so rows sum to exactly 1 (row-stochastic is the
  # property the recurrence's mass conservation relies on).
  W = W / rowSums(W)
  dimnames(W) = dimnames(M)
  W
}



apply_percentile_transition = function(P, M_by_age) {

  #----------------------------------------------------------------------------
  # Apply a within-age percentile transition to a cell-state matrix, in
  # percentile-index space. For each age row a, the next-period percentile
  # distribution of that cohort's mass is P[a, ] %*% M_a (M_a[q, p] = P(p next |
  # q now)). This IS the re-binning onto the fresh percentile grid; no separate
  # re-binning step is needed (plan D12).
  #
  # Parameters:
  #   - P        (matrix) : (n_ages x n_bins) cell-state, rows = ages
  #   - M_by_age (list)   : named (by age) list of n_bins x n_bins row-stochastic
  #                         matrices, OR a single n_bins x n_bins matrix applied
  #                         to every age. Identity = full persistence.
  #
  # Returns: (n_ages x n_bins) matrix, the post-transition state (pre age-shift).
  #----------------------------------------------------------------------------

  n_ages = nrow(P)
  if (is.matrix(M_by_age)) {
    return(P %*% M_by_age)
  }
  out = P
  ages_chr = rownames(P)
  for (i in seq_len(n_ages)) {
    Ma = M_by_age[[ages_chr[i]]]
    if (is.null(Ma)) next            # missing age -> identity (no churn)
    out[i, ] = as.numeric(P[i, , drop = FALSE] %*% Ma)
  }
  out
}



cohort_recurrence_step = function(P_prev, growth, inflow, A, M_by_age) {

  #----------------------------------------------------------------------------
  # One step of a cohort bathtub recurrence over (age x percentile) cells:
  #
  #   P_next = growth (.) [ t(A) %*% (P_prev applied through M) ]  +  inflow
  #
  # i.e. the carried-in state is first re-gridded across percentiles (M), then
  # aged one year (crossprod(A, .) = t(A) %*% . : row a gathers from a-1, with
  # the top age self-looping), then grown element-wise by `growth`; finally the
  # fresh `inflow` enters at face value (end-of-year convention -- it does NOT
  # grow in its arrival year). NO survival/mortality factor (deaths are handled
  # at aggregation; plan D1). All of growth, inflow, P_prev are (n_ages x n_bins)
  # matrices conformable with A (n_ages x n_ages).
  #
  # Pure and side-effect-free, so the recurrence can be unit-tested on toy cells.
  #----------------------------------------------------------------------------

  PM      = apply_percentile_transition(P_prev, M_by_age)
  carried = crossprod(A, PM)
  growth * carried + inflow
}



cohort_state_dir = function(scenario_info, subdir, pass = 'conventional') {

  #----------------------------------------------------------------------------
  # Directory for a cohort bathtub's per-year state, under a pass root. Mirrors
  # kg_dyn_state_dir() but parameterized by subdir/pass so different channels
  # (and passes) keep separate state trees.
  #
  # Parameters:
  #   - scenario_info (list) : output of get_scenario_info()
  #   - subdir        (str)  : channel-specific subdir (e.g. 'wealth_dynamics_state')
  #   - pass          (str)  : 'conventional' (default) or 'static'
  #
  # Returns: directory path (str).
  #----------------------------------------------------------------------------

  file.path(scenario_info$output_path, pass, 'supplemental', subdir)
}

cohort_state_path = function(scenario_info, subdir, year, pass = 'conventional') {
  file.path(cohort_state_dir(scenario_info, subdir, pass), paste0(year, '.rds'))
}

write_cohort_state = function(state, scenario_info, subdir, year,
                              pass = 'conventional') {
  dir.create(cohort_state_dir(scenario_info, subdir, pass),
             recursive = TRUE, showWarnings = FALSE)
  saveRDS(state, cohort_state_path(scenario_info, subdir, year, pass))
  invisible(NULL)
}

read_cohort_state = function(scenario_info, subdir, year, pass = 'conventional') {
  path = cohort_state_path(scenario_info, subdir, year, pass)
  if (!file.exists(path)) {
    stop('cohort bathtub state missing: ', path,
         ' (was the pre-pass run for this year?)')
  }
  readRDS(path)
}
