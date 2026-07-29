#-------------------------------------------------------------------------------
# cohort_bathtub.R
#
# Contains general functions for stepping cohort state forward over cells of age
# by percentile
#-------------------------------------------------------------------------------

# Nothing here knows about wealth, gains, taxes or estates: only ages,
# percentiles, weights, transition matrices, and reading and writing state by
# year. The wealth model uses these now, and the gains model could be pointed at
# them later.
#
# The pattern is the same in both: state held per cell, aged one year at a time
# and moved across percentiles, with a fresh inflow each year, and one state file
# per year. run_wealth_bathtub_pass() is the concrete case.
#-------------------------------------------------------------------------------



build_aging_matrix = function(ages) {

  #----------------------------------------------------------------------------
  # Builds the operator that moves each age to the next. The top-coded age points
  # at itself, so that pool accumulates. Shared by the wealth and gains models.
  #
  # Applied as crossprod(A, X), so that each row gathers from the row below it and
  # the top age also gathers itself.
  #
  # Parameters:
  #   - ages (int[]) : contiguous, increasing age grid
  #
  # Returns: square matrix of zeros and ones, labelled by age.
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
  # Finds weighted quantiles: the value at which the cumulative share of weight
  # first reaches each of the given probabilities. Used to split an age cohort into
  # percentiles holding equal weighted headcount.
  #
  # A record exactly on a boundary falls in the lower bin.
  #
  # Parameters:
  #   - values  (dbl[]) : the ranking value per record
  #   - weights (dbl[]) : record weights
  #   - probs   (dbl[]) : increasing probabilities between 0 and 1
  #
  # Returns: numeric vector of cut points, or infinities where the population is
  #          empty.
  #----------------------------------------------------------------------------

  if (length(values) == 0 || sum(weights) <= 0) {
    return(rep(Inf, length(probs)))
  }
  ord = order(values)
  v   = values[ord]
  w   = weights[ord]
  cum = cumsum(w) / sum(w)
  # For each probability, the first value whose cumulative share reaches it
  idx = findInterval(probs, cum, left.open = TRUE) + 1L
  idx = pmin(idx, length(v))
  v[idx]
}



compute_within_age_cutoffs = function(value, weight, age_cohort,
                                      ages, n_bins, positive_only = TRUE) {

  #----------------------------------------------------------------------------
  # Finds the boundaries that split each age cohort into percentiles of equal
  # weighted headcount.
  #
  # Where positive_only is set, records at or below zero are dropped from the
  # ranking and fall in no cell. Note that this differs from distribution.R, which
  # keeps records at exactly zero. A record with no net worth has nothing to draw
  # down and leaves no taxable estate, so dropping it is right for the saving
  # channel and keeps the accounting exact over the records that do have wealth.
  #
  # Boundaries are stored rather than bin assignments, so that the pre-pass and the
  # applier can assign the same percentiles from the same boundaries. Both see the
  # same records before any erosion, so the cells line up.
  #
  # Parameters:
  #   - value         (dbl[]) : the ranking value per record
  #   - weight        (dbl[]) : record weight
  #   - age_cohort    (int[]) : cohort age per record
  #   - ages          (int[]) : the age grid
  #   - n_bins        (int)   : number of percentiles
  #   - positive_only (bool)  : drop records at or below zero
  #
  # Returns: named list by age of the interior cut points, or infinities for an
  #          empty cohort.
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
  # Assigns each record to its percentile within its age, using the boundaries from
  # compute_within_age_cutoffs(). A record at or below zero, or of unknown age, gets
  # no cell.
  #
  # The pre-pass and the applier both call this with the same boundaries, so a
  # record lands in the same cell in both. That is required for the applier to drain
  # exactly what that record's cell accumulated.
  #
  # Parameters:
  #   - value         (dbl[]) : the ranking value per record
  #   - age_cohort    (int[]) : cohort age per record
  #   - cutoffs       (list)  : the boundaries
  #   - n_bins        (int)   : number of percentiles
  #   - positive_only (bool)  : records at or below zero get no cell
  #
  # Returns: integer vector of percentiles, NA where there is no cell.
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
  # Scales a nonnegative square matrix until both its rows and its columns sum to
  # one, by alternating between the two.
  #
  # The recurrence works in percentile space, where each percentile holds an equal
  # weighted headcount, so the transition has to preserve that. Rows summing to one
  # conserves dollars; columns summing to one also conserves the equal headcount. A
  # properly built transition matrix nearly satisfies this already, and this makes it
  # exact. It does nothing to the identity or to a uniform matrix.
  #
  # Parameters:
  #   - M        (matrix) : nonnegative square matrix
  #   - max_iter (int)    : iteration cap
  #   - tol      (dbl)    : tolerance on the row and column sums
  #
  # Returns: the scaled matrix.
  #----------------------------------------------------------------------------

  stopifnot(nrow(M) == ncol(M), all(M >= 0))
  n  = nrow(M)
  W  = M
  # An empty row or column cannot be scaled, so seed it as uniform.
  if (any(rowSums(W) == 0) || any(colSums(W) == 0)) {
    W = W + 1e-12
  }
  for (i in seq_len(max_iter)) {
    W = W / rowSums(W)
    W = sweep(W, 2, colSums(W), '/')
    if (max(abs(rowSums(W) - 1)) < tol && max(abs(colSums(W) - 1)) < tol) break
  }
  # Normalize rows last, since it is rows summing to one that conserves dollars.
  W = W / rowSums(W)
  dimnames(W) = dimnames(M)
  W
}



apply_percentile_transition = function(P, M_by_age) {

  #----------------------------------------------------------------------------
  # Moves each age cohort's state across percentiles. This is also the re-binning
  # onto the new percentile grid, so no separate step is needed.
  #
  # Parameters:
  #   - P        (matrix) : cell state, ages on rows
  #   - M_by_age (list)   : the transition, either one matrix applied at every age
  #                         or a list of them by age. The identity means everyone
  #                         stays where they are.
  #
  # Returns: the state after the transition, before aging.
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
  # Steps cell state forward one year:
  #
  #   next = growth * [aged and moved state] + inflow
  #
  # The state carried in moves across percentiles, then ages a year, then grows.
  # This year's inflow enters at face value and does not grow in its first year.
  # There is no mortality term: deaths are handled at aggregation instead.
  #
  # Side-effect free, so the recurrence can be tested on made-up cells.
  #
  # Returns: the new cell state.
  #----------------------------------------------------------------------------

  PM      = apply_percentile_transition(P_prev, M_by_age)
  carried = crossprod(A, PM)
  growth * carried + inflow
}



cohort_state_dir = function(scenario_info, subdir, pass = 'conventional') {

  #----------------------------------------------------------------------------
  # Locates a channel's state folder for one pass. Taking the folder and the pass as
  # arguments keeps the channels' state separate.
  #
  # Parameters:
  #   - scenario_info (list) : the scenario
  #   - subdir        (str)  : the channel's folder name
  #   - pass          (str)  : 'static', 'mechanical' or 'conventional'
  #
  # Returns: path to the folder (str).
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
