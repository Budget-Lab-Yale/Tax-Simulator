#--------------------------------------------------------------------------
# rng.R  (decision S23)
#
# The simulation's precomputed random numbers, keyed by RECORD ID rather
# than by row position and record count.
#
# WHY. `config_parser.R` used to build `random_numbers` as nine
# `runif(length(sample_ids))` columns drawn one after another from the
# global stream, immediately after a `sample_frac()` that consumed RNG in
# proportion to the record count. Two consequences, both measured on
# 2026-09-12 while running the federal validation battery:
#
#   * At a FULL sample `sample_frac(size = 1)` returns every row in random
#     ORDER. Its only consumer is `id %in% sample_ids` (run.R:352), which is
#     order-insensitive, so it selects nothing -- but it moves the stream.
#     Two Tax-Data vintages of different sizes therefore got entirely
#     different draws: for n = 220,896 / 374,630 / 1,399,234 the first five
#     uniforms shared no digits; with the shuffle removed, the prefixes were
#     identical.
#   * Drawing the nine columns in sequence means column k depends on the
#     lengths of columns 1..k-1, so even without the shuffle only the first
#     column would line up between two record sets.
#
# Together these made an A/B between two vintages unreadable: the draws
# moved for every record whether or not the data did.
#
# WHAT THIS GIVES. A record's `r.*` values depend on its id and nothing
# else -- not on how many other records are in the file, not on their order,
# not on the sample fraction. Take-up, eligibility and behavioural draws are
# then the same record-by-record across scenarios, vintages and years, which
# is what the precomputation was for in the first place.
#
# HOW. Each column seeds its own stream and draws the uniform vector for a
# FIXED id space once, then indexes it by id. Fixed is the point: sizing the
# space to the data would reintroduce the dependence. One 80MB temporary per
# column, released immediately.
#
# The global RNG state is saved and restored, so nothing else in the model
# sees a different stream because of these draws.
#
# Tax-Data has a sibling implementation for its imputations
# (`src/imputations/rng.R` there, with its own stream table). They are
# deliberately separate: different draws, different records, different
# owners. Only the indexing technique is shared, and it is short enough to
# state twice rather than couple two repositories.
#--------------------------------------------------------------------------

# Must cover every id Tax-Data emits. Filers sit below 1e6; each non-filer
# pool year occupies its own block of 1e6, so seven years reach ~7.2M.
RNG_ID_SPACE = 10000000L
RNG_BASE_SEED = 76L

# One index per draw. Append-only: reusing or renumbering an index silently
# rewrites every record's value on that stream.
RNG_STREAMS = c(
  bus_loss        = 1L,
  cdctc_takeup    = 2L,
  salt_workaround = 3L,
  oasdi_exp       = 4L,
  new_car         = 5L,
  behavior1       = 6L,
  behavior2       = 7L,
  behavior3       = 8L,
  eitc_precert    = 9L
)
stopifnot(!anyDuplicated(RNG_STREAMS), !anyDuplicated(names(RNG_STREAMS)))


#' Uniform draw per record id, invariant to the record set.
#'
#' @param ids     record ids, 1..RNG_ID_SPACE.
#' @param stream  a name in RNG_STREAMS.
#' @return        numeric vector, one per id.
draw_by_id = function(ids, stream) {
  if (!(stream %in% names(RNG_STREAMS))) {
    stop('draw_by_id(): unknown stream "', stream, '"', call. = FALSE)
  }
  if (anyNA(ids)) {
    stop('draw_by_id(): NA ids on stream "', stream, '"', call. = FALSE)
  }
  if (any(ids < 1) || any(ids > RNG_ID_SPACE)) {
    stop('draw_by_id(): ids outside [1, ', RNG_ID_SPACE, '] on stream "',
         stream, '" (range ', min(ids), '..', max(ids), '). Tax-Data has ',
         'started emitting ids beyond the space these draws are defined ',
         'over; raising it moves every draw.', call. = FALSE)
  }
  if (exists('.Random.seed', envir = globalenv())) {
    saved = get('.Random.seed', envir = globalenv())
    on.exit(assign('.Random.seed', saved, envir = globalenv()), add = TRUE)
  }
  set.seed(RNG_BASE_SEED + RNG_STREAMS[[stream]] * 1000L)
  runif(RNG_ID_SPACE)[ids]
}


#' The precomputed random numbers, one row per id, in the order given.
#'
#' Rows align with `ids`, so the caller binds them to a frame that is in the
#' same order. `r.oasdi_exp` keeps its rounded Exp(1/4) shape, taken by
#' inverse CDF from the record's own uniform.
#'
#' @param ids  record ids, in the order the rows will be bound in.
build_random_numbers = function(ids) {
  tibble(
    r.bus_loss        = draw_by_id(ids, 'bus_loss'),        # excess business loss limitation eligibility
    r.cdctc_takeup    = draw_by_id(ids, 'cdctc_takeup'),    # CDCTC takeup
    r.salt_workaround = draw_by_id(ids, 'salt_workaround'), # SALT workaround participation
    r.oasdi_exp       = round(qexp(draw_by_id(ids, 'oasdi_exp'), rate = 1/4)),
    r.new_car         = draw_by_id(ids, 'new_car'),         # p(new car | car loan interest)
    r.behavior1       = draw_by_id(ids, 'behavior1'),       # spare, for behavioural modules
    r.behavior2       = draw_by_id(ids, 'behavior2'),
    r.behavior3       = draw_by_id(ids, 'behavior3'),
    r.eitc_precert    = draw_by_id(ids, 'eitc_precert')     # EITC pre-certification check
  )
}


#--------------------------------------------------------------------------
# Standalone tests. Run with:  Rscript src/misc/rng.R
#--------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(dplyr))
  cat('--- rng.R tests ---\n')

  # The property this exists for.
  a = draw_by_id(c(5L, 17L, 900001L), 'bus_loss')
  b = draw_by_id(c(1:5000, 900001L, 17L, 5L), 'bus_loss')
  stopifnot(identical(a[1], b[length(b)]), identical(a[2], b[length(b) - 1L]),
            identical(a[3], b[length(b) - 2L]))
  cat('  [PASS] a record draws the same value whatever else is in the file\n')

  stopifnot(draw_by_id(42L, 'bus_loss') != draw_by_id(42L, 'cdctc_takeup'))
  cat('  [PASS] streams are independent\n')

  set.seed(999); before = runif(3)
  set.seed(999); invisible(draw_by_id(1:10, 'new_car')); after = runif(3)
  stopifnot(identical(before, after))
  cat('  [PASS] global RNG state is preserved\n')

  small = build_random_numbers(c(3L, 8L, 11L))
  big   = build_random_numbers(c(1:2000))
  stopifnot(nrow(small) == 3L, ncol(small) == 9L)
  for (cc in names(small)) {
    stopifnot(isTRUE(all.equal(small[[cc]], big[[cc]][c(3L, 8L, 11L)])))
  }
  cat('  [PASS] build_random_numbers: every column is id-keyed\n')

  e = build_random_numbers(1:200000)$r.oasdi_exp
  stopifnot(all(e >= 0), all(e == round(e)), abs(mean(e) - 4) < 0.15)
  cat('  [PASS] r.oasdi_exp keeps its rounded Exp(1/4) shape\n')

  for (bad in list(function() draw_by_id(1L, 'nope'),
                   function() draw_by_id(c(1L, NA), 'bus_loss'),
                   function() draw_by_id(RNG_ID_SPACE + 1, 'bus_loss'))) {
    stopifnot(inherits(try(bad(), silent = TRUE), 'try-error'))
  }
  cat('  [PASS] unknown stream, NA id and out-of-range id all stop\n')

  cat('\nAll tests passed.\n')
}
