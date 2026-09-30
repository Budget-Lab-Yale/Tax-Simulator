#---------------------------------------------------------------
# The State-Weights reader (build_state_weights(method =
# 'interface')) and the parse-time pin check
# (validate_state_weights_pins()), on a temp-directory fixture:
# a three-record, three-state vintage with its dependencies.csv.
#
# Cases: a well-formed year reads and filters; a record with no
# row stops; weights that do not sum to the national weight stop;
# pct_sample rescales; a missing year file stops; an unknown
# state stops; the pin check accepts the matching Tax-Data
# vintage and refuses another; the placeholder still works.
#
# Defines functions only (sourced by main.R's recursive walk).
# Run manually after sourcing ./src:  test_state_weights_reader()
#---------------------------------------------------------------


test_state_weights_reader = function() {

  #----------------------------------------------------------------------------
  # Runs the reader and pin-check tests.
  #
  # Returns: TRUE invisibly if all tests pass (throws otherwise).
  #----------------------------------------------------------------------------

  expect_stop = function(expr, pattern, what) {
    msg = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(msg))          stop('FAIL ', what, ': did not stop')
    if (!grepl(pattern, msg))  stop('FAIL ', what, ': unexpected message: ', msg)
    invisible(TRUE)
  }

  # --- fixture: <vintage>/baseline/state_weights_2022.csv.gz + dependencies.csv
  vint = file.path(tempdir(), 'sw_fixture'); root = file.path(vint, 'baseline')
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  units = tibble(id = c(1, 2, 3), weight = c(10, 4, 0.5))
  sw = tibble(id    = c(1, 1, 1,   2, 2,   3, 3),
              state = c('CT', 'NY', 'OA', 'CT', 'NY', 'NY', 'OA'),
              weight = c(5, 3, 2,   1, 3,   0.25, 0.25))
  data.table::fwrite(sw, file.path(root, 'state_weights_2022.csv.gz'), compress = 'gzip')
  write_csv(tibble(ID = 'state_weights', interface = c('Tax-Data', 'Tax-Simulator'), version = 1,
                   vintage = c('TD_A', 'base_A'), scenario = 'baseline'),
            file.path(vint, 'dependencies.csv'))

  # --- reads, checks, filters
  out = build_state_weights(units, 2022, method = 'interface', states = c('CT', 'NY'), root = root)
  stopifnot(nrow(out) == 5, all(out$state %in% c('CT', 'NY')),
            abs(sum(out$weight[out$id == 1]) - 8) < 1e-12)     # OA excluded by the filter, not lost

  # --- pct_sample: file weights are full-sample; tax_units are already scaled
  half = units %>% mutate(weight = weight / 0.5)
  out = build_state_weights(half, 2022, method = 'interface', states = 'CT', root = root, pct_sample = 0.5)
  stopifnot(abs(out$weight[out$id == 1] - 10) < 1e-12)

  # --- failures name their cause
  expect_stop(build_state_weights(bind_rows(units, tibble(id = 9, weight = 1)), 2022,
                                  method = 'interface', states = 'CT', root = root),
              'have no row', 'a record without a row stops')
  expect_stop(build_state_weights(units %>% mutate(weight = weight * 1.01), 2022,
                                  method = 'interface', states = 'CT', root = root),
              'do not sum to the national weight', 'a sum mismatch stops')
  expect_stop(build_state_weights(units, 2023, method = 'interface', states = 'CT', root = root),
              'no state weights for 2023', 'a missing year stops')
  expect_stop(build_state_weights(units, 2022, method = 'interface', states = 'ZZ', root = root),
              'unknown jurisdiction', 'an unknown state stops')
  expect_stop(build_state_weights(units, 2022, method = 'interface', states = 'CT', root = NULL),
              'no State-Weights interface path', 'a missing root stops')

  # --- the placeholder is unchanged
  ph = build_state_weights(units, 2022, method = 'placeholder', states = c('CT', 'NY'))
  stopifnot(nrow(ph) == 6, abs(ph$weight[1] - 10 / 53) < 1e-12)

  # --- pin check
  paths = tibble(ID = 'baseline', interface = 'State-Weights', path = root)
  deps_ok  = tibble(ID = 'baseline', interface = c('Tax-Data', 'State-Weights'), version = 1,
                    vintage = c('TD_A', 'sw_fixture'), scenario = 'baseline')
  deps_bad = deps_ok %>% mutate(vintage = if_else(interface == 'Tax-Data', 'TD_B', vintage))
  stopifnot(isTRUE(validate_state_weights_pins(deps_ok, paths)))
  expect_stop(validate_state_weights_pins(deps_bad, paths), 'was fit on Tax-Data TD_A',
              'a State-Weights vintage fit on another Tax-Data vintage is refused')
  stopifnot(isTRUE(validate_state_weights_pins(deps_ok, paths %>% filter(interface != 'State-Weights'))))

  unlink(vint, recursive = TRUE)
  message('test_state_weights_reader: all passed')
  invisible(TRUE)
}
