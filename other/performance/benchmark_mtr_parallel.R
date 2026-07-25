# Paired full-sample timing check for within-year MTR parallelism.
#
# Usage:
#   Rscript other/performance/benchmark_mtr_parallel.R <staging_dir>

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop('Usage: benchmark_mtr_parallel.R <staging_dir>')
}
staging_dir = args[1]

source('./src/slurm/common.R')
reconstitute_environment(staging_dir)

config = readRDS(file.path(staging_dir, 'baseline', 'config.rds'))
scenario_info = config$scenario_info
year = min(scenario_info$years)
benchmark_root = file.path(tempdir(), 'taxsim_mtr_parallel_benchmark')

run_once = function(n_cores, label) {
  Sys.setenv(TAXSIM_MTR_CORES = as.character(n_cores))
  si = scenario_info
  si$output_path = file.path(benchmark_root, label)

  started = proc.time()[['elapsed']]
  result = run_one_year(
    year                 = year,
    scenario_info        = si,
    tax_law              = config$tax_law,
    baseline_mtrs        = NULL,
    indexes              = config$indexes,
    vat_price_offset     = config$vat_price_offset,
    excess_growth_offset = config$excess_growth_offset,
    pass_type            = 'static'
  )
  elapsed = proc.time()[['elapsed']] - started
  detail_path = file.path(si$output_path, 'static', 'detail',
                          paste0(year, '.csv'))

  list(elapsed = elapsed, result = result,
       detail_md5 = unname(tools::md5sum(detail_path)))
}

# Discard one warm-up so filesystem cache and allocator initialization do not
# favor either arm.
invisible(run_once(1, 'warmup'))

timings = matrix(NA_real_, nrow = 2, ncol = 2,
                 dimnames = list(NULL, c('serial', 'parallel')))
checks = logical(2)
for (round in seq_len(2)) {
  serial = run_once(1, paste0('serial_', round))
  parallel = run_once(2, paste0('parallel_', round))
  timings[round, ] = c(serial$elapsed, parallel$elapsed)
  same_result = identical(serial$result, parallel$result)
  same_mtrs = identical(serial$result$mtrs, parallel$result$mtrs)
  same_totals = identical(serial$result$static_totals,
                          parallel$result$static_totals)
  same_detail = identical(serial$detail_md5, parallel$detail_md5)
  checks[round] = same_mtrs && same_totals && same_detail
  cat(sprintf(
    paste0('round %d: serial %.1fs, 2-core %.1fs, speedup %.2fx, ',
           'result=%s, mtrs=%s, totals=%s, detail=%s\n'),
    round, serial$elapsed, parallel$elapsed,
    serial$elapsed / parallel$elapsed, same_result, same_mtrs, same_totals,
    same_detail
  ))
  if (!same_mtrs) {
    cat('MTR comparison:\n')
    print(all.equal(serial$result$mtrs, parallel$result$mtrs))
    for (nm in names(serial$result$mtrs)) {
      x = serial$result$mtrs[[nm]]
      y = parallel$result$mtrs[[nm]]
      if (!identical(x, y)) {
        max_diff = if (is.numeric(x) && is.numeric(y)) {
                     max(abs(x - y), na.rm = TRUE)
                   } else {
                     NA_real_
                   }
        cat(sprintf('  %s: identical=FALSE, max_abs_diff=%s\n',
                    nm, format(max_diff, scientific = TRUE)))
      }
    }
  }
  if (!same_totals) {
    cat('Totals comparison:\n')
    print(all.equal(serial$result$static_totals,
                    parallel$result$static_totals))
  }
}

cat(sprintf(
  'median: serial %.1fs, 2-core %.1fs, wall-time saving %.1f%%\n',
  median(timings[, 'serial']), median(timings[, 'parallel']),
  100 * (1 - median(timings[, 'parallel']) / median(timings[, 'serial']))
))
if (!all(checks)) stop('Serial and parallel results differed')
