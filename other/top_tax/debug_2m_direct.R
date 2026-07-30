#-------------------------------------------------------------------------------
# debug_2m_direct.R
#
# Runs the mechanical pass for one scenario-year against the staging directory
# the grid tranche already built, printing the condition and the call stack that
# the array run reported without a message
#-------------------------------------------------------------------------------

args     = commandArgs(trailingOnly = TRUE)
staging  = args[1]
scenario = args[2]
year     = as.integer(args[3])

source('./src/slurm/common.R')
reconstitute_environment(staging)

config = readRDS(file.path(staging, scenario, 'config.rds'))
config_activate(economy  = config$scenario_info$resolved_economy,
                behavior = config$scenario_info$resolved_behavior)

static_mtrs_year = readRDS(file.path(staging, scenario,
                                     paste0('year_', year, '_static.rds')))$mtrs

cat('scenario:', scenario, ' year:', year, '\n')
cat('mtr_vars:', paste(config$scenario_info$mtr_vars, collapse = ' '), '\n')
cat('tax_law_baseline is null:', is.null(config$tax_law_baseline), '\n\n')

# Force the one-worker path so the failing call appears in this process's stack
# rather than inside a fork
Sys.setenv(TAXSIM_MTR_CORES = '1')

withCallingHandlers(
  tryCatch(
    run_one_year(
      year             = year,
      scenario_info    = config$scenario_info,
      tax_law          = config$tax_law,
      baseline_mtrs    = NULL,
      indexes          = config$indexes,
      vat_price_offset = config$vat_price_offset,
      pass_type        = 'mechanical',
      static_mtrs_year = static_mtrs_year,
      tax_law_baseline = config$tax_law_baseline
    ),
    error = function(e) {
      cat('\n==== CONDITION ====\n')
      print(class(e))
      cat('message: [', conditionMessage(e), ']\n', sep = '')
      cat('call   : ')
      print(conditionCall(e))
      cat('\n==== TRACEBACK ====\n')
      print(sys.calls())
    }
  ),
  warning = function(w) {
    cat('WARNING: ', conditionMessage(w), '\n', sep = '')
    invokeRestart('muffleWarning')
  }
)

cat('\ndone\n')
