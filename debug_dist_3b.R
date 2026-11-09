# Reproduces the Phase 3b distribution failure for kg_mech_50_frac/deemed
# with a full traceback (one-off diagnostic)

staging_dir = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/kg_mech_50_frac/_slurm_staging'
source('./src/slurm/common.R')
runtime_args = reconstitute_environment(staging_dir)

withCallingHandlers(
  tryCatch(
    build_distribution_tables('deemed', baseline_id = 'baseline'),
    error = function(e) {
      cat('CAUGHT ERROR\nclass:', paste(class(e), collapse = ', '),
          '\nmessage: [', conditionMessage(e), ']\n')
      print(e$call)
    }
  ),
  warning = function(w) {
    cat('WARNING:', conditionMessage(w), '\n')
    invokeRestart('muffleWarning')
  }
)
traceback()
cat('DONE\n')
