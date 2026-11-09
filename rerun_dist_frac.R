# Regenerates deemed distribution tables for the two-leg vintages. The
# deemed-only relaunch subset the runscript, so globals$interface_paths lacks
# the ID='baseline' rows get_other_taxes() needs; clone them from the deemed
# rows (same default interface vintages) before running.

root = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'

for (v in c('kg_mech_50_frac', 'kg_td0609_frac')) {
  cat('=== vintage', v, '\n')
  source('./src/slurm/common.R')
  runtime_args = reconstitute_environment(file.path(root, v, '_slurm_staging'))

  if (!('baseline' %in% globals$interface_paths$ID)) {
    g = globals
    g$interface_paths = bind_rows(
      g$interface_paths,
      g$interface_paths %>% filter(ID == 'deemed') %>% mutate(ID = 'baseline')
    )
    globals <<- g
  }

  build_distribution_tables('deemed', baseline_id = 'baseline')
  cat('distribution rebuilt for deemed in', v, '\n')
}
cat('DONE\n')
