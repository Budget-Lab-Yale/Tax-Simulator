# Compares kg_dynamics mechanical state between the two Tax-Data vintages to
# locate the source of the deemed revenue drop (one-off diagnostic)

root = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'

for (yr in c(2025, 2030)) {
  cat('\n========== year', yr, '==========\n')
  for (v in c('kg_mech_50', 'kg_mech_50_td0609')) {
    s = readRDS(file.path(root, v, 'deemed/static/supplemental/kg_dynamics_mech_state',
                          paste0(yr, '.rds')))
    ct = s$cell_table
    cat('\n--', v, '| regime:', paste(unlist(s$regime), collapse = ' '), '\n')
    cat('cols:', paste(names(ct), collapse = ' '), '\n')
    num = ct[sapply(ct, is.numeric)]
    for (cn in names(num)) {
      cat(sprintf('%-22s sum=%14.4g  mean=%10.4g\n', cn,
                  sum(num[[cn]], na.rm = TRUE), mean(num[[cn]], na.rm = TRUE)))
    }
  }
}
cat('\nDONE\n')
