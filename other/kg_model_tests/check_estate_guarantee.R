# Guarantee-path equivalence: est2009_guarantee (no 'estate' in mtr_vars,
# run.R fallback) must produce mtr_estate / mtr_estate_ded and a bathtub
# e_S bit-equal to est2009_pure_kg (registered path, same tax law).
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
V='/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
g = fread(file.path(V,'estate_offset_ab3_v1/est2009_guarantee/static/detail/2027.csv'),
          select=c('id','mtr_estate','mtr_estate_ded'), showProgress=FALSE)
r = fread(file.path(V,'estate_offset_ab2_v1/est2009_pure_kg/static/detail/2027.csv'),
          select=c('id','mtr_estate','mtr_estate_ded'), showProgress=FALSE)
j = merge(g, r, by='id', suffixes=c('_g','_r'))
stopifnot(nrow(j) == nrow(g))
d1 = max(abs(j$mtr_estate_g - j$mtr_estate_r))
d2 = max(abs(j$mtr_estate_ded_g - j$mtr_estate_ded_r))
cat(sprintf('max |mtr_estate diff| = %.3e ; max |mtr_estate_ded diff| = %.3e\n', d1, d2))
sg = readRDS(file.path(V,'estate_offset_ab3_v1/est2009_guarantee/conventional/supplemental/kg_dynamics_state/2027.rds'))$cell_table
sr = readRDS(file.path(V,'estate_offset_ab2_v1/est2009_pure_kg/conventional/supplemental/kg_dynamics_state/2027.rds'))$cell_table
d3 = max(abs(sg$estate_e_S - sr$estate_e_S)); d4 = max(abs(sg$r_D_S - sr$r_D_S))
cat(sprintf('max |e_S diff| = %.3e ; max |r_D_S diff| = %.3e\n', d3, d4))
if (max(d1,d2,d3,d4) > 1e-12) stop('GUARANTEE PATH DIVERGES FROM REGISTERED PATH')
cat('GUARANTEE PATH == REGISTERED PATH (bitwise within 1e-12)\n')
