#-------------------------------------------------------------------------------
# aggregate_decomp.R
#
# Contains a script to decompose the static-to-conventional revenue wedge for
# selected v5 scenarios into a decumulation piece (conventional minus the
# conventional-no-wealth pass) and a corporate external-income piece
#-------------------------------------------------------------------------------

library(data.table)

vintage   = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v5'
scenarios = c('s_corp_r28', 's_cg_r30', 'pc_corpr35_cgr30')
out_dir   = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/report_prep/decomp'

cols = c('weight', 'liab_iit_net', 'liab_pr', 'liab_wealth', 'estate_m',
         'estate_p_dsue', 'liab_estate_nodsue', 'liab_estate_dsue',
         'corp_dY_exog')

# Sum weighted liabilities for one detail file, in billions
sum_legs = function(path) {
  d = fread(path, select = intersect(cols, names(fread(path, nrows = 0))))
  if (!'corp_dY_exog' %in% names(d)) d[, corp_dY_exog := 0]
  d[, .(
    iit    = sum(weight * liab_iit_net) / 1e9,
    pr     = sum(weight * liab_pr) / 1e9,
    wealth = sum(weight * liab_wealth) / 1e9,
    estate = sum(weight * estate_m * (estate_p_dsue * liab_estate_dsue +
               (1 - estate_p_dsue) * liab_estate_nodsue)) / 1e9,
    dY_exog = sum(weight * corp_dY_exog) / 1e9
  )]
}

results = list()
for (s in scenarios) {
  years = gsub('.csv', '', list.files(file.path(vintage, s, 'conventional/detail')))
  for (y in years) {
    conv   = sum_legs(file.path(vintage, s, 'conventional/detail', paste0(y, '.csv')))
    convnw = sum_legs(file.path(vintage, s, 'conventional_no_wealth/detail', paste0(y, '.csv')))
    results[[paste(s, y)]] = data.table(
      scenario = s, year = as.integer(y),
      conv_iit = conv$iit,   convnw_iit = convnw$iit,
      conv_pr  = conv$pr,    convnw_pr  = convnw$pr,
      conv_wealth = conv$wealth, convnw_wealth = convnw$wealth,
      conv_estate = conv$estate, convnw_estate = convnw$estate,
      dY_exog = conv$dY_exog
    )
    cat(s, y, 'done\n')
  }
}

fwrite(rbindlist(results), file.path(out_dir, 'decomp_legs.csv'))
