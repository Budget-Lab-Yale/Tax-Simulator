# NJ cross-model triage (2026-10-01): what makes our NJ gross income exceed
# federal AGI on the records that miss? Joins harness records to the full
# federal pre-pass and lines the gap up against candidate mechanisms.
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
decomp = function(model, yr) {
  rc = c('id','state','fed_aligned','excluded','abs_diff','diff','agi','st_agi','st_subtractions','st_additions','st_retirement_excl','st_txbl_inc','filing_status')
  if (model == 'taxsim') rc = c(rc, 'v32_state_agi', 'v36_state_taxable_income')
  r = fread(sprintf('research/state_tax/cross_model/results/raw/%s_%d.csv', model, yr), select = rc)
  r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE]
  fc = c('id','gross_inc','txbl_kg','kg_st','kg_lt','sole_prop','part_scorp','part_scorp_loss','net_rent','rent_loss','net_estate','farm',
         'trad_contr_ira','keogh_contr','se_health','hsa_contr','sl_int_ded','alimony_exp','other_above_ded','txbl_ss','ui','exempt_int',
         'txbl_pens_dist','txbl_ira_dist','liab_seca','excess_bus_loss','wages')
  f = readRDS(sprintf('research/state_tax/cross_model/cache/fed_calc_%d.rds', yr))$tax_units
  f = as.data.table(f)[, intersect(fc, names(f)), with = FALSE]
  d = merge(r, f, by = 'id')
  d[, `:=`(gap = st_agi - agi, miss = abs_diff > 100,
           cand_adj   = trad_contr_ira + keogh_contr + se_health + hsa_contr + sl_int_ded + other_above_ded + liab_seca / 2,
           cand_alim  = alimony_exp,
           cand_kg    = pmax(0, -txbl_kg),
           cand_bus   = pmax(0, -(sole_prop + part_scorp + farm)),
           cand_rent  = pmax(0, -(net_rent + net_estate)),
           cand_excl  = -(txbl_ss + ui))]
  d[, explained := cand_adj + cand_alim + cand_kg + cand_bus + cand_rent + cand_excl - st_retirement_excl]
  cat(sprintf('\n==== NJ %s %d: n %d, match %.3f; ours higher on %.2f of misses\n', model, yr, nrow(d), mean(!d$miss), mean(d$diff[d$miss] > 0)))
  m = d[miss == TRUE]
  for (v in c('gap','cand_adj','cand_alim','cand_kg','cand_bus','cand_rent','cand_excl','st_retirement_excl','explained'))
    cat(sprintf('  %-20s misses: nonzero %.2f median %10.0f p90 %10.0f | matches: nonzero %.2f median %8.0f\n', v,
                mean(abs(m[[v]]) > 1), median(m[[v]]), quantile(m[[v]], .9), mean(abs(d[miss == FALSE][[v]]) > 1), median(d[miss == FALSE][[v]])))
  m[, resid := gap - explained]
  cat(sprintf('  gap minus candidates on misses: |resid| <= $100 on %.2f; median resid %.0f\n', mean(abs(m$resid) <= 100), median(m$resid)))
  if (model == 'taxsim') { m[, d_v32 := st_agi - v32_state_agi]; cat(sprintf('  vs TAXSIM state AGI: |ours - v32| <= 100 on %.2f of misses, median %.0f; TAXSIM v32 - agi median %.0f\n', mean(abs(m$d_v32) <= 100), median(m$d_v32), median(m$v32_state_agi - m$agi))) }
  # which candidate is the gap on high-income misses?
  h = m[agi >= 150000]
  cat(sprintf('  AGI>=150k misses: %d; share with cand_kg > 0 %.2f, cand_bus > 0 %.2f, cand_adj > 1000 %.2f, cand_rent > 0 %.2f\n', nrow(h), mean(h$cand_kg > 0), mean(h$cand_bus > 0), mean(h$cand_adj > 1000), mean(h$cand_rent > 0)))
  invisible(d)
}
decomp('taxsim', 2019); decomp('policyengine', 2023)
