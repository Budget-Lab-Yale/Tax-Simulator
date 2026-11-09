#!/usr/bin/env Rscript
# Compare top-tail population counts: raw SCF (donor source) vs Tax-Data output
# (old + billionaire vintages). Population-weighted, to isolate the wealth
# imputation from mortality. Net worth = sum(value.*) - sum(debt cols).
suppressPackageStartupMessages({library(data.table)})

SCF_RDS = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/resources/cache/scf_tax_units.rds'
OUT_OLD = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026050315/baseline/tax_units_2022.csv'
OUT_NEW = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline/tax_units_2022.csv'

VALUE = c('value.cash','value.equities','value.bonds','value.dc','value.db',
          'value.life_ins','value.annuities','value.trusts','value.other_fin',
          'value.pass_throughs','value.primary_home','value.other_home',
          'value.re_fund','value.other_nonfin')
DEBT  = c('value.credit_cards','value.credit_lines','value.installment_debt',
          'value.other_debt','value.primary_mortgage','value.other_mortgage')

bands = c(50e6, 100e6, 500e6, 1e9, 1e13)
blab  = c('50M-100M','100M-500M','500M-1B','1B+')

tab = function(nw, w, name) {
  cut_b = cut(nw, breaks = c(bands[1], bands[-1]), labels = blab, right = FALSE)
  cat(sprintf('\n=== %s ===\n', name))
  cat(sprintf('%-12s %14s %16s\n','band','pop(sum wt)','wealth($B)'))
  for (i in seq_along(blab)) {
    sel = !is.na(cut_b) & cut_b == blab[i]
    cat(sprintf('%-12s %14.1f %16.1f\n', blab[i], sum(w[sel]), sum(nw[sel]*w[sel])/1e9))
  }
}

# --- SCF --- (cache uses bare asset/debt names, not value.*)
SCF_VALUE = c('cash','equities','bonds','dc','db','life_ins','annuities','trusts',
              'other_fin','pass_throughs','primary_home','other_home','re_fund',
              'other_nonfin')
SCF_DEBT  = c('credit_cards','credit_lines','installment_debt','other_debt',
              'primary_mortgage','other_mortgage')
scf = readRDS(SCF_RDS)
setDT(scf)
vcols = intersect(SCF_VALUE, names(scf)); dcols = intersect(SCF_DEBT, names(scf))
cat(sprintf('SCF: matched %d value cols, %d debt cols\n', length(vcols), length(dcols)))
scf_nw = rowSums(as.matrix(scf[, ..vcols]), na.rm=TRUE) -
         (if(length(dcols)) rowSums(as.matrix(scf[, ..dcols]), na.rm=TRUE) else 0)
# SCF cache weight already divided by 5 implicates in stage1; collapse not needed.
tab(scf_nw, as.numeric(scf$weight), 'RAW SCF (cache)')

# --- output vintages ---
for (pair in list(c('OLD vintage 2026050315', OUT_OLD), c('NEW vintage 2026052823 (billionaires)', OUT_NEW))) {
  d = fread(pair[2], select = c('weight', VALUE, DEBT), showProgress = FALSE)
  nw = rowSums(as.matrix(d[, ..VALUE]), na.rm=TRUE) - rowSums(as.matrix(d[, ..DEBT]), na.rm=TRUE)
  tab(nw, as.numeric(d$weight), pair[1])
}
