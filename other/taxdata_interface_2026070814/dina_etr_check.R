#-------------------------------------------------------------------------------
# dina_etr_check.R
#
# Compute ETRs BY INCOME GROUP directly from DINA 2019, with tax-type
# composition, under several DINA income denominators -- a reference to compare
# against our on-model `other` ETR tier and diagnose the U-shape.
#
# Tax-unit aggregation mirrors Tax-Data/src/imputations/state_local_tax.R:
# split-per-spouse vars SUM; replicated vars (e.g. residential property tax)
# take FIRST. Replication is auto-detected (within-married-id constant).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(haven); library(data.table) })

DINA <- '/nfs/roberts/project/pi_nrs36/shared/raw_data/DINA/v1/2023082913/historical/usdina2019.dta'
d <- as.data.table(read_dta(DINA))
cat(sprintf('DINA 2019: rows=%d cols=%d\n', nrow(d), ncol(d)))

want <- c('id','dweght','dweghttaxu','married',
          'fiinc','fninc','ptinc','peinc',
          'ssinc_oa','ssinc_di','uiinc',
          'tax','ditax','ditaf','ditas','salestax','corptax','estatetax',
          'proprestax','propbustax','govcontrib','ssuicontrib','othercontrib')
present <- intersect(want, names(d)); missing <- setdiff(want, names(d))
cat('present:', paste(present, collapse=' '), '\n')
cat('MISSING:', paste(missing, collapse=' '), '\n')

wcol <- if ('dweghttaxu' %in% names(d)) 'dweghttaxu' else 'dweght'
cat('weight col:', wcol, '\n')

vars <- intersect(c('fiinc','fninc','ptinc','peinc','ssinc_oa','ssinc_di','uiinc',
                    'tax','ditax','ditaf','ditas','salestax','corptax','estatetax',
                    'proprestax','propbustax','govcontrib','ssuicontrib','othercontrib'),
                  names(d))
# coerce (haven_labelled -> numeric)
for (v in c(vars, wcol)) set(d, j = v, value = as.numeric(d[[v]]))

# --- detect replication (residential property etc. are replicated on spouses) --
d[, nad := .N, by = id]
mar <- d[nad > 1]
repl_frac <- sapply(vars, function(v) {
  s <- mar[, .(c = (max(get(v)) - min(get(v))) < 1e-6), by = id]$c
  if (length(s)) mean(s) else NA_real_
})
cat('\n-- within-married-id constant fraction (>0.9 => replicated => FIRST) --\n')
print(round(repl_frac, 3))
repl <- names(repl_frac)[!is.na(repl_frac) & repl_frac > 0.9]
sumv <- setdiff(vars, repl)
cat('replicated (FIRST):', paste(repl, collapse = ' '), '\n')
cat('split (SUM):       ', paste(sumv, collapse = ' '), '\n')

# --- aggregate to tax unit ---
tu_sum   <- d[, c(list(w = first(get(wcol)) / 1e5), lapply(.SD, sum)), by = id, .SDcols = sumv]
tu <- tu_sum
if (length(repl)) {
  tu_first <- d[, lapply(.SD, function(x) x[1]), by = id, .SDcols = repl]
  tu <- merge(tu_sum, tu_first, by = 'id')
}

g <- function(nm) if (nm %in% names(tu)) tu[[nm]] else rep(0, nrow(tu))
comp <- data.table(
  id = tu$id, w = tu$w,
  fed_inc = g('ditaf'), st_inc = g('ditas'), sales = g('salestax'),
  corp = g('corptax'), estate = g('estatetax'),
  res_prop = g('proprestax'), bus_prop = g('propbustax'),
  soc = g('govcontrib')
)
comp[, other_bundle := st_inc + sales + res_prop]    # == our dina_other_taxes numerator
comp[, total := if ('tax' %in% names(tu)) tu$tax else
       fed_inc + st_inc + sales + corp + estate + res_prop + bus_prop + soc]
comp[, `:=`(
  fiinc = g('fiinc'),
  broad = g('fiinc') + g('ssinc_oa') + g('ssinc_di') + g('uiinc'),  # our rate's base
  ptinc = g('ptinc'),
  peinc = g('peinc')
)]

wq <- function(x, w, p) { o <- order(x); cw <- cumsum(w[o]); cw <- cw / cw[length(cw)]
                          approx(cw, x[o], p, rule = 2, ties = mean)$y }

COMPS <- c('total','other_bundle','fed_inc','soc','st_inc','sales','res_prop','corp','estate')
etr_table <- function(dt, denom) {
  D <- dt[[denom]]; ok <- is.finite(D); dt2 <- dt[ok]; D <- D[ok]
  cutQ <- wq(D, dt2$w, c(.2,.4,.6,.8)); cutT <- wq(D, dt2$w, c(.90,.95,.99,.999))
  grp <- cut(D, c(-Inf, cutQ, Inf), labels = c('Q1','Q2','Q3','Q4','Q5'))
  tops <- list('Top10%'=D>=cutT[1],'Top5%'=D>=cutT[2],'Top1%'=D>=cutT[3],'Top0.1%'=D>=cutT[4])
  emit <- function(label, mask) {
    w <- dt2$w[mask]; denomB <- sum(w * D[mask])
    vals <- sapply(COMPS, function(c) 100 * sum(w * dt2[[c]][mask]) / denomB)
    cat(sprintf('%-8s %9.0f  %s\n', label, denomB/1e9, paste(sprintf('%6.1f', vals), collapse=' ')))
  }
  cat(sprintf('\n### DENOMINATOR = %s   (ETR %%; denom in $B)\n', denom))
  cat(sprintf('%-8s %9s  %s\n', 'group', 'denom$B', paste(sprintf('%6s', COMPS), collapse=' ')))
  for (q in c('Q1','Q2','Q3','Q4','Q5')) emit(q, which(grp == q))
  for (nm in names(tops)) emit(nm, which(tops[[nm]]))
  emit('ALL', seq_len(nrow(dt2)))
}

for (den in c('broad','fiinc','ptinc','peinc'))
  if (den %in% names(comp) && any(is.finite(comp[[den]]) & comp[[den]] != 0)) etr_table(comp, den)

cat('\nNOTE: other_bundle = st_inc + sales + res_prop = the exact numerator of our imputed\n')
cat('dina_other_taxes_rate. "broad" = fiinc + SS + UI = the base that rate was divided by.\n')
cat('Done.\n')
