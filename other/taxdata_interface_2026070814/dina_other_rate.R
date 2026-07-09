#-------------------------------------------------------------------------------
# dina_other_rate.R
#
# ONE rate: the DINA analog of our `other` ETR tier =
#   (ditas + salestax + proprestax) / income
# = state income tax + sales/excise + residential property tax, as a share of
# income, by income group, with the sales / property / state-income split.
#
# Aggregation to tax unit: residential property tax is replicated on both
# spouses -> FIRST; everything else (incl. SS/UI and the income pieces) is
# split/equal-split per adult -> SUM (this fixes the earlier SS/UI halving).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(haven); library(data.table) })

DINA <- '/nfs/roberts/project/pi_nrs36/shared/raw_data/DINA/v1/2023082913/historical/usdina2019.dta'
d <- as.data.table(read_dta(DINA))

keep <- c('id','dweghttaxu','fiinc','ptinc','peinc','ssinc_oa','ssinc_di','uiinc',
          'ditas','salestax','proprestax')
for (v in keep) set(d, j = v, value = as.numeric(d[[v]]))

# aggregate to tax unit: proprestax = FIRST (replicated full on each spouse),
# all else = SUM
sumv <- c('fiinc','ptinc','peinc','ssinc_oa','ssinc_di','uiinc','ditas','salestax')
tu <- d[, c(list(w = first(dweghttaxu)/1e5,
                 proprestax = proprestax[1]),
            lapply(.SD, sum)), by = id, .SDcols = sumv]

tu[, `:=`(
  st_inc   = ditas,
  sales    = salestax,
  res_prop = proprestax,
  other    = ditas + salestax + proprestax,               # == our `other` numerator
  broad    = fiinc + ssinc_oa + ssinc_di + uiinc,          # our rate's native base
  peinc    = peinc
)]

wq <- function(x, w, p) { o <- order(x); cw <- cumsum(w[o]); cw <- cw/cw[length(cw)]
                          approx(cw, x[o], p, rule = 2, ties = mean)$y }

show <- function(denom_name) {
  D <- tu[[denom_name]]; ok <- is.finite(D) & D != 0
  x <- tu[ok]; D <- D[ok]
  cutQ <- wq(D, x$w, c(.2,.4,.6,.8)); cutT <- wq(D, x$w, c(.90,.99,.999))
  grp  <- cut(D, c(-Inf, cutQ, Inf), labels = c('Q1','Q2','Q3','Q4','Q5'))
  rows <- list(Q1=which(grp=='Q1'), Q2=which(grp=='Q2'), Q3=which(grp=='Q3'),
               Q4=which(grp=='Q4'), Q5=which(grp=='Q5'),
               `Top10%`=which(D>=cutT[1]), `Top1%`=which(D>=cutT[2]),
               `Top0.1%`=which(D>=cutT[3]), ALL=seq_len(nrow(x)))
  cat(sprintf('\n### DINA "other" rate = (state income + sales/excise + res property) / %s\n', denom_name))
  cat(sprintf('%-8s %9s | %8s | %7s %8s %8s\n',
              'group','income$B','OTHER %','sales%','resprop%','stinc%'))
  for (nm in names(rows)) {
    m <- rows[[nm]]; w <- x$w[m]; den <- sum(w*D[m])
    o  <- 100*sum(w*x$other[m])/den
    s  <- 100*sum(w*x$sales[m])/den
    rp <- 100*sum(w*x$res_prop[m])/den
    si <- 100*sum(w*x$st_inc[m])/den
    cat(sprintf('%-8s %9.0f | %7.1f%% | %6.1f%% %7.1f%% %7.1f%%\n', nm, den/1e9, o, s, rp, si))
  }
}

show('broad')   # fiinc + SS + UI  (closest to our `expanded`)
show('peinc')   # pre-tax national income (PSZ standard)
cat('\nOur chart, for reference (expanded income): Q1 24% -> Q5 11% -> Top1% 9.6%.\n')
cat('Done.\n')
