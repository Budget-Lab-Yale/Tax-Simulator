#-------------------------------------------------------------------------------
# Diagnostic: bottom tail of the 2026 net worth distribution.
# Where are the negatives? How big is the exact-zero mass? Are debts captured?
#-------------------------------------------------------------------------------
suppressPackageStartupMessages(library(data.table))

INPUT_CSV <- '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline/tax_units_2026.csv'
ASSET_COLS <- c('value.cash','value.equities','value.bonds','value.dc','value.db',
                'value.life_ins','value.annuities','value.trusts','value.other_fin',
                'value.pass_throughs','value.primary_home','value.other_home',
                'value.re_fund','value.other_nonfin')
DEBT_COLS  <- c('value.primary_mortgage','value.other_mortgage','value.credit_lines',
                'value.credit_cards','value.installment_debt','value.other_debt')

dt <- fread(INPUT_CSV, select = c('id','weight','age1', ASSET_COLS, DEBT_COLS))
cat(sprintf('Rows read: %d\n', nrow(dt)))
cat(sprintf('weight  range: [%.2f, %.2f]   n(weight<=0)=%d\n', min(dt$weight), max(dt$weight), sum(dt$weight<=0)))
cat(sprintf('age1    range: [%.2f, %.2f]\n', min(dt$age1), max(dt$age1)))
cat(sprintf('id      range: [%.0f, %.0f]\n', min(dt$id), max(dt$id)))

dt[, assets := rowSums(.SD), .SDcols = ASSET_COLS]
dt[, debts  := rowSums(.SD), .SDcols = DEBT_COLS]
dt[, networth := assets - debts]

W <- sum(dt$weight)
shr <- function(mask) sprintf('%.2f%% (%.1fM units, %d records)',
                              100*sum(dt$weight[mask])/W, sum(dt$weight[mask])/1e6, sum(mask))
cat('\n--- net worth sign (weighted) ---\n')
cat('  < 0 :', shr(dt$networth < 0), '\n')
cat('  = 0 :', shr(dt$networth == 0), '\n')
cat('  > 0 :', shr(dt$networth > 0), '\n')

cat('\n--- any positive debt? ---\n')
cat('  debts > 0 :', shr(dt$debts > 0), '\n')
cat(sprintf('  total debt outstanding (wtd): $%.3f T\n', sum(dt$debts*dt$weight)/1e12))
cat(sprintf('  total assets (wtd):           $%.3f T\n', sum(dt$assets*dt$weight)/1e12))

# weighted quantile (same estimator as the main script)
wq <- function(x, w, probs){ o<-order(x); x<-x[o]; w<-w[o]; cw<-cumsum(w); p<-(cw-0.5*w)/sum(w); approx(p,x,probs,rule=2,ties='ordered')$y }
probs <- c(0.005,0.01,0.02,0.03,0.05,0.10,0.15,0.20,0.25,0.50)
cat('\n--- low percentiles, ALL ages (weighted) ---\n')
qs <- wq(dt$networth, dt$weight, probs)
print(data.table(pctile=paste0('p',probs*100), networth=round(qs)))

cat(sprintf('\nmin net worth: $%s   (most negative record)\n', formatC(min(dt$networth), format='d', big.mark=',')))

# by age bin: share negative and p1/p3/p5
brks <- c(-Inf, seq(25,80,5), Inf)
labs <- c('<25','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+')
dt[, age_bin := cut(age1, brks, labs, right=FALSE)]
tab <- dt[, .(pct_neg = round(100*sum(weight[networth<0])/sum(weight),1),
              pct_zero= round(100*sum(weight[networth==0])/sum(weight),1),
              p1 = round(wq(networth,weight,0.01)),
              p3 = round(wq(networth,weight,0.03)),
              p5 = round(wq(networth,weight,0.05))), by=age_bin][order(age_bin)]
cat('\n--- by age bin: % negative, % exactly zero, and p1/p3/p5 ---\n')
print(tab)
