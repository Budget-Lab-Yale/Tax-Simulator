#-------------------------------------------------------------------------------
# summarize_explore.R — headline interaction deltas + sanity checks.
# The bathtub channel is isolated as conv(s=0.5) - conv(s=0) (only s flips).
#-------------------------------------------------------------------------------
suppressMessages(library(data.table))
OUT <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
tot <- fread(file.path(OUT, "explore_totals.csv"))
nw  <- fread(file.path(OUT, "explore_networth.csv"))

reforms <- list(
  cg5 = "cg5", warren = "warren", nd = "nd",
  estate2009 = "estate2009", warren_estate2009 = "warren_estate2009")

getv <- function(d, sc, ps, yr, col) {
  v <- d[scenario == sc & pass == ps & year == yr][[col]]
  if (length(v) == 0) NA_real_ else v[1]
}

yrs_show <- c(2026, 2030, 2035)
cat("================ BATHTUB INTERACTIONS: conv(s=0.5) - conv(s=0) ================\n")
cat("(estate & wealth & income tax in $B, CY liability; nw in $T)\n\n")

rows <- list()
for (nm in names(reforms)) {
  s0 <- paste0(reforms[[nm]], "_s0"); s50 <- paste0(reforms[[nm]], "_s50")
  cat(sprintf("---- %s ----\n", nm))
  for (yr in yrs_show) {
    est0  <- getv(tot, s0, "conventional", yr, "est_tax")
    est50 <- getv(tot, s50,"conventional", yr, "est_tax")
    wlt0  <- getv(tot, s0, "conventional", yr, "wealth_tax")
    wlt50 <- getv(tot, s50,"conventional", yr, "wealth_tax")
    iit0  <- getv(tot, s0, "conventional", yr, "liab_iit_net")
    iit50 <- getv(tot, s50,"conventional", yr, "liab_iit_net")
    nw0   <- getv(nw,  s0, "conventional", yr, "nw_total")
    nw50  <- getv(nw,  s50,"conventional", yr, "nw_total")
    hcmean<- getv(nw,  s50,"conventional", yr, "hc_mean")
    hcmax <- getv(nw,  s50,"conventional", yr, "hc_max")
    nclmp <- getv(nw,  s50,"conventional", yr, "n_clamped")
    dalloc<- getv(nw,  s50,"conventional", yr, "D_alloc_total")
    cat(sprintf("  %d: dEstate=%+.3f (%+.2f%%)  dWealth=%+.3f (%+.2f%%)  dIIT=%+.3f  dNW=%+.4fT  hc(mean=%.4f max=%.3f clamp=%g)\n",
                yr, est50-est0, 100*(est50-est0)/est0,
                ifelse(is.na(wlt0)||wlt0==0,NA,wlt50-wlt0),
                ifelse(is.na(wlt0)||wlt0==0,NA,100*(wlt50-wlt0)/wlt0),
                iit50-iit0, nw50-nw0, hcmean, hcmax, nclmp))
    rows[[paste(nm,yr)]] <- data.frame(reform=nm, year=yr,
      est_s0=est0, est_s50=est50, d_est=est50-est0,
      wlt_s0=wlt0, wlt_s50=wlt50, d_wlt=wlt50-wlt0,
      iit_s0=iit0, iit_s50=iit50, d_iit=iit50-iit0,
      nw_s0=nw0, nw_s50=nw50, d_nw=nw50-nw0,
      D_alloc_B=dalloc, hc_mean=hcmean, hc_max=hcmax, n_clamped=nclmp)
  }
  cat("\n")
}
fwrite(rbindlist(rows), file.path(OUT, "explore_interactions.csv"))

cat("================ SANITY CHECKS ================\n")
# (1) Dormancy: at s=0, conv estate == static estate (channel off)
cat("\n[1] Dormancy s=0: conv estate == static estate (max |diff| over years)\n")
for (nm in names(reforms)) {
  s0 <- paste0(reforms[[nm]], "_s0")
  d <- merge(tot[scenario==s0 & pass=="conventional", .(year, c=est_tax)],
             tot[scenario==s0 & pass=="static",       .(year, s=est_tax)], by="year")
  cat(sprintf("    %-20s max|conv-static estate| = %.3e\n", nm, max(abs(d$c-d$s), na.rm=TRUE)))
}
# (2) Estate-only control: bathtub inert (d_est ~ 0, d_nw ~ 0 across years)
cat("\n[2] Estate-only control (estate2009): channel should be INERT\n")
e0<-tot[scenario=="estate2009_s0",.(year,p=pass,e=est_tax)]
e50<-tot[scenario=="estate2009_s50" & pass=="conventional",.(year,e50=est_tax)]
m<-merge(tot[scenario=="estate2009_s0"&pass=="conventional",.(year,e0=est_tax)],e50,by="year")
cat(sprintf("    max |conv estate(s50)-conv estate(s0)| = %.3e  (expect ~0)\n", max(abs(m$e50-m$e0))))
nwm<-merge(nw[scenario=="estate2009_s0"&pass=="conventional",.(year,n0=nw_total)],
           nw[scenario=="estate2009_s50"&pass=="conventional",.(year,n50=nw_total)],by="year")
cat(sprintf("    max |conv NW(s50)-conv NW(s0)|          = %.3e T (expect ~0)\n", max(abs(nwm$n50-nwm$n0))))
# (3) Ordering of erosion magnitude at final year
cat("\n[3] Erosion ordering at 2035 (|dNW|, should be nd > warren > cg ~ combined):\n")
for (nm in c("nd","warren","warren_estate2009","cg5","estate2009")) {
  v <- getv(nw,paste0(nm,"_s50"),"conventional",2035,"nw_total") -
       getv(nw,paste0(nm,"_s0"), "conventional",2035,"nw_total")
  cat(sprintf("    %-20s dNW(2035) = %+.4f T\n", nm, v))
}
cat("\nDONE\n")
