#-------------------------------------------------------------------------------
# chartdata_explore.R — emit compact, chart-ready series for the HTML report.
#-------------------------------------------------------------------------------
suppressMessages({ library(data.table) })
have_json <- requireNamespace("jsonlite", quietly = TRUE)
OUT <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
tot <- fread(file.path(OUT, "explore_totals.csv"))
nw  <- fread(file.path(OUT, "explore_networth.csv"))

reforms <- c("cg5","warren","nd","estate2009","warren_estate2009")
report_years <- 2026:2035   # drop the 2036 FY lead-out from the charts

ser <- function(d, sc, ps, col) {
  x <- d[scenario==sc & pass==ps]
  setNames(x[[col]], x$year)[as.character(report_years)]
}

out <- list(years = report_years)
for (r in reforms) {
  s0 <- paste0(r,"_s0"); s50 <- paste0(r,"_s50")
  est0  <- ser(tot,s0,"conventional","est_tax");    est50 <- ser(tot,s50,"conventional","est_tax")
  wlt0  <- ser(tot,s0,"conventional","wealth_tax");  wlt50 <- ser(tot,s50,"conventional","wealth_tax")
  iit0  <- ser(tot,s0,"conventional","liab_iit_net");iit50 <- ser(tot,s50,"conventional","liab_iit_net")
  nw0   <- ser(nw, s0,"conventional","nw_total");    nw50  <- ser(nw, s50,"conventional","nw_total")
  out[[r]] <- list(
    est_s0  = unname(est0),  est_s50 = unname(est50),  d_est = unname(est50-est0),
    wlt_s0  = unname(wlt0),  wlt_s50 = unname(wlt50),  d_wlt = unname(wlt50-wlt0),
    iit_s0  = unname(iit0),  iit_s50 = unname(iit50),  d_iit = unname(iit50-iit0),
    nw_s0   = unname(nw0),   nw_s50  = unname(nw50),   d_nw  = unname(nw50-nw0),
    D_alloc = unname(ser(nw,s50,"conventional","D_alloc_total")),
    hc_max  = unname(ser(nw,s50,"conventional","hc_max")),
    n_clamp = unname(ser(nw,s50,"conventional","n_clamped"))
  )
}
if (have_json) {
  jsonlite::write_json(out, file.path(OUT,"explore_chartdata.json"),
                       auto_unbox=TRUE, digits=8, na="null")
  cat("wrote explore_chartdata.json\n")
}
# Also dump to stdout so it can be read from the log.
cat("=====CHARTDATA_BEGIN=====\n")
cat(jsonlite::toJSON(out, auto_unbox=TRUE, digits=8, na="null"))
cat("\n=====CHARTDATA_END=====\n")
