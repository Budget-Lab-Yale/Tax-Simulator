#-------------------------------------------------------------------------------
# analyze_networth_shares.R  <vintage>   (RUN VIA SBATCH -- reads big detail)
#
# Weighted top-1% and top-0.1% share of NET WORTH, by scenario and year, from
# the per-record detail. Reforms use the CONVENTIONAL detail (post wealth-bathtub
# haircut, i.e. with the s=1 saving-financing decumulation); baseline uses STATIC
# (no behavior/channel). Top share = sum of weight*net_worth for the top X% of
# the population (ranked by net worth, fractional boundary record) over total
# weight*net_worth (all records, incl. negatives).
#-------------------------------------------------------------------------------
suppressMessages(library(data.table))

VINT  <- commandArgs(trailingOnly = TRUE)[1]
ROOT  <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
OUT   <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
YEARS <- c(2026, 2035, 2045, 2055)                 # yr 1, 10, 20, 30
SCEN  <- list(baseline = "static", warren = "conventional", nickeldime = "conventional")

topshare <- function(nw, w, frac) {
  o <- order(nw, decreasing = TRUE); nw <- nw[o]; w <- w[o]
  totW <- sum(w); totNW <- sum(nw * w)
  cutW <- frac * totW
  cw   <- cumsum(w)
  k    <- which(cw >= cutW)[1]; if (is.na(k)) k <- length(w)
  prevW <- if (k > 1) cw[k - 1] else 0
  full  <- if (k > 1) sum(nw[1:(k - 1)] * w[1:(k - 1)]) else 0
  top   <- full + nw[k] * (cutW - prevW)           # fractional boundary record
  100 * top / totNW
}

rows <- list()
for (sc in names(SCEN)) {
  for (y in YEARS) {
    p <- file.path(ROOT, VINT, sc, SCEN[[sc]], "detail", paste0(y, ".csv"))
    if (!file.exists(p)) {
      alt <- file.path(ROOT, VINT, sc, "conventional", "detail", paste0(y, ".csv"))
      if (file.exists(alt)) p <- alt else { cat("MISSING:", p, "\n"); next }
    }
    dt <- fread(p, select = c("weight", "net_worth"))
    rows[[paste(sc, y)]] <- data.table(
      scenario   = sc, year = y,
      nw_total_T = sum(dt$weight * dt$net_worth) / 1e12,
      top1_pct   = topshare(dt$net_worth, dt$weight, 0.01),
      top0_1_pct = topshare(dt$net_worth, dt$weight, 0.001))
  }
}
res <- rbindlist(rows)
fwrite(res, file.path(OUT, paste0("networth_shares_", VINT, ".csv")))
cat("\n===== TOP NET-WORTH SHARES (%) =====\n"); print(res)
cat("\n-- top 1% share by year --\n");   print(dcast(res, scenario ~ year, value.var = "top1_pct"))
cat("\n-- top 0.1% share by year --\n"); print(dcast(res, scenario ~ year, value.var = "top0_1_pct"))
