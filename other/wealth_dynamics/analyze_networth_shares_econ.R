#-------------------------------------------------------------------------------
# analyze_networth_shares_econ.R  <vintage>   (SBATCH)
#
# ECONOMIC net-worth shares (value.* - debts), isolating the REAL wealth-stock
# effect of the financing channel and EXCLUDING the avoidance reporting response.
#
# The avoidance module overwrites the net_worth COLUMN with the reported base but
# leaves value.* untouched; value.* are not in the detail. However each year the
# conventional pass starts from Tax-Data's fresh projected balance sheet and the
# haircut removes exactly D_alloc dollars (D_alloc = f * economic_gross) BEFORE
# avoidance overwrites net_worth. So real post-haircut economic net worth =
#   economic(id, y) = static.net_worth(id, y) - conventional.D_alloc(id, y)
# exact for unclamped records (the entire top tail). Baseline: D_alloc = 0.
# Drain is capped at the record's positive net worth (clamped low-NW records;
# immaterial to the top shares).
#-------------------------------------------------------------------------------
suppressMessages(library(data.table))
VINT  <- commandArgs(trailingOnly = TRUE)[1]
ROOT  <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
OUT   <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
YEARS <- c(2026, 2035, 2045, 2055)
SCEN  <- c("baseline", "warren", "nickeldime")

wshare <- function(nw, w, frac) {
  o <- order(nw, decreasing = TRUE); nw <- nw[o]; w <- w[o]
  cutW <- frac * sum(w); cw <- cumsum(w); k <- which(cw >= cutW)[1]
  prevW <- if (k > 1) cw[k - 1] else 0
  full  <- if (k > 1) sum(nw[1:(k - 1)] * w[1:(k - 1)]) else 0
  100 * (full + nw[k] * (cutW - prevW)) / sum(nw * w)
}

rows <- list()
for (sc in SCEN) {
  for (y in YEARS) {
    sp <- file.path(ROOT, VINT, sc, "static", "detail", paste0(y, ".csv"))
    if (!file.exists(sp)) { cat("MISSING static:", sp, "\n"); next }
    st <- fread(sp, select = c("id", "weight", "net_worth"))
    if (sc == "baseline") {
      st[, econ := net_worth]
    } else {
      cp <- file.path(ROOT, VINT, sc, "conventional", "detail", paste0(y, ".csv"))
      cv <- fread(cp, select = c("id", "D_alloc"))
      st <- merge(st, cv, by = "id", all.x = TRUE)
      st[is.na(D_alloc), D_alloc := 0]
      st[, drain := pmin(D_alloc, pmax(net_worth, 0))]      # cap at positive NW
      st[, econ := net_worth - drain]
    }
    rows[[paste(sc, y)]] <- data.table(
      scenario = sc, year = y,
      econ_total_T = sum(st$weight * st$econ) / 1e12,
      top1_pct   = wshare(st$econ, st$weight, 0.01),
      top0_1_pct = wshare(st$econ, st$weight, 0.001))
  }
}
res <- rbindlist(rows)
fwrite(res, file.path(OUT, paste0("networth_shares_econ_", VINT, ".csv")))
cat("\n===== ECONOMIC TOP NET-WORTH SHARES (%) =====\n"); print(res)
cat("\n-- top 1% (economic) --\n");   print(dcast(res, scenario ~ year, value.var = "top1_pct"))
cat("\n-- top 0.1% (economic) --\n"); print(dcast(res, scenario ~ year, value.var = "top0_1_pct"))
