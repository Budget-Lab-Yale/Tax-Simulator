#-------------------------------------------------------------------------------
# diag_share_questions.R   (SBATCH)  -- investigate two questions:
#  Q2 (immediate impact): decompose the YEAR-1 (2026) top-share drop into the
#      AVOIDANCE behavior (reported-base reduction) vs the FINANCING haircut
#      (real stock drawdown), using warren_bound_identity {baseline, warren_s00
#      (avoidance only, s=0), warren_s100 (avoidance + identity haircut, s=1)}.
#  Q1 (baseline drift): why does baseline top-share fall 2026->2055? Decompose
#      the no-tax baseline (warren_nd_30yr/baseline/static) into top-1% vs rest
#      mean-wealth growth, head-count (weight), and mean age.
#-------------------------------------------------------------------------------
suppressMessages(library(data.table))
ROOT <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"

wshare <- function(nw, w, frac) {
  o <- order(nw, decreasing = TRUE); nw <- nw[o]; w <- w[o]
  cutW <- frac * sum(w); cw <- cumsum(w); k <- which(cw >= cutW)[1]
  prevW <- if (k > 1) cw[k - 1] else 0
  full  <- if (k > 1) sum(nw[1:(k - 1)] * w[1:(k - 1)]) else 0
  100 * (full + nw[k] * (cutW - prevW)) / sum(nw * w)
}
rd <- function(v, sc, k, y, cols = c("weight","net_worth"))
  fread(file.path(ROOT, v, sc, k, "detail", paste0(y, ".csv")), select = cols)

cat("================ Q2: YEAR-1 (2026) DECOMPOSITION ================\n")
cat(sprintf("%-26s %12s %9s %9s\n","scenario (channel)","nw_total_T","top1%","top0.1%"))
for (r in list(c("baseline","static","baseline: no tax"),
               c("warren_s00","conventional","warren s=0: AVOIDANCE only"),
               c("warren_s100","conventional","warren s=1: avoidance + haircut(identity)"))) {
  d <- tryCatch(rd("warren_bound_identity", r[1], r[2], 2026), error = function(e) NULL)
  if (is.null(d)) { cat("  MISSING:", r[1], r[2], "\n"); next }
  cat(sprintf("%-26s %12.2f %9.2f %9.2f\n", r[3],
              sum(d$weight*d$net_worth)/1e12,
              wshare(d$net_worth,d$weight,.01), wshare(d$net_worth,d$weight,.001)))
}

cat("\n================ Q1: BASELINE DRIFT 2026 vs 2055 ================\n")
cat("(warren_nd_30yr/baseline/static -- the no-tax trajectory)\n")
cat(sprintf("%6s %10s %10s %11s %12s %12s %9s\n",
            "year","totNW_T","totW_M","top1_share","top1_meanNW_M","rest_meanNW_K","top1_age"))
prev <- NULL
for (y in c(2026, 2055)) {
  d <- tryCatch(rd("warren_nd_30yr","baseline","static", y, c("weight","net_worth","age1")),
                error = function(e) NULL)
  if (is.null(d)) { cat("  MISSING baseline", y, "\n"); next }
  o <- order(d$net_worth, decreasing = TRUE)
  nw <- d$net_worth[o]; w <- d$weight[o]; ag <- d$age1[o]
  totW <- sum(w); totNW <- sum(nw*w); cw <- cumsum(w); k1 <- which(cw >= .01*totW)[1]
  t1nw <- sum(nw[1:k1]*w[1:k1]); t1w <- sum(w[1:k1])
  cat(sprintf("%6d %10.1f %10.2f %11.2f %12.2f %12.1f %9.1f\n",
              y, totNW/1e12, totW/1e6, 100*t1nw/totNW,
              t1nw/t1w/1e6, (totNW-t1nw)/(totW-t1w)/1e3, weighted.mean(ag[1:k1], w[1:k1])))
  cur <- c(totNW=totNW, top1nw=t1nw, restnw=totNW-t1nw, top1w=t1w, restw=totW-t1w)
  if (!is.null(prev)) {
    g <- function(a,b) sprintf("%+.1f%%", 100*(cur[a]/prev[a]-1))
    cat(sprintf("  growth 2026->2055:  total NW %s | top1 NW %s | rest NW %s | top1 count %s | rest count %s\n",
                g("totNW","totNW"), g("top1nw","top1nw"), g("restnw","restnw"),
                g("top1w","top1w"), g("restw","restw")))
  }
  prev <- cur
}
