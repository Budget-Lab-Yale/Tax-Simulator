#-------------------------------------------------------------------------------
# analyze_s_impact.R
#
# Cross-base interaction experiment: what does turning the wealth saving-
# financing channel ON (s = 0.5 vs s = 0) do to the 10-year revenue estimate of
# (a) a capital-gains +5pp reform and (b) a simple top-ordinary-rate -> 39.6
# reform, and how does that change split across kinds of tax (income / payroll /
# estate / wealth)?
#
# Logic:
#   * Both reforms use the kg_dynamics/turnover behavior module.
#   * The reform's conventional revenue ESTIMATE = conv receipts(reform) -
#     baseline receipts. Baseline is shared, so the IMPACT OF INCLUDING s>0 is
#     simply  conv(reform, s=0.5) - conv(reform, s=0)  -- baseline cancels.
#   * Receipts are FY-booked (estate lands in FY death-year+1), pulled from
#     totals/receipts.csv on the CONVENTIONAL pass (the channel is conventional-
#     only; static is identical across s by construction).
#
# Inputs (two vintages on local scratch):
#   CG +5pp leg   -> wealth_explore : cg5_s0, cg5_s50  (already run)
#   Top-rate leg  -> wealth_toprate : toprate_s0, toprate_s50
#   baseline      -> either (copied into wealth_toprate); used only for the
#                    reform-vs-baseline headline, not for the s>0 impact.
#
# Outputs (other/wealth_dynamics/):
#   s_impact_receipts.csv   long: leg x scenario x s x year, all revenue lines
#   s_impact_summary.csv    leg x tax_type: 10y reform estimate at s0 & s50,
#                           and the s>0 impact (s50 - s0)
#   s_impact_drain.csv      leg x s x year: weighted net_worth stock + cumulative
#                           drain (D_alloc) -- the mechanical size of the channel
#   s_impact_data.json      everything, for the HTML artifact
#-------------------------------------------------------------------------------

suppressMessages(library(data.table))
have_json <- requireNamespace("jsonlite", quietly = TRUE)

LOCAL <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
OUT   <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"

# (leg, scenario, s, vintage)
legs <- rbindlist(list(
  data.table(leg="cg5",     scenario="cg5_s0",      s=0.0, vintage="wealth_explore"),
  data.table(leg="cg5",     scenario="cg5_s50",     s=0.5, vintage="wealth_explore"),
  data.table(leg="toprate", scenario="toprate_s0",  s=0.0, vintage="wealth_toprate"),
  data.table(leg="toprate", scenario="toprate_s50", s=0.5, vintage="wealth_toprate")
))

rev_cols <- c("revenues_income_tax","revenues_payroll_tax","revenues_estate_tax",
              "revenues_wealth_tax","revenues_corp_tax","revenues_vat","revenues_other")
# refundable-credit outlays (subtracted to get net budget effect)
outlay_col <- "outlays_tax_credits"

read_csv_safe <- function(p) if (file.exists(p)) as.data.frame(fread(p)) else NULL

# Receipts are FY-booked with a filing-season t+1 lag, so policy effective CY2026
# first lands in FY2027; the 10 fiscal years actually present are FY2027-2036.
WIN <- 2027:2036   # 10-year budget window in receipts (FY) space

#-------------------------------------------------------------------------------
# 1. Conventional receipts levels per (leg, scenario, s, year)
#-------------------------------------------------------------------------------
rcpt_rows <- list()
for (i in seq_len(nrow(legs))) {
  r <- legs[i]
  p <- file.path(LOCAL, r$vintage, r$scenario, "conventional", "totals", "receipts.csv")
  d <- read_csv_safe(p)
  if (is.null(d)) { cat("MISSING:", p, "\n"); next }
  have <- intersect(rev_cols, names(d))
  df <- data.frame(leg=r$leg, scenario=r$scenario, s=r$s, year=d$year)
  for (cc in rev_cols) df[[cc]] <- if (cc %in% have) d[[cc]] else 0
  df[[outlay_col]] <- if (outlay_col %in% names(d)) d[[outlay_col]] else 0
  # net budget effect = all revenue lines minus refundable-credit outlays
  df$revenues_total <- rowSums(df[rev_cols], na.rm=TRUE) - df[[outlay_col]]
  rcpt_rows[[r$scenario]] <- df
}
rcpt <- rbindlist(rcpt_rows, fill=TRUE)
fwrite(rcpt, file.path(OUT, "s_impact_receipts.csv"))
cat("Wrote s_impact_receipts.csv:", nrow(rcpt), "rows\n")

# baseline receipts (for the reform-vs-baseline headline)
base_p <- file.path(LOCAL, "wealth_toprate", "baseline", "conventional", "totals", "receipts.csv")
if (!file.exists(base_p))
  base_p <- file.path(LOCAL, "wealth_explore", "baseline", "conventional", "totals", "receipts.csv")
if (!file.exists(base_p))   # baseline has no behavior -> static==conventional; fall back to static
  base_p <- file.path(LOCAL, "wealth_explore", "baseline", "static", "totals", "receipts.csv")
bd <- read_csv_safe(base_p)
base <- data.frame(year = bd$year)
for (cc in rev_cols) base[[cc]] <- if (cc %in% names(bd)) bd[[cc]] else 0
base[[outlay_col]] <- if (outlay_col %in% names(bd)) bd[[outlay_col]] else 0
base$revenues_total <- rowSums(base[rev_cols], na.rm=TRUE) - base[[outlay_col]]

#-------------------------------------------------------------------------------
# 2. Summary: per leg x tax_type, 10y reform estimate at s0 & s50, and s>0 impact
#-------------------------------------------------------------------------------
tax_types <- c(income="revenues_income_tax", payroll="revenues_payroll_tax",
               estate="revenues_estate_tax", wealth="revenues_wealth_tax",
               refundable_outlays="outlays_tax_credits", total="revenues_total")

win_sum <- function(df, col) sum(df[[col]][df$year %in% WIN], na.rm=TRUE)

summ <- list()
for (lg in unique(rcpt$leg)) {
  d0  <- as.data.frame(rcpt[leg==lg & s==0.0])
  d50 <- as.data.frame(rcpt[leg==lg & s==0.5])
  for (tt in names(tax_types)) {
    col <- tax_types[[tt]]
    est_s0  <- win_sum(d0,  col) - win_sum(base, col)   # reform - baseline, s=0
    est_s50 <- win_sum(d50, col) - win_sum(base, col)   # reform - baseline, s=0.5
    impact  <- win_sum(d50, col) - win_sum(d0, col)     # s>0 impact (baseline cancels)
    summ[[paste(lg,tt)]] <- data.frame(
      leg=lg, tax_type=tt,
      est_s0_10y=est_s0, est_s50_10y=est_s50, s_impact_10y=impact
    )
  }
}
summary_tbl <- rbindlist(summ)
fwrite(summary_tbl, file.path(OUT, "s_impact_summary.csv"))
cat("Wrote s_impact_summary.csv:", nrow(summary_tbl), "rows\n")
print(summary_tbl)

#-------------------------------------------------------------------------------
# 3. Mechanical drain diagnostics from detail (net_worth stock + D_alloc)
#-------------------------------------------------------------------------------
sel <- c("weight","net_worth","D_alloc")
drain_rows <- list()
for (i in seq_len(nrow(legs))) {
  r <- legs[i]
  ddir <- file.path(LOCAL, r$vintage, r$scenario, "conventional", "detail")
  if (!dir.exists(ddir)) next
  yrs <- sort(as.integer(gsub("\\.csv$","",list.files(ddir, pattern="\\.csv$"))))
  for (y in yrs) {
    p <- file.path(ddir, paste0(y, ".csv"))
    hdr <- tryCatch(names(fread(p, nrows=0)), error=function(e) character(0))
    use <- intersect(sel, hdr)
    if (!all(c("weight","net_worth") %in% use)) next
    dt <- tryCatch(fread(p, select=use), error=function(e) NULL); if (is.null(dt)) next
    drain_rows[[paste(r$scenario,y)]] <- data.frame(
      leg=r$leg, scenario=r$scenario, s=r$s, year=y,
      nw_total_T   = sum(dt$weight*dt$net_worth, na.rm=TRUE)/1e12,
      D_alloc_B    = if ("D_alloc" %in% use) sum(dt$weight*dt$D_alloc, na.rm=TRUE)/1e9 else NA_real_
    )
  }
}
drain <- rbindlist(drain_rows, fill=TRUE)
fwrite(drain, file.path(OUT, "s_impact_drain.csv"))
cat("Wrote s_impact_drain.csv:", nrow(drain), "rows\n")

#-------------------------------------------------------------------------------
# 4. JSON bundle
#-------------------------------------------------------------------------------
if (have_json) {
  jsonlite::write_json(
    list(receipts=rcpt, baseline=base, summary=summary_tbl, drain=drain,
         meta=list(window=range(WIN), generated=as.character(Sys.time()))),
    file.path(OUT, "s_impact_data.json"), auto_unbox=TRUE, digits=10, na="null")
  cat("Wrote s_impact_data.json\n")
}
cat("DONE\n")
