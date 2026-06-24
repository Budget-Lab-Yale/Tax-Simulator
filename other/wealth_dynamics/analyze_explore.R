#-------------------------------------------------------------------------------
# analyze_explore.R
#
# Reads the wealth-bathtub exploration vintage and produces tidy summaries for
# the results artifact. The clean isolation of the bathtub channel is the
# s=0 vs s=0.5 contrast on the CONVENTIONAL pass (everything else held fixed;
# only the saving share flips). conv-vs-static conflates the avoidance/kg
# behavior with the channel, so we report both but lead with s0-vs-s50.
#
# Outputs (in other/wealth_dynamics/):
#   explore_totals.csv     - long: scenario x pass x year, all headline totals
#   explore_networth.csv   - long: scenario x pass x year, weighted NW stock +
#                            haircut diagnostics (from detail)
#   explore_data.json      - everything, nested, for the HTML artifact
#-------------------------------------------------------------------------------

suppressMessages({
  library(data.table)
})
have_json <- requireNamespace("jsonlite", quietly = TRUE)

ROOT <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/wealth_explore"
OUT  <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"

scenarios <- c("baseline",
               "cg5_s0", "cg5_s50",
               "warren_s0", "warren_s50",
               "nd_s0", "nd_s50",
               "estate2009_s0", "estate2009_s50",
               "warren_estate2009_s0", "warren_estate2009_s50")

read_csv_safe <- function(p) {
  if (!file.exists(p)) return(NULL)
  tryCatch(as.data.frame(fread(p)), error = function(e) NULL)
}

# Capital-income flow columns we can pull off 1040.csv (gross flows, $B).
cap_cols <- c("txbl_int", "exempt_int", "div_ord", "div_pref", "txbl_kg",
              "net_rent", "net_estate", "sch_e", "sole_prop", "part_scorp", "farm")

#-------------------------------------------------------------------------------
# 1. Headline totals (fast: reads totals/*.csv only)
#-------------------------------------------------------------------------------
tot_rows <- list()
for (sc in scenarios) {
  for (pass in c("static", "conventional")) {
    base <- file.path(ROOT, sc, pass, "totals")
    d1040   <- read_csv_safe(file.path(base, "1040.csv"))
    destate <- read_csv_safe(file.path(base, "estate.csv"))
    dwealth <- read_csv_safe(file.path(base, "wealth.csv"))
    drcpt   <- read_csv_safe(file.path(base, "receipts.csv"))
    if (is.null(d1040)) next

    df <- data.frame(scenario = sc, pass = pass, year = d1040$year)
    df$liab_iit_net <- if ("liab_iit_net" %in% names(d1040)) d1040$liab_iit_net else NA_real_
    df$liab_iit     <- if ("liab_iit"     %in% names(d1040)) d1040$liab_iit     else NA_real_
    df$agi          <- if ("agi"          %in% names(d1040)) d1040$agi          else NA_real_
    # capital-income base aggregate (sum of available cap flow cols)
    have <- intersect(cap_cols, names(d1040))
    df$cap_income   <- if (length(have)) rowSums(d1040[have], na.rm = TRUE) else NA_real_
    for (cc in cap_cols) df[[cc]] <- if (cc %in% names(d1040)) d1040[[cc]] else NA_real_

    # estate / wealth liabilities (CY)
    m <- match(df$year, if (!is.null(destate)) destate$year else integer(0))
    df$est_tax    <- if (!is.null(destate)) destate$est_tax_exp[m] else NA_real_
    mw <- match(df$year, if (!is.null(dwealth)) dwealth$year else integer(0))
    df$wealth_tax <- if (!is.null(dwealth)) dwealth$wealth_tax[mw] else NA_real_

    # FY-booked receipts (lagged) for the budget view
    if (!is.null(drcpt)) {
      mr <- match(df$year, drcpt$year)
      df$rcpt_income <- drcpt$revenues_income_tax[mr]
      df$rcpt_estate <- drcpt$revenues_estate_tax[mr]
      df$rcpt_wealth <- drcpt$revenues_wealth_tax[mr]
      df$rcpt_payroll<- drcpt$revenues_payroll_tax[mr]
    } else {
      df$rcpt_income <- df$rcpt_estate <- df$rcpt_wealth <- df$rcpt_payroll <- NA_real_
    }
    tot_rows[[paste(sc, pass)]] <- df
  }
}
totals <- rbindlist(tot_rows, fill = TRUE)
fwrite(totals, file.path(OUT, "explore_totals.csv"))
cat("Wrote explore_totals.csv:", nrow(totals), "rows\n")

#-------------------------------------------------------------------------------
# 2. Net-worth stock + haircut diagnostics from detail (slower)
#    Only the conventional + static passes; select narrow columns.
#-------------------------------------------------------------------------------
nw_rows <- list()
sel <- c("weight", "net_worth", "net_worth_raw", "wealth_haircut", "D_alloc",
         "economic_gross", "liab_estate_dsue", "liab_estate_nodsue", "estate_m")
for (sc in scenarios) {
  for (pass in c("static", "conventional")) {
    ddir <- file.path(ROOT, sc, pass, "detail")
    if (!dir.exists(ddir)) next
    yrs <- sort(as.integer(gsub("\\.csv$", "", list.files(ddir, pattern = "\\.csv$"))))
    for (y in yrs) {
      p <- file.path(ddir, paste0(y, ".csv"))
      hdr <- tryCatch(names(fread(p, nrows = 0)), error = function(e) character(0))
      use <- intersect(sel, hdr)
      if (!("weight" %in% use) || !("net_worth" %in% use)) next
      dt <- tryCatch(fread(p, select = use), error = function(e) NULL)
      if (is.null(dt)) next
      w  <- dt$weight
      r <- data.frame(
        scenario = sc, pass = pass, year = y,
        nw_total      = sum(w * dt$net_worth, na.rm = TRUE) / 1e12,            # $T
        nw_raw_total  = if ("net_worth_raw" %in% use) sum(w * dt$net_worth_raw, na.rm = TRUE)/1e12 else NA_real_,
        gross_total   = if ("economic_gross" %in% use) sum(w * dt$economic_gross, na.rm = TRUE)/1e12 else NA_real_,
        D_alloc_total = if ("D_alloc" %in% use) sum(w * dt$D_alloc, na.rm = TRUE)/1e9 else NA_real_, # $B drained (cumulative deficit)
        hc_mean       = if ("wealth_haircut" %in% use) weighted.mean(dt$wealth_haircut, w, na.rm = TRUE) else NA_real_,
        hc_max        = if ("wealth_haircut" %in% use) max(abs(dt$wealth_haircut), na.rm = TRUE) else NA_real_,
        n_clamped     = if ("wealth_haircut" %in% use) sum(abs(dt$wealth_haircut) >= 0.8999, na.rm = TRUE) else NA_real_
      )
      nw_rows[[paste(sc, pass, y)]] <- r
    }
  }
}
networth <- rbindlist(nw_rows, fill = TRUE)
fwrite(networth, file.path(OUT, "explore_networth.csv"))
cat("Wrote explore_networth.csv:", nrow(networth), "rows\n")

#-------------------------------------------------------------------------------
# 3. Bundle to JSON for the artifact
#-------------------------------------------------------------------------------
out <- list(
  totals   = totals,
  networth = networth,
  meta = list(
    root = ROOT,
    scenarios = scenarios,
    generated = as.character(Sys.time())
  )
)
if (have_json) {
  jsonlite::write_json(out, file.path(OUT, "explore_data.json"), auto_unbox = TRUE,
                       digits = 10, na = "null")
  cat("Wrote explore_data.json\n")
} else {
  cat("jsonlite not available; skipped JSON (CSVs written).\n")
}
cat("DONE\n")
