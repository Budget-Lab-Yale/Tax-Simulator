#-------------------------------------------------------------------------------
# Synthetic-cohort net-worth growth by age, 2026 -> 2027  (v2, smoothed).
#
# Purpose: inform the wealth-dynamics feature's two age-indexed placeholders:
#   - r_total : rate the wealth deficit compounds at. Placeholder = nominal
#               GDP-per-capita growth (computed below).
#   - s(a)    : share of an above-baseline tax dollar financed from saving.
#               Anchored to the SHAPE of the implied saving rate by age.
#
# v1 used raw single-year-age means; their ratios exploded where mean net worth
# is small (e.g. age 25). Tax-Data grows wealth ~uniformly in time (same-age YoY
# ~3-4% at every age), so the real signal is the life-cycle LEVEL curve. v2
# smooths log(net worth) within each year, then derives the synthetic-cohort
# growth and saving rate from the smoothed levels.
#
#   g_smooth(a)   = nw27_smooth(a+1) / nw26_smooth(a) - 1     (total growth)
#   sigma(a; r)   = g_smooth(a) - r                            (saving / wealth)
#
# CAVEATS (printed): (1) mortality selection (survivors richer) biases old-age
# growth UP, so individual decumulation is steeper than shown; (2) the 80+ cell
# is topcode-pooled and pulled up by Forbes clones; (3) levels reflect Tax-Data's
# own projection assumptions.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(library(data.table))

DATA_DIR <- "/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline"
MACRO    <- "/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline/projections.csv"
OUT_DIR  <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

ASSET_COLS <- c('value.cash','value.equities','value.bonds','value.dc','value.db',
                'value.life_ins','value.annuities','value.trusts','value.other_fin',
                'value.pass_throughs','value.primary_home','value.other_home',
                'value.re_fund','value.other_nonfin')
DEBT_COLS  <- c('value.primary_mortgage','value.other_mortgage','value.credit_lines',
                'value.credit_cards','value.installment_debt','value.other_debt')

read_year <- function(year) {
  f <- file.path(DATA_DIR, sprintf("tax_units_%d.csv", year))
  stopifnot(file.exists(f))
  dt <- fread(f, select = c('id','weight','age1', ASSET_COLS, DEBT_COLS))
  dt[, assets   := rowSums(.SD, na.rm = TRUE), .SDcols = ASSET_COLS]
  dt[, debts    := rowSums(.SD, na.rm = TRUE), .SDcols = DEBT_COLS]
  dt[, networth := assets - debts]
  dt[, age_cell := pmin(as.integer(age1), 80L)]
  dt[age_cell >= 18L]
}

by_age <- function(dt) {
  dt[, .(nw  = sum(weight * networth) / sum(weight),
         pop = sum(weight)),
     by = age_cell][order(age_cell)]
}

cat("Reading 2026 ...\n"); d26 <- read_year(2026)
cat("Reading 2027 ...\n"); d27 <- read_year(2027)
m26 <- by_age(d26)
m27 <- by_age(d27)

ages <- 18:80

# ---- smooth log-levels within each year, evaluate on integer ages -----------
sm <- function(m) {
  lo <- lowess(m$age_cell, log(pmax(m$nw, 1)), f = 0.30)
  exp(approx(lo$x, lo$y, xout = ages, rule = 2)$y)
}
nw26_s <- sm(m26)
nw27_s <- sm(m27)
names(nw26_s) <- names(nw27_s) <- ages

# raw (for plotting points)
raw26 <- setNames(m26$nw, m26$age_cell)[as.character(ages)]
raw27 <- setNames(m27$nw, m27$age_cell)[as.character(ages)]

# ---- synthetic-cohort growth from smoothed levels: a (2026) -> a+1 (2027) ----
a_next <- pmin(ages + 1L, 80L)
g_smooth <- nw27_s[as.character(a_next)] / nw26_s[as.character(ages)] - 1
g_raw    <- raw27[as.character(a_next)]  / raw26[as.character(ages)]  - 1

# same-age YoY (the ~uniform time-growth component)
g_yoy <- raw27[as.character(ages)] / raw26[as.character(ages)] - 1

# ---- GDP-per-capita growth (r_total placeholder) -----------------------------
mp <- fread(MACRO)
pop_cols <- grep("^(unmarried|married)_[0-9]+$", names(mp), value = TRUE)
mp[, pop := rowSums(.SD, na.rm = TRUE), .SDcols = pop_cols]
g_gdp_nom <- mp[year == 2027, gdp]  / mp[year == 2026, gdp]  - 1
g_rgdp    <- mp[year == 2027, rgdp] / mp[year == 2026, rgdp] - 1
g_pop     <- mp[year == 2027, pop]  / mp[year == 2026, pop]  - 1
g_gdppc_n <- (1 + g_gdp_nom) / (1 + g_pop) - 1
g_gdppc_r <- (1 + g_rgdp)    / (1 + g_pop) - 1

g_nw_agg <- sum(d27$weight * d27$networth) / sum(d26$weight * d26$networth) - 1

out <- data.table(age = ages,
                  nw26_raw = round(raw26), nw26_smooth = round(nw26_s),
                  g_smooth = g_smooth, g_yoy = g_yoy,
                  sigma_r04 = g_smooth - 0.04,
                  sigma_r05 = g_smooth - 0.05)
fwrite(out, file.path(OUT_DIR, "cohort_wealth_growth_2026_2027.csv"))

# ---- plot (3 panels) ---------------------------------------------------------
png(file.path(OUT_DIR, "cohort_wealth_growth_2026_2027.png"),
    width = 1100, height = 1650, res = 130)
par(mfrow = c(3, 1), mar = c(4.0, 4.6, 2.6, 1.2))

# Panel 1: life-cycle net worth LEVEL (log scale)
plot(ages, raw26 / 1e6, type = "n", log = "y",
     xlab = "Age (2026)", ylab = "Mean net worth ($M, log)",
     main = "Life-cycle net worth (2026 cross-section)")
points(ages, raw26 / 1e6, pch = 16, col = "grey60", cex = 0.7)
lines(ages, nw26_s / 1e6, col = "navy", lwd = 2.5)
legend("bottomright", bty = "n", cex = 0.8,
       legend = c("raw weighted mean", "smoothed"),
       col = c("grey60", "navy"), pch = c(16, NA), lwd = c(NA, 2.5))

# Panel 2: synthetic-cohort total growth g(a)
plot(ages, 100 * g_smooth, type = "n", ylim = c(-2, 16),
     xlab = "Age in 2026 (synthetic cohort)", ylab = "Total net-worth growth (%)",
     main = "Synthetic-cohort total growth: g(a) = nw(a+1,2027)/nw(a,2026)-1")
abline(h = 0, col = "grey75")
abline(h = 100 * g_gdppc_n, col = "darkgreen", lty = 2)
abline(h = 5, col = "grey80", lty = 3)
points(ages, 100 * g_raw, pch = 1, col = "grey70", cex = 0.6)
lines(ages, 100 * g_smooth, col = "steelblue", lwd = 2.5)
legend("topright", bty = "n", cex = 0.8,
       legend = c("g_smooth(a)", "raw (noisy)",
                  sprintf("nom. GDP/capita = %.1f%%", 100 * g_gdppc_n),
                  "assumed return 5%"),
       col = c("steelblue", "grey70", "darkgreen", "grey80"),
       pch = c(NA, 1, NA, NA), lty = c(1, NA, 2, 3), lwd = c(2.5, NA, 1, 1))

# Panel 3: implied saving rate sigma(a) = g(a) - r
plot(ages, 100 * (g_smooth - 0.05), type = "n", ylim = c(-8, 12),
     xlab = "Age in 2026 (synthetic cohort)",
     ylab = "Implied saving rate, % of wealth",
     main = "Implied saving-out-of-wealth  sigma(a) = g(a) - r")
abline(h = 0, col = "grey50", lwd = 1.3)
lines(ages, 100 * (g_smooth - 0.04), col = "darkorange", lwd = 2.3)
lines(ages, 100 * (g_smooth - 0.05), col = "purple",     lwd = 2.3)
legend("topright", bty = "n", cex = 0.8,
       legend = c("sigma, r = 4%", "sigma, r = 5%", "sigma = 0 (accumulate above / decumulate below)"),
       col = c("darkorange", "purple", "grey50"), lty = c(1, 1, 1), lwd = c(2.3, 2.3, 1.3))

dev.off()

# ---- summary -----------------------------------------------------------------
cat("\n================ SUMMARY ================\n")
cat(sprintf("Nominal GDP growth 2026->2027         : %6.2f%%\n", 100 * g_gdp_nom))
cat(sprintf("Real   GDP growth 2026->2027          : %6.2f%%\n", 100 * g_rgdp))
cat(sprintf("Population growth 2026->2027          : %6.2f%%\n", 100 * g_pop))
cat(sprintf("Nominal GDP/capita growth (r_total*)  : %6.2f%%\n", 100 * g_gdppc_n))
cat(sprintf("Real    GDP/capita growth             : %6.2f%%\n", 100 * g_gdppc_r))
cat(sprintf("Aggregate nominal net-worth growth    : %6.2f%%\n", 100 * g_nw_agg))
cat("\n--- smoothed synthetic-cohort growth by age ---\n")
sel <- out[age %in% seq(20, 80, 5)]
print(sel[, .(age,
              nw_smooth_M = round(nw26_smooth / 1e6, 2),
              g_smooth_pct = round(100 * g_smooth, 1),
              sigma_r4_pct = round(100 * sigma_r04, 1),
              sigma_r5_pct = round(100 * sigma_r05, 1))])
cat("\nWrote:\n  ", file.path(OUT_DIR, "cohort_wealth_growth_2026_2027.csv"), "\n  ",
    file.path(OUT_DIR, "cohort_wealth_growth_2026_2027.png"), "\n", sep = "")
cat("=========================================\n")
