################################################################################
# Alt max waterfall decomposition
#
# For tax units below the alt max eligibility threshold, decompose WHY the alt
# max doesn't deliver a large tax cut: which existing provisions (standard
# deduction, CTC, EITC, other credits) have already pushed their liability
# below the alt max?
#
# Approach: analytical waterfall on the microdata (no new simulation runs).
#   T0 = bracket_tax(AGI)                   -- raw liability, no deductions/credits
#   T1 = bracket_tax(AGI - std_ded)         -- after standard deduction
#   T2 = T1 - CTC                           -- after CTC (nonref + ref)
#   T3 = T2 - EITC                          -- after EITC
#   A  = alt_max_rate * max(0, AGI - exempt) -- alt max tax
#
# "Potential alt max cut" = max(0, T0 - A)
# Each provision displaces part of that potential cut.
# Residual between analytical remainder and actual simulated cut = "other"
# (CDCTC, education credits, other nonref/ref credits, interaction effects).
################################################################################

library(data.table)
library(ggplot2)

# --- Configuration -----------------------------------------------------------
vintage   <- "202603120647"
out_root  <- file.path("/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1", vintage)
year_show <- 2026

# --- Load data ---------------------------------------------------------------
cat("Loading detail files...\n")
bl <- fread(file.path(out_root, "baseline/static/detail", paste0(year_show, ".csv")))
am <- fread(file.path(out_root, "alt_max/static/detail",  paste0(year_show, ".csv")),
            select = c("id", "liab_iit_net"))
setnames(am, "liab_iit_net", "liab_iit_net_am")

# Merge on id
bl <- merge(bl, am, by = "id")
bl[, actual_cut := liab_iit_net - liab_iit_net_am]

cat("Loading tax law...\n")
tl_bl <- fread(file.path(out_root, "baseline/static/supplemental/tax_law.csv"))
tl_bl <- tl_bl[year == year_show]

# Alt max params live in the reform tax law (baseline has them zeroed out)
tl_am <- fread(file.path(out_root, "alt_max/static/supplemental/tax_law.csv"))
tl_am <- tl_am[year == year_show]

# --- Extract tax law parameters by filing status -----------------------------
get_tl <- function(fs) {
  row_bl <- tl_bl[filing_status == fs]
  row_am <- tl_am[filing_status == fs]
  brackets <- as.numeric(row_bl[, paste0("ord.brackets", 1:7), with = FALSE])
  rates    <- as.numeric(row_bl[, paste0("ord.rates",    1:7), with = FALSE])
  alt_exempt <- as.numeric(row_am[, "alt_max.exempt"])
  alt_rate   <- as.numeric(row_am[, "alt_max.rate"])
  alt_qual   <- as.numeric(row_am[, "alt_max.qualify_mult"])
  list(brackets = brackets, rates = rates,
       alt_exempt = alt_exempt, alt_rate = alt_rate, alt_qual = alt_qual)
}

params <- list(`1` = get_tl(1), `2` = get_tl(2), `3` = get_tl(3), `4` = get_tl(4))

# --- Bracket tax function (vectorized per filing status) ---------------------
bracket_tax <- function(taxable_inc, brackets, rates) {
  tax <- rep(0, length(taxable_inc))
  for (i in seq_along(rates)) {
    lo  <- brackets[i]
    hi  <- if (i < length(brackets)) brackets[i + 1] else Inf
    tax <- tax + rates[i] * pmax(0, pmin(taxable_inc, hi) - lo)
  }
  return(tax)
}

# --- Compute waterfall for each tax unit ------------------------------------
cat("Computing waterfall...\n")

bl[, `:=`(T0 = NA_real_, T1 = NA_real_, alt_max_tax = NA_real_,
          eligible = FALSE, threshold = NA_real_)]

for (fs in c("1", "2", "3", "4")) {
  p   <- params[[fs]]
  idx <- bl$filing_status == as.integer(fs)
  if (sum(idx) == 0) next

  agi_fs <- bl$agi[idx]
  std_fs <- bl$std_ded[idx]

  # T0: bracket tax on full AGI (no deductions)
  bl[idx, T0 := bracket_tax(pmax(0, agi_fs), p$brackets, p$rates)]

  # T1: bracket tax after standard deduction
  bl[idx, T1 := bracket_tax(pmax(0, agi_fs - std_fs), p$brackets, p$rates)]

  # Alt max tax
  bl[idx, alt_max_tax := p$alt_rate * pmax(0, agi_fs - p$alt_exempt)]

  # Eligibility
  bl[idx, threshold := p$alt_qual * p$alt_exempt]
  bl[idx, eligible  := agi_fs < (p$alt_qual * p$alt_exempt)]
}

# T2: after CTC
bl[, T2 := T1 - ctc_nonref - ctc_ref]

# T3: after EITC
bl[, T3 := T2 - eitc]

# --- Waterfall components ----------------------------------------------------
# Potential alt max cut (before any provisions eat into it)
bl[, potential_cut := pmax(0, T0 - alt_max_tax)]

# At each step, the "remaining benefit" = max(0, current_liability - alt_max_tax)
bl[, benefit_after_T0 := pmax(0, T0 - alt_max_tax)]  # = potential_cut
bl[, benefit_after_T1 := pmax(0, T1 - alt_max_tax)]
bl[, benefit_after_T2 := pmax(0, T2 - alt_max_tax)]
bl[, benefit_after_T3 := pmax(0, T3 - alt_max_tax)]

bl[, displace_std  := benefit_after_T0 - benefit_after_T1]
bl[, displace_ctc  := benefit_after_T1 - benefit_after_T2]
bl[, displace_eitc := benefit_after_T2 - benefit_after_T3]

# Residual: difference between analytical remainder and actual simulated cut
# Captures CDCTC, education credits, other credits, and interaction effects
bl[, displace_other := benefit_after_T3 - pmax(0, actual_cut)]

bl[, remaining_cut := pmax(0, actual_cut)]

# --- Flag: is this a parent? -------------------------------------------------
bl[, is_parent := (n_dep_ctc > 0)]

# --- Filter to eligible population -------------------------------------------
elig <- bl[eligible == TRUE & dep_status == 0]
cat(sprintf("Eligible population: %s tax units (weighted: %s)\n",
            format(nrow(elig), big.mark = ","),
            format(round(sum(elig$weight)), big.mark = ",")))

# --- Summary tables -----------------------------------------------------------

# Table 1: By cut threshold — what share sees < $X cut, and why?
cat("\n===== Table 1: Share of eligible pop seeing < $X actual alt max cut =====\n")
thresholds <- c(0, 100, 500, 1000, 2000)
total_wt   <- sum(elig$weight)

tab1 <- rbindlist(lapply(thresholds, function(x) {
  sub <- elig[actual_cut < x]
  wt  <- sum(sub$weight)
  data.table(
    cut_threshold   = x,
    n_units         = nrow(sub),
    weighted_units  = round(wt),
    pct_of_eligible = round(100 * wt / total_wt, 1),
    avg_potential   = round(weighted.mean(sub$potential_cut,   sub$weight), 0),
    avg_displ_std   = round(weighted.mean(sub$displace_std,    sub$weight), 0),
    avg_displ_ctc   = round(weighted.mean(sub$displace_ctc,    sub$weight), 0),
    avg_displ_eitc  = round(weighted.mean(sub$displace_eitc,   sub$weight), 0),
    avg_displ_other = round(weighted.mean(sub$displace_other,  sub$weight), 0),
    avg_actual_cut  = round(weighted.mean(sub$actual_cut,      sub$weight), 0)
  )
}))
print(tab1)

# Table 2: By parent status and filing status (eligible, actual cut < $500)
cat("\n===== Table 2: Waterfall by parent × filing status (eligible, actual cut < $500) =====\n")
sub500 <- elig[actual_cut < 500]

tab2 <- sub500[, .(
  n              = .N,
  weighted_n     = round(sum(weight)),
  avg_agi        = round(weighted.mean(agi, weight), 0),
  avg_potential  = round(weighted.mean(potential_cut, weight), 0),
  avg_displ_std  = round(weighted.mean(displace_std, weight), 0),
  avg_displ_ctc  = round(weighted.mean(displace_ctc, weight), 0),
  avg_displ_eitc = round(weighted.mean(displace_eitc, weight), 0),
  avg_displ_other = round(weighted.mean(displace_other, weight), 0),
  avg_actual_cut = round(weighted.mean(actual_cut, weight), 0),
  avg_liab_iit   = round(weighted.mean(liab_iit_net, weight), 0)
), by = .(is_parent, filing_status)]
tab2 <- tab2[order(is_parent, filing_status)]
print(tab2)

# Table 3: Overall waterfall — parents vs non-parents (all eligible)
cat("\n===== Table 3: Waterfall by parent status (all eligible) =====\n")
tab3 <- elig[, .(
  weighted_n       = round(sum(weight)),
  avg_agi          = round(weighted.mean(agi, weight), 0),
  avg_T0           = round(weighted.mean(T0, weight), 0),
  avg_T1           = round(weighted.mean(T1, weight), 0),
  avg_T2           = round(weighted.mean(T2, weight), 0),
  avg_T3           = round(weighted.mean(T3, weight), 0),
  avg_alt_max_tax  = round(weighted.mean(alt_max_tax, weight), 0),
  avg_potential    = round(weighted.mean(potential_cut, weight), 0),
  avg_displ_std    = round(weighted.mean(displace_std, weight), 0),
  avg_displ_ctc    = round(weighted.mean(displace_ctc, weight), 0),
  avg_displ_eitc   = round(weighted.mean(displace_eitc, weight), 0),
  avg_displ_other  = round(weighted.mean(displace_other, weight), 0),
  avg_actual_cut   = round(weighted.mean(actual_cut, weight), 0),
  pct_cut_lt_100   = round(100 * sum(weight[actual_cut < 100]) / sum(weight), 1),
  pct_cut_lt_500   = round(100 * sum(weight[actual_cut < 500]) / sum(weight), 1)
), by = .(is_parent)]
print(tab3)

# Table 4: Waterfall by AGI bin — eligible parents
cat("\n===== Table 4: Waterfall by AGI bin — eligible parents =====\n")
parents <- elig[is_parent == TRUE]
parents[, agi_bin := cut(agi, breaks = c(-Inf, 0, 10000, 20000, 30000, 40000,
                                          50000, 60000, 80000, Inf),
                         labels = c("<0", "0-10k", "10-20k", "20-30k", "30-40k",
                                    "40-50k", "50-60k", "60-80k", "80k+"))]

tab4 <- parents[, .(
  weighted_n      = round(sum(weight)),
  avg_agi         = round(weighted.mean(agi, weight), 0),
  avg_potential   = round(weighted.mean(potential_cut, weight), 0),
  avg_displ_std   = round(weighted.mean(displace_std, weight), 0),
  avg_displ_ctc   = round(weighted.mean(displace_ctc, weight), 0),
  avg_displ_eitc  = round(weighted.mean(displace_eitc, weight), 0),
  avg_displ_other = round(weighted.mean(displace_other, weight), 0),
  avg_actual_cut  = round(weighted.mean(actual_cut, weight), 0),
  pct_no_benefit  = round(100 * sum(weight[actual_cut < 100]) / sum(weight), 1)
), by = agi_bin]
tab4 <- tab4[order(agi_bin)]
print(tab4)

# Table 5: Waterfall by AGI bin — eligible non-parents
cat("\n===== Table 5: Waterfall by AGI bin — eligible non-parents =====\n")
nonparents <- elig[is_parent == FALSE]
nonparents[, agi_bin := cut(agi, breaks = c(-Inf, 0, 10000, 20000, 30000, 40000,
                                             50000, 60000, 80000, Inf),
                            labels = c("<0", "0-10k", "10-20k", "20-30k", "30-40k",
                                       "40-50k", "50-60k", "60-80k", "80k+"))]

tab5 <- nonparents[, .(
  weighted_n      = round(sum(weight)),
  avg_agi         = round(weighted.mean(agi, weight), 0),
  avg_potential   = round(weighted.mean(potential_cut, weight), 0),
  avg_displ_std   = round(weighted.mean(displace_std, weight), 0),
  avg_displ_ctc   = round(weighted.mean(displace_ctc, weight), 0),
  avg_displ_eitc  = round(weighted.mean(displace_eitc, weight), 0),
  avg_displ_other = round(weighted.mean(displace_other, weight), 0),
  avg_actual_cut  = round(weighted.mean(actual_cut, weight), 0),
  pct_no_benefit  = round(100 * sum(weight[actual_cut < 100]) / sum(weight), 1)
), by = agi_bin]
tab5 <- tab5[order(agi_bin)]
print(tab5)

# ==============================================================================
# Chart: Stacked bar decomposition of potential alt max cut by $5k AGI bin
# Two panels: parents vs non-parents
# ==============================================================================
cat("\n===== Building chart... =====\n")

# Prepare chart data: $5k AGI bins, AGI > 0, up to $100k
chart_dt <- elig[agi > 0 & agi <= 160000]
chart_dt[, agi_bin_5k := floor(agi / 5000) * 5000]

# Aggregate by AGI bin × parent status
chart_agg <- chart_dt[, .(
  weighted_n      = sum(weight),
  potential_cut   = weighted.mean(potential_cut, weight),
  displace_std    = weighted.mean(displace_std, weight),
  displace_ctc    = weighted.mean(displace_ctc, weight),
  displace_eitc   = weighted.mean(displace_eitc, weight),
  displace_other  = weighted.mean(displace_other, weight),
  actual_cut      = weighted.mean(remaining_cut, weight)
), by = .(agi_bin_5k, is_parent)]

# Reshape to long for ggplot stacked bars
# Stack order bottom-to-top: actual cut, other, EITC, CTC, std ded
chart_long <- melt(chart_agg,
                   id.vars = c("agi_bin_5k", "is_parent", "weighted_n", "potential_cut"),
                   measure.vars = c("actual_cut", "displace_other", "displace_eitc",
                                    "displace_ctc", "displace_std"),
                   variable.name = "component", value.name = "dollars")

# Labels and ordering
chart_long[, component := factor(component,
  levels = c("displace_std", "displace_ctc", "displace_eitc",
             "displace_other", "actual_cut"),
  labels = c("Displaced by\nStandard Deduction",
             "Displaced by\nChild Tax Credit",
             "Displaced by\nEITC",
             "Displaced by\nOther Provisions",
             "Actual Tax Cut")
)]

chart_long[, panel := ifelse(is_parent, "Parents", "Non-Parents")]
chart_long[, panel := factor(panel, levels = c("Parents", "Non-Parents"))]

# Bin label for x axis (center of bin)
chart_long[, agi_mid := agi_bin_5k + 2500]

# Colors: actual cut in blue, displacements in warm/gray tones
colors <- c(
  "Actual Tax Cut"                    = "#2166AC",
  "Displaced by\nOther Provisions"    = "#BDBDBD",
  "Displaced by\nEITC"               = "#FB9A99",
  "Displaced by\nChild Tax Credit"   = "#E31A1C",
  "Displaced by\nStandard Deduction" = "#FF7F00"
)

p <- ggplot(chart_long, aes(x = agi_mid, y = dollars, fill = component)) +
  geom_col(width = 4500, position = "stack") +
  facet_wrap(~panel) +
  scale_fill_manual(values = colors, name = NULL) +
  scale_x_continuous(
    name = "Adjusted Gross Income",
    labels = function(x) paste0("$", formatC(x / 1000, format = "f", digits = 0), "k"),
    breaks = seq(0, 160000, by = 20000)
  ) +
  scale_y_continuous(
    name = "Average Dollars",
    labels = scales::dollar_format()
  ) +
  labs(
    title = "Decomposition of Potential Alternative Maximum Tax Benefit",
    subtitle = paste0("Eligible tax units (AGI < eligibility threshold), ", year_show),
    caption = "Source: The Budget Lab at Yale. Stacking order: standard deduction applied first, then CTC, then EITC."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

chart_path <- file.path(out_root, "alt_max_waterfall_decomposition.pdf")
ggsave(chart_path, p, width = 12, height = 6.5)
cat(sprintf("Chart saved to: %s\n", chart_path))

# Also save a PNG
chart_path_png <- file.path(out_root, "alt_max_waterfall_decomposition.png")
ggsave(chart_path_png, p, width = 12, height = 6.5, dpi = 200)
cat(sprintf("PNG saved to: %s\n", chart_path_png))

cat("\nDone.\n")
