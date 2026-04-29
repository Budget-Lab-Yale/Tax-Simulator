################################################################################
# CTC value comparison: current law vs alt max world
#
# For each tax unit, compute:
#   ctc_value_cl  = liab(no_ctc, current law) - liab(baseline, current law)
#   ctc_value_am  = liab(no_ctc, alt max)     - liab(baseline, alt max)
#   lost_ctc      = ctc_value_cl - ctc_value_am
#
# If the alt max erodes CTC value, lost_ctc > 0 for parents.
# Chart: AGI on x-axis ($5k bins), two panels (parents / non-parents),
#        lines showing CTC value under each world + the alt max tax cut.
################################################################################

library(data.table)
library(ggplot2)

# --- Configuration -----------------------------------------------------------
out_root_cl <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ctc_analysis"
out_root_am <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ctc_analysis_altmax"

year_show <- 2026

# --- Load data ---------------------------------------------------------------
cat("Loading detail files...\n")

# Current law world: baseline and no_ctc
bl_cl <- fread(file.path(out_root_cl, "baseline/static/detail", paste0(year_show, ".csv")),
               select = c("id", "weight", "agi", "filing_status", "dep_status",
                           "n_dep_ctc", "liab_iit_net", "liab_bc", "ctc_nonref", "ctc_ref", "eitc"))
no_ctc_cl <- fread(file.path(out_root_cl, "no_ctc/static/detail", paste0(year_show, ".csv")),
                   select = c("id", "liab_iit_net"))
setnames(no_ctc_cl, "liab_iit_net", "liab_no_ctc_cl")

# Alt max world: baseline (= alt max) and no_ctc (= alt max + no ctc)
bl_am <- fread(file.path(out_root_am, "baseline/static/detail", paste0(year_show, ".csv")),
               select = c("id", "liab_iit_net", "liab_bc"))
setnames(bl_am, c("liab_iit_net", "liab_bc"), c("liab_am", "liab_bc_am"))
no_ctc_am <- fread(file.path(out_root_am, "no_ctc/static/detail", paste0(year_show, ".csv")),
                   select = c("id", "liab_iit_net"))
setnames(no_ctc_am, "liab_iit_net", "liab_no_ctc_am")

# Merge everything onto baseline current law
dt <- bl_cl
dt <- merge(dt, no_ctc_cl, by = "id")
dt <- merge(dt, bl_am, by = "id")
dt <- merge(dt, no_ctc_am, by = "id")

# --- Compute CTC value in each world ----------------------------------------
# CTC value = how much does having CTC reduce your tax?
# = liab(without CTC) - liab(with CTC)
dt[, ctc_value_cl := liab_no_ctc_cl - liab_iit_net]     # CTC value under current law
dt[, ctc_value_am := liab_no_ctc_am - liab_am]           # CTC value under alt max

# How much CTC value is lost due to the alt max
dt[, ctc_value_lost := ctc_value_cl - ctc_value_am]

# Alt max tax cut (from the actual runs)
dt[, alt_max_cut := liab_iit_net - liab_am]

# Pre-credit tax cut from alt max
dt[, pre_credit_cut := liab_bc - liab_bc_am]

# Flags
dt[, is_parent := (n_dep_ctc > 0)]

# Filter to nondependent filers with positive AGI
dt <- dt[dep_status == 0 & agi > 0]

# Alt max eligibility (read from reform tax law)
tl_am <- fread(file.path(out_root_am, "baseline/static/supplemental/tax_law.csv"))
tl_am <- tl_am[year == year_show]
for (fs in 1:4) {
  exempt <- tl_am[filing_status == fs, alt_max.exempt]
  qual   <- tl_am[filing_status == fs, alt_max.qualify_mult]
  dt[filing_status == fs, eligible := agi < qual * exempt]
}

cat(sprintf("Total records: %s\n", format(nrow(dt), big.mark = ",")))

# --- Summary tables -----------------------------------------------------------
cat("\n===== CTC value comparison: parents, by AGI bin =====\n")
parents <- dt[is_parent == TRUE & eligible == TRUE]
parents[, agi_bin := cut(agi, breaks = seq(0, 160000, by = 10000),
                         labels = paste0(seq(0, 150, by = 10), "k"),
                         include.lowest = TRUE)]

tab_parents <- parents[!is.na(agi_bin), .(
  weighted_n      = round(sum(weight)),
  avg_ctc_val_cl  = round(weighted.mean(ctc_value_cl, weight), 0),
  avg_ctc_val_am  = round(weighted.mean(ctc_value_am, weight), 0),
  avg_ctc_lost    = round(weighted.mean(ctc_value_lost, weight), 0),
  avg_pre_cred_cut = round(weighted.mean(pre_credit_cut, weight), 0),
  avg_alt_max_cut = round(weighted.mean(alt_max_cut, weight), 0)
), by = agi_bin]
print(tab_parents[order(agi_bin)])

cat("\n===== CTC value comparison: non-parents, by AGI bin =====\n")
nonparents <- dt[is_parent == FALSE & eligible == TRUE]
nonparents[, agi_bin := cut(agi, breaks = seq(0, 160000, by = 10000),
                            labels = paste0(seq(0, 150, by = 10), "k"),
                            include.lowest = TRUE)]

tab_nonparents <- nonparents[!is.na(agi_bin), .(
  weighted_n      = round(sum(weight)),
  avg_ctc_val_cl  = round(weighted.mean(ctc_value_cl, weight), 0),
  avg_ctc_val_am  = round(weighted.mean(ctc_value_am, weight), 0),
  avg_ctc_lost    = round(weighted.mean(ctc_value_lost, weight), 0),
  avg_pre_cred_cut = round(weighted.mean(pre_credit_cut, weight), 0),
  avg_alt_max_cut = round(weighted.mean(alt_max_cut, weight), 0)
), by = agi_bin]
print(tab_nonparents[order(agi_bin)])

# --- Chart: CTC value under current law vs alt max, by AGI ------------------
cat("\n===== Building charts... =====\n")

# $5k bins for chart
chart_dt <- dt[eligible == TRUE & agi <= 160000]
chart_dt[, agi_bin_5k := floor(agi / 5000) * 5000 + 2500]  # bin midpoint

chart_agg <- chart_dt[, .(
  ctc_value_cl   = weighted.mean(ctc_value_cl, weight),
  ctc_value_am   = weighted.mean(ctc_value_am, weight),
  ctc_value_lost = weighted.mean(ctc_value_lost, weight),
  alt_max_cut    = weighted.mean(alt_max_cut, weight),
  pre_credit_cut = weighted.mean(pre_credit_cut, weight),
  weighted_n     = sum(weight)
), by = .(agi_bin_5k, is_parent, n_dep_ctc)]

chart_agg[, panel := ifelse(is_parent, "Parents", "Non-Parents")]
chart_agg[, panel := factor(panel, levels = c("Parents", "Non-Parents"))]

# --- Chart 1: CTC value under each world ------------------------------------
chart_long <- melt(chart_agg,
                   id.vars = c("agi_bin_5k", "panel", "weighted_n"),
                   measure.vars = c("ctc_value_cl", "ctc_value_am"),
                   variable.name = "world", value.name = "ctc_value")
chart_long[, world := factor(world,
  levels = c("ctc_value_cl", "ctc_value_am"),
  labels = c("Current Law", "With Alternative Maximum Tax")
)]

p1 <- ggplot(chart_long, aes(x = agi_bin_5k, y = ctc_value, color = world)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  facet_wrap(~panel) +
  scale_color_manual(values = c("Current Law" = "#2166AC",
                                "With Alternative Maximum Tax" = "#E31A1C"),
                     name = NULL) +
  scale_x_continuous(
    name = "Adjusted Gross Income",
    labels = function(x) paste0("$", formatC(x / 1000, format = "f", digits = 0), "k"),
    breaks = seq(0, 160000, by = 20000)
  ) +
  scale_y_continuous(
    name = "Average Value of CTC ($)",
    labels = scales::dollar_format()
  ) +
  labs(
    title = "Value of the Child Tax Credit: Current Law vs. Alternative Maximum Tax",
    subtitle = paste0("Average CTC benefit by AGI, eligible tax units, ", year_show),
    caption = "Source: The Budget Lab at Yale. CTC value = tax liability without CTC minus tax liability with CTC."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(out_root_cl, "ctc_value_comparison.pdf"), p1, width = 12, height = 6.5)
ggsave(file.path(out_root_cl, "ctc_value_comparison.png"), p1, width = 12, height = 6.5, dpi = 200)
cat("Chart 1 saved (CTC value comparison).\n")

# --- Chart 2: Alt max cut decomposition — pre-credit cut vs net cut ----------
# Shows: the pre-credit cut is the same for parents and non-parents,
# but the net cut differs because CTC value is lost
chart_cuts <- melt(chart_agg,
                   id.vars = c("agi_bin_5k", "panel"),
                   measure.vars = c("pre_credit_cut", "alt_max_cut"),
                   variable.name = "measure", value.name = "dollars")
chart_cuts[, measure := factor(measure,
  levels = c("pre_credit_cut", "alt_max_cut"),
  labels = c("Pre-Credit Tax Cut", "Net Tax Cut (After Credits)")
)]

p2 <- ggplot(chart_cuts, aes(x = agi_bin_5k, y = dollars, color = measure)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  facet_wrap(~panel) +
  scale_color_manual(values = c("Pre-Credit Tax Cut" = "#BDBDBD",
                                "Net Tax Cut (After Credits)" = "#2166AC"),
                     name = NULL) +
  scale_x_continuous(
    name = "Adjusted Gross Income",
    labels = function(x) paste0("$", formatC(x / 1000, format = "f", digits = 0), "k"),
    breaks = seq(0, 160000, by = 20000)
  ) +
  scale_y_continuous(
    name = "Average Tax Cut ($)",
    labels = scales::dollar_format()
  ) +
  labs(
    title = "Alternative Maximum Tax: Pre-Credit vs. Net Tax Cut",
    subtitle = paste0("The gap between the lines is CTC value lost to the alt max (", year_show, ")"),
    caption = "Source: The Budget Lab at Yale."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(out_root_cl, "alt_max_pre_vs_net_cut.pdf"), p2, width = 12, height = 6.5)
ggsave(file.path(out_root_cl, "alt_max_pre_vs_net_cut.png"), p2, width = 12, height = 6.5, dpi = 200)
cat("Chart 2 saved (pre-credit vs net cut).\n")

# --- Chart 3: The lost CTC value (shaded area) ------------------------------
chart_parents <- chart_agg[panel == "Parents" & n_dep_ctc %in% c(1, 2)]
chart_parents[, kid_panel := factor(paste0(n_dep_ctc, ifelse(n_dep_ctc == 1, " Child", " Children")))]
p3 <- ggplot(chart_parents, aes(x = agi_bin_5k)) +
  facet_wrap(~kid_panel) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_ribbon(aes(ymin = ctc_value_am, ymax = ctc_value_cl),
              fill = "#E31A1C", alpha = 0.25) +
  geom_line(aes(y = ctc_value_cl, color = "Current Law"), linewidth = 0.9) +
  geom_line(aes(y = ctc_value_am, color = "CVH Plan"), linewidth = 0.9) +
  scale_color_manual(values = c("Current Law" = "black",
                                "CVH Plan" = "#E31A1C"),
                     name = NULL) +
  scale_x_continuous(
    name = "Adjusted Gross Income",
    labels = function(x) paste0("$", formatC(x / 1000, format = "f", digits = 0), "k"),
    breaks = seq(0, 160000, by = 20000),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    name = "Average Tax Benefit of CTC",
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = paste0("Tax Benefit of CTC, Current Law Versus CVH Plan (", year_show, ")"),
    subtitle = "Tax value is calculated as the effect of repealing the CTC on net income tax liability (times negative 1)",
    caption = "Source: The Budget Lab at Yale."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Twitter: 16:9, high DPI, large enough for mobile
ggsave(file.path(out_root_cl, "ctc_value_erosion.pdf"), p3, width = 10, height = 5.625)
ggsave(file.path(out_root_cl, "ctc_value_erosion.png"), p3, width = 10, height = 5.625, dpi = 300)
cat("Chart 3 saved (CTC erosion ribbon).\n")

cat("\nDone.\n")
