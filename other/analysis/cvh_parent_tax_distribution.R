library(data.table)
library(ggplot2)

out_root <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ctc_analysis"
bl <- fread(file.path(out_root, "baseline/static/detail/2026.csv"),
            select = c("id", "weight", "agi", "dep_status", "n_dep_ctc", "liab_iit_net"))

dt <- bl[dep_status == 0 & n_dep_ctc > 0 & agi > 0 & agi <= 160000]

dt[, group := fcase(
  liab_iit_net <= 0,    "nonpositive",
  liab_iit_net <= 5000, "low_positive",
  default = "high_positive"
)]

cat("Overall:\n")
cat(sprintf("  Zero or negative:    %.1f%%\n",
    100 * dt[, weighted.mean(group == "nonpositive", weight)]))
cat(sprintf("  $1-$5,000:           %.1f%%\n",
    100 * dt[, weighted.mean(group == "low_positive", weight)]))
cat(sprintf("  $5,000+:             %.1f%%\n",
    100 * dt[, weighted.mean(group == "high_positive", weight)]))

# $10k bins to avoid gaps at high AGI where data is sparse
smooth_dt <- dt[, .(
  sh_nonpositive   = weighted.mean(group == "nonpositive", weight),
  sh_low_positive  = weighted.mean(group == "low_positive", weight),
  sh_high_positive = weighted.mean(group == "high_positive", weight)
), by = .(agi_bin = floor(agi / 10000) * 10000 + 5000)]
setorder(smooth_dt, agi_bin)

# Fill any NA shares with 0
smooth_dt[is.na(sh_nonpositive), sh_nonpositive := 0]
smooth_dt[is.na(sh_low_positive), sh_low_positive := 0]
smooth_dt[is.na(sh_high_positive), sh_high_positive := 0]

# Ensure complete grid of bins (fill missing bins via interpolation)
all_bins <- data.table(agi_bin = seq(min(smooth_dt$agi_bin), max(smooth_dt$agi_bin),
                                     by = diff(smooth_dt$agi_bin[1:2])))
smooth_dt <- merge(all_bins, smooth_dt, by = "agi_bin", all.x = TRUE)
for (col in c("sh_nonpositive", "sh_low_positive", "sh_high_positive")) {
  smooth_dt[is.na(get(col)), (col) := 0]
}

smooth_long <- melt(smooth_dt, id.vars = "agi_bin",
                    measure.vars = c("sh_high_positive", "sh_low_positive", "sh_nonpositive"),
                    variable.name = "group", value.name = "share")
smooth_long[is.na(share), share := 0]
smooth_long[, group := factor(group,
  levels = c("sh_high_positive", "sh_low_positive", "sh_nonpositive"),
  labels = c("$5,000+ Income Tax", "$1-$5,000 Income Tax", "Zero or Negative Income Tax")
)]

colors <- c("Zero or Negative Income Tax" = "#A8C4D8",
            "$1-$5,000 Income Tax"         = "#C8C8C8",
            "$5,000+ Income Tax"           = "#6E6E6E")

# Label positions
smooth_dt[, cum_hp := sh_high_positive]
smooth_dt[, cum_lp := sh_high_positive + sh_low_positive]
smooth_dt[, mid_hp := cum_hp / 2]
smooth_dt[, mid_lp := (cum_hp + cum_lp) / 2]
smooth_dt[, mid_np := (cum_lp + 1) / 2]

# Place labels at weighted centroid of each band
label_x_np <- smooth_dt[, weighted.mean(agi_bin, sh_nonpositive)]
label_x_lp <- smooth_dt[, weighted.mean(agi_bin, sh_low_positive)]
label_x_hp <- smooth_dt[, weighted.mean(agi_bin, sh_high_positive)]
nearest <- function(x) smooth_dt[which.min(abs(agi_bin - x))]

labels_dt <- data.table(
  x = c(label_x_np, label_x_lp, label_x_hp + 8000),
  y = c(nearest(label_x_np)$mid_np, nearest(label_x_lp)$mid_lp, nearest(label_x_hp)$mid_hp + 0.06),
  label = c("Zero or Negative\nIncome Tax", "$1-$5,000\nIncome Tax", "$5,000+\nIncome Tax")
)

p <- ggplot(smooth_long, aes(x = agi_bin, y = share, fill = group)) +
  geom_area(position = "stack", alpha = 0.75) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_text(data = labels_dt, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, size = 4.5, fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(values = colors, guide = "none") +
  scale_x_continuous(
    name = "Adjusted Gross Income",
    labels = function(x) paste0("$", formatC(x / 1000, format = "f", digits = 0), "k"),
    breaks = seq(0, 160000, by = 20000),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    name = "Share of Tax Units",
    labels = function(x) paste0(round(x * 100), "%"),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Income Tax Liability Distribution Among CTC-Eligible Parents (2026)",
    subtitle = "Current law, by AGI",
    caption = "Source: The Budget Lab at Yale."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(out_root, "parent_tax_liability_distribution.png"), p,
       width = 10, height = 5.625, dpi = 300)
ggsave(file.path(out_root, "parent_tax_liability_distribution.pdf"), p,
       width = 10, height = 5.625)
cat("Chart saved.\n")
