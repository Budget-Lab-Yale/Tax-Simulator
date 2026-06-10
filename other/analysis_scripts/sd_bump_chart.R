library(tidyverse)
library(scales)

d <- read_csv("other/analysis_scripts/sd_bump_results.csv", show_col_types = FALSE)
totals <- read_csv("other/analysis_scripts/iit_totals.csv", show_col_types = FALSE)
d <- d %>% left_join(totals, by = "scenario")

baseline_iit <- d %>% filter(scenario == "baseline") %>% pull(liab_iit_net_total)

d <- d %>%
  mutate(
    share_pct = share * 100,
    cost_b    = baseline_iit - liab_iit_net_total,
    label     = if_else(scenario == "baseline", "—",
                        paste0("-$", formatC(cost_b, format = "f", digits = 0), "B")),
    x_label   = paste0("$", formatC(sd_single, big.mark = ",", format = "d")),
    hit_50    = share_pct >= 50
  ) %>%
  arrange(sd_single) %>%
  mutate(x_label = factor(x_label, levels = x_label))

p <- ggplot(d, aes(x = x_label, y = share_pct)) +
  geom_col(width = 0.75, fill = "#2E5C8A") +
  geom_text(aes(label = label), vjust = -0.6, size = 3.4, color = "grey25") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.6, y = 51.5, label = "50%", color = "grey40",
           size = 3.2, hjust = 0, fontface = "italic") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(d$share_pct) * 1.13),
                     expand = c(0, 0)) +
  labs(
    title    = "At what standard deduction level does half the population pay no income tax?",
    subtitle = "Static 2026 IIT revenue cost (vs. current law) shown above each bar",
    x        = "Single-filer standard deduction",
    y        = "Share of tax units not paying income tax",
    caption  = "Joint and head-of-household standard deductions scaled proportionally with the single-filer value.\nSource: The Budget Lab calculations."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 17, margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "grey35", size = 13, margin = margin(b = 14)),
    plot.caption       = element_text(color = "grey45", hjust = 0, size = 11, margin = margin(t = 14)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin        = margin(18, 18, 14, 18),
    axis.title.x       = element_text(margin = margin(t = 10), color = "grey25"),
    axis.title.y       = element_text(margin = margin(r = 10), color = "grey25"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y        = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey92")
  )

ggsave("/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/analysis/sd_bump_nonpayer.png",
       p, width = 10.5, height = 7.5, dpi = 200)
ggsave("/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/analysis/sd_bump_nonpayer.pdf",
       p, width = 10.5, height = 7.5)

cat("Saved chart to other/analysis/sd_bump_nonpayer.{png,pdf}\n")
