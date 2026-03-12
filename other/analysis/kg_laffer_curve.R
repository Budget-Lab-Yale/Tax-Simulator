library(tidyverse)

# Output vintage path
vintage_dir = "/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202603121607"

# Read all revenue estimates
results = expand_grid(pp = 1:25, elast = c(62, 72)) %>%
  mutate(
    scenario = paste0("kg_top_", pp, "pp_e", elast),
    top_rate = 20 + pp,
    elasticity = paste0("-0.", elast),
    path = file.path(vintage_dir, scenario, "conventional/supplemental/revenue_estimates.csv")
  ) %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  unnest(data) %>%
  filter(year == 2026) %>%
  select(pp, top_rate, elasticity, revenue = total)

# Plot: Laffer curve — revenue delta vs top rate increase, by elasticity
p = results %>%
  ggplot(aes(x = top_rate, y = revenue, color = elasticity)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = seq(21, 45, 2), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Capital Gains Laffer Curve: Revenue vs. Top Rate (2026)",
    subtitle = "Conventional revenue change from raising top preferred rate (baseline = 20%)",
    x = "Top Capital Gains Rate",
    y = "Revenue Change ($B)",
    color = "Elasticity"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(vintage_dir, "kg_laffer_curve.pdf"), p, width = 10, height = 6)
ggsave(file.path(vintage_dir, "kg_laffer_curve.png"), p, width = 10, height = 6, dpi = 200)

cat("Saved to", vintage_dir, "\n")
