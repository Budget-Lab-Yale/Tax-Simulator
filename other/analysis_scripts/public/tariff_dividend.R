library(ggplot2)

#-------------------------
# Data
#-------------------------
years <- 2025:2035   # 11 years

gdp_alt <- c(1.982, 2.571, 1.633, 1.502, 1.762, 1.736, 1.711, 1.708, 1.736, 1.861, 1.883)
gdp_base <- c(1.982, 2.243, 1.823, 1.618, 1.762, 1.706, 1.703, 1.718, 1.743, 1.865, 1.886)

cpi_alt <- c(2.745, 2.914, 2.598, 2.268, 2.257, 2.360, 2.283, 2.438, 2.576, 2.434, 2.434)
cpi_base <- c(2.745, 2.903, 2.532, 2.189, 2.202, 2.324, 2.246, 2.398, 2.534, 2.392, 2.392)

u_alt <- c(4.241, 4.375, 4.363, 4.434, 4.353, 4.242, 4.189, 4.141, 4.152, 4.125, 4.110)
u_base <- c(4.241, 4.519, 4.528, 4.478, 4.351, 4.251, 4.214, 4.165, 4.171, 4.141, 4.125)

y10_alt <- c(4.302, 4.063, 4.020, 3.999, 4.028, 4.043, 4.030, 4.011, 4.000, 3.999, 3.997)
y10_base <- c(4.302, 3.975, 3.891, 3.912, 3.961, 3.970, 3.948, 3.926, 3.914, 3.912, 3.909)

# Deltas
gdp_growth_delta <- gdp_alt - gdp_base
gdp_level_delta  <- cumsum(gdp_growth_delta)

cpi_growth_delta <- cpi_alt - cpi_base
cpi_level_delta  <- cumsum(cpi_growth_delta)

u_level_delta    <- u_alt - u_base
y10_level_delta  <- y10_alt - y10_base

#-------------------------
# Long data for facet_wrap
#-------------------------
df <- rbind(
  data.frame(year = years,
             variable = "Real GDP",
             series   = "Growth delta",
             value    = gdp_growth_delta),
  data.frame(year = years,
             variable = "Real GDP",
             series   = "Level delta",
             value    = gdp_level_delta),
  data.frame(year = years,
             variable = "CPI",
             series   = "Growth delta",
             value    = cpi_growth_delta),
  data.frame(year = years,
             variable = "CPI",
             series   = "Level delta",
             value    = cpi_level_delta),
  data.frame(year = years,
             variable = "Unemployment",
             series   = "Level delta",
             value    = u_level_delta),
  data.frame(year = years,
             variable = "10-Year Treasury Yield",
             series   = "Level delta",
             value    = y10_level_delta)
)

df$variable <- factor(df$variable,
                      levels = c("Real GDP", "CPI", "Unemployment", "10-Year Treasury Yield"))

#-------------------------
# Plot with facet_wrap
#-------------------------

panel_cols <- c(
  "Real GDP"     = "#1b9e77",
  "CPI"          = "#d95f02",
  "Unemployment" = "#7570b3",
  "10-Year Treasury Yield"  = "#e7298a"
)

ggplot(df,
       aes(x = year, y = value,
           color   = variable,
           linetype = series,
           shape    = series)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ variable, ncol = 2, scales = "fixed") +   # <- fixed scales
  scale_color_manual(values = panel_cols, guide = "none") +
  scale_x_continuous(
    breaks = years,
    minor_breaks = NULL
  ) +
  labs(
    x = "Year",
    y = "Percentage Points (Growth) or Percent (Level)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey90", colour = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)     # <- rotated labels
  )
