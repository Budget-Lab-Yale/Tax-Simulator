library(tidyverse)

# Paths
base_path = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/202603120647"

years = 2026:2055

results = map_dfr(years, function(yr) {

  # Read baseline and surtax detail
  bl = read_csv(file.path(base_path, "baseline/static/detail", paste0(yr, ".csv")),
                show_col_types = FALSE)
  st = read_csv(file.path(base_path, "surtax/static/detail", paste0(yr, ".csv")),
                show_col_types = FALSE)

  # Compute surtax delta per unit
  merged = bl %>%
    select(id, weight, wages, txbl_int, div_ord, div_pref, txbl_kg,
           sole_prop, sch_e, farm, agi,
           liab_iit_bl = liab_iit) %>%
    inner_join(
      st %>% select(id, liab_iit_st = liab_iit),
      by = "id"
    ) %>%
    mutate(
      delta = liab_iit_st - liab_iit_bl,
      # Only keep those hit by surtax (positive delta)
      hit = delta > 0
    )

  # For those hit, compute revenue-weighted income shares
  hit_units = merged %>% filter(hit)

  if (nrow(hit_units) == 0) return(tibble(year = yr))

  # Income categories (as shares of AGI)
  hit_units %>%
    mutate(
      inc_wages      = wages,
      inc_passthru   = sole_prop + sch_e + farm,
      inc_kg         = txbl_kg,
      inc_div        = div_ord + div_pref,
      inc_interest   = txbl_int,
      inc_other      = agi - (inc_wages + inc_passthru + inc_kg + inc_div + inc_interest),
      # Revenue weight
      rev_weight     = delta * weight
    ) %>%
    summarise(
      year           = yr,
      n_hit          = sum(weight),
      total_revenue  = sum(rev_weight),
      mean_agi       = weighted.mean(agi, rev_weight),
      # Revenue-weighted income shares
      sh_wages       = sum(inc_wages * rev_weight) / sum(agi * rev_weight),
      sh_passthru    = sum(inc_passthru * rev_weight) / sum(agi * rev_weight),
      sh_kg          = sum(inc_kg * rev_weight) / sum(agi * rev_weight),
      sh_div         = sum(inc_div * rev_weight) / sum(agi * rev_weight),
      sh_interest    = sum(inc_interest * rev_weight) / sum(agi * rev_weight),
      sh_other       = sum(inc_other * rev_weight) / sum(agi * rev_weight)
    )
})

cat("\n=== CVH Surtax: Revenue-Weighted Income Decomposition ===\n\n")

# Print year-by-year
results %>%
  mutate(across(starts_with("sh_"), ~ round(. * 100, 1))) %>%
  mutate(n_hit = round(n_hit),
         total_revenue = round(total_revenue / 1e9, 2)) %>%
  print(n = 50, width = 200)

# Print budget window average
cat("\n=== Budget Window Average (2026-2035) ===\n")
bw = results %>% filter(year >= 2026, year <= 2035)
cat(sprintf("Avg revenue/yr: $%.1fB\n", mean(bw$total_revenue) / 1e9))
cat(sprintf("Avg # hit:      %.0f\n", mean(bw$n_hit)))
cat(sprintf("\nRevenue-weighted AGI shares:\n"))
cat(sprintf("  Wages:        %.1f%%\n", mean(bw$sh_wages) * 100))
cat(sprintf("  Pass-through: %.1f%%\n", mean(bw$sh_passthru) * 100))
cat(sprintf("  Capital gains:%.1f%%\n", mean(bw$sh_kg) * 100))
cat(sprintf("  Dividends:    %.1f%%\n", mean(bw$sh_div) * 100))
cat(sprintf("  Interest:     %.1f%%\n", mean(bw$sh_interest) * 100))
cat(sprintf("  Other:        %.1f%%\n", mean(bw$sh_other) * 100))

# Full window average
cat("\n=== Full Window Average (2026-2055) ===\n")
cat(sprintf("Avg revenue/yr: $%.1fB\n", mean(results$total_revenue) / 1e9))
cat(sprintf("Avg # hit:      %.0f\n", mean(results$n_hit)))
cat(sprintf("\nRevenue-weighted AGI shares:\n"))
cat(sprintf("  Wages:        %.1f%%\n", mean(results$sh_wages) * 100))
cat(sprintf("  Pass-through: %.1f%%\n", mean(results$sh_passthru) * 100))
cat(sprintf("  Capital gains:%.1f%%\n", mean(results$sh_kg) * 100))
cat(sprintf("  Dividends:    %.1f%%\n", mean(results$sh_div) * 100))
cat(sprintf("  Interest:     %.1f%%\n", mean(results$sh_interest) * 100))
cat(sprintf("  Other:        %.1f%%\n", mean(results$sh_other) * 100))
