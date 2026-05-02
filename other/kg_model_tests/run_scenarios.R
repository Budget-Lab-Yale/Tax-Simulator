#-------------------------------------------------------------------------------
# Driver script: load tax-data once, run canonical scenarios, emit plots.
# Run from repo root: Rscript other/kg_model_tests/run_scenarios.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

this_script_dir = function() {
  args = commandArgs(trailingOnly = FALSE)
  fa = grep("^--file=", args, value = TRUE)
  if (length(fa) > 0) return(dirname(normalizePath(sub("^--file=", "", fa[1]))))
  if (!is.null(sys.frame(1)$ofile)) return(dirname(normalizePath(sys.frame(1)$ofile)))
  "other/kg_model_tests"
}
source(file.path(this_script_dir(), "kg_minimal.R"))

OUT_DIR = "other/kg_model_tests/output"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

#-------------------------------------------------------------------------------
# Define scenarios
#-------------------------------------------------------------------------------

scenarios = list(
  make_scenario("baseline_check",     tau_S = 0.20, c_phi = 0,     delta_vanish = 1),
  make_scenario("rate_up_5pp",        tau_S = 0.25, c_phi = 0,     delta_vanish = 1),
  make_scenario("rate_down_5pp",      tau_S = 0.15, c_phi = 0,     delta_vanish = 1),
  make_scenario("carryover",          tau_S = 0.20, c_phi = THETA,
                delta_vanish = 0, delta_route = 1),
  make_scenario("deemed",             tau_S = 0.20, c_phi = 1,
                delta_vanish = 0, delta_realize = 1),
  make_scenario("rate_up_carryover",  tau_S = 0.25, c_phi = THETA,
                delta_vanish = 0, delta_route = 1),
  make_scenario("rate_up_deemed",     tau_S = 0.25, c_phi = 1,
                delta_vanish = 0, delta_realize = 1)
)
names(scenarios) = sapply(scenarios, `[[`, "id")

#-------------------------------------------------------------------------------
# Load baseline cells once
#-------------------------------------------------------------------------------

baseline_cells = load_baseline_cells(YEARS, TAX_DATA_DIR)

#-------------------------------------------------------------------------------
# Run all scenarios
#-------------------------------------------------------------------------------

omega = build_heir_matrix(AGE_MIN:AGE_MAX)
A     = build_aging_matrix(AGE_MIN:AGE_MAX)

cat("Running scenarios...\n")
results = lapply(scenarios, function(s) {
  cat("  ", s$id, "\n", sep = "")
  simulate_scenario(s, baseline_cells, YEARS,
                    omega = omega, A = A)
})

cells_all  = bind_rows(lapply(results, `[[`, "cells"))
totals_all = bind_rows(lapply(results, `[[`, "totals"))

#-------------------------------------------------------------------------------
# Verification assertions
#-------------------------------------------------------------------------------

cat("\nVerification:\n")

# 1. Identity reproduction
b = totals_all %>% filter(scenario == "baseline_check")
stopifnot(max(abs(b$dG_total))  < 1e-6)
stopifnot(max(abs(b$dT_total))  < 1e-6)
cat("  [PASS] baseline_check has dG = dT = 0 throughout\n")

# 4. Deemed first-year revenue floor
deemed = totals_all %>% filter(scenario == "deemed", year == min(YEARS))
expected = baseline_cells[[as.character(min(YEARS))]] %>%
  summarise(x = sum(m * G_B) * 0.20) %>% pull(x)
stopifnot(abs(deemed$R_death - expected) / max(1, expected) < 1e-6)
cat("  [PASS] deemed first-year R_death matches sum(m * G_B * tau)\n")

# 5. Heir matrix row stochasticity already asserted in build_heir_matrix

cat("\n")

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------

theme_set(theme_minimal(base_size = 11))

scen_levels = c("baseline_check", "rate_down_5pp", "rate_up_5pp",
                "carryover", "deemed",
                "rate_up_carryover", "rate_up_deemed")
totals_all = totals_all %>%
  mutate(scenario = factor(scenario, levels = scen_levels))
cells_all = cells_all %>%
  mutate(scenario = factor(scenario, levels = scen_levels))

# 1. Revenue paths
p1 = totals_all %>%
  filter(scenario != "baseline_check") %>%
  ggplot(aes(year, dT_total / 1e9, color = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.7) +
  labs(title = "Reform vs baseline revenue impact",
       subtitle = "Single-asset minimal kg model",
       y = "Delta T  ($B)", x = NULL, color = NULL)
ggsave(file.path(OUT_DIR, "kg_revenue_paths.pdf"), p1, width = 9, height = 5.5)

# 2. Stock paths
p2 = totals_all %>%
  filter(scenario != "baseline_check") %>%
  ggplot(aes(year, dG_total / 1e12, color = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.7) +
  labs(title = "Policy-induced delta in unrealized gain stock",
       subtitle = "Total across all age cells",
       y = "Delta G  ($T)", x = NULL, color = NULL)
ggsave(file.path(OUT_DIR, "kg_stock_paths.pdf"), p2, width = 9, height = 5.5)

# 3. Age profile at terminal year
T_terminal = max(YEARS)
p3 = cells_all %>%
  filter(scenario != "baseline_check", year == T_terminal) %>%
  ggplot(aes(age, dG / 1e9, fill = scenario)) +
  geom_col() +
  facet_wrap(~ scenario, scales = "free_y") +
  guides(fill = "none") +
  labs(title = paste0("Age profile of Delta G at ", T_terminal),
       y = "Delta G  ($B)", x = "Age")
ggsave(file.path(OUT_DIR, "kg_age_profile.pdf"), p3, width = 11, height = 7)

# 4. Carryover decomposition
co = cells_all %>%
  filter(scenario == "carryover") %>%
  group_by(year) %>%
  summarise(survivor   = sum(delta_surv),
            inheritance = sum(delta_inh),
            total      = sum(delta_surv + delta_inh),
            .groups = "drop") %>%
  pivot_longer(c(survivor, inheritance, total), names_to = "channel", values_to = "value")
p4 = co %>%
  ggplot(aes(year, value / 1e12, color = channel, linetype = channel)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.7) +
  scale_linetype_manual(values = c(survivor = "11", inheritance = "11", total = "solid")) +
  labs(title = "Carryover scenario: where Delta G comes from",
       subtitle = "Channel decomposition of next-period Delta G inflow",
       y = "Delta G inflow  ($T)", x = NULL,
       color = NULL, linetype = NULL)
ggsave(file.path(OUT_DIR, "kg_decomposition.pdf"), p4, width = 9, height = 5.5)

#-------------------------------------------------------------------------------
# Console summary
#-------------------------------------------------------------------------------

cat("Cumulative 25-year revenue (delta_T summed over years), $B:\n")
totals_all %>%
  group_by(scenario) %>%
  summarise(`dT cumulative ($B)` = round(sum(dT_total) / 1e9, 1),
            `dT_real cumulative ($B)` = round(sum(dT_real) / 1e9, 1),
            `R_death cumulative ($B)` = round(sum(R_death) / 1e9, 1),
            .groups = "drop") %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\nWrote plots to ", OUT_DIR, "\n", sep = "")
