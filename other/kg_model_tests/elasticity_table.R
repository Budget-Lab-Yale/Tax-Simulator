#-------------------------------------------------------------------------------
# Compute the implied realization elasticity by simulation year for each
# scenario with a tax-rate change. Reports years 1, 5, 10, 30.
#
#   eta_t = log(R_S_t / R_B_t) / log(tau_S / tau_B)
#
# where R_S_t is the reform's realization-channel aggregate and R_B_t is the
# baseline aggregate. For combined regime+rate scenarios (rate_up_carryover,
# rate_up_deemed) the elasticity conflates the rate channel and the regime
# channel; flagged in the output.
#-------------------------------------------------------------------------------

this_script_dir = function() {
  args = commandArgs(trailingOnly = FALSE)
  fa = grep("^--file=", args, value = TRUE)
  if (length(fa) > 0) return(dirname(normalizePath(sub("^--file=", "", fa[1]))))
  if (!is.null(sys.frame(1)$ofile)) return(dirname(normalizePath(sys.frame(1)$ofile)))
  "other/kg_model_tests"
}
SCRIPT_DIR = this_script_dir()
source(file.path(SCRIPT_DIR, "kg_minimal.R"))

YEARS = 2026:2055   # extend horizon to cover year 30
report_years = c(1, 5, 10, 30)

scenarios = list(
  make_scenario("rate_up_5pp",       tau_S = 0.25, c_phi = 0,     delta_vanish = 1),
  make_scenario("rate_down_5pp",     tau_S = 0.15, c_phi = 0,     delta_vanish = 1),
  make_scenario("rate_up_carryover", tau_S = 0.25, c_phi = THETA, delta_vanish = 0, delta_route = 1),
  make_scenario("rate_up_deemed",    tau_S = 0.25, c_phi = 1,     delta_vanish = 0, delta_realize = 1)
)
names(scenarios) = sapply(scenarios, `[[`, "id")

baseline_cells = load_baseline_cells(YEARS, TAX_DATA_DIR)
omega = build_heir_matrix(AGE_MIN:AGE_MAX)
A     = build_aging_matrix(AGE_MIN:AGE_MAX)

cat("Running scenarios...\n")
results = lapply(scenarios, function(s) {
  cat("  ", s$id, "\n", sep = "")
  simulate_scenario(s, baseline_cells, YEARS, omega = omega, A = A)
})

#-------------------------------------------------------------------------------
# Compute year-level R_S and R_B aggregates
#-------------------------------------------------------------------------------

elast = bind_rows(lapply(scenarios, function(s) {
  cells = results[[s$id]]$cells
  totals = results[[s$id]]$totals

  # baseline R aggregate by year (same across scenarios)
  R_B_yr = cells %>% group_by(year) %>% summarise(R_B = sum(R_B, na.rm = TRUE), .groups = "drop")
  R_S_yr = cells %>% group_by(year) %>% summarise(dR = sum(dR, na.rm = TRUE), .groups = "drop")

  d = R_B_yr %>%
    left_join(R_S_yr, by = "year") %>%
    left_join(totals %>% select(year, R_death), by = "year") %>%
    mutate(R_S        = R_B + dR,
           R_S_total  = R_S + R_death / s$tau_S,   # gain-equivalent of forced realizations
           dlog_tau   = log(s$tau_S / BASELINE_TAU),
           eta_real   = log(R_S      / R_B) / dlog_tau,
           eta_total  = log(R_S_total / R_B) / dlog_tau,
           sim_year   = year - min(year) + 1,
           scenario   = s$id)

  d %>% select(scenario, year, sim_year, R_B, R_S, R_S_total, eta_real, eta_total)
}))

#-------------------------------------------------------------------------------
# Print a clean table
#-------------------------------------------------------------------------------

table_out = elast %>%
  filter(sim_year %in% report_years) %>%
  arrange(scenario, sim_year) %>%
  mutate(eta_real  = round(eta_real,  3),
         eta_total = round(eta_total, 3)) %>%
  select(scenario, sim_year, year, eta_real, eta_total)

cat("\nImplied realization elasticity by year\n")
cat("  eta_real  : log(R_S / R_B) / log(tau_S / tau_B), realization channel only\n")
cat("  eta_total : log((R_S + R_death/tau) / R_B) / log(tau_S / tau_B), incl. forced realizations\n")
cat("  (For pure-rate scenarios eta_real == eta_total since R_death = 0.)\n\n")

# pivot for easier reading
wide_real = elast %>%
  filter(sim_year %in% report_years) %>%
  select(scenario, sim_year, eta_real) %>%
  pivot_wider(names_from = sim_year, values_from = eta_real,
              names_prefix = "yr_") %>%
  mutate(across(starts_with("yr_"), ~ round(., 3)))
cat("eta_real (realization channel):\n")
print(as.data.frame(wide_real), row.names = FALSE)

wide_total = elast %>%
  filter(sim_year %in% report_years) %>%
  select(scenario, sim_year, eta_total) %>%
  pivot_wider(names_from = sim_year, values_from = eta_total,
              names_prefix = "yr_") %>%
  mutate(across(starts_with("yr_"), ~ round(., 3)))
cat("\neta_total (realization channel + R_death gain-equivalent):\n")
print(as.data.frame(wide_total), row.names = FALSE)

cat("\nNote: for combined rate+regime scenarios (rate_up_carryover, rate_up_deemed)\n",
    "the implied elasticity conflates the rate channel with the regime channel,\n",
    "so it is not interpretable as a pure tax-rate elasticity.\n", sep = "")
