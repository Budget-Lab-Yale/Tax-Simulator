#-------------------------------------------------------------------------------
# Calibrate eta (microfoundation behavioral curvature) to hit a target
# permanent realization elasticity under step-up. Reports implied eta and
# year-by-year elasticity profiles for the calibrated value, then verifies
# step-up identity.
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

YEARS  = 2026:2085   # 60 years to ensure steady state
TARGET = -0.6
ANCHOR_TAU_S = 0.25  # 5pp hike from 0.20

baseline_cells = load_baseline_cells(YEARS, TAX_DATA_DIR)
omega = build_heir_matrix(AGE_MIN:AGE_MAX)
A     = build_aging_matrix(AGE_MIN:AGE_MAX)

eta_at_year_30 = function(eta_val) {
  s = make_scenario("calib", tau_S = ANCHOR_TAU_S, c_phi = 0, delta_vanish = 1)
  out = simulate_scenario(s, baseline_cells, YEARS,
                          eta = eta_val, omega = omega, A = A)
  yr30 = min(YEARS) + 29
  cells_30 = out$cells %>% filter(year == yr30) %>%
    summarise(R_B = sum(R_B), dR = sum(dR))
  log((cells_30$R_B + cells_30$dR) / cells_30$R_B) / log(ANCHOR_TAU_S / BASELINE_TAU)
}

cat("Calibrating eta for step-up rate-hike scenario\n")
cat("Target: eta_30 =", TARGET, "at tau: 0.20 -> 0.25\n\n")

# Sweep eta on a coarse grid to bracket the target
eta_grid = c(1, 3, 5, 8, 12, 20)
cat("Sweep:\n")
res = sapply(eta_grid, function(e) {
  v = eta_at_year_30(e)
  cat(sprintf("  eta = %5.2f  ->  eta_30 = %.3f\n", e, v))
  v
})

# Bisect
below = which(res > TARGET)   # less negative
above = which(res < TARGET)   # more negative
if (length(below) == 0 || length(above) == 0) {
  stop("Grid does not bracket target. Extend eta_grid.")
}

i_lo = max(below); i_hi = min(above)
e_lo = eta_grid[i_lo]; e_hi = eta_grid[i_hi]

# Bisection
for (k in 1:25) {
  e_mid = (e_lo + e_hi) / 2
  v_mid = eta_at_year_30(e_mid)
  if (abs(v_mid - TARGET) < 1e-3) break
  if (v_mid > TARGET) e_lo = e_mid else e_hi = e_mid
}

eta_star = (e_lo + e_hi) / 2
final = eta_at_year_30(eta_star)
cat(sprintf("\nCalibrated eta = %.4f  (eta_30 = %.4f)\n", eta_star, final))
cat("\nUpdate ETA in kg_minimal.R to this value.\n")

#-------------------------------------------------------------------------------
# Profile of eta at this eta_star: year 1, 5, 10, 30, 60
#-------------------------------------------------------------------------------

s = make_scenario("calib", tau_S = ANCHOR_TAU_S, c_phi = 0, delta_vanish = 1)
out = simulate_scenario(s, baseline_cells, YEARS,
                         eta = eta_star, omega = omega, A = A)

profile = out$cells %>%
  group_by(year) %>%
  summarise(R_B = sum(R_B), dR = sum(dR), .groups = "drop") %>%
  mutate(R_S = R_B + dR,
         eta = log(R_S / R_B) / log(ANCHOR_TAU_S / BASELINE_TAU),
         sim_year = year - min(year) + 1) %>%
  filter(sim_year %in% c(1, 5, 10, 30, 60)) %>%
  select(sim_year, eta) %>%
  mutate(eta = round(eta, 3))

cat("\nElasticity profile by simulation year (rate_up_5pp under step-up):\n")
print(as.data.frame(profile), row.names = FALSE)
