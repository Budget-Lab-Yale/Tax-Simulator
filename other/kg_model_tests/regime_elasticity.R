#-------------------------------------------------------------------------------
# Implied within-regime realization elasticity. For each background regime
# (step-up, carryover, deemed) and (under carryover only) each value of the
# bequest motive theta, perturb tau from 0.20 to 0.25 *holding the regime
# fixed* and measure how realizations respond.
#
# Under the microfoundation, the within-regime semi-elasticity is
#
#   d log r_S / d tau   =   - eta * (1 - M(c))
#
# so eta_within is determined by the bracket (1 - M(c)), which depends on
# c, lambda_r, beta, and the cell's mortality path. theta enters only
# under carryover, where c = theta.
#
# Reports two flavors:
#   eta_real  = elasticity of the realization channel only (R_B + dR)
#   eta_total = elasticity of total realizations including R_death gain-equiv
#               under deemed (R_S + R_death / tau)
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

YEARS     = 2026:2085           # 60 years to ensure steady state
TARGET_YR = min(YEARS) + 29     # year 30
TAU_LOW   = 0.20
TAU_HIGH  = 0.25
DLOG_TAU  = log(TAU_HIGH / TAU_LOW)

# (regime, c_phi, routing) configurations to test
# theta only varies under carryover; step-up and deemed have fixed c
config_list = list(
  list(label = "step_up",            c = 0,    dv = 1, dr = 0, dx = 0),
  list(label = "carryover_th=0.00",  c = 0.00, dv = 0, dr = 1, dx = 0),
  list(label = "carryover_th=0.25",  c = 0.25, dv = 0, dr = 1, dx = 0),
  list(label = "carryover_th=0.50",  c = 0.50, dv = 0, dr = 1, dx = 0),
  list(label = "carryover_th=0.75",  c = 0.75, dv = 0, dr = 1, dx = 0),
  list(label = "carryover_th=1.00",  c = 1.00, dv = 0, dr = 1, dx = 0),
  list(label = "deemed",             c = 1,    dv = 0, dr = 0, dx = 1)
)

baseline_cells = load_baseline_cells(YEARS, TAX_DATA_DIR)
omega = build_heir_matrix(AGE_MIN:AGE_MAX)
A     = build_aging_matrix(AGE_MIN:AGE_MAX)

run_one = function(cfg, tau_val, label_suffix) {
  s = make_scenario(paste0(cfg$label, "_", label_suffix),
                    tau_S = tau_val, c_phi = cfg$c,
                    delta_vanish = cfg$dv,
                    delta_route = cfg$dr,
                    delta_realize = cfg$dx)
  out = simulate_scenario(s, baseline_cells, YEARS,
                          eta = ETA, omega = omega, A = A)
  R_B_yr30 = out$cells %>% filter(year == TARGET_YR) %>%
    summarise(x = sum(R_B)) %>% pull(x)
  tot = out$totals %>% filter(year == TARGET_YR)
  list(R_B = R_B_yr30, dR = tot$dR_total, R_death = tot$R_death)
}

cat("Computing within-regime elasticities at year 30...\n")
cat("(eta =", ETA, ", tau perturbation 0.20 -> 0.25)\n\n")

rows = list()
for (cfg in config_list) {
  cat(sprintf("  config: %-22s (c = %.2f)\n", cfg$label, cfg$c))

  base = run_one(cfg, TAU_LOW,  "low")
  hike = run_one(cfg, TAU_HIGH, "high")

  R_S_base = base$R_B + base$dR
  R_S_hike = hike$R_B + hike$dR
  R_total_base = R_S_base + base$R_death / TAU_LOW
  R_total_hike = R_S_hike + hike$R_death / TAU_HIGH

  eta_real  = log(R_S_hike    / R_S_base)    / DLOG_TAU
  eta_total = log(R_total_hike / R_total_base) / DLOG_TAU

  rows[[length(rows) + 1]] = tibble(
    config       = cfg$label,
    c_phi        = cfg$c,
    eta_real     = eta_real,
    eta_total    = eta_total
  )
}
result = bind_rows(rows)

eta_step_up = result %>% filter(config == "step_up") %>% pull(eta_total)

result = result %>%
  mutate(discount_to_step_up = round(eta_total / eta_step_up, 3),
         eta_real            = round(eta_real,  3),
         eta_total           = round(eta_total, 3))

cat("\n=== Within-regime elasticity at year 30 (eta =", ETA, ") ===\n")
print(as.data.frame(result), row.names = FALSE)

cat("\nLegend:\n")
cat("  eta_real            : elasticity of realization-channel R_S only\n")
cat("  eta_total           : elasticity of (R_S + R_death/tau) total realizations\n")
cat("  discount_to_step_up : eta_total / eta_step_up (TPC heuristic = 0.50 under deemed)\n")
