#-------------------------------------------------------------------------------
# Sensitivity sweep helper. Run from repo root, e.g.:
#   Rscript other/kg_model_tests/sensitivity.R carryover theta
#   Rscript other/kg_model_tests/sensitivity.R rate_up_5pp eta
#   Rscript other/kg_model_tests/sensitivity.R deemed lambda_r
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
})

this_script_dir = function() {
  args = commandArgs(trailingOnly = FALSE)
  fa = grep("^--file=", args, value = TRUE)
  if (length(fa) > 0) return(dirname(normalizePath(sub("^--file=", "", fa[1]))))
  if (!is.null(sys.frame(1)$ofile)) return(dirname(normalizePath(sys.frame(1)$ofile)))
  "other/kg_model_tests"
}
SCRIPT_DIR = this_script_dir()
source(file.path(SCRIPT_DIR, "kg_minimal.R"))

OUT_DIR = "other/kg_model_tests/output"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

#-------------------------------------------------------------------------------
# Parse args
#-------------------------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript sensitivity.R <scenario_id> <param>\n",
       "  scenario_id  one of: rate_up_5pp, rate_down_5pp, carryover, deemed,\n",
       "               rate_up_carryover, rate_up_deemed\n",
       "  param        one of: eta, theta, lambda_r, beta, tau_S")
}
scen_id = args[1]
param   = args[2]

stopifnot(param %in% c("eta", "theta", "lambda_r", "beta", "tau_S"))

#-------------------------------------------------------------------------------
# Build the scenario object
#-------------------------------------------------------------------------------

# c_phi default per regime: step-up = 0, carryover = THETA, deemed = 1
scen_table = list(
  rate_up_5pp        = list(tau_S = 0.25, c_phi = 0,     dv = 1, dr = 0, dx = 0),
  rate_down_5pp      = list(tau_S = 0.15, c_phi = 0,     dv = 1, dr = 0, dx = 0),
  carryover          = list(tau_S = 0.20, c_phi = THETA, dv = 0, dr = 1, dx = 0),
  deemed             = list(tau_S = 0.20, c_phi = 1,     dv = 0, dr = 0, dx = 1),
  rate_up_carryover  = list(tau_S = 0.25, c_phi = THETA, dv = 0, dr = 1, dx = 0),
  rate_up_deemed     = list(tau_S = 0.25, c_phi = 1,     dv = 0, dr = 0, dx = 1)
)
if (!scen_id %in% names(scen_table)) stop("unknown scenario_id: ", scen_id)
sp = scen_table[[scen_id]]

#-------------------------------------------------------------------------------
# Define parameter sweep grid (-50%, -25%, default, +25%, +50%)
#-------------------------------------------------------------------------------

defaults = list(eta = ETA, theta = THETA, lambda_r = LAMBDA_R, beta = BETA,
                tau_S = sp$tau_S)
mults    = c(0.5, 0.75, 1.0, 1.25, 1.5)
grid     = defaults[[param]] * mults
labs     = sprintf("%.0f%%", mults * 100)

if (param == "tau_S") {
  grid = sp$tau_S + c(-0.04, -0.02, 0, 0.02, 0.04)
  labs = sprintf("%.2f", grid)
} else if (param == "beta") {
  grid = c(0.92, 0.94, 0.96, 0.97, 0.98)
  labs = sprintf("%.2f", grid)
}

cat("Sweeping", param, "for scenario", scen_id, "\n")
cat("Grid:", paste(round(grid, 4), collapse = ", "), "\n\n")

#-------------------------------------------------------------------------------
# Load baselines and pre-build matrices
#-------------------------------------------------------------------------------

baseline_cells = load_baseline_cells(YEARS, TAX_DATA_DIR)
omega = build_heir_matrix(AGE_MIN:AGE_MAX)
A     = build_aging_matrix(AGE_MIN:AGE_MAX)

#-------------------------------------------------------------------------------
# Sweep
#-------------------------------------------------------------------------------

paths = list()
for (i in seq_along(grid)) {
  v = grid[i]

  # Theta sweep updates c_phi only for non-step-up regimes
  c_phi_use = if (param == "theta" && sp$dr > 0) v else
              if (param == "theta") sp$c_phi else
              sp$c_phi

  scenario = make_scenario(
    id = paste0(scen_id, "_", labs[i]),
    tau_S         = if (param == "tau_S") v else sp$tau_S,
    c_phi         = c_phi_use,
    delta_vanish  = sp$dv,
    delta_route   = sp$dr,
    delta_realize = sp$dx
  )

  res = simulate_scenario(
    scenario, baseline_cells, YEARS,
    eta      = if (param == "eta")      v else ETA,
    beta     = if (param == "beta")     v else BETA,
    lambda_r = if (param == "lambda_r") v else LAMBDA_R,
    omega    = omega, A = A
  )

  paths[[i]] = res$totals %>%
    mutate(level = labs[i],
           value = v) %>%
    select(level, value, year, dT_total, dG_total)
}

sweep_df = bind_rows(paths) %>%
  mutate(level = factor(level, levels = labs))

#-------------------------------------------------------------------------------
# Plot fan chart
#-------------------------------------------------------------------------------

p = sweep_df %>%
  ggplot(aes(year, dT_total / 1e9, color = level, group = level)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.7) +
  scale_color_brewer(palette = "RdBu", direction = -1) +
  labs(title = paste0("Sensitivity of revenue impact to ", param,
                      " -- scenario: ", scen_id),
       y = "Delta T  ($B)", x = NULL,
       color = paste0(param))
out_pdf = file.path(OUT_DIR, paste0("kg_sensitivity_", scen_id, "_", param, ".pdf"))
ggsave(out_pdf, p, width = 9, height = 5.5)

#-------------------------------------------------------------------------------
# Summary
#-------------------------------------------------------------------------------

cat("Cumulative dT by level ($B):\n")
sweep_df %>%
  group_by(level) %>%
  summarise(`dT cumulative ($B)` = round(sum(dT_total) / 1e9, 1),
            .groups = "drop") %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\nWrote ", out_pdf, "\n", sep = "")
