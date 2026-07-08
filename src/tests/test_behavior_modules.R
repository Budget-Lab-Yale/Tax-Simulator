#---------------------------------------------------------------
# Unit tests for behavioral feedback modules
#
# Tests elasticity assignment and application logic using
# synthetic data — no microdata or full simulation required.
#---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)

# Source the behavior helper functions
source("src/sim/behavior.R")

n_passed = 0
n_failed = 0

assert = function(desc, condition) {
  if (isTRUE(condition)) {
    cat(sprintf("  PASS: %s\n", desc))
    n_passed <<- n_passed + 1
  } else {
    cat(sprintf("  FAIL: %s\n", desc))
    n_failed <<- n_failed + 1
  }
}


# =============================================================
# Test: apply_mtr_elasticity with semi-elasticity
# =============================================================

cat("\nTest: apply_mtr_elasticity semi-elasticity\n")

tax_units = tibble(
  id   = 1:3,
  year = 2026,
  kg_lt = c(100000, 50000, 0),
  e_kg_lt = rep(-0.62 / 0.238, 3),
  e_kg_lt_type = rep("semi", 3)
)

baseline_mtrs = tibble(
  id   = 1:3,
  year = 2026,
  mtr_kg_lt = c(0.238, 0.238, 0.238)
)

static_mtrs = tibble(
  id   = 1:3,
  year = 2026,
  mtr_kg_lt = c(0.338, 0.338, 0.238)  # 10pp surtax for first two, no change for third
)

result = tax_units %>%
  apply_mtr_elasticity("kg_lt", baseline_mtrs, static_mtrs, 3)

assert(
  "Positive CG reduced by surtax",
  result$kg_lt[1] < 100000
)

assert(
  "Zero CG stays zero",
  result$kg_lt[3] == 0
)

assert(
  "No MTR change means no adjustment",
  abs(result$kg_lt[3] - 0) < 0.01
)

# Verify semi-elasticity formula: pct_chg = exp((mtr - mtr_baseline) * e) - 1
expected_pct = exp((0.338 - 0.238) * (-0.62 / 0.238)) - 1
expected_kg = 100000 * (1 + expected_pct)
assert(
  "Semi-elasticity formula matches expected value",
  abs(result$kg_lt[1] - expected_kg) < 0.01
)


# =============================================================
# Test: apply_mtr_elasticity caps at max_adj
# =============================================================

cat("\nTest: apply_mtr_elasticity max_adj capping\n")

tax_units_extreme = tibble(
  id   = 1,
  year = 2026,
  kg_lt = 100000,
  e_kg_lt = -10,  # extreme elasticity
  e_kg_lt_type = "semi"
)

baseline_extreme = tibble(id = 1, year = 2026, mtr_kg_lt = 0.1)
static_extreme   = tibble(id = 1, year = 2026, mtr_kg_lt = 0.5)

result_capped = tax_units_extreme %>%
  apply_mtr_elasticity("kg_lt", baseline_extreme, static_extreme, 1)

# With max_adj = 1, pct_chg is capped at [-1, 1], so kg_lt in [0, 200000]
assert(
  "Capped at max_adj = 1 (no more than 100% increase)",
  result_capped$kg_lt[1] <= 200000
)
assert(
  "Capped at max_adj = 1 (no less than -100% decrease)",
  result_capped$kg_lt[1] >= 0
)


# =============================================================
# Test: apply_mtr_elasticity net-of-tax elasticity type
# =============================================================

cat("\nTest: apply_mtr_elasticity net-of-tax type\n")

tax_units_not = tibble(
  id   = 1,
  year = 2026,
  kg_lt = 100000,
  e_kg_lt = -0.5,
  e_kg_lt_type = "netoftax"
)

baseline_not = tibble(id = 1, year = 2026, mtr_kg_lt = 0.238)
static_not   = tibble(id = 1, year = 2026, mtr_kg_lt = 0.338)

result_not = tax_units_not %>%
  apply_mtr_elasticity("kg_lt", baseline_not, static_not, 3)

# net-of-tax: pct_chg = e * ((1 - mtr) / (1 - mtr_baseline) - 1)
expected_not_pct = -0.5 * ((1 - 0.338) / (1 - 0.238) - 1)
expected_not_kg = 100000 * (1 + expected_not_pct)
assert(
  "Net-of-tax elasticity formula correct",
  abs(result_not$kg_lt[1] - expected_not_kg) < 0.01
)


# =============================================================
# Test: CG behavior module file consistency
# =============================================================

cat("\nTest: CG behavior module consistency\n")

kg_files = list.files("config/scenarios/behavior/kg", pattern = "\\.R$", full.names = TRUE)

for (f in kg_files) {
  code = readLines(f)
  # Every CG module should reference kg_lt
  has_kg_lt = any(grepl("kg_lt", code))
  assert(
    sprintf("%s references kg_lt", basename(f)),
    has_kg_lt
  )

  # Every CG module should use semi elasticity type
  has_semi = any(grepl("semi", code))
  assert(
    sprintf("%s uses semi elasticity type", basename(f)),
    has_semi
  )
}


# =============================================================
# Summary
# =============================================================

cat(sprintf("\n%d passed, %d failed\n", n_passed, n_failed))
if (n_failed > 0) {
  stop(sprintf("%d test(s) failed", n_failed))
}
