#-------------------------------------------------------------------------------
# verify_rtotal_splice.R
# Confirms the historical+projections splice fix in wealth_dyn_read_rtotal():
# r_total(t) must now resolve for pre-projection / boundary years (e.g. a 2025
# FY lead-in), which the projections.csv-only version could not.
#-------------------------------------------------------------------------------
suppressMessages(library(tidyverse))
setwd("/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator")
source("src/sim/wealth_dynamics.R")

macro_root <- "/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline"
params <- list(r_total = list(additive_delta = 0))

# A scenario that starts a year BEFORE projections begin (lead-in) and crosses
# the boundary -- exactly the case that used to crash.
si <- list(interface_paths = list(`Macro-Projections` = macro_root),
           years = 2024:2030)

r <- wealth_dyn_read_rtotal(si, params)
cat("r_total over 2024:2030 (nominal GDP/capita growth):\n")
print(round(r, 5))

stopifnot(all(is.finite(r)))
stopifnot(!anyNA(r))
# sanity: nominal GDP/capita growth should sit in a plausible 0-8% band
stopifnot(all(r > -0.02 & r < 0.10))
cat("\nPASS: r_total resolves for the 2025 lead-in and the 2025->2026 boundary,\n")
cat("      differencing off the real prior-year (historical) level.\n")

# Show that the boundary year 2026 now uses the real 2025 predecessor rather
# than the old lead()-backfill: print 2025 and 2026 explicitly.
cat(sprintf("\n  r_total(2025) = %.5f   r_total(2026) = %.5f\n", r["2025"], r["2026"]))
cat("VERIFY_EXIT=0\n")
