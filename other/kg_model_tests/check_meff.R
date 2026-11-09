suppressPackageStartupMessages({
  library(dplyr)
})

state = readRDS('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/202605041906/rate_up_5pp/conventional/supplemental/kg_dynamics_state/2030.rds')
ct = state$cell_table

# Compare cell-mean m vs effective m = mG_record/G_B
comp = ct %>%
  mutate(m_eff = if_else(G_B > 0, mG_record / G_B, NA_real_),
         ratio = if_else(m > 0, m_eff / m, NA_real_))

cat("=== Per-cell comparison (year 2030) ===\n")
cat(sprintf("%4s %14s %14s %14s %14s %10s\n",
            "age", "G_B ($B)", "m_cell", "m_eff", "m * G_B ($M)", "mG_rec ($M)"))
for (i in seq_len(nrow(comp))) {
  r = comp[i,]
  cat(sprintf("%4d %14.1f %14.5f %14.5f %14.0f %10.0f  ratio=%.3f\n",
              r$age, r$G_B / 1e9, r$m, r$m_eff,
              r$m * r$G_B / 1e6, r$mG_record / 1e6, r$ratio))
}

# Aggregate G-weighted comparison
total_mGB        = sum(comp$m * comp$G_B,     na.rm=TRUE)
total_mG_record  = sum(comp$mG_record,        na.rm=TRUE)

cat("\n=== Aggregate over all age cells (year 2030) ===\n")
cat(sprintf("Sum( m * G_B    ) [cell-product, current bug]    = %12.1f $M\n", total_mGB / 1e6))
cat(sprintf("Sum( mG_record  ) [per-record correct]           = %12.1f $M\n", total_mG_record / 1e6))
cat(sprintf("Bug overstatement factor:                          %.3fx\n", total_mGB / total_mG_record))

# G-weighted average ratio
gw_ratio = sum(comp$m_eff * comp$G_B, na.rm=TRUE) / sum(comp$m * comp$G_B, na.rm=TRUE)
cat(sprintf("G-weighted m_eff / m:                              %.3f\n", gw_ratio))
