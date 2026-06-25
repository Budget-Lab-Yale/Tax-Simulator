#-------------------------------------------------------------------------------
# analyze_bounding_detail.R   (RUN VIA SBATCH -- reads ~11GB of detail)
#
# Mechanical-channel diagnostics for the s x M bounding sweep. Per (M, s, year)
# reads only {weight, net_worth, D_alloc, wealth_haircut} from the CONVENTIONAL
# detail and reports: weighted net-worth stock ($T), cumulative wealth drained
# into the base ($B, = sum w*D_alloc), weighted-mean & max |haircut|, and the
# clamp incidence at fmax=0.9 (raw record count + weighted population). The clamp
# numbers flag s=1 as a CONSERVATIVE bound (the linear recurrence would drain
# more than fmax permits per record).
#-------------------------------------------------------------------------------
suppressMessages(library(data.table))

ROOT  <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
OUT   <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
FMAX  <- 0.9
SEL   <- c("weight", "net_worth", "D_alloc", "wealth_haircut")

legs <- rbind(
  data.table(M = "identity", vintage = "cgcarry_bound_identity",
             scen = paste0("cgcarry_s", c("25","50","75","100")), s = c(.25,.5,.75,1)),
  data.table(M = "uniform",  vintage = "cgcarry_bound_uniform",
             scen = paste0("cgcarry_s", c("25","50","75","100")), s = c(.25,.5,.75,1))
)

rows <- list()
for (i in seq_len(nrow(legs))) {
  r  <- legs[i]
  dd <- file.path(ROOT, r$vintage, r$scen, "conventional", "detail")
  if (!dir.exists(dd)) { cat("MISSING dir:", dd, "\n"); next }
  yrs <- sort(as.integer(gsub("\\.csv$", "", list.files(dd, pattern = "\\.csv$"))))
  for (y in yrs) {
    p   <- file.path(dd, paste0(y, ".csv"))
    hdr <- tryCatch(names(fread(p, nrows = 0)), error = function(e) character(0))
    use <- intersect(SEL, hdr)
    if (!all(c("weight","net_worth") %in% use)) next
    dt  <- tryCatch(fread(p, select = use), error = function(e) NULL); if (is.null(dt)) next
    hc  <- if ("wealth_haircut" %in% use) dt$wealth_haircut else rep(0, nrow(dt))
    da  <- if ("D_alloc" %in% use) dt$D_alloc else rep(0, nrow(dt))
    nz  <- abs(hc) > 1e-12
    rows[[paste(r$M, r$scen, y)]] <- data.table(
      M = r$M, s = r$s, scen = r$scen, year = y,
      nw_total_T   = sum(dt$weight * dt$net_worth, na.rm = TRUE) / 1e12,
      drain_B      = sum(dt$weight * da,           na.rm = TRUE) / 1e9,
      hc_mean_nz   = if (any(nz)) weighted.mean(abs(hc[nz]), dt$weight[nz]) else 0,
      hc_max       = max(abs(hc), na.rm = TRUE),
      n_clamped    = sum(abs(hc) >= FMAX - 1e-9, na.rm = TRUE),
      pop_clamped_M = sum(dt$weight[abs(hc) >= FMAX - 1e-9], na.rm = TRUE) / 1e6
    )
  }
  cat("done:", r$M, r$scen, "\n")
}
drain <- rbindlist(rows, fill = TRUE)
fwrite(drain, file.path(OUT, "bounding_drain.csv"))
cat("wrote bounding_drain.csv:", nrow(drain), "rows\n")
print(drain[year == max(year), .(M, s, nw_total_T, drain_B, hc_max, n_clamped, pop_clamped_M)])
