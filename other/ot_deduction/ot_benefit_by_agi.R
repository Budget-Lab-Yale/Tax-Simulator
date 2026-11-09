#-----------------------------------------------------------------------
# OT deduction benefit by AGI group, 2026 (static)
# benefit = repeal_ot income tax - baseline income tax  (= value of the OT
# deduction to the taxpayer). Counts a return as "benefiting" if that delta
# is positive (its tax falls because the deduction exists).
#-----------------------------------------------------------------------
suppressMessages({library(data.table)})

root <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ot_benefit_2026"
base_f   <- file.path(root, "baseline",  "static", "detail", "2026.csv")
repeal_f <- file.path(root, "repeal_ot", "static", "detail", "2026.csv")

keep <- c("id", "weight", "agi", "ot_ded", "liab_iit", "liab_iit_net", "liab_pr")
b <- fread(base_f,   select = keep)
r <- fread(repeal_f, select = keep)
setnames(b, keep, paste0(keep, "_b")); setnames(b, "id_b", "id")
setnames(r, keep, paste0(keep, "_r")); setnames(r, "id_r", "id")

d <- merge(b, r, by = "id")
cat("rows merged:", nrow(d), " (base", nrow(b), "repeal", nrow(r), ")\n")

# sanity: AGI should be identical (OT deduction is below-the-line)
cat("max |agi_b - agi_r|:", max(abs(d$agi_b - d$agi_r)), "\n")
cat("returns w/ ot_ded>0 (weighted):",
    format(sum(d$weight_b * (d$ot_ded_b > 0)), big.mark = ","), "\n")

# benefit = tax under repeal minus tax under baseline (income tax, net of refundable credits)
d[, benefit     := liab_iit_net_r - liab_iit_net_b]
d[, benefit_iit := liab_iit_r     - liab_iit_b]      # pre-refundable, cross-check
d[, agi_k := agi_b / 1000]

brks <- c(-Inf, 0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, Inf)
labs <- c("<0", "0-20k", "20-40k", "40-60k", "60-80k", "80-100k",
          "100-120k", "120-140k", "140-160k", "160-180k", "180-200k", "200k+")
d[, agi_grp := cut(agi_k, breaks = brks, labels = labs, right = FALSE)]

thresh <- 0.50  # dollars; avoid float noise
tab <- d[, .(
  returns_total       = sum(weight_b),
  returns_with_otded  = sum(weight_b * (ot_ded_b > 0)),
  returns_benefiting  = sum(weight_b * (benefit > thresh)),
  total_benefit       = sum(weight_b * benefit),
  total_otded_dollars = sum(weight_b * ot_ded_b)
), by = agi_grp][order(agi_grp)]

tot <- d[, .(agi_grp = "TOTAL",
  returns_total       = sum(weight_b),
  returns_with_otded  = sum(weight_b * (ot_ded_b > 0)),
  returns_benefiting  = sum(weight_b * (benefit > thresh)),
  total_benefit       = sum(weight_b * benefit),
  total_otded_dollars = sum(weight_b * ot_ded_b))]
tab <- rbind(tab, tot)

tab[, avg_benefit_per_benefiting := fifelse(returns_benefiting > 0,
                                            total_benefit / returns_benefiting, 0)]

# pretty print
pt <- copy(tab)
pt[, `:=`(
  returns_M            = round(returns_total / 1e6, 3),
  returns_w_otded_M    = round(returns_with_otded / 1e6, 3),
  returns_benefiting_M = round(returns_benefiting / 1e6, 3),
  total_benefit_B      = round(total_benefit / 1e9, 3),
  avg_benefit_usd      = round(avg_benefit_per_benefiting, 0)
)]
pt <- pt[, .(agi_grp, returns_M, returns_w_otded_M, returns_benefiting_M,
             total_benefit_B, avg_benefit_usd)]
cat("\n================ OT deduction benefit by AGI group, 2026 (static) ================\n")
print(pt, nrows = 100)
cat("\n(returns in millions; total_benefit_B in $billions; avg_benefit_$ per benefiting return)\n")

outdir <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/ot_deduction"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
fwrite(tab, file.path(outdir, "ot_benefit_by_agi_2026.csv"))
cat("\nwrote:", file.path(outdir, "ot_benefit_by_agi_2026.csv"), "\n")
