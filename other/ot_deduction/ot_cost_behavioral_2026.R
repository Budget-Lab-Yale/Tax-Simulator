#-----------------------------------------------------------------------
# OT deduction: STATIC vs CONVENTIONAL budget cost by AGI group, 2026.
# Adopt framing: baseline = NO OT deduction; ot_cl = current-law deduction
# (+ ot/france_1yr behavioral response, full phase-in in 2026).
#
# Cost of the provision = revenue WITHOUT deduction - revenue WITH deduction
#   = liab(baseline) - liab(ot_cl)   [positive = revenue lost = cost].
# Static  : ot_cl/static       (mechanical, OT held at baseline)
# Conv    : ot_cl/conventional (france-induced extra OT, then deducted)
# Behavioral piece = conventional - static.
#-----------------------------------------------------------------------
suppressMessages({library(data.table)})

root <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ot_behavior_2026"
base_f  <- file.path(root, "baseline", "static",       "detail", "2026.csv")  # no deduction
stat_f  <- file.path(root, "ot_cl",    "static",       "detail", "2026.csv")  # deduction, OT fixed
conv_f  <- file.path(root, "ot_cl",    "conventional", "detail", "2026.csv")  # deduction + behavior

keep <- c("id","weight","agi","ot_ded","liab_iit_net","liab_pr")
b <- fread(base_f, select = keep); setnames(b, keep, paste0(keep,"_b")); setnames(b,"id_b","id")
s <- fread(stat_f, select = keep); setnames(s, keep, paste0(keep,"_s")); setnames(s,"id_s","id")
c <- fread(conv_f, select = keep); setnames(c, keep, paste0(keep,"_c")); setnames(c,"id_c","id")

d <- merge(merge(b, s, by="id"), c, by="id")
cat("rows:", nrow(d), "\n")
w <- d$weight_b

# diagnostic: induced response shows up as a bigger deductible base (ot_ded)
ded_s <- sum(w*d$ot_ded_s); ded_c <- sum(w*d$ot_ded_c)
cat("OT deduction claimed, weighted ($B)  static:", round(ded_s/1e9,2),
    "  conventional:", round(ded_c/1e9,2),
    "  induced:", round((ded_c-ded_s)/1e9,2),
    sprintf("  (+%.1f%%)\n", 100*(ded_c-ded_s)/ded_s))

# cost = baseline (no ded) minus deduction-on scenario  (positive = revenue lost)
d[, cost_static_iit := liab_iit_net_b - liab_iit_net_s]
d[, cost_conv_iit   := liab_iit_net_b - liab_iit_net_c]
# total federal = income tax (net) + payroll
d[, cost_conv_total := (liab_iit_net_b + liab_pr_b) - (liab_iit_net_c + liab_pr_c)]
d[, agi_k := agi_b/1000]

brks <- c(-Inf,0,20,40,60,80,100,120,140,160,180,200,Inf)
labs <- c("<0","0-20k","20-40k","40-60k","60-80k","80-100k",
          "100-120k","120-140k","140-160k","160-180k","180-200k","200k+")
d[, agi_grp := cut(agi_k, breaks=brks, labels=labs, right=FALSE)]

agg <- function(dt) dt[, .(
  returns_benefiting_static = sum(weight_b*(cost_static_iit > 0.5)),
  cost_static_iit   = sum(weight_b*cost_static_iit),
  cost_conv_iit     = sum(weight_b*cost_conv_iit),
  cost_conv_total   = sum(weight_b*cost_conv_total)
), by=agi_grp]
tab <- agg(d)[order(agi_grp)]
tot <- d[, .(agi_grp="TOTAL",
  returns_benefiting_static = sum(weight_b*(cost_static_iit > 0.5)),
  cost_static_iit = sum(weight_b*cost_static_iit),
  cost_conv_iit   = sum(weight_b*cost_conv_iit),
  cost_conv_total = sum(weight_b*cost_conv_total))]
tab <- rbind(tab, tot)
tab[, behavioral_iit := cost_conv_iit - cost_static_iit]

pt <- copy(tab)
pt[, `:=`(
  returns_benefiting_M = round(returns_benefiting_static/1e6,3),
  cost_static_B        = round(cost_static_iit/1e9,3),
  cost_conv_iit_B      = round(cost_conv_iit/1e9,3),
  cost_conv_total_B    = round(cost_conv_total/1e9,3),
  behavioral_iit_B     = round(behavioral_iit/1e9,3)
)]
pt <- pt[, .(agi_grp, returns_benefiting_M, cost_static_B, cost_conv_iit_B,
             behavioral_iit_B, cost_conv_total_B)]
cat("\n====== OT deduction budget cost by AGI group, 2026 (adopt framing) ======\n")
print(pt, nrows=100)
cat("\ncost_static_B   = static income-tax cost (OT fixed)\n")
cat("cost_conv_iit_B = conventional income-tax cost (france induced OT, full phase-in)\n")
cat("behavioral_iit_B= conventional - static (extra cost from induced OT)\n")
cat("cost_conv_total_B = conventional cost incl. payroll offset from induced OT\n")

outdir <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/ot_deduction"
fwrite(tab, file.path(outdir, "ot_cost_behavioral_by_agi_2026.csv"))
cat("\nwrote:", file.path(outdir, "ot_cost_behavioral_by_agi_2026.csv"), "\n")
