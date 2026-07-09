#-----------------------------------------------------------------------
# OT deduction, 2026 — full AGI table on the CONVENTIONAL run (france
# behavioral response, full 1-yr phase-in). Adopt framing:
#   baseline = NO deduction (static);  ot_cl = deduction + ot/france_1yr (conv).
# benefit = liab_iit_net(baseline) - liab_iit_net(ot_cl conventional).
# Counts (returns/claimants/agi) come from the deduction-on world (ot_cl conv).
#-----------------------------------------------------------------------
suppressMessages({library(data.table)})

root <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ot_behavior_2026"
base <- fread(file.path(root,"baseline","static","detail","2026.csv"),
              select=c("id","liab_iit_net"))
setnames(base,"liab_iit_net","liab_base")            # no-deduction counterfactual
cl <- fread(file.path(root,"ot_cl","conventional","detail","2026.csv"),
            select=c("id","weight","filer","agi","ot_ded","liab_iit_net"))
d <- merge(cl, base, by="id")
cat("rows:", nrow(d), "\n")

d[, benefit    := liab_base - liab_iit_net]   # tax saved by the deduction (w/ behavior)
d[, claimant   := ot_ded > 0]
d[, benefiting := benefit > 0.5]
d[, agi_k := agi/1000]
brks <- c(-Inf,0,20,40,60,80,100,120,140,160,180,200,Inf)
labs <- c("<0","0-20k","20-40k","40-60k","60-80k","80-100k",
          "100-120k","120-140k","140-160k","160-180k","180-200k","200k+")
d[, agi_grp := cut(agi_k, breaks=brks, labels=labs, right=FALSE)]

mk <- function(dt) dt[, .(
  tax_units     = sum(weight),
  returns       = sum(weight*filer),
  claimants     = sum(weight*claimant),
  benefiting    = sum(weight*benefiting),
  total_benefit = sum(weight*benefit)
), by=agi_grp]
tab <- mk(d)[order(agi_grp)]
tot <- cbind(data.table(agi_grp="TOTAL"), mk(d)[, lapply(.SD, sum), .SDcols=-1])
tab <- rbind(tab, tot)
tab[, avg_benefit := fifelse(benefiting>0, total_benefit/benefiting, 0)]

pt <- copy(tab)
pt[, `:=`(units_M=round(tax_units/1e6,3), ret_M=round(returns/1e6,3),
          clm_M=round(claimants/1e6,3), ben_M=round(benefiting/1e6,3),
          avg=round(avg_benefit,0), tot_B=round(total_benefit/1e9,3))]
print(pt[, .(agi_grp,units_M,ret_M,clm_M,ben_M,avg,tot_B)], nrows=100)

outdir <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/ot_deduction"
fwrite(tab, file.path(outdir,"ot_agi_full_table_conventional_2026.csv"))
cat("\nwrote:", file.path(outdir,"ot_agi_full_table_conventional_2026.csv"), "\n")
