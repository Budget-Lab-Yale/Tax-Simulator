#-----------------------------------------------------------------------
# OT deduction, 2025 — full AGI table on the CONVENTIONAL run (france
# behavioral response, FULL effect / no phase-in). Adopt framing:
#   baseline = NO deduction (static); ot_cl = deduction + ot/france_full.
# Also reports the static (mechanical) totals for context.
#-----------------------------------------------------------------------
suppressMessages({library(data.table)})

root <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ot_behavior_2025"
yr <- "2025.csv"
base <- fread(file.path(root,"baseline","static","detail",yr), select=c("id","liab_iit_net"))
setnames(base,"liab_iit_net","liab_base")
sta  <- fread(file.path(root,"ot_cl","static","detail",yr), select=c("id","liab_iit_net"))
setnames(sta,"liab_iit_net","liab_static")
cl   <- fread(file.path(root,"ot_cl","conventional","detail",yr),
              select=c("id","weight","filer","agi","ot_ded","liab_iit_net"))
setnames(cl,"liab_iit_net","liab_conv")
d <- Reduce(function(a,b) merge(a,b,by="id"), list(cl, base, sta))
cat("rows:", nrow(d), "\n")

d[, benefit    := liab_base - liab_conv]      # conventional benefit (w/ behavior)
d[, ben_static := liab_base - liab_static]    # mechanical
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
cat("\n===== OT deduction by AGI group, 2025 (conventional, full effect) =====\n")
print(pt[, .(agi_grp,units_M,ret_M,clm_M,ben_M,avg,tot_B)], nrows=100)

cat(sprintf("\nTOTAL static (mechanical):  $%.3fB\n", sum(d$weight*d$ben_static)/1e9))
cat(sprintf("TOTAL conventional        :  $%.3fB\n", sum(d$weight*d$benefit)/1e9))
cat(sprintf("Behavioral increment      :  $%.3fB (%.1f%%)\n",
            sum(d$weight*(d$benefit-d$ben_static))/1e9,
            100*sum(d$weight*(d$benefit-d$ben_static))/sum(d$weight*d$ben_static)))

outdir <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/ot_deduction"
fwrite(tab, file.path(outdir,"ot_agi_full_table_conventional_2025.csv"))
cat("\nwrote:", file.path(outdir,"ot_agi_full_table_conventional_2025.csv"), "\n")
