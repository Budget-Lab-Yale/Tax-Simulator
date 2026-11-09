#-----------------------------------------------------------------------
# Average OT deduction amount per claimant, by AGI group, 2025 & 2026
# (conventional detail — deduction incl. the induced-overtime response).
#-----------------------------------------------------------------------
suppressMessages({library(data.table)})

brks <- c(-Inf,0,20,40,60,80,100,120,140,160,180,200,Inf)
labs <- c("<0","0-20k","20-40k","40-60k","60-80k","80-100k",
          "100-120k","120-140k","140-160k","160-180k","180-200k","200k+")

run <- function(vintage, yr){
  f <- sprintf("/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/%s/ot_cl/conventional/detail/%d.csv", vintage, yr)
  d <- fread(f, select=c("weight","agi","ot_ded"))
  d[, agi_grp := cut(agi/1000, breaks=brks, labels=labs, right=FALSE)]
  tab <- d[, .(claimants = sum(weight*(ot_ded>0)),
               total_ded = sum(weight*ot_ded)), by=agi_grp][order(agi_grp)]
  tot <- d[, .(agi_grp="TOTAL", claimants=sum(weight*(ot_ded>0)),
               total_ded=sum(weight*ot_ded))]
  tab <- rbind(tab, tot)
  tab[, avg_ded := fifelse(claimants>0, total_ded/claimants, 0)]
  cat(sprintf("\n===== avg OT deduction per claimant, %d (conventional) =====\n", yr))
  print(tab[, .(agi_grp, claimants_M=round(claimants/1e6,3),
                avg_ded=round(avg_ded,0))], nrows=100)
  invisible(tab)
}
run("ot_behavior_2025", 2025)
run("ot_behavior_2026", 2026)
