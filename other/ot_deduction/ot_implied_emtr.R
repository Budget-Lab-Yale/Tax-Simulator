#-----------------------------------------------------------------------
# Implied EMTR on the OT deduction by AGI group = benefit / deduction.
#   benefit = liab_iit_net(no-deduction) - liab_iit_net(deduction, conv).
# Two cuts:
#   rate_benefiting = Sum benefit / Sum ot_ded over BENEFITING returns
#                     (the effective marginal rate where the deduction bites)
#   rate_allclaim   = Sum benefit / Sum ot_ded over ALL claimants
#                     (diluted by claimants with no tax to offset)
#-----------------------------------------------------------------------
suppressMessages({library(data.table)})

brks <- c(-Inf,0,20,40,60,80,100,120,140,160,180,200,Inf)
labs <- c("<0","0-20k","20-40k","40-60k","60-80k","80-100k",
          "100-120k","120-140k","140-160k","160-180k","180-200k","200k+")

run <- function(vintage, yr){
  root <- sprintf("/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/%s", vintage)
  base <- fread(file.path(root,"baseline","static","detail",paste0(yr,".csv")),
                select=c("id","liab_iit_net")); setnames(base,"liab_iit_net","liab_base")
  cl <- fread(file.path(root,"ot_cl","conventional","detail",paste0(yr,".csv")),
              select=c("id","weight","agi","ot_ded","liab_iit_net"))
  d <- merge(cl, base, by="id")
  d[, benefit := liab_base - liab_iit_net]
  d[, agi_grp := cut(agi/1000, breaks=brks, labels=labs, right=FALSE)]
  d[, ben := benefit > 0.5]

  agg <- function(dt) dt[, .(
    ded_claim   = sum(weight*ot_ded*(ot_ded>0)),
    ben_claim   = sum(weight*benefit*(ot_ded>0)),
    ded_benf    = sum(weight*ot_ded*ben),
    ben_benf    = sum(weight*benefit*ben)
  ), by=agi_grp]
  tab <- agg(d)[order(agi_grp)]
  tot <- cbind(data.table(agi_grp="TOTAL"), agg(d)[, lapply(.SD, sum), .SDcols=-1])
  tab <- rbind(tab, tot)
  tab[, `:=`(rate_benefiting = 100*ben_benf/ded_benf,
             rate_allclaim   = 100*ben_claim/ded_claim)]
  cat(sprintf("\n========== implied EMTR, %s (conventional) ==========\n", yr))
  print(tab[, .(agi_grp,
                EMTR_benefiting=round(rate_benefiting,1),
                EMTR_allclaim=round(rate_allclaim,1))], nrows=100)
  invisible(tab)
}
run("ot_behavior_2025","2025")
run("ot_behavior_2026","2026")
