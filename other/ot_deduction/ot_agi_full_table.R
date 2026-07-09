#-----------------------------------------------------------------------
# OT deduction, 2026 (static) — full AGI table:
#   tax units, returns (filers), claimants (ot_ded>0), number benefiting,
#   average benefit.
# baseline = current law WITH deduction; repeal_ot = deduction removed.
# benefit = liab_iit_net(repeal) - liab_iit_net(baseline)  [tax saved by the ded].
#-----------------------------------------------------------------------
suppressMessages({library(data.table)})

root <- "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/ot_benefit_2026"
b <- fread(file.path(root,"baseline","static","detail","2026.csv"),
           select=c("id","weight","filer","agi","ot_ded","liab_iit_net"))
r <- fread(file.path(root,"repeal_ot","static","detail","2026.csv"),
           select=c("id","liab_iit_net"))
setnames(r,"liab_iit_net","liab_iit_net_r")
d <- merge(b, r, by="id")
cat("filer values:", paste(sort(unique(d$filer)), collapse=" "), "\n")

d[, benefit   := liab_iit_net_r - liab_iit_net]
d[, claimant  := ot_ded > 0]
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
pt[, `:=`(
  tax_units_M  = round(tax_units/1e6,3),
  returns_M    = round(returns/1e6,3),
  claimants_M  = round(claimants/1e6,3),
  benefiting_M = round(benefiting/1e6,3),
  avg_benefit_usd = round(avg_benefit,0),
  total_benefit_B = round(total_benefit/1e9,3)
)]
pt <- pt[, .(agi_grp, tax_units_M, returns_M, claimants_M, benefiting_M,
             avg_benefit_usd, total_benefit_B)]
cat("\n=========== OT deduction by AGI group, 2026 (static) ===========\n")
print(pt, nrows=100)
cat("\ncounts in millions; avg_benefit_usd = total_benefit / number benefiting\n")

outdir <- "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/ot_deduction"
fwrite(tab, file.path(outdir,"ot_agi_full_table_2026.csv"))
cat("\nwrote:", file.path(outdir,"ot_agi_full_table_2026.csv"), "\n")
