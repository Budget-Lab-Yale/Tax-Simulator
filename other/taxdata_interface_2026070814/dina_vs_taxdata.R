#-------------------------------------------------------------------------------
# dina_vs_taxdata.R
#
# Matched comparison: state+local+excise effective rate by income group, the
# DINA analog of our `other` ETR tier, computed the SAME way as distribution_etrs:
#
#   numerator   = (state income + sales/excise + res property)         [DINA actual]
#                 = dina_other_taxes_rate * pmax(broad, 0)             [Tax-Data imputed]
#   denominator = broad income  (fiscal income incl KG + SS + UI)
#   ranking     = add_rank_groups() rules: rank ONLY the nonnegative population;
#                 negatives get NA rank -> separate "Negative income" bucket;
#                 zeros are ranked (>= 0). Quintiles over the nonnegative pop.
#
# Both datasets, 2019.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(haven); library(data.table) })

# Replicate add_rank_groups (helpers.R:112-137): percentile among nonnegative
# rank_var, cumulative-weight in rank order; NA (=> "Negative income") if < 0.
rank_groups <- function(rank_val, w) {
  o <- order(rank_val)
  nonneg <- rank_val >= 0
  pct <- rep(NA_real_, length(rank_val))
  denomw <- sum(w * nonneg)
  pct[o] <- cumsum((w * nonneg)[o]) / denomw
  pct[!nonneg] <- NA_real_
  grp <- fifelse(is.na(pct), 'Negative income',
         fifelse(pct <= 0.2, 'Quintile 1',
         fifelse(pct <= 0.4, 'Quintile 2',
         fifelse(pct <= 0.6, 'Quintile 3',
         fifelse(pct <= 0.8, 'Quintile 4', 'Quintile 5')))))
  list(grp = grp,
       `Top 10%`  = !is.na(pct) & pct > 0.90,
       `Top 1%`   = !is.na(pct) & pct > 0.99,
       `Top 0.1%` = !is.na(pct) & pct > 0.999)
}

report <- function(tag, tax, broad, w) {
  rg <- rank_groups(broad, w)
  ord <- c('Negative income','Quintile 1','Quintile 2','Quintile 3','Quintile 4',
           'Quintile 5','Top 10%','Top 1%','Top 0.1%','ALL')
  masks <- c(lapply(ord[1:6], function(g) which(rg$grp == g)),
             list(which(rg$`Top 10%`), which(rg$`Top 1%`), which(rg$`Top 0.1%`),
                  seq_along(broad)))
  names(masks) <- ord
  cat(sprintf('\n== %s ==\n', tag))
  cat(sprintf('%-16s %10s %12s\n', 'group', 'broad$B', 'other ETR%'))
  for (nm in ord) {
    m <- masks[[nm]]
    denB <- sum(w[m] * broad[m])
    etr  <- 100 * sum(w[m] * tax[m]) / denB
    cat(sprintf('%-16s %10.0f %11.1f%%\n', nm, denB/1e9, etr))
  }
}

z0 <- function(x) fifelse(is.na(x), 0, x)

## ---- DINA 2019 (actual state+local+excise) ----
D <- as.data.table(read_dta('/nfs/roberts/project/pi_nrs36/shared/raw_data/DINA/v1/2023082913/historical/usdina2019.dta'))
for (v in c('dweghttaxu','fiinc','ssinc_oa','ssinc_di','uiinc','ditas','salestax','proprestax'))
  set(D, j = v, value = as.numeric(D[[v]]))
dtu <- D[, .(w = first(dweghttaxu)/1e5, proprestax = proprestax[1],
             fiinc = sum(fiinc), ss = sum(ssinc_oa) + sum(ssinc_di), ui = sum(uiinc),
             ditas = sum(ditas), salestax = sum(salestax)), by = id]
dtu[, broad := fiinc + ss + ui]
dtu[, salt  := ditas + salestax + proprestax]            # actual dollars
report('DINA 2019 (actual SALT / broad)', dtu$salt, dtu$broad, dtu$w)

## ---- Tax-Data 2019 (our imputed rate, same numerator rule as distribution_etrs) ----
COMPS <- c('wages','sole_prop','farm','scorp_active','scorp_active_loss','scorp_179',
           'scorp_passive','scorp_passive_loss','part_active','part_active_loss','part_179',
           'part_passive','part_passive_loss','txbl_int','exempt_int','div_ord','div_pref',
           'kg_lt','kg_st','gross_ss','gross_pens_dist','ui','rent','rent_loss','estate','estate_loss')
TD <- fread('/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026070814/baseline/tax_units_2019.csv',
            select = c('id','weight','dina_other_taxes_rate', COMPS))
TD[, broad := z0(wages)+z0(sole_prop)+z0(farm)+z0(scorp_active)-z0(scorp_active_loss)-z0(scorp_179)+
               z0(scorp_passive)-z0(scorp_passive_loss)+z0(part_active)-z0(part_active_loss)-z0(part_179)+
               z0(part_passive)-z0(part_passive_loss)+z0(txbl_int)+z0(exempt_int)+z0(div_ord)+z0(div_pref)+
               z0(kg_lt)+z0(kg_st)+z0(gross_ss)+z0(gross_pens_dist)+z0(ui)+z0(rent)-z0(rent_loss)+z0(estate)-z0(estate_loss)]
TD[, other := z0(dina_other_taxes_rate) * pmax(broad, 0)]  # SAME rule as read_other_taxes_base
report('Tax-Data 2019 (imputed rate x pmax(broad,0) / broad)', TD$other, TD$broad, TD$weight)

cat('\nRanking + numerator handling identical to distribution_etrs (add_rank_groups:\n')
cat('nonneg-only rank, negatives -> "Negative income"; numerator floors broad at 0).\n')
cat('Done.\n')
