# NJ (2026-10-01): composition of the large state-AGI gaps (ours - federal AGI > $10k)
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
yr = 2019
r = fread(sprintf('research/state_tax/cross_model/results/raw/taxsim_%d.csv', yr),
          select = c('id','state','fed_aligned','excluded','abs_diff','diff','agi','st_agi','v32_state_agi'))
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE]
fc = c('id','other_inc','other_gains','kg_st','kg_lt','txbl_kg','sole_prop','part_scorp','part_scorp_loss','part_active_loss','part_passive_loss',
       'scorp_active_loss','scorp_passive_loss','rent','rent_loss','estate','estate_loss','farm','excess_bus_loss','wages','gross_inc')
f = as.data.table(readRDS(sprintf('research/state_tax/cross_model/cache/fed_calc_%d.rds', yr))$tax_units)
f = f[, intersect(fc, names(f)), with = FALSE]
d = merge(r, f, by = 'id'); d[, gap := st_agi - agi]
big = d[gap > 10000]
cat(sprintf('NJ %d: %d records with ours - fed AGI > $10k (%d misses); median gap %.0f\n', yr, nrow(big), sum(big$abs_diff > 100), median(big$gap)))
neg = function(x) pmax(0, -x)
comp = big[, .(nol_other_inc = neg(other_inc), f4797_other_gains = neg(other_gains), kg_beyond = neg(kg_st + kg_lt) - neg(txbl_kg),
               part_scorp_net = neg(part_scorp), rent_net = neg(rent - rent_loss), estate_net = neg(estate - estate_loss),
               sole_prop_net = neg(sole_prop), farm_net = neg(farm), excess_bus = excess_bus_loss)]
for (v in names(comp)) cat(sprintf('  %-18s present on %.2f of large-gap records; median when present %10.0f\n', v, mean(comp[[v]] > 1), median(comp[[v]][comp[[v]] > 1])))
big[, sum_cand := rowSums(comp)]
cat(sprintf('  sum of these within 10%% of the gap on %.2f of large-gap records\n', mean(abs(big$sum_cand - big$gap) <= 0.1 * big$gap)))
cat(sprintf('  TAXSIM v32 vs fed AGI on these: median %.0f (TAXSIM lets the loss through if ~0)\n', median(big$v32_state_agi - big$agi)))
