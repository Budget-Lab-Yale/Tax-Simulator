#-------------------------------------------------------------------------------
# Does a record-level (by-id) wealth-deficit recurrence have a stable panel?
# Checks id-set stability and weight evolution across 2026 -> 2027 Tax-Data.
#-------------------------------------------------------------------------------
suppressPackageStartupMessages(library(data.table))
DIR <- "/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline"

d26 <- fread(file.path(DIR, "tax_units_2026.csv"), select = c("id", "weight", "age1"))
d27 <- fread(file.path(DIR, "tax_units_2027.csv"), select = c("id", "weight", "age1"))

i26 <- d26$id; i27 <- d27$id
common <- length(intersect(i26, i27))

cat("\n================ ID PERSISTENCE 2026 -> 2027 ================\n")
cat(sprintf("rows 2026                       : %d\n", nrow(d26)))
cat(sprintf("rows 2027                       : %d\n", nrow(d27)))
cat(sprintf("unique ids 2026                 : %d\n", uniqueN(i26)))
cat(sprintf("unique ids 2027                 : %d\n", uniqueN(i27)))
cat(sprintf("ids in BOTH years               : %d\n", common))
cat(sprintf("  share of 2026 ids retained    : %.3f%%\n", 100 * common / uniqueN(i26)))
cat(sprintf("  ids in 2026 only (exit)        : %d\n", uniqueN(i26) - common))
cat(sprintf("  ids in 2027 only (entry)       : %d\n", uniqueN(i27) - common))
cat(sprintf("weight sum 2026                  : %.0f\n", sum(d26$weight)))
cat(sprintf("weight sum 2027                  : %.0f\n", sum(d27$weight)))

# For records present in both years, how does weight change? (mortality => weights fall)
m <- merge(d26[, .(id, w26 = weight, age26 = age1)],
           d27[, .(id, w27 = weight)], by = "id")
m[, dw := w27 / w26 - 1]
cat("\n--- weight change for retained ids, by 2026 age band ---\n")
m[, band := cut(age26, c(-Inf, 30, 50, 65, 75, Inf),
                labels = c("<30", "30-49", "50-64", "65-74", "75+"))]
print(m[, .(n = .N,
            wsum26 = round(sum(w26)),
            wsum27 = round(sum(w27)),
            med_weight_chg_pct = round(100 * median(dw), 2)), by = band][order(band)])
cat("=============================================================\n")
