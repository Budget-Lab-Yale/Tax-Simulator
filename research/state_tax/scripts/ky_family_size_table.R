# Kentucky family-size tax credit table (KRS 141.066; Schedule ITC Table C) as
# a GENERATOR rather than a transcription. KRS 141.066(1)(a) defines the
# poverty threshold as the HHS guideline available on June 30 of the tax year
# (published in January of that year), and (3)(c) fixes the bands at 100 /
# 104 / 108 / 112 / 116 / 120 / 124 / 127 / 130 / 133 percent of the threshold
# for family sizes one to four-or-more (credit 100% down to 10%, zero above
# 133%). Band upper bounds are the percentage times the threshold rounded
# half-up to the dollar; lower bounds are the previous upper plus one. This
# reproduces every cell of the printed 2018 and 2025 Table C (the one
# transcription slip in the old file, 41,695 for 41,795, is corrected).
#
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_tax/scripts/ky_family_size_table.R        # writes the CSV
#   Rscript research/state_tax/scripts/ky_family_size_table.R check  # compares only
suppressPackageStartupMessages(library(data.table))
OUT = 'config/scenarios/tax_law_state/baseline/ky/credit_tables.csv'

# HHS poverty guidelines, 48 contiguous states: first person and each
# additional person (Federal Register, January of each year; aspe.hhs.gov)
hhs = data.table(
  year  = 2017:2026,
  first = c(12060, 12140, 12490, 12760, 12880, 13590, 14580, 15060, 15650, 15960),
  addl  = c( 4180,  4320,  4420,  4480,  4540,  4720,  5140,  5380,  5500,  5680))
PCT  = c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.24, 1.27, 1.30, 1.33)
RATE = c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
round_half_up = function(x) floor(x + 0.5)

rows = rbindlist(lapply(seq_len(nrow(hhs)), function(i) rbindlist(lapply(1:4, function(size) {
  thr = hhs$first[i] + hhs$addl[i] * (size - 1)
  upper = round_half_up(PCT * thr)
  data.table(credit_id = 'family_size_tax_credit', state = 'KY', year = hhs$year[i],
             filing_status = 0L, key_concept = size,
             income_lower = c(-Inf, head(upper, -1) + 1), income_upper = upper, value = RATE)
}))))

old = fread(OUT)
# The old file labels the TY2018 table "2017"
cmp = merge(old[, .(year = fifelse(year == 2017, 2018L, year), key_concept, income_lower, income_upper, value)],
            rows[, .(year, key_concept, income_lower, income_upper, value)],
            by = c('year', 'key_concept', 'value'), suffixes = c('_old', '_new'))
diffs = cmp[income_lower_old != income_lower_new | income_upper_old != income_upper_new]
cat(sprintf('old rows %d; matched %d; cells differing: %d\n', nrow(old), nrow(cmp), nrow(diffs)))
if (nrow(diffs)) print(diffs)
# Only the 41,695-for-41,795 slip (and the next band's lower bound it shifts) may differ
stopifnot(nrow(cmp) == nrow(old), all(diffs$year == 2025 & diffs$key_concept == 4 & diffs$value %in% c(0.1, 0.2)))
if (!('check' %in% commandArgs(trailingOnly = TRUE))) {
  fwrite(rows, OUT)
  cat('wrote', nrow(rows), 'rows for', paste(range(rows$year), collapse = '-'), 'to', OUT, '\n')
}
