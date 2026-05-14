#-------------------------------------------------------------------------------
# build_heir_distribution.R
#
# Builds the dollar-weighted heir-age distribution that kg_dynamics uses to
# construct the omega heir-allocation matrix (every row of omega is a copy of
# this vector — see kg_dyn_build_heir_matrix). Output is a static resource at
# resources/heir_distribution_scf2022.csv (2 cols: age, share; 63 rows, ages
# 18-80, shares summing to 1).
#
# Run from repo root via sbatch (see build_heir_distribution.sbatch).
# Re-run whenever the SCF vintage updates; otherwise the resource is stable.
#
# Method:
#   1. Read SCF 2022 detailed (p22i6.dta): up to 3 inheritance slots per
#      household, with kind (x5803), value (x5804), year (x5805) — and the
#      parallel x5808/x5809/x5810, x5813/x5814/x5815.
#   2. Filter to inheritance/trust (kind != 3 — i.e., drop transfers/gifts).
#      Per the SCF 2022 codebook:
#        1 = INHERITANCE; life insurance; other settlements
#        2 = TRUST
#        3 = TRANSFER/GIFT   ← excluded
#        6 = INHERITED TRUST
#   3. Apply Gale-Sabelhaus (2024) recency probability weights to convert the
#      stock of reported inheritances to a current-year flow (matches the
#      Estate-Tax-Distribution convention in src/tcja_ext.R). For SCF 2022:
#        year == 2022 → p = 1.00
#        year == 2020 → p = 0.28  (year-received rounding adjustment)
#        other       → p = 0     (drop)
#   4. Join SCF summary (SCFP2022.csv) for household weight (WGT) and age of
#      head (AGE). Cap age at [18, 80] to match the bathtub topcode.
#   5. Aggregate sum(weight * p * value) by recipient age, normalize.
#
# Note on spousal inheritances: SCF X5806 "from whom" has codes Grandparent,
# Parent, Child, Aunt/Uncle, Sibling, Friend, Family n.e.c., Other. There is
# no current-spouse code (the SCF inheritance question treats the household
# as a unit; intra-household transfers aren't framed as inheritances). The
# only spouse-adjacent code is "Divorced former spouse" (= ex-spouse who
# subsequently died), which the public release collapses into Family n.e.c.
# No spousal filter is required: the data is already a clean inter-household
# flow.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
})

SCF_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2022/historical'
AGE_MIN  = 18L
AGE_MAX  = 80L
OUTPUT   = './resources/heir_distribution_scf2022.csv'


# Detailed file — one row per household, three inheritance slots
det = read_dta(file.path(SCF_ROOT, 'p22i6.dta'),
               col_select = c(yy1, y1,
                              x5803, x5804, x5805,
                              x5808, x5809, x5810,
                              x5813, x5814, x5815))

# Summary file — household weight and age of head
sm = read_csv(file.path(SCF_ROOT, 'SCFP2022.csv'), show_col_types = FALSE) %>%
  transmute(yy1 = YY1, y1 = Y1, weight = WGT, age = AGE)


# Pivot to long: one row per (household, slot)
long = det %>%
  transmute(yy1, y1,
            kind.1 = x5803, value.1 = x5804, year.1 = x5805,
            kind.2 = x5808, value.2 = x5809, year.2 = x5810,
            kind.3 = x5813, value.3 = x5814, year.3 = x5815) %>%
  pivot_longer(cols          = -c(yy1, y1),
               names_pattern = '(kind|value|year)\\.(\\d)',
               names_to      = c('var', 'slot')) %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(p = case_when(
    year == 2022 ~ 1.00,
    year == 2020 ~ 0.28,
    TRUE         ~ 0.00
  )) %>%
  filter(value > 0, kind != 3, p > 0)


# Aggregate dollar-weighted heir mass by recipient age (capped at 80)
agg = long %>%
  inner_join(sm, by = c('yy1', 'y1')) %>%
  mutate(age_cohort = pmax(AGE_MIN, pmin(AGE_MAX, age))) %>%
  group_by(age_cohort) %>%
  summarise(h = sum(weight * p * value, na.rm = TRUE), .groups = 'drop')


# Fill missing ages with 0, normalize
out = tibble(age = AGE_MIN:AGE_MAX) %>%
  left_join(agg, by = c('age' = 'age_cohort')) %>%
  mutate(h = if_else(is.na(h), 0, h),
         share = h / sum(h)) %>%
  select(age, share)


stopifnot(abs(sum(out$share) - 1) < 1e-12,
          all(out$share >= 0),
          nrow(out) == AGE_MAX - AGE_MIN + 1L)


write_csv(out, OUTPUT)

cat(sprintf('Wrote %s\n  rows = %d\n  sum  = %.10f\n  max  = age %d at share %.6f\n',
            OUTPUT, nrow(out),
            sum(out$share),
            out$age[which.max(out$share)], max(out$share)))
