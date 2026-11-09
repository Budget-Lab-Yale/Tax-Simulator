# Checks the death-gain exclusion machinery on synthetic records: the pro-rata
# apportionment identity, the married doubling, the widowed two-branch blend,
# and the carryover cell mix. Run via sbatch test_death_gain_excl.sbatch.
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE))))
library(purrr)
return_vars <<- list()   # some src post-processing files reference this at source time
list.files('./src', recursive = TRUE) %>%
  walk(~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/'))
         source(file.path('./src', .x)))

tol = 1e-9

# The corporate exposure column read during cell aggregation is not under
# test and needs an active scenario configuration; shadow it out.
corp_kg_state_exposed_value = function(tax_units) rep(0, nrow(tax_units))

# A synthetic widowhood table in place of the configured one: attach resolves
# get_widowhood_table() from the global environment, so shadowing it here keeps
# the test free of scenario configuration.
P_WIDOW_OLD_F = 0.6
get_widowhood_table = function() {
  list(survey_year = 2022,
       bands = tibble(lo = c(18, 65), hi = c(64, Inf),
                      p_widow_male   = c(0.1, 0.4),
                      p_widow_female = c(0.2, P_WIDOW_OLD_F)))
}

# Four records: a widowed-age single woman, a married couple, a divorced single
# man, and a below-exclusion single. Gains: equities 3M, pass-throughs 1M,
# primary home 1.25M (1M after the 250k Sec. 121), so the eligible pool under
# an all-classes carryover regime is 5M for the first three.
EXCL_X  = 1e6
EXCL_2X = 2e6
mk_records = function() {
  n = 4
  rec = tibble(
    id = 1:n, weight = 1,
    filing_status = c(1, 2, 1, 1),
    age1 = c(70, 70, 70, 70), age2 = c(NA, 68, NA, NA),
    male1 = c(0, 1, 1, 0), divorce_year = c(NA, NA, 2010, NA),
    kg_lt = 0, q_death1 = 0.1, q_death2 = c(NA, 0.1, NA, NA),
    `pref.kg_sec121_excl`     = c(250000, 500000, 250000, 250000),
    `pref.kg_death_gain_excl` = c(EXCL_X, EXCL_2X, EXCL_X, EXCL_X),
    year = 2026
  )
  for (nm in ESTATE_ASSET_COLS) rec[[nm]] = 0
  for (nm in KG_DYN_ASSET_BASIS_COLS) rec[[nm]] = 0
  rec[['value.equities']]      = c(3e6, 3e6, 3e6, 0.2e6)
  rec[['value.pass_throughs']] = c(1e6, 1e6, 1e6, 0)
  rec[['value.primary_home']]  = c(1.25e6, 1.5e6, 1.25e6, 0)
  rec[['value.other_home']]    = 0
  rec[['value.re_fund']]       = 0
  for (k in KG_DYN_ASSET_CLASSES) {
    rec[[paste0('pref.kg_death_regime_', k)]] = 1
  }
  rec
}

rec = kg_dyn_attach_record_attrs(mk_records(), death_excl_married = EXCL_2X)

# Pro-rata apportionment identity: the above-exclusion slices sum to
# pool - excl when the pool exceeds it, and to zero when it does not.
above_cols = paste0(KG_DYN_ASSET_GAIN_COLS, '_above_excl')
above_sum = rowSums(as.matrix(rec[, above_cols]))
pool = c(5e6, 5e6, 5e6, 0.2e6)   # married primary stake = 1.5M - 500k = 1M

# Record 3 (divorced) and record 2 (married) have no widow branch. Record 1 is
# a 70-year-old unmarried woman: p_widow = 0.6, blend of (pool - 1M) and
# (pool - 2M). Record 4 sits below the exclusion entirely.
p1 = P_WIDOW_OLD_F
stopifnot(abs(rec$p_widow[1] - p1)  < tol,
          rec$p_widow[2] == 0, rec$p_widow[3] == 0,
          abs(rec$p_widow[4] - p1) < tol)
stopifnot(abs(above_sum[1] - (p1 * (pool[1] - EXCL_2X) +
                              (1 - p1) * (pool[1] - EXCL_X))) < 1e-3)
stopifnot(abs(above_sum[2] - (pool[2] - EXCL_2X)) < 1e-3)   # married doubling
stopifnot(abs(above_sum[3] - (pool[3] - EXCL_X))  < 1e-3)   # divorced: own amount
stopifnot(above_sum[4] == 0)                                # below the exclusion

# Per-class pro-rata: each class's slice carries its share of the pool.
stopifnot(abs(rec$gain.equities_above_excl[3] -
                (pool[3] - EXCL_X) * 3e6 / 5e6) < 1e-3)

# Applier two-branch split under a deemed regime: kg_deemed_full blends the
# X and 2X branches by p_widow, and both branches respect the pro-rata form.
rec2 = mk_records()
for (k in KG_DYN_ASSET_CLASSES) rec2[[paste0('pref.kg_death_regime_', k)]] = 2
rec2 = kg_dyn_attach_record_attrs(rec2, death_excl_married = EXCL_2X)
cell_syn = tibble(age = 70, G_B = sum(rec2$G_unit), R_B = 1, r_B = 0.05,
                  p_char = 0, rate_factor = 1, extra_R = 0, deemed_factor = 1)
codes = setNames(as.list(rep(2, 5)), KG_DYN_ASSET_CLASSES)
realize = setNames(as.list(rep(1, 5)), KG_DYN_ASSET_CLASSES)
out = kg_dyn_apply_to_records(rec2, cell_syn, realize,
                              regime_codes = codes,
                              death_excl_married = EXCL_2X)
keep = 1 - kg_setting('deemed_avoidance')
# The haircut discounts value with basis zero, so the discounted pool is
# keep * value with the Sec. 121 netting on the primary home.
pool_hc = keep * c(3e6 + 1e6, 3e6 + 1e6, 3e6 + 1e6, 0.2e6) +
          pmax(0, keep * c(1.25e6, 1.5e6, 1.25e6, 0) - c(250000, 500000, 250000, 250000))
f_x  = pmax(0, pool_hc - c(EXCL_X, EXCL_2X, EXCL_X, EXCL_X)) / pool_hc
f_2x = pmax(0, pool_hc - c(EXCL_2X, EXCL_2X, EXCL_2X, EXCL_2X)) / pool_hc
stopifnot(max(abs(out$kg_deemed_full_x  - pool_hc * f_x))  < 1e-3,
          max(abs(out$kg_deemed_full_2x - pool_hc * f_2x)) < 1e-3,
          max(abs(out$kg_deemed_full -
                    (out$p_widow * out$kg_deemed_full_2x +
                     (1 - out$p_widow) * out$kg_deemed_full_x))) < tol)

# Cell mix under carryover: the routed share and c_phi shrink by exactly the
# excluded share, while the vanish share keeps the full gains.
cells = kg_dyn_aggregate_cells(rec, ages = 70)
mix = kg_dyn_build_regime_mix(codes_carry <- setNames(as.list(rep(1, 5)),
                                                      KG_DYN_ASSET_CLASSES),
                              theta = 0.5, baseline_t = cells,
                              ages_bathtub = 70)
expected_route = sum(above_sum) / sum(cells$G_B)
stopifnot(abs(mix$delta_route - expected_route) < tol,
          abs(mix$c_phi - 0.5 * expected_route) < tol,
          mix$delta_vanish == 0)

cat('test_death_gain_excl: ALL PASS\n')
