#-------------------------------------------------------------------------------
# t6_smear_benchmark.R
#
# T6 smear-benchmark gate for the wealth-carry channel (plan:
# enumerated-meandering-pinwheel, review addition; run BEFORE the T5 A/B).
#
# Question: how much does the age-cell GAIN-WEIGHTED SMEAR of the carrying
# cost h (the shipped design) misstate aggregate extra realizations relative
# to a full record-level response, given the within-cell heterogeneity of
# h_i = mtr_net_worth_i * mtr_kg_lt_i?
#
# On one frozen full-sample year (2036, vintage top_tax_dials_30y_v1):
#   (a) record-level: sum_i w_i G_i r_B(a_i) (exp(eta * PV(h_i)) - 1)
#   (b) cell smear:   sum_a G(a)  r_B(a)     (exp(eta * PV(hbar(a))) - 1)
# with the SAME stationary PV mapping PV(h) = h * bs(a)/(1 - bs(a)),
# bs(a) = beta*(1 - m_gw(a)) — the plan's young-cell MC-wedge magnitude.
# The PV factor is cell-level in BOTH branches, so the comparison isolates
# exactly the Jensen error of smearing h within cells.
#
# Expectation (Jensen, exp convex): smear UNDERSTATES (a) >= (b);
# gate: |error| < 1% at the mild lever, < 5% at the aggressive corner.
# If breached: escalate to the exposed/unexposed two-state split (author
# decision) — evidence-gated, not preemptive.
#
# Levers available in the frozen vintage (closest brackets to the plan's
# "v1 lever" and "3% corner"):
#   mild   = pc_wealthr1t1000_deemed   (1% above $1B)
#   middle = pc_cgr30_wealthr3t500     (3% above $500M, +30pp CG — the T5
#                                       wealth scenario)
#   corner = pc_wealthr4t50_estater60e5 (4% above $50M — harsher than the
#                                       plan's 3% corner, conservative)
#
# Sbatch-only: sbatch other/kg_model_tests/t6_smear_benchmark.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
source('./src/sim/kg_dynamics.R')

VROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v1'
YEAR  = 2036
ETA   = KG_DYN_DEFAULT_ETA
BETA  = KG_DYN_BETA     # constant-beta stationary PV; identical in (a) and (b)

levers = c(mild   = 'pc_wealthr1t1000_deemed',
           middle = 'pc_cgr30_wealthr3t500',
           corner = 'pc_wealthr4t50_estater60e5')
gate   = c(mild = 0.01, middle = 0.05, corner = 0.05)

# Record-level gain stocks + age cells from the frozen pass's inputs cache
# (any wealth scenario's cache works: td_slim/baseline_cells are baseline-side)
cache = readRDS(file.path(VROOT, levers[['middle']], 'static', 'supplemental',
                          'kg_dynamics_mech_state', 'inputs_cache.rds'))
td = cache$td_slim_by_year[[as.character(YEAR)]]
bt = cache$baseline_cells[[as.character(YEAR)]]
stopifnot(!is.null(td), !is.null(bt))

# Gain-weighted cell mortality (kg_dyn_pack_baseline_grid convention) and the
# stationary PV factor per age: PV_fac(a) = bs/(1 - bs), bs = BETA*(1 - m_gw)
cell = bt %>%
  transmute(age_cohort = as.integer(age),
            r_B_cell   = r_B,
            m_gw       = ifelse(G_B > 0, mG_record / G_B, m),
            bs         = BETA * (1 - pmin(pmax(m_gw, 0), 1)),
            pv_fac     = bs / (1 - bs))

results = imap_dfr(levers, function(scen, tag) {
  det = fread(file.path(VROOT, scen, 'static', 'detail',
                        paste0(YEAR, '.csv')),
              select = c('id', 'mtr_net_worth', 'mtr_kg_lt'),
              showProgress = FALSE) %>% as_tibble()

  df = td %>%
    left_join(det, by = 'id') %>%
    left_join(cell, by = 'age_cohort') %>%
    filter(G_unit > 0) %>%
    mutate(h_i = coalesce(mtr_net_worth, 0) * coalesce(mtr_kg_lt, 0),
           wG  = weight * G_unit)

  # (a) record-level response, (b) cell smear with hbar = gain-weighted mean,
  # (c) the escalation candidate: exposed/unexposed two-state split within
  #     each age cell (smear only among h > 0 records)
  df_a = df %>%
    mutate(resp = wG * r_B_cell * (exp(ETA * pv_fac * h_i) - 1))
  by_cell = df %>%
    group_by(age_cohort, r_B_cell, pv_fac) %>%
    summarise(G = sum(wG), hbar = sum(wG * h_i) / sum(wG), .groups = 'drop')
  by_cell2 = df %>%
    group_by(age_cohort, exposed = h_i > 0, r_B_cell, pv_fac) %>%
    summarise(G = sum(wG), hbar = sum(wG * h_i) / sum(wG), .groups = 'drop')
  extra_a = sum(df_a$resp)
  extra_b = sum(by_cell$G * by_cell$r_B_cell *
                  (exp(ETA * by_cell$pv_fac * by_cell$hbar) - 1))
  extra_c = sum(by_cell2$G * by_cell2$r_B_cell *
                  (exp(ETA * by_cell2$pv_fac * by_cell2$hbar) - 1))

  err  = (extra_b - extra_a) / extra_a
  err2 = (extra_c - extra_a) / extra_a
  hw  = df %>% summarise(
    hbar_gw    = sum(wG * h_i) / sum(wG),
    h_pos_share = sum(wG[h_i > 0]) / sum(wG),
    h_max      = max(h_i))

  tibble(lever = tag, scenario = scen,
         extra_realizations_record = extra_a,
         extra_realizations_smear  = extra_b,
         extra_realizations_2state = extra_c,
         rel_error = err,
         rel_error_2state = err2,
         understates = extra_b <= extra_a + abs(extra_a) * 1e-12,
         within_gate = abs(err) < gate[[tag]],
         hbar_gw = hw$hbar_gw, h_pos_gain_share = hw$h_pos_share,
         h_max = hw$h_max)
})

print(as.data.frame(results), digits = 4)
write_csv(results, 'other/kg_model_tests/t6_smear_benchmark_result.csv')

# Gain-weighted h distribution by age at the corner lever (report table)
det_c = fread(file.path(VROOT, levers[['corner']], 'static', 'detail',
                        paste0(YEAR, '.csv')),
              select = c('id', 'mtr_net_worth', 'mtr_kg_lt'),
              showProgress = FALSE) %>% as_tibble()
td %>%
  left_join(det_c, by = 'id') %>%
  filter(G_unit > 0) %>%
  mutate(h_i = coalesce(mtr_net_worth, 0) * coalesce(mtr_kg_lt, 0),
         age_band = cut(age_cohort, c(17, 40, 50, 60, 70, 80),
                        labels = c('18-40', '41-50', '51-60', '61-70',
                                   '71-80'))) %>%
  group_by(age_band) %>%
  summarise(hbar_gw = sum(weight * G_unit * h_i) / sum(weight * G_unit),
            h_pos_gain_share = sum(weight * G_unit * (h_i > 0)) /
                               sum(weight * G_unit),
            .groups = 'drop') %>%
  write_csv('other/kg_model_tests/t6_h_by_age_corner.csv')

if (!all(results$understates)) {
  stop('T6: smear does not understate the record-level response ',
       '(Jensen direction violated) — a correctness invariant, investigate.')
}
# Magnitude gate RULED 2026-07-12 (author): breach ACCEPTED. The smear's
# error is one-signed (conservative) and the exposed/unexposed two-state
# split — which closes it to <=1.5% (rel_error_2state above) — was declined
# as not worth the extra state. Report, don't stop. Ruling recorded in
# other/top_tax/margins_gap_assessment.md §1.1.
if (!all(results$within_gate)) {
  cat('\nNOTE: smear error exceeds the original plan gate at lever(s): ',
      paste(results$lever[!results$within_gate], collapse = ', '),
      ' — breach RULED ACCEPTED 2026-07-12 (conservative; disclosed).\n',
      sep = '')
}
cat('\nT6 SMEAR BENCHMARK COMPLETE (direction invariant holds)\n')
