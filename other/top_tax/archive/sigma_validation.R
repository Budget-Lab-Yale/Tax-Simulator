#-------------------------------------------------------------------------------
# sigma_validation.R
#
# Build item 2 (demoted to an INFORMAL validation check, DESIGN_LOCK ruling
# 2): the 2x2 own/cross elasticity matrix from the two perturbation legs —
# a +5pp top-ordinary-rate hike (tests/topord_plus5) and a CG mirror leg —
# eyeballed against the literature brackets. NOT a formal fail gate, NOT
# residual calibration; if it looks wrong we iterate on sigma / the pool.
#
# Literature brackets (plan Step 6):
#   own-ordinary ETI              : SSG 0.12 - 0.40
#   ord <- CG cross               : Mortenson -0.24 .. -2.4 (expect BELOW face)
#   own-gains                     : Dowd/Mortenson -0.8 .. -0.9
#   gains <- ord cross            : positive, persistent, << +2.77
#
# Usage (sbatch, from repo root):
#   Rscript other/top_tax/sigma_validation.R <output_root> <ord_leg_id> \
#           <cg_leg_id> <first_year> <last_year>
# e.g.
#   Rscript other/top_tax/sigma_validation.R \
#     /nfs/roberts/scratch/.../v1/<vintage> topord_plus5 cg_mirror 2025 2035
#
# Writes: other/top_tax/sigma_validation_out/{elasticity_matrix.csv,
#         composition.csv, wedge_symmetry.csv, sigma_validation.md}
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop('Usage: Rscript sigma_validation.R <output_root> <ord_leg_id> ',
       '<cg_leg_id> <first_year> <last_year>')
}
root     = args[1]
ord_leg  = args[2]
cg_leg   = args[3]
years    = as.integer(args[4]):as.integer(args[5])
out_dir  = 'other/top_tax/sigma_validation_out'
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DETAIL_COLS = c('id', 'weight', 'filing_status', 'txbl_inc', 'txbl_kg',
                'kg_lt', 'wages', 'wages1', 'wages2', 'liab_iit_net')

read_detail = function(scenario, pass, year, cols = DETAIL_COLS) {
  fread(file.path(root, scenario, pass, 'detail', paste0(year, '.csv')),
        select = cols, showProgress = FALSE) %>% as_tibble()
}

read_sigma_tracker = function(scenario, year) {
  p = file.path(root, scenario, 'conventional', 'supplemental',
                'kg_dynamics_state', paste0(year, '.rds'))
  if (!file.exists(p)) return(NULL)
  readRDS(p)$sigma
}

#-------------------------------------------------------------------------------
# Per-leg, per-year aggregates. Income concepts:
#   O  = ordinary taxable income excl. net gains (ETI-literature concept):
#        sum w * pmax(txbl_inc - pmax(txbl_kg, 0), 0), computed on the TOP
#        subset (baseline txbl_inc >= the leg's gate threshold, fixed at
#        baseline membership so composition doesn't contaminate the delta)
#   R  = LT gains realizations: sum w * pmax(kg_lt, 0), all records
# The known baseline-on-baseline wages residual (wages != wages1 + wages2;
# do_taxes rebuilds wages = wages1 + wages2 on the behavior path) is netted
# out of O by rebuilding the same concept on both frames from the LEGS
# rather than the wages aggregate wherever wages enters txbl_inc — since we
# read txbl_inc directly, we instead SUBTRACT the residual effect measured
# as (wages - wages1 - wages2) on the baseline frame from the reform-side O
# delta (the conventional frame has residual 0 after the rebuild).
#-------------------------------------------------------------------------------

top_threshold = function(tracker) {
  # gate thresholds persisted in the sigma tracker (filing-status keyed)
  tracker$thresholds
}

leg_aggregates = function(scenario) {
  map_dfr(years, function(t) {
    base = read_detail('baseline', 'static', t)
    conv = read_detail(scenario, 'conventional', t)
    stopifnot(identical(base$id, conv$id))

    tr = read_sigma_tracker(scenario, t)
    th = if (!is.null(tr)) top_threshold(tr) else
      tibble(filing_status = c(1, 2, 3, 4), sigma_thresh = Inf)

    base = base %>% left_join(th, by = 'filing_status')
    top  = !is.na(base$sigma_thresh) & base$txbl_inc >= base$sigma_thresh

    O_of = function(d, sel) with(d[sel, ],
      sum(weight * pmax(txbl_inc - pmax(txbl_kg, 0), 0)))
    R_of = function(d) with(d, sum(weight * pmax(kg_lt, 0)))

    # wages residual on the baseline frame among the top subset (the
    # conventional frame rebuilds wages = wages1 + wages2, so the reform
    # O delta embeds -residual spuriously; add it back).
    resid_top = with(base[top, ], sum(weight * (wages - wages1 - wages2)))

    tibble(
      year        = t,
      O_base_top  = O_of(base, top),
      O_conv_top  = O_of(conv, top) + resid_top,
      R_base      = R_of(base),
      R_conv      = R_of(conv),
      conv_inflow = if (!is.null(tr)) tr$conv_total else NA_real_,
      pool_dollars = if (!is.null(tr)) tr$pool_dollars else NA_real_,
      pool_weighted = if (!is.null(tr)) tr$pool_weighted else NA_real_,
      conv_wages  = if (!is.null(tr)) tr$conv_dollars_wages else NA_real_,
      conv_pt     = if (!is.null(tr)) tr$conv_dollars_pt else NA_real_,
      mean_dW     = if (!is.null(tr)) tr$mean_dW_pooled else NA_real_,
      mean_dtau_eq = if (!is.null(tr)) tr$mean_dtau_eq else NA_real_
    )
  })
}

# Rate changes for the net-of-tax denominators, measured as dollar-weighted
# mean MTR changes on the relevant base (static reform vs baseline static
# detail: the law-only price change).
leg_rate_change = function(scenario) {
  map_dfr(years, function(t) {
    b = read_detail('baseline', 'static', t,
                    cols = c('id', 'weight', 'txbl_inc', 'kg_lt', 'wages',
                             'mtr_wages1', 'mtr_kg_lt'))
    s = read_detail(scenario, 'static', t,
                    cols = c('id', 'mtr_wages1', 'mtr_kg_lt'))
    j = b %>% left_join(s, by = 'id', suffix = c('_b', '_s'))
    w_ord = pmax(j$wages, 0); w_kg = pmax(j$kg_lt, 0)
    tibble(
      year      = t,
      dtau_ord  = with(j, sum(w_ord * (mtr_wages1_s - mtr_wages1_b), na.rm = TRUE) /
                          sum(w_ord[!is.na(mtr_wages1_s - mtr_wages1_b)])),
      tau_ord_b = with(j, sum(w_ord * mtr_wages1_b, na.rm = TRUE) /
                          sum(w_ord[!is.na(mtr_wages1_b)])),
      dtau_cg   = with(j, sum(w_kg * (mtr_kg_lt_s - mtr_kg_lt_b), na.rm = TRUE) /
                          sum(w_kg[!is.na(mtr_kg_lt_s - mtr_kg_lt_b)])),
      tau_cg_b  = with(j, sum(w_kg * mtr_kg_lt_b, na.rm = TRUE) /
                          sum(w_kg[!is.na(mtr_kg_lt_b)]))
    )
  })
}

elasticity = function(y_conv, y_base, tau_b, dtau) {
  # net-of-tax elasticity: dlog(Y) / dlog(1 - tau)
  dlog_y   = log(y_conv / y_base)
  dlog_ntr = log((1 - (tau_b + dtau)) / (1 - tau_b))
  ifelse(abs(dlog_ntr) > 1e-12, dlog_y / dlog_ntr, NA_real_)
}

cat('Reading ordinary leg:', ord_leg, '\n')
agg_ord  = leg_aggregates(ord_leg)
rate_ord = leg_rate_change(ord_leg)
cat('Reading CG leg:', cg_leg, '\n')
agg_cg   = leg_aggregates(cg_leg)
rate_cg  = leg_rate_change(cg_leg)

# Net-of-tax-to-rate-form conversion factor for a discrete change:
# dlog(1-tau)/dlog(tau). Multiplying an NOT-form elasticity by this gives
# the rate-form elasticity (dlogY/dlog tau), the convention of the
# gains-side literature brackets (Dowd/Mortenson).
rate_factor = function(tau_b, dtau) {
  ifelse(abs(dtau) > 1e-12,
         log((1 - (tau_b + dtau)) / (1 - tau_b)) / log((tau_b + dtau) / tau_b),
         NA_real_)
}

mat = bind_rows(
  agg_ord %>% left_join(rate_ord, by = 'year') %>%
    transmute(year, leg = 'tau_ord', tau_b = tau_ord_b, dtau = dtau_ord,
              e_ordinary = elasticity(O_conv_top, O_base_top, tau_ord_b, dtau_ord),
              e_gains    = elasticity(R_conv, R_base, tau_ord_b, dtau_ord),
              e_gains_rateform = e_gains * rate_factor(tau_ord_b, dtau_ord)),
  agg_cg %>% left_join(rate_cg, by = 'year') %>%
    transmute(year, leg = 'tau_cg', tau_b = tau_cg_b, dtau = dtau_cg,
              e_ordinary = elasticity(O_conv_top, O_base_top, tau_cg_b, dtau_cg),
              e_gains    = elasticity(R_conv, R_base, tau_cg_b, dtau_cg),
              e_gains_rateform = e_gains * rate_factor(tau_cg_b, dtau_cg))
)
write_csv(mat, file.path(out_dir, 'elasticity_matrix.csv'))
write_csv(bind_rows(rate_ord %>% mutate(leg = 'tau_ord'),
                    rate_cg  %>% mutate(leg = 'tau_cg')),
          file.path(out_dir, 'rate_changes.csv'))

composition = bind_rows(
  agg_ord %>% mutate(leg = 'tau_ord'),
  agg_cg  %>% mutate(leg = 'tau_cg')) %>%
  select(leg, year, pool_dollars, pool_weighted, conv_inflow, conv_wages,
         conv_pt, mean_dW, mean_dtau_eq)
write_csv(composition, file.path(out_dir, 'composition.csv'))

# Symmetry check in pool-weighted wedge units: mean_dW per statutory pp,
# compared across legs.
symmetry = composition %>%
  group_by(leg) %>%
  summarise(mean_dW = mean(mean_dW, na.rm = TRUE),
            mean_dtau_eq = mean(mean_dtau_eq, na.rm = TRUE),
            mean_conv = mean(conv_inflow, na.rm = TRUE), .groups = 'drop')
write_csv(symmetry, file.path(out_dir, 'wedge_symmetry.csv'))

# Markdown summary with the literature brackets alongside, conventions
# stated explicitly: ordinary-income rows in NET-OF-TAX form (the ETI /
# Mortenson Table 5 convention, dlogY/dlog(1-tau)); gains rows in RATE form
# (the realization-literature convention, dlogR/dlog tau).
summary_years = years[years >= min(years) + 1]  # drop lead-in year
sm = mat %>% filter(year %in% summary_years) %>%
  group_by(leg) %>%
  summarise(e_ordinary = mean(e_ordinary, na.rm = TRUE),
            e_gains    = mean(e_gains, na.rm = TRUE),
            e_gains_rateform = mean(e_gains_rateform, na.rm = TRUE),
            .groups = 'drop')

md = c(
  '# sigma validation: 2x2 own/cross elasticity matrix',
  '',
  sprintf('Root: `%s`; legs: `%s` (ordinary), `%s` (CG); years %d-%d;',
          root, ord_leg, cg_leg, min(years), max(years)),
  sprintf('averages over %d-%d (lead-in year dropped). INFORMAL check',
          min(summary_years), max(summary_years)),
  '(DESIGN_LOCK ruling 2) - eyeball vs brackets, iterate sigma/pool if off.',
  '',
  'Conventions: ordinary-income rows are NET-OF-TAX-form elasticities',
  '(dlogY/dlog(1-tau), the ETI and Mortenson-Table-5 convention); gains rows',
  'are RATE-form (dlogR/dlog tau, the realization-literature convention),',
  'converted from the estimated NOT-form via dlog(1-tau)/dlog(tau).',
  '',
  '| response \\ leg | tau_ord leg | bracket | tau_cg leg | bracket |',
  '|---|---|---|---|---|',
  sprintf('| ordinary income, top subset (NOT-form) | %.3f | SSG ETI 0.12-0.40 | %.3f | Mortenson cross face -0.77 (range -0.24..-2.4); expect BELOW face |',
          sm$e_ordinary[sm$leg == 'tau_ord'], sm$e_ordinary[sm$leg == 'tau_cg']),
  sprintf('| LT gains realizations (RATE-form) | %.3f | positive, persistent, << +2.77 (Mortenson face) | %.3f | Dowd/Mortenson own-gains -0.8..-0.9 |',
          sm$e_gains_rateform[sm$leg == 'tau_ord'],
          sm$e_gains_rateform[sm$leg == 'tau_cg']),
  '',
  '## Composition (sigma tracker)',
  '',
  'See composition.csv: conversion inflow by year, wages-vs-PT split, pool',
  'size, mean pooled wedge, mean equity-leg change. The corporate-base leg',
  'of diverted compensation is the entity-shifting module (conservation',
  'diagnostics in the Phase 2C logs).',
  '',
  '## Symmetry',
  '',
  'wedge_symmetry.csv states each leg in pool-weighted wedge units',
  '(mean_dW per leg; the CG leg was sized from leg 1 pool-weighted dW).'
)
writeLines(md, file.path(out_dir, 'sigma_validation.md'))

cat('\nWrote:\n  ', file.path(out_dir, 'elasticity_matrix.csv'), '\n  ',
    file.path(out_dir, 'composition.csv'), '\n  ',
    file.path(out_dir, 'wedge_symmetry.csv'), '\n  ',
    file.path(out_dir, 'sigma_validation.md'), '\n')
print(as.data.frame(sm))
