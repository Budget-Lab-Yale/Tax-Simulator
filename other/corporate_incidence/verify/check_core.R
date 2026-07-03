#-------------------------------------------------------------------------------
# check_core.R
#
# Verification for the corp_incidence.R pure core (build-order commit 2):
#   1. the whole src tree (incl. the new module) sources cleanly,
#   2. corp_selfcheck_paths() passes (permanent-constant mu, rent-share floor
#      decay, windowed expiry, priced-as-permanent corner, telescoping,
#      pre-enactment inertness),
#   3. the seesaw guard accepts a rate-like wedge and rejects a
#      depreciation-signature wedge (01_bonus shape),
#   4. corporate_meta.yaml validation: absent -> NULL; valid -> list;
#      invalid (net-of-offset / wrong provision type / bad horizon) -> stop.
#
# Run via sbatch (never on the login node):
#   sbatch other/corporate_incidence/verify/check_core.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)

return_vars <<- list()
list.files('./src', recursive = TRUE) %>%
  walk(.f = ~ {
    if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/')) {
      source(file.path('./src/', .x))
    }
  })
message('OK: src tree sourced (corp_incidence.R included)')

# --- 2. path-property self-checks --------------------------------------------
corp_selfcheck_paths()

# --- 3. seesaw guard ----------------------------------------------------------
rate_like = c(0, 0, 337, 350, 364, 378, 393, 409, 425, 442, 0.2, -0.1)
stopifnot(isTRUE(corp_seesaw_check(rate_like)$ok))

bonus_like = c(-61, -40, -12, 10, 35, 30, 22, 15, 8, 3)
stopifnot(isFALSE(corp_seesaw_check(bonus_like)$ok))
message('OK: seesaw guard (rate path accepted, depreciation signature rejected)')

# --- 4. metadata contract ------------------------------------------------------
tmp = file.path(tempdir(), 'ome_fixture')
dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

# absent -> NULL
stopifnot(is.null(corp_read_meta(tmp)))

# valid -> list
writeLines(c('gross_of_offset: true',
             'provision_type: rate',
             'beyond_horizon: zero',
             'produced_by: check_core fixture'),
           file.path(tmp, 'corporate_meta.yaml'))
m = corp_read_meta(tmp)
stopifnot(is.list(m), identical(m$beyond_horizon, 'zero'))

# invalid: declared net-of-offset -> hard stop
writeLines(c('gross_of_offset: false',
             'provision_type: rate',
             'beyond_horizon: extend'),
           file.path(tmp, 'corporate_meta.yaml'))
r = tryCatch({ corp_read_meta(tmp); 'no-error' }, error = function(e) 'error')
stopifnot(identical(r, 'error'))

# invalid: depreciation provision type -> hard stop
writeLines(c('gross_of_offset: true',
             'provision_type: cost_recovery',
             'beyond_horizon: extend'),
           file.path(tmp, 'corporate_meta.yaml'))
r = tryCatch({ corp_read_meta(tmp); 'no-error' }, error = function(e) 'error')
stopifnot(identical(r, 'error'))

# invalid: bad beyond_horizon -> hard stop
writeLines(c('gross_of_offset: true',
             'provision_type: rate',
             'beyond_horizon: forever'),
           file.path(tmp, 'corporate_meta.yaml'))
r = tryCatch({ corp_read_meta(tmp); 'no-error' }, error = function(e) 'error')
stopifnot(identical(r, 'error'))
message('OK: corporate_meta.yaml contract (absent -> NULL, valid -> list, ',
        'invalid -> hard stop)')

# --- 5. rollover ramp against the real schedule --------------------------------
roll = corp_rollover_ramp()
stopifnot(all(roll(c(-3, 0)) == 0),
          abs(roll(1) - 0.336) < 0.01,
          roll(50) == 1,
          !is.unsorted(roll(0:30)))
message('OK: debt rollover ramp (0 at enactment, ~0.34 at t+1, 1 at horizon)')

# --- 6. record applier on a synthetic frame ------------------------------------
years_all = 2024:2200
pi0 = 4000; gg = 0.035
macro_syn = tibble(
  year = years_all,
  pi_at = pi0 * (1 + gg)^(years_all - min(years_all)),
  tsy_10y = 4.2,
  gdp_interest    = 2000 * (1 + gg)^(years_all - min(years_all)),
  gdp_rent        = 1100 * (1 + gg)^(years_all - min(years_all)),
  gdp_proprietors = 2200 * (1 + gg)^(years_all - min(years_all))
)
wedge_syn = tibble(year = 2024:2040,
                   w = if_else(year >= 2026,
                               0.05 * macro_syn$pi_at[match(year, macro_syn$year)],
                               0))
paths_syn = corp_build_paths_core(
  wedge_syn, macro_syn, sim_years = 2025:2036, beyond_horizon = 'extend',
  sigma_n = 0.375, kappa = 0.4,
  roll_fn = function(t) pmin(pmax(t, 0) / 10, 1),
  pt_weight = WEALTH_CAP_FLOWS_PT_WEIGHT)

mk_frame = function() {
  f = tibble(
    id = 1:4, weight = 1, filing_status = c(1, 2, 1, 2),
    div_ord = c(100, 0, 50, 0), div_pref = c(20, 0, 0, 0),
    txbl_int = c(30, 10, 0, 0), exempt_int = c(5, 0, 0, 0),
    rent = c(40, 0, 0, 0), rent_loss = c(10, 0, 0, 0),
    sole_prop = c(0, 200, 0, 0),
    part_active = c(0, 50, 0, 0), part_passive = 0,
    part_active_loss = c(0, 20, 0, 0), part_passive_loss = 0, part_179 = 0,
    scorp_active = 0, scorp_passive = 0,
    scorp_active_loss = 0, scorp_passive_loss = 0, scorp_179 = 0,
    farm = 0, part_se1 = c(0, 30, 0, 0), part_se2 = 0,
    sole_prop1 = c(0, 200, 0, 0), sole_prop2 = 0, farm1 = 0, farm2 = 0,
    kg_lt = c(500, 0, -50, 0), kg_lt_basis = c(300, 0, 100, 0),
    kg_st = c(60, 0, 0, 0),
    txbl_ira_dist = c(80, 0, 0, 40), txbl_pens_dist = c(70, 0, 0, 90),
    gross_pens_dist = c(75, 0, 0, 95)
  )
  for (a in ESTATE_ASSET_COLS) f[[a]] = 0
  for (d in WEALTH_DEBT_COLS)  f[[d]] = 0
  f$value.equities = c(10000, 0, 2000, 0)
  f$value.dc       = c(3000, 0, 0, 0)      # record 4: DB only
  f$value.db       = c(1000, 0, 0, 5000)
  f$value.trusts   = c(500, 0, 0, 0)
  f$value.re_fund  = c(200, 0, 0, 0)
  f$value.cash     = c(1000, 100, 100, 100)
  f$value.pass_throughs = c(0, 4000, 0, 0)
  f$value.primary_mortgage = c(2000, 0, 0, 0)
  f$net_worth = rowSums(as.matrix(f[ESTATE_ASSET_COLS])) -
                rowSums(as.matrix(f[WEALTH_DEBT_COLS]))
  f
}

fr = mk_frame()
yr = 2027
p  = paths_syn$sim[match(yr, paths_syn$sim$year), ]
out = corp_apply_to_records(fr, paths_syn, yr, kg_dynamics_active = FALSE)

# (a) analytic dY_exog identity, hand-recomputed
pt_net_1 = 0; pt_net_2 = 200 + (50 - 20)
dY_hand = c(
  (p$fac_div - 1) * 120 + (p$fac_int - 1) * 35 + (p$fac_rent - 1) * 30,
  (p$fac_int - 1) * 10 + (p$fac_pt - 1) * pt_net_2,
  (p$fac_div - 1) * 50,
  0)
stopifnot(max(abs(out$corp_dY_exog - dY_hand)) < 1e-9)
stopifnot(all(out$corp_dY_exog[c(1, 2, 3)] < 0))          # P8: hike -> income cut

# (b) stocks: exposure-weighted markdown; unexposed untouched; basis fixed
stopifnot(abs(out$value.equities[1] - 10000 * (1 - 1.00 * p$mu)) < 1e-9,
          abs(out$value.dc[1]       -  3000 * (1 - 0.55 * p$mu)) < 1e-9,
          abs(out$value.trusts[1]   -   500 * (1 - 0.50 * p$mu)) < 1e-9,
          abs(out$value.re_fund[1]  -   200 * (1 - 0.30 * p$mu)) < 1e-9,
          out$value.db[4] == 5000,                # DB never debited (D10)
          out$value.cash[1] == 1000,
          out$value.pass_throughs[2] == 4000)     # pt stocks flows-only (P14)
nw_hand = rowSums(as.matrix(out[ESTATE_ASSET_COLS])) -
          rowSums(as.matrix(out[WEALTH_DEBT_COLS]))
stopifnot(max(abs(out$net_worth - nw_hand)) < 1e-9)

# (c) kg forms (non-kg run): D18 exact per-record form
kg1_hand = 500 + CORP_OMEGA_KG * (p$phi * 500 - p$mu * (500 + 300))
kg3_hand = -50 + CORP_OMEGA_KG * (p$phi * -50 - p$mu * pmax(-50 + 100, 0))
stopifnot(abs(out$kg_lt[1] - kg1_hand) < 1e-9,
          abs(out$kg_lt[3] - kg3_hand) < 1e-9,
          abs(out$kg_lt_basis[1] - 300 * (1 + CORP_OMEGA_KG * p$phi)) < 1e-9,
          abs(out$kg_st[1] - 60 * (1 + CORP_OMEGA_KG * p$phi)) < 1e-9)

# (d) retirement: ira at full dc markdown; pens at dc-share; DB-only untouched
omega_dc = unname(CORP_ASSET_EXPOSURE['value.dc'])
dc_share1 = 3000 / 4000
stopifnot(abs(out$txbl_ira_dist[1] - 80 * (1 - omega_dc * p$mu)) < 1e-9,
          abs(out$txbl_pens_dist[1] - 70 * (1 - omega_dc * p$mu * dc_share1)) < 1e-9,
          abs(out$gross_pens_dist[1] - 75 * (1 - omega_dc * p$mu * dc_share1)) < 1e-9,
          out$txbl_pens_dist[4] == 90, out$gross_pens_dist[4] == 95,
          out$txbl_ira_dist[4] == 40)

# (e) kg run: kg columns untouched (state debit + post-behavior phi instead)
out_kg = corp_apply_to_records(fr, paths_syn, yr, kg_dynamics_active = TRUE)
stopifnot(identical(out_kg$kg_lt, fr$kg_lt),
          identical(out_kg$kg_lt_basis, fr$kg_lt_basis),
          identical(out_kg$kg_st, fr$kg_st),
          abs(out_kg$value.equities[1] - 10000 * (1 - p$mu)) < 1e-9)

# (f) pre-enactment year: byte-identical frame, no diagnostic columns
out_pre = corp_apply_to_records(fr, paths_syn, 2025, kg_dynamics_active = FALSE)
stopifnot(identical(out_pre, fr))
message('OK: record applier (dY_exog identity, exposure markdown, D18 kg ',
        'forms, P7 retirement split, kg-run skip, pre-enactment dormancy)')

# --- 7. kg_dynamics glue --------------------------------------------------------
# (a) exposed-value helper: equities at 1.0 + re_fund at 0.30; dc/trusts are
#     exposed assets but NOT kg classes, so they must not enter
v_hand = 1.00 * fr$value.equities + 0.30 * fr$value.re_fund
stopifnot(max(abs(corp_kg_state_exposed_value(fr) - v_hand)) < 1e-9)

# (b) quantity term: kg_lt / kg_st / kg_lt_basis by (1 + omega_kg * phi);
#     deemed columns untouched; inert year identical
fr_kg = fr %>% mutate(kg_deemed_full = c(10, 0, 0, 0), kg_deemed = c(1, 0, 0, 0))
out_q = corp_apply_kg_quantity_to_records(fr_kg, paths_syn, yr)
fac_q = 1 + CORP_OMEGA_KG * p$phi
stopifnot(abs(out_q$kg_lt[1] - 500 * fac_q) < 1e-9,
          abs(out_q$kg_st[1] - 60 * fac_q) < 1e-9,
          abs(out_q$kg_lt_basis[1] - 300 * fac_q) < 1e-9,
          identical(out_q$kg_deemed_full, fr_kg$kg_deemed_full),
          identical(out_q$div_ord, fr_kg$div_ord))
stopifnot(identical(corp_apply_kg_quantity_to_records(fr_kg, paths_syn, 2025),
                    fr_kg))

# (c) cell-table debit wiring: extra_R uses (dG - corp_gain_debit),
#     deemed_factor stays on clean dG
ages_bt = 18:80
ages_chr = as.character(ages_bt)
nA = length(ages_bt)
bt_syn = tibble(
  age = ages_bt, G_B = 1000, R_B = 50, r_B = 0.05, m = 0.01,
  mG_record = 10, mR_record = 0.5,
  p_char = 0, p_char_extensive = 0, p_char_intensive = 0,
  estate_2026_m_avg_dgw = NA_real_,
  G_B_equities = 600, G_B_pass_throughs = 200, G_B_primary_home = 100,
  G_B_other_home = 50, G_B_re_fund = 50, G_B_primary_above_cap = 80,
  V_corp_exposed = 800)
vec  = function(x) setNames(rep(x, nA), ages_chr)
mix_syn = tibble(age = ages_bt, delta_vanish = 1, delta_route = 0,
                 delta_realize = 0, c_phi = 0)
debit_syn = vec(40)
ct = kg_dyn_build_cell_table(
  baseline_t = bt_syn, year_idx = 1,
  r_S_vec = vec(0.05), lambda_I_vec = vec(0.01),
  r_V_B_vec = vec(0.04), r_V_S_vec = vec(0.04),
  delta_prev = vec(100),
  tau_B_col = vec(0.2), tau_S_col = vec(0.25),
  W_B_col = vec(1), W_S_col = vec(1), MC_B_col = vec(0.1), MC_S_col = vec(0.1),
  kappa_col = vec(0.5), r_D_B_col = vec(0.05), r_D_S_col = vec(0.05),
  regime_mix = mix_syn, corp_debit = debit_syn)
stopifnot(max(abs(ct$extra_R - 0.05 * (100 - 40))) < 1e-12,
          max(abs(ct$deemed_factor - (1000 + 100) / 1000)) < 1e-12,
          max(abs(ct$corp_gain_debit - 40)) < 1e-12)
ct0 = kg_dyn_build_cell_table(
  baseline_t = bt_syn, year_idx = 1,
  r_S_vec = vec(0.05), lambda_I_vec = vec(0.01),
  r_V_B_vec = vec(0.04), r_V_S_vec = vec(0.04),
  delta_prev = vec(100),
  tau_B_col = vec(0.2), tau_S_col = vec(0.25),
  W_B_col = vec(1), W_S_col = vec(1), MC_B_col = vec(0.1), MC_S_col = vec(0.1),
  kappa_col = vec(0.5), r_D_B_col = vec(0.05), r_D_S_col = vec(0.05),
  regime_mix = mix_syn)
stopifnot(max(abs(ct0$extra_R - 0.05 * 100)) < 1e-12,
          all(ct0$corp_gain_debit == 0))
message('OK: kg glue (exposed value, phi quantity term + inert year, ',
        'cell-table debit in extra_R only)')

# --- 8. conservation diagnostic -------------------------------------------------
diag_root = file.path(tempdir(), 'diag_fixture')
d = corp_write_conservation_diag(pre = fr, post = out, paths = paths_syn,
                                 year = yr, conv_root = diag_root)
diag_file = file.path(diag_root, 'supplemental',
                      sprintf('corp_conservation_diag_%d.csv', yr))
stopifnot(file.exists(diag_file),
          abs(d$dY_total_realized - d$dY_total_analytic) < 1e-9,
          d$B_flow_hh > 0,                    # hike: household income burden +
          d$markdown_position_hh > 0,         # hike: positive markdown position
          is.finite(d$residual_unallocated))
# realized == analytic to machine precision on the clean synthetic frame; a
# deliberate line overwrite must trip the WARN
# corruption must be $B-scale to clear the diag's $0.05B tolerance floor
out_broken = out %>% mutate(div_ord = div_ord + 2e8)
warned = FALSE
withCallingHandlers(
  corp_write_conservation_diag(pre = fr, post = out_broken, paths = paths_syn,
                               year = yr, conv_root = diag_root),
  warning = function(w) { warned <<- TRUE; invokeRestart('muffleWarning') })
stopifnot(isTRUE(warned))
message('OK: conservation diagnostic (file written, realized == analytic, ',
        'signs, overwrite trips the WARN)')

message('ALL CORE CHECKS PASSED')
