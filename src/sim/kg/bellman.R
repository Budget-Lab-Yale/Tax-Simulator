#-------------------------------------------------------------------------------
# bellman.R
#
# Bellman backward induction and the tau / grid packing it consumes.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Bellman backward induction. Outer loop backward in time, inner loop
# backward in age, on the extended grid [18, 119]. Terminal condition
# W[A_max+1, .] = 0 (m(119) ~ 1, binds quickly). At year t_max we seed
# W[, t_max+1] with a stationary backward sweep using t_max primitives.
# Pass 1 solves baseline and recovers kappa; Pass 2 solves the scenario
# from that kappa via the clipped quadratic FOC.
#-------------------------------------------------------------------------------

kg_dyn_pack_tau = function(tau_list, years,
                            ages_bellman = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN) {

  # Packs per-year tau vectors (over [18, 80]) into a matrix on the
  # Bellman grid; ages 81+ inherit tau(80).

  out = matrix(0, length(ages_bellman), length(years),
               dimnames = list(as.character(ages_bellman), as.character(years)))
  for (t in years) {
    key = as.character(t)
    v = tau_list[[key]]
    out[as.character(KG_DYN_AGE_MIN:KG_DYN_AGE_MAX), key] = as.numeric(v)
    out[as.character((KG_DYN_AGE_MAX + 1):KG_DYN_AGE_MAX_BELLMAN), key] =
      as.numeric(v[as.character(KG_DYN_AGE_MAX)])
  }
  out
}



kg_dyn_pack_baseline_grid = function(grid_ext, years,
                                     ages_bellman = KG_DYN_AGE_MIN:
                                                    KG_DYN_AGE_MAX_BELLMAN) {

  # Packs per-year (m, r_B) into [age, year] matrices for the Bellman.
  #
  # m is gain-stock-weighted: sum(w*m_household*G_unit) / sum(w*G_unit).
  # The Bellman is normalized per dollar of unrealized gain, so it needs
  # the probability that the *dollar's* holder dies, not the average
  # taxpayer in the cell. (Taxpayer-weighted m is biased upward 2-3x in
  # practice since wealthier holders die less.) Falls back to taxpayer-
  # weighted m where G_B = 0 — i.e., 81+ on the extended grid, where
  # grid_ext$m is the SSA life-table mortality anyway.

  n_ages  = length(ages_bellman)
  n_years = length(years)
  ages_chr = as.character(ages_bellman)
  years_chr = as.character(years)

  m   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_B = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  p_char = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  for (t_chr in years_chr) {
    bt = grid_ext[[t_chr]]
    m_gw = ifelse(bt$G_B > 0, bt$mG_record / bt$G_B, bt$m)
    m  [, t_chr] = pmin(pmax(m_gw, 0), 1)
    r_B[, t_chr] = bt$r_B
    p_char[, t_chr] = pmin(pmax(bt$p_char, 0), 1)
  }
  list(m = m, r_B = r_B, p_char = p_char)
}



kg_dyn_bellman_sweep_age = function(W_next, m_col, r_B_col, tau_col,
                                     c_phi_col, p_char_col, eta, beta,
                                     kappa_col = NULL, stationary = FALSE,
                                     h_col = NULL, e_col = NULL,
                                     form = kg_dyn_response_form()) {

  # One age-backward sweep through [a_min, a_max] for a single year column.
  #
  # c_phi_col is a length-n_ages vector of cell-level burden shares
  # (built by kg_dyn_build_regime_mix on the bathtub grid then extended to
  # the Bellman grid in kg_dyn_run_bathtub_pass by repeating the age-80
  # value forward — same pattern as tau_col).
  #
  # h_col is the per-cell wealth-tax CARRYING COST of one more year of
  # deferral: the gain-weighted mean of the RECORD-LEVEL product
  # tau_w,i * tau_cg,i (kg_dyn_aggregate_cell_carry — never a product of
  # separately averaged rates, Cov(tau_w, tau_cg) > 0 at the top). It
  # debits the SURVIVOR continuation: death_cont = bs*(W_next - h) + bm*F.
  # Timing convention: the wealth tax is assessed on END-OF-YEAR holdings,
  # so in-year realizers and decedents pay no carrying cost for that year
  # (their deferred liability has left the wealth base by assessment) —
  # only continued deferral into next year is charged. Top-age SURVIVORS
  # still pay h (the -h term persists at W_next = 0). NULL = zeros
  # (baseline pass, and every scenario without an active wealth tax).
  #
  # e_col is the per-cell ESTATE EXPOSURE of the death value: the
  # gain-weighted mean of the RECORD-LEVEL switch-gated marginal estate
  # rate mtr_estate_ded (kg_dyn_aggregate_cell_estate), clamped to [0, 1].
  # It discounts the death-CG value: F = (1 - c_phi_eff) * tau * (1 - e).
  # Economics: for an estate-taxable cell, CG tax paid (or forgiven) at
  # death interacts with the estate base through the Sec. 2053-style
  # deduction -- realizing during life shrinks the future estate while
  # dying with the gain forgoes (under deemed) or captures (under step-up)
  # that offset, so the net death-forgiveness value is tau * (1 - e).
  # Inert under deemed (c_phi_eff = 1 => F = 0 regardless); bites hardest
  # under step-up; scales carryover. Below-exemption cells have e = 0
  # (exact no-op). UNLIKE h_col, e is LEG-PAIRED: Pass 1 receives the
  # baseline-law exposure e_B (current law HAS an estate tax, so e_B > 0
  # for top cells) and Pass 2 the reform-law e_S -- a single shared matrix
  # would zero out estate-only reforms (e_S > e_B with tau unchanged =>
  # MC_S = MC_B => no response), the exact margin this term exists to
  # price. NULL = zeros (isolated solver unit tests only).
  #
  # form selects the realization cost primitive (see kg_dyn_response_form).
  # BOTH have C'(r_D_B) = 0, so Pass 1 is form-invariant (kappa = MC exactly)
  # and the levels path is bit-identical to the pre-toggle code:
  #   'levels' -- ENTROPY / KL cost C(r_D) = (1/eta)*[r_D*ln(r_D/r_D_B) - r_D +
  #       r_D_B], FOC closed form r_D = r_D_B*exp(-eta*(MC - MC_B)) (constant
  #       semi-elasticity in the wedge MC).
  #   'logs' -- power cost C(r_D) = (1 - MC_B)*[(eta/(eta+1))*r_D_B*
  #       (r_D/r_D_B)^((eta+1)/eta) - r_D + r_D_B/(eta+1)], FOC closed form
  #       r_D = r_D_B*((1 - MC)/(1 - MC_B))^eta (constant net-of-tax elasticity
  #       in 1 - MC). Hard-stops if any cell's MC or MC_B >= KG_DYN_LOGS_MC_CAP
  #       (the (1 - MC)^eta base would go non-positive; never clamps silently).
  # Both anchored at r_D_B = clip(r_B, 0, 1) (single pool: whole rate
  # discretionary), clipped to [0, 1]; r_D_B = 0 cells stay 0.
  #
  # kappa_col = NULL: Pass 1 (baseline). r_D = r_D_B; kappa = MC exactly.
  # kappa_col supplied: Pass 2 (scenario). kappa carries MC_B.
  #
  # stationary = TRUE seeds the terminal year by pulling W[i+1] from the
  # same sweep (year-t_max primitives constant forward); otherwise uses
  # W_next[i+1]. At i == n_ages the continuation is 0 (W[A_max+1] = 0).

  n_ages = length(m_col)
  W     = numeric(n_ages)
  MC    = numeric(n_ages)
  r_D   = numeric(n_ages)
  kappa = numeric(n_ages)

  is_baseline_pass = is.null(kappa_col)
  if (!form %in% c('levels', 'logs'))
    stop(sprintf("kg_dynamics: form must be 'levels' or 'logs'; got '%s'.", form))

  # Precompute age-vector quantities used inside the loop.
  # Charity peels proportionally from the regime mix (not preferentially from
  # the would-be-taxed share). Bathtub split in kg_dyn_step_recurrence uses
  # the same assumption.
  c_phi_eff    = c_phi_col * (1 - pmin(pmax(p_char_col, 0), 1))
  if (is.null(e_col)) e_col = numeric(n_ages)
  # Estate offset on the death value (leg-paired e; see docstring). The
  # aggregator clamps e to [0, 1], so (1 - e_col) can never flip F's sign.
  F_vec        = (1 - c_phi_eff) * tau_col * (1 - e_col)
  # Single pool: the WHOLE baseline rate is discretionary (no r_exog carve-out),
  # so the realization cap is 1 and the entropy cost's reference point is r_B.
  r_D_cap_vec  = rep(1, length(r_B_col))
  # Baseline discretionary rate = the entropy cost's reference point. Both
  # passes recompute it from r_B_col (Pass 2 needs it for the exp response).
  r_D_B_vec    = pmin(pmax(r_B_col, 0), 1)
  bs_vec       = beta * (1 - m_col)   # survivor discount
  bm_vec       = beta * m_col         # death-state discount
  if (is.null(h_col)) h_col = numeric(n_ages)

  for (i in n_ages:1) {
    tau_i   = tau_col[i]
    F_i     = F_vec[i]
    r_D_B_i = r_D_B_vec[i]

    W_next_i = if (i == n_ages) 0 else if (stationary) W[i + 1] else W_next[i + 1]

    # Wealth-tax carrying cost rides the survivor branch only (see
    # docstring). MC and W pick it up consistently through death_cont;
    # h > 0 => MC_S < MC_B => r_D = r_D_B*exp(-eta*(MC_S - MC_B)) rises.
    death_cont = bs_vec[i] * (W_next_i - h_col[i]) + bm_vec[i] * F_i
    MC_i       = tau_i + death_cont
    r_D_cap    = r_D_cap_vec[i]

    # Net-of-tax domain guard: (1 - MC)^eta is undefined near MC = 1. Fail loud
    # (never clamp) if this cell's MC -- or its MC_B (kappa) in Pass 2 -- reaches
    # the cap. Levels is unconstrained, so this is skipped there entirely.
    if (form == 'logs') {
      mc_max = if (is_baseline_pass) MC_i else max(MC_i, kappa_col[i])
      if (mc_max >= KG_DYN_LOGS_MC_CAP) {
        stop(sprintf(paste0('kg_dynamics (logs form): marginal cost %.4f >= cap ',
                     '%.2f at age index %d (MC = %.4f, MC_B = %.4f). The ',
                     'net-of-tax response (1 - MC)^eta_tilde is undefined near ',
                     'MC = 1; refusing to clamp silently. Suspect a broken ',
                     'discount series or death-continuation, not a real economy.'),
                     mc_max, KG_DYN_LOGS_MC_CAP, i, MC_i,
                     if (is_baseline_pass) MC_i else kappa_col[i]))
      }
    }

    if (is_baseline_pass) {
      # C'(r_D_B) = 0 for BOTH cost forms => kappa = MC exactly (no premium).
      r_D_i   = r_D_B_i
      kappa_i = MC_i
    } else {
      # Scenario FOC closed form. kappa_col carries MC_B; only the upper clip
      # can bind, and r_D_B = 0 cells stay 0.
      kappa_i = kappa_col[i]
      if (form == 'levels') {
        r_D_i = if (r_D_B_i > 0)
                  min(r_D_B_i * exp(-eta * (MC_i - kappa_i)), r_D_cap)
                else 0
      } else {
        # Net-of-tax: r_D = r_D_B * ((1 - MC)/(1 - MC_B))^eta_tilde.
        r_D_i = if (r_D_B_i > 0)
                  min(r_D_B_i * ((1 - MC_i) / (1 - kappa_i))^eta, r_D_cap)
                else 0
      }
    }

    if (form == 'levels') {
      # Entropy realization cost C(r_D) = (1/eta)*[r_D*ln(r_D/r_D_B) - r_D + r_D_B].
      # Explicit r_D_B = 0 -> 0 branch avoids log(0); the r_D*ln(r_D) term -> 0 as
      # r_D -> 0, so a clipped-to-zero r_D needs no special handling beyond that.
      if (r_D_B_i > 0) {
        xlogx = if (r_D_i > 0) r_D_i * log(r_D_i / r_D_B_i) else 0
        C_i   = (xlogx - r_D_i + r_D_B_i) / eta
      } else {
        C_i = 0
      }
    } else {
      # Net-of-tax power cost whose FOC generates the (1 - MC)^eta response:
      #   C(r) = (1 - MC_B)*[(eta/(eta+1))*r_B*(r/r_B)^((eta+1)/eta) - r
      #          + r_B/(eta+1)],  MC_B = kappa (C(r_B) = 0, C'(r_B) = 0, convex).
      # r_D_B = 0 -> C = 0 (and avoids the r/r_B division). A clipped r_D -> 0
      # is fine: (0)^((eta+1)/eta) = 0 for eta > 0.
      if (r_D_B_i > 0) {
        C_i = (1 - kappa_i) *
              ((eta / (eta + 1)) * r_D_B_i * (r_D_i / r_D_B_i)^((eta + 1) / eta) -
               r_D_i + r_D_B_i / (eta + 1))
      } else {
        C_i = 0
      }
    }

    remaining = max(1 - r_D_i, 0)
    W[i]     = kappa_i * r_D_i - C_i - tau_i * r_D_i + remaining * death_cont
    MC[i]    = MC_i
    r_D[i]   = r_D_i
    kappa[i] = kappa_i
  }

  list(W = W, MC = MC, r_D = r_D, kappa = kappa)
}



kg_dyn_solve_bellman = function(grid_packed, tau_mat, c_phi_mat,
                                kappa_mat     = NULL,
                                eta           = kg_dyn_active_eta(),
                                beta_by_year  = NULL,
                                c_phi         = NULL,
                                h_mat         = NULL,
                                e_mat         = NULL,
                                form          = kg_dyn_response_form()) {

  #----------------------------------------------------------------------------
  # Backward induction over (age, year) cells.
  #
  # When kappa_mat = NULL: Pass 1 (baseline). Recovers kappa from the FOC
  # by forcing optimal r_D to equal the observed baseline realization rate
  # r_B; C'(r_D_B) = 0 (both cost forms) makes kappa = MC exactly.
  # c_phi_mat is typically all-zero under current-law step-up.
  #
  # When kappa_mat is supplied: Pass 2 (scenario). Solves the FOC closed form
  # for the active form (see kg_dyn_bellman_sweep_age / kg_dyn_response_form):
  # levels r_D = clip(r_D_B*exp(-eta*(MC - MC_B)), 0, 1); logs
  # r_D = clip(r_D_B*((1 - MC)/(1 - MC_B))^eta, 0, 1). eta is the active form's
  # constant (semi-elasticity for levels, net-of-tax elasticity for logs).
  #
  # c_phi_mat is an [n_ages, n_years] matrix of cell-level burden shares
  # produced by kg_dyn_build_regime_mix on the bathtub grid, then extended
  # to the Bellman grid by repeating the age-80 value forward (same
  # treatment as tau_mat). Scalars are accepted for unit tests and
  # broadcast to a constant matrix.
  #
  # beta_by_year[j] discounts between year j and j+1; NULL falls back to a
  # constant kg.beta_fallback vector for isolated solver unit tests.
  #
  # h_mat is the [n_ages, n_years] wealth-tax carrying-cost matrix (see
  # kg_dyn_bellman_sweep_age): per-cell gain-weighted mean of the record
  # product tau_w * tau_cg, arriving PRE-MULTIPLIED from
  # kg_dyn_aggregate_cell_carry. NULL (all existing callers) and scalar 0
  # broadcast to zeros — bit-identical to the pre-carry code path.
  #
  # e_mat is the [n_ages, n_years] estate-exposure matrix (see
  # kg_dyn_bellman_sweep_age): per-cell gain-weighted mean of the record
  # switch-gated marginal estate rate mtr_estate_ded, clamped to [0, 1]
  # (kg_dyn_aggregate_cell_estate). LEG-PAIRED, unlike h: Pass 1 must get
  # the baseline-law matrix e_B and Pass 2 the reform-law e_S. NULL and
  # scalar 0 broadcast to zeros — bit-identical to the pre-estate-offset
  # code path.
  #
  # Returns: list(W, MC, kappa, r_D), each [age, year].
  #----------------------------------------------------------------------------

  m_mat   = grid_packed$m
  r_B_mat = grid_packed$r_B
  p_char_mat = grid_packed$p_char
  n_ages  = nrow(m_mat); n_years = ncol(m_mat)
  ages_chr  = rownames(m_mat); years_chr = colnames(m_mat)

  if (is.null(beta_by_year)) beta_by_year = rep(kg_setting('beta_fallback'), n_years)
  stopifnot(length(beta_by_year) == n_years)

  if (missing(c_phi_mat) || is.null(c_phi_mat)) {
    c_phi_mat = c_phi %||% 0
  }

  if (length(c_phi_mat) == 1) {
    c_phi_mat = matrix(c_phi_mat, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(c_phi_mat), c(n_ages, n_years)))

  if (is.null(p_char_mat)) {
    p_char_mat = matrix(0, n_ages, n_years,
                        dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(p_char_mat), c(n_ages, n_years)))

  if (is.null(h_mat)) h_mat = 0
  if (length(h_mat) == 1) {
    h_mat = matrix(h_mat, n_ages, n_years,
                   dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(h_mat), c(n_ages, n_years)))

  if (is.null(e_mat)) e_mat = 0
  if (length(e_mat) == 1) {
    e_mat = matrix(e_mat, n_ages, n_years,
                   dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(e_mat), c(n_ages, n_years)))

  W     = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  MC    = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  kappa = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_D   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))

  sweep = function(j, W_next_col, stationary) {
    kg_dyn_bellman_sweep_age(
      W_next        = W_next_col,
      m_col         = m_mat  [, j],
      r_B_col       = r_B_mat[, j],
      tau_col       = tau_mat[, j],
      c_phi_col     = c_phi_mat[, j],
      p_char_col    = p_char_mat[, j],
      eta           = eta,
      beta          = beta_by_year[j],
      kappa_col     = if (is.null(kappa_mat)) NULL else kappa_mat[, j],
      stationary    = stationary,
      h_col         = h_mat[, j],
      e_col         = e_mat[, j],
      form          = form
    )
  }

  # Terminal year: stationary backward solve in age (W[a+1] from the same
  # sweep), treating year-n_years primitives as constant forward.
  res = sweep(n_years, W_next_col = NULL, stationary = TRUE)
  W    [, n_years] = res$W
  MC   [, n_years] = res$MC
  kappa[, n_years] = res$kappa
  r_D  [, n_years] = res$r_D

  # March backward in time.
  if (n_years >= 2) {
    for (j in (n_years - 1):1) {
      res = sweep(j, W_next_col = W[, j + 1], stationary = FALSE)
      W    [, j] = res$W
      MC   [, j] = res$MC
      kappa[, j] = res$kappa
      r_D  [, j] = res$r_D
    }
  }

  list(W = W, MC = MC, kappa = kappa, r_D = r_D)
}



