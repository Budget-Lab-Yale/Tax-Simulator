#-------------------------------------------------------------------------------
# estate_module.R
#
# Locked-spec reduced-form estate tax module (new_estate_modeling_thoughts.md §2,
# §10). Record-level: no cell collapse, filing_status carried throughout so the
# joint (both-die, 2x exemption) and single (DSUE-blend) paths are computed per
# record. Sourceable, side-effect free: functions and constants only.
#
# Per-record pipeline:
#   reported  = economic_gross x r x [1 + (rho_pt - 1) * s_pt]    [valuation]
#   taxable   = max(reported - debts - f_ded(bin) * reported, 0)  [debts explicit]
#   base      = taxable + gamma(bin) * reported                   [gift add-back]
#   joint  (filing_status == 2 & q2 > 0), both-die event:
#     liability = L(base, 2 * exemption);          m = q1 * q2
#   single (everyone else), DSUE blend of two FULL calcs through the kink:
#     liability = p_dsue * L(base, exemption + dsue) +
#                 (1 - p_dsue) * L(base, exemption); m = q1
#   E[tax]    = weight * m * liability
# where L(base, excl) = max(T(base) - T(excl), 0) with T the graduated tentative
# schedule (unified credit as a credit at the exemption, so reforms with
# graduated top rates work; under current law this is exactly flat 40% above the
# exemption because the brackets live below $1M).
#
# f_ded / p_dsue / f_dsue / gamma are ESTIMATED from SOI (per death year, per
# size bin), not calibrated. The only calibration knobs are the valuation
# parameters (r, rho_pt) — frozen resources, not tax law.
#
# Mortality: m_raw from Tax-Data q_death1/q_death2. Optional smoothed variant
# (§10b): WLS fit of m on a low-df natural spline of log(gross), weights =
# population weight, separately for joint and non-joint records, replacing each
# record's m with m_hat(wealth). A stiff global fit is informed by the whole
# top tail's wealth-mortality gradient, diluting any one donor-clone cluster's
# leverage; intercept-included WLS preserves total death-weight over the fit
# range (residuals orthogonal to the intercept).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(splines)
})


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------

ESTATE_ASSET_COLS = c(
  'value.cash', 'value.equities', 'value.bonds', 'value.dc', 'value.db',
  'value.life_ins', 'value.annuities', 'value.trusts', 'value.other_fin',
  'value.pass_throughs', 'value.primary_home', 'value.other_home',
  'value.re_fund', 'value.other_nonfin'
)

ESTATE_DEBT_COLS = c(
  'value.primary_mortgage', 'value.other_mortgage', 'value.credit_lines',
  'value.credit_cards', 'value.installment_debt', 'value.other_debt'
)

# Size-bin label -> [lo, hi) bounds. Includes pre-TCJA labels so the SOI table
# can be extended back without code changes; per year we use whichever labels
# that year's SOI rows carry (finest available partition).
ESTATE_BIN_BOUNDS = list(
  'under_5m'  = c(0,     5e6),
  '5m_10m'    = c(5e6,   10e6),
  'under_10m' = c(0,     10e6),
  '10m_20m'   = c(10e6,  20e6),
  '20m_50m'   = c(20e6,  50e6),
  '50m_plus'  = c(50e6,  Inf)
)


#-------------------------------------------------------------------------------
# Tax-Data records
#-------------------------------------------------------------------------------

load_estate_records = function(tax_units_path) {
  #----------------------------------------------------------------------------
  # Loads one Tax-Data tax_units CSV at the record level and derives the
  # estate-relevant variables. No cell collapse: filing status, mortality, and
  # asset composition stay per-record.
  #
  # Parameters:
  #   - tax_units_path (str) : path to tax_units_{year}.csv
  #
  # Returns: tibble with id, weight, filing_status, married (joint both-alive
  #          flag), q1, q2, m_raw (household death probability: q1*q2 joint,
  #          q1 otherwise), economic_gross, debts, pass_through, s_pt
  #----------------------------------------------------------------------------
  required = c('id', 'weight', 'filing_status', 'q_death1', 'q_death2',
               ESTATE_ASSET_COLS, ESTATE_DEBT_COLS)
  header = names(fread(tax_units_path, nrows = 0, showProgress = FALSE))
  missing = setdiff(required, header)
  if (length(missing) > 0) {
    stop('Missing columns in ', tax_units_path, ': ',
         paste(missing, collapse = ', '))
  }

  td = fread(tax_units_path, select = required, showProgress = FALSE) %>%
    as_tibble()

  assets = as.matrix(td[, ESTATE_ASSET_COLS])
  assets[is.na(assets)] = 0
  debts = as.matrix(td[, ESTATE_DEBT_COLS])
  debts[is.na(debts)] = 0

  td %>%
    transmute(
      id,
      weight         = as.numeric(weight),
      filing_status,
      q1             = replace_na(as.numeric(q_death1), 0),
      q2             = replace_na(as.numeric(q_death2), 0),
      married        = filing_status == 2 & q2 > 0,
      m_raw          = if_else(married, q1 * q2, q1),
      economic_gross = rowSums(assets),
      debts          = rowSums(debts),
      pass_through   = assets[, 'value.pass_throughs'],
      s_pt           = if_else(economic_gross > 0,
                               pass_through / economic_gross, 0)
    ) %>%
    filter(weight > 0, economic_gross > 0) %>%
    mutate(m = m_raw)
}


#-------------------------------------------------------------------------------
# Smoothed mortality (clone-robust variant, thoughts doc §10b)
#-------------------------------------------------------------------------------

fit_smooth_mortality = function(records, floor = 1e6, cap = 1e9, df = 4) {
  #----------------------------------------------------------------------------
  # Fits household mortality as a smooth function of wealth, separately for
  # joint (m = q1*q2) and non-joint (m = q1) records with gross in
  # (floor, cap]. Low df is the point: the fit borrows the global
  # wealth-mortality gradient and dilutes the leverage of any single
  # replicated-donor cluster.
  #
  # The cap exists because the clone pathology lives in the DRF-sampled
  # ~$10M-$1B band; $1B+ records are Forbes-enumerated with REAL ages, so
  # smoothing there would destroy genuine information (and extrapolating the
  # spline into the billionaire tail visibly distorts top-bin tax).
  #
  # Parameters:
  #   - records (df) : output of load_estate_records()
  #   - floor (dbl)  : minimum gross for fit and application
  #   - cap (dbl)    : maximum gross for fit and application (raw m above)
  #   - df (int)     : natural spline degrees of freedom in log(gross)
  #
  # Returns: list(single = lm, joint = lm, floor, cap, df, diagnostics)
  #----------------------------------------------------------------------------
  fit_one = function(df_sub) {
    lm(m_raw ~ ns(log(economic_gross), df = df),
       data = df_sub, weights = weight)
  }
  sub = records %>% filter(economic_gross > floor, economic_gross <= cap)
  fits = list(
    single = fit_one(sub %>% filter(!married)),
    joint  = fit_one(sub %>% filter(married)),
    floor  = floor,
    cap    = cap,
    df     = df
  )

  # Death-weight preservation check (exact pre-clamping; clamping at 0 can
  # introduce a small wedge, reported here rather than silently absorbed)
  smoothed = apply_smooth_mortality(records, fits)
  fits$diagnostics = records %>%
    mutate(m_smooth = smoothed$m) %>%
    filter(economic_gross > floor, economic_gross <= cap) %>%
    group_by(married) %>%
    summarise(
      dw_raw    = sum(weight * m_raw),
      dw_smooth = sum(weight * m_smooth),
      ratio     = dw_smooth / dw_raw,
      .groups = 'drop'
    )
  fits
}

apply_smooth_mortality = function(records, fits) {
  #----------------------------------------------------------------------------
  # Replaces m with the fitted m_hat(wealth) for records inside the fit band
  # (floor, cap]; outside it (irrelevant below, Forbes-real above) m stays raw.
  #
  # Returns: records with m overwritten (m_raw retained)
  #----------------------------------------------------------------------------
  in_band = records$economic_gross > fits$floor &
    records$economic_gross <= fits$cap
  m_hat = records$m_raw
  for (grp in c('single', 'joint')) {
    sel = in_band & (records$married == (grp == 'joint'))
    if (any(sel)) {
      m_hat[sel] = predict(fits[[grp]], newdata = records[sel, ])
    }
  }
  records %>% mutate(m = pmin(pmax(m_hat, 0), 1))
}


apply_cluster_cap_mortality = function(records, fits, k = 1.25, floor = 5e6,
                                       verbose = TRUE) {
  #----------------------------------------------------------------------------
  # Surgical clone fix: raw mortality everywhere EXCEPT exact-gross clusters
  # whose aggregate death-weight is anomalous relative to their own wealth.
  #
  # Tax-Data replicates thin donor pools into byte-identical wealth clusters
  # that all inherit one donor's age; where that donor is elderly, the cluster
  # carries pathological death-weight (the $17.65M / $11.46M archetypes). For
  # each cluster (records sharing an exact rounded gross above `floor`, n >= 2)
  # compare raw death-weight sum(w * m_raw) to the smooth-fit-implied
  # sum(w * m_hat(wealth)); if raw exceeds k x implied, scale the cluster's m
  # down to k x implied. Cap-down only: clusters with young copied donors are
  # left alone, as are all singleton records (incl. Forbes $1B+ real ages).
  # Unlike global smoothing, this preserves the genuine wealth-mortality joint
  # everywhere the data is not degenerate.
  #
  # Parameters:
  #   - records (df) : load_estate_records() output
  #   - fits (list)  : fit_smooth_mortality() output (supplies m_hat)
  #   - k (dbl)      : tolerance multiple before a cluster is capped
  #   - floor (dbl)  : minimum gross for cluster detection
  #
  # Returns: records with m overwritten (m_raw retained)
  #----------------------------------------------------------------------------
  out = records %>%
    mutate(
      m_hat = apply_smooth_mortality(records, fits)$m,
      cluster_key = if_else(economic_gross > floor,
                            round(economic_gross), NA_real_)
    ) %>%
    group_by(cluster_key) %>%
    mutate(
      n_clust = n(),
      dw_raw  = sum(weight * m_raw),
      dw_fit  = sum(weight * m_hat),
      capped  = !is.na(cluster_key) & n_clust >= 2 &
        dw_raw > k * pmax(dw_fit, 1e-12),
      m       = if_else(capped, m_raw * k * dw_fit / dw_raw, m_raw)
    ) %>%
    ungroup()

  if (verbose) {
    flagged = out %>%
      filter(capped) %>%
      group_by(cluster_key) %>%
      summarise(n = first(n_clust), dw_raw = first(dw_raw),
                dw_capped = first(k * dw_fit), .groups = 'drop')
    cat(sprintf(paste0('Cluster cap (k = %.2f): %d clusters flagged, ',
                       'death-weight %0.0f -> %0.0f (removed %0.0f)\n'),
                k, nrow(flagged), sum(flagged$dw_raw), sum(flagged$dw_capped),
                sum(flagged$dw_raw) - sum(flagged$dw_capped)))
    top = flagged %>% arrange(desc(dw_raw)) %>% head(5)
    for (i in seq_len(nrow(top))) {
      cat(sprintf('  $%.3fM x %d recs: dw %0.0f -> %0.0f\n',
                  top$cluster_key[i] / 1e6, top$n[i], top$dw_raw[i],
                  top$dw_capped[i]))
    }
  }

  out %>% select(-m_hat, -cluster_key, -n_clust, -dw_raw, -dw_fit, -capped)
}


apply_cluster_abscap_mortality = function(records, cap = 300, floor = 5e6,
                                          verbose = TRUE) {
  #----------------------------------------------------------------------------
  # Absolute cluster death-weight cap (the §8 winsorization, generalized): any
  # exact-gross cluster (n >= 2, gross > floor) whose total death-weight
  # sum(w * m_raw) exceeds `cap` expected deaths has members' m scaled down so
  # the cluster totals `cap`. Only the few pathological mega-clusters can trip
  # an absolute threshold, so — unlike the smooth-relative cap, which flags
  # hundreds of legitimately elderly-skewed clusters and biases the top tail
  # down — everything else stays exactly raw.
  #
  # The cap VALUE is tunable per vintage (legitimate cluster scale was ~236 on
  # 2026052823, ~160 on 2026060918 excluding the two outliers); the RULE is
  # generic — never keyed to a dollar wealth value.
  #
  # Returns: records with m overwritten (m_raw retained)
  #----------------------------------------------------------------------------
  out = records %>%
    mutate(cluster_key = if_else(economic_gross > floor,
                                 round(economic_gross), NA_real_)) %>%
    group_by(cluster_key) %>%
    mutate(
      n_clust = n(),
      dw_raw  = sum(weight * m_raw),
      capped  = !is.na(cluster_key) & n_clust >= 2 & dw_raw > cap,
      m       = if_else(capped, m_raw * cap / dw_raw, m_raw)
    ) %>%
    ungroup()

  if (verbose) {
    flagged = out %>%
      filter(capped) %>%
      group_by(cluster_key) %>%
      summarise(n = first(n_clust), dw_raw = first(dw_raw), .groups = 'drop') %>%
      arrange(desc(dw_raw))
    cat(sprintf(paste0('Absolute cluster cap (cap = %0.0f): %d clusters ',
                       'flagged, death-weight %0.0f -> %0.0f\n'),
                cap, nrow(flagged), sum(flagged$dw_raw), nrow(flagged) * cap))
    for (i in seq_len(min(5, nrow(flagged)))) {
      cat(sprintf('  $%.3fM x %d recs: dw %0.0f -> %0.0f\n',
                  flagged$cluster_key[i] / 1e6, flagged$n[i],
                  flagged$dw_raw[i], cap))
    }
  }

  out %>% select(-cluster_key, -n_clust, -dw_raw, -capped)
}


#-------------------------------------------------------------------------------
# SOI-derived inputs (estimated, not calibrated)
#-------------------------------------------------------------------------------

load_soi_estate_table = function(path) {
  # integer64 = 'double': the large amount columns otherwise load as bit64
  # integer64, whose arithmetic throws overflow warnings in the ratio fields
  fread(path, integer64 = 'double') %>% as_tibble()
}

soi_inputs = function(soi, death_year, exemption, gamma_pool_factor = 1.5) {
  #----------------------------------------------------------------------------
  # Estimates the per-bin assumed parameters and pulls the per-bin targets for
  # one death year (= filing year - 1) from the SOI TAXABLE universe:
  #   f_ded  : non-debt deductions / gross (debts are subtracted explicitly)
  #   p_dsue : share of returns claiming DSUE
  #   f_dsue : average DSUE claimed, expressed as a fraction of average gross
  #   gamma  : adjusted taxable gifts / gross (lifetime-gift add-back)
  #
  # Blank SOI cells (disclosure suppression, e.g. 50m_plus DSUE in filing years
  # 2021-2023) are treated as 0, matching the python diagnostics' num().
  #
  # GIFT SELECTION TRAP: gamma must NOT be estimated per bin. In any bin the
  # exemption cuts into, the taxable universe is selected toward gift-rich
  # estates (taxable BECAUSE of gifts): gifts/gross runs 0.33-1.45 in under_10m
  # and ~0.16-0.21 in 10m_20m post-TCJA, vs a stable ~0.08 in 50m_plus. Using
  # those bins' own ratios would create millions of phantom payers. gamma is
  # therefore pooled over bins whose LOWER bound >= gamma_pool_factor x
  # exemption (taxability not gift-driven there) and applied as one scalar --
  # the same logic as MODELING_STATUS 10b's gamma ~ 0.108. This adapts
  # automatically: pre-TCJA ($5.49M exemption) pools from 10m_20m up, so the
  # 5m_10m bin becomes modelable without inheriting its selection bias.
  #
  # Returns: tibble(size_bin, lo, hi, f_ded, p_dsue, f_dsue, gamma,
  #                 target_count, target_tax)
  #----------------------------------------------------------------------------
  rows = soi %>%
    filter(year == death_year + 1, tax_status == 'taxable',
           size_bin %in% names(ESTATE_BIN_BOUNDS))
  if (nrow(rows) == 0) {
    stop('No taxable SOI rows for death year ', death_year)
  }

  # Use the finest partition available: drop under_10m if 5M splits exist
  if (all(c('under_5m', '5m_10m') %in% rows$size_bin)) {
    rows = rows %>% filter(size_bin != 'under_10m')
  }

  out = rows %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    transmute(
      size_bin,
      lo = map_dbl(size_bin, ~ ESTATE_BIN_BOUNDS[[.x]][1]),
      hi = map_dbl(size_bin, ~ ESTATE_BIN_BOUNDS[[.x]][2]),
      f_ded  = if_else(gross_estate_for_tax_purposes_amt > 0,
                       (total_allowable_deductions_amt - debts_and_mortgages_amt) /
                         gross_estate_for_tax_purposes_amt, 0),
      p_dsue = if_else(gross_estate_for_tax_purposes_n > 0,
                       deceased_spousal_unused_exclusion_n /
                         gross_estate_for_tax_purposes_n, 0),
      f_dsue = if_else(
        deceased_spousal_unused_exclusion_n > 0 &
          gross_estate_for_tax_purposes_n > 0,
        (deceased_spousal_unused_exclusion_amt /
           deceased_spousal_unused_exclusion_n) /
          (gross_estate_for_tax_purposes_amt /
             gross_estate_for_tax_purposes_n),
        0),
      gifts_amt = adjusted_taxable_gifts_amt,
      gross_amt = gross_estate_for_tax_purposes_amt,
      target_count = gross_estate_for_tax_purposes_n,
      target_tax   = net_estate_tax_amt
    ) %>%
    arrange(lo)

  pool = out %>% filter(lo >= gamma_pool_factor * exemption)
  if (nrow(pool) == 0) pool = out %>% slice_max(lo, n = 1)
  out = out %>%
    mutate(gamma = sum(pool$gifts_amt) / max(sum(pool$gross_amt), 1)) %>%
    select(-gifts_amt, -gross_amt)

  # The selected bins must tile [0, Inf) without overlap
  if (any(head(out$hi, -1) != tail(out$lo, -1)) ||
      out$lo[1] != 0 || !is.infinite(out$hi[nrow(out)])) {
    stop('SOI size bins for death year ', death_year,
         ' do not tile [0, Inf): ', paste(out$size_bin, collapse = ', '))
  }
  out
}

assign_size_bin = function(reported_gross, bins) {
  bins$size_bin[findInterval(reported_gross, c(bins$lo, Inf),
                             rightmost.closed = FALSE)]
}


#-------------------------------------------------------------------------------
# Policy: graduated schedule with unified credit
#-------------------------------------------------------------------------------

estate_policy_current_law = function() {
  #----------------------------------------------------------------------------
  # IRC 2001(c) tentative tax schedule. Field names mirror the planned
  # estate.yaml so on-model integration is a config read, not a refactor.
  # The exemption (basic exclusion amount) is passed separately by year.
  #----------------------------------------------------------------------------
  list(
    brackets = c(0, 10e3, 20e3, 40e3, 60e3, 80e3, 100e3,
                 150e3, 250e3, 500e3, 750e3, 1e6),
    rates    = c(0.18, 0.20, 0.22, 0.24, 0.26, 0.28,
                 0.30, 0.32, 0.34, 0.37, 0.39, 0.40),
    portability = TRUE
  )
}

tentative_tax = function(x, policy) {
  # Vectorized graduated schedule: T(x) = sum over brackets of rate * overlap
  x = pmax(x, 0)
  thresholds = policy$brackets
  rates = policy$rates
  upper = c(thresholds[-1], Inf)
  out = numeric(length(x))
  for (i in seq_along(rates)) {
    out = out + rates[i] * pmax(pmin(x, upper[i]) - thresholds[i], 0)
  }
  out
}

liability_with_credit = function(base, exclusion, policy) {
  # Unified credit: tentative tax on the exclusion offsets tentative tax on the
  # base. Equals flat top-rate * (base - exclusion) when both exceed the top
  # bracket threshold, i.e. always at estate-tax-relevant sizes.
  pmax(tentative_tax(base, policy) - tentative_tax(exclusion, policy), 0)
}


#-------------------------------------------------------------------------------
# Core liability calculation (locked spec)
#-------------------------------------------------------------------------------

compute_estate_liability = function(records, exemption, soi_in,
                                    policy = estate_policy_current_law(),
                                    valuation = list(r = 1, rho_pt = 1),
                                    gift_addback = TRUE,
                                    wealth_scale = 1,
                                    count_mode = c('expected', 'nodsue')) {
  #----------------------------------------------------------------------------
  # Applies the locked-spec pipeline to records under a given exemption and
  # rate schedule. Expected-value throughout: no RNG, no materialized deaths.
  #
  # Parameters:
  #   - records (df)     : load_estate_records() output (m already raw or
  #                        smoothed)
  #   - exemption (dbl)  : basic exclusion amount for the death year
  #   - soi_in (df)      : soi_inputs() for the death year (f_ded/p_dsue/
  #                        f_dsue/gamma by bin + bin bounds)
  #   - policy (list)    : brackets/rates/portability (estate.yaml shape)
  #   - valuation (list) : frozen calibration knobs r and rho_pt; reported
  #                        gross = economic_gross * r * [1 + (rho_pt-1)*s_pt]
  #   - gift_addback     : include gamma(bin) * reported in the unified base
  #   - wealth_scale     : deflator for reading base-year wealth at another
  #                        death year's real-equivalent cut points (FRED NW)
  #   - count_mode       : 'expected' = probability-correct taxable count
  #                        (DSUE blend of indicators); 'nodsue' = count if
  #                        taxable ignoring DSUE (python diagnostic parity)
  #
  # Returns: records + size_bin, reported_gross, taxable_estate, estate_base,
  #          liability (expected per-death), count_flag (expected indicator),
  #          exp_deaths = weight*m, exp_tax = weight*m*liability
  #----------------------------------------------------------------------------
  count_mode = match.arg(count_mode)

  out = records
  g = out$economic_gross * wealth_scale
  d = out$debts * wealth_scale

  r_factor = valuation$r * (1 + (valuation$rho_pt - 1) * out$s_pt)
  reported = g * r_factor

  out$size_bin = assign_size_bin(reported, soi_in)
  idx = match(out$size_bin, soi_in$size_bin)

  taxable = pmax(reported - d - soi_in$f_ded[idx] * reported, 0)
  base = taxable +
    if (gift_addback) soi_in$gamma[idx] * reported else 0

  p_dsue = if (policy$portability) soi_in$p_dsue[idx] else 0
  dsue = soi_in$f_dsue[idx] * reported

  # Joint both-alive records: both-die event, 2x exemption, no DSUE channel.
  # Singles (incl. widows already single in the cross-section): DSUE blend of
  # two complete liability calculations, each through the kink (never an
  # expected DSUE inside one calc -- the pmax kink is nonlinear).
  liab_joint = liability_with_credit(base, 2 * exemption, policy)
  liab_wo    = liability_with_credit(base, exemption, policy)
  liab_w     = liability_with_credit(base, exemption + dsue, policy)
  liab_single = p_dsue * liab_w + (1 - p_dsue) * liab_wo

  out$reported_gross = reported
  out$taxable_estate = taxable
  out$estate_base    = base
  out$liability      = if_else(out$married, liab_joint, liab_single)

  out$count_flag = if (count_mode == 'expected') {
    if_else(
      out$married,
      as.numeric(liab_joint > 0),
      p_dsue * (liab_w > 0) + (1 - p_dsue) * (liab_wo > 0)
    )
  } else {
    if_else(out$married, as.numeric(liab_joint > 0), as.numeric(liab_wo > 0))
  }

  out$exp_deaths = out$weight * out$m
  out$exp_tax    = out$weight * out$m * out$liability
  out
}


summarize_estate_bins = function(out, soi_in) {
  #----------------------------------------------------------------------------
  # Per-bin expected taxable-return counts and tax, with SOI targets attached.
  #----------------------------------------------------------------------------
  out %>%
    group_by(size_bin) %>%
    summarise(
      model_count = sum(weight * m * count_flag),
      model_tax   = sum(exp_tax),
      .groups = 'drop'
    ) %>%
    right_join(soi_in %>% select(size_bin, lo, target_count, target_tax),
               by = 'size_bin') %>%
    arrange(lo) %>%
    select(-lo) %>%
    mutate(across(c(model_count, model_tax), ~ replace_na(., 0)))
}
