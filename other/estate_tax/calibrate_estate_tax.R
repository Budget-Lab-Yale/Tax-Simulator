#!/usr/bin/env Rscript

#-------------------------------------------------------------------------------
# calibrate_estate_tax.R
#
# Side-script calibration for an on-model estate tax calculator. This script
# reads raw Tax-Data wealth/mortality records, cleaned SOI estate tax tables,
# and CBO/JCT score targets. It estimates reporting-factor parameters across a
# small functional-form tournament and writes parameter/moment diagnostics.
#
# This file intentionally does not source or mutate the simulator tax
# calculator. It is a standalone calibration tool.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
})


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------

ESTATE_VALUE_COLS = c(
  'value.cash', 'value.equities', 'value.bonds', 'value.dc', 'value.db',
  'value.life_ins', 'value.annuities', 'value.trusts', 'value.other_fin',
  'value.pass_throughs', 'value.primary_home', 'value.other_home',
  'value.re_fund', 'value.other_nonfin'
)

SIZE_BINS = tibble(
  size_bin = c('under_10m', '10m_20m', '20m_50m', '50m_plus'),
  min_gross = c(0, 10e6, 20e6, 50e6),
  max_gross = c(10e6, 20e6, 50e6, Inf)
)

GIFT_TAX_HAIRCUT = 0.10
SCORE_TARGET_MULTIPLIER = 1 - GIFT_TAX_HAIRCUT

BASE_YEAR = 2026
SUNSET_BEA_2026 = 7.2e6
OBBBA_BEA_2026 = 15e6

HISTORICAL_BEA = c(
  '2018' = 11.18e6,
  '2019' = 11.40e6,
  '2020' = 11.58e6,
  '2021' = 11.70e6,
  '2022' = 12.06e6,
  '2023' = 12.92e6,
  '2024' = 13.61e6,
  '2025' = 13.99e6
)

SOI_UNDER_10M_WEIGHT = 0.25

SCRIPT_PATH = tryCatch({
  cmd = commandArgs(trailingOnly = FALSE)
  file_arg = cmd[grepl('^--file=', cmd)]
  if (length(file_arg) == 0) NA_character_ else normalizePath(sub('^--file=', '', file_arg[1]))
}, error = function(e) NA_character_)
SCRIPT_DIR = if (is.na(SCRIPT_PATH)) getwd() else dirname(SCRIPT_PATH)
DEFAULT_SCORE_TARGETS = file.path(SCRIPT_DIR, 'score_targets_estate_gift.csv')


#-------------------------------------------------------------------------------
# CLI
#-------------------------------------------------------------------------------

usage = function() {
  cat(paste0(
    'Usage:\n',
    '  Rscript other/estate_tax/calibrate_estate_tax.R \\\n',
    '    --tax-data-root <Tax-Data baseline dir> \\\n',
    '    --soi-file <estate_tax_filed_2019_2023.csv> \\\n',
    '    --output-dir <output dir> \\\n',
    '    [--score-targets <targets.csv>] \\\n',
    '    [--macro-root <Macro-Projections baseline dir>] \\\n',
    '    [--wealth-cells 1000] [--optim-starts 3] [--maxit 75]\n\n',
    'Inputs:\n',
    '  --tax-data-root   Directory containing tax_units_<year>.csv with value.* and q_death*.\n',
    '  --soi-file        Cleaned SOI estate tax filing table.\n',
    '  --score-targets   Tidy score target CSV. Defaults to bundled CBO/JCT targets.\n',
    '  --macro-root      Optional Macro-Projections baseline with ccpiu_irs/cpiu.\n',
    '  --output-dir      Directory for calibration CSV/Markdown outputs.\n\n',
    'Options:\n',
    '  --wealth-cells    Collapse each year to this many log-wealth cells; 0 keeps records.\n',
    '  --optim-starts    Number of best grid points used as local-optimizer starts.\n',
    '  --maxit           Max iterations for each local optimization.\n',
    '  --quick           Small grid and fewer optimizer iterations for smoke tests.\n'
  ))
}

parse_args = function(args) {
  if ('--help' %in% args || '-h' %in% args) {
    usage()
    quit(save = 'no', status = 0)
  }

  out = list(
    score_targets = DEFAULT_SCORE_TARGETS,
    macro_root = NA_character_,
    wealth_cells = 1000L,
    optim_starts = 3L,
    maxit = 75L,
    quick = FALSE
  )

  i = 1
  while (i <= length(args)) {
    key = args[i]
    if (key == '--quick') {
      out$quick = TRUE
      i = i + 1
      next
    }
    if (!grepl('^--', key)) {
      stop('Unexpected positional argument: ', key)
    }
    if (i == length(args)) {
      stop('Missing value after ', key)
    }
    value = args[i + 1]
    name = sub('^--', '', key) %>% gsub('-', '_', .)
    out[[name]] = value
    i = i + 2
  }

  required = c('tax_data_root', 'soi_file', 'output_dir')
  missing = required[!required %in% names(out)]
  if (length(missing) > 0) {
    usage()
    stop('Missing required argument(s): ', paste(missing, collapse = ', '))
  }

  out$wealth_cells = as.integer(out$wealth_cells)
  out$optim_starts = as.integer(out$optim_starts)
  out$maxit = as.integer(out$maxit)
  if (out$quick) {
    out$wealth_cells = min(out$wealth_cells, 150L)
    out$optim_starts = 1L
    out$maxit = min(out$maxit, 20L)
  }
  out
}


#-------------------------------------------------------------------------------
# Utility helpers
#-------------------------------------------------------------------------------

check_cols = function(df_names, required, context) {
  missing = setdiff(required, df_names)
  if (length(missing) > 0) {
    stop(context, ' missing required columns: ', paste(missing, collapse = ', '))
  }
}

clamp = function(x, lo, hi) {
  pmin(pmax(x, lo), hi)
}

log_rel_error = function(model, target) {
  ifelse(is.na(target) | abs(target) <= 0,
         NA_real_,
         log(pmax(model, 1e-9) / pmax(target, 1e-9)))
}

size_bin_for = function(x) {
  case_when(
    x < 10e6 ~ 'under_10m',
    x < 20e6 ~ '10m_20m',
    x < 50e6 ~ '20m_50m',
    TRUE ~ '50m_plus'
  )
}

bin_midpoint = function(size_bin) {
  case_when(
    size_bin == 'under_10m' ~ 7.5e6,
    size_bin == '10m_20m' ~ 15e6,
    size_bin == '20m_50m' ~ 32.5e6,
    size_bin == '50m_plus' ~ 100e6,
    TRUE ~ NA_real_
  )
}

param_string = function(par) {
  paste(names(par), signif(par, 8), sep = '=', collapse = ';')
}


#-------------------------------------------------------------------------------
# Estate tax law helpers
#-------------------------------------------------------------------------------

tentative_estate_tax = function(x) {
  y = pmax(x, 0)
  out = numeric(length(y))

  brackets = tibble(
    lower = c(0, 10000, 20000, 40000, 60000, 80000, 100000, 150000, 250000, 500000, 750000, 1000000),
    base = c(0, 1800, 3800, 8200, 13000, 18200, 23800, 38200, 70800, 155800, 248300, 345800),
    rate = c(0.18, 0.20, 0.22, 0.24, 0.26, 0.28, 0.32, 0.34, 0.37, 0.39, 0.39, 0.40),
    upper = c(10000, 20000, 40000, 60000, 80000, 100000, 150000, 250000, 500000, 750000, 1000000, Inf)
  )

  for (j in seq_len(nrow(brackets))) {
    idx = y > brackets$lower[j] & y <= brackets$upper[j]
    out[idx] = brackets$base[j] + brackets$rate[j] * (y[idx] - brackets$lower[j])
  }
  out
}

estate_tax_liability = function(taxable_estate, applicable_exclusion) {
  pmax(0, tentative_estate_tax(taxable_estate) -
         tentative_estate_tax(applicable_exclusion))
}

load_policy_index = function(years, macro_root) {
  years = sort(unique(c(years, BASE_YEAR)))
  if (is.na(macro_root) || !nzchar(macro_root)) {
    warning('No --macro-root supplied; using flat 2026-dollar policy thresholds.')
    return(tibble(year = years, policy_index = 1))
  }

  files = file.path(macro_root, c('historical.csv', 'projections.csv'))
  files = files[file.exists(files)]
  if (length(files) == 0) {
    warning('Could not find historical.csv/projections.csv at --macro-root; using flat thresholds.')
    return(tibble(year = years, policy_index = 1))
  }

  macro = map_dfr(files, ~ fread(.x, showProgress = FALSE) %>% as_tibble())
  index_col = if ('ccpiu_irs' %in% names(macro)) 'ccpiu_irs' else 'cpiu'
  check_cols(names(macro), c('year', index_col), 'Macro-Projections')

  out = macro %>%
    filter(year %in% years) %>%
    arrange(year) %>%
    distinct(year, .keep_all = TRUE) %>%
    transmute(year, index_value = as.numeric(.data[[index_col]]))

  if (!BASE_YEAR %in% out$year || any(!years %in% out$year)) {
    warning('Macro-Projections missing some policy years; using flat thresholds for missing years.')
  }

  base_value = out$index_value[out$year == BASE_YEAR][1]
  if (is.na(base_value)) base_value = 1

  tibble(year = years) %>%
    left_join(out, by = 'year') %>%
    mutate(policy_index = if_else(is.na(index_value), 1, index_value / base_value)) %>%
    select(year, policy_index)
}

bea_for_year = function(years, policy, policy_index) {
  idx = policy_index$policy_index[match(years, policy_index$year)]
  idx[is.na(idx)] = 1

  hist = HISTORICAL_BEA[as.character(years)]
  out = rep(NA_real_, length(years))
  out[!is.na(hist)] = as.numeric(hist[!is.na(hist)])

  future = is.na(out)
  if (any(future)) {
    if (policy == 'obbba') {
      out[future] = OBBBA_BEA_2026 * idx[future]
    } else if (policy %in% c('baseline', 'sunset', 'historical')) {
      out[future] = SUNSET_BEA_2026 * idx[future]
    } else {
      stop('Unknown policy: ', policy)
    }
  }
  out
}


#-------------------------------------------------------------------------------
# Input loading
#-------------------------------------------------------------------------------

load_score_targets = function(path) {
  targets = fread(path, showProgress = FALSE) %>% as_tibble()
  check_cols(names(targets), c('scenario', 'target_type', 'year', 'value', 'source'),
             'score targets')

  if (!'units' %in% names(targets)) targets$units = 'millions'
  if (!'scope' %in% names(targets)) targets$scope = 'estate_gift'

  has_baseline = any(targets$scenario == 'baseline' &
                       targets$target_type == 'baseline_receipts')
  has_policy_delta = any(targets$scenario == 'obbba_vs_sunset' &
                           targets$target_type == 'policy_delta')
  if (!has_baseline || !has_policy_delta) {
    stop('score targets must include baseline/baseline_receipts and ',
         'obbba_vs_sunset/policy_delta rows.')
  }

  targets %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value),
      value_millions = case_when(
        tolower(units) %in% c('billion', 'billions', 'bn') ~ value * 1000,
        TRUE ~ value
      ),
      estate_multiplier = if_else(tolower(scope) == 'estate_gift',
                                  SCORE_TARGET_MULTIPLIER, 1),
      estate_value_millions = value_millions * estate_multiplier
    )
}

load_soi_targets = function(path, policy_index) {
  soi = fread(path, showProgress = FALSE) %>% as_tibble()
  required = c(
    'year', 'tax_status', 'size_bin',
    'gross_estate_for_tax_purposes_n',
    'gross_estate_for_tax_purposes_amt',
    'taxable_estate_amt',
    'deceased_spousal_unused_exclusion_n',
    'deceased_spousal_unused_exclusion_amt',
    'net_estate_tax_amt'
  )
  check_cols(names(soi), required, 'SOI file')

  # We model the taxable universe: death-weighted units whose taxable estate
  # exceeds the exemption. Nontaxable filers (marital-deduction first deaths,
  # portability/DSUE-only elections, fully-charitable estates) arise from
  # mechanisms the model does not represent, so we calibrate against taxable
  # returns only -- which also fits the taxable-fraction and DSUE tables on the
  # payer population rather than all filers.
  soi %>%
    filter(tax_status == 'taxable', size_bin != 'all') %>%
    mutate(
      filing_year = as.integer(year),
      death_year = filing_year - 1L,
      size_bin = as.character(size_bin),
      gross_n = as.numeric(gross_estate_for_tax_purposes_n),
      gross_amt = as.numeric(gross_estate_for_tax_purposes_amt),
      taxable_estate_amt = as.numeric(taxable_estate_amt),
      net_estate_tax_amt = as.numeric(net_estate_tax_amt),
      dsue_n = as.numeric(deceased_spousal_unused_exclusion_n),
      dsue_amt = as.numeric(deceased_spousal_unused_exclusion_amt),
      avg_gross = if_else(gross_n > 0, gross_amt / gross_n, bin_midpoint(size_bin)),
      taxable_fraction = clamp(taxable_estate_amt / pmax(gross_amt, 1), 1e-5, 0.999),
      historical_bea = bea_for_year(death_year, 'historical', policy_index)
    ) %>%
    left_join(SIZE_BINS, by = 'size_bin')
}

load_tax_data_cells = function(tax_data_root, years, wealth_cells) {
  required = c('id', 'weight', 'filing_status', 'q_death1', 'q_death2',
               ESTATE_VALUE_COLS)
  rows = vector('list', length(years))
  names(rows) = as.character(years)

  for (t in years) {
    path = file.path(tax_data_root, paste0('tax_units_', t, '.csv'))
    if (!file.exists(path)) {
      stop('Missing Tax-Data file: ', path)
    }
    header = fread(path, nrows = 0, showProgress = FALSE) %>% names()
    check_cols(header, required, paste0('Tax-Data ', t))

    td = fread(path, select = required, showProgress = FALSE) %>%
      as_tibble()

    estate = as.matrix(td[, ESTATE_VALUE_COLS])
    estate[is.na(estate)] = 0
    economic_gross = rowSums(estate)
    pass_through = estate[, 'value.pass_throughs']

    q1 = replace_na(as.numeric(td$q_death1), 0)
    q2 = replace_na(as.numeric(td$q_death2), 0)
    mortality = if_else(td$filing_status == 2 & q2 > 0, q1 * q2, q1)

    rows[[as.character(t)]] = tibble(
      death_year = as.integer(t),
      receipt_year = as.integer(t) + 1L,
      economic_gross = economic_gross,
      pass_through = pass_through,
      expected_weight = as.numeric(td$weight) * mortality
    ) %>%
      filter(is.finite(economic_gross), economic_gross > 0,
             is.finite(expected_weight), expected_weight > 0)
  }

  cells = bind_rows(rows)
  if (wealth_cells <= 0) return(cells)

  cells %>%
    group_by(death_year) %>%
    group_modify(function(df, key) {
      if (nrow(df) <= wealth_cells) return(df)
      logw = log(df$economic_gross)
      breaks = unique(quantile(logw, probs = seq(0, 1, length.out = wealth_cells + 1),
                               na.rm = TRUE, names = FALSE))
      if (length(breaks) < 3) return(df)
      df %>%
        mutate(wealth_cell = cut(logw, breaks = breaks, include.lowest = TRUE,
                                 labels = FALSE)) %>%
        group_by(wealth_cell) %>%
        summarise(
          receipt_year = first(receipt_year),
          cell_weight = sum(expected_weight, na.rm = TRUE),
          weighted_gross = sum(expected_weight * economic_gross, na.rm = TRUE),
          weighted_pt = sum(expected_weight * pass_through, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        transmute(
          receipt_year,
          expected_weight = cell_weight,
          pass_through = weighted_pt / pmax(cell_weight, 1e-12),
          economic_gross = weighted_gross / pmax(cell_weight, 1e-12)
        )
    }) %>%
    ungroup() %>%
    select(death_year, receipt_year, economic_gross, pass_through, expected_weight)
}


#-------------------------------------------------------------------------------
# SOI-derived taxable fraction and DSUE inputs
#-------------------------------------------------------------------------------

fit_taxable_fraction_models = function(soi_targets) {
  bin_lookup = soi_targets %>%
    group_by(size_bin) %>%
    summarise(
      taxable_fraction = clamp(sum(taxable_estate_amt, na.rm = TRUE) /
                                 pmax(sum(gross_amt, na.rm = TRUE), 1),
                               1e-5, 0.999),
      .groups = 'drop'
    )

  fit_rows = soi_targets %>%
    mutate(
      x = log(pmax(avg_gross, 1) / 1e7),
      y = qlogis(taxable_fraction),
      weight = pmax(gross_amt, 1)
    ) %>%
    filter(is.finite(x), is.finite(y), weight > 0)

  smooth_fit = lm(y ~ x + I(x^2), data = fit_rows, weights = weight)

  list(bin_lookup = bin_lookup, smooth_fit = smooth_fit)
}

build_dsue_table = function(soi_targets) {
  soi_targets %>%
    group_by(size_bin) %>%
    summarise(
      p_dsue = clamp(sum(dsue_n, na.rm = TRUE) /
                       pmax(sum(gross_n, na.rm = TRUE), 1), 0, 1),
      dsue_to_bea = clamp(sum(dsue_amt, na.rm = TRUE) /
                            pmax(sum(dsue_n * historical_bea, na.rm = TRUE), 1),
                          0, 1),
      .groups = 'drop'
    )
}

predict_taxable_fraction = function(gross, form, fits) {
  if (form == 'bin_lookup') {
    b = size_bin_for(gross)
    ratio = fits$bin_lookup$taxable_fraction[match(b, fits$bin_lookup$size_bin)]
    ratio[is.na(ratio)] = median(fits$bin_lookup$taxable_fraction, na.rm = TRUE)
    return(clamp(ratio, 1e-5, 0.999))
  }
  if (form == 'smooth_logit') {
    x = log(pmax(gross, 1) / 1e7)
    eta = predict(fits$smooth_fit, newdata = tibble(x = x))
    return(clamp(plogis(eta), 1e-5, 0.999))
  }
  stop('Unknown taxable fraction form: ', form)
}


#-------------------------------------------------------------------------------
# Reporting-factor forms
#-------------------------------------------------------------------------------

# Every form carries one additional composition parameter, rho_pt: the
# pass-through (closely-held business) reporting factor RELATIVE to all other
# assets. The size form sets the baseline (the all-else rate / gradient); the
# composition multiplier M = 1 + (rho_pt - 1) * s_pt tilts it by the
# pass-through share. M = 1 when s_pt = 0, so the size form and rho_pt are
# separately identified (no scale redundancy).
reporting_par_names = function(form) {
  size_pars = switch(
    form,
    constant = c('a'),
    log_linear = c('a', 'b'),
    log_quadratic = c('a', 'b', 'c'),
    bounded_log_quadratic = c('a', 'b', 'c'),
    bin_lookup = c('under_10m', '10m_20m', '20m_50m', '50m_plus'),
    stop('Unknown reporting form: ', form)
  )
  c(size_pars, 'rho_pt')
}

predict_reporting_factor = function(w, s_pt, form, par) {
  x = log(pmax(w, 1) / 1e7)
  r_size = if (form == 'constant') {
    rep(clamp(exp(par[['a']]), 0.01, 5), length(w))
  } else if (form == 'log_linear') {
    clamp(exp(par[['a']] + par[['b']] * x), 0.01, 5)
  } else if (form == 'log_quadratic') {
    clamp(exp(par[['a']] + par[['b']] * x + par[['c']] * x^2), 0.01, 5)
  } else if (form == 'bounded_log_quadratic') {
    0.05 + (2.5 - 0.05) * plogis(par[['a']] + par[['b']] * x + par[['c']] * x^2)
  } else if (form == 'bin_lookup') {
    clamp(exp(par[size_bin_for(w)]), 0.01, 5)
  } else {
    stop('Unknown reporting form: ', form)
  }

  s_pt = clamp(replace_na(s_pt, 0), 0, 1)
  composition = 1 + (par[['rho_pt']] - 1) * s_pt
  clamp(r_size * composition, 0.01, 5)
}

initial_grid = function(form, quick = FALSE) {
  if (quick) {
    levels_a = log(c(0.5, 1.0, 1.5))
    levels_b = c(-0.25, 0, 0.25)
    levels_c = c(-0.05, 0, 0.05)
    bin_levels = log(c(0.5, 1, 1.5))
    rho_levels = c(0.6, 1.0)
  } else {
    levels_a = log(c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 2))
    levels_b = c(-0.75, -0.35, 0, 0.35, 0.75)
    levels_c = c(-0.15, 0, 0.15)
    bin_levels = log(c(0.35, 0.6, 0.85, 1.1, 1.5))
    rho_levels = c(0.4, 0.6, 0.8, 1.0)
  }

  if (form == 'constant') {
    grid = expand.grid(a = levels_a, rho_pt = rho_levels)
  } else if (form == 'log_linear') {
    grid = expand.grid(a = levels_a, b = levels_b, rho_pt = rho_levels)
  } else if (form == 'log_quadratic') {
    grid = expand.grid(a = levels_a, b = levels_b, c = levels_c, rho_pt = rho_levels)
  } else if (form == 'bounded_log_quadratic') {
    grid = expand.grid(a = seq(-2, 2, length.out = ifelse(quick, 3, 5)),
                       b = levels_b,
                       c = levels_c,
                       rho_pt = rho_levels)
  } else if (form == 'bin_lookup') {
    grid = expand.grid(
      under_10m = bin_levels,
      `10m_20m` = bin_levels,
      `20m_50m` = bin_levels,
      `50m_plus` = bin_levels,
      rho_pt = rho_levels,
      check.names = FALSE
    )
  } else {
    stop('Unknown reporting form: ', form)
  }

  as_tibble(grid)
}

param_bounds = function(form) {
  nms = reporting_par_names(form)
  if (form == 'bounded_log_quadratic') {
    lower = c(a = -8, b = -4, c = -2)
    upper = c(a = 8, b = 4, c = 2)
  } else {
    size_nms = setdiff(nms, 'rho_pt')
    lower = setNames(rep(-5, length(size_nms)), size_nms)
    upper = setNames(rep(2, length(size_nms)), size_nms)
    if (form %in% c('log_linear', 'log_quadratic')) {
      lower[size_nms != 'a'] = -3
      upper[size_nms != 'a'] = 3
    }
  }
  # Pass-through reporting factor relative to all-else, bounded to a plausible
  # valuation-discount range.
  lower['rho_pt'] = 0.3
  upper['rho_pt'] = 1.2
  list(lower = lower[nms], upper = upper[nms])
}


#-------------------------------------------------------------------------------
# Model evaluation
#-------------------------------------------------------------------------------

apply_candidate = function(cells, reporting_form, reporting_par,
                           taxable_form, taxable_fits, dsue_table,
                           policy, policy_index) {
  out = cells
  out$s_pt = out$pass_through / pmax(out$economic_gross, 1e-9)
  out$reporting_factor = predict_reporting_factor(
    out$economic_gross, out$s_pt, reporting_form, reporting_par
  )
  out$reported_gross = out$economic_gross * out$reporting_factor
  out$size_bin = size_bin_for(out$reported_gross)
  out$taxable_fraction = predict_taxable_fraction(
    out$reported_gross, taxable_form, taxable_fits
  )
  out$taxable_estate = out$reported_gross * out$taxable_fraction

  out$bea = bea_for_year(out$death_year, policy, policy_index)
  dsue_idx = match(out$size_bin, dsue_table$size_bin)
  out$p_dsue = dsue_table$p_dsue[dsue_idx]
  out$dsue_to_bea = dsue_table$dsue_to_bea[dsue_idx]
  out$p_dsue[is.na(out$p_dsue)] = 0
  out$dsue_to_bea[is.na(out$dsue_to_bea)] = 0
  out$dsue_amount = out$dsue_to_bea * out$bea
  out$liability_no_dsue = estate_tax_liability(out$taxable_estate, out$bea)
  out$liability_with_dsue =
    estate_tax_liability(out$taxable_estate, out$bea + out$dsue_amount)
  out$liability =
    (1 - out$p_dsue) * out$liability_no_dsue +
    out$p_dsue * out$liability_with_dsue
  out$filed = out$reported_gross >= out$bea
  # The modeled universe: units whose taxable estate exceeds the exemption and
  # therefore owe estate tax. This is the basis for the SOI moments.
  out$taxable = out$taxable_estate > out$bea

  out
}

model_soi_moments = function(cells, soi_targets, reporting_form, reporting_par,
                             taxable_form, taxable_fits, dsue_table,
                             policy_index) {
  modeled = apply_candidate(cells, reporting_form, reporting_par,
                            taxable_form, taxable_fits, dsue_table,
                            'historical', policy_index) %>%
    filter(death_year %in% unique(soi_targets$death_year)) %>%
    group_by(death_year, size_bin) %>%
    summarise(
      gross_count = sum(expected_weight * taxable, na.rm = TRUE),
      gross_amount = sum(expected_weight * taxable * reported_gross, na.rm = TRUE),
      taxable_estate = sum(expected_weight * taxable * taxable_estate, na.rm = TRUE),
      net_estate_tax = sum(expected_weight * liability, na.rm = TRUE),
      .groups = 'drop'
    )

  targets = soi_targets %>%
    transmute(
      death_year, size_bin,
      target_gross_count = gross_n,
      target_gross_amount = gross_amt,
      target_taxable_estate = taxable_estate_amt,
      target_net_estate_tax = net_estate_tax_amt
    )

  targets %>%
    left_join(modeled, by = c('death_year', 'size_bin')) %>%
    mutate(across(c(gross_count, gross_amount, taxable_estate, net_estate_tax),
                  ~ replace_na(.x, 0)))
}

model_score_moments = function(cells, score_targets, reporting_form, reporting_par,
                               taxable_form, taxable_fits, dsue_table,
                               policy_index) {
  needed_receipt_years = unique(score_targets$year)

  # Current-law baseline is OBBBA: the $15M permanent exclusion. The CBO
  # baseline-receipts target reflects this world (post-OBBBA, no TCJA sunset),
  # so it is matched to the OBBBA scenario.
  current_law = apply_candidate(cells, reporting_form, reporting_par,
                                taxable_form, taxable_fits, dsue_table,
                                'obbba', policy_index) %>%
    filter(receipt_year %in% needed_receipt_years) %>%
    group_by(receipt_year) %>%
    summarise(current_law_receipts = sum(expected_weight * liability, na.rm = TRUE) / 1e6,
              .groups = 'drop')

  # The pre-OBBBA TCJA sunset ($7.2M exclusion) is NOT the current baseline; it
  # exists only as the counterfactual JCT scored OBBBA against, so it is used
  # solely to form the obbba-vs-sunset policy delta.
  sunset = apply_candidate(cells, reporting_form, reporting_par,
                           taxable_form, taxable_fits, dsue_table,
                           'baseline', policy_index) %>%
    filter(receipt_year %in% needed_receipt_years) %>%
    group_by(receipt_year) %>%
    summarise(sunset_receipts = sum(expected_weight * liability, na.rm = TRUE) / 1e6,
              .groups = 'drop')

  modeled = full_join(current_law, sunset, by = 'receipt_year') %>%
    mutate(
      current_law_receipts = replace_na(current_law_receipts, 0),
      sunset_receipts = replace_na(sunset_receipts, 0),
      policy_delta = current_law_receipts - sunset_receipts
    ) %>%
    rename(year = receipt_year)

  score_targets %>%
    select(scenario, target_type, year, estate_value_millions, source) %>%
    mutate(
      modeled_value_millions = case_when(
        scenario == 'baseline' & target_type == 'baseline_receipts' ~
          modeled$current_law_receipts[match(year, modeled$year)],
        scenario == 'obbba_vs_sunset' & target_type == 'policy_delta' ~
          modeled$policy_delta[match(year, modeled$year)],
        TRUE ~ NA_real_
      ),
      modeled_value_millions = replace_na(modeled_value_millions, 0)
    )
}

objective_components = function(soi_moments, score_moments) {
  soi_long = soi_moments %>%
    transmute(
      death_year, size_bin,
      gross_count_model = gross_count,
      gross_count_target = target_gross_count,
      gross_amount_model = gross_amount,
      gross_amount_target = target_gross_amount,
      taxable_estate_model = taxable_estate,
      taxable_estate_target = target_taxable_estate,
      net_estate_tax_model = net_estate_tax,
      net_estate_tax_target = target_net_estate_tax
    ) %>%
    pivot_longer(
      cols = -c(death_year, size_bin),
      names_to = c('moment', '.value'),
      names_pattern = '(.+)_(model|target)$'
    ) %>%
    mutate(
      group = 'soi',
      year = death_year,
      weight = case_when(
        moment == 'net_estate_tax' ~ 2,
        TRUE ~ 1
      ),
      weight = weight * if_else(size_bin == 'under_10m', SOI_UNDER_10M_WEIGHT, 1),
      error = log_rel_error(model, target),
      objective = weight * error^2
    )

  score_long = score_moments %>%
    group_by(scenario, target_type, source) %>%
    summarise(
      model = sum(modeled_value_millions, na.rm = TRUE),
      target = sum(estate_value_millions, na.rm = TRUE),
      first_year = min(year),
      last_year = max(year),
      .groups = 'drop'
    ) %>%
    mutate(
      group = 'score',
      moment = paste(scenario, target_type, paste0(first_year, '_', last_year), sep = ':'),
      size_bin = NA_character_,
      year = NA_integer_,
      weight = 4,
      error = if_else(abs(target) <= 0, NA_real_, (model - target) / abs(target)),
      objective = weight * error^2
    ) %>%
    select(group, year, size_bin, moment, model, target, weight, error, objective)

  score_annual = score_moments %>%
    transmute(
      group = 'score_annual',
      year,
      size_bin = NA_character_,
      moment = paste(scenario, target_type, sep = ':'),
      model = modeled_value_millions,
      target = estate_value_millions,
      weight = 0,
      error = if_else(abs(target) <= 0, NA_real_, (model - target) / abs(target)),
      objective = 0
    )

  bind_rows(
    soi_long %>% select(group, year, size_bin, moment, model, target, weight, error, objective),
    score_annual,
    score_long
  )
}

objective_value = function(soi_moments, score_moments) {
  soi_specs = list(
    list(model = 'gross_count', target = 'target_gross_count', weight = 1),
    list(model = 'gross_amount', target = 'target_gross_amount', weight = 1),
    list(model = 'taxable_estate', target = 'target_taxable_estate', weight = 1),
    list(model = 'net_estate_tax', target = 'target_net_estate_tax', weight = 2)
  )

  soi_obj = 0
  bin_weight = if_else(soi_moments$size_bin == 'under_10m',
                       SOI_UNDER_10M_WEIGHT, 1)
  for (spec in soi_specs) {
    model = soi_moments[[spec$model]]
    target = soi_moments[[spec$target]]
    err = log_rel_error(model, target)
    soi_obj = soi_obj + sum(spec$weight * bin_weight * err^2, na.rm = TRUE)
  }

  score_obj = score_moments %>%
    group_by(scenario, target_type, source) %>%
    summarise(
      model = sum(modeled_value_millions, na.rm = TRUE),
      target = sum(estate_value_millions, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(error = if_else(abs(target) <= 0, NA_real_,
                           (model - target) / abs(target))) %>%
    summarise(objective = sum(4 * error^2, na.rm = TRUE)) %>%
    pull(objective)

  soi_obj + score_obj
}

evaluate_candidate = function(par, cells, soi_targets, score_targets,
                              reporting_form, taxable_form,
                              taxable_fits, dsue_table, policy_index) {
  names(par) = reporting_par_names(reporting_form)
  soi = model_soi_moments(cells, soi_targets, reporting_form, par,
                          taxable_form, taxable_fits, dsue_table, policy_index)
  scores = model_score_moments(cells, score_targets, reporting_form, par,
                               taxable_form, taxable_fits, dsue_table, policy_index)
  comps = objective_components(soi, scores)

  list(
    objective = sum(comps$objective, na.rm = TRUE),
    components = comps,
    soi_moments = soi,
    score_moments = scores
  )
}

evaluate_candidate_objective = function(par, cells, soi_targets, score_targets,
                                        reporting_form, taxable_form,
                                        taxable_fits, dsue_table, policy_index) {
  names(par) = reporting_par_names(reporting_form)
  soi = model_soi_moments(cells, soi_targets, reporting_form, par,
                          taxable_form, taxable_fits, dsue_table, policy_index)
  scores = model_score_moments(cells, score_targets, reporting_form, par,
                               taxable_form, taxable_fits, dsue_table, policy_index)
  objective_value(soi, scores)
}

calibrate_candidate = function(cells, soi_targets, score_targets,
                               reporting_form, taxable_form,
                               taxable_fits, dsue_table, policy_index,
                               quick, optim_starts, maxit) {
  grid = initial_grid(reporting_form, quick = quick)
  par_names = reporting_par_names(reporting_form)

  score_par = function(par) {
    names(par) = par_names
    evaluate_candidate_objective(par, cells, soi_targets, score_targets,
                                 reporting_form, taxable_form,
                                 taxable_fits, dsue_table, policy_index)
  }

  grid_scores = apply(grid[, par_names, drop = FALSE], 1, function(row) {
    score_par(as.numeric(row))
  })
  starts = grid[order(grid_scores), par_names, drop = FALSE] %>%
    slice_head(n = optim_starts)

  bounds = param_bounds(reporting_form)
  best_par = as.numeric(starts[1, ])
  names(best_par) = par_names
  best_obj = score_par(best_par)

  for (s in seq_len(nrow(starts))) {
    start = as.numeric(starts[s, ])
    names(start) = par_names
    opt = tryCatch(
      optim(
        par = start,
        fn = score_par,
        method = 'L-BFGS-B',
        lower = bounds$lower,
        upper = bounds$upper,
        control = list(maxit = maxit)
      ),
      error = function(e) NULL
    )
    if (!is.null(opt) && is.finite(opt$value) && opt$value < best_obj) {
      best_obj = opt$value
      best_par = opt$par
    }
  }

  final = evaluate_candidate(best_par, cells, soi_targets, score_targets,
                             reporting_form, taxable_form,
                             taxable_fits, dsue_table, policy_index)
  final$par = best_par
  final
}


#-------------------------------------------------------------------------------
# Output helpers
#-------------------------------------------------------------------------------

write_diagnostics = function(path, parameter_rows, score_targets, args) {
  best = parameter_rows %>% arrange(total_objective) %>% slice(1)
  lines = c(
    '# Estate Tax Calibration Diagnostics',
    '',
    paste0('- Tax-Data root: `', args$tax_data_root, '`'),
    paste0('- SOI file: `', args$soi_file, '`'),
    paste0('- Score targets: `', args$score_targets, '`'),
    paste0('- Gift-tax haircut on estate-and-gift targets: ', GIFT_TAX_HAIRCUT * 100, '%'),
    paste0('- Best objective candidate: `', best$reporting_form, ' / ', best$taxable_form, '`'),
    paste0('- Best objective value: ', signif(best$total_objective, 6)),
    '',
    '## Effective Score Targets',
    '',
    'Values below are in millions after the fixed gift-tax haircut when applicable.',
    '',
    score_targets %>%
      transmute(line = paste0(
        '- ', scenario, ' ', target_type, ' ', year, ': ',
        signif(estate_value_millions, 8), ' (', source, ')'
      )) %>%
      pull(line),
    '',
    '## Candidate Ranking',
    '',
    parameter_rows %>%
      arrange(total_objective) %>%
      transmute(line = paste0(
        '- ', reporting_form, ' / ', taxable_form,
        ': total=', signif(total_objective, 6),
        ', SOI=', signif(soi_objective, 6),
        ', score=', signif(score_objective, 6)
      )) %>%
      pull(line)
  )
  writeLines(lines, path)
}


#-------------------------------------------------------------------------------
# Main
#-------------------------------------------------------------------------------

main = function() {
  args = parse_args(commandArgs(trailingOnly = TRUE))

  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

  message('Loading score targets...')
  score_targets = load_score_targets(args$score_targets)

  receipt_years = unique(score_targets$year)
  score_death_years = receipt_years - 1L

  # Include all years required by SOI after loading its filing years. Policy
  # index needs both SOI death years and score death years, so load a broad
  # range first from the score years plus historical calibration range.
  candidate_policy_years = sort(unique(c(2018:2035, score_death_years)))
  policy_index = load_policy_index(candidate_policy_years, args$macro_root)

  message('Loading SOI targets...')
  soi_targets = load_soi_targets(args$soi_file, policy_index)

  needed_death_years = sort(unique(c(soi_targets$death_year, score_death_years)))

  message('Loading Tax-Data cells for years: ', paste(needed_death_years, collapse = ', '))
  cells = load_tax_data_cells(args$tax_data_root, needed_death_years, args$wealth_cells)
  message('  model cells: ', nrow(cells))

  # Taxable-fraction and DSUE tables use the full SOI panel (they are pure SOI
  # ratios, independent of Tax-Data wealth coverage).
  taxable_fits = fit_taxable_fraction_models(soi_targets)
  dsue_table = build_dsue_table(soi_targets)

  # The SOI *comparison* targets, however, can only be matched where the model
  # has mass. Drop (a) death years with no Tax-Data wealth coverage and (b)
  # size bins whose upper bound is below the filing exclusion -- the model never
  # files an estate below the exclusion, whereas SOI's small-estate filers are
  # mostly portability/DSUE-election returns we do not represent.
  available_death_years = sort(unique(cells$death_year))
  soi_targets_model = soi_targets %>%
    mutate(filing_exclusion = bea_for_year(death_year, 'historical', policy_index)) %>%
    filter(death_year %in% available_death_years, max_gross > filing_exclusion)

  dropped_years = setdiff(unique(soi_targets$death_year), available_death_years)
  if (length(dropped_years) > 0) {
    warning('No Tax-Data wealth coverage for SOI death year(s): ',
            paste(sort(dropped_years), collapse = ', '),
            '. Dropping them from the SOI objective.')
  }
  message('  SOI comparison cells retained: ', nrow(soi_targets_model),
          ' (death years: ',
          paste(sort(unique(soi_targets_model$death_year)), collapse = ', '), ')')
  if (nrow(soi_targets_model) == 0) {
    stop('No SOI comparison cells remain after coverage/exclusion filtering.')
  }

  reporting_forms = c('constant', 'log_linear', 'log_quadratic',
                      'bounded_log_quadratic', 'bin_lookup')
  taxable_forms = c('bin_lookup', 'smooth_logit')
  candidates = expand_grid(reporting_form = reporting_forms,
                           taxable_form = taxable_forms)

  parameter_rows = list()
  moment_rows = list()

  for (j in seq_len(nrow(candidates))) {
    rf = candidates$reporting_form[j]
    tf = candidates$taxable_form[j]
    message(sprintf('Calibrating candidate %d/%d: %s / %s',
                    j, nrow(candidates), rf, tf))

    fit = calibrate_candidate(
      cells = cells,
      soi_targets = soi_targets_model,
      score_targets = score_targets,
      reporting_form = rf,
      taxable_form = tf,
      taxable_fits = taxable_fits,
      dsue_table = dsue_table,
      policy_index = policy_index,
      quick = args$quick,
      optim_starts = args$optim_starts,
      maxit = args$maxit
    )

    comps = fit$components
    parameter_rows[[j]] = tibble(
      reporting_form = rf,
      taxable_form = tf,
      parameters = param_string(fit$par),
      total_objective = fit$objective,
      soi_objective = sum(comps$objective[comps$group == 'soi'], na.rm = TRUE),
      score_objective = sum(comps$objective[comps$group == 'score'], na.rm = TRUE),
      gift_tax_haircut = GIFT_TAX_HAIRCUT,
      wealth_cells = args$wealth_cells
    )

    moment_rows[[j]] = comps %>%
      mutate(
        reporting_form = rf,
        taxable_form = tf,
        parameters = param_string(fit$par)
      ) %>%
      select(reporting_form, taxable_form, parameters,
             group, year, size_bin, moment, model, target, weight, error, objective)
  }

  parameters = bind_rows(parameter_rows) %>% arrange(total_objective)
  moments = bind_rows(moment_rows)
  pareto = parameters %>%
    mutate(rank_total = row_number()) %>%
    select(rank_total, reporting_form, taxable_form, total_objective,
           soi_objective, score_objective, parameters, gift_tax_haircut,
           wealth_cells)

  write_csv(parameters, file.path(args$output_dir, 'estate_calibration_parameters.csv'))
  write_csv(moments, file.path(args$output_dir, 'estate_calibration_moments.csv'))
  write_csv(pareto, file.path(args$output_dir, 'estate_calibration_pareto.csv'))
  write_csv(score_targets, file.path(args$output_dir, 'estate_score_targets_effective.csv'))
  write_diagnostics(file.path(args$output_dir, 'estate_calibration_diagnostics.md'),
                    parameters, score_targets, args)

  message('Wrote calibration outputs to ', args$output_dir)
}

is_rscript_entrypoint = function() {
  cmd = commandArgs(trailingOnly = FALSE)
  file_arg = cmd[grepl('^--file=', cmd)]
  if (length(file_arg) == 0 || is.na(SCRIPT_PATH)) return(FALSE)
  normalizePath(sub('^--file=', '', file_arg[1]), winslash = '/', mustWork = FALSE) ==
    normalizePath(SCRIPT_PATH, winslash = '/', mustWork = FALSE)
}

if (is_rscript_entrypoint()) {
  main()
}
