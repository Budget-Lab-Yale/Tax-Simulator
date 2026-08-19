#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/validate_state_weights.R
#
# Phase 1 §4 comparison harness, part 2: decision-relevant validation of the
# sweep's candidate weight sets (configs 7 and 11 of research/state_weights/scripts/sweep_state_weights.R,
# plus the counts-IPF prior as the Approach-A-alone reference).
#
# Usage (repo root; run under sbatch -- see workstream log for memory):
#   Rscript research/state_weights/scripts/validate_state_weights.R --misses
#       persistent-miss diagnosis: targeted cells with err > 2% in EVERY
#       candidate config (7, 8, 11, 12, 13) -> structural-infeasibility list
#   Rscript research/state_weights/scripts/validate_state_weights.R --untargeted
#       held-out HT2 series never used in calibration (Schedule C net income,
#       taxable pensions, taxable social security, federal income tax after
#       credits), share-normalized exactly like the targeted set
#   Rscript research/state_weights/scripts/validate_state_weights.R --pilot
#       IL/CO/NY state income tax liability under each candidate weight set
#       (filer + nonfiler partitions), for comparison against external
#       collections benchmarks
#
# Outputs: CSVs + logs under {SCRATCH}/sweep/validation/
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr)
  library(stringr); library(yaml)
})
source('src/data/state_weights.R')

SCRATCH   <- '/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp'
SWEEP_DIR <- file.path(SCRATCH, 'sweep')
VAL_DIR   <- file.path(SWEEP_DIR, 'validation')
YEAR      <- 2022L
CANDIDATES <- c(7L, 11L)            # decision candidates
ALL_CFGS   <- c(7L, 8L, 11L, 12L, 13L)

dir.create(VAL_DIR, showWarnings = FALSE, recursive = TRUE)

load_inputs_full <- function() {
  h5 <- readRDS(file.path(SCRATCH, 'hardened_fit_2022.rds'))
  h5$inputs
}

# Reload tu exactly as research/state_weights/scripts/build_state_weights.R (the row order p$idx indexes),
# plus the detail columns the held-out series need
load_tu <- function() {
  input_cols <- c('id','weight','filer','filing_status','n_dep','age1',
                  'wages','txbl_int','exempt_int','div_ord','div_pref',
                  'kg_st','kg_lt','salt_inc_sales','salt_prop',
                  'first_mort_int','second_mort_int','txbl_pens_dist',
                  'txbl_ira_dist','gross_ss','sole_prop')
  td_vintage <- read_yaml('./config/interfaces/interface_versions.yaml')$`Tax-Data`$default_vintage
  td_path <- file.path(read_yaml('./config/interfaces/output_roots.yaml')$production,
                       'model_data/Tax-Data/v1', td_vintage, 'baseline',
                       sprintf('tax_units_%d.csv', YEAR))
  detail_dir <- file.path(read_yaml('./config/interfaces/output_roots.yaml')$local,
                          'model_data/Tax-Simulator/v1/weights_2022/baseline/static/detail')
  tu <- fread(td_path, select = input_cols)
  detail <- fread(file.path(detail_dir, sprintf('%d.csv', YEAR)),
                  select = c('id','agi','eitc','txbl_ss','liab_iit','liab_iit_net'))
  tu <- merge(tu, detail, by = 'id', all.x = TRUE)
  stopifnot(!anyNA(tu$agi))
  tu
}

load_P <- function(cfg_id) readRDS(file.path(SWEEP_DIR, paste0('fit_', cfg_id, '.rds')))$P

target_errs <- function(p, P) {
  rbindlist(lapply(seq_along(p$targets), function(i) {
    t <- p$targets[[i]]
    data.table(tgt_i = i, series = t$series, state_i = t$state,
               stub = if (is.null(t$stub)) NA_integer_ else t$stub,
               target = t$target,
               that = sum(p$w[t$rows] * P[t$rows, t$state] * t$x),
               support = sum(t$x != 0))
  }))[, err := abs(that / target - 1)]
}

args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]

#----------------------------------------------------------
# --misses: cells that fail in every candidate configuration
#----------------------------------------------------------
if (identical(mode, '--misses')) {
  inputs <- load_inputs_full()
  p <- inputs$filers
  jur <- inputs$jurisdictions

  errs <- rbindlist(lapply(ALL_CFGS, function(cfg) {
    e <- target_errs(p, load_P(cfg)); e[, cfg := cfg]; e
  }))
  wide <- dcast(errs, tgt_i + series + state_i + stub + target + support ~ cfg,
                value.var = 'err')
  cfg_cols <- as.character(ALL_CFGS)
  wide[, miss_all := rowSums(.SD > 0.02) == length(cfg_cols), .SDcols = cfg_cols]
  wide[, state := jur[state_i]]

  cat(sprintf('Targeted cells: %d | missed (>2%%) in ALL %d configs: %d (%.1f%%)\n\n',
              nrow(wide), length(ALL_CFGS), sum(wide$miss_all),
              100 * mean(wide$miss_all)))
  cat('Persistent misses by series:\n')
  print(wide[, .(n = .N, n_miss = sum(miss_all)), by = series][order(-n_miss)])
  cat('\nPersistent misses by stub:\n')
  print(wide[, .(n = .N, n_miss = sum(miss_all)), by = stub][order(stub)])
  cat('\nPersistent misses by state (top 15):\n')
  print(wide[, .(n = .N, n_miss = sum(miss_all)), by = state][order(-n_miss)][1:15])
  cat('\nSupport profile (records with x != 0 in the cell):\n')
  print(wide[, .(median_support = as.numeric(median(support)),
                 p10_support = quantile(support, .1)), by = miss_all])
  cat('\nTarget size profile (abs target, $ or returns):\n')
  print(wide[, .(median_target = median(abs(target))), by = .(miss_all, amount = grepl('amt', series))])

  fwrite(wide[miss_all == TRUE][order(series, state, stub)],
         file.path(VAL_DIR, 'persistent_misses.csv'))
  cat('\nWrote ', file.path(VAL_DIR, 'persistent_misses.csv'), '\n')
  quit(save = 'no')
}

#----------------------------------------------------------
# --untargeted: held-out HT2 series (never calibrated)
#----------------------------------------------------------
if (identical(mode, '--untargeted')) {
  inputs <- load_inputs_full()
  p   <- inputs$filers
  jur <- inputs$jurisdictions
  tu  <- load_tu()
  tu_f <- tu[p$idx]
  stopifnot(nrow(tu_f) == length(p$w), all(abs(tu_f$weight - p$w) < 1e-9))
  stub <- assign_ht2_stub(tu_f$agi, YEAR)

  # Held-out HT2 codes -> PUF x-vectors. A00900 Schedule C net income;
  # A01700 taxable pensions/annuities; A02500 taxable social security;
  # A06500 income tax after credits. kg_amt (A01000) -- excluded from
  # calibration for mixed sign -- evaluated on the positive part only.
  HELD_OUT <- list(
    n_bus       = list(code = 'N00900', x = as.numeric(tu_f$sole_prop != 0)),
    bus_amt     = list(code = 'A00900', x = tu_f$sole_prop),
    n_pens      = list(code = 'N01700', x = as.numeric(tu_f$txbl_pens_dist != 0)),
    pens_amt    = list(code = 'A01700', x = tu_f$txbl_pens_dist),
    n_ss        = list(code = 'N02500', x = as.numeric(tu_f$txbl_ss > 0)),
    ss_amt      = list(code = 'A02500', x = tu_f$txbl_ss),
    n_fedtax    = list(code = 'N06500', x = as.numeric(tu_f$liab_iit > 0)),
    fedtax_amt  = list(code = 'A06500', x = pmax(tu_f$liab_iit, 0)),
    kg_amt_pos  = list(code = 'A01000', x = pmax(tu_f$kg_st + tu_f$kg_lt, 0))
  )

  # Raw HT2 held-out cells (state x stub), same reader conventions as read_ht2
  ht2_path <- file.path(read_yaml('./config/interfaces/output_roots.yaml')$production,
                        'raw_data/IRS-GEO/state/HT2', sprintf('ht2_%d.csv.gz', YEAR))
  d <- fread(cmd = paste('zcat', shQuote(ht2_path)), colClasses = 'character')
  setnames(d, toupper(names(d)))
  parse_num <- function(x) as.numeric(str_replace_all(x, ',', ''))
  codes <- unique(vapply(HELD_OUT, `[[`, '', 'code'))
  ht2 <- d[, c('STATE', 'AGI_STUB', codes), with = FALSE]
  ht2 <- ht2[STATE %in% jur & AGI_STUB != '0']
  for (cc in codes) ht2[, (cc) := parse_num(get(cc)) * ifelse(grepl('^A', cc), 1000, 1)]
  ht2[, stub := as.integer(AGI_STUB)]

  # Candidate P matrices: prior alone (Approach A reference) + configs
  prior <- readRDS(file.path(SWEEP_DIR, 'prior_counts_2022.rds'))
  Ps <- c(list(prior = prior),
          setNames(lapply(CANDIDATES, load_P), paste0('cfg', CANDIDATES)))

  res <- rbindlist(lapply(names(HELD_OUT), function(ser) {
    ho   <- HELD_OUT[[ser]]
    x    <- ho$x
    natl <- sum(p$w * x)
    rbindlist(lapply(sort(unique(ht2$stub)), function(sb) {
      rows  <- which(stub == sb)
      if (sum(x[rows] != 0) == 0) return(NULL)
      cells <- ht2[stub == sb & get(ho$code) > 0]
      shares <- cells[[ho$code]] / sum(cells[[ho$code]])
      tgt_natl <- sum(p$w[rows] * x[rows])              # PUF national for the stub
      if (tgt_natl <= 0) return(NULL)
      rbindlist(lapply(names(Ps), function(nm) {
        P <- Ps[[nm]]
        that <- vapply(seq_len(nrow(cells)), function(k) {
          s <- match(cells$STATE[k], jur)
          sum(p$w[rows] * P[rows, s] * x[rows])
        }, numeric(1))
        data.table(series = ser, config = nm, stub = sb, state = cells$STATE,
                   target = tgt_natl * shares, that = that)
      }))
    }))
  }))
  res[, err := abs(that / target - 1)]

  cat('=== UNTARGETED (held-out HT2 series), share-normalized like calibration ===\n')
  smry <- dcast(res[, .(within2 = round(100 * mean(err <= 0.02), 1),
                        mard = round(100 * mean(err), 2)), by = .(series, config)],
                series ~ config, value.var = c('within2', 'mard'))
  print(smry)
  cat('\nOverall by config:\n')
  print(res[, .(within2 = round(100 * mean(err <= 0.02), 1),
                mard = round(100 * mean(err), 2), n = .N), by = config])
  fwrite(res, file.path(VAL_DIR, 'untargeted_heldout.csv'))
  cat('\nWrote ', file.path(VAL_DIR, 'untargeted_heldout.csv'), '\n')
  quit(save = 'no')
}

#----------------------------------------------------------
# --pilot: IL/CO/NY liability under candidate weights
#----------------------------------------------------------
if (identical(mode, '--pilot')) {
  # Full model stack for the state calculator (mirrors research/state_tax/cross_model/run_cross_model.R)
  return_vars <- list()
  invisible(capture.output(suppressPackageStartupMessages(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  )))
  list.files('./src', recursive = TRUE, pattern = '\\.[Rr]$') %>%
    purrr::walk(~ if (.x != 'main.R' && !startsWith(.x, 'slurm/')) source(file.path('./src/', .x)))

  PILOTS <- c('IL', 'CO', 'NY')
  inputs <- load_inputs_full()
  jur    <- inputs$jurisdictions

  # Post-federal 2022 tax units + indexes from the cross-model cache
  prep <- readRDS('./research/state_tax/cross_model/cache/fed_calc_2022.rds')
  state_law <- build_state_tax_law(states = PILOTS, years = 2022L,
                                   indexes = prep$indexes)
  credit_tables <- attr(state_law, 'credit_tables')

  liab <- rbindlist(lapply(PILOTS, function(st) {
    law_slice <- state_law %>% filter(state == st, year == 2022) %>% select(-state, -year)
    out <- prep$tax_units %>%
      left_join(law_slice, by = 'filing_status') %>%
      do_state_taxes(credit_tables = state_credit_tables_for_year(credit_tables, st, 2022))
    data.table(id = prep$tax_units$id, state = st, liab_st = out$liab_st_iit)
  }))

  # Candidate weight sets: filers from the sweep fits; nonfilers from the
  # exact counts-IPF on the nonfiler partition (shared across candidates)
  tu <- load_tu()
  pn <- inputs$nonfilers
  Pn <- fit_calibration(pn$w, pn$P0, pn$targets, n_iter = 50)$P
  pf <- inputs$filers

  totals <- rbindlist(lapply(CANDIDATES, function(cfg) {
    Pf <- load_P(cfg)
    rbindlist(lapply(PILOTS, function(st) {
      s <- match(st, jur)
      W <- data.table(id = c(tu$id[pf$idx], tu$id[pn$idx]),
                      w_st = c(pf$w * Pf[, s], pn$w * Pn[, s]))
      l <- merge(liab[state == st], W, by = 'id')
      data.table(config = paste0('cfg', cfg), state = st,
                 liab_bn = sum(l$w_st * l$liab_st) / 1e9,
                 n_returns_m = sum(l$w_st[l$liab_st != 0]) / 1e6)
    }))
  }))

  # National (weights-free) reference: what the record x national-weight total
  # would be if the WHOLE country faced this state's law -- context only
  cat('=== Pilot-state IIT liability under candidate weights ($bn, TY2022) ===\n')
  print(dcast(totals, state ~ config, value.var = 'liab_bn'))
  cat('\nReturns with nonzero liability (millions):\n')
  print(dcast(totals, state ~ config, value.var = 'n_returns_m'))
  fwrite(totals, file.path(VAL_DIR, 'pilot_liability.csv'))
  cat('\nWrote ', file.path(VAL_DIR, 'pilot_liability.csv'), '\n')
  quit(save = 'no')
}

stop('mode must be --misses, --untargeted, or --pilot')
