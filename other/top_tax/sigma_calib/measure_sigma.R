#-------------------------------------------------------------------------------
# measure_sigma.R  --  pin the conversion margin, and write it.
#
# Measures the top-subset ordinary ETI at each trial sigma in the sweep grid,
# interpolates for the sigma that hits the Saez-Slemrod-Giertz central target of
# 0.25, and writes that value into config/calibrations/kg/conversion.yaml.
#
# This supersedes other/top_tax/archive/tests/compute_top_eti.R, which measured ONE
# leg and printed the number for a person to interpolate by hand. The ETI formula
# below is that script's, unchanged; what is new is that the grid, the inversion and
# the write are one operation, so the shipped number is the measured number and
# nobody transcribes it.
#
# THE ETI, exactly as the exhibit defines it:
#   O          = sum(w * pmax(txbl_inc - pmax(txbl_kg, 0), 0)) over the top subset
#                -- taxable income EXCLUDING net capital gains, after deductions
#   top subset = baseline-fixed membership: txbl_inc >= the scenario's sigma gate
#                threshold for that filing status. Fixed at baseline so the measured
#                elasticity is not contaminated by movement across the threshold.
#   ETI        = dlog(O) / dlog(1 - tau), tau = wage-weighted mean mtr_wages1
#   numerator  = the CONVENTIONAL leg (behavior on), denominator the STATIC one
#                (the mechanical rate change), which is what makes this a response.
#   The first year is dropped: it is the lead-in.
#
# Usage (through the sbatch wrapper -- it reads whole detail files):
#   sbatch other/top_tax/sigma_calib/measure_sigma.sbatch [charity]
#
# charity defaults to 100, matching the sweep generator's default and the conditions
# the shipped 0.16 was derived under. Pass 50 for the deferred re-derivation.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

source('src/misc/calibration_writer.R')

args    = commandArgs(trailingOnly = TRUE)
CHARITY = if (length(args) >= 1) args[1] else '100'
stopifnot(CHARITY %in% c('50', '100'))

LOCAL_ROOT = Sys.getenv('KG_CALIB_OUTPUT_ROOT',
                        '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1')
VINTAGE_SUFFIX = Sys.getenv('VINTAGE_SUFFIX', '')

CALIB_FILE  = 'config/calibrations/kg/conversion.yaml'
SHOCK       = 'topord_plus5'
TARGET_ETI  = 0.25

# The grid, which must agree with write_sigma_sweep.py's GRID.
runs = tribble(
  ~sigma, ~tag,
  0.00,   '00',
  0.16,   '16',
  0.30,   '30'
) %>%
  mutate(vintage = paste0('sigma_calib_', CHARITY, '_conv_', tag, VINTAGE_SUFFIX))

# One leg's ETI, averaged over the years after the lead-in.
leg_eti = function(vintage) {
  root = file.path(LOCAL_ROOT, vintage)
  years = list.files(file.path(root, 'baseline/static/detail'), pattern = '^\\d{4}[.]csv$') %>%
    str_remove('[.]csv$') %>% as.integer() %>% sort()
  if (length(years) < 2) {
    stop('vintage ', vintage, ' has fewer than two detail years -- has it run?')
  }

  per_year = map_dfr(years, function(t) {
    b = fread(file.path(root, 'baseline/static/detail', paste0(t, '.csv')),
              select = c('id', 'weight', 'filing_status', 'txbl_inc', 'txbl_kg',
                         'mtr_wages1', 'wages'), showProgress = FALSE)
    s = fread(file.path(root, SHOCK, 'static/detail', paste0(t, '.csv')),
              select = c('id', 'mtr_wages1'), showProgress = FALSE)
    cv = fread(file.path(root, SHOCK, 'conventional/detail', paste0(t, '.csv')),
               select = c('id', 'weight', 'txbl_inc', 'txbl_kg'), showProgress = FALSE)

    # The gate thresholds come from the shock leg's own sigma tracker. The old
    # no-sigma leg had none and had to borrow them from another run; binding the
    # grid's floor at sigma = 0 rather than dropping the module means every leg
    # writes its own, so nothing is borrowed.
    th = readRDS(file.path(root, SHOCK, 'conventional/supplemental',
                           'kg_dynamics_state', paste0(t, '.rds')))$sigma$thresholds

    b = merge(b, as.data.table(th), by = 'filing_status', all.x = TRUE)
    setkey(b, id); setkey(cv, id)
    top = !is.na(b$sigma_thresh) & b$txbl_inc >= b$sigma_thresh

    O = function(d, sel) d[sel, sum(weight * pmax(txbl_inc - pmax(txbl_kg, 0), 0))]
    O_b = O(b, top)
    O_c = O(cv[b[, .(id)], on = 'id'], top)

    j = merge(b[, .(id, weight, wages, mtr_wages1)],
              s[, .(id, mtr_s = mtr_wages1)], by = 'id')
    w  = pmax(j$wages, 0)
    ok = !is.na(j$mtr_s - j$mtr_wages1)
    dtau  = j[ok, sum(pmax(wages, 0) * (mtr_s - mtr_wages1))] / sum(w[ok])
    tau_b = j[ok, sum(pmax(wages, 0) * mtr_wages1)] / sum(w[ok])
    dlog_ntr = log((1 - (tau_b + dtau)) / (1 - tau_b))

    tibble(year = t, O_base = O_b, O_conv = O_c,
           dlogO = log(O_c / O_b), dlog_ntr = dlog_ntr,
           eti = if (abs(dlog_ntr) > 1e-12) log(O_c / O_b) / dlog_ntr else NA_real_)
  })

  list(per_year = per_year,
       eti = mean(per_year$eti[per_year$year > min(years)], na.rm = TRUE))
}

missing = runs$vintage[!dir.exists(file.path(LOCAL_ROOT, runs$vintage))]
if (length(missing) > 0) {
  stop('these sweep vintages are not on disk:\n  ', paste(missing, collapse = '\n  '),
       '\nGenerate the grid and launch it first:\n',
       '  python3 other/top_tax/sigma_calib/write_sigma_sweep.py --charity ', CHARITY, '\n',
       '  sbatch other/top_tax/sigma_calib/launch_sigma_calib.sbatch ', CHARITY)
}

measured = runs %>%
  mutate(eti = map_dbl(vintage, ~ leg_eti(.x)$eti))

out = sprintf('other/top_tax/sigma_calib/sigma_eti_charity%s.csv', CHARITY)
write_csv(measured, out)
print(as.data.frame(measured))
cat(sprintf('\nwrote %s\n', out))

#-------------------------------------------------------------------------------
# Interpolate for the sigma that hits the target ETI.
#
# Piecewise-linear between the bracketing grid points, NOT through the origin: at
# sigma = 0 the ETI is already about 0.22 (entity shifting and evasion), so the
# relation has a large intercept by construction. That intercept is the whole reason
# sigma is described as a residual, and forcing a line through zero would bury it.
#-------------------------------------------------------------------------------
o = measured[order(measured$sigma), ]
sigma_star = NA_real_
for (i in 1:(nrow(o) - 1)) {
  lo = o$eti[i]; hi = o$eti[i + 1]
  if ((lo - TARGET_ETI) * (hi - TARGET_ETI) <= 0) {
    wgt = (TARGET_ETI - lo) / (hi - lo)
    sigma_star = o$sigma[i] + wgt * (o$sigma[i + 1] - o$sigma[i])
    break
  }
}

cat('\n--- sigma pin ---------------------------------------------------\n')
cat(sprintf('charity elasticity module : charity/%s\n', CHARITY))
cat(sprintf('target top-subset ETI     : %.4f\n', TARGET_ETI))
cat(sprintf('grid sigma                : %s\n', paste(o$sigma, collapse = ', ')))
cat(sprintf('measured ETI              : %s\n', paste(sprintf('%.4f', o$eti), collapse = ', ')))
cat(sprintf('ETI at sigma = 0 (floor)  : %.4f  <- entity shifting + evasion alone\n',
            o$eti[o$sigma == 0]))
cat(sprintf('sigma* (piecewise linear) : %.4f\n', sigma_star))
cat('----------------------------------------------------------------\n')

if (is.na(sigma_star)) {
  stop('The grid does not bracket the target ETI (', TARGET_ETI, '), so there is ',
       'no interpolation to ship. Widen GRID in ',
       'other/top_tax/sigma_calib/write_sigma_sweep.py and re-run the sweep.')
}

#-------------------------------------------------------------------------------
# Write it. Reproduce-in-place / drift-to-.proposed, the same contract every other
# calibrator here follows: a calibrated value moving is a finding for the author.
#
# Expect drift at charity/50. That run is not a reproduction -- it is the deferred
# re-derivation, and moving the value is its purpose.
#-------------------------------------------------------------------------------

charity_note = if (CHARITY == '100') {
  "Derived under charity/100 (elasticity -1.0), which is what the shipped value
   has always been conditioned on, and which product runs do NOT use -- they use
   charity/50. That mismatch is the dated waiver on this entry and is not resolved
   by this run."
} else {
  "Derived under charity/50 (elasticity -0.5), the elasticity product runs
   actually use. This is the re-derivation the waiver was waiting for; if the value
   moved, the waiver on this entry should now be removed rather than re-dated."
}

calib_write_entry(
  path   = CALIB_FILE,
  entry  = 'conv',
  value  = round(sigma_star, 4),
  fields = list(
    kind = 'calibrated',
    set  = format(Sys.Date()),
    target = calib_prose(sprintf(
      'Top-subset ordinary-income ETI of %.2f (Saez-Slemrod-Giertz central; taxable
       income EXCLUDING net capital gains, after deductions, top-bracket membership
       fixed at baseline) measured on the +5pp top-ordinary leg (%s, %s) with the
       full behavior stack running. sigma is the RESIDUAL conversion margin: at
       sigma = 0 entity shifting and evasion alone produce an ETI of %.4f, so sigma
       closes the remaining gap. Grid sigma %s gave ETI %s; the pin is the
       piecewise-linear interpolation onto the target, which does NOT force the
       origin because the intercept is the point.',
      TARGET_ETI, SHOCK, '2025:2035', o$eti[o$sigma == 0],
      paste(o$sigma, collapse = ' '),
      paste(sprintf('%.4f', o$eti), collapse = ' '))),
    derived_under = list(tax_data          = '2026070814',
                         macro_projections = '2026022522'),
    invalidated_by = c('src/behavior/entity_shifting/pearce_prisinzano.R',
                       'src/behavior/evasion/debacker.R',
                       sprintf('src/behavior/charity/%s.R', CHARITY),
                       'src/sim/sigma_conversion.R'),
    rederive = 'other/top_tax/sigma_calib/measure_sigma.R',
    note = calib_prose(sprintf(
      "History 0.08 -> 0.157 -> 0.16. The 2026-07-12 move was driven by
       entity-shifting tau_eq repricing plus the evasion cross-base fix, not by the
       estate build. Method and measured legs: other/top_tax/sigma_explainer.md.
       %s
       The kg Bellman calibration is orthogonal (disjoint base: ordinary income
       excludes realizations) unless the pool ever starts taxing gains.
       Measured from vintages: %s. Per-year ETIs:
       other/top_tax/sigma_calib/sigma_eti_charity%s.csv.",
      charity_note, paste(measured$vintage, collapse = ' '), CHARITY))))
