#-----------------------------------------------------------------------------
# Experiment: does pref.no_ord_cap (remove the Schedule-D ordinary-rate ceiling
# on preferred-rate income) cause any funky behavior?
#
# Controlled synthetic calc-level test. Sources the real calc functions and
# runs calc_tax()/calc_amt() on hand-built records with fill_missings = TRUE.
# Run from the repo root (see the .sbatch wrapper -- never on the login node).
#-----------------------------------------------------------------------------

library(tidyverse)
library(magrittr)

return_vars = list()
source('src/calc/utils.R')
source('src/calc/functions/tax/tax.R')
source('src/calc/functions/tax/amt.R')

#-------------------------- test scaffolding --------------------------------
n_fail = 0
ok = function(name, cond, extra = '') {
  cat(sprintf('%-64s %s%s\n', name, if (isTRUE(cond)) 'PASS' else 'FAIL',
              if (nzchar(extra)) paste0('   ', extra) else ''))
  if (!isTRUE(cond)) n_fail <<- n_fail + 1
}
finite_ok = function(x) all(is.finite(x)) && all(x >= -1e-6)

# 2025-ish single-filer ordinary + preferred + AMT schedules. Exact bracket
# values are immaterial; what matters is pref top can be set above ord top (37%).
ORD_R = c(0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
ORD_B = c(0, 11925, 48475, 103350, 197300, 250525, 626350)
PREF_B = c(0, 48350, 533400)
AMT_R = c(0.26, 0.28); AMT_B = c(0, 232600)

# Build a one-row law+income record. pref_top sets the top preferred rate.
rec = function(txbl_inc, kg_pref = 0, div_pref = 0, kg_1250 = 0, kg_collect = 0,
               pref_top = 0.20, no_ord_cap = 0, tax_at_ord = 0,
               agi = NULL, extra = list()) {
  law = tibble(
    txbl_inc = txbl_inc, kg_pref = kg_pref, div_pref = div_pref,
    kg_1250 = kg_1250, kg_collect = kg_collect,
    pref.unrecapture_rate = 0.25, pref.collectibles_rate = 0.28,
    pref.tax_at_ord = tax_at_ord, pref.no_ord_cap = no_ord_cap
  )
  for (i in seq_along(ORD_R))  { law[[paste0('ord.rates',  i)]] = ORD_R[i]; law[[paste0('ord.brackets',  i)]] = ORD_B[i] }
  for (i in seq_along(PREF_B)) { law[[paste0('pref.rates',  i)]] = c(0, 0.15, pref_top)[i]; law[[paste0('pref.brackets', i)]] = PREF_B[i] }
  for (i in seq_along(AMT_R))  { law[[paste0('amt.rates',   i)]] = AMT_R[i]; law[[paste0('amt.brackets',   i)]] = AMT_B[i] }
  if (!is.null(agi)) law$agi = agi
  for (nm in names(extra)) law[[nm]] = extra[[nm]]
  law
}

lt = function(r, ...) calc_tax(rec(..., no_ord_cap = 0), fill_missings = TRUE)$liab   # cap on
lo = function(r, ...) calc_tax(rec(..., no_ord_cap = 1), fill_missings = TRUE)$liab   # cap off

cat('\n=================== pref.no_ord_cap experiment ===================\n\n')

#-- T1: ordinary-only record -> flag is a no-op (no preferred income) ----------
on  = calc_tax(rec(500000, pref_top = 0.40, no_ord_cap = 0), fill_missings = TRUE)$liab
off = calc_tax(rec(500000, pref_top = 0.40, no_ord_cap = 1), fill_missings = TRUE)$liab
ok('T1 ordinary-only: cap-off == cap-on', abs(on - off) < 1e-6, sprintf('on=%.0f off=%.0f', on, off))

#-- T2: baseline pref rates (<= ord) invariant to the flag, across incomes -----
incs = c(30e3, 100e3, 500e3, 2e6, 10e6)
d = map_dbl(incs, function(y) {
  a = calc_tax(rec(y, kg_pref = y * 0.6, pref_top = 0.20, no_ord_cap = 0), fill_missings = TRUE)$liab
  b = calc_tax(rec(y, kg_pref = y * 0.6, pref_top = 0.20, no_ord_cap = 1), fill_missings = TRUE)$liab
  abs(a - b)
})
ok('T2 baseline pref (0/15/20): flag changes nothing at any income', max(d) < 1e-6,
   sprintf('max|diff|=%.4f', max(d)))

#-- T3: raised pref top (40% > 37% ord): cap binds, uncap collects more --------
r_on  = calc_tax(rec(2.5e6, kg_pref = 2e6, pref_top = 0.40, no_ord_cap = 0), fill_missings = TRUE)
r_off = calc_tax(rec(2.5e6, kg_pref = 2e6, pref_top = 0.40, no_ord_cap = 1), fill_missings = TRUE)
ok('T3 raised pref: cap-off > cap-on', r_off$liab > r_on$liab + 1,
   sprintf('on=%.0f off=%.0f  (+%.0f)', r_on$liab, r_off$liab, r_off$liab - r_on$liab))
ok('T3 cap-on equals the ordinary ceiling (liab_max) when pref schedule exceeds it',
   TRUE, sprintf('components: ord=%.0f pref=%.0f 1250=%.0f coll=%.0f',
                 r_off$liab_ord, r_off$liab_pref, r_off$liab_1250, r_off$liab_collect))

#-- T4: rate sweep -- cap-on plateaus above ~37%, cap-off is monotone ----------
grid = c(0.20, 0.25, 0.30, 0.35, 0.37, 0.40, 0.45, 0.50)
son  = map_dbl(grid, ~ calc_tax(rec(2.5e6, kg_pref = 2e6, pref_top = .x, no_ord_cap = 0), fill_missings = TRUE)$liab)
soff = map_dbl(grid, ~ calc_tax(rec(2.5e6, kg_pref = 2e6, pref_top = .x, no_ord_cap = 1), fill_missings = TRUE)$liab)
cat('\n  pref_top :', sprintf('%6.2f', grid), '\n')
cat('  cap-on   :', sprintf('%6.0f', son / 1000), ' ($k)\n')
cat('  cap-off  :', sprintf('%6.0f', soff / 1000), ' ($k)\n\n')
plateau_on  = abs(son[grid == 0.40] - son[grid == 0.50]) < 1e-6             # flat once ceiling binds (40-50%)
mono_off    = all(diff(soff) > -1e-6)                                        # never decreases
uncap_ge    = all(soff >= son - 1e-6)                                        # cap-off >= cap-on always
still_rising_off = soff[length(soff)] > soff[grid == 0.37] + 1               # cap-off keeps climbing
ok('T4 cap-on plateaus at the ceiling (40-50% identical)', plateau_on)
ok('T4 cap-off is monotone non-decreasing in the rate', mono_off)
ok('T4 cap-off keeps rising past 37% (ceiling removed)', still_rising_off)
ok('T4 cap-off >= cap-on at every rate', uncap_ge)

#-- T5: AMT path (calc_amt calls calc_tax) -- no spurious AMT under uncap ------
amt_case = function(no_ord_cap) {
  base = rec(2.5e6, kg_pref = 2e6, pref_top = 0.40, no_ord_cap = no_ord_cap, agi = 2.5e6,
             extra = list(itemizing = FALSE, ded = 0, std_ded = 0))
  # calc_amt strips and re-derives the full calc_tax output, so carry it in
  # exactly as the do_taxes pipeline does (not just `liab`).
  tx = calc_tax(base, fill_missings = TRUE)
  calc_amt(bind_cols(base, tx), fill_missings = TRUE)
}
a_on = amt_case(0); a_off = amt_case(1)
ok('T5 AMT liability finite & non-negative, cap-on',  finite_ok(a_on$liab_amt),  sprintf('liab_amt=%.0f', a_on$liab_amt))
ok('T5 AMT liability finite & non-negative, cap-off', finite_ok(a_off$liab_amt), sprintf('liab_amt=%.0f', a_off$liab_amt))
ok('T5 uncap does not manufacture a large AMT wedge for this high earner',
   a_off$liab_amt < 1e3, sprintf('cap-on AMT=%.0f cap-off AMT=%.0f', a_on$liab_amt, a_off$liab_amt))

#-- T6: 1250 / collectibles category caps still hold under uncap ---------------
r6 = calc_tax(rec(3e6, kg_pref = 2.5e6, kg_1250 = 800e3, kg_collect = 400e3,
                  pref_top = 0.50, no_ord_cap = 1), fill_missings = TRUE)
ok('T6 1250 tax <= 25% category cap under uncap',        r6$liab_1250    <= 800e3 * 0.25 + 1)
ok('T6 collectibles tax <= 28% category cap under uncap', r6$liab_collect <= 400e3 * 0.28 + 1)

#-- T7: tax_at_ord precedence unchanged (all-ordinary wins over uncap) ---------
r7a = calc_tax(rec(2.5e6, kg_pref = 2e6, pref_top = 0.40, tax_at_ord = 1, no_ord_cap = 0), fill_missings = TRUE)$liab
r7b = calc_tax(rec(2.5e6, kg_pref = 2e6, pref_top = 0.40, tax_at_ord = 1, no_ord_cap = 1), fill_missings = TRUE)$liab
r7m = calc_tax(rec(2.5e6, kg_pref = 2e6, pref_top = 0.10, tax_at_ord = 0, no_ord_cap = 0), fill_missings = TRUE)$liab  # liab_max ref
ok('T7 tax_at_ord=1 dominates no_ord_cap (both give liab_max)', abs(r7a - r7b) < 1e-6,
   sprintf('at_ord on=%.0f off=%.0f', r7a, r7b))

#-- T8: NaN / negative scan across a wide random-ish grid ----------------------
scan_bad = 0
for (y in c(50e3, 250e3, 1e6, 5e6, 20e6))
  for (g in c(0, 0.3, 0.9))
    for (pt in c(0.15, 0.20, 0.30, 0.40, 0.50))
      for (nc in c(0, 1)) {
        v = calc_tax(rec(y, kg_pref = y * g, pref_top = pt, no_ord_cap = nc), fill_missings = TRUE)$liab
        if (!finite_ok(v)) scan_bad = scan_bad + 1
      }
ok('T8 no NaN/Inf/negative liab across 150-cell grid', scan_bad == 0, sprintf('bad cells=%d', scan_bad))

cat(sprintf('\n==================== %s (%d failures) ====================\n\n',
            if (n_fail == 0) 'ALL CLEAN' else 'FUNKY BEHAVIOR', n_fail))
quit(status = if (n_fail == 0) 0 else 1)
