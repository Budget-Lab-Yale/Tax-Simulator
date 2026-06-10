#-------------------------------------------------------------------------------
# test_allocator.R
#
# Synthetic unit tests for the stage-2 rank-matching heir allocator
# (src/data/post_processing/estate_allocator.R). Run via sbatch from the repo
# root (test_allocator.sbatch). Exits nonzero on any failure.
#-------------------------------------------------------------------------------

library(tidyverse)
library(magrittr)

setwd(Sys.getenv('TAXSIM_ROOT', unset = '.'))
source('./src/data/post_processing/estate_allocator.R')

fails = 0
check = function(label, cond) {
  status = if (isTRUE(cond)) 'PASS' else 'FAIL'
  if (!isTRUE(cond)) fails <<- fails + 1
  cat(sprintf('[%s] %s\n', status, label))
}

# Helper: detail rows. Heirs must exist in detail to carry weight; non-decedent
# rows get estate_m = 0
detail_row = function(id, weight = 1, dep_status = 0, m = 0, p_dsue = 0,
                      T_dsue = 0, T_nodsue = 0, n = 0) {
  tibble(id = id, weight = weight, dep_status = dep_status, estate_m = m,
         estate_p_dsue = p_dsue, liab_estate_dsue = T_dsue,
         liab_estate_nodsue = T_nodsue, estate_distributable = n)
}


#-------------------------------------------------------------------------------
# 1. Single estate, many heirs: top heirs taxed at the estate's average rate,
#    heirs below the bequest mass get zero, cutoff endogenous
#-------------------------------------------------------------------------------

# Estate: dw = 1, n = 10M, T = 4M -> b = 10M at rate 0.4
# Heirs: x = 6M, 4M, 3M (mu = 6M, 4M, 3M). First two tile exactly 10M
detail = bind_rows(
  detail_row(1, m = 1, T_nodsue = 4e6, n = 10e6),
  detail_row(101), detail_row(102), detail_row(103)
)
heir_px = tibble(id        = c(101, 102, 103),
                 p_inheritance = 1,
                 inheritance   = c(6e6, 4e6, 3e6))
res = allocate_estate_to_heirs(detail, heir_px, 2030, 'test1')
lam = res$heirs$estate_tax_liability[match(c(101, 102, 103), res$heirs$id)]
check('1a: top heir taxed at estate avg rate',  abs(lam[1] - 0.4 * 6e6) < 1)
check('1b: second heir taxed at same rate',     abs(lam[2] - 0.4 * 4e6) < 1)
check('1c: heir below cutoff gets zero',        lam[3] == 0)
check('1d: cutoff_x is last taxed heir',        res$diag$cutoff_x == 4e6)
check('1e: identity', abs(res$diag$allocated_tax - 4e6) < 1e-3)

#-------------------------------------------------------------------------------
# 2. Straddling heir gets the mass-weighted blended rate
#-------------------------------------------------------------------------------

# Estates: b1 = 5M @ 0.4, b2 = 5M @ 0.2. Heir 1: mu = 8M -> covers all of
# estate 1 and 3M of estate 2: blended = (5*.4 + 3*.2)/8 = 0.325
detail = bind_rows(
  detail_row(1, m = 1, T_nodsue = 2.0e6, n = 5e6),
  detail_row(2, m = 1, T_nodsue = 1.0e6, n = 5e6),
  detail_row(101), detail_row(102)
)
heir_px = tibble(id = c(101, 102), p_inheritance = 1,
                 inheritance = c(8e6, 7e6))
res = allocate_estate_to_heirs(detail, heir_px, 2030, 'test2')
lam = res$heirs$estate_tax_liability[match(c(101, 102), res$heirs$id)]
check('2a: straddling heir blended rate', abs(lam[1] - 0.325 * 8e6) < 1)
# Heir 2 takes remaining 2M of estate 2's mass: rate = (2*.2)/7
check('2b: partial heir below', abs(lam[2] - (2e6 * 0.2) / 7e6 * 7e6) < 1)
check('2c: identity', abs(res$diag$allocated_tax - 3e6) < 1e-3)

#-------------------------------------------------------------------------------
# 3. DSUE branch split: only the taxed branch enters, weighted by its state
#    probability
#-------------------------------------------------------------------------------

# Single record at the kink: DSUE state untaxed, no-DSUE state taxed 1M with
# p_dsue = 0.6 -> taxed branch dw = 0.4, tax mass = 0.4M
detail = bind_rows(
  detail_row(1, m = 1, p_dsue = 0.6, T_dsue = 0, T_nodsue = 1e6, n = 8e6),
  detail_row(101)
)
heir_px = tibble(id = 101, p_inheritance = 1, inheritance = 10e6)
res = allocate_estate_to_heirs(detail, heir_px, 2030, 'test3')
check('3a: branch-weighted tax mass', abs(res$diag$tax_mass - 0.4e6) < 1e-3)
check('3b: one taxed branch only',    res$diag$n_taxed_branches == 1)
# Heir absorbs b = 0.4 * 8M = 3.2M of mass at rate 1/8 over 10M inheritance
check('3c: heir liability', abs(res$heirs$estate_tax_liability[1] - 0.4e6) < 1e-3)

#-------------------------------------------------------------------------------
# 4. Heir-ladder exhaustion is a hard error
#-------------------------------------------------------------------------------

detail = bind_rows(
  detail_row(1, m = 1, T_nodsue = 4e6, n = 10e6),
  detail_row(101)
)
heir_px = tibble(id = 101, p_inheritance = 1, inheritance = 5e6)
err = tryCatch({allocate_estate_to_heirs(detail, heir_px, 2030, 'test4'); NULL},
               error = function(e) conditionMessage(e))
check('4a: exhaustion stops', !is.null(err) && grepl('exhausted', err))

#-------------------------------------------------------------------------------
# 5. Aggregate identity on random ladders (1e-10 relative)
#-------------------------------------------------------------------------------

set.seed(42)
n_est = 500; n_heir = 5000
detail = bind_rows(
  tibble(id = 1:n_est, weight = runif(n_est, 1, 100), dep_status = 0,
         estate_m = runif(n_est, 0, 0.2),
         estate_p_dsue = runif(n_est),
         liab_estate_dsue = rexp(n_est, 1e-6) * rbinom(n_est, 1, 0.7),
         liab_estate_nodsue = rexp(n_est, 1e-6),
         estate_distributable = runif(n_est, 1e6, 1e8)),
  tibble(id = 10000 + 1:n_heir, weight = runif(n_heir, 1, 500), dep_status = 0,
         estate_m = 0, estate_p_dsue = 0, liab_estate_dsue = 0,
         liab_estate_nodsue = 0, estate_distributable = 0)
)
heir_px = tibble(id = 10000 + 1:n_heir,
                 p_inheritance = runif(n_heir),
                 inheritance   = rexp(n_heir, 1e-6) * 50)
res = allocate_estate_to_heirs(detail, heir_px, 2030, 'test5')
expected = detail %>%
  filter(estate_m > 0) %>%
  summarise(t = sum(weight * estate_m *
                    (estate_p_dsue * liab_estate_dsue +
                     (1 - estate_p_dsue) * liab_estate_nodsue))) %>%
  pull(t)
heir_w = detail$weight[match(res$heirs$id, detail$id)]
heir_p = heir_px$p_inheritance[match(res$heirs$id, heir_px$id)]
allocated = sum(heir_w * heir_p * res$heirs$estate_tax_liability)
check('5a: identity vs get_estate_totals-style expectation',
      abs(allocated - expected) < 1e-10 * expected)
check('5b: diag matches', abs(res$diag$allocated_tax - expected) < 1e-10 * expected)
check('5c: rates bounded by max ladder rate',
      res$diag$max_rate <= max(c(detail$liab_estate_dsue, detail$liab_estate_nodsue) /
                               pmax(detail$estate_distributable, 1)) + 1e-9)

#-------------------------------------------------------------------------------
# 6. Determinism under input row shuffling
#-------------------------------------------------------------------------------

res2 = allocate_estate_to_heirs(detail %>% slice_sample(prop = 1),
                                heir_px %>% slice_sample(prop = 1),
                                2030, 'test6')
cmp = res$heirs %>%
  inner_join(res2$heirs, by = 'id', suffix = c('', '_shuf'))
check('6a: shuffle-invariant',
      max(abs(cmp$estate_tax_liability - cmp$estate_tax_liability_shuf)) == 0)

#-------------------------------------------------------------------------------
# 7. Zero-distributable taxed estate: dropped with warning, reported in diag
#-------------------------------------------------------------------------------

detail = bind_rows(
  detail_row(1, m = 1, T_nodsue = 5e5, n = 0),   # gift-only base
  detail_row(2, m = 1, T_nodsue = 4e6, n = 10e6),
  detail_row(101)
)
heir_px = tibble(id = 101, p_inheritance = 1, inheritance = 12e6)
warned = FALSE
res = withCallingHandlers(
  allocate_estate_to_heirs(detail, heir_px, 2030, 'test7'),
  warning = function(w) {
    warned <<- grepl('zero distributable', conditionMessage(w)); invokeRestart('muffleWarning')
  }
)
check('7a: warns on zero-distributable tax', warned)
check('7b: dropped mass in diag', abs(res$diag$dropped_zero_n_tax - 5e5) < 1e-3)
check('7c: rest still allocated', abs(res$diag$allocated_tax - 4e6) < 1e-3)

#-------------------------------------------------------------------------------
# 8. No taxed estates: zeros everywhere, no error
#-------------------------------------------------------------------------------

detail = bind_rows(detail_row(1, m = 1, T_nodsue = 0, n = 10e6), detail_row(101))
heir_px = tibble(id = 101, p_inheritance = 1, inheritance = 5e6)
res = allocate_estate_to_heirs(detail, heir_px, 2030, 'test8')
check('8a: zero liabilities', all(res$heirs$estate_tax_liability == 0))
check('8b: zero diag tax',    res$diag$tax_mass == 0)

#-------------------------------------------------------------------------------

cat(sprintf('\n%s: %d failure(s)\n', if (fails == 0) 'ALL TESTS PASSED' else 'TESTS FAILED', fails))
quit(status = as.integer(fails > 0))
