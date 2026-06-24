#-------------------------------------------------------------------------------
# test_wealth_bathtub.R
#
# Unit tests for the wealth bathtub's pure functions (no Tax-Data, no full sim).
# Doubles as a syntax check: it sources every src/**/*.R the way setup.R /
# reconstitute_environment do, so a parse error in any touched file fails here.
#
# RUN VIA SBATCH (never on the login node):
#   sbatch --partition=day -c 1 --time=0:15:00 --mem=8G \
#     --wrap="cd <repo> && module load R/4.4.1-foss-2022b && \
#             Rscript other/wealth_dynamics/test_wealth_bathtub.R"
#-------------------------------------------------------------------------------

# --- Load packages + source all function scripts (mirrors setup.R) ------------
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)
return_vars = list()
invisible(lapply(list.files('./src', recursive = TRUE), function(f) {
  if (f != 'main.R' && !startsWith(f, 'slurm/') && !startsWith(f, 'tests/')) {
    source(file.path('./src', f))
  }
}))
cat('Sourced all src/**/*.R cleanly (syntax OK).\n\n')

# --- Tiny test harness --------------------------------------------------------
.n_pass = 0L; .n_fail = 0L
check = function(name, cond) {
  ok = isTRUE(cond)
  if (ok) { .n_pass <<- .n_pass + 1L; cat(sprintf('  PASS  %s\n', name)) }
  else    { .n_fail <<- .n_fail + 1L; cat(sprintf('  FAIL  %s\n', name)) }
}
approx = function(a, b, tol = 1e-8) all(abs(a - b) <= tol)

cat('== cohort primitives ==\n')

# build_aging_matrix: A[i,i+1]=1, top self-loops, each row sums to 1.
A = build_aging_matrix(1:4)
check('aging: shape A[i,i+1]=1', A[1,2]==1 && A[2,3]==1 && A[3,4]==1)
check('aging: top self-loop',    A[4,4]==1)
check('aging: each row one dest', all(rowSums(A) == 1))

# sinkhorn_rake: identity stays identity; a positive matrix becomes DS.
I3 = diag(3)
check('sinkhorn: identity -> identity', approx(sinkhorn_rake(I3), I3))
U3 = matrix(1/3, 3, 3)
check('sinkhorn: uniform stays DS', approx(rowSums(sinkhorn_rake(U3)), 1) &&
                                    approx(colSums(sinkhorn_rake(U3)), 1))
Mraw = matrix(c(2,1,1, 1,3,1, 1,1,4), 3, 3, byrow = TRUE)
Mds  = sinkhorn_rake(Mraw)
check('sinkhorn: arbitrary -> rows sum 1', approx(rowSums(Mds), 1, 1e-7))
check('sinkhorn: arbitrary -> cols sum 1', approx(colSums(Mds), 1, 1e-7))

# apply_percentile_transition: identity no-op; uniform row-averages each age.
P = matrix(c(10, 0, 0, 20), 2, 2, byrow = TRUE)  # 2 ages x 2 bins
check('M-apply: identity is no-op', approx(apply_percentile_transition(P, diag(2)), P))
Pu = apply_percentile_transition(P, matrix(1/2, 2, 2))
check('M-apply: uniform splits within age', approx(Pu, matrix(c(5,5,10,10), 2, 2, byrow = TRUE)))
check('M-apply: uniform preserves per-age sum', approx(rowSums(Pu), rowSums(P)))

cat('\n== recurrence (toy cells) ==\n')

# Toy grid: 3 ages x 2 bins, identity M, uniform growth g.
ages = 1:3; A3 = build_aging_matrix(ages); I2 = diag(2)
zero = matrix(0, 3, 2); g = 1.10; G = matrix(g, 3, 2); s = 0.5

# D24 timing: one-time inflow s*dT0 in cell (age 1, bin 1).
dT0 = 8; inflow1 = matrix(0, 3, 2); inflow1[1,1] = s * dT0
P1 = cohort_recurrence_step(zero, G, inflow1, A3, I2)
check('timing: arrival year P = s*dT0 (face value)', approx(P1[1,1], s*dT0))
P2 = cohort_recurrence_step(P1, G, matrix(0,3,2), A3, I2)
check('timing: next year aged cell = G*s*dT0', approx(P2[2,1], g*s*dT0))
check('timing: arrival cell empties after aging', approx(P2[1,1], 0))

# Targeted localization under M=I: top-bin-only inflow stays in the top bin.
inflowT = matrix(0,3,2); inflowT[1,2] = 7
Pt1 = cohort_recurrence_step(zero, G, inflowT, A3, I2)
Pt2 = cohort_recurrence_step(Pt1, G, matrix(0,3,2), A3, I2)
check('localization: bin 1 never touched under M=I', approx(Pt2[,1], c(0,0,0)))
check('localization: deficit rides up in bin 2', approx(Pt2[2,2], g*7))

# Mass conservation: with g=1 and inflow 0, total mass is preserved across the
# aging+M step (M row-stochastic; each age feeds exactly one destination).
Pseed = matrix(c(3,1, 2,5, 0,4), 3, 2, byrow = TRUE)
Gc = matrix(1, 3, 2)
Pcons_I = cohort_recurrence_step(Pseed, Gc, matrix(0,3,2), A3, I2)
Pcons_U = cohort_recurrence_step(Pseed, Gc, matrix(0,3,2), A3, matrix(1/2,2,2))
check('mass: conserved under M=I (g=1)',      approx(sum(Pcons_I), sum(Pseed)))
check('mass: conserved under uniform M (g=1)', approx(sum(Pcons_U), sum(Pseed)))
check('mass: total invariant to M choice',     approx(sum(Pcons_I), sum(Pcons_U)))
# With uniform growth g, total scales by g.
Pgrow = cohort_recurrence_step(Pseed, matrix(1.07,3,2), matrix(0,3,2), A3, I2)
check('mass: uniform growth scales total by g', approx(sum(Pgrow), 1.07*sum(Pseed)))

# Linearity / mixed-sign superposition (fixed g, A, M).
Pa = matrix(c(1,0,0,2,0,0),3,2,byrow=TRUE); Pb = matrix(c(0,3,0,0,0,1),3,2,byrow=TRUE)
Ia = matrix(0,3,2); Ia[1,1] = 4; Ib = matrix(0,3,2); Ib[2,2] = -4  # sign-flipping
lhs = cohort_recurrence_step(Pa + Pb, G, Ia + Ib, A3, I2)
rhs = cohort_recurrence_step(Pa, G, Ia, A3, I2) +
      cohort_recurrence_step(Pb, G, Ib, A3, I2)
check('superposition: step is linear (mixed sign)', approx(lhs, rhs))

cat('\n== cells & helpers ==\n')

# Within-age percentile cutoffs/bins: 100 records (NW 1..100) in one age ->
# 4 equal-headcount bins of ~25; zero/negative NW -> no cell.
nw  = c(1:100, 0, -5); wt = rep(1, 102); ac = rep(50L, 102)
cuts = compute_within_age_cutoffs(nw, wt, ac, ages = 50, n_bins = 4)
bins = assign_within_age_bin(nw, ac, cuts, n_bins = 4)
check('cells: positive NW all assigned', !any(is.na(bins[1:100])))
check('cells: zero & negative NW -> NA', is.na(bins[101]) && is.na(bins[102]))
check('cells: bins in [1,4]', all(bins[1:100] >= 1 & bins[1:100] <= 4))
tab = as.integer(table(factor(bins[1:100], levels = 1:4)))
check('cells: ~equal headcount per bin', all(abs(tab - 25) <= 1))

# Cohort key: joint -> max(age1,age2) pre-topcode; single -> age1; topcode 80.
tu = tibble(filing_status = c(2L, 1L, 2L, 1L),
            age1 = c(40, 55, 30, 95),
            age2 = c(70, NA, 88, NA))
ck = wealth_dyn_age_cohort(tu)
check('cohort key: joint uses max(age1,age2)', ck[1] == 70)
check('cohort key: single uses age1',          ck[2] == 55)
check('cohort key: joint max then 80 topcode',  ck[3] == 80)
check('cohort key: single age1 80 topcode',     ck[4] == 80)

# Capital total F = pure + 0.2 * pass-through-net (signed loss pairs).
df = tibble(txbl_int = 100, exempt_int = 0, div_ord = 50, div_pref = 0,
            kg_st = 0, kg_lt = 200, kg_1250 = 0, kg_collect = 0,
            rent = 30, rent_loss = 10, estate = 0, estate_loss = 0,
            sole_prop = 0, part_active = 500, part_passive = 0,
            part_active_loss = 0, part_passive_loss = 0, part_179 = 0,
            scorp_active = 0, scorp_passive = 0, scorp_active_loss = 0,
            scorp_passive_loss = 0, scorp_179 = 0, farm = 0)
Fexp = 100 + 50 + 200 + (30 - 10) + 0.2 * 500   # = 470
check('capital total: pure + 0.2*pt_net', approx(wealth_dyn_capital_total(df), Fexp))

# economic_gross = sum of value.* asset cols, NA-safe.
ag = tibble(value.cash = 100, value.equities = NA, value.bonds = 50,
            value.dc = 0, value.db = 0, value.life_ins = 0, value.annuities = 0,
            value.trusts = 0, value.other_fin = 0, value.pass_throughs = 25,
            value.primary_home = 0, value.other_home = 0, value.re_fund = 0,
            value.other_nonfin = 0)
check('economic_gross: NA-safe sum of value.*', approx(wealth_dyn_economic_gross(ag), 175))

cat('\n== r_total splice (historical + projections) ==\n')

# wealth_dyn_read_rtotal must splice historical.csv ahead of projections.csv so
# a pre-projection lead-in year and the projection-boundary year difference off
# a REAL prior-year level (regression: projections.csv-only left those NA and
# crashed the pre-pass). Synthetic macro dir spanning the boundary at 2026.
.macro_dir = file.path(tempdir(), 'macro_splice_test')
dir.create(.macro_dir, showWarnings = FALSE, recursive = TRUE)
# gdp/pop chosen so the 2025->2026 boundary growth differs from 2026->2027:
# a backfill-from-projections would (wrongly) set r(2026) == r(2027).
write.csv(data.frame(year = 2023:2025, gdp = c(100, 104, 110),
                     unmarried_40 = c(60, 60.5, 61), married_40 = c(40, 40.5, 41)),
          file.path(.macro_dir, 'historical.csv'), row.names = FALSE)
write.csv(data.frame(year = 2026:2028, gdp = c(114, 119, 124),
                     unmarried_40 = c(61.5, 62, 62.5), married_40 = c(41.5, 42, 42.5)),
          file.path(.macro_dir, 'projections.csv'), row.names = FALSE)
.si = list(interface_paths = list(`Macro-Projections` = .macro_dir),
           years = 2024:2027)
.r = wealth_dyn_read_rtotal(.si, list(r_total = list(additive_delta = 0)))
check('r_total: lead-in + boundary years all finite', all(is.finite(.r)) && !anyNA(.r))
# Boundary year differences off the historical 2025 level (pop_2025 = 102):
.r2026_exp = (114 / 110) / ((61.5 + 41.5) / (61 + 41)) - 1
check('r_total: boundary year uses real prior (historical) level',
      approx(unname(.r['2026']), .r2026_exp))
# Pre-projection lead-in year (fully inside historical):
.r2024_exp = (104 / 100) / ((60.5 + 40.5) / (60 + 40)) - 1
check('r_total: pre-projection lead-in resolves', approx(unname(.r['2024']), .r2024_exp))
unlink(.macro_dir, recursive = TRUE)

# --- Summary ------------------------------------------------------------------
cat(sprintf('\n==== %d passed, %d failed ====\n', .n_pass, .n_fail))
if (.n_fail > 0) quit(status = 1)
cat('ALL WEALTH-BATHTUB UNIT TESTS PASSED\n')
