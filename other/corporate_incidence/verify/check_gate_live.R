#-------------------------------------------------------------------------------
# check_gate_live.R
#
# Live-wiring sanity for the corp channel against the REAL test inputs
# (config/runscripts/tests/corp_incidence.csv + the corp_test_20260703 OME
# vintage), without running any simulation: parse globals, resolve each
# scenario's gate decision, and build + print the paths for the active ones.
# Catches interface-path, metadata, and macro-column wiring problems in
# minutes. Run via sbatch.
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

# main.R assigns the result to the global `globals`; mirror that exactly
globals = parse_globals(
  runscript_name   = 'tests/corp_incidence',
  scenario_id      = NULL,
  local            = 1,
  vintage          = 'corp_gate_check',
  baseline_vintage = NULL,
  pct_sample       = 1,
  multicore        = 'none'
)

expect_gate = c(baseline            = FALSE,
                corp_perm           = TRUE,
                corp_sunset         = TRUE,
                corp_nometa         = FALSE,   # no metadata -> OFF (+warning)
                corp_perm_wealth    = TRUE,
                corp_sunset_wealth  = TRUE,
                corp_perm_kg        = TRUE,
                corp_perm_kg_wealth = TRUE)

for (id in names(expect_gate)) {
  si  = get_scenario_info(id)
  got = withCallingHandlers(
    scenario_uses_corp_incidence(si),
    warning = function(w) {
      message('  [expected warning] ', conditionMessage(w))
      invokeRestart('muffleWarning')
    })
  cat(sprintf('gate %-20s = %s (expected %s)\n', id, got, expect_gate[[id]]))
  stopifnot(identical(got, unname(expect_gate[[id]])))
}

for (id in c('corp_perm', 'corp_sunset')) {
  si = get_scenario_info(id)
  p  = corp_get_paths(si)
  cat('\n=== paths:', id, '(r =', round(p$r, 4), ', t0 =', p$t0,
      ', g_tail =', round(p$g_tail, 4), ')\n')
  print(as.data.frame(p$sim %>%
    select(year, w, phi, mu, eta, roll, fac_div, fac_int, fac_rent, fac_pt,
           mu_ret) %>%
    mutate(across(-year, ~ round(., 5)))))
}

# Sunset property on the real inputs: markdown gone at/after expiry (2031)
ps = corp_get_paths(get_scenario_info('corp_sunset'))
stopifnot(all(abs(ps$sim$mu[ps$sim$year >= 2031]) < 1e-10))
# Permanent property: mu positive and declining toward the rent-share floor
pp = corp_get_paths(get_scenario_info('corp_perm'))
live = pp$sim %>% filter(year >= pp$t0)
stopifnot(all(live$mu > 0), all(diff(live$mu) < 1e-9))
message('LIVE GATE + PATH CHECKS PASSED')
