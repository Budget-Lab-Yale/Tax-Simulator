#-------------------------------------------------------------------------------
# validate_config.R
#
# Pre-launch wiring check for the clausing_v2 re-run (wealth bathtub s-grid +
# on-model corporate incidence), without running any simulation:
#   1. all three runscripts (clausing_v2 / _s25 / _s75) parse,
#   2. corp gate: ON for 08_corporate only (new OME vintage 20260706 carries
#      corporate_meta.yaml), OFF for baseline/01-07 (zero wedge),
#   3. corp paths for 08_corporate: t0 = 2030, permanent shape (mu > 0 and
#      declining -- NO sunset signature from the old trailing-zero file),
#      wedge declared 2030-2041 and nonzero throughout,
#   4. wealth gate: ON for every reform row at the runscript's s, OFF for
#      baseline; resolved profile is flat-s / identity-M.
#
# Run via sbatch (never on the login node):
#   sbatch other/clausing_v2/validate_config.sbatch
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
message('OK: src tree sourced')

reform_ids = c('01_clinton_rates', '02_restore_bottom_rates', '03_199a',
               '04_carryover_basis', '05_pref_rates', '06_niit_reform',
               '07_estate', '08_corporate')

for (spec in list(list(rs = 'clausing_v2',     s = 0.50),
                  list(rs = 'clausing_v2_s25', s = 0.25),
                  list(rs = 'clausing_v2_s75', s = 0.75))) {

  globals <<- parse_globals(
    runscript_name   = spec$rs,
    scenario_id      = NULL,
    local            = 1,
    vintage          = paste0(spec$rs, '_check'),
    baseline_vintage = NULL,
    pct_sample       = 1,
    multicore        = 'none'
  )
  cat('\n================ runscript:', spec$rs, '================\n')

  # --- wealth gate: baseline dormant, every reform row flat-s at spec$s ------
  stopifnot(!scenario_uses_wealth_dynamics(get_scenario_info('baseline')))
  for (id in reform_ids) {
    prof = wealth_dyn_resolve_profile(get_scenario_info(id))
    stopifnot(isTRUE(prof$active),
              abs(max(prof$s_mat) - spec$s) < 1e-12,
              abs(min(prof$s_mat) - spec$s) < 1e-12)
  }
  cat('wealth gate: baseline OFF; 01-08 ON at flat s =', spec$s, '\n')

  # --- corp gate: 08 only ----------------------------------------------------
  for (id in c('baseline', reform_ids)) {
    got = withCallingHandlers(
      scenario_uses_corp_incidence(get_scenario_info(id)),
      warning = function(w) {
        message('  [warning] ', conditionMessage(w))
        invokeRestart('muffleWarning')
      })
    stopifnot(identical(got, id == '08_corporate'))
  }
  cat('corp gate: 08_corporate ON; baseline/01-07 OFF\n')
}

# --- corp path properties on the central runscript ---------------------------
globals <<- parse_globals(
  runscript_name   = 'clausing_v2',
  scenario_id      = NULL,
  local            = 1,
  vintage          = 'clausing_v2_check',
  baseline_vintage = NULL,
  pct_sample       = 1,
  multicore        = 'none'
)
si = get_scenario_info('08_corporate')

wedge = corp_read_wedge(si)
live  = wedge %>% filter(abs(w) > 1e-9)
stopifnot(min(live$year) == 2030, max(live$year) == 2041,
          nrow(live) == 12, all(diff(live$w) > 0))
cat('\nwedge: nonzero 2030-2041 only, monotone increasing',
    sprintf('(2030 = %.1f, 2041 = %.1f)\n', live$w[1], live$w[nrow(live)]))

p = corp_get_paths(si)
stopifnot(p$t0 == 2030)
sim_live = p$sim %>% filter(year >= 2030)
stopifnot(all(sim_live$mu > 0),            # permanent: markdown never expires
          all(diff(sim_live$mu) < 1e-9))   # and declines toward the floor
cat(sprintf('paths: t0 = %d, r = %.4f, g_tail = %.4f; mu positive/declining over window\n',
            p$t0, p$r, p$g_tail))
print(as.data.frame(p$sim %>%
  select(year, w, phi, mu, eta, fac_div, fac_int, fac_rent, fac_pt) %>%
  mutate(across(-year, ~ round(., 5)))))

message('CLAUSING_V2 CONFIG CHECKS PASSED')
