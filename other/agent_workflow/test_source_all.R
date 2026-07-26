#-------------------------------------------------------------------------------
# test_source_all.R
#
# Sources every file under src/ exactly the way main.R does. Catches syntax
# errors and any source-time reference to a constant that the assumptions
# migration removed. Then greps the tree for references to retired names.
#-------------------------------------------------------------------------------

setwd('/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator')

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = T)
  ))
)

cat('== sourcing src/ ==\n')
return_vars = list()
list.files('./src', recursive = T) %>%
  walk(.f = ~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') &&
                  !startsWith(.x, 'tests/')) {
    source(file.path('./src/', .x))
  })
cat('  PASS  all src files sourced\n')

fails = 0
check = function(label, ok) {
  cat(if (isTRUE(ok)) '  PASS  ' else '  FAIL  ', label, '\n', sep = '')
  if (!isTRUE(ok)) fails <<- fails + 1
}

cat('\n== retired names are gone ==\n')
retired = c('SIGMA_CONV', 'SIGMA_PT_LABOR_SHARE',
            'CORP_ASSET_EXPOSURE', 'CORP_OMEGA_DIV', 'CORP_OMEGA_KG',
            'CORP_SIGMA_N_DEFAULT', 'CORP_KAPPA_DEFAULT', 'CORP_THETA',
            'CORP_THETA_RES', 'CORP_DELTA_NIPA', 'CORP_EQUITY_PREMIUM',
            'CORP_RATE_ETI', 'WEALTH_CAP_FLOWS_PT_WEIGHT',
            'DIST_CORP_FOREIGN_SHARE', 'DIST_HOUSING_STRUCTURE_SHARE_DEFAULT',
            'KG_DYN_DEFAULT_ETA', 'KG_DYN_DEFAULT_ETA_LOGS',
            'KG_DYN_TIMEABLE_SHARE', 'KG_DYN_TIMEABLE_SHARE_LOGS',
            'KG_DYN_RESPONSE_FORM', 'KG_DYN_APPLIER_ALLOCATION',
            'KG_DYN_DEEMED_AVOIDANCE', 'KG_DYN_BETA',
            'KG_DYN_TIMING_WINDOW', 'KG_DYN_TIMING_REF_WEDGE',
            'KG_DYN_DG_ALLOCATION', 'KG_DYN_CHAR_EXTENSIVE_INTERCEPT',
            'KG_DYN_CHAR_EXTENSIVE_LN_SLOPE', 'KG_DYN_CHAR_INTENSIVE_INTERCEPT',
            'KG_DYN_CHAR_INTENSIVE_LN_SLOPE', 'KG_DYN_CHAR_BASE_YEAR',
            'KG_DYN_CALIB_PROVENANCE', 'kg_dyn_check_calibration_provenance')
for (nm in retired) {
  check(paste0(nm, ' no longer defined'), !exists(nm))
}

cat('\n== new accessors are defined ==\n')
for (nm in c('assumption', 'assumptions_load_defaults', 'assumptions_resolve',
             'assumptions_activate', 'assumptions_check_staleness',
             'assumptions_manifest', 'kg_dyn_response_form',
             'kg_dyn_active_eta', 'kg_dyn_active_timeable_share')) {
  check(paste0(nm, '() defined'), exists(nm) && is.function(get(nm)))
}

cat('\n== every assumption named in code exists in config ==\n')
defaults = assumptions_load_defaults()
src_text = list.files('./src', recursive = T, full.names = TRUE,
                      pattern = '[.]R$') %>%
  map(readLines, warn = FALSE) %>%
  unlist()
refs = str_match_all(paste(src_text, collapse = '\n'),
                     "assumption\\(\\s*'([a-z_]+)'\\s*,\\s*'([a-z0-9_]+)'\\s*\\)")[[1]]
if (nrow(refs) == 0) {
  check('found assumption() call sites to check', FALSE)
} else {
  keys = unique(paste(refs[, 2], refs[, 3], sep = '.'))
  cat('  (', length(keys), ' distinct assumption() references)\n', sep = '')
  for (k in keys) {
    parts = str_split_1(k, '[.]')
    ok = parts[1] %in% names(defaults) && parts[2] %in% names(defaults[[parts[1]]])
    check(paste0(k, ' resolves'), ok)
  }
}

cat('\n== live values match the pre-migration constants ==\n')
assumptions_activate(assumptions_resolve(defaults, list(ID = 'baseline',
                                                        assumptions = NA)))
check('kg.response_form = logs',      identical(kg_dyn_response_form(), 'logs'))
check('active eta = 1.6625 (logs)',   isTRUE(all.equal(kg_dyn_active_eta(), 1.6625)))
check('levels eta = 2.4825',          isTRUE(all.equal(kg_dyn_active_eta('levels'), 2.4825)))
check('active timeable = 0.2542',     isTRUE(all.equal(kg_dyn_active_timeable_share(), 0.2542)))
check('kg.applier_allocation = 0.5',
      identical(as.character(assumption('kg', 'applier_allocation')), '0.5'))
check('kg.dg_allocation = G',         identical(assumption('kg', 'dg_allocation'), 'G'))
check('kg.deemed_avoidance = 0.25',   isTRUE(all.equal(assumption('kg', 'deemed_avoidance'), 0.25)))
check('kg.timing_window = 1',         isTRUE(all.equal(assumption('kg', 'timing_window'), 1)))
check('kg.timing_ref_wedge = 0.05',   isTRUE(all.equal(assumption('kg', 'timing_ref_wedge'), 0.05)))
check('kg.beta_fallback = 0.978',     isTRUE(all.equal(assumption('kg', 'beta_fallback'), 0.978)))
check('kg.wealth_carry_scale = 1',    isTRUE(all.equal(assumption('kg', 'wealth_carry_scale'), 1)))
check('kg.char_base_year = 2026',     isTRUE(all.equal(assumption('kg', 'char_base_year'), 2026)))
check('sigma.conv = 0.16',            isTRUE(all.equal(assumption('sigma', 'conv'), 0.16)))
check('sigma.pt_labor_share = 0.75',  isTRUE(all.equal(assumption('sigma', 'pt_labor_share'), 0.75)))
check('corp.sigma_n = 0.375',         isTRUE(all.equal(assumption('corp', 'sigma_n'), 0.375)))
check('corp.kappa = 0.40',            isTRUE(all.equal(assumption('corp', 'kappa'), 0.40)))
check('corp.theta = 1.0',             isTRUE(all.equal(assumption('corp', 'theta'), 1.0)))
check('corp.theta_res = 0.40',        isTRUE(all.equal(assumption('corp', 'theta_res'), 0.40)))
check('corp.omega_div = 0.85',        isTRUE(all.equal(assumption('corp', 'omega_div'), 0.85)))
check('corp.omega_kg = 0.50',         isTRUE(all.equal(assumption('corp', 'omega_kg'), 0.50)))
check('corp.delta_nipa = 0.057',      isTRUE(all.equal(assumption('corp', 'delta_nipa'), 0.057)))
check('corp.equity_premium = 0.05',   isTRUE(all.equal(assumption('corp', 'equity_premium'), 0.05)))
check('corp.rate_eti = 0.367',        isTRUE(all.equal(assumption('corp', 'rate_eti'), 0.367)))
check('corp.priced_as_permanent FALSE',
      identical(isTRUE(as.logical(assumption('corp', 'priced_as_permanent'))), FALSE))
check('corp asset exposure vector matches the old constant',
      isTRUE(all.equal(corp_asset_exposure(),
                       c('value.equities' = 1.00, 'value.dc' = 0.55,
                         'value.trusts' = 0.50, 'value.re_fund' = 0.30))))
check('wealth.avoid_public_e = -7',   isTRUE(all.equal(assumption('wealth', 'avoid_public_e'), -7)))
check('wealth.avoid_private_e = -17', isTRUE(all.equal(assumption('wealth', 'avoid_private_e'), -17)))
check('wealth.chi_pub = 1.0',         isTRUE(all.equal(assumption('wealth', 'chi_pub'), 1.0)))
check('wealth.chi_priv = 0.5',        isTRUE(all.equal(assumption('wealth', 'chi_priv'), 0.5)))
check('wealth.cap_flows_pt_weight = 0.2',
      isTRUE(all.equal(wealth_cap_flows_pt_weight(), 0.2)))
check('evasion.e_schc = 0.046',       isTRUE(all.equal(assumption('evasion', 'e_schc'), 0.046)))
check('evasion.e_pt = 0.052',         isTRUE(all.equal(assumption('evasion', 'e_pt'), 0.052)))
check('evasion.e_rent = 0.040',       isTRUE(all.equal(assumption('evasion', 'e_rent'), 0.040)))
check('evasion.topend_mult = 1',      isTRUE(all.equal(assumption('evasion', 'topend_mult'), 1)))
check('estate.report_eps = 0.16',     isTRUE(all.equal(assumption('estate', 'report_eps'), 0.16)))
check('distribution.corp_foreign_share = 0.40',
      isTRUE(all.equal(assumption('distribution', 'corp_foreign_share'), 0.40)))
check('distribution.housing_structure_share = 0.70',
      isTRUE(all.equal(dist_housing_structure_share(), 0.70)))

cat('\n== staleness state under the shipped defaults ==\n')
iv = read_yaml('./config/interfaces/interface_versions.yaml')
live_vintages = iv %>%
  keep(~ !is.null(.x$default_vintage)) %>%
  map_chr(~ as.character(.x$default_vintage)) %>%
  set_names(names(.) %>% str_to_lower() %>% str_replace_all('-', '_'))
cat('  live Tax-Data =', live_vintages[['tax_data']],
    '/ Macro =', live_vintages[['macro_projections']], '\n')

resolved_default = assumptions_resolve(defaults, list(ID = 'baseline',
                                                      assumptions = NA))
state = suppressWarnings(assumptions_check_staleness(
  defaults, resolved_default, live_vintages, enforce = FALSE))
if (length(state) == 0) {
  cat('  PASS  no calibrated assumption reads stale\n')
} else {
  cat('  NOTE  ', length(state), ' calibrated assumption(s) read stale:\n', sep = '')
  for (f in state) cat('        - ', f, '\n', sep = '')
}
check('inactive-form kg pairs are exempt (response_form = logs)',
      !any(grepl('kg.eta:', state, fixed = TRUE)) &&
      !any(grepl('kg.timeable_share:', state, fixed = TRUE)))
check('live-form kg pairs are clean',
      !any(grepl('kg.eta_logs', state, fixed = TRUE)) &&
      !any(grepl('kg.timeable_share_logs', state, fixed = TRUE)))
check('enforcement is ON', isTRUE(ASSUMPTIONS_ENFORCE_STALENESS))

cat('\n== timing param validation still works ==\n')
check('validates with the active set',
      isTRUE(try(kg_dyn_validate_timing_params(), silent = TRUE)))

cat('\n', strrep('-', 50), '\n', sep = '')
if (fails == 0) {
  cat('ALL CHECKS PASSED\n')
} else {
  cat(fails, ' CHECK(S) FAILED\n', sep = '')
  quit(status = 1)
}
