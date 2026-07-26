#-------------------------------------------------------------------------------
# equivalence_check.R
#
# Phase 2 gate: for every live runscript, resolve each scenario the OLD way
# (parse_globals / get_scenario_info / assumptions_resolve) and the NEW way
# (migrate_runscripts.py translation -> scenario_config engine), and assert
# value-for-value equality of everything the redesign moves:
#
#   - interface vintages/IDs (dep.* columns -> an economy set's interfaces.yaml)
#   - excess growth settings -> economy.growth entries
#   - the wealth financing spec (s / wealth_financing -> financing_profile)
#   - every config/assumptions value -> its re-homed economy/behavior entry
#   - the behavior module list -> the named set's active responses (+ the
#     magnitudes the old filenames encoded)
#
# The new side reads the set folders the migrator writes -- there are no
# per-value override columns to collect.
#
# Run via sbatch (needs R + the Tax-Data interface dirs on /nfs):
#   sbatch other/config_redesign/run_tests.sbatch . other/config_redesign/equivalence_check.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)
return_vars = list()
list.files('./src', recursive = TRUE) %>%
  walk(.f = ~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/'))
    source(file.path('./src/', .x)))

#---------------------------------
# Old-module -> new-value contracts
#---------------------------------

MOD2RESP = c(
  'kg_dynamics/turnover'                    = 'kg_dynamics',
  'conversion/sigma'                        = 'conversion',
  'entity_shifting/pearce_prisinzano'       = 'entity_shifting',
  'entity_shifting/pearce_prisinzano_legacy'= 'entity_shifting',
  'evasion/debacker'                        = 'evasion',
  'wealth/avoidance'                        = 'wealth_avoidance',
  'estate/avoidance'                        = 'estate_avoidance',
  'employment/bastian'                      = 'employment',
  'child_earnings/34'                       = 'child_earnings',
  'tips/fringe_low'                         = 'tips',
  'ot/france'                               = 'ot',
  'ot/france_1yr'                           = 'ot',
  'ot/france_full'                          = 'ot',
  'auto/hanson'                             = 'auto',
  'charity/100'                             = 'charity',
  'charity/50'                              = 'charity',
  'kg/62'                                   = 'kg_static',
  'kg/50'                                   = 'kg_static',
  'kg/70'                                   = 'kg_static',
  'kg/72'                                   = 'kg_static',
  'kg/22'                                   = 'kg_static',
  'kg/50_w_transitory_2025'                 = 'kg_static'
)

# The magnitude each old filename encoded: module -> list(channel.name = value).
MOD_PARAMS = list(
  'charity/100'   = list('charity.e' = -1.0),
  'charity/50'    = list('charity.e' = -0.5),
  'kg/62'         = list('kg_static.e' = -0.62, 'kg_static.max_adj' = 3),
  'kg/50'         = list('kg_static.e' = -0.50, 'kg_static.max_adj' = 1),
  'kg/70'         = list('kg_static.e' = -0.70, 'kg_static.max_adj' = 1),
  'kg/72'         = list('kg_static.e' = -0.72, 'kg_static.max_adj' = 3),
  'kg/22'         = list('kg_static.e' = -0.22, 'kg_static.max_adj' = 1),
  'ot/france'     = list('ot.phase_in_years' = 3),
  'ot/france_1yr' = list('ot.phase_in_years' = 1),
  'ot/france_full'= list('ot.phase_in_years' = 0),
  'entity_shifting/pearce_prisinzano_legacy' = list('entity_shifting.legacy_pricing' = TRUE),
  'entity_shifting/pearce_prisinzano'        = list('entity_shifting.legacy_pricing' = FALSE)
)

# Where each old assumption channel.name now lives: '{leg}|{channel}|{name}'.
map_assumption_new = function(ch, nm) {
  if (ch == 'corp' && nm == 'rate_eti')            return(c('behavior', 'corp_avoidance', nm))
  if (ch %in% c('corp', 'distribution'))           return(c('economy', ch, nm))
  if (ch == 'kg' && startsWith(nm, 'char_'))       return(c('economy', 'bequest', nm))
  if (ch %in% c('kg', 'sigma', 'evasion'))         return(c('behavior', ch, nm))
  if (ch == 'estate')                              return(c('behavior', 'estate_avoidance', nm))
  if (ch == 'wealth' && nm == 'cap_flows_pt_weight') return(c('economy', 'wealth', nm))
  if (ch == 'wealth')                              return(c('behavior', 'wealth_avoidance', nm))
  stop('no leg mapping for ', ch, '.', nm)
}

#-------------
# Harness
#-------------

same = function(a, b) isTRUE(all.equal(a, b, tolerance = 0)) ||
                      identical(as.character(a), as.character(b))

live_runscripts = list.files('./config/runscripts', pattern = '[.]csv$',
                             recursive = TRUE, full.names = FALSE) %>%
  discard(.p = ~ startsWith(.x, 'archive/') || endsWith(.x, '_legend.csv')) %>%
  str_remove('[.]csv$')

eco_defaults = config_load_defaults('economy')
beh_defaults = config_load_defaults('behavior')

# One runscript = one unit of work; forked via mclapply, so each fork gets its
# own copy of `globals` and its own throwaway vintage (parse_globals writes
# manifest CSVs into the vintage dir -- a shared vintage would race).
check_runscript = function(rs) {

  problems  = c()
  n_checked = 0
  note = function(...) problems <<- c(problems, paste0(...))

  #--- old parse (skip runscripts the old system itself cannot parse)
  globals_env_ok = tryCatch({
    globals <<- parse_globals(
      runscript_name   = rs,
      scenario_id      = NULL,
      local            = 1,
      vintage          = paste0('eqchk_', gsub('/', '_', rs)),
      baseline_vintage = NULL,
      pct_sample       = 1,
      multicore        = 'none'
    )
    TRUE
  }, error = function(e) {
    message('SKIP (old parse fails) ', rs, ': ', conditionMessage(e))
    FALSE
  })
  if (!globals_env_ok) return(list(problems = character(), n = 0, skipped = rs))

  #--- new translation of a temp copy
  tmp = file.path(tempdir(), 'eqcheck', paste0(gsub('/', '_', rs), '.csv'))
  dir.create(dirname(tmp), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path('./config/runscripts', paste0(rs, '.csv')), tmp, overwrite = TRUE)
  rc = system2('python3', c('other/migrations/migrate_runscripts.py', '--write', tmp),
               stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(rc, 'status')) && attr(rc, 'status') != 0) {
    note(rs, ': migration script failed: ', paste(rc, collapse = ' | '))
    return(list(problems = problems, n = 0, skipped = NULL))
  }
  new_rs = suppressMessages(read_csv(tmp, col_types = cols(.default = 'c')))

  for (i in seq_len(nrow(globals$runscript))) {
    id  = globals$runscript$ID[i]
    si  = get_scenario_info(id)
    row = new_rs %>% filter(ID == id) %>% as.list() %>% map(.f = ~ .x[1])
    n_checked = n_checked + 1

    # The migrator emits set FOLDERS, so the new side resolves exactly the way
    # a real run would: default set overlaid by the named set, nothing else.
    eco = config_resolve('economy', eco_defaults, row$economy)
    beh = config_resolve('behavior', beh_defaults, row$behavior)

    #--- 1. interface vintages/IDs (old scenario_info carries a NAMED LIST,
    #        interface name -> path)
    for (iface in names(si$interface_paths)) {
      key      = tolower(gsub('[ -]', '_', iface))
      old_path = si$interface_paths[[iface]]
      new_v  = eco$values$interfaces[[paste0(key, '_vintage')]]
      new_id = eco$values$interfaces[[paste0(key, '_id')]]
      if (is.null(new_v)) { note(rs, '/', id, ': no economy entry for interface ', iface); next }
      if (!grepl(paste0('/', new_v, '/', new_id, '$'), old_path)) {
        note(rs, '/', id, ': interface ', iface, ' old path ', old_path,
             ' vs new ', new_v, '/', new_id)
      }
    }

    #--- 2. excess growth
    for (p in c('excess_growth', 'excess_growth_start_year', 'excess_growth_all_rev')) {
      if (!same(si[[p]], eco$values$growth[[p]])) {
        note(rs, '/', id, ': ', p, ' old ', si[[p]], ' vs new ', eco$values$growth[[p]])
      }
    }

    #--- 3. wealth financing spec
    old_spec = wealth_dyn_profile_spec(si)
    fp = as.character(eco$values$wealth$financing_profile)
    new_spec = if (tolower(fp) %in% c('none', 'off')) list(kind = 'off', value = NA)
               else if (startsWith(fp, 'flat:')) list(kind = 'scalar',
                                                      value = as.numeric(sub('^flat:', '', fp)))
               else list(kind = 'folder', value = fp)
    if (!identical(old_spec$kind, new_spec$kind) ||
        !same(old_spec$value, new_spec$value)) {
      note(rs, '/', id, ': financing spec old (', old_spec$kind, ',', old_spec$value,
           ') vs new (', new_spec$kind, ',', new_spec$value, ')')
    }

    #--- 4. every old assumption value -> its re-homed entry
    for (ch in names(si$assumptions$values)) {
      for (nm in names(si$assumptions$values[[ch]])) {
        dst = map_assumption_new(ch, nm)
        new_vals = if (dst[1] == 'economy') eco$values else beh$values
        new_v = new_vals[[dst[2]]][[dst[3]]]
        if (is.null(new_v)) {
          note(rs, '/', id, ': ', ch, '.', nm, ' has no new home at ',
               paste(dst, collapse = '.'))
        } else if (!same(si$assumptions$values[[ch]][[nm]], new_v)) {
          note(rs, '/', id, ': ', ch, '.', nm, ' old ',
               paste(as.character(si$assumptions$values[[ch]][[nm]]), collapse = ' '),
               ' vs new ', paste(as.character(new_v), collapse = ' '))
        }
      }
    }

    #--- 5. behavior modules -> set membership + encoded magnitudes
    mods = si$behavior_modules %||% character()
    old_resps = unname(MOD2RESP[mods])
    if (any(is.na(old_resps))) {
      note(rs, '/', id, ': unknown old module(s) ', paste(mods[is.na(old_resps)], collapse = ' '))
    } else if (!setequal(old_resps, beh$active %||% character())) {
      note(rs, '/', id, ': responses old {', paste(sort(old_resps), collapse = ' '),
           '} vs new {', paste(sort(beh$active %||% character()), collapse = ' '), '}')
    }
    for (mod in mods) {
      for (key in names(MOD_PARAMS[[mod]] %||% list())) {
        parts = str_split_1(key, '[.]')
        new_v = beh$values[[parts[1]]][[parts[2]]]
        if (!same(MOD_PARAMS[[mod]][[key]], new_v)) {
          note(rs, '/', id, ': ', mod, ' encoded ', key, ' = ',
               MOD_PARAMS[[mod]][[key]], ' but new resolves ', new_v)
        }
      }
    }
  }

  list(problems = problems, n = n_checked, skipped = NULL)
}

#-------------
# Driver (forked across runscripts)
#-------------

n_cores = max(1L, as.integer(Sys.getenv('SLURM_CPUS_PER_TASK', '4')))
results = mclapply(live_runscripts, function(rs) {
  tryCatch(check_runscript(rs),
           error = function(e) list(problems = paste0(rs, ': checker error: ',
                                                      conditionMessage(e)),
                                    n = 0, skipped = NULL))
}, mc.cores = n_cores)

problems  = results %>% map('problems') %>% unlist() %>% discard(is.null)
n_checked = results %>% map_dbl('n') %>% sum()
skipped   = results %>% map('skipped') %>% unlist() %>% discard(is.null)

if (length(skipped) > 0) {
  cat('\nskipped (old parse fails, already unrunnable):\n  - ',
      paste(skipped, collapse = '\n  - '), '\n', sep = '')
}
cat('\nchecked', n_checked, 'scenario resolutions across', length(live_runscripts),
    'runscripts on', n_cores, 'cores\n')
if (length(problems) > 0) {
  cat('\nEQUIVALENCE FAILURES:\n  - ', paste(problems, collapse = '\n  - '), '\n', sep = '')
  stop('equivalence check FAILED (', length(problems), ' findings)')
}
cat('EQUIVALENCE_CHECK_PASS\n')
