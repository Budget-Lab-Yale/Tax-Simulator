#-------------------------------------------------------------------------------
# Unit checks for the two 2026-07-25 fixes to the 2026-07-02 review doc's open
# items:
#
#   FIX 1 (do_taxes.R, calc_mtrs extensive branch): zeroing tips1/tips2/ot1/ot2
#     now also decrements the household-level `tips`/`ot` aggregate columns, so
#     the perturbed record stays internally consistent. The claim being tested
#     is that this is INERT today -- no calculator reads the bare aggregates --
#     so no existing MTR changes.
#
#   FIX 2 (tax_law.R, parse_subparam): an NA cumulative index now hard-stops
#     instead of silently reverting an indexed parameter to its raw nominal
#     base-year value via apply_indexation()'s is.na(i_index) ~ base_value.
#
# Runs on a compute node, single core, no simulation.
#-------------------------------------------------------------------------------

repo = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
setwd(repo)

# Same package set main.R loads (the tax law parser needs yaml::read_yaml)
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = T)
  ))
)

n_fail = 0
check = function(name, pass, note = '') {
  cat(sprintf('  %-66s %s%s\n', name, if (isTRUE(pass)) 'PASS' else 'FAIL',
              if (nzchar(note)) paste0('  [', note, ']') else ''))
  if (!isTRUE(pass)) n_fail <<- n_fail + 1
}


#===============================================================================
# (0) Parse-check the two edited files
#===============================================================================

cat('\n== (0) parse-check ==\n')
for (f in c('src/calc/do_taxes.R', 'src/data/tax_law.R')) {
  res = tryCatch({ parse(file = f); TRUE },
                 error = function(e) conditionMessage(e))
  check(f, isTRUE(res), if (isTRUE(res)) '' else as.character(res))
}


#===============================================================================
# (1) FIX 1 -- the tips/ot aggregates are unread by every candidate consumer
#
# The substantive claim behind "this fix changes no number today" is that no
# calculator reads the bare `tips`/`ot` columns. Two ways to establish it:
#   (1a) static: no reference to the bare aggregates anywhere under src/calc
#   (1b) behavioral: the two functions that plausibly WOULD read them -- the
#        below-the-line tip/OT deductions and payroll -- return identical
#        results whether the aggregate is stale (pre-fix perturbed frame) or
#        correctly zeroed (post-fix perturbed frame).
#===============================================================================

cat('\n== (1) FIX 1: tips/ot aggregates are unread ==\n')

# --- (1a) static scan ---------------------------------------------------------
# Bare `tips`/`ot` as a symbol: not tips1/tips2/ot1/ot2, not tips_lh, not a
# parameter name like pr.tips_exempt, not inside a string or comment.
calc_files = list.files('src/calc', pattern = '\\.R$', recursive = TRUE,
                        full.names = TRUE)
hits = calc_files %>%
  map_dfr(function(f) {
    lines = readLines(f, warn = FALSE)
    # strip comments and quoted strings before scanning
    stripped = lines %>%
      str_replace_all('#.*$', '') %>%
      str_replace_all("'[^']*'", "''") %>%
      str_replace_all('"[^"]*"', '""')
    idx = which(str_detect(stripped, '(?<![A-Za-z0-9_.])(tips|ot)(?![A-Za-z0-9_])'))
    tibble(file = f, line = idx, text = str_trim(lines[idx]))
  })

# The extensive branch of calc_mtrs is the ONE legitimate site: it is where the
# aggregates get maintained. Everything else is a consumer and must be empty.
hits_outside = hits %>% filter(!(file == 'src/calc/do_taxes.R'))
check('no bare tips/ot reference outside do_taxes.R',
      nrow(hits_outside) == 0,
      if (nrow(hits_outside) == 0) '' else
        paste(hits_outside$file, hits_outside$line, collapse = '; '))
cat(sprintf('       (%d maintenance sites inside do_taxes.R calc_mtrs)\n',
            nrow(hits) - nrow(hits_outside)))

# --- (1b) behavioral: stale vs zeroed aggregate, same answer -----------------
return_vars = list()
source('src/calc/utils.R')
source('src/calc/functions/deductions/below_ded.R')
source('src/calc/functions/tax/pr.R')

# A record with $5,000 of tips and $4,000 of OT inside $40,000 of wages, as it
# looks AFTER the extensive-margin perturbation zeroes tips1 (wages1 and wages
# decremented by the tips, tips1 set to 0). The only difference between the two
# frames is the household aggregate.
perturbed = function(tips_agg, ot_agg) {
  tibble(
    filing_status = 1,
    wages1 = 35000, wages2 = 0, wages = 35000,
    tips1 = 0, tips2 = 0, tips = tips_agg,
    ot1 = 4000, ot2 = 0, ot = ot_agg,
    tips_lh1 = 1, tips_lh2 = 0,
    agi = 35000,
    # tip/OT deduction law, OBBBA-like
    below.tip_ded_wage_limit = 160000, below.tip_ded_lh = 0,
    below.tip_ded_limit = 25000, below.tip_ded_po_type = 1,
    below.tip_ded_po_rate = 0.1, below.tip_ded_po_range = 1,
    below.tip_ded_po_thresh = 150000,
    below.ot_ded_wage_limit = 160000, below.ot_ded_half = 0,
    below.ot_ded_limit = 12500, below.ot_ded_po_type = 1,
    below.ot_ded_po_rate = 0.1, below.ot_ded_po_range = 1,
    below.ot_ded_po_thresh = 150000,
    # payroll law
    pr.tips_exempt = 1, pr.ot_exempt = 1,
    pr.seca_taxable_rate = 0.9235, pr.se_thresh = 400,
    pr.oasdi_ee_rates1 = 0.062,  pr.oasdi_ee_brackets1 = 0,
    pr.oasdi_er_rates1 = 0.062,  pr.oasdi_er_brackets1 = 0,
    pr.hi_ee_rates1    = 0.0145, pr.hi_ee_brackets1    = 0,
    pr.hi_er_rates1    = 0.0145, pr.hi_er_brackets1    = 0,
    pr.add_med_rates1  = 0.009,  pr.add_med_brackets1  = 200000
  )
}

# stale = pre-fix (aggregate left at its original value); zeroed = post-fix
stale  = perturbed(tips_agg = 5000, ot_agg = 4000)
zeroed = perturbed(tips_agg = 0,    ot_agg = 4000)   # tips1 was the perturbed var

bd_stale  = calc_below_ded(stale,  fill_missings = TRUE)
bd_zeroed = calc_below_ded(zeroed, fill_missings = TRUE)
check('tip_ded identical with stale vs zeroed `tips` aggregate',
      isTRUE(all.equal(bd_stale$tip_ded, bd_zeroed$tip_ded)),
      sprintf('%.2f vs %.2f', bd_stale$tip_ded, bd_zeroed$tip_ded))
check('ot_ded identical with stale vs zeroed `tips` aggregate',
      isTRUE(all.equal(bd_stale$ot_ded, bd_zeroed$ot_ded)),
      sprintf('%.2f vs %.2f', bd_stale$ot_ded, bd_zeroed$ot_ded))

# tip_ded must reflect the ZEROED tips1, not the stale aggregate: a record whose
# primary-earner tips were just wiped out should get no tip deduction
check('tip_ded is 0 after tips1 zeroed (reads tips1, not aggregate)',
      isTRUE(all.equal(bd_stale$tip_ded, 0)),
      sprintf('%.2f', bd_stale$tip_ded))

pr_stale  = calc_pr(stale,  fill_missings = TRUE)
pr_zeroed = calc_pr(zeroed, fill_missings = TRUE)
check('liab_pr identical with stale vs zeroed `tips` aggregate',
      isTRUE(all.equal(pr_stale$liab_pr, pr_zeroed$liab_pr)),
      sprintf('%.2f vs %.2f', pr_stale$liab_pr, pr_zeroed$liab_pr))

# Same exercise for the OT margin (ot1 the perturbed var, tips left alone)
ot_stale  = perturbed(tips_agg = 5000, ot_agg = 4000) %>% mutate(ot1 = 0, wages1 = 31000, wages = 31000, agi = 31000)
ot_zeroed = ot_stale %>% mutate(ot = 0)
check('ot_ded identical with stale vs zeroed `ot` aggregate',
      isTRUE(all.equal(calc_below_ded(ot_stale,  fill_missings = TRUE)$ot_ded,
                       calc_below_ded(ot_zeroed, fill_missings = TRUE)$ot_ded)))
check('liab_pr identical with stale vs zeroed `ot` aggregate',
      isTRUE(all.equal(calc_pr(ot_stale,  fill_missings = TRUE)$liab_pr,
                       calc_pr(ot_zeroed, fill_missings = TRUE)$liab_pr)))

# --- (1c) the fix's own bookkeeping: other_vars now names the aggregate ------
# Read the branch back out of the source so the test fails if someone reverts it.
src = readLines('src/calc/do_taxes.R', warn = FALSE)
ext_block = paste(src, collapse = '\n')
for (v in c('tips1', 'tips2', 'ot1', 'ot2')) {
  agg = if (str_starts(v, 'tips')) 'tips' else 'ot'
  wag = if (str_ends(v, '1')) 'wages1' else 'wages2'
  pat = sprintf("var == '%s'\\) \\{\\s*\\n\\s*other_vars = c\\('%s', 'wages', '%s'\\)",
                v, wag, agg)
  check(sprintf("extensive branch for %-5s decrements %-6s + wages + %s",
                v, wag, agg),
        str_detect(ext_block, pat))
}


#===============================================================================
# (2) FIX 2 -- NA index guard in the tax law parser
#
# (2a) POSITIVE: every real tax law config still parses inside the horizon. This
#      is the false-positive check that matters -- if any shipped config relies
#      on a silently-NA index, the guard would break the model.
# (2b) NEGATIVE: a simulation window past the end of the index series now stops
#      loudly instead of reverting the standard deduction to nominal 2017-ish
#      dollars.
#===============================================================================

cat('\n== (2) FIX 2: NA index guard ==\n')

source('src/data/helpers.R')
source('src/data/economy.R')
source('src/data/tax_law.R')
source('src/misc/utils.R')

macro_root = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'
vat_root   = '/nfs/roberts/project/pi_nrs36/shared/model_data/Value-Added-Tax-Model/v1/2024050121/baseline'

horizon = read_macro_spliced(macro_root) %>% pull(year) %>% max()
cat(sprintf('  index-series horizon: %d\n', horizon))

build_indexes = function(years) {
  generate_indexes(
    macro_root           = macro_root,
    vat_price_offset     = get_vat_price_offset(macro_root, vat_root, years),
    excess_growth_offset = get_excess_growth_offset(0, min(years), years)
  )
}

# Replicates build_tax_law()'s parse step without the output-path writes
parse_config = function(dir, years, indexes) {
  tax_law = load_tax_law_input('./config/scenarios/tax_law/baseline')
  if (!is.null(dir)) {
    changes = load_tax_law_input(dir)
    for (param in names(changes)) {
      tax_law[[param]][names(changes[[param]])] = changes[[param]]
    }
  }
  tax_law %>%
    map2(.f = parse_param, .y = names(.), years = 2014:max(years),
         indexes = indexes) %>%
    bind_rows()
}

# --- (2a) positive: baseline + every reform config, normal window ------------
years_ok = 2025:2035
idx_ok   = build_indexes(years_ok)

base_parsed = tryCatch(parse_config(NULL, years_ok, idx_ok),
                       error = function(e) conditionMessage(e))
check('baseline parses inside horizon (no false positive)',
      is.data.frame(base_parsed),
      if (is.data.frame(base_parsed)) '' else as.character(base_parsed))

# Standard deduction is indexed: confirm it is actually rising, i.e. the index
# chain is live rather than NA-and-silently-flat
if (is.data.frame(base_parsed)) {
  # parse_param() returns parameter/subparameter/year/filing_status/element/value,
  # with filing-status-mapped names (std.value, not std.value_single)
  std = base_parsed %>%
    filter(parameter == 'std', subparameter == 'value',
           filing_status == 1, element == 1, year %in% c(2026, 2035)) %>%
    arrange(year)
  check('std deduction indexed upward 2026 -> 2035 (index is live)',
        nrow(std) == 2 && std$value[2] > std$value[1],
        if (nrow(std) == 2) sprintf('%.0f -> %.0f', std$value[1], std$value[2]) else 'rows missing')

  # The NA-i_measure sentinel must still freeze the parameter, not error and not
  # index it. ed.llc_po_thresh_single is indexed to cpi/chained_cpi through 2019
  # and then `i_measure: NA` from 2020, i.e. frozen in nominal terms.
  llc = base_parsed %>%
    filter(parameter == 'ed', subparameter == 'llc_po_thresh',
           filing_status == 1, element == 1, year %in% c(2026, 2035)) %>%
    arrange(year)
  check('NA i_measure sentinel still freezes (ed.llc_po_thresh_single flat)',
        nrow(llc) == 2 && isTRUE(all.equal(llc$value[1], llc$value[2])) &&
          isTRUE(all.equal(llc$value[1], 80000)),
        if (nrow(llc) == 2) sprintf('%.0f -> %.0f', llc$value[1], llc$value[2]) else 'rows missing')

  # How the sentinel arrives from YAML -- string 'NA' vs NA_character_ decides
  # which test the guard needs; it handles both, this just records which it is.
  ed_raw = load_tax_law_input('./config/scenarios/tax_law/baseline')$ed
  sentinel = ed_raw$llc_po_thresh_single$i_measure[['2020']]
  cat(sprintf('       (sentinel parses as %s, class %s)\n',
              if (is.na(sentinel)) 'NA' else paste0("'", sentinel, "'"),
              class(sentinel)))
}

# --- (2a-ii) an unknown measure name IS a broken chain and must stop ---------
# A typo'd series name produces an all-NA index and, pre-guard, silently
# un-indexed the parameter. Distinguishable from the NA sentinel.
typo_dir = 'other/tips_ot_index_fix/tmp_typo_config'
dir.create(typo_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(c(
  '---',
  'value_single:',
  '  value:',
  "    '2014': 3000",
  "    '2018': 12000",
  "    '2026': 15750",
  '  i_measure:',
  "    '2026': chained_cpi_TYPO",
  '  i_base_year: 2024',
  '  i_direction: -1',
  '  i_increment: 50'
), file.path(typo_dir, 'std.yaml'))

res_typo = tryCatch({ parse_config(typo_dir, years_ok, idx_ok); 'NO ERROR' },
                    error = function(e) conditionMessage(e))
check('unknown measure name raises the guard',
      res_typo != 'NO ERROR' &&
        str_detect(res_typo, 'Indexation series for subparameter'),
      if (res_typo == 'NO ERROR') 'silently un-indexed' else '')
unlink(typo_dir, recursive = TRUE)

# --- (2b) negative: window past the horizon must stop -----------------------
years_bad = (horizon - 1):(horizon + 2)
idx_bad   = build_indexes(years_bad)

res_bad = tryCatch({ parse_config(NULL, years_bad, idx_bad); 'NO ERROR' },
                   error = function(e) conditionMessage(e))
check('window past horizon raises an error',
      res_bad != 'NO ERROR')
check('error is the NA-index guard, naming subparameter + years',
      str_detect(res_bad, 'Indexation series for subparameter') &&
        str_detect(res_bad, as.character(horizon + 1)),
      res_bad)

# And confirm the pre-fix failure mode is what the guard prevents: with the
# guard bypassed, apply_indexation() maps the NA index back to base_value.
check('apply_indexation maps NA index to base_value (the averted bug)',
      isTRUE(all.equal(
        apply_indexation(tibble(base_value = 12000, i_index = NA_real_,
                                i_direction = 1, i_increment = 50))$value,
        12000)))

# --- (2c) full sweep: every reform config parses inside the horizon ----------
# Slowest check by far (a full baseline parse per config dir), so it runs last.
reform_dirs = list.dirs('./config/scenarios/tax_law', recursive = TRUE) %>%
  keep(~ length(list.files(.x, pattern = '\\.ya?ml$')) > 0) %>%
  discard(~ .x == './config/scenarios/tax_law/baseline')
cat(sprintf('\n  sweeping %d reform config dirs...\n', length(reform_dirs)))

sweep = reform_dirs %>%
  imap_dfr(function(d, i) {
    if (i %% 100 == 0) cat(sprintf('    ...%d/%d\n', i, length(reform_dirs)))
    res = tryCatch({ parse_config(d, years_ok, idx_ok); NA_character_ },
                   error = function(e) conditionMessage(e))
    tibble(dir = d, err = res)
  })
failed = sweep %>% filter(!is.na(err))
guard_failed = failed %>% filter(str_detect(err, 'Indexation series for subparameter'))

check('no reform config trips the new NA-index guard',
      nrow(guard_failed) == 0,
      if (nrow(guard_failed) == 0) '' else paste(guard_failed$dir, collapse = '; '))
if (nrow(failed) > nrow(guard_failed)) {
  cat('  NOTE: configs failing for OTHER (pre-existing) reasons:\n')
  failed %>%
    filter(!str_detect(err, 'Indexation series for subparameter')) %>%
    pwalk(function(dir, err) cat(sprintf('    %s\n      %s\n', dir, err)))
}


#===============================================================================
cat(sprintf('\n== %d failure(s) ==\n', n_fail))
if (n_fail > 0) quit(status = 1)
cat('ALL CHECKS PASSED\n')
