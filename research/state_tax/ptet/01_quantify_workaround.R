#------------------------------------------------------------------------------
# research/state_tax/ptet/01_quantify_workaround.R
#
# What does the federal SALT-cap workaround (do_salt_workaround_baseline(),
# src/calc/do_taxes.R) move today? For each year, the baseline data-prep chain
# of run_one_year() is built twice -- with the workaround and with it zeroed --
# and both are run through do_taxes(). Reported against Comey, Gallagher and
# Gorman (JCT, Oct 2025), "SALT Cap Workarounds": $64.0B of individual SALT
# diverted to entities in TY2024 ($30.9B S corporations, $33.1B
# partnerships), $18.1B federal revenue cost, ~80% of the cost in the top 1%.
#
# Usage (repo root; full sample, two do_taxes() passes a year -> sbatch):
#   Rscript research/state_tax/ptet/01_quantify_workaround.R <years>
#   e.g. ... 2017:2025
#
# Uses the tests/states_all_components runscript (Tax-Data and State-Weights
# 2026102315_rebuild).
#
# Outputs (research/state_tax/ptet/results/):
#   workaround_national.csv     year x amounts, participants, revenue cost
#   workaround_distribution.csv year x income group shares of amount and cost
#   workaround_by_state.csv     year x state amounts under the state weights
#------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = T)
  ))
)
return_vars = list()
list.files('./src', recursive = T, pattern = '\\.[Rr]$') %>%
  walk(.f = ~ if (.x != 'main.R' && !startsWith(.x, 'slurm/')) source(file.path('./src/', .x)))

RUNSCRIPT = 'tests/states_all_components'
OUT_DIR   = 'research/state_tax/ptet/results'

# Income groups for concentration, by weighted rank of the workaround's own
# income concept (inc, a gross income sum), all non-dependent units
TOP_GROUPS = c(top_0.1 = 0.999, top_1 = 0.99, top_5 = 0.95, top_10 = 0.90)

args = commandArgs(trailingOnly = T)
if (length(args) != 1) stop('usage: 01_quantify_workaround.R <years>')
years = eval(parse(text = args[1]))

globals = parse_globals(
  runscript_name   = RUNSCRIPT,
  scenario_id      = NULL,
  local            = 1,
  vintage          = NULL,
  baseline_vintage = NULL,
  pct_sample       = 1,
  multicore        = 'none'
)
scenario_info = get_scenario_info('baseline')

vat_price_offset = get_vat_price_offset(
  macro_root = scenario_info$interface_paths$`Macro-Projections`,
  vat_root   = scenario_info$interface_paths$`Value-Added-Tax-Model`,
  years      = scenario_info$years
)
excess_growth_offset = get_excess_growth_offset(
  excess_growth = scenario_info$excess_growth,
  start_year    = scenario_info$excess_growth_start_year,
  years         = scenario_info$years
)
indexes = generate_indexes(
  macro_root           = scenario_info$interface_paths$`Macro-Projections`,
  vat_price_offset     = vat_price_offset,
  excess_growth_offset = excess_growth_offset
)
tax_law = build_tax_law(scenario_info, indexes)
sw_root = scenario_info$interface_paths$`State-Weights`
if (is.null(sw_root)) stop(RUNSCRIPT, ' pins no State-Weights vintage')


finish_and_tax = function(tax_units, year) {

  # Rest of run_one_year()'s data prep, then the federal calculator
  tax_units %>%
    do_ss_cola(year, vat_price_offset) %>%
    do_capital_adjustment(year, vat_price_offset) %>%
    do_excess_growth(scenario_info, excess_growth_offset) %>%
    calc_kg_cpi_ratio(indexes, year) %>%
    do_taxes(baseline_pr_er = NULL,
             vars_1040      = fed_calc_vars(incl_payroll = F),
             vars_payroll   = return_vars$calc_pr)
}


quantify_year = function(year) {

  t0 = Sys.time()

  # Data prep up to the workaround, as run_one_year() (src/sim/run.R)
  base = scenario_info$interface_paths$`Tax-Data` %>%
    read_microdata(year) %>%
    mutate(year = year) %>%
    (\(d) bind_cols(d, build_random_numbers(d$id))) %>%
    mutate(filing_status_input = filing_status) %>%
    left_join(tax_law %>% distinct(year, filing.repeal_hoh), by = 'year') %>%
    mutate(filing_status = if_else(filing.repeal_hoh == 1 & filing_status == 4,
                                   1, filing_status)) %>%
    left_join(tax_law, by = c('year', 'filing_status'))

  with_wa = do_salt_workaround_baseline(base)
  wa = with_wa %>%
    transmute(id, weight, dep_status, inc, part, scorp, salt_inc_sales_pre = base$salt_inc_sales,
              salt_part, salt_scorp, salt_workaround_part, salt_workaround_scorp,
              moved = salt_workaround_part + salt_workaround_scorp)
  n_na = sum(is.na(wa$moved))
  if (n_na > 0) stop(year, ': ', n_na, ' records with NA workaround amounts')

  # Federal liability with and without the workaround. The counterfactual
  # runs the same function at zero participation, so the part/scorp income
  # columns it recomputes are identical on both sides
  without_wa = base %>%
    mutate(item.salt_workaround_allowed_part     = 0,
           item.salt_workaround_allowed_sstb     = 0,
           item.salt_workaround_allowed_non_sstb = 0) %>%
    do_salt_workaround_baseline()
  stopifnot(all(without_wa$salt_workaround_part == 0),
            all(without_wa$salt_workaround_scorp == 0))
  taxed_with    = finish_and_tax(with_wa, year)
  taxed_without = finish_and_tax(without_wa, year)
  stopifnot(identical(taxed_with$id, wa$id), identical(taxed_without$id, wa$id))
  wa$cost = taxed_without$liab_iit - taxed_with$liab_iit

  national = wa %>%
    summarise(
      year                = year,
      salt_limit          = tax_law$item.salt_limit[tax_law$year == year][1],
      salt_inc_sales_bn   = sum(weight * salt_inc_sales_pre) / 1e9,
      eligible_part_bn    = sum(weight * salt_part) / 1e9,
      eligible_scorp_bn   = sum(weight * salt_scorp) / 1e9,
      moved_part_bn       = sum(weight * salt_workaround_part) / 1e9,
      moved_scorp_bn      = sum(weight * salt_workaround_scorp) / 1e9,
      moved_total_bn      = sum(weight * moved) / 1e9,
      participants_m      = sum(weight * (moved > 0)) / 1e6,
      part_income_bn      = sum(weight * pmax(0, part)) / 1e9,
      scorp_income_bn     = sum(weight * pmax(0, scorp)) / 1e9,
      fed_cost_bn         = sum(weight * cost) / 1e9
    )

  # Concentration: share of amount moved and of federal cost by top group
  ranked = wa %>% filter(dep_status == 0) %>% arrange(inc) %>%
    mutate(pctile = cumsum(weight) / sum(weight))
  distribution = imap_dfr(TOP_GROUPS, function(cut, group) {
    ranked %>%
      summarise(year = year, group = group,
                share_moved = sum(weight * moved * (pctile > cut)) /
                              sum(weight * moved),
                share_cost  = sum(weight * cost * (pctile > cut)) /
                              sum(weight * cost))
  })

  # By state: each record's amount split by its state weights
  sw = fread(file.path(sw_root, paste0('state_weights_', year, '.csv.gz')))
  rec = as.data.table(wa)[moved > 0 | cost != 0, .(id, w_nat = weight, moved, cost)]
  by_state = merge(sw, rec, by = 'id')[
    , .(moved_bn = sum(weight * moved) / 1e9,
        cost_bn  = sum(weight * cost) / 1e9), by = state][, year := year]

  # The state split must conserve the national totals
  if (abs(sum(by_state$moved_bn) - national$moved_total_bn) >
      1e-6 * max(1, national$moved_total_bn)) {
    stop(year, ': state split does not sum to the national amount moved')
  }

  message(sprintf('%d: moved $%.1fB, cost $%.1fB (%.0fs)', year,
                  national$moved_total_bn, national$fed_cost_bn,
                  as.numeric(Sys.time() - t0, units = 'secs')))
  list(national = national, distribution = distribution, by_state = by_state)
}


results = map(years, quantify_year)

dir.create(OUT_DIR, showWarnings = F, recursive = T)
national = map_dfr(results, 'national')
write_csv(national, file.path(OUT_DIR, 'workaround_national.csv'))
write_csv(map_dfr(results, 'distribution'),
          file.path(OUT_DIR, 'workaround_distribution.csv'))
write_csv(rbindlist(map(results, 'by_state')) %>% arrange(year, -moved_bn),
          file.path(OUT_DIR, 'workaround_by_state.csv'))

options(width = 200)
print(national %>% mutate(across(where(is.double), ~ round(.x, 2))))
cat('WORKAROUND_QUANTIFY_SUCCESS\n')
