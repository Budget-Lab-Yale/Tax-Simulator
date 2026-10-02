# Probe: our regular AR schedule liability at Low Income Tax Table points,
# single and HoH, to compare against the DFA tables and the 26-51-301 formula.
#   Rscript research/state_tax/scripts/ar_lit_probe.R <year> <filing_status> <n_dep> <agi...>
suppressPackageStartupMessages(invisible(capture.output(
  lapply(readLines('./requirements.txt'), library, character.only = T))))
return_vars = list()
list.files('./src', recursive = T, pattern = '\\.[Rr]$') %>%
  walk(~ if (. != 'main.R' && !startsWith(., 'slurm/')) source(file.path('./src/', .)))
args = as.numeric(commandArgs(trailingOnly = TRUE))
yr = args[1]; fs = args[2]; nd = args[3]; agis = args[-(1:3)]
law = build_state_tax_law(states = 'AR', years = yr,
  indexes = expand_grid(series = 'cpi', year = 2015:2036) %>% mutate(growth = 0.025))
ct = state_credit_tables_for_year(attr(law, 'credit_tables'), 'AR', yr)
row = law %>% filter(year == yr, filing_status == fs) %>% select(-state, -year, -filing_status)
mfs = law %>% filter(year == yr, filing_status == 3) %>% select(-state, -year, -filing_status)
for (a in agis) {
  u = list(agi = a, wages1 = a, ei1 = a, filing_status = fs, n_dep = nd)
  if (fs == 2) u$age2 = 40
  if (nd > 0) { u$n_dep_ctc = nd; u$dep_age1 = 5; if (nd > 1) u$dep_age2 = 6 }
  r = st_test_unit(u) %>% bind_cols(row) %>% do_state_taxes(credit_tables = ct, law_mfs = mfs)
  cat(sprintf('%d agi %.0f txbl %.0f pre_credit %.2f credits %.2f liab %.2f table %s\n', yr, a,
              r$st_txbl_inc, r$st_tax_pre_credit, r$st_credits_nonref, r$liab_st_iit, r$st_alt_table_used))
}
