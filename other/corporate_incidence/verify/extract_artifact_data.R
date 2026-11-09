#-------------------------------------------------------------------------------
# extract_artifact_data.R
#
# Gathers the corp_incidence verification numbers into one JSON for the
# inspection artifact: analytic paths (from the per-year conservation diags),
# sweep-corner mu paths, FY receipts by scenario, CY estate totals, and the
# wealth-bathtub state trajectories. Run via sbatch.
#
# Output: other/corporate_incidence/verify/out/artifact_data.json
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(purrr); library(jsonlite)
})

root_of = function(v) file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1', v)
central = root_of('corp_test_v1')
out_dir = 'other/corporate_incidence/verify/out'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_diags = function(root, scen) {
  files = Sys.glob(file.path(root, scen, 'conventional', 'supplemental',
                             'corp_conservation_diag_*.csv'))
  map_dfr(files, ~ read_csv(.x, show_col_types = FALSE)) %>% arrange(year)
}

# --- 1. central paths + flow decomposition ------------------------------------
paths = list(
  corp_perm   = read_diags(central, 'corp_perm'),
  corp_sunset = read_diags(central, 'corp_sunset')
)

# --- 2. sweep mu paths ----------------------------------------------------------
sweeps = list(
  sigma0  = read_diags(root_of('corp_sweep_sigma0'),  'corp_perm')  %>% select(year, mu, phi),
  sigma05 = read_diags(root_of('corp_sweep_sigma05'), 'corp_perm')  %>% select(year, mu, phi),
  kappa25 = read_diags(root_of('corp_sweep_kappa25'), 'corp_perm')  %>% select(year, mu, phi),
  kappa50 = read_diags(root_of('corp_sweep_kappa50'), 'corp_perm')  %>% select(year, mu, phi),
  pap     = read_diags(root_of('corp_sweep_pap'),     'corp_sunset') %>% select(year, mu, phi)
)

# --- 3. FY receipts by scenario ---------------------------------------------------
rec = function(scen, pass) {
  read_csv(file.path(central, scen, pass, 'totals', 'receipts_full.csv'),
           show_col_types = FALSE) %>%
    select(year, iit = revenues_income_tax, corp = revenues_corp_tax,
           estate = revenues_estate_tax)
}
receipts = list(
  baseline           = rec('baseline', 'static'),
  corp_perm          = rec('corp_perm', 'conventional'),
  corp_nometa        = rec('corp_nometa', 'conventional'),
  corp_perm_wealth   = rec('corp_perm_wealth', 'conventional'),
  corp_sunset        = rec('corp_sunset', 'conventional'),
  corp_sunset_wealth = rec('corp_sunset_wealth', 'conventional'),
  corp_perm_kg       = rec('corp_perm_kg', 'conventional')
)

# --- 4. CY estate totals -----------------------------------------------------------
est = function(scen, pass) {
  read_csv(file.path(central, scen, pass, 'totals', 'estate.csv'),
           show_col_types = FALSE) %>% select(year, est_tax_exp)
}
estate = list(
  baseline           = est('baseline', 'static'),
  corp_perm          = est('corp_perm', 'conventional'),
  corp_perm_wealth   = est('corp_perm_wealth', 'conventional'),
  corp_sunset        = est('corp_sunset', 'conventional'),
  corp_sunset_wealth = est('corp_sunset_wealth', 'conventional'),
  corp_perm_kg       = est('corp_perm_kg', 'conventional')
)

# --- 5. wealth bathtub trajectories -------------------------------------------------
bathtub = function(scen) {
  files = Sys.glob(file.path(central, scen, 'conventional', 'supplemental',
                             'wealth_dynamics_state', '*.rds'))
  map_dfr(files, function(f) {
    st = readRDS(f)
    tibble(year    = st$year,
           P_sum   = sum(st$P) / 1e9,          # $B, + = deficit (dissaving)
           F_sum   = sum(st$diag$dT0) / 1e9)   # $B forcing that year
  }) %>% arrange(year)
}
wealth = list(
  corp_perm_wealth   = bathtub('corp_perm_wealth'),
  corp_sunset_wealth = bathtub('corp_sunset_wealth')
)

# --- 6. kg state: corp gain debit ---------------------------------------------------
kg_debit = map_dfr(2026:2033, function(y) {
  f = file.path(central, 'corp_perm_kg', 'conventional', 'supplemental',
                'kg_dynamics_state', paste0(y, '.rds'))
  if (!file.exists(f)) return(tibble())
  st = readRDS(f)
  tibble(year = y, debit_total = sum(st$cell_table$corp_gain_debit) / 1e9)
})

payload = list(
  generated  = format(Sys.time(), '%Y-%m-%d %H:%M'),
  paths      = paths,
  sweeps     = sweeps,
  receipts   = receipts,
  estate     = estate,
  wealth     = wealth,
  kg_debit   = kg_debit
)
write_json(payload, file.path(out_dir, 'artifact_data.json'),
           digits = 8, pretty = FALSE)
cat('WROTE', file.path(out_dir, 'artifact_data.json'), '\n')
