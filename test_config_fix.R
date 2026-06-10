# Validates the config_parser fix: scenario-subset runs must retain the
# baseline row in runscript/interface_paths (one-off test)

lapply(readLines('./requirements.txt'), library, character.only = TRUE)
return_vars = list()
for (f in list.files('./src', recursive = TRUE)) {
  if (f != 'main.R' && !startsWith(f, 'slurm/')) source(file.path('./src', f))
}

globals = parse_globals(
  runscript_name   = 'tests/kg_alloc_compare_td0609',
  scenario_id      = 'deemed',
  local            = 1,
  vintage          = 'cfg_fix_test',
  baseline_vintage = 'kg_mech_50_td0609',
  pct_sample       = 0.01,
  multicore        = 'none'
)

stopifnot('baseline' %in% globals$runscript$ID)
stopifnot('deemed'   %in% globals$runscript$ID)
stopifnot(nrow(globals$runscript) == 2)
ip = globals$interface_paths
stopifnot(any(ip$ID == 'baseline' & ip$interface == 'Off-Model-Estimates'))
cat('baseline Off-Model path:',
    ip$path[ip$ID == 'baseline' & ip$interface == 'Off-Model-Estimates'], '\n')

# Invalid scenario id must fail loudly
err = tryCatch(
  { parse_globals('tests/kg_alloc_compare_td0609', 'nonsense', 1,
                  'cfg_fix_test', NULL, 0.01, 'none'); NULL },
  error = function(e) conditionMessage(e)
)
stopifnot(grepl('not found in runscript', err))
cat('invalid-id check:', err, '\n')

cat('ALL CHECKS PASSED\n')
