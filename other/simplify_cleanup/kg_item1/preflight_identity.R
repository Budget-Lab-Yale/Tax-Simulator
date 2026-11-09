#-------------------------------------------------------------------------------
# preflight_identity.R
#
# Structural proof that item #1 (Part A aging matrix + Part B state paths) and
# bundled #9 (ESTATE_ASSET_COLS alias) are behavior-preserving, INDEPENDENT of
# the scenario run. Sources the *edited* repo exactly as main.R does, then
# asserts that each refactored construct reproduces the OLD (pre-edit) behavior
# byte-for-byte (the OLD forms are hardcoded here as the reference).
#-------------------------------------------------------------------------------

setwd('/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator')

suppressPackageStartupMessages(invisible(capture.output(
  lapply(readLines('./requirements.txt'), library, character.only = TRUE)
)))

# Source all src the way main.R does (also a full load-time smoke of the edits).
# main.R seeds return_vars before the walk (a src file references it at source
# time); mirror that so the load succeeds.
return_vars = list()
invisible(list.files('./src', recursive = TRUE) %>%
  walk(~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/'))
         source(file.path('./src/', .x))))

fail = function(msg) { cat('IDENTITY_FAIL:', msg, '\n'); quit(status = 1) }

# ---- Identity 1: aging matrix (Part A) -------------------------------------
# The OLD kg private copy, reconstructed verbatim.
old_aging = function(ages) {
  n = length(ages)
  A = matrix(0, n, n, dimnames = list(ages, ages))
  for (i in seq_len(n - 1)) A[i, i + 1] = 1
  A[n, n] = 1
  A
}
for (ages in list(KG_DYN_AGE_MIN:KG_DYN_AGE_MAX, 18:80, 0:5, 40:41)) {
  if (!identical(build_aging_matrix(ages), old_aging(ages))) fail('aging matrix')
}

# ---- Identity 2: estate asset value cols (#9) ------------------------------
# The OLD kg literal (content AND order matter: used to index as.matrix()).
old_estate_cols = c(
  'value.cash', 'value.equities', 'value.bonds', 'value.dc', 'value.db',
  'value.life_ins', 'value.annuities', 'value.trusts', 'value.other_fin',
  'value.pass_throughs', 'value.primary_home', 'value.other_home',
  'value.re_fund', 'value.other_nonfin'
)
if (!identical(ESTATE_ASSET_COLS, old_estate_cols)) fail('ESTATE_ASSET_COLS content/order')

# ---- Identity 3: state path helpers (Part B) -------------------------------
# The NEW thin wrappers must reproduce the OLD literal file.path() strings.
si = list(output_path = '/ROOT/scen name')  # include a space to catch join bugs
chk = function(got, want) if (!identical(got, want))
  fail(paste0('path mismatch: [', got, '] != [', want, ']'))

chk(kg_dyn_state_dir(si),
    file.path('/ROOT/scen name', 'conventional', 'supplemental', 'kg_dynamics_state'))
chk(kg_dyn_state_path(si, 2027),
    file.path('/ROOT/scen name', 'conventional', 'supplemental', 'kg_dynamics_state', '2027.rds'))
chk(kg_dyn_mech_state_dir(si),
    file.path('/ROOT/scen name', 'static', 'supplemental', 'kg_dynamics_mech_state'))
chk(kg_dyn_mech_state_path(si, 2027),
    file.path('/ROOT/scen name', 'static', 'supplemental', 'kg_dynamics_mech_state', '2027.rds'))
chk(kg_dyn_inputs_cache_path(si),
    file.path('/ROOT/scen name', 'static', 'supplemental', 'kg_dynamics_mech_state', 'inputs_cache.rds'))

cat('ALL_IDENTITIES_PASS\n')
