# Consistency check for research/state_tax/tax_data_gaps.csv, the registry of
# state provisions blocked on a variable Tax-Data lacks (decision S43). Run from
# the repo root; exits non-zero on the first class of failure so it can gate a
# commit. Checks: header and controlled vocabularies, yaml_file exists under
# config/scenarios/tax_law_state/baseline/, kd_lines point at rows of
# known_differences.csv for the same state (or ALL), overlay_variable is either
# in the supplemental-inputs schema (when that file exists) or an `other:` name,
# and no (state, provision) is duplicated.
suppressPackageStartupMessages(library(data.table))

GAPS_PATH   = 'research/state_tax/tax_data_gaps.csv'
KD_PATH     = 'src/tests/state/cross_model/known_differences.csv'
YAML_ROOT   = 'config/scenarios/tax_law_state/baseline'
SCHEMA_PATH = 'config/interfaces/supplemental_inputs_schema.yaml'

HEADER = c('state', 'provision', 'yaml_file', 'yaml_line', 'required_variable', 'key',
           'tax_data_column', 'tax_data_status', 'current_handling', 'imputation_item',
           'tier', 'overlay_variable', 'candidate_source', 'kd_lines', 'materiality',
           'direction', 'note')
VOCAB = list(
  key              = c('unit', 'person', 'state'),
  tax_data_status  = c('absent', 'present_unused', 'present_zero', 'present_capped',
                       'present_pooled', 'present_partial'),
  current_handling = c('omitted', 'constant', 'yaml_share', 'proxy', 'pooled',
                       'split_even', 'age_proxy', 'gated_off'),
  imputation_item  = c(as.character(1:14), 'new', 'none'),
  tier             = c('1', '2', '3', 'none'),
  materiality      = c('high', 'medium', 'low', 'de_minimis', 'unknown'),
  direction        = c('overtax', 'undertax', 'both', 'none')
)

gaps = fread(GAPS_PATH, colClasses = 'character')
fail = character()
if (!identical(names(gaps), HEADER)) fail = c(fail, 'header does not match the schema')

for (col in names(VOCAB)) {
  bad = gaps[!(get(col) %in% VOCAB[[col]]), unique(get(col))]
  if (length(bad)) fail = c(fail, sprintf('%s: unknown value(s) %s', col, paste(bad, collapse = ', ')))
}

dupes = gaps[, .N, by = .(state, provision)][N > 1]
if (nrow(dupes)) fail = c(fail, sprintf('duplicate (state, provision): %s',
                                        paste(dupes[, paste(state, provision, sep = '/')], collapse = '; ')))

yaml_rows = gaps[yaml_file != 'none' & yaml_file != '']
missing_yaml = yaml_rows[!file.exists(file.path(YAML_ROOT, yaml_file)) & !file.exists(yaml_file), unique(yaml_file)]
if (length(missing_yaml)) fail = c(fail, sprintf('yaml_file not found: %s', paste(missing_yaml, collapse = ', ')))

kd = fread(KD_PATH, colClasses = 'character'); kd[, line := .I + 1]
kd_rows = gaps[kd_lines != 'none' & kd_lines != '']
for (i in seq_len(nrow(kd_rows))) {
  lines = as.integer(strsplit(kd_rows$kd_lines[i], ';')[[1]])
  hit = kd[line %in% lines]
  if (length(lines) != nrow(hit))
    fail = c(fail, sprintf('%s/%s: kd_lines %s not all in known_differences.csv',
                           kd_rows$state[i], kd_rows$provision[i], kd_rows$kd_lines[i]))
  wrong = hit[state != 'ALL' & state != kd_rows$state[i]]
  if (nrow(wrong))
    fail = c(fail, sprintf('%s/%s: kd line(s) %s belong to %s',
                           kd_rows$state[i], kd_rows$provision[i],
                           paste(wrong$line, collapse = ';'), paste(unique(wrong$state), collapse = ',')))
}

if (file.exists(SCHEMA_PATH)) {
  schema_vars = names(yaml::read_yaml(SCHEMA_PATH))
  bad = gaps[overlay_variable != 'none' & !grepl('^other:', overlay_variable) &
             !(overlay_variable %in% schema_vars), unique(overlay_variable)]
  if (length(bad)) fail = c(fail, sprintf('overlay_variable not in schema: %s', paste(bad, collapse = ', ')))
}

cat(sprintf('%d rows, %d states, %d with a Tax-Data column present-but-unused, %d absent\n',
            nrow(gaps), uniqueN(gaps$state),
            sum(gaps$tax_data_status == 'present_unused'), sum(gaps$tax_data_status == 'absent')))
if (length(fail)) { cat('FAIL\n'); cat(paste(' -', fail), sep = '\n'); quit(status = 1) }
cat('OK\n')
