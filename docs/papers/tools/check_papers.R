#------------------------------------------------------------------------------
# Validates the docs/papers/ literature store: every manifest row has its PDF
# (with a matching sha256) and summary, every PDF is registered, and every
# extracted parameter belongs to a registered paper and says where it came
# from. Exits non-zero on any failure.
#
# Usage (repo root): Rscript docs/papers/tools/check_papers.R [papers_root]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(digest)
})

PAPERS_ROOT    = commandArgs(trailingOnly = T)[1]
if (is.na(PAPERS_ROOT)) PAPERS_ROOT = 'docs/papers'
REPO_ROOT      = normalizePath(file.path(PAPERS_ROOT, '..', '..'))
STATUS_VALUES  = c('registered', 'read', 'extracted', 'verified')
USE_VALUES     = c('calibration_target', 'parameter', 'validation_benchmark', 'context')
VERIFIED_VALUES = c('y', 'n')

failures = character()
fail = function(...) failures <<- c(failures, paste0(...))

manifest   = read_csv(file.path(PAPERS_ROOT, 'manifest.csv'),   col_types = cols(.default = 'c'))
parameters = read_csv(file.path(PAPERS_ROOT, 'parameters.csv'), col_types = cols(.default = 'c'))

# Manifest rows
dupes = manifest$paper_id[duplicated(manifest$paper_id)]
if (length(dupes) > 0) fail('manifest: duplicate paper_id ', paste(dupes, collapse = ', '))

for (i in seq_len(nrow(manifest))) {
  row = manifest[i, ]
  id  = row$paper_id
  if (!grepl('^[a-z0-9_]+$', id)) fail(id, ': paper_id is not snake_case')
  if (!row$status %in% STATUS_VALUES) fail(id, ': status "', row$status, '" not in ', paste(STATUS_VALUES, collapse = '/'))

  pdf = file.path(REPO_ROOT, row$pdf_file)
  if (!file.exists(pdf)) {
    fail(id, ': missing PDF ', row$pdf_file)
  } else if (!identical(digest(file = pdf, algo = 'sha256'), row$sha256)) {
    fail(id, ': sha256 mismatch for ', row$pdf_file)
  }

  if (row$status != 'registered' && !file.exists(file.path(REPO_ROOT, row$summary_file))) {
    fail(id, ': status is ', row$status, ' but summary ', row$summary_file, ' is missing')
  }
}

# Every PDF is registered
pdfs = list.files(file.path(PAPERS_ROOT, 'pdfs'), pattern = '\\.pdf$', ignore.case = T)
registered = basename(manifest$pdf_file)
for (pdf in setdiff(pdfs, registered)) fail('pdfs/', pdf, ': not in manifest.csv')

# Parameters
dupes = parameters$param_id[duplicated(parameters$param_id)]
if (length(dupes) > 0) fail('parameters: duplicate param_id ', paste(dupes, collapse = ', '))

for (i in seq_len(nrow(parameters))) {
  row = parameters[i, ]
  pid = row$param_id
  if (!row$paper_id %in% manifest$paper_id) fail(pid, ': paper_id ', row$paper_id, ' not in manifest')
  if (!startsWith(pid, paste0(row$paper_id, '.'))) fail(pid, ': param_id must start with "<paper_id>."')
  if (is.na(row$source_loc) || row$source_loc == '') fail(pid, ': empty source_loc')
  if (!row$use %in% USE_VALUES) fail(pid, ': use "', row$use, '" not in ', paste(USE_VALUES, collapse = '/'))
  if (!row$verified %in% VERIFIED_VALUES) fail(pid, ': verified must be y or n')
}

# Tables belong to a registered paper
tables = list.files(file.path(PAPERS_ROOT, 'tables'), pattern = '\\.csv$')
for (tbl in tables) {
  if (!any(startsWith(tbl, paste0(manifest$paper_id, '_')))) fail('tables/', tbl, ': no matching paper_id prefix')
}

if (length(failures) > 0) {
  cat('check_papers: ', length(failures), ' failure(s)\n', paste0('  - ', failures, collapse = '\n'), '\n', sep = '')
  quit(status = 1)
}
cat('check_papers: OK (', nrow(manifest), ' papers, ', nrow(parameters), ' parameters, ',
    length(tables), ' tables)\n', sep = '')
