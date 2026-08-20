#!/usr/bin/env Rscript
#-------------------------------------------------------------------------------
# check_conventions.R
#
# Enforces research/CONVENTIONS.md against the tree. The conventions exist
# because an unenforced naming scheme drifts -- other/state_tax_research/ reached
# 212 files across ten filename patterns before the 2026-08-19 reorganization --
# and four shell one-liners pasted in a README are not enforcement. This is.
#
# Run from the repo root:
#   Rscript research/tools/check_conventions.R [--report-only] [--check N] [-v]
#   Rscript research/tools/check_conventions.R --selftest
#
#   --report-only   print findings, exit 0 anyway (for a non-blocking hook)
#   --selftest      plant one violation per check and confirm each is caught
#   --check N       run only check N (repeatable: --check 5 --check 6)
#   -v              also list what each check covered
#
# Exit status is 1 if any check fails, so this can gate a commit or a CI job.
#
# WHAT IT DELIBERATELY DOES NOT DO: it never reads prose for meaning. Whether
# STATUS.md agrees with a plan is what the `sot:` field is for -- a checker that
# tried would produce noise, and noise is how a check gets ignored.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(yaml) })

if (!file.exists('./src/main.R')) stop('Run from the Tax-Simulator repo root')

args        <- commandArgs(trailingOnly = TRUE)
report_only <- '--report-only' %in% args
verbose     <- '-v' %in% args
only_checks <- as.integer(args[which(args == '--check') + 1])
only_checks <- only_checks[!is.na(only_checks)]

#-------------------------------------------------------------------------------
# The conventions, as data. Every closed vocabulary in CONVENTIONS.md appears
# here once; a typo'd `role:` would otherwise silently exempt a file from the
# one-plan-per-workstream check.
#-------------------------------------------------------------------------------

ROLES       <- c('plan', 'method', 'procedure', 'evidence',
                 'notes', 'review', 'status', 'index')
STATUSES    <- c('current', 'open', 'deferred', 'historical',
                 'frozen', 'executed', 'superseded')
WORKSTREAMS <- c('state_tax', 'state_weights', 'cross-cutting')

# Archive `{reason}` is closed too, but `pre-{change}` is open-ended.
ARCHIVE_REASON_RE <- '(pre-[a-z0-9-]+|stale-render|executed|superseded|imported)'

# Workstreams that own a plan. `cross-cutting` documents sit at the root and
# have no plan to be cited from.
PLAN_WORKSTREAMS <- c('state_tax', 'state_weights')

# Directories holding ARTIFACTS rather than documents. Front matter is a
# document convention: source packets defer their status to the rollout CSV
# (decision S6), cross-model reports are generated, and research/raw/ is
# append-only verbatim output that is never edited. Archived snapshots keep
# whatever front matter they had when they were living, which is often none.
#
# This list is the rule's real scope. CONVENTIONS.md says "every living research
# .md", which overclaims: 101 of 136 live files are in these classes, with zero
# stragglers outside them.
ARTIFACT_DIRS <- c('research/raw/',
                   'research/source_packets/',
                   'research/archive/')
ARTIFACT_RE   <- '/results/reports/'

# File extensions a citation might carry. Used to tell a path citation from
# prose that merely contains a slash.
CITE_EXT <- 'md|Rmd|R|r|py|csv|tsv|yaml|yml|json|txt|docx|xlsx|xls|pdf|rds|sbatch|do|sh'

EXTERNAL_PATHS_FILE <- 'research/tools/known_external_paths.csv'

#-------------------------------------------------------------------------------
# Findings
#-------------------------------------------------------------------------------

findings <- new.env(parent = emptyenv())
findings$rows <- list()
covered      <- new.env(parent = emptyenv())
covered$n    <- list()

add <- function(check, path, line, msg) {
  findings$rows[[length(findings$rows) + 1]] <-
    list(check = check, path = path, line = line, msg = msg)
}
note_covered <- function(check, n, what) {
  covered$n[[as.character(check)]] <- sprintf('%d %s', n, what)
}

#-------------------------------------------------------------------------------
# Helpers
#-------------------------------------------------------------------------------

md_files <- function(include_archive = TRUE) {
  f <- list.files('research', pattern = '\\.md$', recursive = TRUE,
                  full.names = TRUE)
  f <- sub('^\\./', '', f)
  if (!include_archive) f <- f[!startsWith(f, 'research/archive/')]
  sort(f)
}

is_artifact <- function(path) {
  any(startsWith(path, ARTIFACT_DIRS)) || grepl(ARTIFACT_RE, path, fixed = TRUE)
}

# Returns NULL when the file has no front-matter block, else a list of
#   values : the parsed YAML (a `key: null` disappears here, which is why...)
#   keys   : ...the raw key names are kept alongside it
#   lines  : line number of each key, so findings can point at one
read_front_matter <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (!length(lines) || trimws(lines[1]) != '---') return(NULL)
  close_at <- which(trimws(lines[-1]) == '---')[1]
  if (is.na(close_at)) return(NULL)
  block <- lines[2:close_at]
  keys  <- sub('^([A-Za-z_]+):.*$', '\\1', block)
  hit   <- grepl('^[A-Za-z_]+:', block)
  values <- tryCatch(yaml::yaml.load(paste(block, collapse = '\n')),
                     error = function(e) NULL)
  list(values = values,
       keys   = keys[hit],
       lines  = setNames(which(hit) + 1L, keys[hit]),
       raw    = setNames(sub('^[A-Za-z_]+: *', '', block[hit]), keys[hit]),
       n      = close_at + 1L)
}

fm_field <- function(fm, field) {
  if (is.null(fm)) return(NA_character_)
  v <- fm$values[[field]]
  if (is.null(v) || !length(v)) return(NA_character_)
  as.character(v)[1]
}
fm_line <- function(fm, field) {
  if (is.null(fm) || !(field %in% names(fm$lines))) return(1L)
  unname(fm$lines[[field]])
}

git_last_commit_date <- function(path) {
  out <- suppressWarnings(system2('git', c('log', '-1', '--format=%ad',
                                           '--date=short', '--', shQuote(path)),
                                  stdout = TRUE, stderr = FALSE))
  if (!length(out) || !nzchar(out[1])) NA_character_ else out[1]
}

# Every path-shaped citation in a file, with its line number. Placeholders such
# as {year} are skipped: they name a family of files, not one.
citations <- function(path) {
  lines <- readLines(path, warn = FALSE)
  re <- sprintf('(?:research|src|config|docs|other)/[A-Za-z0-9_./*-]+\\.(?:%s)\\b',
                CITE_EXT)
  out <- list()
  for (i in seq_along(lines)) {
    m <- regmatches(lines[i], gregexpr(re, lines[i], perl = TRUE))[[1]]
    for (p in m) if (!grepl('[{}*]', p)) out[[length(out) + 1]] <- list(line = i, path = p)
  }
  out
}

read_external_allowlist <- function() {
  if (!file.exists(EXTERNAL_PATHS_FILE)) return(character(0))
  d <- utils::read.csv(EXTERNAL_PATHS_FILE, stringsAsFactors = FALSE,
                       comment.char = '#')
  if (!all(c('path', 'kind', 'reason') %in% names(d))) {
    add(6, EXTERNAL_PATHS_FILE, 1,
        'allowlist must have columns: path, kind, reason')
    return(character(0))
  }
  bad <- d$kind[!d$kind %in% c('cross-repo', 'planned')]
  if (length(bad)) {
    add(6, EXTERNAL_PATHS_FILE, 1,
        sprintf('kind must be cross-repo or planned; found %s',
                paste(unique(bad), collapse = ', ')))
  }
  # A `planned` entry that now exists has served its purpose and should go, or
  # it will mask a later move of the very file it was standing in for.
  now_real <- d$path[d$kind == 'planned' & file.exists(d$path)]
  for (p in now_real) {
    add(6, EXTERNAL_PATHS_FILE, 1,
        sprintf('`%s` is listed as planned but now exists -- remove the entry', p))
  }
  d$path
}

#-------------------------------------------------------------------------------
# 1. Exactly one `role: plan` per workstream
#-------------------------------------------------------------------------------
check_01 <- function() {
  for (w in PLAN_WORKSTREAMS) {
    f <- md_files(include_archive = FALSE)
    f <- f[startsWith(f, paste0('research/', w, '/'))]
    plans <- f[vapply(f, function(p) identical(fm_field(read_front_matter(p), 'role'),
                                               'plan'), logical(1))]
    if (length(plans) != 1) {
      add(1, paste0('research/', w), 1,
          sprintf('expected exactly 1 role: plan, found %d%s', length(plans),
                  if (length(plans)) paste0(': ', paste(plans, collapse = ', ')) else ''))
    }
  }
  note_covered(1, length(PLAN_WORKSTREAMS), 'workstreams')
}

#-------------------------------------------------------------------------------
# 2. Front matter present on every document (artifact directories exempt)
#-------------------------------------------------------------------------------
check_02 <- function() {
  f <- Filter(function(p) !is_artifact(p), md_files(include_archive = FALSE))
  for (p in f) {
    if (is.null(read_front_matter(p))) {
      add(2, p, 1, 'no front-matter block on line 1')
    }
  }
  note_covered(2, length(f), 'documents')
}

#-------------------------------------------------------------------------------
# 3. role / status / workstream drawn from the closed vocabularies
#-------------------------------------------------------------------------------
check_03 <- function() {
  f <- Filter(function(p) !is_artifact(p), md_files(include_archive = FALSE))
  n <- 0
  for (p in f) {
    fm <- read_front_matter(p)
    if (is.null(fm)) next
    n <- n + 1
    for (spec in list(list('role', ROLES), list('status', STATUSES),
                      list('workstream', WORKSTREAMS))) {
      field <- spec[[1]]; allowed <- spec[[2]]
      v <- fm_field(fm, field)
      if (is.na(v)) {
        add(3, p, 1, sprintf('front matter is missing `%s:`', field))
      } else if (!v %in% allowed) {
        add(3, p, fm_line(fm, field),
            sprintf('%s: `%s` is not one of %s', field, v,
                    paste(allowed, collapse = ' / ')))
      }
    }
  }
  note_covered(3, n, 'documents with front matter')
}

#-------------------------------------------------------------------------------
# 4. `updated:` not behind the file's own last commit date
#
# The real failure mode: a document edited without its header bumped. Files with
# no commit yet are skipped -- they are the edit in progress.
#-------------------------------------------------------------------------------
check_04 <- function() {
  f <- Filter(function(p) !is_artifact(p), md_files(include_archive = FALSE))
  n <- 0
  for (p in f) {
    fm <- read_front_matter(p); if (is.null(fm)) next
    u <- fm_field(fm, 'updated'); if (is.na(u)) next
    g <- git_last_commit_date(p); if (is.na(g)) next
    n <- n + 1
    if (u < g) {
      add(4, p, fm_line(fm, 'updated'),
          sprintf('updated: %s is behind the last commit (%s)', u, g))
    }
  }
  note_covered(4, n, 'committed documents')
}

#-------------------------------------------------------------------------------
# 5. A note with `status: open` is cited from its workstream's plan
#
# The rule that guards against outstanding work going missing -- which is the
# failure the reorganization was fixing, and which this check found still live
# in all three open notes on 2026-08-19.
#-------------------------------------------------------------------------------
check_05 <- function() {
  f <- Filter(function(p) !is_artifact(p), md_files(include_archive = FALSE))
  n <- 0
  for (p in f) {
    fm <- read_front_matter(p); if (is.null(fm)) next
    if (!identical(fm_field(fm, 'status'), 'open')) next
    if (!identical(fm_field(fm, 'role'), 'notes')) next
    w <- fm_field(fm, 'workstream')
    if (is.na(w) || !w %in% PLAN_WORKSTREAMS) {
      add(5, p, fm_line(fm, 'workstream'),
          sprintf('status: open but workstream `%s` owns no plan', w))
      next
    }
    n <- n + 1
    plan <- sprintf('research/%s/plan.md', w)
    if (!file.exists(plan)) { add(5, p, 1, sprintf('%s does not exist', plan)); next }
    if (!any(grepl(basename(p), readLines(plan, warn = FALSE), fixed = TRUE))) {
      add(5, p, fm_line(fm, 'status'),
          sprintf('status: open but not cited from %s', plan))
    }
  }
  note_covered(5, n, 'open notes')
}

#-------------------------------------------------------------------------------
# 6. Cited paths resolve
#
# The check that found `pub5785_hazard.csv` -- a file the design memo claimed
# was in the repo and never was. A citation can fail to resolve for three
# reasons and only one is a bug: it moved (bug), it is in another repo, or it
# does not exist yet. The last two go in the allowlist, which turns a silent
# unknown into a reviewed decision.
#-------------------------------------------------------------------------------
check_06 <- function() {
  allow <- read_external_allowlist()
  f <- md_files(include_archive = FALSE)
  n <- 0
  for (p in f) {
    for (c in citations(p)) {
      # the pre-reorganization tree is check 8's business, not this one
      if (startsWith(c$path, 'other/state_tax_research')) next
      n <- n + 1
      if (!file.exists(c$path) && !(c$path %in% allow)) {
        add(6, p, c$line,
            sprintf('cited path does not exist: `%s` (if intentional, add it to %s)',
                    c$path, EXTERNAL_PATHS_FILE))
      }
    }
  }
  note_covered(6, n, 'citations')
}

#-------------------------------------------------------------------------------
# 7. `sot:` and `supersedes:` targets exist
#-------------------------------------------------------------------------------
check_07 <- function() {
  f <- Filter(function(p) !is_artifact(p), md_files(include_archive = FALSE))
  n <- 0
  for (p in f) {
    fm <- read_front_matter(p); if (is.null(fm)) next
    sot <- fm_field(fm, 'sot')
    if (!is.na(sot) && sot != 'self') {
      n <- n + 1
      if (!file.exists(sot)) {
        add(7, p, fm_line(fm, 'sot'), sprintf('sot: `%s` does not exist', sot))
      }
    }
    for (t in as.character(fm$values$supersedes)) {
      if (!nzchar(t)) next
      n <- n + 1
      if (!file.exists(t)) {
        add(7, p, fm_line(fm, 'supersedes'),
            sprintf('supersedes: `%s` does not exist', t))
      }
    }
  }
  note_covered(7, n, 'sot/supersedes targets')
}

#-------------------------------------------------------------------------------
# 8. Nothing outside archive/ cites the pre-reorganization locations
#
# Bare directory mentions in prose are fine and expected (decisions_log.md
# explains why the tree moved); a path with a file extension is a citation.
#-------------------------------------------------------------------------------
check_08 <- function() {
  f <- md_files(include_archive = FALSE)
  re <- sprintf('other/state_tax_research/[A-Za-z0-9_./-]+\\.(?:%s)\\b', CITE_EXT)
  n <- 0
  for (p in f) {
    lines <- readLines(p, warn = FALSE)
    for (i in seq_along(lines)) {
      m <- regmatches(lines[i], gregexpr(re, lines[i], perl = TRUE))[[1]]
      for (hit in m) {
        n <- n + 1
        add(8, p, i, sprintf('cites the pre-2026-08-19 location: `%s`', hit))
      }
    }
  }
  note_covered(8, length(f), 'files scanned')
}

#-------------------------------------------------------------------------------
# 9. Archive naming, and archive/README.md coverage
#-------------------------------------------------------------------------------
check_09 <- function() {
  files <- setdiff(list.files('research/archive'), 'README.md')
  readme <- 'research/archive/README.md'
  rl <- if (file.exists(readme)) readLines(readme, warn = FALSE) else character(0)
  re <- sprintf('^.+_[0-9]{4}-[0-9]{2}-[0-9]{2}_%s\\.[A-Za-z]+$', ARCHIVE_REASON_RE)
  for (f in files) {
    if (!grepl(re, f)) {
      add(9, file.path('research/archive', f), 1,
          'name does not match {basename}_{YYYY-MM-DD}_{reason}.{ext}')
    }
    if (!any(grepl(f, rl, fixed = TRUE))) {
      add(9, readme, 1, sprintf('no entry for `%s`', f))
    }
  }
  note_covered(9, length(files), 'archived files')
}

#-------------------------------------------------------------------------------
# 10. No living document claims to be superseded
#
# The archive direction is deliberately not checked: a snapshot archived as
# `executed` was completed, not replaced, so it has nothing to point at.
#-------------------------------------------------------------------------------
check_10 <- function() {
  f <- Filter(function(p) !is_artifact(p), md_files(include_archive = FALSE))
  n <- 0
  for (p in f) {
    fm <- read_front_matter(p); if (is.null(fm)) next
    n <- n + 1
    raw <- if ('superseded_by' %in% names(fm$raw)) trimws(fm$raw[['superseded_by']]) else NA
    if (!is.na(raw) && nzchar(raw) && raw != 'null') {
      add(10, p, fm_line(fm, 'superseded_by'),
          sprintf('living document declares superseded_by: %s -- archive it instead', raw))
    }
    if (identical(fm_field(fm, 'status'), 'superseded')) {
      add(10, p, fm_line(fm, 'status'),
          'status: superseded on a document still in the live tree')
    }
  }
  note_covered(10, n, 'living documents')
}

#-------------------------------------------------------------------------------
# Self-test: plant one violation per check and confirm the check catches it.
#
# A checker that has only ever been run on a clean tree proves nothing -- green
# is indistinguishable from broken. Every mutation below is undone by on.exit(),
# including on error, so an interrupted run cannot leave the tree modified.
#-------------------------------------------------------------------------------

selftest <- function() {
  tmp  <- 'research/state_weights/notes/_selftest_tmp.md'
  arc  <- 'research/archive/_selftest_badname.md'
  real <- 'research/state_weights/nonfiler_residual/04_findings.md'
  orig <- readLines(real, warn = FALSE)
  on.exit({ unlink(c(tmp, arc)); writeLines(orig, real) }, add = TRUE)

  doc <- function(role = 'notes', status = 'current',
                  sot = 'research/state_weights/plan.md',
                  superseded_by = 'null', body = '') {
    sprintf(paste0('---\ntitle: "selftest"\nrole: %s\nworkstream: state_weights\n',
                   'status: %s\nupdated: 2099-01-01\nsot: %s\nsupersedes: []\n',
                   'superseded_by: %s\n---\n%s\n'),
            role, status, sot, superseded_by, body)
  }
  plant_doc <- function(...) { txt <- doc(...); function() writeLines(txt, tmp) }

  cases <- list(
    list(1,  'a second role: plan in one workstream',   plant_doc(role = 'plan')),
    list(2,  'document with no front matter',           function() writeLines('# none', tmp)),
    list(3,  'role: outside the closed vocabulary',     plant_doc(role = 'memo')),
    list(4,  'updated: behind the last commit date',
         function() writeLines(sub('^updated: .*$', 'updated: 2000-01-01', orig), real)),
    list(5,  'status: open note not cited from a plan', plant_doc(status = 'open')),
    list(6,  'citation to a nonexistent path',
         plant_doc(body = 'see `research/state_weights/nope_missing.md`')),
    list(7,  'sot: target does not exist',              plant_doc(sot = 'research/nope.md')),
    list(8,  'cites a pre-2026-08-19 location',
         plant_doc(body = 'see `other/state_tax_research/nonfiler_residual/04_findings.md`')),
    list(9,  'archive file with a non-conforming name', function() writeLines('x', arc)),
    list(10, 'living document declares superseded_by',
         plant_doc(superseded_by = 'research/state_weights/plan.md'))
  )

  cat('check_conventions.R — self-test\n')
  cat(strrep('-', 72), '\n', sep = '')
  fired <- 0
  for (cs in cases) {
    id <- cs[[1]]; label <- cs[[2]]; plant <- cs[[3]]
    findings$rows <- list()
    plant()
    CHECKS[[as.character(id)]]$fn()
    caught <- length(findings$rows) > 0
    fired <- fired + caught
    cat(sprintf('%-6s %-2s. %s\n', if (caught) 'fired' else 'MISSED', id, label))
    unlink(c(tmp, arc)); writeLines(orig, real)
  }
  cat(strrep('-', 72), '\n', sep = '')
  cat(sprintf('%d/%d checks caught a planted violation\n', fired, length(cases)))
  quit(status = if (fired == length(cases)) 0L else 1L)
}

#-------------------------------------------------------------------------------
# Run
#-------------------------------------------------------------------------------

CHECKS <- list(
  `1`  = list(fn = check_01, name = 'one role: plan per workstream'),
  `2`  = list(fn = check_02, name = 'front matter present on documents'),
  `3`  = list(fn = check_03, name = 'role/status/workstream vocabularies'),
  `4`  = list(fn = check_04, name = 'updated: not behind last commit'),
  `5`  = list(fn = check_05, name = 'open notes cited from their plan'),
  `6`  = list(fn = check_06, name = 'cited paths resolve'),
  `7`  = list(fn = check_07, name = 'sot/supersedes targets exist'),
  `8`  = list(fn = check_08, name = 'no pre-reorganization citations'),
  `9`  = list(fn = check_09, name = 'archive naming and README coverage'),
  `10` = list(fn = check_10, name = 'no living document is superseded')
)

if ('--selftest' %in% args) selftest()

run <- if (length(only_checks)) as.character(only_checks) else names(CHECKS)
for (id in run) {
  if (is.null(CHECKS[[id]])) { message('no such check: ', id); next }
  CHECKS[[id]]$fn()
}

rows <- findings$rows
by_check <- split(rows, vapply(rows, function(r) as.character(r$check), character(1)))

cat('research/CONVENTIONS.md — tree check\n')
cat(strrep('-', 72), '\n', sep = '')
for (id in run) {
  hits <- by_check[[id]]
  mark <- if (is.null(hits)) 'ok  ' else sprintf('FAIL')
  cat(sprintf('%-4s %-2s. %-42s %s\n', mark, id, CHECKS[[id]]$name,
              if (is.null(hits)) '' else sprintf('%d finding%s', length(hits),
                                                 if (length(hits) == 1) '' else 's')))
  if (verbose && !is.null(covered$n[[id]])) {
    cat(sprintf('        covered: %s\n', covered$n[[id]]))
  }
  for (h in hits) cat(sprintf('        %s:%d  %s\n', h$path, h$line, h$msg))
}
cat(strrep('-', 72), '\n', sep = '')

n_fail <- length(unique(vapply(rows, function(r) r$check, numeric(1))))
if (!length(rows)) {
  cat('PASS — no findings\n')
} else {
  cat(sprintf('%d finding%s across %d check%s\n', length(rows),
              if (length(rows) == 1) '' else 's', n_fail,
              if (n_fail == 1) '' else 's'))
}
quit(status = if (length(rows) && !report_only) 1L else 0L)
