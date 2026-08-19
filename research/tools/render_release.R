#!/usr/bin/env Rscript
#-------------------------------------------------------------------------------
# render_release.R
#
# Renders one or more research Markdown sources into a single date-stamped Word
# document under research/releases/. Markdown is the source of truth; the .docx
# is a committed point-in-time snapshot for outside review and is never edited
# back -- comments come back as edits to the Markdown and a new release.
#
# Run from the repo root:
#   Rscript research/tools/render_release.R <slug> [--date YYYY-MM-DD] [--dry-run]
#
# <slug> names a manifest at research/tools/releases/<slug>.yaml:
#
#   title: "..."                      # Word Title style
#   subtitle: "..."                    # Word Subtitle style
#   reference_doc: research/tools/reference.docx   # optional styling template
#   sections:
#     - heading: "Where the work stands"
#       source: research/STATUS.md
#       fence: nonfiler-status         # optional: extract only a fenced region
#
# To release part of a document, wrap the region in HTML comments, which pandoc
# drops from the .docx:
#   <!-- release:begin nonfiler-status -->  ...  <!-- release:end nonfiler-status -->
#
# Requires pandoc on PATH (3.x). Uses pandoc directly rather than rmarkdown:
# combining several sources needs an intermediate file either way, and this
# avoids per-document `output:` YAML blocks -- the mechanism that let two
# undated renders go stale beside their sources.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(yaml) })

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop('usage: render_release.R <slug> [--date YYYY-MM-DD] [--dry-run]')
}
if (!file.exists('./src/main.R')) stop('Run from the Tax-Simulator repo root')

slug <- args[1]
get_flag <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
render_date <- get_flag('--date', format(Sys.Date(), '%Y-%m-%d'))
dry_run     <- '--dry-run' %in% args

manifest_path <- file.path('research/tools/releases', paste0(slug, '.yaml'))
if (!file.exists(manifest_path)) stop('No manifest at ', manifest_path)
man <- yaml::read_yaml(manifest_path)

reference_doc <- if (is.null(man$reference_doc)) 'research/tools/reference.docx' else man$reference_doc


#-----------------------------------------------------------------------------
# Provenance: what this release was made from
#-----------------------------------------------------------------------------

git_out <- function(...) {
  out <- suppressWarnings(system2('git', c(...), stdout = TRUE, stderr = FALSE))
  if (!length(out)) '' else out
}
sha      <- git_out('rev-parse', '--short', 'HEAD')
branch   <- git_out('rev-parse', '--abbrev-ref', 'HEAD')
# Sources only: releases/ is this script's own output, so including it would make
# every render report a dirty tree. Test with any(nzchar()), not length(): git_out
# returns '' rather than character(0) when a command prints nothing.
dirty <- any(nzchar(git_out('status', '--porcelain', '--',
                           'research', ':(exclude)research/releases')))

front_matter_field <- function(path, field) {
  ln <- readLines(path, warn = FALSE, n = 40)
  if (!length(ln) || !grepl('^---\\s*$', ln[1])) return('--')
  close_at <- which(grepl('^---\\s*$', ln))[2]
  if (is.na(close_at)) return('--')
  hit <- grep(paste0('^', field, ':'), ln[2:(close_at - 1)], value = TRUE)
  if (!length(hit)) '--' else trimws(sub('^[^:]+:', '', hit[1]))
}

last_commit_date <- function(path) {
  out <- git_out('log', '-1', '--format=%ad', '--date=short', '--', path)
  if (nzchar(out[1])) out[1] else 'uncommitted'
}


#-----------------------------------------------------------------------------
# Read one section: strip front matter, optionally fence-extract, demote headings
#-----------------------------------------------------------------------------

read_section <- function(source, fence = NULL, shift = 1L) {

  if (!file.exists(source)) stop('Manifest source does not exist: ', source)
  ln <- readLines(source, warn = FALSE)

  # Strip a leading front-matter block; it is metadata, not body text
  if (length(ln) && grepl('^---\\s*$', ln[1])) {
    close_at <- which(grepl('^---\\s*$', ln))[2]
    if (!is.na(close_at)) ln <- ln[(close_at + 1):length(ln)]
  }

  if (!is.null(fence)) {
    open_at  <- grep(sprintf('<!--\\s*release:begin\\s+%s\\s*-->', fence), ln)
    close_at <- grep(sprintf('<!--\\s*release:end\\s+%s\\s*-->',   fence), ln)
    if (!length(open_at) || !length(close_at)) {
      stop('release fence "', fence, '" not found in ', source)
    }
    ln <- ln[(open_at[1] + 1):(close_at[1] - 1)]
  }

  # Demote ATX headings so the source nests under its release heading. Track
  # fenced code blocks so a '#' comment inside one is left alone.
  in_code <- FALSE
  for (i in seq_along(ln)) {
    if (grepl('^\\s*(```|~~~)', ln[i])) {
      in_code <- !in_code
    } else if (!in_code && grepl('^#{1,5} ', ln[i])) {
      ln[i] <- paste0(strrep('#', shift), ln[i])
    }
  }
  ln
}


#-----------------------------------------------------------------------------
# Assemble
#-----------------------------------------------------------------------------

source_row <- function(s) {
  sprintf('| `%s` | %s | %s | %s |',
          s$source,
          front_matter_field(s$source, 'role'),
          front_matter_field(s$source, 'updated'),
          last_commit_date(s$source))
}

about <- c(
  '# About this document',
  '',
  '| | |',
  '|---|---|',
  sprintf('| Rendered | %s |', render_date),
  sprintf('| Repository | Tax-Simulator, branch `%s`, commit `%s`%s |',
          branch, sha,
          if (dirty) ' **(plus uncommitted edits under research/)**' else ''),
  sprintf('| Regenerate with | `Rscript research/tools/render_release.R %s` |', slug),
  '',
  'Assembled from the Markdown sources below, which are the source of truth.',
  'This Word file is a point-in-time snapshot: comments on it are welcome, but',
  'edits are not carried back -- they are applied to the Markdown and a new',
  'release is cut.',
  '',
  '| Source | Role | `updated:` | Last commit |',
  '|---|---|---|---|',
  vapply(man$sections, source_row, character(1)),
  ''
)

body <- unlist(lapply(man$sections, function(s) {
  c('', paste('#', s$heading), '', read_section(s$source, s$fence, 1L), '')
}))

tmp <- file.path(tempdir(), paste0(slug, '.md'))
writeLines(c(about, body), tmp, useBytes = TRUE)   # useBytes: no BOM

out_dir <- 'research/releases'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(out_dir, sprintf('%s_%s.docx', render_date, slug))

# Quote anything that can contain a space. system2() does NOT quote arguments on
# Windows, so an unquoted `-M title=A state-weight-inclusive ...` reaches pandoc as
# several arguments and the second becomes a filename.
q <- function(x) shQuote(x, type = if (.Platform$OS.type == 'windows') 'cmd' else 'sh')

pandoc_args <- c(
  q(tmp),
  '--from=markdown', '--to=docx',
  if (file.exists(reference_doc)) sprintf('--reference-doc=%s', q(reference_doc)),
  '--toc', '--toc-depth=3',
  '-M', q(paste0('title=',    man$title)),
  '-M', q(paste0('subtitle=', man$subtitle)),
  '-M', q(sprintf('date=%s - %s @ %s', render_date, branch, sha)),
  '--resource-path=.:research',
  sprintf('--output=%s', q(out))
)

if (dry_run) {
  cat('intermediate:', tmp, '\n')
  cat('lines        :', length(c(about, body)), '\n')
  cat('pandoc', paste(pandoc_args, collapse = ' '), '\n')
  quit(save = 'no', status = 0)
}

status <- system2('pandoc', pandoc_args)
if (status != 0L) stop('pandoc failed with status ', status)
message('Wrote ', out)
