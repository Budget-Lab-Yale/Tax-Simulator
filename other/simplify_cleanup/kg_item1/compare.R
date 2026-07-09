#-------------------------------------------------------------------------------
# compare.R  --  pre/post output equivalence check for item #1 + #9
#
# Usage: Rscript compare.R [pre_vintage] [post_vintage]   (defaults kg_sr_pre/post)
#
# Walks the two output vintages, confirms the same file set, then classifies
# every common file:
#   - .rds  : DESERIALIZED content via identical() (robust to gzip framing)
#   - .xlsx : unzip + member-wise byte compare. openxlsx embeds a creation
#             timestamp in docProps/core.xml, so a file whose ONLY differing
#             member is that timestamp is 'timestamp-only' (spurious), not a
#             data difference. Any other member diff is 'genuine'.
#   - other : byte-for-byte via md5sum, with a vintage-name-neutralized recheck
#             for text files (spurious path-string diff vs genuine).
#-------------------------------------------------------------------------------

.a    = commandArgs(trailingOnly = TRUE)
.root = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
pre   = file.path(.root, if (length(.a) >= 1) .a[1] else 'kg_sr_pre')
post  = file.path(.root, if (length(.a) >= 2) .a[2] else 'kg_sr_post')
.vp   = basename(pre); .vq = basename(post)
cat('PRE :', pre, '\nPOST:', post, '\n\n')

rel = function(root) sort(list.files(root, recursive = TRUE, all.files = TRUE,
                                     no.. = TRUE))
pf = rel(pre); qf = rel(post)
only_pre = setdiff(pf, qf); only_post = setdiff(qf, pf); common = intersect(pf, qf)

cat('files in pre :', length(pf), '\n')
cat('files in post:', length(qf), '\n')
cat('only in pre  :', length(only_pre), '\n'); if (length(only_pre))  cat(paste0('   - ', only_pre), sep = '\n')
cat('only in post :', length(only_post), '\n'); if (length(only_post)) cat(paste0('   - ', only_post), sep = '\n')

# ---- xlsx member-wise classifier ----
classify_xlsx = function(a, b) {
  da = file.path(tempdir(), 'cmp_xa'); db = file.path(tempdir(), 'cmp_xb')
  unlink(da, recursive = TRUE); unlink(db, recursive = TRUE)
  ea = tryCatch(unzip(a, exdir = da), error = function(e) NULL)
  eb = tryCatch(unzip(b, exdir = db), error = function(e) NULL)
  if (is.null(ea) || is.null(eb)) return('genuine (unzip failed)')
  fa = sort(list.files(da, recursive = TRUE)); fb = sort(list.files(db, recursive = TRUE))
  if (!identical(fa, fb)) return('genuine (member set differs)')
  diffm = fa[tools::md5sum(file.path(da, fa)) != tools::md5sum(file.path(db, fb))]
  if (length(diffm) == 0) return('identical')
  if (identical(diffm, 'docProps/core.xml')) {
    strip = function(p) gsub('<dcterms:created[^<]*</dcterms:created>', '',
                             paste(readLines(p, warn = FALSE), collapse = ''))
    if (strip(file.path(da, 'docProps/core.xml')) == strip(file.path(db, 'docProps/core.xml')))
      return('timestamp-only')
  }
  paste0('genuine (members: ', paste(diffm, collapse = ', '), ')')
}

rds_bad = character(0); xlsx_ts = character(0); xlsx_bad = character(0)
csv_bad = character(0); other_bad = character(0)
for (f in common) {
  a = file.path(pre, f); b = file.path(post, f)
  if (grepl('[.]rds$', f, ignore.case = TRUE)) {
    ok = tryCatch(identical(readRDS(a), readRDS(b)), error = function(e) NA)
    if (is.na(ok) || !ok) rds_bad = c(rds_bad, f)
  } else if (grepl('[.]xlsx$', f, ignore.case = TRUE)) {
    cls = classify_xlsx(a, b)
    if (cls == 'timestamp-only') xlsx_ts = c(xlsx_ts, f)
    else if (cls != 'identical')  xlsx_bad = c(xlsx_bad, paste0(f, '  [', cls, ']'))
  } else {
    ma = tools::md5sum(a); mb = tools::md5sum(b)
    if (is.na(ma) || is.na(mb) || ma != mb) {
      # neutralize vintage name for text files; guard against binary content
      neutral = tryCatch({
        ta = readLines(a, warn = FALSE); tb = readLines(b, warn = FALSE)
        identical(gsub(.vp, 'V', ta, fixed = TRUE), gsub(.vq, 'V', tb, fixed = TRUE))
      }, error = function(e) FALSE, warning = function(e) FALSE)
      if (isTRUE(neutral)) other_bad = c(other_bad, paste0(f, '  [vintage-name only]'))
      else                 csv_bad   = c(csv_bad, f)
    }
  }
}

cat('\n---- classification of common files ----\n')
cat('rds content mismatches         :', length(rds_bad), '\n');  if (length(rds_bad))  cat(paste0('   * ', rds_bad),  sep = '\n')
cat('csv/data byte mismatches       :', length(csv_bad), '\n');  if (length(csv_bad))  cat(paste0('   * ', csv_bad),  sep = '\n')
cat('xlsx timestamp-only (spurious) :', length(xlsx_ts), '\n')
cat('xlsx GENUINE data mismatches   :', length(xlsx_bad), '\n'); if (length(xlsx_bad)) cat(paste0('   * ', xlsx_bad), sep = '\n')
cat('other text (vintage-name only) :', length(other_bad), '\n');if (length(other_bad))cat(paste0('   * ', other_bad),sep = '\n')

data_pass = length(only_pre) == 0 && length(only_post) == 0 &&
            length(rds_bad) == 0 && length(csv_bad) == 0 && length(xlsx_bad) == 0
cat('\n==== VERDICT ====\n')
cat('DATA-IDENTICAL (ignoring xlsx creation-timestamp & vintage-name strings):', data_pass, '\n')
cat('  spurious diffs -', length(xlsx_ts), 'xlsx timestamps,', length(other_bad), 'vintage-name text files\n')
cat('=================\n')
