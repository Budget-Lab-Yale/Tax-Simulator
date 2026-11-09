# Dry-run the rewritten re-pinner, then confirm it changes ONLY hash values.
suppressPackageStartupMessages({library(tidyverse); library(yaml)})
walk(list.files('src/misc', pattern = '[.]R$', full.names = TRUE), source)
walk(list.files('src/sim', pattern = '[.]R$', full.names = TRUE), function(f) try(source(f), silent = TRUE))

cat('=== DRY RUN ===\n')
dry = config_repin_hashes('economy', dry_run = TRUE)

before = list.files('config/scenarios/economy/default', full.names = TRUE) %>%
  set_names(basename(.)) %>% map(~ readLines(.x, warn = FALSE))

cat('\n=== LIVE ===\n')
res = config_repin_hashes('economy')

after = list.files('config/scenarios/economy/default', full.names = TRUE) %>%
  set_names(basename(.)) %>% map(~ readLines(.x, warn = FALSE))

cat('\n=== only hash lines changed? ===\n')
ok = TRUE
for (nm in names(before)) {
  b = before[[nm]]; a = after[[nm]]
  if (length(b) != length(a)) { cat('LINE COUNT CHANGED:', nm, '\n'); ok = FALSE; next }
  d = which(b != a)
  for (i in d) {
    # every differing line must be a hash line whose only change is the hex value
    if (!grepl('^\\s+\\S+:\\s*[0-9a-f]{32}\\s*$', b[i]) ||
        !identical(sub(':\\s*[0-9a-f]{32}\\s*$', '', b[i]),
                   sub(':\\s*[0-9a-f]{32}\\s*$', '', a[i]))) {
      cat('NON-HASH CHANGE in', nm, 'line', i, '\n  old:', b[i], '\n  new:', a[i], '\n')
      ok = FALSE
    }
  }
  if (length(d)) cat(sprintf('%-22s %d hash line(s) changed\n', nm, length(d)))
}
cat('\ncomment/structure preserved:', ok, '\n')
cat('dry run and live agree:', identical(nrow(dry), nrow(res)), '\n')
if (!ok) quit(status = 1)
cat('REPIN_TEST_PASS\n')
