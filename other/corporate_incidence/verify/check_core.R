#-------------------------------------------------------------------------------
# check_core.R
#
# Verification for the corp_incidence.R pure core (build-order commit 2):
#   1. the whole src tree (incl. the new module) sources cleanly,
#   2. corp_selfcheck_paths() passes (permanent-constant mu, rent-share floor
#      decay, windowed expiry, priced-as-permanent corner, telescoping,
#      pre-enactment inertness),
#   3. the seesaw guard accepts a rate-like wedge and rejects a
#      depreciation-signature wedge (01_bonus shape),
#   4. corporate_meta.yaml validation: absent -> NULL; valid -> list;
#      invalid (net-of-offset / wrong provision type / bad horizon) -> stop.
#
# Run via sbatch (never on the login node):
#   sbatch other/corporate_incidence/verify/check_core.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)

return_vars <<- list()
list.files('./src', recursive = TRUE) %>%
  walk(.f = ~ {
    if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/')) {
      source(file.path('./src/', .x))
    }
  })
message('OK: src tree sourced (corp_incidence.R included)')

# --- 2. path-property self-checks --------------------------------------------
corp_selfcheck_paths()

# --- 3. seesaw guard ----------------------------------------------------------
rate_like = c(0, 0, 337, 350, 364, 378, 393, 409, 425, 442, 0.2, -0.1)
stopifnot(isTRUE(corp_seesaw_check(rate_like)$ok))

bonus_like = c(-61, -40, -12, 10, 35, 30, 22, 15, 8, 3)
stopifnot(isFALSE(corp_seesaw_check(bonus_like)$ok))
message('OK: seesaw guard (rate path accepted, depreciation signature rejected)')

# --- 4. metadata contract ------------------------------------------------------
tmp = file.path(tempdir(), 'ome_fixture')
dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

# absent -> NULL
stopifnot(is.null(corp_read_meta(tmp)))

# valid -> list
writeLines(c('gross_of_offset: true',
             'provision_type: rate',
             'beyond_horizon: zero',
             'produced_by: check_core fixture'),
           file.path(tmp, 'corporate_meta.yaml'))
m = corp_read_meta(tmp)
stopifnot(is.list(m), identical(m$beyond_horizon, 'zero'))

# invalid: declared net-of-offset -> hard stop
writeLines(c('gross_of_offset: false',
             'provision_type: rate',
             'beyond_horizon: extend'),
           file.path(tmp, 'corporate_meta.yaml'))
r = tryCatch({ corp_read_meta(tmp); 'no-error' }, error = function(e) 'error')
stopifnot(identical(r, 'error'))

# invalid: depreciation provision type -> hard stop
writeLines(c('gross_of_offset: true',
             'provision_type: cost_recovery',
             'beyond_horizon: extend'),
           file.path(tmp, 'corporate_meta.yaml'))
r = tryCatch({ corp_read_meta(tmp); 'no-error' }, error = function(e) 'error')
stopifnot(identical(r, 'error'))

# invalid: bad beyond_horizon -> hard stop
writeLines(c('gross_of_offset: true',
             'provision_type: rate',
             'beyond_horizon: forever'),
           file.path(tmp, 'corporate_meta.yaml'))
r = tryCatch({ corp_read_meta(tmp); 'no-error' }, error = function(e) 'error')
stopifnot(identical(r, 'error'))
message('OK: corporate_meta.yaml contract (absent -> NULL, valid -> list, ',
        'invalid -> hard stop)')

# --- 5. rollover ramp against the real schedule --------------------------------
roll = corp_rollover_ramp()
stopifnot(all(roll(c(-3, 0)) == 0),
          abs(roll(1) - 0.336) < 0.01,
          roll(50) == 1,
          !is.unsorted(roll(0:30)))
message('OK: debt rollover ramp (0 at enactment, ~0.34 at t+1, 1 at horizon)')

message('ALL CORE CHECKS PASSED')
