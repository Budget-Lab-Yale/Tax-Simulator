#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 08_dependency_validation.R  (item 3 of the four JI approved 2026-08-28)
#
# Compare our dependency assignment against the survey's own, PERSON BY PERSON
# rather than count against count. Permitted, and only in this direction: the
# Census tax model's DEPSTAT is a verification benchmark and never an input
# (S12). `LINENO` was pulled precisely so its pointers resolve exactly.
#
# The question this answers: our constructed adult dependents run 16.3M against
# the survey recode's 13.8M and an administrative floor of 5.58M. A count
# difference cannot say whether we disagree with the recode about a few
# identifiable categories or about everyone a little. That decides whether the
# unit-rule changes in item 4 are worth making -- a diffuse disagreement would
# not be fixed by any rule change.
#
# Writes: results/dependency_confusion_{year}.csv (the 2x2 and its margins),
#         results/dependency_disagreement_{year}.csv (who we disagree about)
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/08_dependency_validation.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES   <- 'research/state_weights/nonfiler_pool/results'

RELATE_LABEL <- c(`101` = 'householder', `201` = 'spouse', `202` = 'spouse',
                  `203` = 'spouse', `301` = 'child', `303` = 'stepchild',
                  `501` = 'parent', `701` = 'sibling', `901` = 'grandchild',
                  `1001` = 'other relative', `1113` = 'partner/roommate',
                  `1114` = 'unmarried partner', `1115` = 'housemate',
                  `1116` = 'unmarried partner', `1117` = 'unmarried partner',
                  `1241` = 'roomer/boarder', `1242` = 'foster child',
                  `1260` = 'other nonrelative')

for (yr in YEARS) {
  message('=== TY', yr)
  st <- readRDS(file.path(RES, sprintf('units_%d.rds', yr)))
  p  <- st$persons[AGE >= 18]

  p[, ours   := is_dependent == TRUE]
  p[, theirs := DEPSTAT > 0]
  p[, rel := fifelse(is.na(RELATE_LABEL[as.character(RELATE)]), 'other',
                     RELATE_LABEL[as.character(RELATE)])]

  #---------------------------------------------------------------------------
  # The 2x2
  #---------------------------------------------------------------------------
  cm <- p[, .(persons_M = sum(ASECWT) / 1e6), keyby = .(ours, theirs)]
  tot <- cm[, sum(persons_M)]
  both  <- cm[ours == TRUE  & theirs == TRUE,  persons_M]
  us    <- cm[ours == TRUE  & theirs == FALSE, persons_M]
  them  <- cm[ours == FALSE & theirs == TRUE,  persons_M]
  neither <- cm[ours == FALSE & theirs == FALSE, persons_M]

  message(sprintf('  adults 18+: %.2fM', tot))
  message(sprintf('    both call dependent      %5.2fM', both))
  message(sprintf('    only WE do               %5.2fM', us))
  message(sprintf('    only the SURVEY does     %5.2fM', them))
  message(sprintf('    neither                  %5.2fM', neither))
  message(sprintf('  ours %.2fM vs theirs %.2fM (net %+.2fM) -- but %.2fM of ',
                  both + us, both + them, us - them, us + them),
          'disagreement underneath, ', sprintf('%.0f%% of the larger count',
                  100 * (us + them) / max(both + us, both + them)))
  agree <- (both + neither) / tot
  message(sprintf('  person-level agreement %.1f%%; of those either side calls ',
                  100 * agree),
          sprintf('a dependent, both agree on %.1f%%',
                  100 * both / (both + us + them)))

  fwrite(cm, file.path(RES, sprintf('dependency_confusion_%d.csv', yr)))

  #---------------------------------------------------------------------------
  # Where the disagreement lives -- the question item 4 turns on
  #---------------------------------------------------------------------------
  dis <- p[ours != theirs,
           .(persons_M = sum(ASECWT) / 1e6,
             median_age = as.integer(median(rep(AGE, pmax(1, round(ASECWT / 1e3))))),
             with_wages_M = sum(ASECWT * (INCWAGE > 0)) / 1e6),
           keyby = .(side = fifelse(ours, 'only ours', 'only survey'), rel)]
  dis <- dis[order(side, -persons_M)]
  message('  where the disagreement lives, by relationship to the householder:')
  for (i in seq_len(nrow(dis))) {
    if (dis$persons_M[i] < 0.15) next
    message(sprintf('    %-12s %-19s %5.2fM  median age %2d  with wages %4.2fM',
                    dis$side[i], dis$rel[i], dis$persons_M[i],
                    dis$median_age[i], dis$with_wages_M[i]))
  }
  fwrite(dis, file.path(RES, sprintf('dependency_disagreement_%d.csv', yr)))

  # The two rules item 4 proposes, sized against this: unrelated household
  # members we never test, and the household-vs-family support concept.
  unrel <- p[ours == FALSE & theirs == TRUE &
             rel %in% c('partner/roommate', 'unmarried partner', 'housemate',
                        'roomer/boarder', 'other nonrelative'),
             sum(ASECWT)]
  message(sprintf(paste('  of which UNRELATED household members the survey calls',
                        'dependents and our rule never tests: %.2fM'),
                  unrel / 1e6))
  message('  wrote dependency_confusion_', yr, '.csv, dependency_disagreement_', yr, '.csv')
}
