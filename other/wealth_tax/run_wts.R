#-------------------------------------------------------------------------------
# run_wts.R — run the standalone Wealth-Tax-Simulator for cross-model comparison.
# Bypasses its main.R so we can redirect output to scratch (not shared prod) and
# limit the year range. Produces baseline + nickel_dime detail/totals.
#-------------------------------------------------------------------------------

setwd('/nfs/roberts/project/pi_nrs36/jar335/Repositories/Wealth-Tax-Simulator')

suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(data.table)
  library(Hmisc)
})

# Globals consumed by config.R / sim.R (normally set in WTS main.R — use plain
# `=` at top level exactly as main.R does; `<<-` hit a locked binding)
scenario_ids    = c('baseline', 'nickel_dime')
years           = 2025:2030
output_root     = '/nfs/roberts/scratch/pi_nrs36/jar335/wts_compare'
write_microdata = TRUE

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

source('./src/config.R')   # parses scenarios, creates timestamped output folder
source('./src/data.R')
source('./src/calc.R')
source('./src/sim.R')

walk(scenario_ids, run_simulation)

cat('WTS_DONE time_stamp=', time_stamp, '\n', sep = '')
cat('WTS output root: ', file.path(output_root, time_stamp), '\n', sep = '')
