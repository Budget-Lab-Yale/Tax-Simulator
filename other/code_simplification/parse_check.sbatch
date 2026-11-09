#!/bin/bash
#SBATCH --job-name=parse_check
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:10:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/code_simplification/parse_check_%j.log

# Parses and sources every src/ file (no simulation) to catch syntax errors and
# load-order problems after a refactor.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

Rscript -e '
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines("./requirements.txt"), library, character.only = TRUE)
  ))
)

files = list.files("./src", recursive = TRUE, pattern = "[.]R$")
files = files[files != "main.R" & !startsWith(files, "slurm/") & !startsWith(files, "tests/")]

# 1. Parse every file
for (f in files) {
  out = tryCatch({ parse(file.path("./src", f)); "ok" },
                 error = function(e) paste("PARSE ERROR:", conditionMessage(e)))
  if (out != "ok") cat(sprintf("%-55s %s\n", f, out))
}
cat("parsed", length(files), "files\n")

# 2. Source them all in the loader order main.R uses
return_vars = list()
for (f in files) {
  out = tryCatch({ source(file.path("./src", f)); "ok" },
                 error = function(e) paste("SOURCE ERROR:", conditionMessage(e)))
  if (out != "ok") cat(sprintf("%-55s %s\n", f, out))
}
cat("sourced", length(files), "files\n")

# 3. Confirm the new helpers exist with the expected signatures
for (fn in c("strip_calc_vars", "mtr_actuals", "calc_one_mtr", "run_mtr_block",
             "collect_totals", "write_detail", "fold_deemed",
             "check_raw_data_channel_compat")) {
  cat(sprintf("%-32s %s\n", fn,
              if (exists(fn) && is.function(get(fn))) "present" else "MISSING"))
}
cat("PARSE_CHECK_DONE\n")
'
echo "EXIT=$?"
