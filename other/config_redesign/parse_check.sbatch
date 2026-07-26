#!/bin/bash
#SBATCH --job-name=cfg_parse_check
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:10:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/config_redesign/logs/parse_check_%j.log

# Parses and sources every src/ file (no simulation) in a given tree, then
# exercises the shared CLI parser's contract (new arity, retired-user_id
# error). Usage: sbatch parse_check.sbatch [tree_dir]
cd "${1:-/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator}"
module load R/4.4.1-foss-2022b

Rscript -e '
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines("./requirements.txt"), library, character.only = TRUE)
  ))
)

files = list.files("./src", recursive = TRUE, pattern = "[.]R$")
files = files[files != "main.R" & !startsWith(files, "slurm/") & !startsWith(files, "tests/")]

for (f in files) {
  out = tryCatch({ parse(file.path("./src", f)); "ok" },
                 error = function(e) paste("PARSE ERROR:", conditionMessage(e)))
  if (out != "ok") cat(sprintf("%-55s %s\n", f, out))
}
cat("parsed", length(files), "files\n")

return_vars = list()
for (f in files) {
  out = tryCatch({ source(file.path("./src", f)); "ok" },
                 error = function(e) paste("SOURCE ERROR:", conditionMessage(e)))
  if (out != "ok") cat(sprintf("%-55s %s\n", f, out))
}
cat("sourced", length(files), "files\n")

# CLI parser contract
ok = function(x) if (isTRUE(x)) "pass" else "FAIL"
new_main  = c("baseline/baseline", "NULL", "1", "NULL", "0.05", "1", "NULL", "0", "none")
new_setup = new_main[1:8]
old_main  = append(new_main, "user_test", after = 2)

r1 = tryCatch(parse_cli_args(new_main, "main"),        error = function(e) e)
r2 = tryCatch(parse_cli_args(new_setup, "slurm_setup"), error = function(e) e)
r3 = tryCatch(parse_cli_args(old_main, "main"),         error = function(e) e)

cat("main 9-arg parse:     ", ok(is.list(r1) && r1$pct_sample == 0.05 && r1$multicore == "none"), "\n")
cat("setup 8-arg parse:    ", ok(is.list(r2) && is.null(r2$scenario_id) && r2$multicore == "none"), "\n")
cat("old-arity user_id err:", ok(inherits(r3, "error") && grepl("user_id", conditionMessage(r3))), "\n")
cat("PARSE_CHECK_DONE\n")
'
echo "EXIT=$?"
