#!/bin/bash
#SBATCH --job-name=cfg_unit_tests
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/config_redesign/logs/unit_tests_%j.log

# Runs the scenario_config engine unit tests (and any other test scripts
# passed as arguments) in a given tree.
# Usage: sbatch run_tests.sbatch [tree_dir] [test_script ...]
TREE="${1:-/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator}"
shift || true
cd "$TREE"
module load R/4.4.1-foss-2022b

if [ "$#" -eq 0 ]; then
  set -- other/config_redesign/test_scenario_config.R
fi

rc=0
for t in "$@"; do
  echo "=== $t ==="
  Rscript "$t" || rc=1
done
echo "EXIT=$rc"
exit $rc
