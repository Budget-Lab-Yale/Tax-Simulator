#!/bin/bash
#SBATCH --array=1-10000
#SBATCH --job-name batch-202403191726
#SBATCH --output=/gpfs/gibbs/project/sarin/jar335/Repositories/Tax-Simulator/config/batch-submissions/202403191726/output/slurm-%A_%a.out
#SBATCH --error=/gpfs/gibbs/project/sarin/jar335/Repositories/Tax-Simulator/config/batch-submissions/202403191726/output/slurm-%A_%a.err
#SBATCH --mem-per-cpu 10g 
#SBATCH --time=2:30:00
#SBATCH --partition scavenge 
#SBATCH --requeue
module load miniconda 
conda activate ybl-rbash
file=/gpfs/gibbs/project/sarin/jar335/Repositories/Tax-Simulator/config/batch-submissions/202403191726/batch_array.txt
for i in 0 1; do
  index=$((2*SLURM_ARRAY_TASK_ID + i))
  scenario_id=$(awk "NR=="${index}"{print}" $file)
  Rscript /gpfs/gibbs/project/sarin/jar335/Repositories/Tax-Simulator/src/main.R policy_runs/ctc/simulator/interactive_simulator_runs "${scenario_id}" jar335 1 202403191726 1 0 202403191726 1 0
done