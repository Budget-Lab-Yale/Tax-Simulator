#!/bin/bash
#SBATCH --array=1-200
#SBATCH --job-name batch-2024013008
#SBATCH --output=/gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/config/batch-submissions/2024013008/output/slurm-%A_%a.out
#SBATCH --error=/gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/config/batch-submissions/2024013008/output/slurm-%A_%a.err
#SBATCH --mem-per-cpu 10g 
#SBATCH --time=1:00:00
#SBATCH --partition scavenge 
#SBATCH --requeue
module load miniconda 
conda activate ybl-rbash
file=/gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/config/batch-submissions/2024013008/batch_array.txt
for i in 0 1; do
  index=$((2*SLURM_ARRAY_TASK_ID + i))
  scenario_id=$(awk "NR=="${index}"{print}" $file)
  Rscript /gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/src/main.R policy_runs/tcja/simulator/interactive_simulator_runs "${scenario_id}" jmk263 1 2024013008 1 0 2024013008
done