
stamp = '202404080754'

runscript_name   = "policy_runs/ctc/simulator/interactive_simulator_runs"
scenario_id      = "baseline"
user_id          = "jar335"
local            = 1
vintage          = stamp
pct_sample       = 1
stacked          = 0
baseline_vintage = stamp
delete_detail    = 1
multicore        = 0

start = Sys.time()
source("src/main.R")
end = Sys.time()

runtime = as.numeric(end - start)

scripts = read_csv("config/runscripts/policy_runs/ctc/simulator/interactive_simulator_runs.csv") %>%
  select(ID)

batch_path = file.path("config/batch-submissions", stamp)

if(!file.exists(batch_path)){
  dir.create(batch_path)
}

dir.create(file.path(batch_path, "output"))
write_csv(scripts, file = file.path(batch_path, "batch_array.txt"), col_names = F)
splits = ceiling(nrow(scripts)/20000)


for(i in 1:splits){
  job_path = file.path(batch_path, paste0("batchjob-",stamp,"-",i,".sh"))
  file.create(job_path)
  
  cat(paste0('#!/bin/bash',
             '\n#SBATCH --array=',(10000*(i-1))+1,'-', ifelse(i==splits, nrow(scripts)/2, 10000*i),
             '\n#SBATCH --job-name batch-',stamp,
             '\n#SBATCH --output=/gpfs/gibbs/project/sarin/', user_id, '/Repositories/Tax-Simulator/config/batch-submissions/',stamp,'/output/slurm-%A_%a.out',
             '\n#SBATCH --error=/gpfs/gibbs/project/sarin/', user_id, '/Repositories/Tax-Simulator/config/batch-submissions/',stamp,'/output/slurm-%A_%a.err',
             '\n#SBATCH --mem-per-cpu 10g \n#SBATCH --time=',splits,':30:00', 
             '\n#SBATCH --partition scavenge \n#SBATCH --requeue',
             '\nmodule load miniconda \nconda activate ybl-rbash',
             '\nfile=/gpfs/gibbs/project/sarin/', user_id, '/Repositories/Tax-Simulator/config/batch-submissions/',stamp,'/batch_array.txt',
             '\nfor i in 0 1; do\n  index=$((2*SLURM_ARRAY_TASK_ID + i))',
             '\n  scenario_id=$(awk "NR=="${index}"{print}" $file)',
             '\n  Rscript /gpfs/gibbs/project/sarin/', user_id, '/Repositories/Tax-Simulator/src/main.R ', runscript_name, ' "${scenario_id}" ', user_id, ' ',
             local, ' ', stamp, ' ', pct_sample,' ', stacked, ' ', stamp,' 0 0',
             '\ndone'
  ),
  file = job_path,
  append = T
  )
  
} 




