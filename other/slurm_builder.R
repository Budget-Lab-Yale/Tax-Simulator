
runscript_name   = "policy_runs/tcja/simulator/interactive_simulator_runs"
scenario_id      = "baseline"
user_id          = "jmk263"
local            = 1
vintage          = NULL
pct_sample       = 1
stacked          = 0
baseline_vintage = NULL

start = Sys.time()
source("src/main.R")
end = Sys.time()

runtime = as.numeric(end - start)

stamp = paste0(lubridate::year(end), 
               lubridate::month(end) %>%
                 paste0('0', .) %>% 
                 str_sub(-2), 
               lubridate::day(end) %>%
                 paste0('0', .) %>% 
                 str_sub(-2), 
               lubridate::hour(end) %>%
                 paste0('0', .) %>% 
                 str_sub(-2))


scripts = read_csv("config/runscripts/policy_runs/tcja/simulator/interactive_simulator_runs.csv") %>%
  select(ID)

batch_path = file.path("config/batch-submissions", stamp)

if(!file.exists(batch_path)){
  dir.create(batch_path)
}

dir.create(file.path(batch_path, "output"))
write_csv(scripts, file = file.path(batch_path, "batch_array.txt"), col_names = F)
batch_path = file.path(batch_path, paste0("batchjob-",stamp,".sh"))
file.create(batch_path)

cat(paste0('#!/bin/bash',
           '\n#SBATCH --array=1-200', #nrow(scripts)/2,
           '\n#SBATCH --job-name batch-',stamp,
           #'\n#SBATCH --ntasks',
           '\n#SBATCH --output=/gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/config/batch-submissions/',stamp,'/output/slurm-%A_%a.out',
           '\n#SBATCH --error=/gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/config/batch-submissions/',stamp,'/output/slurm-%A_%a.err',
           '\n#SBATCH --mem-per-cpu 10g \n#SBATCH --time=1:00:00', 
           '\n#SBATCH --partition scavenge \n#SBATCH --requeue',
           '\nmodule load miniconda \nconda activate ybl-rbash',
           '\nfile=/gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/config/batch-submissions/',stamp,'/batch_array.txt',
           '\nfor i in 0 1; do\n  index=$((2*SLURM_ARRAY_TASK_ID + i))',
           '\n  scenario_id=$(awk "NR=="${index}"{print}" $file)',
           '\n  Rscript /gpfs/gibbs/project/sarin/jmk263/Repositories/Tax-Simulator/src/main.R ', runscript_name, ' "${scenario_id}" ', user_id, ' ',
           local, ' ', stamp, ' ', pct_sample,' ', stacked, ' ', stamp,
           '\ndone'
           ),
    file = batch_path,
    append = T
    )



