
scripts = read_csv("config/runscripts/policy_runs/tcja/simulator/interactive_simulator_runs.csv") %>%
  select(ID) %>%
  unlist()

runscript_name   = "policy_runs/tcja/simulator/interactive_simulator_runs"
scenario_id      = "baseline"
user_id          = "jmk263"
local            = 1
vintage          = NULL
pct_sample       = 1
baseline_vintage = NULL
parsed           = T

start = Sys.time()

source('src/main.R')

end = Sys.time()

stamp = paste0(lubridate::year(start), 
               lubridate::month(start) %>%
                 paste0('0', .) %>% 
                 str_sub(-2), 
               lubridate::day(start) %>%
                 paste0('0', .) %>% 
                 str_sub(-2), 
               lubridate::hour(start) %>%
                 paste0('0', .) %>% 
                 str_sub(-2))

runtime_m = as.numeric(end - start) * 2

wide = ceiling(15 / runtime_m)

batch_path = file.path("config/batch-submissions", stamp)

if(!file.exists(batch_path)){
  dir.create(batch_path)
  batch_path = file.path(batch_path, "batchlist.txt")
  file.create(batch_path)
}

index = 1
line = "module load miniconda; conda activate ybl-rbash; "
wide_l = 0

while(index <= length(scripts)) {
  line = paste0(line, "Rscript src/main.R", " policy_runs/tcja/simulator/interactive_simulator_runs.csv ", scripts[index], " jmk263", " 1 ", stamp, " 1", " 0 ", stamp, "; ") 
  index = index + 1
  wide_l = wide_l + 1

  if(wide_l == wide) {
    write(line, file = batch_path, append = TRUE)
    wide_l = 0
    line = "module load miniconda; conda activate ybl-rbash; "
  }
  else if(index > length(scripts)) {
    write(line, file = batch_path, append = TRUE) 
  }
}

