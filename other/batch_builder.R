
commandArgs = function() {
  return(c("policy_runs/tcja/simulator/10", "jmk263", "1", "NULL", "1", "0", "2024011614"))
}

commandArgs = function() {
  return(c("policy_runs/tcja/simulator/baseline", "jmk263", "1", "NULL", "1", "0", "NULL"))
}

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

scripts = list.files(path = "config/runscripts/policy_runs/tcja/simulator")
scripts = scripts[!grepl("baseline", scripts)]

# Calculate wall time based on jobs, cores, and partition
walltime = ((runtime_m * length(scripts)) / 4) * 1.25
walltime = signif(walltime, digits = -1)

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
  line = paste0(line, "Rscript src/main.R", " policy_runs/tcja/simulator/", str_sub(scripts[index], 1, -5), " jmk263", " 1 ", stamp, " 1", " 0 ", stamp, "; ") 
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

print("Enter the following commands in the grace terminal:")
print("module load dSQ")
print(paste0("dsq --job-file batchlist.txt -c 4 --mem-per-cpu 4g -t ", walltime, ":00 --mail-type ALL --partition scavenge --requeue"))

#system("source /etc/profile.d/modules.sh")
#system("module load dSQ")