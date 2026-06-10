# Re-runs distribution post-processing for the kg_mech_50 vintage to pick up
# deemed heir reattribution in distribution.R (existing tables backed up as
# distribution_pre_reattribution.csv)

staging_dir = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/kg_mech_50/_slurm_staging'

source('./src/slurm/common.R')
runtime_args = reconstitute_environment(staging_dir)

for (sid in c('carryover', 'deemed')) {
  cat('Rebuilding distribution tables:', sid, '\n')
  build_distribution_tables(sid, baseline_id = 'baseline')
}
cat('DONE\n')
