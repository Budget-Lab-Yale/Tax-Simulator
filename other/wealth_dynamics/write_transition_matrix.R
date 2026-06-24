#-------------------------------------------------------------------------------
# write_transition_matrix.R
#
# Materializes config/wealth/wealth_transition_matrix.rds, the within-age
# net-worth percentile transition operator M for the wealth bathtub.
#
# v1 PLACEHOLDER: 100x100 identity (full persistence) applied to every age. NOTE
# the sim does NOT require this file: build_within_age_transition() defaults to
# the in-memory identity when wealth_financing_params.yaml's
# transition_matrix_file is null. This script exists for the SWAP PATH -- write
# a real transition matrix (PSID within-age percentile transitions, raked to
# doubly-stochastic) here, then point transition_matrix_file at the output.
#
# Output contract (consumed by build_within_age_transition):
#   - a single n_bins x n_bins row-stochastic matrix (applied to all ages), OR
#   - a named (by age, as.character) list of such matrices.
# Either is raked to doubly-stochastic by the loader (sinkhorn_rake).
#
# RUN VIA SBATCH (never on the login node):
#   sbatch --wrap="module load R/4.4.1-foss-2022b && \
#                  Rscript other/wealth_dynamics/write_transition_matrix.R"
#-------------------------------------------------------------------------------

n_bins = 100L

# v1: identity (full persistence). Swap this assignment for an estimated matrix.
M = diag(n_bins)

out_path = './config/wealth/wealth_transition_matrix.rds'
saveRDS(M, out_path)
cat('Wrote', nrow(M), 'x', ncol(M), 'identity transition matrix to', out_path, '\n')
