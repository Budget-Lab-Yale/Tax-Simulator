#-------------------------------------------------------------------------------
# write_bounding_matrices.R
#
# Materializes the within-age net-worth percentile transition operator M for the
# EXTREME-DIFFUSION bound of the s x M bounding exercise (CG +5pp carryover).
#
# Two M bounds are studied:
#   * identity  (full persistence)  -- the v1 default; NO file needed
#                                       (wealth_financing_params.yaml
#                                        transition_matrix_file: null => diag(100)).
#   * uniform   (extreme diffusion / memoryless) -- THIS file. Each year every
#                                       age cohort's percentile distribution is
#                                       re-flattened to uniform; percentile rank
#                                       carries no information year-over-year.
#
# The loader (build_within_age_transition) sinkhorn-rakes whatever it reads to
# doubly-stochastic; the uniform matrix is already doubly-stochastic so raking
# is idempotent. n_bins MUST equal wealth_financing_params.yaml n_pctiles (100).
#
# RUN VIA SBATCH (never on the login node):
#   sbatch --wrap="module load R/4.4.1-foss-2022b && \
#                  Rscript other/wealth_dynamics/write_bounding_matrices.R"
#-------------------------------------------------------------------------------

n_bins = 100L

# Extreme-diffusion bound: every entry 1/n (row- AND column-stochastic already).
U = matrix(1 / n_bins, n_bins, n_bins)

out_path = './config/wealth/wealth_transition_uniform.rds'
saveRDS(U, out_path)

cat('Wrote', nrow(U), 'x', ncol(U), 'uniform (1/n) transition matrix to', out_path, '\n')
cat('  row sums range:', range(rowSums(U)), '\n')
cat('  col sums range:', range(colSums(U)), '\n')
