#------------------------------------------------------------------------------
# research/state_tax/aggregate/gap_bins.R
#
# Income-class edges shared by 01_gap_slices.R and 02_gap_report.R.
#------------------------------------------------------------------------------

# Federal AGI classes, as 1040_by_agi.csv (summary_stats.R:159)
AGI_BREAKS = c(-Inf, 1, 1e4, 2e4, 3e4, 4e4, 5e4, 7.5e4, 1e5, 2e5, 5e5, 1e6,
               1.5e6, 2e6, 5e6, 1e7, Inf)
AGI_LABELS = c('Negative AGI', '$1-$10K', '$10K-$20K', '$20K-$30K',
               '$30K-$40K', '$40K-$50K', '$50K-$75K', '$75K-$100K',
               '$100K-$200K', '$200K-$500K', '$500K-$1M', '$1M-$1.5M',
               '$1.5M-$2M', '$2M-$5M', '$5M-$10M', 'Over $10M')

# State-AGI classes for agency comparisons: the coarsest edges that nest
# inside the NY (DTF), CA (FTB B-4A) and CT (DRS) published bins, which are
# drawn on each state's own AGI
ST_AGI_BREAKS = c(-Inf, 1e4, 2e4, 3e4, 4e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6,
                  1e7, Inf)
ST_AGI_LABELS = c('Under $10K', '$10K-$20K', '$20K-$30K', '$30K-$40K',
                  '$40K-$50K', '$50K-$100K', '$100K-$200K', '$200K-$500K',
                  '$500K-$1M', '$1M-$2M', '$2M-$5M', '$5M-$10M', 'Over $10M')
