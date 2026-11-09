################################################################################
# kg_dynamics revenue-impact decomposition
#
# Splits the conventional revenue delta for a kg_dynamics scenario into four
# channels, year-by-year:
#
#   [1] mechanical rate    : (tau_S - tau_B) * R_B
#                            (taxes baseline realizations at new rate; equals
#                            the static run's revenue_estimates total exactly)
#   [2] realization-rate   : tau_S * G_B * (r_S - r_B)
#                            (lock-in / unlock from the Bellman ordinary +
#                            planned-timing buckets, on the baseline gain stock)
#   [3] policy-induced     : tau_S * r_S * dG
#                            (realizations on the gain stock that wouldn't exist
#                            under baseline -- nonzero only when dG != 0, i.e.
#                            carryover or deemed regimes)
#   [4] deemed-at-death    : tau_S * delta_realize * sum mG_record * deemed_factor
#                            (extra realizations from death; nonzero only under
#                            deemed regime)
#
# Channels [2] [3] [4] are computed at the cell (age) level from
# kg_dynamics_age_profile.csv using each cell's own tau_S, then summed across
# ages. The four channels are an algebra-on-cells approximation to the engine
# ΔRev: the residual (conventional ΔRev minus channel sum) is reported as a
# nonlinearity/interaction wedge from bracket/NIIT/AMT interactions the cell
# algebra cannot see. A close-to-zero wedge means the decomposition reconciles.
#
# Usage:
#   Rscript other/analysis_scripts/kg_dyn_revenue_decomp.R <vintage> <scenario>
#
# Example:
#   Rscript other/analysis_scripts/kg_dyn_revenue_decomp.R kg_dyn_scf_check rate_up_5pp
#
# Outputs (written next to the scenario's supplemental dir):
#   conventional/supplemental/kg_dynamics_revenue_decomp.csv
#   conventional/supplemental/kg_dynamics_revenue_decomp.png
################################################################################

library(tidyverse)
library(data.table)

# --- Args / config ----------------------------------------------------------
args     = commandArgs(trailingOnly = TRUE)
vintage  = if (length(args) >= 1) args[1] else 'kg_dyn_scf_check'
scenario = if (length(args) >= 2) args[2] else 'rate_up_5pp'

out_root = file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1', vintage)
scn_dir  = file.path(out_root, scenario)

age_profile_path = file.path(scn_dir, 'conventional/supplemental/kg_dynamics_age_profile.csv')
conv_rev_path    = file.path(scn_dir, 'conventional/supplemental/revenue_estimates.csv')
static_rev_path  = file.path(scn_dir, 'static/supplemental/revenue_estimates.csv')

stopifnot(file.exists(age_profile_path), file.exists(conv_rev_path),
          file.exists(static_rev_path))

# --- Cell-level channel algebra --------------------------------------------
age = read_csv(age_profile_path, show_col_types = FALSE)

# Per-cell delta_realize / delta_route already live on age_profile (added by
# kg_dyn_build_regime_mix). Under the per-asset regime schema these are
# gain-stock-weighted mixes in [0, 1], not 0/1 flags, so the deemed-channel
# gating happens at the cell level rather than year level.

# --- Decedent-weighted tau (fix for the deemed-channel compositional bias)--
# Cohort tau in age_profile is realization-weighted (averages MTR across
# dollars currently realizing). But the deemed channel hits dollars weighted
# by m * G_unit — concentrated on older, gain-rich, current-income-poor
# decedents who sit in lower brackets. Re-weight tau by decedent_flag *
# kg_lt_pos from the conventional detail. Under deemed regime, decedents'
# conventional kg_lt ≈ G_unit (because the deemed factor pushes the full
# unrealized stock into realizations), so kg_lt-weighting among decedents
# closely approximates the m * G_unit weighting we actually want.
compute_tau_deemed_by_cohort = function(scn_dir, years) {
  detail_dir = file.path(scn_dir, 'conventional', 'detail')
  bind_rows(lapply(years, function(t) {
    f = file.path(detail_dir, paste0(t, '.csv'))
    if (!file.exists(f)) return(tibble(year = t, age = integer(),
                                       tau_deemed_S = numeric()))
    dt = fread(f, select = c('weight', 'filing_status', 'age1', 'age2',
                             'kg_lt', 'mtr_kg_lt', 'decedent_flag'))
    dt[, kg_pos := pmax(kg_lt, 0)]
    dt[, age_cohort := fifelse(filing_status == 2L,
                               pmax(age1, age2, na.rm = TRUE),
                               age1)]
    dt[, age_cohort := pmax(18L, pmin(80L, as.integer(age_cohort)))]
    dt = dt[decedent_flag == 1L & kg_pos > 0]
    if (nrow(dt) == 0L) return(tibble(year = t, age = integer(),
                                      tau_deemed_S = numeric()))
    agg = dt[, .(num = sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE),
                 den = sum(weight * kg_pos,             na.rm = TRUE)),
             by = age_cohort]
    agg[, tau_deemed_S := num / den]
    tibble(year = t, age = as.integer(agg$age_cohort),
           tau_deemed_S = agg$tau_deemed_S)
  }))
}

cat('Reading conventional detail to compute decedent-weighted tau ...\n')
tau_deemed = compute_tau_deemed_by_cohort(scn_dir, unique(age$year))

# Channel sums (in dollars, then divided to $B to match revenue_estimates)
age_aug = age %>% left_join(tau_deemed, by = c('year', 'age')) %>%
  # Fallback to realization-weighted tau where no decedents in cohort
  mutate(tau_deemed_S = if_else(is.na(tau_deemed_S), tau_S, tau_deemed_S))

channels = age_aug %>%
  group_by(year) %>%
  summarise(
    ch_lockin_unlock = sum(tau_S * G_B * (r_S - r_B)),
    ch_stock         = sum(tau_S * extra_R),                          # = tau_S * r_S * dG
    ch_deemed        = sum(tau_deemed_S * mG_record * deemed_factor   # cell-level delta_realize
                           * delta_realize),
    .groups = 'drop'
  ) %>%
  mutate(across(starts_with('ch_'), ~ . / 1e9))   # dollars -> $B

# --- Engine numbers ---------------------------------------------------------
conv_rev   = read_csv(conv_rev_path,   show_col_types = FALSE) %>% rename(conv_rev   = total)
static_rev = read_csv(static_rev_path, show_col_types = FALSE) %>% rename(static_rev = total)

decomp = channels %>%
  inner_join(conv_rev,   by = 'year') %>%
  inner_join(static_rev, by = 'year') %>%
  mutate(
    mechanical    = static_rev,
    cell_sum      = mechanical + ch_lockin_unlock + ch_stock + ch_deemed,
    nonlinearity  = conv_rev - cell_sum
  ) %>%
  select(year, mechanical, ch_lockin_unlock, ch_stock, ch_deemed,
         nonlinearity, cell_sum, conv_rev, static_rev)

# --- Write CSV --------------------------------------------------------------
decomp_csv = file.path(scn_dir, 'conventional/supplemental/kg_dynamics_revenue_decomp.csv')
write_csv(decomp, decomp_csv)
cat('Wrote ', decomp_csv, '\n', sep = '')

# --- Stacked bar ------------------------------------------------------------
plot_df = decomp %>%
  select(year, mechanical, ch_lockin_unlock, ch_stock, ch_deemed, nonlinearity) %>%
  pivot_longer(-year, names_to = 'channel', values_to = 'rev') %>%
  mutate(channel = factor(channel,
                          levels = c('mechanical', 'ch_lockin_unlock',
                                     'ch_stock', 'ch_deemed', 'nonlinearity'),
                          labels = c('1. Mechanical rate',
                                     '2. Realization-rate response',
                                     '3. Revenue on policy-induced stock (dG)',
                                     '4. Deemed at death',
                                     '5. Nonlinearity / interaction wedge')))

engine_df = decomp %>% select(year, conv_rev)

palette = c('1. Mechanical rate'                       = '#1f77b4',
            '2. Realization-rate response'             = '#d62728',
            '3. Revenue on policy-induced stock (dG)'  = '#2ca02c',
            '4. Deemed at death'                       = '#9467bd',
            '5. Nonlinearity / interaction wedge'      = '#7f7f7f')

p = ggplot(plot_df, aes(x = year, y = rev, fill = channel)) +
  geom_col(position = 'stack', width = 0.8) +
  geom_line(data = engine_df, aes(x = year, y = conv_rev),
            inherit.aes = FALSE, linewidth = 0.7, color = 'black') +
  geom_point(data = engine_df, aes(x = year, y = conv_rev),
             inherit.aes = FALSE, size = 1.2, color = 'black') +
  scale_fill_manual(values = palette, name = NULL) +
  scale_y_continuous(labels = scales::dollar_format(suffix = 'B')) +
  labs(title = paste0('kg_dynamics revenue decomposition: ', scenario,
                      ' (', vintage, ')'),
       subtitle = paste0('Stacked bars = channel attribution (cell algebra); ',
                         'black line = conventional engine ΔRev.\n',
                         'Channel sum reconciles to engine up to nonlinearity wedge.'),
       x = NULL, y = 'Revenue change vs. baseline') +
  theme_minimal(base_size = 11) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = 'grey30'))

plot_path = file.path(scn_dir, 'conventional/supplemental/kg_dynamics_revenue_decomp.png')
ggsave(plot_path, p, width = 10, height = 6, dpi = 150)
cat('Wrote ', plot_path, '\n', sep = '')

# --- Console summary --------------------------------------------------------
cat('\nFirst/last 3 years of decomposition ($B):\n')
print(bind_rows(head(decomp, 3), tail(decomp, 3)) %>%
        select(year, mechanical, ch_lockin_unlock, ch_stock,
               ch_deemed, nonlinearity, conv_rev),
      n = Inf)

cat('\nReconciliation check (cell_sum vs conv_rev, $B):\n')
print(decomp %>%
        summarise(mean_abs_err = mean(abs(conv_rev - cell_sum)),
                  max_abs_err  = max(abs(conv_rev - cell_sum)),
                  mean_pct_err = mean(abs(conv_rev - cell_sum) / abs(conv_rev)) * 100))
