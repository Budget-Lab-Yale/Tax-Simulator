################################################################################
# Stacked distribution analysis for Booker-KYPA proposal
# Produces: contribution to pct change in ATI by quintile, stacked bar chart
################################################################################

library(tidyverse)
library(scales)

# --- Configuration -----------------------------------------------------------
vintage   = '202603120745'
out_root  = file.path('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1', vintage)
year_show = 2026

# Stacked scenarios in cumulative order
scenarios = c('std', 'std_eitc', 'std_eitc_ctc', 'std_eitc_ctc_ord')

piece_labels = c(
  'std'  = 'Increase in Standard Deduction',
  'eitc' = 'EITC Expansion',
  'ctc'  = 'CTC Expansion',
  'ord'  = 'Increase in Top Rates'
)

colors = c(
  'Increase in Standard Deduction' = '#2166AC',
  'EITC Expansion'                 = '#4DAF4A',
  'CTC Expansion'                  = '#FF7F00',
  'Increase in Top Rates'          = '#E41A1C'
)

# --- Read distribution tables ------------------------------------------------
read_dist = function(scenario) {
  path = file.path(out_root, scenario, 'static/supplemental/distribution.csv')
  read_csv(path, show_col_types = FALSE) %>%
    mutate(scenario = scenario)
}

dist_all = map_dfr(scenarios, read_dist)

# --- Reusable chart builder --------------------------------------------------
dist_footnote = str_wrap("Source: The Budget Lab calculations. Note: Estimate universe is nondependent tax units, including nonfilers. 'Income' is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, and inheritances.", width = 120)

build_stacked_chart = function(group_dim, groups, x_positions, x_labs,
                               separator = NULL, brackets = NULL,
                               fig_number, title_suffix, filename) {

  # Filter and compute marginal contributions
  df = dist_all %>%
    filter(
      year == year_show,
      taxes_included == 'iit_pr',
      group_dimension == group_dim,
      group %in% groups
    ) %>%
    select(scenario, group, pct_chg_ati) %>%
    pivot_wider(names_from = scenario, values_from = pct_chg_ati) %>%
    mutate(
      std  = std,
      eitc = std_eitc - std,
      ctc  = std_eitc_ctc - std_eitc,
      ord  = std_eitc_ctc_ord - std_eitc_ctc
    ) %>%
    select(group, std, eitc, ctc, ord)

  # Print table
  cat('\n=====', title_suffix, '(', year_show, ') =====\n\n')
  df %>%
    mutate(total = std + eitc + ctc + ord) %>%
    mutate(across(where(is.numeric), ~ round(.x * 100, 2))) %>%
    print(n = Inf, width = Inf)

  # Plot data
  plot_data = df %>%
    pivot_longer(cols = c(std, eitc, ctc, ord), names_to = 'piece', values_to = 'pct_chg') %>%
    mutate(
      pct_chg = pct_chg * 100,
      piece = factor(piece, levels = c('std', 'eitc', 'ctc', 'ord'),
                     labels = piece_labels),
      group = factor(group, levels = groups),
      xpos  = x_positions[as.character(group)]
    )

  # Net change bubbles
  net_dots = plot_data %>%
    group_by(group, xpos) %>%
    summarise(net = sum(pct_chg), .groups = 'drop') %>%
    mutate(net_label = paste0(ifelse(net >= 0, '+', ''),
                              formatC(round(net, 1), format = 'f', digits = 1)))

  # Bracket placement
  bar_totals = plot_data %>%
    group_by(group) %>%
    summarise(pos = sum(pct_chg[pct_chg > 0]), neg = sum(pct_chg[pct_chg < 0]))
  y_range = c(min(bar_totals$neg), max(bar_totals$pos))

  # Build plot
  p = ggplot(plot_data, aes(x = xpos, y = pct_chg, fill = piece)) +
    geom_col(position = position_stack(), width = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    # Net change bubbles: white filled circle with black outline + label
    geom_point(data = net_dots, aes(x = xpos, y = net),
               inherit.aes = FALSE, shape = 21, size = 10,
               fill = 'white', color = 'black', stroke = 0.8) +
    geom_text(data = net_dots, aes(x = xpos, y = net, label = net_label),
              inherit.aes = FALSE, size = 2.8, fontface = 'bold')

  # Optional separator + brackets
  if (!is.null(separator)) {
    p = p + geom_vline(xintercept = separator, linetype = 'dashed',
                       color = 'grey60', linewidth = 0.3)
  }
  if (!is.null(brackets)) {
    bracket_y = y_range[1] - diff(y_range) * 0.18
    label_y_b = y_range[1] - diff(y_range) * 0.24
    for (b in brackets) {
      p = p +
        annotate('segment', x = b$x1, xend = b$x2, y = bracket_y, yend = bracket_y, color = 'grey40') +
        annotate('segment', x = b$x1, xend = b$x1, y = bracket_y, yend = bracket_y + diff(y_range) * 0.02, color = 'grey40') +
        annotate('segment', x = b$x2, xend = b$x2, y = bracket_y, yend = bracket_y + diff(y_range) * 0.02, color = 'grey40') +
        annotate('text', x = (b$x1 + b$x2) / 2, y = label_y_b, label = b$label,
                 fontface = 'bold', size = 4, color = 'grey30')
    }
  }

  # Winner/loser share annotations (from the full cumulative scenario)
  full_scn = scenarios[length(scenarios)]
  wl = dist_all %>%
    filter(
      year == year_show,
      taxes_included == 'iit_pr',
      group_dimension == group_dim,
      group %in% groups,
      scenario == full_scn
    ) %>%
    mutate(
      xpos = x_positions[as.character(group)],
      win_pct  = `share_cut.100` * 100,
      lose_pct = `share_raise.100` * 100,
      win_label  = if_else(win_pct > 0 & win_pct < 0.5, '<1%', paste0(round(win_pct), '%')),
      lose_label = if_else(lose_pct > 0 & lose_pct < 0.5, '<1%', paste0(round(lose_pct), '%'))
    )

  base_offset = if (!is.null(brackets)) 0.34 else 0.18
  row_gap     = 0.08
  wl_y1 = y_range[1] - diff(y_range) * base_offset
  wl_y2 = y_range[1] - diff(y_range) * (base_offset + row_gap)
  label_x = min(x_positions) - 0.5

  p = p +
    annotate('text', x = label_x, y = wl_y1, label = 'Tax cut >$100:',
             fontface = 'bold', size = 3, hjust = 1, color = 'grey30') +
    annotate('text', x = label_x, y = wl_y2, label = 'Tax hike >$100:',
             fontface = 'bold', size = 3, hjust = 1, color = 'grey30') +
    geom_text(data = wl, aes(x = xpos, y = wl_y1, label = win_label),
              inherit.aes = FALSE, size = 3, color = 'grey30') +
    geom_text(data = wl, aes(x = xpos, y = wl_y2, label = lose_label),
              inherit.aes = FALSE, size = 3, color = 'grey30')

  p = p +
    scale_x_continuous(breaks = x_positions, labels = x_labs) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    coord_cartesian(clip = 'off') +
    labs(
      title   = paste0('Figure ', fig_number, '. Contribution to Change in After-Tax Income ', title_suffix, ' (', year_show, ')'),
      x       = NULL,
      y       = 'Change in After-Tax Income (pp)',
      fill    = NULL,
      caption = dist_footnote
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x        = element_text(size = 11, face = 'bold'),
      legend.position     = 'bottom',
      panel.grid.major.x  = element_blank(),
      plot.title          = element_text(face = 'bold', size = 14),
      plot.caption        = element_text(hjust = 0, size = 8, color = 'grey40'),
      plot.margin         = margin(10, 10, 50, 40),
      plot.background     = element_rect(fill = 'transparent', color = NA),
      panel.background    = element_rect(fill = 'transparent', color = NA)
    )

  out_path = file.path(out_root, filename)
  ggsave(out_path, plot = p, width = 11, height = 7, dpi = 200, bg = 'transparent')
  cat('\nChart saved to:', out_path, '\n')
}

# --- Chart 1: Income quintiles + top breakout --------------------------------
income_groups = c('Quintile 1', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5',
                  'Top 10%', 'Top 5%', 'Top 1%', 'Top 0.1%')
income_xpos = c(1, 2, 3, 4, 5, 6.5, 7.5, 8.5, 9.5)
names(income_xpos) = income_groups
income_xlabs = c('Q1\n(Bottom)', 'Q2', 'Q3', 'Q4', 'Q5',
                 'Top\n10%', 'Top\n5%', 'Top\n1%', 'Top\n0.1%')
names(income_xlabs) = income_groups

build_stacked_chart(
  group_dim    = 'Income',
  groups       = income_groups,
  x_positions  = income_xpos,
  x_labs       = income_xlabs,
  separator    = 5.75,
  brackets     = list(
    list(x1 = 1, x2 = 5, label = 'Quintiles'),
    list(x1 = 6.5, x2 = 9.5, label = 'Top Decile Breakout')
  ),
  fig_number   = 1,
  title_suffix = 'by Income Group',
  filename     = 'stacked_distribution_ati.png'
)

# --- Chart 2: Age groups -----------------------------------------------------
age_groups = c('29 and under', '30 - 39', '40 - 49', '50 - 64', '65+')
age_xpos = seq_along(age_groups)
names(age_xpos) = age_groups
age_xlabs = age_groups
names(age_xlabs) = age_groups

build_stacked_chart(
  group_dim    = 'Age',
  groups       = age_groups,
  x_positions  = age_xpos,
  x_labs       = age_xlabs,
  separator    = NULL,
  brackets     = NULL,
  fig_number   = 2,
  title_suffix = 'by Age Group',
  filename     = 'stacked_distribution_ati_age.png'
)

# --- Chart 3: Horizontal equity (IQR of ETR within groups) ------------------
# Read horizontal equity tables from the full policy scenario
he_path = file.path(out_root, 'std_eitc_ctc_ord/static/supplemental/horizontal.csv')
he = read_csv(he_path, show_col_types = FALSE) %>%
  filter(year == year_show, group_dimension == 'Income')

he_groups = c('Quintile 1', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5', 'Overall')
he_xlabs  = c('Q1\n(Bottom)', 'Q2', 'Q3', 'Q4', 'Q5', 'Overall')

he_plot = he %>%
  filter(group %in% he_groups) %>%
  mutate(
    iqr   = avg_within_group_iqr * 100,
    group = factor(group, levels = he_groups),
    scenario = if_else(scenario == 'baseline', 'Current Law', 'Proposal')
  )

cat('\n===== Horizontal Equity: Within-Group IQR of ETR (', year_show, ') =====\n\n')
he_plot %>%
  select(scenario, group, iqr) %>%
  pivot_wider(names_from = scenario, values_from = iqr) %>%
  print(n = Inf, width = Inf)

he_colors = c('Current Law' = '#636363', 'Proposal' = '#2166AC')

p_he = ggplot(he_plot, aes(x = group, y = iqr, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = formatC(round(iqr, 1), format = 'f', digits = 1)),
            position = position_dodge(width = 0.7), vjust = -0.5,
            size = 3.5, fontface = 'bold') +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_discrete(labels = he_xlabs) +
  scale_fill_manual(values = he_colors, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, ' pp')) +
  labs(
    title   = paste0('Figure 3. Within-Group IQR of Effective Tax Rate (', year_show, ')'),
    x       = NULL,
    y       = 'IQR of Effective Tax Rate (pp)',
    caption = dist_footnote
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(size = 11),
    legend.position     = 'bottom',
    panel.grid.major.x  = element_blank(),
    plot.title          = element_text(face = 'bold', size = 14),
    plot.caption        = element_text(hjust = 0, size = 8, color = 'grey40'),
    plot.margin         = margin(10, 10, 10, 10),
    plot.background     = element_rect(fill = 'transparent', color = NA),
    panel.background    = element_rect(fill = 'transparent', color = NA)
  )

he_out = file.path(out_root, 'horizontal_equity.png')
ggsave(he_out, plot = p_he, width = 10, height = 6, dpi = 200, bg = 'transparent')
cat('\nChart saved to:', he_out, '\n')

# --- Chart 4: Average MTR on wages by AGI percentile, parent vs nonparent ---
cat('\n===== Building MTR chart... =====\n')

# Read detail files for baseline and full policy
read_detail = function(scenario) {
  path = file.path(out_root, scenario, 'static/detail', paste0(year_show, '.csv'))
  read_csv(path, show_col_types = FALSE,
           col_select = c(weight, agi, mtr_wages1, dep_age1, dep_age2, dep_age3))
}

detail_baseline = read_detail('baseline') %>% mutate(scenario = 'Current Law')
detail_policy   = read_detail('std_eitc_ctc_ord') %>% mutate(scenario = 'Proposal')

mtr_data = bind_rows(detail_baseline, detail_policy) %>%
  # Exclude negative AGI
  filter(agi >= 0) %>%
  # Parent status
  mutate(
    parent = if_else(
      (!is.na(dep_age1) & dep_age1 < 18) |
      (!is.na(dep_age2) & dep_age2 < 18) |
      (!is.na(dep_age3) & dep_age3 < 18),
      'Parent', 'Non-parent'
    )
  )

# Compute AGI percentile within each scenario
mtr_data = mtr_data %>%
  group_by(scenario) %>%
  arrange(agi) %>%
  mutate(
    cum_weight = cumsum(weight),
    agi_pctile = ceiling(cum_weight / sum(weight) * 100),
    agi_pctile = pmin(agi_pctile, 100)
  ) %>%
  ungroup()

# Weighted average MTR by AGI percentile × parent status × scenario
mtr_summary = mtr_data %>%
  group_by(scenario, parent, agi_pctile) %>%
  summarise(
    avg_mtr = weighted.mean(mtr_wages1, weight) * 100,
    .groups = 'drop'
  ) %>%
  mutate(
    scenario = factor(scenario, levels = c('Current Law', 'Proposal')),
    parent   = factor(parent, levels = c('Non-parent', 'Parent'))
  )

mtr_colors = c('Current Law' = '#636363', 'Proposal' = '#2166AC')

p_mtr = ggplot(mtr_summary, aes(x = agi_pctile, y = avg_mtr, color = scenario)) +
  geom_line(linewidth = 0.4, alpha = 0.35) +
  geom_point(size = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  facet_wrap(~ parent) +
  scale_color_manual(values = mtr_colors, name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  labs(
    title   = paste0('Figure 5. Average Effective Marginal Tax Rate on Wages by AGI Percentile (', year_show, ')'),
    x       = 'AGI Percentile',
    y       = 'Average MTR on Wages',
    caption = 'Source: The Budget Lab calculations.'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = 'top',
    strip.text       = element_text(face = 'bold', size = 12),
    plot.title       = element_text(face = 'bold', size = 14),
    plot.caption     = element_text(hjust = 0, size = 8, color = 'grey40'),
    panel.spacing    = unit(1.5, 'lines'),
    plot.margin      = margin(10, 10, 10, 10),
    plot.background  = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA),
    strip.background = element_rect(fill = 'transparent', color = NA)
  )

mtr_out = file.path(out_root, 'mtr_wages_by_agi_pctile.png')
ggsave(mtr_out, plot = p_mtr, width = 12, height = 6, dpi = 200, bg = 'transparent')
cat('\nChart saved to:', mtr_out, '\n')

# --- Chart 5: Stacked MTR change decomposition by AGI percentile ------------
cat('\n===== Building stacked MTR decomposition chart... =====\n')

# Read detail for all scenarios + baseline
read_detail_scenario = function(scn) {
  path = file.path(out_root, scn, 'static/detail', paste0(year_show, '.csv'))
  read_csv(path, show_col_types = FALSE,
           col_select = c(weight, agi, mtr_wages1, dep_age1, dep_age2, dep_age3)) %>%
    mutate(scn = scn)
}

all_detail = map_dfr(c('baseline', scenarios), read_detail_scenario) %>%
  filter(agi >= 0) %>%
  mutate(
    parent = if_else(
      (!is.na(dep_age1) & dep_age1 < 18) |
      (!is.na(dep_age2) & dep_age2 < 18) |
      (!is.na(dep_age3) & dep_age3 < 18),
      'Parent', 'Non-parent'
    )
  )

# Compute AGI percentile within each scenario
all_detail = all_detail %>%
  group_by(scn) %>%
  arrange(agi) %>%
  mutate(
    agi_pctile = ceiling(cumsum(weight) / sum(weight) * 100),
    agi_pctile = pmin(agi_pctile, 100)
  ) %>%
  ungroup()

# Weighted average MTR by percentile × parent × scenario
mtr_by_scn = all_detail %>%
  group_by(scn, parent, agi_pctile) %>%
  summarise(avg_mtr = weighted.mean(mtr_wages1, weight) * 100, .groups = 'drop')

# Pivot wide and compute marginal contributions
mtr_wide = mtr_by_scn %>%
  pivot_wider(names_from = scn, values_from = avg_mtr)

mtr_contribs = mtr_wide %>%
  mutate(
    std  = std - baseline,
    eitc = std_eitc - std - (baseline - baseline),  # std_eitc - std already relative
    ctc  = std_eitc_ctc - std_eitc,
    ord  = std_eitc_ctc_ord - std_eitc_ctc
  ) %>%
  # Fix: eitc is just incremental from std
  mutate(eitc = std_eitc - std - baseline + baseline) %>%
  select(parent, agi_pctile, std = std, eitc, ctc, ord)

# Actually recompute cleanly: each piece is diff from previous cumulative vs baseline
mtr_contribs = mtr_wide %>%
  mutate(
    d_std  = std - baseline,
    d_eitc = std_eitc - std,
    d_ctc  = std_eitc_ctc - std_eitc,
    d_ord  = std_eitc_ctc_ord - std_eitc_ctc
  ) %>%
  select(parent, agi_pctile, std = d_std, eitc = d_eitc, ctc = d_ctc, ord = d_ord)

mtr_stack = mtr_contribs %>%
  pivot_longer(cols = c(std, eitc, ctc, ord), names_to = 'piece', values_to = 'value') %>%
  mutate(
    piece  = factor(piece, levels = c('std', 'eitc', 'ctc', 'ord'),
                    labels = piece_labels),
    parent = factor(parent, levels = c('Non-parent', 'Parent'))
  )

# Net change dots
mtr_net = mtr_stack %>%
  group_by(parent, agi_pctile) %>%
  summarise(net = sum(value), .groups = 'drop')

p_mtr_stack = ggplot(mtr_stack, aes(x = agi_pctile, y = value, fill = piece)) +
  geom_col(position = position_stack(), width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_point(data = mtr_net, aes(x = agi_pctile, y = net),
             inherit.aes = FALSE, shape = 21, size = 1.8,
             fill = 'white', color = 'black', stroke = 0.5) +
  facet_wrap(~ parent) +
  scale_fill_manual(values = colors, name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_y_continuous(labels = function(x) paste0(x, ' pp')) +
  labs(
    title   = paste0('Figure 6. Contribution to Change in Average Effective Marginal Tax Rate on Wages by AGI Percentile (', year_show, ')'),
    x       = 'AGI Percentile',
    y       = 'Change in Average MTR (pp)',
    caption = 'Source: The Budget Lab calculations.'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = 'top',
    strip.text       = element_text(face = 'bold', size = 12),
    plot.title       = element_text(face = 'bold', size = 13),
    plot.caption     = element_text(hjust = 0, size = 8, color = 'grey40'),
    panel.spacing    = unit(1.5, 'lines'),
    plot.margin      = margin(10, 10, 10, 10),
    plot.background  = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA),
    strip.background = element_rect(fill = 'transparent', color = NA)
  )

mtr_stack_out = file.path(out_root, 'mtr_wages_stacked_decomposition.png')
ggsave(mtr_stack_out, plot = p_mtr_stack, width = 12, height = 6, dpi = 200, bg = 'transparent')
cat('\nChart saved to:', mtr_stack_out, '\n')

# --- Chart 6: Time burden levels (baseline vs policy) ------------------------
cat('\n===== Building time burden chart... =====\n')

# Read time burden for the full policy scenario
tb = read_csv(file.path(out_root, 'std_eitc_ctc_ord/static/supplemental/time_burden.csv'),
              show_col_types = FALSE)

tb_groups = c('Quintile 1', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5', 'Overall')
tb_xlabs  = c('Q1\n(Bottom)', 'Q2', 'Q3', 'Q4', 'Q5', 'Overall')

tb_plot = tb %>%
  filter(year == year_show, metric == 'mean_burden', group %in% tb_groups) %>%
  select(group, baseline, reform) %>%
  pivot_longer(cols = c(baseline, reform), names_to = 'scenario', values_to = 'hours') %>%
  mutate(
    scenario = if_else(scenario == 'baseline', 'Current Law', 'Proposal'),
    group    = factor(group, levels = tb_groups),
    label    = formatC(round(hours, 1), format = 'f', digits = 1)
  )

cat('\n===== Mean Filing Time Burden (hours, ', year_show, ') =====\n\n')
tb_plot %>%
  select(group, scenario, hours) %>%
  pivot_wider(names_from = scenario, values_from = hours) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print(n = Inf, width = Inf)

tb_colors = c('Current Law' = '#636363', 'Proposal' = '#2166AC')

p_tb = ggplot(tb_plot, aes(x = group, y = hours, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.7), vjust = -0.5,
            size = 3.5, fontface = 'bold') +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 5.5, linetype = 'dashed', color = 'grey60', linewidth = 0.3) +
  scale_x_discrete(labels = tb_xlabs) +
  scale_fill_manual(values = tb_colors, name = NULL) +
  labs(
    title   = paste0('Figure 4. Average Tax Filing Time Burden by Income Group (', year_show, ')'),
    x       = NULL,
    y       = 'Mean Filing Time (hours)',
    caption = dist_footnote
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(size = 11),
    legend.position     = 'bottom',
    panel.grid.major.x  = element_blank(),
    plot.title          = element_text(face = 'bold', size = 14),
    plot.caption        = element_text(hjust = 0, size = 8, color = 'grey40'),
    plot.margin         = margin(10, 10, 10, 10),
    plot.background     = element_rect(fill = 'transparent', color = NA),
    panel.background    = element_rect(fill = 'transparent', color = NA)
  )

tb_out = file.path(out_root, 'time_burden.png')
ggsave(tb_out, plot = p_tb, width = 10, height = 6, dpi = 200, bg = 'transparent')
cat('\nChart saved to:', tb_out, '\n')

# --- Table: Stacked revenue estimates with % of GDP ---------------------------
cat('\n===== Building stacked revenue table... =====\n')

# Read revenue estimates for each cumulative scenario
read_rev = function(scn) {
  read_csv(file.path(out_root, scn, 'static/supplemental/revenue_estimates.csv'),
           show_col_types = FALSE) %>%
    mutate(scn = scn)
}

rev = map_dfr(scenarios, read_rev) %>%
  pivot_wider(names_from = scn, values_from = total)

# Marginal contributions
rev_stacked = rev %>%
  mutate(
    `Increase in Standard Deduction` = std,
    `EITC Expansion`                 = std_eitc - std,
    `CTC Expansion`                  = std_eitc_ctc - std_eitc,
    `Increase in Top Rates`          = std_eitc_ctc_ord - std_eitc_ctc,
    `Total`                          = std_eitc_ctc_ord
  ) %>%
  select(year, `Increase in Standard Deduction`, `EITC Expansion`, `CTC Expansion`, `Increase in Top Rates`, Total)

# Read GDP projections
gdp = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2026022522/baseline/projections.csv',
               show_col_types = FALSE) %>%
  select(year, gdp_fy)

rev_stacked = rev_stacked %>% left_join(gdp, by = 'year')

# Build summary rows for budget windows
make_window = function(df, start, end, label) {
  sub = df %>% filter(year >= start, year <= end)
  gdp_total = sum(sub$gdp_fy)
  tibble(
    year  = label,
    `Increase in Standard Deduction` = sum(sub$`Increase in Standard Deduction`),
    `EITC Expansion`                 = sum(sub$`EITC Expansion`),
    `CTC Expansion`                  = sum(sub$`CTC Expansion`),
    `Increase in Top Rates`          = sum(sub$`Increase in Top Rates`),
    Total                            = sum(sub$Total),
    gdp_fy                           = gdp_total
  )
}

windows = bind_rows(
  make_window(rev_stacked, 2026, 2035, '2026-2035'),
  make_window(rev_stacked, 2036, 2045, '2036-2045'),
  make_window(rev_stacked, 2046, 2054, '2046-2054')
)

provisions = c('Increase in Standard Deduction', 'EITC Expansion', 'CTC Expansion', 'Increase in Top Rates', 'Total')

# --- Single wide table: provisions as rows, years + windows as columns ---
# Annual dollars (wide)
annual_wide = rev_stacked %>%
  filter(year >= 2026, year <= 2035) %>%
  mutate(year = as.character(year)) %>%
  select(year, all_of(provisions)) %>%
  pivot_longer(cols = all_of(provisions), names_to = 'Provision', values_to = 'val') %>%
  pivot_wider(names_from = year, values_from = val)

# Window dollars + % GDP (Total row only gets % GDP appended)
window_labels = c('2026-2035', '2036-2045', '2046-2054')
nice_labels   = c('Budget\nWindow', 'Second\nDecade', 'Third\nDecade')

window_dollars = windows %>%
  select(year, all_of(provisions)) %>%
  pivot_longer(cols = all_of(provisions), names_to = 'Provision', values_to = 'val') %>%
  pivot_wider(names_from = year, values_from = val) %>%
  rename_with(~ nice_labels[match(.x, window_labels)], .cols = all_of(window_labels))

window_pct = windows %>%
  mutate(across(all_of(provisions), ~ .x / gdp_fy * 100)) %>%
  select(year, all_of(provisions)) %>%
  pivot_longer(cols = all_of(provisions), names_to = 'Provision', values_to = 'val') %>%
  pivot_wider(names_from = year, values_from = val) %>%
  rename_with(~ paste0(nice_labels[match(.x, window_labels)], '\n(% GDP)'), .cols = all_of(window_labels))

wide_table = annual_wide %>%
  left_join(window_dollars, by = 'Provision') %>%
  left_join(window_pct, by = 'Provision') %>%
  mutate(Provision = factor(Provision, levels = provisions)) %>%
  arrange(Provision)

# Save CSV
rev_table_out = file.path(out_root, 'stacked_revenue_table.csv')
wide_table %>%
  rename_with(~ gsub('\n', ' ', .x)) %>%
  write_csv(rev_table_out)

# --- Render as image ---
library(gridExtra)
library(grid)

fmt_d = function(x) formatC(round(x), format = 'f', digits = 0, big.mark = ',')
fmt_p = function(x) formatC(x, format = 'f', digits = 2)

disp = wide_table %>%
  mutate(
    across(as.character(2026:2035), fmt_d),
    across(all_of(nice_labels), fmt_d),
    across(contains('% GDP'), fmt_p),
    Provision = as.character(Provision)
  )

# Column order: provision, annuals, then (dollars, %gdp) pairs for each window
col_order = c('Provision', as.character(2026:2035))
for (nl in nice_labels) {
  col_order = c(col_order, nl, paste0(nl, '\n(% GDP)'))
}
disp = disp[, col_order]

# Alternating row shading via fill colors
n_rows = nrow(disp)
n_cols = ncol(disp)
fill_colors = matrix('white', nrow = n_rows, ncol = n_cols)
fill_colors[seq(2, n_rows, 2), ] = 'grey95'

tt = ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 9, fontface = 'plain'),
    bg_params = list(fill = fill_colors, col = NA),
    padding   = unit(c(6, 4), 'pt')
  ),
  colhead = list(
    fg_params = list(fontsize = 9, fontface = 'bold'),
    bg_params = list(fill = 'grey85', col = NA),
    padding   = unit(c(6, 4), 'pt')
  )
)

g = tableGrob(disp, rows = NULL, theme = tt)

# Add horizontal line above Total row
g = gtable::gtable_add_grob(g,
  grobs = segmentsGrob(x0 = 0, x1 = 1, y0 = 1, y1 = 1,
                        gp = gpar(lwd = 1.5)),
  t = nrow(disp) + 1, l = 1, r = ncol(disp))

title = textGrob('Table 1. Estimated Conventional Budgetary Effects, FY2026-2055',
                  gp = gpar(fontsize = 13, fontface = 'bold'), just = 'left', x = 0.02)
subtitle = textGrob('Billions of dollars; window totals with percent of GDP',
                     gp = gpar(fontsize = 10, fontface = 'italic', col = 'grey40'),
                     just = 'left', x = 0.02)

row_h = 0.28
tbl_grob = arrangeGrob(title, subtitle, g, ncol = 1,
                        heights = unit(c(0.4, 0.3, nrow(disp) * row_h + 0.5), 'inches'))

rev_img_out = file.path(out_root, 'stacked_revenue_table.png')
ggsave(rev_img_out, plot = tbl_grob,
       width = 16, height = nrow(disp) * row_h + 1.5, dpi = 200, bg = 'transparent')

cat('\nTable image saved to:', rev_img_out, '\n')
cat('Table CSV saved to:', rev_table_out, '\n')
