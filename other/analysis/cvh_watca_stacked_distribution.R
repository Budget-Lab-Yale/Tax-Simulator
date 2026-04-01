################################################################################
# Stacked distribution analysis for CVH-WATCA (Working Americans' Tax Cut Act)
# Produces: contribution to pct change in ATI by quintile, stacked bar chart
################################################################################

library(tidyverse)
library(scales)
library(data.table)

# --- Configuration -----------------------------------------------------------
vintage   = '202603120647'
out_root  = file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1', vintage)
year_show = 2026

# Stacked scenarios in cumulative order
scenarios = c('alt_max', 'surtax')

piece_labels = c(
  'alt_max' = 'Alternative Maximum Tax',
  'surtax'  = 'AGI Surtax'
)

colors = c(
  'Alternative Maximum Tax' = '#2166AC',
  'AGI Surtax'              = '#E41A1C'
)

dist_footnote = str_wrap("Source: The Budget Lab calculations. Note: Estimate universe is nondependent tax units, including nonfilers. 'Income' is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, and inheritances.", width = 120)

# --- Figure 1: Policy illustration — liability by AGI for two filer types -----
cat('\n===== Building policy illustration chart... =====\n')

# Read 2026 tax law parameters
tax_law_all = read_csv(file.path(out_root, 'baseline/static/supplemental/tax_law.csv'),
                       show_col_types = FALSE) %>%
  filter(year == year_show)

# Extract bracket schedules by filing status
get_brackets = function(fs) {
  tl = tax_law_all %>% filter(filing_status == fs)
  brackets = rates = c()
  for (i in 1:7) {
    brackets = c(brackets, tl[[paste0('ord.brackets', i)]])
    rates    = c(rates, tl[[paste0('ord.rates', i)]])
  }
  list(brackets = brackets, rates = rates, std_ded = tl[['std.value']])
}

sched_single  = get_brackets(1)
sched_married = get_brackets(2)

# Bracket tax calculator
calc_bracket_tax = function(agi, std_ded, brackets, rates) {
  txbl = pmax(0, agi - std_ded)
  tax  = rep(0, length(txbl))
  for (i in seq_along(rates)) {
    top = if (i < length(rates)) brackets[i + 1] else Inf
    tax = tax + rates[i] * pmax(0, pmin(txbl, top) - brackets[i])
  }
  tax
}

# Alt max parameters
alt_max_rate    = 0.255
alt_max_qualify = 1.75

# EITC calculator
calc_eitc = function(earnings, agi, pi_rate, pi_end, po_thresh, po_rate) {
  credit = pmin(pi_rate * earnings, pi_rate * pi_end)
  phaseout = pmax(0, po_rate * (agi - po_thresh))
  pmax(0, credit - phaseout)
}

# CTC calculator: returns nonrefundable + refundable (ACTC)
calc_ctc = function(earnings, sec1_tax, n_kids, ctc_per_kid, max_refund_per_kid,
                    pi_thresh, pi_rate) {
  ctc_total    = n_kids * ctc_per_kid
  ctc_nonref   = pmin(ctc_total, sec1_tax)
  ctc_remaining = ctc_total - ctc_nonref
  actc_earned  = pi_rate * pmax(0, earnings - pi_thresh)
  actc_cap     = n_kids * max_refund_per_kid
  ctc_ref      = pmin(ctc_remaining, actc_earned, actc_cap)
  list(nonref = ctc_nonref, ref = ctc_ref)
}

# Extract EITC parameters by filing status and n_kids
eitc_s0 = tax_law_all %>% filter(filing_status == 1) %>%
  transmute(pi_rate = eitc.pi_rate_0, pi_end = eitc.pi_end_0,
            po_thresh = eitc.po_thresh_0, po_rate = eitc.po_rate_0) %>% as.list()
eitc_m2 = tax_law_all %>% filter(filing_status == 2) %>%
  transmute(pi_rate = eitc.pi_rate_2, pi_end = eitc.pi_end_2,
            po_thresh = eitc.po_thresh_2, po_rate = eitc.po_rate_2) %>% as.list()

# CTC parameters
ctc_p = tax_law_all %>% filter(filing_status == 2) %>%
  transmute(per_kid = ctc.value_old1, max_refund = ctc.max_refund_old,
            pi_thresh = ctc.pi_thresh, pi_rate = ctc.pi_rate) %>% as.list()

# Build schedules for both filer types
build_filer_schedule = function(agi_seq, sched, fs_label, exempt,
                                 n_kids, eitc_pars) {
  threshold   = alt_max_qualify * exempt
  earnings    = agi_seq  # all income from earnings
  bracket_tax = calc_bracket_tax(agi_seq, sched$std_ded, sched$brackets, sched$rates)

  # EITC (refundable — same under current law and proposal)
  eitc = calc_eitc(earnings, agi_seq, eitc_pars$pi_rate, eitc_pars$pi_end,
                   eitc_pars$po_thresh, eitc_pars$po_rate)

  # Current law: bracket tax - CTC (nonref + ref) - EITC
  ctc_cl = calc_ctc(earnings, bracket_tax, n_kids, ctc_p$per_kid,
                    ctc_p$max_refund, ctc_p$pi_thresh, ctc_p$pi_rate)
  current_law = bracket_tax - ctc_cl$nonref - ctc_cl$ref - eitc

  # Proposal: alt max cap on Section 1 tax, then credits
  alt_max_tax = alt_max_rate * pmax(0, agi_seq - exempt)
  qualifies   = agi_seq < threshold
  sec1_policy = if_else(qualifies & alt_max_tax < bracket_tax, alt_max_tax, bracket_tax)
  ctc_pr = calc_ctc(earnings, sec1_policy, n_kids, ctc_p$per_kid,
                    ctc_p$max_refund, ctc_p$pi_thresh, ctc_p$pi_rate)
  proposal = sec1_policy - ctc_pr$nonref - ctc_pr$ref - eitc

  tibble(
    agi         = agi_seq,
    current_law = current_law,
    proposal    = proposal,
    filer       = fs_label,
    threshold   = threshold,
    exempt      = exempt
  )
}

agi_seq = seq(0, 200000, by = 500)

panel_data = bind_rows(
  build_filer_schedule(agi_seq, sched_single, 'Single Filer, No Children',
                       exempt = 46000, n_kids = 0, eitc_pars = eitc_s0),
  build_filer_schedule(agi_seq, sched_married, 'Married Couple, Two Children',
                       exempt = 92000, n_kids = 2, eitc_pars = eitc_m2)
)

library(patchwork)

# Base panel builder — shared logic
make_base = function(panel_title) {
  list(
    scale_x_continuous(labels = function(x) paste0('$', x, 'K'), limits = c(0, 200)),
    scale_y_continuous(labels = function(x) paste0('$', x, 'K')),
    labs(title = panel_title, x = 'Adjusted Gross Income', y = NULL),
    theme_minimal(base_size = 12),
    theme(plot.title = element_text(face = 'bold', size = 11, hjust = 0.5))
  )
}

# Compute y position inside the panel (near top of expanded range)
calc_label_y = function(vals) {
  r = range(vals)
  span = r[2] - r[1]
  r[2] + span * 0.22
}

# Helper to build a liability panel
make_liab_panel = function(fd, th, panel_title, show_legend = FALSE) {
  liab = fd %>%
    pivot_longer(cols = c(current_law, proposal),
                 names_to = 'scenario', values_to = 'value') %>%
    mutate(scenario = if_else(scenario == 'current_law', 'Current Law', 'Proposal'))
  label_y = calc_label_y(liab$value / 1000)
  ggplot(liab, aes(x = agi / 1000, y = value / 1000,
                    color = scenario, linetype = scenario)) +
    geom_vline(xintercept = th$threshold / 1000, linetype = 'dashed', color = 'grey30', linewidth = 0.6) +
    geom_vline(xintercept = th$exempt / 1000, linetype = 'dashed', color = 'grey30', linewidth = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_line(linewidth = 1) +
    annotate('label', x = th$threshold / 1000 + 1, y = label_y,
             label = paste0('Qualification Threshold\n($', round(th$threshold / 1000), 'K)'),
             size = 2.8, color = 'grey20', fill = 'white', label.padding = unit(0.2, 'lines'),
             label.size = 0, vjust = 1, hjust = 0, fontface = 'bold') +
    annotate('label', x = th$exempt / 1000 - 1, y = label_y,
             label = paste0('Exemption\n($', round(th$exempt / 1000), 'K)'),
             size = 2.8, color = 'grey20', fill = 'white', label.padding = unit(0.2, 'lines'),
             label.size = 0, vjust = 1, hjust = 1, fontface = 'bold') +
    scale_color_manual(values = c('Current Law' = 'black', 'Proposal' = '#C0392B')) +
    scale_linetype_manual(values = c('Current Law' = 'solid', 'Proposal' = 'solid')) +
    make_base(panel_title) +
    scale_y_continuous(labels = function(x) paste0('$', x, 'K'),
                       limits = c(NA, label_y * 1.15)) +
    labs(color = NULL, linetype = NULL) +
    theme(legend.position = if (show_legend) 'top' else 'none')
}

# Helper to build a change panel
make_change_panel = function(fd, th, panel_title) {
  delta = fd %>% mutate(value = proposal - current_law)
  label_y = calc_label_y(delta$value / 1000)
  ggplot(delta, aes(x = agi / 1000, y = value / 1000)) +
    geom_vline(xintercept = th$threshold / 1000, linetype = 'dashed', color = 'grey30', linewidth = 0.6) +
    geom_vline(xintercept = th$exempt / 1000, linetype = 'dashed', color = 'grey30', linewidth = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_line(linewidth = 1, color = '#C0392B') +
    annotate('label', x = th$threshold / 1000 + 1, y = label_y,
             label = paste0('Qualification Threshold\n($', round(th$threshold / 1000), 'K)'),
             size = 2.8, color = 'grey20', fill = 'white', label.padding = unit(0.2, 'lines'),
             label.size = 0, vjust = 1, hjust = 0, fontface = 'bold') +
    annotate('label', x = th$exempt / 1000 - 1, y = label_y,
             label = paste0('Exemption\n($', round(th$exempt / 1000), 'K)'),
             size = 2.8, color = 'grey20', fill = 'white', label.padding = unit(0.2, 'lines'),
             label.size = 0, vjust = 1, hjust = 1, fontface = 'bold') +
    make_base(panel_title) +
    scale_y_continuous(labels = function(x) paste0('$', x, 'K'),
                       limits = c(NA, label_y * 1.15))
}

# Build 4 panels
fd_s = panel_data %>% filter(filer == 'Single Filer, No Children')
th_s = fd_s %>% distinct(threshold, exempt)
fd_m = panel_data %>% filter(filer == 'Married Couple, Two Children')
th_m = fd_m %>% distinct(threshold, exempt)

p1 = make_liab_panel(fd_s, th_s, 'Tax Liability')
p2 = make_change_panel(fd_s, th_s, 'Change in Tax Liability')
p3 = make_liab_panel(fd_m, th_m, 'Tax Liability')
p4 = make_change_panel(fd_m, th_m, 'Change in Tax Liability')

# Shared legend extracted from a dummy plot
p_leg = ggplot(tibble(x = c(0, 1, 0, 1),
                       y = c(0, 0, 0, 0),
                       scenario = rep(c('Current Law', 'Proposal'), each = 2)),
               aes(x, y, color = scenario, linetype = scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c('Current Law' = 'black', 'Proposal' = '#C0392B')) +
  scale_linetype_manual(values = c('Current Law' = 'solid', 'Proposal' = 'solid')) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)),
         linetype = guide_legend(override.aes = list(linewidth = 1.5))) +
  labs(color = NULL, linetype = NULL) +
  theme_void() +
  theme(legend.position = 'top',
        legend.text = element_text(size = 12),
        legend.key.width = unit(2, 'lines'))
leg_grobs = ggplotGrob(p_leg)$grobs
leg_names = sapply(leg_grobs, function(x) x$name)
legend_grob = leg_grobs[[grep('guide-box', leg_names)[1]]]
shared_legend = wrap_elements(legend_grob)

# Row labels
lab_s = wrap_elements(grid::textGrob('Single Filer, No Children',
          gp = grid::gpar(fontsize = 14, fontface = 'bold')))
lab_m = wrap_elements(grid::textGrob('Married Couple, Two Children',
          gp = grid::gpar(fontsize = 14, fontface = 'bold')))

# Layout: legend, then label + panels for each filer type
# Elements in order: A=legend, B=lab_s, C=p1, D=p2, E=lab_m, F=p3, G=p4
design = "
AA
BB
CD
EE
FG
"

p_illustration = shared_legend + lab_s + p1 + p2 + lab_m + p3 + p4 +
  plot_layout(design = design, heights = c(0.06, 0.05, 1, 0.05, 1)) +
  plot_annotation(
    title    = paste0('Figure 1. Alternative Maximum Tax: Illustrative Tax Liability (', year_show, ')'),
    subtitle = 'All income from earnings; standard deduction',
    caption  = 'Source: The Budget Lab calculations.',
    theme = theme(
      plot.title    = element_text(face = 'bold', size = 14),
      plot.subtitle = element_text(color = 'grey40', size = 10),
      plot.caption  = element_text(hjust = 0, size = 8, color = 'grey40')
    )
  )

illustration_out = file.path(out_root, 'policy_illustration_liability.png')
ggsave(illustration_out, plot = p_illustration, width = 14, height = 10, dpi = 200)
cat('\nChart saved to:', illustration_out, '\n')

# --- Read distribution tables ------------------------------------------------
read_dist = function(scenario) {
  path = file.path(out_root, scenario, 'static/supplemental/distribution.csv')
  read_csv(path, show_col_types = FALSE) %>%
    mutate(scenario = scenario)
}

dist_all = map_dfr(scenarios, read_dist)

# --- Preload detail files with fread for speed --------------------------------
cat('\n===== Preloading detail files... =====\n')
detail_cols = c('id', 'weight', 'filing_status', 'agi', 'mtr_wages1',
                'dep_age1', 'dep_age2', 'dep_age3',
                'gross_ss', 'txbl_ss', 'liab_ord', 'liab_pref', 'itemizing',
                'liab_iit_net')

preload_detail = function(scn) {
  path = file.path(out_root, scn, 'static/detail', paste0(year_show, '.csv'))
  fread(path, select = detail_cols) %>% as_tibble() %>% mutate(scn = scn)
}

detail_cache = map_dfr(c('baseline', scenarios), preload_detail)
cat('  Loaded', nrow(detail_cache), 'rows across', n_distinct(detail_cache$scn), 'scenarios\n')

# --- Reusable chart builder --------------------------------------------------
build_stacked_chart = function(group_dim, groups, x_positions, x_labs,
                               separator = NULL, brackets = NULL,
                               fig_number, title_suffix, filename) {

  # Filter and compute marginal contributions
  df = dist_all %>%
    filter(
      year == year_show,
      taxes_included == 'iit_pr',
      group_dimension %in% group_dim,
      group %in% groups
    ) %>%
    select(scenario, group, pct_chg_ati) %>%
    pivot_wider(names_from = scenario, values_from = pct_chg_ati) %>%
    mutate(
      alt_max = alt_max,
      surtax  = surtax - alt_max
    ) %>%
    select(group, alt_max, surtax)

  # Print table
  cat('\n=====', title_suffix, '(', year_show, ') =====\n\n')
  df %>%
    mutate(total = alt_max + surtax) %>%
    mutate(across(where(is.numeric), ~ round(.x * 100, 2))) %>%
    print(n = Inf, width = Inf)

  # Plot data
  plot_data = df %>%
    pivot_longer(cols = c(alt_max, surtax), names_to = 'piece', values_to = 'pct_chg') %>%
    mutate(
      pct_chg = pct_chg * 100,
      piece = factor(piece, levels = c('alt_max', 'surtax'),
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

  # Optional separator
  if (!is.null(separator)) {
    p = p + geom_vline(xintercept = separator, linetype = 'dashed',
                       color = 'grey60', linewidth = 0.3)
  }

  # Winner/loser share annotations (from the full cumulative scenario)
  full_scn = scenarios[length(scenarios)]
  wl = dist_all %>%
    filter(
      year == year_show,
      taxes_included == 'iit_pr',
      group_dimension %in% group_dim,
      group %in% groups,
      scenario == full_scn
    ) %>%
    mutate(
      xpos = x_positions[as.character(group)],
      avg_label  = {
        sign_chr = if_else(avg > 0, '+', if_else(avg < 0, '-', ''))
        amt = abs(avg)
        num_str = case_when(
          amt == 0        ~ '$0',
          amt < 1000      ~ paste0('$', formatC(amt, format = 'f', digits = 0)),
          amt < 1000000   ~ paste0('$', formatC(amt / 1000, format = 'f', digits = 1), 'K'),
          TRUE            ~ paste0('$', formatC(amt / 1000000, format = 'f', digits = 1), 'M')
        )
        paste0(sign_chr, num_str)
      },
      is_quintile = grepl('^Quintile', group),
      has_overlap = any(!is_quintile & !grepl('^Negative', group)),
      net_share  = {
        denom = sum(net_change[is_quintile | grepl('^Negative', group)])
        net_change / denom * 100
      },
      net_share_label = if_else(
        has_overlap & !is_quintile,
        '--',
        paste0(formatC(net_share, format = 'f', digits = 1), '%')
      ),
      win_pct  = `share_cut.100` * 100,
      lose_pct = `share_raise.100` * 100,
      win_label  = if_else(win_pct > 0 & win_pct < 0.5, '<1%', paste0(round(win_pct), '%')),
      lose_label = if_else(lose_pct > 0 & lose_pct < 0.5, '<1%', paste0(round(lose_pct), '%'))
    )

  # Annotation rows below bars but above x-axis labels
  base_offset = if (!is.null(brackets)) 0.34 else 0.30
  row_gap     = 0.08
  avg_y  = y_range[1] - diff(y_range) * base_offset
  shr_y  = y_range[1] - diff(y_range) * (base_offset + row_gap)
  wl_y1  = y_range[1] - diff(y_range) * (base_offset + 2 * row_gap)
  wl_y2  = y_range[1] - diff(y_range) * (base_offset + 3 * row_gap)
  label_x = min(x_positions) - 0.5

  p = p +
    annotate('text', x = label_x, y = avg_y, label = 'Avg. tax change:',
             fontface = 'bold', size = 3, hjust = 1, color = 'grey30') +
    annotate('text', x = label_x, y = shr_y, label = 'Share of net change:',
             fontface = 'bold', size = 3, hjust = 1, color = 'grey30') +
    annotate('text', x = label_x, y = wl_y1, label = 'Tax cut >$100:',
             fontface = 'bold', size = 3, hjust = 1, color = 'grey30') +
    annotate('text', x = label_x, y = wl_y2, label = 'Tax hike >$100:',
             fontface = 'bold', size = 3, hjust = 1, color = 'grey30') +
    geom_text(data = wl, aes(x = xpos, y = avg_y, label = avg_label),
              inherit.aes = FALSE, size = 3, color = 'grey30') +
    geom_text(data = wl, aes(x = xpos, y = shr_y, label = net_share_label),
              inherit.aes = FALSE, size = 3, color = 'grey30') +
    geom_text(data = wl, aes(x = xpos, y = wl_y1, label = win_label),
              inherit.aes = FALSE, size = 3, color = 'grey30') +
    geom_text(data = wl, aes(x = xpos, y = wl_y2, label = lose_label),
              inherit.aes = FALSE, size = 3, color = 'grey30')

  # Optional brackets — placed at the TOP
  if (!is.null(brackets)) {
    bracket_y = y_range[2] + diff(y_range) * 0.18
    label_y_b = y_range[2] + diff(y_range) * 0.24
    for (b in brackets) {
      p = p +
        annotate('segment', x = b$x1, xend = b$x2, y = bracket_y, yend = bracket_y, color = 'grey40') +
        annotate('segment', x = b$x1, xend = b$x1, y = bracket_y, yend = bracket_y - diff(y_range) * 0.02, color = 'grey40') +
        annotate('segment', x = b$x2, xend = b$x2, y = bracket_y, yend = bracket_y - diff(y_range) * 0.02, color = 'grey40') +
        annotate('text', x = (b$x1 + b$x2) / 2, y = label_y_b, label = b$label,
                 fontface = 'bold', size = 4, color = 'grey30')
    }
  }

  # Set y-axis lower limit so the last gridline sits above the annotations
  y_lower = floor(y_range[1] * 2) / 2  # round down to nearest 0.5

  p = p +
    scale_x_continuous(breaks = x_positions, labels = x_labs) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    coord_cartesian(ylim = c(y_lower, NA), clip = 'off') +
    labs(
      title   = paste0('Figure ', fig_number, '. Contribution to Change in After-Tax Income ', title_suffix, ' (', year_show, ')'),
      x       = NULL,
      y       = 'Change in After-Tax Income (pp)',
      fill    = NULL,
      caption = dist_footnote
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x         = element_text(size = 13, face = 'bold'),
      legend.position      = 'top',
      panel.grid.major.x   = element_blank(),
      panel.grid.minor.x   = element_blank(),
      panel.grid.minor.y   = element_blank(),
      plot.title           = element_text(face = 'bold', size = 14),
      plot.caption         = element_text(hjust = 0, size = 8, color = 'grey40',
                                          margin = margin(t = 75)),
      plot.margin          = margin(30, 10, 70, 40)
    )

  out_path = file.path(out_root, filename)
  ggsave(out_path, plot = p, width = 11, height = 7, dpi = 200)
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
  fig_number   = 2,
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
  fig_number   = 3,
  title_suffix = 'by Age Group',
  filename     = 'stacked_distribution_ati_age.png'
)

# --- Chart 3: Horizontal equity (IQR of ETR within groups) ------------------
# Read horizontal equity tables from the full policy scenario
he_path = file.path(out_root, 'surtax/static/supplemental/horizontal.csv')
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
    title   = paste0('Figure 4. Within-Group IQR of Effective Tax Rate (', year_show, ')'),
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
    plot.margin         = margin(10, 10, 10, 10)
  )

he_out = file.path(out_root, 'horizontal_equity.png')
ggsave(he_out, plot = p_he, width = 10, height = 6, dpi = 200)
cat('\nChart saved to:', he_out, '\n')

# --- Chart 4: Average MTR on wages by AGI percentile, parent vs nonparent ---
cat('\n===== Building MTR chart... =====\n')

# Use preloaded detail for baseline and full policy
mtr_data = detail_cache %>%
  filter(scn %in% c('baseline', 'surtax')) %>%
  mutate(scenario = if_else(scn == 'baseline', 'Current Law', 'Proposal')) %>%
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

# Weighted average MTR by AGI percentile x parent status x scenario
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
    title   = paste0('Figure 6. Average Effective Marginal Tax Rate on Wages by AGI Percentile (', year_show, ')'),
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
    plot.margin      = margin(10, 10, 10, 10)
  )

mtr_out = file.path(out_root, 'mtr_wages_by_agi_pctile.png')
ggsave(mtr_out, plot = p_mtr, width = 12, height = 6, dpi = 200)
cat('\nChart saved to:', mtr_out, '\n')

# --- Chart 5: Stacked MTR change decomposition by AGI percentile ------------
cat('\n===== Building stacked MTR decomposition chart... =====\n')

# Use preloaded detail for all scenarios
all_detail = detail_cache %>%
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

# Weighted average MTR by percentile x parent x scenario
mtr_by_scn = all_detail %>%
  group_by(scn, parent, agi_pctile) %>%
  summarise(avg_mtr = weighted.mean(mtr_wages1, weight) * 100, .groups = 'drop')

# Pivot wide and compute marginal contributions
mtr_wide = mtr_by_scn %>%
  pivot_wider(names_from = scn, values_from = avg_mtr)

mtr_contribs = mtr_wide %>%
  mutate(
    d_alt_max = alt_max - baseline,
    d_surtax  = surtax - alt_max
  ) %>%
  select(parent, agi_pctile, alt_max = d_alt_max, surtax = d_surtax)

mtr_stack = mtr_contribs %>%
  pivot_longer(cols = c(alt_max, surtax), names_to = 'piece', values_to = 'value') %>%
  mutate(
    piece  = factor(piece, levels = c('alt_max', 'surtax'),
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
    title   = paste0('Figure 7. Contribution to Change in Average Effective Marginal Tax Rate on Wages by AGI Percentile (', year_show, ')'),
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
    plot.margin      = margin(10, 10, 10, 10)
  )

mtr_stack_out = file.path(out_root, 'mtr_wages_stacked_decomposition.png')
ggsave(mtr_stack_out, plot = p_mtr_stack, width = 12, height = 6, dpi = 200)
cat('\nChart saved to:', mtr_stack_out, '\n')

# --- Chart 6: Time burden levels (baseline vs policy) ------------------------
cat('\n===== Building time burden chart... =====\n')

# Read time burden for the full policy scenario
tb = read_csv(file.path(out_root, 'surtax/static/supplemental/time_burden.csv'),
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
    title   = paste0('Figure 5. Average Tax Filing Time Burden by Income Group (', year_show, ')'),
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
    plot.margin         = margin(10, 10, 10, 10)
  )

tb_out = file.path(out_root, 'time_burden.png')
ggsave(tb_out, plot = p_tb, width = 10, height = 6, dpi = 200)
cat('\nChart saved to:', tb_out, '\n')

# --- Smoothed stacked-share helper -------------------------------------------
# Compute raw shares at $1K AGI bins, loess-smooth each rate bin, renormalize
smooth_bin_shares = function(raw_data, bin_col, bin_levels, span = 0.15) {
  # raw_data must have: scenario, agi_bin, <bin_col>, n
  raw_data = raw_data %>%
    group_by(scenario, agi_bin, .data[[bin_col]]) %>%
    summarise(n = sum(n), .groups = 'drop') %>%
    group_by(scenario, agi_bin) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

  # Ensure all scenario × agi_bin × bin combinations exist
  grid = expand.grid(
    scenario = unique(raw_data$scenario),
    agi_bin  = unique(raw_data$agi_bin),
    bin      = bin_levels,
    stringsAsFactors = FALSE
  )
  names(grid)[3] = bin_col
  raw_data = raw_data %>%
    right_join(grid, by = c('scenario', 'agi_bin', bin_col)) %>%
    mutate(share = replace_na(share, 0), n = replace_na(n, 0))

  # Loess smooth per scenario × bin, then renormalize
  smoothed = raw_data %>%
    group_by(scenario, .data[[bin_col]]) %>%
    arrange(agi_bin) %>%
    mutate(
      share_smooth = pmax(0, predict(loess(share ~ agi_bin, span = span)))
    ) %>%
    ungroup() %>%
    group_by(scenario, agi_bin) %>%
    mutate(share_smooth = share_smooth / sum(share_smooth)) %>%
    ungroup()

  smoothed
}

# --- Chart 7: MTR bin distribution by AGI group (smoothed stacked area) ------
cat('\n===== Building MTR bin distribution chart... =====\n')

mtr_bin_levels = c('40%+', '30% to <40%', '20% to <30%', '10% to <20%', '0% to <10%', 'Below 0%')

mtr_bin_raw = detail_cache %>%
  filter(scn %in% c('baseline', 'surtax'), agi >= 0, agi <= 175000) %>%
  mutate(
    scenario = if_else(scn == 'baseline', 'Current Law', 'Proposal'),
    agi_bin  = floor(agi / 1000) * 1000,
    mtr_pct  = mtr_wages1 * 100,
    mtr_bin  = cut(mtr_pct,
      breaks = c(-Inf, 0, 10, 20, 30, 40, Inf),
      labels = rev(mtr_bin_levels),
      right  = FALSE
    ),
    mtr_bin = factor(mtr_bin, levels = mtr_bin_levels)
  ) %>%
  group_by(scenario, agi_bin, mtr_bin) %>%
  summarise(n = sum(weight), .groups = 'drop')

mtr_bin_data = smooth_bin_shares(mtr_bin_raw, 'mtr_bin', mtr_bin_levels) %>%
  mutate(
    scenario = factor(scenario, levels = c('Current Law', 'Proposal')),
    mtr_bin  = factor(mtr_bin, levels = mtr_bin_levels)
  )

mtr_bin_colors = c(
  '40%+'        = '#B2182B',
  '30% to <40%' = '#EF8A62',
  '20% to <30%' = '#FDDBC7',
  '10% to <20%' = '#D1E5F0',
  '0% to <10%'  = '#67A9CF',
  'Below 0%'    = '#2166AC'
)

p_mtr_bin = ggplot(mtr_bin_data, aes(x = agi_bin / 1000, y = share_smooth, fill = mtr_bin)) +
  geom_area(position = position_stack()) +
  facet_wrap(~ scenario) +
  scale_x_continuous(
    breaks = seq(0, 175, 25),
    labels = function(x) paste0('$', x, 'K')
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = mtr_bin_colors, name = 'Marginal Tax Rate') +
  labs(
    title   = paste0('Figure 8. Distribution of Marginal Tax Rates on Wages by AGI Group (', year_show, ')'),
    x       = 'Adjusted Gross Income',
    y       = 'Share of Tax Units',
    caption = 'Source: The Budget Lab calculations.'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = 'right',
    strip.text         = element_text(face = 'bold', size = 12),
    plot.title         = element_text(face = 'bold', size = 14),
    plot.caption       = element_text(hjust = 0, size = 8, color = 'grey40'),
    panel.grid.major.x = element_blank(),
    panel.spacing      = unit(1.5, 'lines'),
    plot.margin        = margin(10, 10, 10, 10)
  )

mtr_bin_out = file.path(out_root, 'mtr_bin_distribution_by_agi.png')
ggsave(mtr_bin_out, plot = p_mtr_bin, width = 16, height = 7, dpi = 200)
cat('\nChart saved to:', mtr_bin_out, '\n')

mtr_bin_csv = file.path(out_root, 'mtr_bin_distribution_by_agi.csv')
mtr_bin_data %>%
  select(scenario, agi_bin, mtr_bin, share) %>%
  pivot_wider(names_from = mtr_bin, values_from = share, values_fill = 0) %>%
  arrange(scenario, agi_bin) %>%
  write_csv(mtr_bin_csv)
cat('CSV saved to:', mtr_bin_csv, '\n')

# Animated GIF version: morph between Current Law and Proposal
library(gganimate)

# Aggregate share labels per scenario × mtr_bin
mtr_bin_labels = mtr_bin_raw %>%
  group_by(scenario, mtr_bin) %>%
  summarise(total_n = sum(n), .groups = 'drop') %>%
  group_by(scenario) %>%
  mutate(agg_share = total_n / sum(total_n)) %>%
  ungroup() %>%
  mutate(mtr_bin = factor(mtr_bin, levels = mtr_bin_levels)) %>%
  arrange(scenario, mtr_bin) %>%
  group_by(scenario) %>%
  mutate(
    cum_share = cumsum(agg_share),
    y_pos     = cum_share - agg_share / 2,
    label     = as.character(mtr_bin)
  ) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = c('Current Law', 'Proposal')))

p_mtr_bin_anim = ggplot(mtr_bin_data, aes(x = agi_bin / 1000, y = share_smooth, fill = mtr_bin)) +
  geom_area(position = position_stack()) +
  geom_text(data = mtr_bin_labels,
            aes(x = 165, y = y_pos, label = label),
            inherit.aes = FALSE, size = 3.2, fontface = 'bold',
            color = 'white', show.legend = FALSE) +
  scale_x_continuous(
    breaks = seq(0, 175, 25),
    labels = function(x) paste0('$', x, 'K')
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = mtr_bin_colors, name = 'Marginal Tax Rate') +
  labs(
    title   = paste0('Figure 8. Distribution of Marginal Tax Rates on Wages by AGI Group (', year_show, ')'),
    subtitle = '{closest_state}',
    x       = 'Adjusted Gross Income',
    y       = 'Share of Tax Units',
    caption = 'Source: The Budget Lab calculations.'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = 'right',
    plot.title         = element_text(face = 'bold', size = 14),
    plot.subtitle      = element_text(face = 'bold', size = 12, color = '#2166AC'),
    plot.caption       = element_text(hjust = 0, size = 8, color = 'grey40'),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(10, 10, 10, 10)
  ) +
  transition_states(scenario, transition_length = 3, state_length = 4) +
  ease_aes('cubic-in-out')

mtr_gif_out = file.path(out_root, 'mtr_bin_distribution_by_agi.gif')
anim = animate(p_mtr_bin_anim, nframes = 100, fps = 10, width = 900, height = 500,
               renderer = file_renderer(dir = tempdir(), prefix = 'mtr_frame_', overwrite = TRUE))

# Stitch PNGs into GIF with magick
library(magick)
frames = list.files(tempdir(), pattern = 'mtr_frame_', full.names = TRUE)
frames = frames[order(frames)]
imgs = image_read(frames)
gif = image_animate(imgs, fps = 10, optimize = TRUE)
image_write(gif, mtr_gif_out)
cat('\nGIF saved to:', mtr_gif_out, '\n')

# --- Chart 8: Average tax rate bin distribution by AGI group -----------------
cat('\n===== Building ATR bin distribution chart... =====\n')

atr_bin_levels = c('Negative', '0%', '0% to <5%', '5% to <10%', '10% to <15%', '15%+')

atr_bin_raw = detail_cache %>%
  filter(scn %in% c('baseline', 'surtax'), agi > 0, agi <= 175000) %>%
  mutate(
    scenario = if_else(scn == 'baseline', 'Current Law', 'Proposal'),
    agi_bin  = floor(agi / 1000) * 1000,
    atr_pct  = liab_iit_net / agi * 100,
    atr_bin  = case_when(
      atr_pct < 0   ~ 'Negative',
      atr_pct == 0  ~ '0%',
      atr_pct < 5   ~ '0% to <5%',
      atr_pct < 10  ~ '5% to <10%',
      atr_pct < 15  ~ '10% to <15%',
      TRUE          ~ '15%+'
    ),
    atr_bin = factor(atr_bin, levels = atr_bin_levels)
  ) %>%
  group_by(scenario, agi_bin, atr_bin) %>%
  summarise(n = sum(weight), .groups = 'drop')

atr_bin_data = smooth_bin_shares(atr_bin_raw, 'atr_bin', atr_bin_levels, span = 0.15) %>%
  mutate(scenario = factor(scenario, levels = c('Current Law', 'Proposal')))

atr_bin_colors = c(
  '15%+'         = '#B2182B',
  '10% to <15%'  = '#EF8A62',
  '5% to <10%'   = '#FDDBC7',
  '0% to <5%'    = '#D1E5F0',
  '0%'           = '#67A9CF',
  'Negative'     = '#2166AC'
)

# Static faceted version
p_atr_bin = ggplot(atr_bin_data, aes(x = agi_bin / 1000, y = share_smooth, fill = atr_bin)) +
  geom_area(position = position_stack()) +
  facet_wrap(~ scenario) +
  scale_x_continuous(
    breaks = seq(0, 175, 25),
    labels = function(x) paste0('$', x, 'K')
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = atr_bin_colors, name = 'Average Tax Rate') +
  labs(
    title   = paste0('Figure 9. Distribution of Average Income Tax Rates by AGI Group (', year_show, ')'),
    x       = 'Adjusted Gross Income',
    y       = 'Share of Tax Units',
    caption = 'Source: The Budget Lab calculations. Average tax rate = net income tax liability / AGI.'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = 'right',
    strip.text         = element_text(face = 'bold', size = 12),
    plot.title         = element_text(face = 'bold', size = 14),
    plot.caption       = element_text(hjust = 0, size = 8, color = 'grey40'),
    panel.grid.major.x = element_blank(),
    panel.spacing      = unit(1.5, 'lines'),
    plot.margin        = margin(10, 10, 10, 10)
  )

atr_bin_out = file.path(out_root, 'atr_bin_distribution_by_agi.png')
ggsave(atr_bin_out, plot = p_atr_bin, width = 16, height = 7, dpi = 200)
cat('\nChart saved to:', atr_bin_out, '\n')

# CSV
atr_bin_csv = file.path(out_root, 'atr_bin_distribution_by_agi.csv')
atr_bin_data %>%
  select(scenario, agi_bin, atr_bin, share) %>%
  pivot_wider(names_from = atr_bin, values_from = share, values_fill = 0) %>%
  arrange(scenario, agi_bin) %>%
  write_csv(atr_bin_csv)
cat('CSV saved to:', atr_bin_csv, '\n')

# Animated GIF
atr_bin_labels = atr_bin_raw %>%
  group_by(scenario, atr_bin) %>%
  summarise(total_n = sum(n), .groups = 'drop') %>%
  group_by(scenario) %>%
  mutate(agg_share = total_n / sum(total_n)) %>%
  ungroup() %>%
  arrange(scenario, atr_bin) %>%
  group_by(scenario) %>%
  mutate(
    cum_share = cumsum(agg_share),
    y_pos     = cum_share - agg_share / 2,
    label     = as.character(atr_bin)
  ) %>%
  ungroup()

p_atr_bin_anim = ggplot(atr_bin_data, aes(x = agi_bin / 1000, y = share_smooth, fill = atr_bin)) +
  geom_area(position = position_stack()) +
  geom_text(data = atr_bin_labels,
            aes(x = 165, y = y_pos, label = label),
            inherit.aes = FALSE, size = 3.2, fontface = 'bold',
            color = 'white', show.legend = FALSE) +
  scale_x_continuous(
    breaks = seq(0, 175, 25),
    labels = function(x) paste0('$', x, 'K')
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = atr_bin_colors, name = 'Average Tax Rate') +
  labs(
    title   = paste0('Figure 9. Distribution of Average Income Tax Rates by AGI Group (', year_show, ')'),
    subtitle = '{closest_state}',
    x       = 'Adjusted Gross Income',
    y       = 'Share of Tax Units',
    caption = 'Source: The Budget Lab calculations. Average tax rate = net income tax liability / AGI.'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = 'right',
    plot.title         = element_text(face = 'bold', size = 14),
    plot.subtitle      = element_text(face = 'bold', size = 12, color = '#2166AC'),
    plot.caption       = element_text(hjust = 0, size = 8, color = 'grey40'),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(10, 10, 10, 10)
  ) +
  transition_states(scenario, transition_length = 3, state_length = 4) +
  ease_aes('cubic-in-out')

atr_gif_out = file.path(out_root, 'atr_bin_distribution_by_agi.gif')
anim_atr = animate(p_atr_bin_anim, nframes = 100, fps = 10, width = 900, height = 500,
                   renderer = file_renderer(dir = tempdir(), prefix = 'atr_frame_', overwrite = TRUE))

atr_frames = list.files(tempdir(), pattern = 'atr_frame_', full.names = TRUE)
atr_frames = atr_frames[order(atr_frames)]
atr_imgs = image_read(atr_frames)
atr_gif = image_animate(atr_imgs, fps = 10, optimize = TRUE)
image_write(atr_gif, atr_gif_out)
cat('\nGIF saved to:', atr_gif_out, '\n')

# --- Notch analysis: Alt Max qualification cliff -----------------------------
cat('\n===== Alt Max Notch Analysis =====\n')

# Use preloaded baseline detail
baseline_detail = detail_cache %>% filter(scn == 'baseline') %>% select(-scn)

# Read the alt_max tax law params from the scenario's tax_law.csv
# (hardcoded for robustness — these are the 2026 values from the YAML)
alt_max_rate       = 0.255
alt_max_qualify    = 1.75

# Compute MAGI, exemption, qualification, and notch size
notch_data = baseline_detail %>%
  mutate(
    magi = agi + pmax(0, gross_ss - txbl_ss),
    exempt = case_when(
      filing_status == 2 ~ 92000,
      filing_status == 3 ~ 46000,   # MFS = married / 2
      filing_status == 4 ~ 64400,
      TRUE               ~ 46000    # single
    ),
    threshold  = alt_max_qualify * exempt,
    qualifies  = magi < threshold & !(filing_status == 3 & itemizing),
    sec1_tax   = liab_ord + liab_pref,  # uncapped section 1 tax
    capped_tax = alt_max_rate * pmax(0, magi - exempt),
    cap_binds  = qualifies & capped_tax < sec1_tax,
    notch_size = if_else(cap_binds, sec1_tax - capped_tax, 0),
    dist_to_threshold = threshold - magi
  )

# Summary stats
n_total     = sum(notch_data$weight)
n_cap_binds = sum(notch_data$weight[notch_data$cap_binds])

cat('\nTotal filers:', formatC(round(n_total / 1e6, 1), format = 'f', digits = 1), 'M\n')
cat('Filers where cap binds:', formatC(round(n_cap_binds / 1e6, 1), format = 'f', digits = 1), 'M',
    paste0('(', round(n_cap_binds / n_total * 100, 1), '%)\n'))

# Among those where cap binds
capped = notch_data %>% filter(cap_binds)
cat('\nAmong filers where cap binds:\n')
cat('  Avg notch size (benefit lost at cliff): $',
    formatC(round(weighted.mean(capped$notch_size, capped$weight)), big.mark = ','), '\n')
# Weighted median via sorted cumulative weight
capped_sorted = capped %>% arrange(notch_size)
cum_w = cumsum(capped_sorted$weight) / sum(capped_sorted$weight)
w_median = capped_sorted$notch_size[which(cum_w >= 0.5)[1]]
cat('  Median notch size: $', formatC(round(w_median), big.mark = ','), '\n')
cat('  Avg distance to threshold: $',
    formatC(round(weighted.mean(capped$dist_to_threshold, capped$weight)),
            format = 'f', digits = 0, big.mark = ','), '\n')

# Cross-tab: proximity to cliff x tax increase if cap lost
# Universe: filers where alt-max cap binds
cat('\nShare of capped filers by proximity to threshold and size of tax increase if cap lost:\n')
cat('(Rows: distance to losing alt-max; Columns: tax increase under alt-max loss)\n\n')

crosstab = notch_data %>%
  filter(cap_binds) %>%
  mutate(
    dist_bucket = cut(dist_to_threshold,
      breaks = c(-Inf, 1000, 5000, 10000, 25000, 50000, Inf),
      labels = c('< $1K', '$1K-$5K', '$5K-$10K', '$10K-$25K', '$25K-$50K', '$50K+'),
      right = TRUE
    ),
    notch_bucket = cut(notch_size,
      breaks = c(-Inf, 500, 1000, 2000, 5000, Inf),
      labels = c('< $500', '$500-$1K', '$1K-$2K', '$2K-$5K', '$5K+'),
      right = TRUE
    )
  )

total_capped = sum(crosstab$weight)

# Build the cross-tab with row and column totals
ct = crosstab %>%
  group_by(dist_bucket, notch_bucket) %>%
  summarise(n = sum(weight), .groups = 'drop') %>%
  mutate(share = n / total_capped * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = notch_bucket, values_from = share, values_fill = 0)

# Add row totals
ct = ct %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Add column totals
col_totals = ct %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(dist_bucket = 'Total')

ct = bind_rows(ct, col_totals)

# Print with formatting
ct %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print(n = Inf, width = Inf)

cat('\nN capped filers:', formatC(round(total_capped / 1e6, 1), format = 'f', digits = 1), 'M\n')

# Also report weighted average notch size by distance bucket
cat('\nAverage tax increase by proximity to threshold:\n')
crosstab %>%
  group_by(dist_bucket) %>%
  summarise(
    n_millions = round(sum(weight) / 1e6, 1),
    avg_notch  = round(weighted.mean(notch_size, weight)),
    .groups = 'drop'
  ) %>%
  print(n = Inf, width = Inf)

# --- Notch visualization: ETR schedule with and without cap ------------------
cat('\n===== Building notch visualization... =====\n')

# Simulate a single filer with no deductions, no SS, no cap gains
# across a range of MAGI values to show the ETR cliff
exempt_single = 46000
threshold_single = alt_max_qualify * exempt_single

# Read baseline tax law to get the 2026 ordinary rate schedule (single filer)
tax_law = read_csv(file.path(out_root, 'baseline/static/supplemental/tax_law.csv'),
                   show_col_types = FALSE) %>%
  filter(year == year_show, filing_status == 1)

brackets = c()
rates    = c()
for (i in 1:7) {
  bname = paste0('ord.brackets', i)
  rname = paste0('ord.rates', i)
  if (bname %in% names(tax_law) && rname %in% names(tax_law)) {
    brackets = c(brackets, tax_law[[bname]])
    rates    = c(rates, tax_law[[rname]])
  }
}

std_ded = tax_law[['std.value']]

# Compute regular tax on taxable income using the bracket schedule
calc_regular_tax = function(agi, std_ded, brackets, rates) {
  txbl = pmax(0, agi - std_ded)
  tax = rep(0, length(txbl))
  for (i in seq_along(rates)) {
    top = if (i < length(rates)) brackets[i + 1] else Inf
    tax = tax + rates[i] * pmax(0, pmin(txbl, top) - brackets[i])
  }
  tax
}

# Build schedule across MAGI range
magi_range = seq(0, 120000, by = 200)
schedule = tibble(
  magi = magi_range,
  regular_tax = calc_regular_tax(magi_range, std_ded, brackets, rates),
  capped_tax  = alt_max_rate * pmax(0, magi_range - exempt_single),
  qualifies   = magi_range < threshold_single,
  effective_tax = if_else(qualifies & capped_tax < regular_tax, capped_tax, regular_tax),
  etr_regular = regular_tax / pmax(1, magi_range) * 100,
  etr_policy  = effective_tax / pmax(1, magi_range) * 100
)

# Find the notch point
notch_magi = threshold_single
notch_below = schedule %>% filter(magi == max(magi[magi < notch_magi]))
notch_above_tax = calc_regular_tax(notch_magi, std_ded, brackets, rates)
notch_above_etr = notch_above_tax / notch_magi * 100

p_notch = ggplot(schedule, aes(x = magi / 1000)) +
  # Regular tax ETR (dashed)
  geom_line(aes(y = etr_regular, color = 'Current Law'), linewidth = 0.8, linetype = 'dashed') +
  # Policy ETR (solid)
  geom_line(aes(y = etr_policy, color = 'With Alt Max Cap'), linewidth = 1.2) +
  # Notch annotation
  annotate('segment',
    x = notch_magi / 1000, xend = notch_magi / 1000,
    y = notch_below$etr_policy, yend = notch_above_etr,
    color = '#E41A1C', linewidth = 1.5, arrow = arrow(length = unit(0.15, 'inches'))
  ) +
  annotate('text',
    x = notch_magi / 1000 + 3, y = (notch_below$etr_policy + notch_above_etr) / 2,
    label = paste0('Notch: ',
      round(notch_above_etr - notch_below$etr_policy, 1), ' pp\njump at $',
      formatC(round(notch_magi / 1000), format = 'f', digits = 0), 'K'),
    hjust = 0, size = 3.5, fontface = 'bold', color = '#E41A1C'
  ) +
  # Threshold line
  geom_vline(xintercept = notch_magi / 1000, linetype = 'dotted', color = 'grey50') +
  # Exemption line
  geom_vline(xintercept = exempt_single / 1000, linetype = 'dotted', color = 'grey70') +
  annotate('text', x = exempt_single / 1000, y = -0.8, label = 'Exemption\n($46K)',
           size = 2.8, color = 'grey50') +
  annotate('text', x = notch_magi / 1000, y = -0.8, label = paste0('175% Threshold\n($',
           round(notch_magi / 1000), 'K)'), size = 2.8, color = 'grey50') +
  scale_color_manual(values = c('Current Law' = '#636363', 'With Alt Max Cap' = '#2166AC')) +
  scale_x_continuous(labels = function(x) paste0('$', x, 'K'), breaks = seq(0, 120, 20)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  coord_cartesian(ylim = c(-1, 25), clip = 'off') +
  labs(
    title    = paste0('Figure 8. Effective Tax Rate Schedule: Single Filer (', year_show, ')'),
    subtitle = 'Section 1 income tax only, standard deduction, no capital gains',
    x        = 'Modified Adjusted Gross Income',
    y        = 'Effective Tax Rate',
    color    = NULL,
    caption  = 'Source: The Budget Lab calculations. Section 1 income tax only.'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = 'top',
    plot.title       = element_text(face = 'bold', size = 14),
    plot.subtitle    = element_text(color = 'grey40', size = 10),
    plot.caption     = element_text(hjust = 0, size = 8, color = 'grey40'),
    plot.margin      = margin(10, 10, 30, 10)
  )

notch_out = file.path(out_root, 'alt_max_notch_etr.png')
ggsave(notch_out, plot = p_notch, width = 10, height = 6, dpi = 200)
cat('\nChart saved to:', notch_out, '\n')

# --- Table: Stacked revenue estimates with % of GDP ---------------------------
cat('\n===== Building stacked revenue table... =====\n')

# Read revenue estimates for each cumulative scenario
read_rev = function(scn) {
  read_csv(file.path(out_root, scn, 'conventional/supplemental/revenue_estimates.csv'),
           show_col_types = FALSE) %>%
    mutate(scn = scn)
}

rev = map_dfr(scenarios, read_rev) %>%
  pivot_wider(names_from = scn, values_from = total)

# Marginal contributions
rev_stacked = rev %>%
  mutate(
    `Alternative Maximum Tax` = alt_max,
    `AGI Surtax`              = surtax - alt_max,
    `Total`                   = surtax
  ) %>%
  select(year, `Alternative Maximum Tax`, `AGI Surtax`, Total)

# Read GDP projections
gdp = read_csv('/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline/projections.csv',
               show_col_types = FALSE) %>%
  select(year, gdp_fy)

rev_stacked = rev_stacked %>% left_join(gdp, by = 'year')

# Build summary rows for budget windows
provisions = c('Alternative Maximum Tax', 'AGI Surtax', 'Total')

make_window = function(df, start, end, label) {
  sub = df %>% filter(year >= start, year <= end)
  gdp_total = sum(sub$gdp_fy)
  tibble(
    year           = label,
    `Alternative Maximum Tax`  = sum(sub$`Alternative Maximum Tax`),
    `AGI Surtax`               = sum(sub$`AGI Surtax`),
    `Total`        = sum(sub$Total),
    gdp_fy         = gdp_total
  )
}

windows = bind_rows(
  make_window(rev_stacked, 2026, 2035, '2026-2035'),
  make_window(rev_stacked, 2036, 2045, '2036-2045'),
  make_window(rev_stacked, 2046, 2055, '2046-2055')
)

# --- Single wide table: provisions as rows, years + windows as columns ---
# Annual dollars (wide)
annual_wide = rev_stacked %>%
  filter(year >= 2026, year <= 2035) %>%
  mutate(year = as.character(year)) %>%
  select(year, all_of(provisions)) %>%
  pivot_longer(cols = all_of(provisions), names_to = 'Provision', values_to = 'val') %>%
  pivot_wider(names_from = year, values_from = val)

# Window dollars + % GDP
window_labels = c('2026-2035', '2036-2045', '2046-2055')
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
       width = 16, height = nrow(disp) * row_h + 1.5, dpi = 200)

cat('\nTable image saved to:', rev_img_out, '\n')
cat('Table CSV saved to:', rev_table_out, '\n')
