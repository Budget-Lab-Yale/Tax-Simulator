################################################################################
# Distribution analysis for the OBBBA + 1997 rate hybrid (Clausing-Sarin)
# Produces: Figure 1, % change in after-tax income by income group, single bars
#   with circular net-change labels, a per-group annotation table
#   (avg tax change / share of net change / tax cut & hike >$100), and
#   Quintiles + Top Decile Breakout brackets.
#
# Mirrors the layout of other/analysis/booker_kypa_stacked_distribution.R but
# charts a SINGLE scenario rather than a stacked decomposition.
################################################################################

library(tidyverse)
library(scales)

# --- Configuration -----------------------------------------------------------
# Points at the start-in-2026 policy run (clausing_2026_policy), which sets the
# full hybrid effective in 2026 and computes the distribution table for 2026.
# Overridable via env vars (CLAUSING_VINTAGE / CLAUSING_YEAR / CLAUSING_SCENARIO);
# defaults are the start-in-2026 run.
vintage   = Sys.getenv('CLAUSING_VINTAGE',  'clausing_2026_policy')
out_root  = file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1', vintage)
year_show = as.integer(Sys.getenv('CLAUSING_YEAR', '2026'))  # distribution year shown
scenario  = Sys.getenv('CLAUSING_SCENARIO', '06_niit_reform') # full hybrid scenario ID

bar_color = '#8B2222'             # deep maroon to match the original figure

# Subtitle. Leave rev_label = NULL to auto-compute the 10-year revenue and % of
# GDP from revenue_estimates.csv + Macro-Projections; or hard-code it to match
# an existing figure exactly (e.g. '$4.9 trillion (1.3% of GDP)').
subtitle_prefix = 'Hybrid (OBBBA Bottom + 1997 Upper)'
rev_label       = NULL
rev_window      = NULL            # c(start, end); NULL => first 10 years in file
gdp_path        = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline/projections.csv'

# --- Read distribution table -------------------------------------------------
dist = read_csv(file.path(out_root, scenario, 'static/supplemental/distribution.csv'),
                show_col_types = FALSE)

# --- Build the 10-year revenue subtitle --------------------------------------
build_rev_label = function() {
  rev = read_csv(file.path(out_root, scenario, 'static/supplemental/revenue_estimates.csv'),
                 show_col_types = FALSE)
  win = if (is.null(rev_window)) {
    s = min(rev$year); c(s, s + 9)
  } else rev_window
  rev_sub = rev %>% filter(year >= win[1], year <= win[2])
  rev_total = sum(rev_sub$total)                       # $ billions

  gdp = read_csv(gdp_path, show_col_types = FALSE) %>% select(year, gdp_fy)
  gdp_total = gdp %>% filter(year >= win[1], year <= win[2]) %>% pull(gdp_fy) %>% sum()

  pct_gdp = rev_total / gdp_total * 100
  paste0('$', formatC(rev_total / 1000, format = 'f', digits = 1),
         ' trillion (', formatC(pct_gdp, format = 'f', digits = 1), '% of GDP)')
}

if (is.null(rev_label)) rev_label = tryCatch(build_rev_label(), error = function(e) {
  warning('Could not auto-compute revenue: ', conditionMessage(e)); NA_character_
})

chart_subtitle = if (is.na(rev_label)) subtitle_prefix else
  paste0(subtitle_prefix, '   |   10-Year Revenue: ', rev_label)

# --- Footnote ----------------------------------------------------------------
dist_footnote = str_wrap("Source: The Budget Lab calculations. Note: Estimate universe is nondependent tax units, including nonfilers. 'Income' is measured as AGI plus: above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits), nondeductible capital losses, employer-side payroll taxes, and inheritances.", width = 120)

# --- Chart builder -----------------------------------------------------------
build_dist_chart = function(group_dim, groups, x_positions, x_labs,
                            separator = NULL, brackets = NULL,
                            fig_number, title_suffix, filename,
                            plot_height = 9) {

  # Single-scenario bar data: % change in after-tax income
  df = dist %>%
    filter(
      year == year_show,
      taxes_included == 'iit_pr_wealth',
      group_dimension == group_dim,
      group %in% groups
    )

  plot_data = df %>%
    transmute(
      group = factor(group, levels = groups),
      xpos  = x_positions[as.character(group)],
      pct_chg = pct_chg_ati * 100
    )

  # Print table
  cat('\n=====', title_suffix, '(', year_show, ') =====\n\n')
  plot_data %>% mutate(pct_chg = round(pct_chg, 2)) %>% print(n = Inf, width = Inf)

  # Net change bubbles (one bar => net == bar height)
  net_dots = plot_data %>%
    mutate(net = pct_chg,
           net_label = paste0(ifelse(net >= 0, '+', ''),
                              formatC(round(net, 1), format = 'f', digits = 1)))

  # y-range from bars
  y_range = c(min(0, min(plot_data$pct_chg)), max(0, max(plot_data$pct_chg)))

  y_lower = floor(y_range[1] * 2) / 2
  y_upper = y_range[2] + diff(y_range) * (if (!is.null(brackets)) 0.30 else 0.15)
  vis_range = y_upper - y_lower

  plot_area_inches = plot_height - 2.5
  dpi = vis_range / plot_area_inches

  xlab_gap    = dpi * (if (!is.null(brackets)) 1.35 else 1.00)
  row_step    = dpi * 0.25
  footnote_gap = dpi * 0.45

  avg_y  = y_lower - xlab_gap
  shr_y  = avg_y - row_step
  wl_y1  = shr_y - row_step
  wl_y2  = wl_y1 - row_step
  foot_y = wl_y2 - footnote_gap
  label_x = min(x_positions) - 0.5

  p = ggplot(plot_data, aes(x = xpos, y = pct_chg)) +
    geom_col(width = 0.7, fill = bar_color) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_point(data = net_dots, aes(x = xpos, y = net),
               inherit.aes = FALSE, shape = 21, size = 10,
               fill = 'white', color = 'black', stroke = 0.8) +
    geom_text(data = net_dots, aes(x = xpos, y = net, label = net_label),
              inherit.aes = FALSE, size = 2.8, fontface = 'bold')

  if (!is.null(separator)) {
    p = p + geom_vline(xintercept = separator, linetype = 'dashed',
                       color = 'grey60', linewidth = 0.3)
  }

  # Per-group annotation table (avg tax change / share of net change / win-lose)
  wl = df %>%
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
      net_share  = {
        denom = if (group_dim == 'Income') {
          sum(net_change[grepl('^Quintile', group)])
        } else {
          sum(net_change)
        }
        net_change / denom * 100
      },
      net_share_label = paste0(formatC(net_share, format = 'f', digits = 1), '%'),
      win_pct  = `share_cut.100` * 100,
      lose_pct = `share_raise.100` * 100,
      win_label  = if_else(win_pct > 0 & win_pct < 0.5, '<1%', paste0(round(win_pct), '%')),
      lose_label = if_else(lose_pct > 0 & lose_pct < 0.5, '<1%', paste0(round(lose_pct), '%'))
    )

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

  p = p +
    annotate('text', x = label_x, y = foot_y, label = dist_footnote,
             hjust = 0, vjust = 1, size = 2.8, color = 'grey40', lineheight = 1.1)

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

  total_below = xlab_gap + 4 * row_step + footnote_gap + dpi * 0.45
  bottom_margin_pt = total_below / vis_range * plot_area_inches * 72

  p = p +
    scale_x_continuous(breaks = x_positions, labels = x_labs) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    coord_cartesian(ylim = c(y_lower, y_upper), clip = 'off') +
    labs(
      title    = paste0('Figure ', fig_number, '. Change in After-Tax Income ', title_suffix, ' (', year_show, ')'),
      subtitle = chart_subtitle,
      x        = NULL,
      y        = 'Change in After-Tax Income (pp)'
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x         = element_text(size = 13, face = 'bold'),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor.x  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      plot.title          = element_text(face = 'bold', size = 14),
      plot.subtitle       = element_text(size = 11, color = 'grey40'),
      plot.margin         = margin(30, 10, bottom_margin_pt, 40)
    )

  out_path = file.path(out_root, filename)
  ggsave(out_path, plot = p, width = 11, height = plot_height, dpi = 200, bg = 'transparent')
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

build_dist_chart(
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
  filename     = 'clausing_hybrid_distribution_ati.png'
)
