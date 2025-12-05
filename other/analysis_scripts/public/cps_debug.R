library(tidyverse)

cps <- read_csv('C:/Users/jar335/Downloads/cps_00052.csv.gz', show_col_types = FALSE)
cps <- cps %>% filter(YEAR == max(YEAR))

# ============================================================================
# KEY QUESTION: Does including INCRETIR change the median?
# ============================================================================

cat('Rows:', nrow(cps), '\n')
cat('Unique households:', n_distinct(paste(cps$YEAR, cps$SERIAL)), '\n\n')

# Check column names
cat('Columns:\n')
print(names(cps))

# Check if INCRETI1/INCRETI2 exist
cat('\nRetirement vars exist:', 'INCRETI1' %in% names(cps), 'INCRETI2' %in% names(cps), '\n')

# Check for large values that might be NIU codes
cat('\nMax values in income vars:\n')
income_vars <- c('INCWAGE', 'INCBUS', 'INCFARM', 'INCINT', 'INCDIVID', 'INCRENT')
for (v in income_vars) {
  if (v %in% names(cps)) {
    cat(v, ': max=', max(cps[[v]], na.rm=TRUE),
        ' min=', min(cps[[v]], na.rm=TRUE),
        ' n_99999999=', sum(cps[[v]] == 99999999, na.rm=TRUE), '\n')
  }
}

# Compare weighted medians with different cleaning approaches
cat('\n--- Weighted Median Comparison ---\n')

weighted_median <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0
  x <- x[valid]
  w <- w[valid]
  if (length(x) == 0) return(NA_real_)
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  x[which(cum_w >= 0.5)[1]]
}

# My approach: explicit codes
clean_income_explicit <- function(x) {
  x <- as.numeric(x)
  x <- if_else(x %in% c(99999999, 9999999, 999999) | x < -99999, 0, x)
  replace_na(x, 0)
}

# GPT approach: all 9s regex
clean_income_regex <- function(x) {
  x <- as.numeric(x)
  # Values that are all 9s
  all_9s <- !is.na(x) & grepl("^9+$", as.character(as.integer(x)))
  x[all_9s] <- NA
  replace_na(x, 0)
}

# Build household market income - EXPLICIT approach
cps_explicit <- cps %>%
  mutate(across(c(INCWAGE, INCBUS, INCFARM, INCINT, INCDIVID, INCRENT), clean_income_explicit)) %>%
  mutate(person_market = INCWAGE + INCBUS + INCFARM + INCINT + INCDIVID + INCRENT) %>%
  group_by(YEAR, SERIAL) %>%
  summarise(
    hh_market = sum(person_market, na.rm = TRUE),
    ASECWTH = first(ASECWTH),
    .groups = 'drop'
  )

# Build household market income - REGEX approach
cps_regex <- cps %>%
  mutate(across(c(INCWAGE, INCBUS, INCFARM, INCINT, INCDIVID, INCRENT), clean_income_regex)) %>%
  mutate(person_market = INCWAGE + INCBUS + INCFARM + INCINT + INCDIVID + INCRENT) %>%
  group_by(YEAR, SERIAL) %>%
  summarise(
    hh_market = sum(person_market, na.rm = TRUE),
    ASECWTH = first(ASECWTH),
    .groups = 'drop'
  )

# NO cleaning - raw values
cps_raw <- cps %>%
  mutate(person_market = INCWAGE + INCBUS + INCFARM + INCINT + INCDIVID + INCRENT) %>%
  group_by(YEAR, SERIAL) %>%
  summarise(
    hh_market = sum(person_market, na.rm = TRUE),
    ASECWTH = first(ASECWTH),
    .groups = 'drop'
  )

cat('Median (explicit cleaning): $', format(round(weighted_median(cps_explicit$hh_market, cps_explicit$ASECWTH)), big.mark=','), '\n', sep='')
cat('Median (regex cleaning):    $', format(round(weighted_median(cps_regex$hh_market, cps_regex$ASECWTH)), big.mark=','), '\n', sep='')
cat('Median (no cleaning):       $', format(round(weighted_median(cps_raw$hh_market, cps_raw$ASECWTH)), big.mark=','), '\n', sep='')

# Check number of households
cat('\nNumber of households:\n')
cat('  Explicit: ', nrow(cps_explicit), '\n')
cat('  Regex:    ', nrow(cps_regex), '\n')
cat('  Raw:      ', nrow(cps_raw), '\n')

# Check how many values got cleaned
cat('\nValues cleaned (set to 0):\n')
for (v in income_vars) {
  if (v %in% names(cps)) {
    explicit_cleaned <- sum(cps[[v]] %in% c(99999999, 9999999, 999999) | cps[[v]] < -99999, na.rm=TRUE)
    regex_cleaned <- sum(grepl("^9+$", as.character(as.integer(cps[[v]]))), na.rm=TRUE)
    cat(v, ': explicit=', explicit_cleaned, ' regex=', regex_cleaned, '\n')
  }
}

# Check the actual distribution of market income
cat('\nMarket income distribution (explicit cleaning):\n')
print(quantile(cps_explicit$hh_market, probs = c(0.1, 0.25, 0.4, 0.45, 0.5, 0.55, 0.6, 0.75, 0.9)))

# ============================================================================
# KEY TEST: With vs Without INCRETIR
# ============================================================================

cat('\n\n========== INCLUDING RETIREMENT INCOME (INCRETIR) ==========\n')

# Check INCRETIR values
cat('INCRETIR: max=', max(cps$INCRETIR, na.rm=TRUE),
    ' n_9999999=', sum(cps$INCRETIR == 9999999, na.rm=TRUE), '\n\n')

# WITH retirement (INCRETIR)
cps_with_ret <- cps %>%
  mutate(across(c(INCWAGE, INCBUS, INCFARM, INCINT, INCDIVID, INCRENT, INCRETIR), clean_income_explicit)) %>%
  mutate(person_market = INCWAGE + INCBUS + INCFARM + INCINT + INCDIVID + INCRENT + INCRETIR) %>%
  group_by(YEAR, SERIAL) %>%
  summarise(hh_market = sum(person_market), ASECWTH = first(ASECWTH), .groups = 'drop')

cat('Median WITHOUT retirement (INCRETIR): $',
    format(round(weighted_median(cps_explicit$hh_market, cps_explicit$ASECWTH)), big.mark=','), '\n', sep='')
cat('Median WITH retirement (INCRETIR):    $',
    format(round(weighted_median(cps_with_ret$hh_market, cps_with_ret$ASECWTH)), big.mark=','), '\n', sep='')

cat('\nDifference: $',
    format(round(weighted_median(cps_with_ret$hh_market, cps_with_ret$ASECWTH) -
                 weighted_median(cps_explicit$hh_market, cps_explicit$ASECWTH)), big.mark=','), '\n', sep='')
