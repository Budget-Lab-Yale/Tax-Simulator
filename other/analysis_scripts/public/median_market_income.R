# =============================================================================
# Median Market Income Analysis: GDP Shock Impact
# =============================================================================
#
# Market income definition (ex-retirement):
#   - Wages and salaries
#   - Business income (self-employment, farm)
#   - Capital income (interest, dividends, rent)
#   - EXCLUDES: Social Security, UI, private pensions/retirement
#
# Two data sources:
#   1. CPS ASEC (Census households)
#   2. Tax-Data (IRS tax units)
#
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# PARAMETERS
# -----------------------------------------------------------------------------

gdp_shock_pct <- -0.5

tax_data_path <- "C:/Users/jar335/Documents/Interfaces/model_data/Tax-Data/v1/2025060316/baseline"
cps_path <- "C:/Users/jar335/Downloads/cps_00052.csv.gz"
tax_data_year <- 2025

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

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

clean_income <- function(x) {
  x <- as.numeric(x)
  x <- if_else(x %in% c(99999999, 9999999, 999999) | x < -99999, 0, x)
  replace_na(x, 0)
}

# =============================================================================
# CPS ANALYSIS (Census Households)
# =============================================================================

cat("=============================================================================\n")
cat("CPS ASEC ANALYSIS (Census Households)\n")
cat("=============================================================================\n\n")

cps_raw <- read_csv(cps_path, show_col_types = FALSE)
cps_year <- max(cps_raw$YEAR)
cps <- cps_raw %>% filter(YEAR == cps_year)

cat("Year:", cps_year, "(income reference year:", cps_year - 1, ")\n\n")

# Market income: wages + business + capital (NO retirement)
cps <- cps %>%
  mutate(
    across(c(INCWAGE, INCBUS, INCFARM, INCINT, INCDIVID, INCRENT), clean_income),
    person_market_income = INCWAGE + INCBUS + INCFARM + INCINT + INCDIVID + INCRENT
  )

cps_hh <- cps %>%
  group_by(YEAR, SERIAL) %>%
  summarise(
    hh_market_income = sum(person_market_income, na.rm = TRUE),
    ASECWTH = first(ASECWTH),
    .groups = "drop"
  )

cps_median <- weighted_median(cps_hh$hh_market_income, cps_hh$ASECWTH)
cps_mean <- weighted.mean(cps_hh$hh_market_income, cps_hh$ASECWTH)
cps_n <- sum(cps_hh$ASECWTH)

cat("Market income definition: wages + business + capital (ex-retirement)\n\n")
cat("  Median: $", format(round(cps_median), big.mark = ","), "\n", sep = "")
cat("  Mean:   $", format(round(cps_mean), big.mark = ","), "\n", sep = "")
cat("  N:      ", format(round(cps_n), big.mark = ","), " households\n\n", sep = "")

# =============================================================================
# TAX-DATA ANALYSIS (IRS Tax Units)
# =============================================================================

cat("=============================================================================\n")
cat("TAX-DATA ANALYSIS (IRS Tax Units)\n")
cat("=============================================================================\n\n")

tax_units <- read_csv(
  file.path(tax_data_path, paste0("tax_units_", tax_data_year, ".csv")),
  show_col_types = FALSE
)

cat("Year:", tax_data_year, "(projection)\n\n")

# Market income: wages + business + capital (NO retirement distributions)
tax_units <- tax_units %>%
  mutate(
    market_income =
      wages +
      txbl_int + exempt_int + div_ord + div_pref +
      kg_st + kg_lt + kg_1250 + kg_collect + other_gains +
      sole_prop +
      part_active - part_active_loss + part_passive - part_passive_loss +
      scorp_active - scorp_active_loss + scorp_passive - scorp_passive_loss +
      rent - rent_loss + estate - estate_loss +
      farm +
      other_inc + alimony
      # NOTE: excluding txbl_ira_dist and txbl_pens_dist
  )

primary_units <- tax_units %>% filter(dep_status == 0)

tax_median <- weighted_median(primary_units$market_income, primary_units$weight)
tax_mean <- weighted.mean(primary_units$market_income, primary_units$weight)
tax_n <- sum(primary_units$weight)

cat("Market income definition: wages + business + capital (ex-retirement)\n\n")
cat("  Median: $", format(round(tax_median), big.mark = ","), "\n", sep = "")
cat("  Mean:   $", format(round(tax_mean), big.mark = ","), "\n", sep = "")
cat("  N:      ", format(round(tax_n), big.mark = ","), " tax units\n\n", sep = "")

# =============================================================================
# GDP SHOCK IMPACT (0.5% decline)
# =============================================================================

cat("=============================================================================\n")
cat("GDP SHOCK IMPACT (", gdp_shock_pct, "% decline)\n", sep = "")
cat("=============================================================================\n\n")

cat("Assumption: Percent pass-through — all units experience same % change\n")
cat("            in market income as aggregate GDP.\n\n")

# CPS calculations
cps_median_loss <- cps_median * (gdp_shock_pct / 100)
cps_mean_loss <- cps_mean * (gdp_shock_pct / 100)

# Tax-Data calculations
tax_median_loss <- tax_median * (gdp_shock_pct / 100)
tax_mean_loss <- tax_mean * (gdp_shock_pct / 100)

cat("CPS HOUSEHOLDS:\n")
cat("  Median market income:  $", format(round(cps_median), big.mark = ","), "\n", sep = "")
cat("  Mean market income:    $", format(round(cps_mean), big.mark = ","), "\n", sep = "")
cat("  Median annual loss:    $", format(round(cps_median_loss), big.mark = ","),
    " (", gdp_shock_pct, "%)\n", sep = "")
cat("  Mean annual loss:      $", format(round(cps_mean_loss), big.mark = ","),
    " (", gdp_shock_pct, "%)\n", sep = "")
cat("  Median monthly loss:   $", format(round(cps_median_loss / 12), big.mark = ","), "\n", sep = "")
cat("  Mean monthly loss:     $", format(round(cps_mean_loss / 12), big.mark = ","), "\n\n", sep = "")

cat("TAX UNITS:\n")
cat("  Median market income:  $", format(round(tax_median), big.mark = ","), "\n", sep = "")
cat("  Mean market income:    $", format(round(tax_mean), big.mark = ","), "\n", sep = "")
cat("  Median annual loss:    $", format(round(tax_median_loss), big.mark = ","),
    " (", gdp_shock_pct, "%)\n", sep = "")
cat("  Mean annual loss:      $", format(round(tax_mean_loss), big.mark = ","),
    " (", gdp_shock_pct, "%)\n", sep = "")
cat("  Median monthly loss:   $", format(round(tax_median_loss / 12), big.mark = ","), "\n", sep = "")
cat("  Mean monthly loss:     $", format(round(tax_mean_loss / 12), big.mark = ","), "\n\n", sep = "")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("=============================================================================\n")
cat("SUMMARY\n")
cat("=============================================================================\n\n")

cat(sprintf("%-25s  %12s  %12s  %12s  %12s\n",
            "", "Median Inc", "Mean Inc", "Median Loss", "Mean Loss"))
cat(sprintf("%-25s  %12s  %12s  %12s  %12s\n",
            "-------------------------", "------------", "------------", "------------", "------------"))
cat(sprintf("%-25s  $%11s  $%11s  $%11s  $%11s\n",
            "CPS Households",
            format(round(cps_median), big.mark = ","),
            format(round(cps_mean), big.mark = ","),
            format(round(cps_median_loss), big.mark = ","),
            format(round(cps_mean_loss), big.mark = ",")))
cat(sprintf("%-25s  $%11s  $%11s  $%11s  $%11s\n",
            "Tax Units",
            format(round(tax_median), big.mark = ","),
            format(round(tax_mean), big.mark = ","),
            format(round(tax_median_loss), big.mark = ","),
            format(round(tax_mean_loss), big.mark = ",")))

cat("\n")
cat("Note: Market income = wages + business + capital income (ex-retirement).\n")
cat("      Loss calculated as ", gdp_shock_pct, "% of income (percent pass-through assumption).\n", sep = "")
