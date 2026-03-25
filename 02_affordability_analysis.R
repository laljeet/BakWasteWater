## =============================================================================
## 02_affordability_analysis.R
## Apply the Maxcy-Brown et al. (2024) affordability methodology to
## the proposed Bakersfield sewer rate schedule.
##
## Method: For each census tract, determine the income threshold below which
## sewer charges exceed 2.5% of annual household income. Then use linear
## interpolation within ACS income brackets to estimate the number of
## households below that threshold.
## =============================================================================

library(tidyverse)

## --- Load processed data -----------------------------------------------------

income_long    <- readRDS("data/income_brackets_long.rds")
tracts_spatial <- readRDS("data/tracts_spatial.rds")

## --- Diagnostics: inspect the data structure ----------------------------------

cat("=== Data structure check ===\n")
cat(sprintf("  Unique tracts: %d\n", n_distinct(income_long$GEOID)))
cat(sprintf("  Rows in income_long: %d\n", nrow(income_long)))
cat(sprintf("  Rows per tract: %d (should be 16)\n",
            nrow(income_long) / n_distinct(income_long$GEOID)))

# Check a sample tract
sample_tract <- income_long %>%
  filter(GEOID == first(GEOID)) %>%
  arrange(bracket_order)

cat("\nSample tract:", sample_tract$GEOID[1], "\n")
cat(sprintf("  Total HH (weighted): %.1f\n", sample_tract$total_hh_weighted[1]))
cat(sprintf("  Sum of bracket HH (weighted): %.1f\n",
            sum(sample_tract$hh_count_weighted)))
cat(sprintf("  MHI: $%.0f\n", sample_tract$mhi[1]))
cat("  Brackets:\n")
print(sample_tract %>%
        select(bracket_order, lower_bound, upper_bound,
               hh_count_weighted) %>%
        as.data.frame(),
      row.names = FALSE)

## --- Define rate schedule -----------------------------------------------------

rate_schedule <- tibble(
  scenario       = c("Current", "FY 26/27", "FY 27/28", "FY 28/29",
                      "FY 29/30", "FY 30/31"),
  annual_rate    = c(247.13, 475.00, 575.00, 675.00, 775.00, 875.00),
  scenario_order = 1:6
) %>%
  mutate(
    income_threshold = annual_rate / 0.025
  )

cat("\nRate schedule with affordability thresholds:\n")
print(rate_schedule)

## --- Core affordability calculation per tract ---------------------------------

calculate_unaffordable_hh <- function(bracket_data, threshold) {
  ## bracket_data must have: lower_bound, upper_bound, hh_count_weighted
  ## arranged by bracket_order
  ## Returns: number of households with income below threshold
  
  total_unaffordable <- 0
  
  for (i in seq_len(nrow(bracket_data))) {
    lb <- bracket_data$lower_bound[i]
    ub <- bracket_data$upper_bound[i]
    hh <- bracket_data$hh_count_weighted[i]
    
    # Skip if no households in this bracket
    if (is.na(hh) || hh == 0) next
    
    # Top bracket ($200,000+): open-ended, cannot interpolate
    if (is.na(ub)) {
      # For sewer affordability, threshold will never reach $200k+
      # so these households are all affordable
      next
    }
    
    if (threshold >= ub) {
      # Entire bracket below threshold: all unaffordable
      total_unaffordable <- total_unaffordable + hh
    } else if (threshold > lb && threshold < ub) {
      # Threshold falls within this bracket: linear interpolation
      bracket_width <- ub - lb
      fraction_below <- (threshold - lb) / bracket_width
      total_unaffordable <- total_unaffordable + (hh * fraction_below)
    }
    # If threshold <= lb: bracket is entirely affordable, skip
  }
  
  return(total_unaffordable)
}

## --- Apply to all tracts x rate scenarios ------------------------------------

cat("\nCalculating unaffordable households per tract per scenario...\n")

# Pre-compute tract-level summaries
tract_summary <- income_long %>%
  group_by(GEOID) %>%
  summarise(
    total_hh_weighted = first(total_hh_weighted),
    sum_bracket_hh    = sum(hh_count_weighted, na.rm = TRUE),
    mhi               = first(mhi),
    .groups = "drop"
  )

# Nest the income data by tract for efficient calculation
income_nested <- income_long %>%
  arrange(GEOID, bracket_order) %>%
  group_by(GEOID) %>%
  nest(.key = "bracket_data")

# Cross tracts with scenarios
results <- expand_grid(
  GEOID = unique(income_long$GEOID),
  scenario_order = rate_schedule$scenario_order
) %>%
  left_join(rate_schedule, by = "scenario_order") %>%
  left_join(income_nested, by = "GEOID") %>%
  rowwise() %>%
  mutate(
    unaffordable_hh = calculate_unaffordable_hh(bracket_data, income_threshold)
  ) %>%
  ungroup() %>%
  select(-bracket_data) %>%
  left_join(tract_summary, by = "GEOID") %>%
  mutate(
    # Use sum of bracket households as denominator (more reliable than total_hh
    # which can differ due to rounding in ACS)
    pct_unaffordable = ifelse(sum_bracket_hh > 0,
                               unaffordable_hh / sum_bracket_hh * 100,
                               0),
    # Cap at 100% as a sanity check
    pct_unaffordable = pmin(pct_unaffordable, 100),
    # Traditional MHI-based affordability
    mhi_pct_spent    = annual_rate / mhi * 100,
    mhi_unaffordable = mhi_pct_spent >= 2.5
  )

## --- Diagnostic: check for implausible values --------------------------------

cat("\n=== Diagnostic: per-tract pct_unaffordable distribution ===\n")
for (s in rate_schedule$scenario) {
  r <- results %>% filter(scenario == s)
  cat(sprintf("  %s: min=%.1f%%, median=%.1f%%, max=%.1f%%, >100%%: %d tracts\n",
              s,
              min(r$pct_unaffordable, na.rm = TRUE),
              median(r$pct_unaffordable, na.rm = TRUE),
              max(r$pct_unaffordable, na.rm = TRUE),
              sum(r$pct_unaffordable > 100, na.rm = TRUE)))
}

## --- Summary statistics ------------------------------------------------------

cat("\n=== SUMMARY: Households with Unaffordable Sewer Access ===\n\n")

summary_table <- results %>%
  group_by(scenario, annual_rate, income_threshold, scenario_order) %>%
  summarise(
    n_tracts             = n(),
    total_hh             = sum(sum_bracket_hh, na.rm = TRUE),
    unaffordable_hh      = sum(unaffordable_hh, na.rm = TRUE),
    pct_unaffordable     = unaffordable_hh / total_hh * 100,
    # MHI-based: count tracts where rate >= 2.5% of MHI
    tracts_mhi_unaffordable = sum(mhi_unaffordable, na.rm = TRUE),
    hh_in_mhi_unaffordable_tracts = sum(
      sum_bracket_hh[mhi_unaffordable], na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(scenario_order)

cat("Bracket-based results:\n")
print(summary_table %>%
        transmute(
          scenario,
          annual_rate,
          income_threshold = scales::comma(income_threshold),
          total_hh         = scales::comma(round(total_hh)),
          unaffordable_hh  = scales::comma(round(unaffordable_hh)),
          pct_unaffordable = paste0(round(pct_unaffordable, 1), "%"),
          tracts_mhi_over  = tracts_mhi_unaffordable
        ))

## --- Comparison: MHI-based vs bracket-based -----------------------------------

cat("\n=== MHI-Based vs Income Bracket-Based Comparison ===\n\n")

comparison <- summary_table %>%
  mutate(
    mhi_based_pct     = hh_in_mhi_unaffordable_tracts / total_hh * 100,
    bracket_based_pct = pct_unaffordable,
    ratio             = ifelse(mhi_based_pct > 0,
                                bracket_based_pct / mhi_based_pct,
                                NA_real_)
  ) %>%
  select(scenario, annual_rate, mhi_based_pct, bracket_based_pct, ratio)

print(comparison %>% mutate(across(where(is.numeric), ~round(., 1))))

## --- Distributional analysis by income level ---------------------------------

cat("\n=== Burden by Income Bracket at FY 30/31 Rate ($875/yr) ===\n\n")

burden_by_bracket <- income_long %>%
  group_by(bracket_order, lower_bound, upper_bound) %>%
  summarise(
    total_hh = sum(hh_count_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    midpoint = ifelse(is.na(upper_bound), 250000,
                      (lower_bound + upper_bound) / 2),
    sewer_pct_of_income = 875 / midpoint * 100,
    bracket_label = case_when(
      is.na(upper_bound) ~ paste0("$", scales::comma(lower_bound), "+"),
      TRUE ~ paste0("$", scales::comma(lower_bound), "-$",
                    scales::comma(upper_bound))
    )
  )

print(burden_by_bracket %>%
        select(bracket_label, total_hh, sewer_pct_of_income) %>%
        mutate(total_hh = round(total_hh),
               sewer_pct_of_income = round(sewer_pct_of_income, 2)))

## --- Save results ------------------------------------------------------------

saveRDS(results, "data/affordability_results.rds")
saveRDS(summary_table, "data/summary_table.rds")
write_csv(summary_table, "output/summary_table.csv")
write_csv(results, "output/tract_level_results.csv")

cat("\nResults saved to data/ and output/ directories.\n")
