## =============================================================================
## 05_income_growth_sensitivity.R
## Sensitivity analysis: does projected income growth offset the rate increases?
##
## Objection: "Income will grow by 2030, so affordability impact won't be as bad."
## Response: Even under optimistic income growth assumptions, rate increases
## far outpace income growth, and a substantial share of households still
## face unaffordable sewer access by FY 30/31.
## =============================================================================

library(tidyverse)
library(scales)

## --- Load base data ----------------------------------------------------------

income_long <- readRDS("data/income_brackets_long.rds")

## --- Parameters --------------------------------------------------------------

## Rate schedule (annual sewer rate, single-family residential)
rate_schedule <- tibble(
  scenario       = c("Current", "FY 26/27", "FY 27/28", "FY 28/29",
                      "FY 29/30", "FY 30/31"),
  annual_rate    = c(247.13, 475.00, 575.00, 675.00, 775.00, 875.00),
  scenario_order = 1:6,
  ## Approximate years from ACS midpoint (2021) to rate year midpoint
  ## ACS 2019-2023 midpoint ~ 2021; FY 26/27 starts July 2026
  years_from_base = c(0, 5, 6, 7, 8, 9)
)

## Income growth scenarios (annual nominal growth rate)
## - 0%: no growth (baseline, what the raw ACS data reflects)
## - 2%: roughly inflation only
## - 3%: moderate real + inflation (generous for Bakersfield)
## - 5%: very optimistic (Bakersfield has not sustained this)
growth_rates <- c(0, 0.02, 0.03, 0.05)
growth_labels <- c("0% (no growth)", "2% (inflation only)",
                    "3% (moderate)", "5% (optimistic)")

## --- Reuse the affordability function from Script 02 -------------------------

calculate_unaffordable_hh <- function(bracket_data, threshold) {
  total_unaffordable <- 0
  for (i in seq_len(nrow(bracket_data))) {
    lb <- bracket_data$lower_bound[i]
    ub <- bracket_data$upper_bound[i]
    hh <- bracket_data$hh_count_weighted[i]
    if (is.na(hh) || hh == 0) next
    if (is.na(ub)) next
    if (threshold >= ub) {
      total_unaffordable <- total_unaffordable + hh
    } else if (threshold > lb && threshold < ub) {
      bracket_width <- ub - lb
      fraction_below <- (threshold - lb) / bracket_width
      total_unaffordable <- total_unaffordable + (hh * fraction_below)
    }
  }
  return(total_unaffordable)
}

## --- Run sensitivity analysis ------------------------------------------------
## For each growth rate, inflate the income brackets forward, then
## recalculate affordability at each rate step.
##
## Approach: shift bracket bounds upward by the growth factor.
## Household counts within each bracket stay the same (the distribution
## shifts rightward with income growth).

cat("Running income growth sensitivity analysis...\n\n")

# Nest income data by tract
income_nested <- income_long %>%
  arrange(GEOID, bracket_order) %>%
  group_by(GEOID) %>%
  nest(.key = "bracket_data")

# Total households (constant across scenarios)
total_hh <- sum(
  income_long %>%
    group_by(GEOID) %>%
    summarise(hh = sum(hh_count_weighted, na.rm = TRUE), .groups = "drop") %>%
    pull(hh)
)

sensitivity_results <- tibble()

for (g in seq_along(growth_rates)) {
  rate <- growth_rates[g]
  label <- growth_labels[g]
  
  for (s in seq_len(nrow(rate_schedule))) {
    yrs <- rate_schedule$years_from_base[s]
    growth_factor <- (1 + rate)^yrs
    threshold <- rate_schedule$annual_rate[s] / 0.025
    
    ## Adjust threshold downward instead of inflating all brackets
    ## (mathematically equivalent: if incomes grow by factor F,
    ## then the effective threshold in base-year dollars is threshold / F)
    adjusted_threshold <- threshold / growth_factor
    
    # Calculate unaffordable HH across all tracts
    unaff <- income_nested %>%
      rowwise() %>%
      mutate(
        unaff_hh = calculate_unaffordable_hh(bracket_data, adjusted_threshold)
      ) %>%
      ungroup() %>%
      summarise(total_unaff = sum(unaff_hh, na.rm = TRUE)) %>%
      pull(total_unaff)
    
    sensitivity_results <- bind_rows(sensitivity_results, tibble(
      growth_rate   = rate,
      growth_label  = label,
      scenario      = rate_schedule$scenario[s],
      scenario_order = rate_schedule$scenario_order[s],
      annual_rate   = rate_schedule$annual_rate[s],
      years_forward = yrs,
      growth_factor = growth_factor,
      threshold_nominal = threshold,
      threshold_base_yr = adjusted_threshold,
      unaffordable_hh   = unaff,
      pct_unaffordable  = unaff / total_hh * 100
    ))
  }
  
  cat(sprintf("  %s: FY 30/31 unaffordable = %.1f%% (%s HH)\n",
              label,
              sensitivity_results %>%
                filter(growth_label == label, scenario == "FY 30/31") %>%
                pull(pct_unaffordable),
              comma(round(sensitivity_results %>%
                filter(growth_label == label, scenario == "FY 30/31") %>%
                pull(unaffordable_hh)))))
}

## --- Rate increase vs income growth comparison -------------------------------

cat("\n=== Rate Increase vs Income Growth ===\n")
cat(sprintf("  Rate increase Current -> FY 30/31: %.0f%% ($%.0f -> $%.0f)\n",
            (875 - 247.13) / 247.13 * 100, 247.13, 875))
for (g in seq_along(growth_rates)) {
  cumulative <- (1 + growth_rates[g])^9 - 1
  cat(sprintf("  Income growth at %s over 9 years: %.0f%%\n",
              growth_labels[g], cumulative * 100))
}

## --- Figure 7: Sensitivity line chart ----------------------------------------

fig7_data <- sensitivity_results %>%
  mutate(
    scenario = factor(scenario, levels = c("Current", "FY 26/27", "FY 27/28",
                                            "FY 28/29", "FY 29/30", "FY 30/31")),
    growth_label = factor(growth_label, levels = growth_labels)
  )

fig7 <- ggplot(fig7_data, aes(x = scenario, y = pct_unaffordable,
                               color = growth_label, group = growth_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("0% (no growth)" = "#d73027",
               "2% (inflation only)" = "#fc8d59",
               "3% (moderate)" = "#fee08b",
               "5% (optimistic)" = "#1a9850"),
    name = "Annual Income Growth"
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Income Growth Does Not Offset Rate Increases",
    subtitle = paste0(
      "% of households with unaffordable sewer access under ",
      "different income growth assumptions"
    ),
    x = "Proposed Rate Schedule",
    y = "Households Exceeding 2.5% Threshold (%)",
    caption = paste0(
      "Base income: ACS 2019-2023 (midpoint ~2021) | ",
      "Growth applied forward to each fiscal year\n",
      "Rate increase = 254% over 5 years; ",
      "even 5%/yr income growth = 55% over 9 years"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.75)
  )

ggsave("output/fig7_income_growth_sensitivity.png", fig7,
       width = 10, height = 6, dpi = 300, bg = "white")

## --- Save results ------------------------------------------------------------

write_csv(sensitivity_results, "output/sensitivity_results.csv")
saveRDS(sensitivity_results, "data/sensitivity_results.rds")

cat("\nResults and figure saved.\n")

