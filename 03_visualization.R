## =============================================================================
## 03_visualization.R
## Generate maps and summary figures for the Bakersfield sewer affordability
## analysis.
## =============================================================================

library(tidyverse)
library(sf)
library(scales)

## --- Load data ----------------------------------------------------------------

results        <- readRDS("data/affordability_results.rds")
tracts_spatial <- readRDS("data/tracts_spatial.rds")
sewer_boundary  <- readRDS("data/bakersfield_boundary.rds")
summary_table  <- readRDS("data/summary_table.rds")

## --- Color palette (consistent with Maxcy-Brown et al. 2024) -----------------

afford_colors <- c(
  "< 25%"      = "#2166ac",   # blue - mostly affordable
  "25% - 49.9%" = "#fdb863",  # light orange - emerging concern
  "50% - 74.9%" = "#e66101",  # dark orange - widespread
  ">= 75%"     = "#b2182b"   # red - severe
)

mhi_colors <- c(
  "< 1.00%"      = "#1a9850",
  "1.00% - 1.99%" = "#a6d96a",
  "2.00% - 2.49%" = "#fee08b",
  ">= 2.50%"     = "#d73027"
)

## --- Figure 1: Bar chart - % HH unaffordable by rate scenario ----------------

fig1_data <- summary_table %>%
  mutate(
    scenario = factor(scenario, levels = c("Current", "FY 26/27", "FY 27/28",
                                            "FY 28/29", "FY 29/30", "FY 30/31")),
    label = paste0(round(pct_unaffordable, 1), "%\n(",
                   scales::comma(round(unaffordable_hh)), " HH)")
  )

fig1 <- ggplot(fig1_data, aes(x = scenario, y = pct_unaffordable)) +
  geom_col(fill = "#d73027", alpha = 0.85, width = 0.65) +
  geom_text(aes(label = label), vjust = -0.5, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Estimated Households with Unaffordable Sewer Access",
    subtitle = "Bakersfield sewer service area | Households exceeding 2.5% of income threshold",
    x = "Proposed Rate Schedule",
    y = "Households Exceeding Affordability Threshold (%)",
    caption = paste0(
      "Data: ACS 2019-2023 5-Year Estimates (Table B19001) | ",
      "Method: Maxcy-Brown et al. (2024)\n",
      "Rate schedule from City of Bakersfield Prop 218 notice (Feb 2026)"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("output/fig1_unaffordable_hh_by_scenario.png", fig1,
       width = 8, height = 5, dpi = 300, bg = "white")

## --- Figure 2: Comparison - MHI-based vs bracket-based -----------------------
## Demonstrates the masking effect of using MHI alone.

fig2_data <- summary_table %>%
  mutate(
    scenario = factor(scenario, levels = c("Current", "FY 26/27", "FY 27/28",
                                            "FY 28/29", "FY 29/30", "FY 30/31")),
    mhi_based_pct = hh_in_mhi_unaffordable_tracts / total_hh * 100
  ) %>%
  select(scenario, mhi_based_pct, pct_unaffordable) %>%
  pivot_longer(
    cols = c(mhi_based_pct, pct_unaffordable),
    names_to = "method",
    values_to = "pct"
  ) %>%
  mutate(
    method = case_when(
      method == "mhi_based_pct" ~ "MHI-Based (traditional)",
      method == "pct_unaffordable" ~ "Income Bracket-Based"
    )
  )

fig2 <- ggplot(fig2_data, aes(x = scenario, y = pct, fill = method)) +
  geom_col(position = "dodge", width = 0.65, alpha = 0.85) +
  scale_fill_manual(
    values = c("MHI-Based (traditional)" = "#4393c3",
               "Income Bracket-Based" = "#d73027"),
    name = "Method"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "MHI-Based Analysis Underestimates Affordability Challenges",
    subtitle = "Comparison of traditional vs. income bracket-based affordability assessment",
    x = "Proposed Rate Schedule",
    y = "Households with Unaffordable Sewer Access (%)",
    caption = paste0(
      "MHI-based: household in unaffordable tract if rate >= 2.5% of tract MHI\n",
      "Income bracket-based: interpolation within income brackets ",
      "(Maxcy-Brown et al. 2024)"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave("output/fig2_mhi_vs_bracket_comparison.png", fig2,
       width = 9, height = 5.5, dpi = 300, bg = "white")

## --- Figure 3: Map - % HH unaffordable at FY 30/31 rate ----------------------

# Join results to spatial data for the final rate scenario
map_data <- tracts_spatial %>%
  left_join(
    results %>%
      filter(scenario == "FY 30/31") %>%
      select(GEOID, pct_unaffordable, unaffordable_hh, mhi_pct_spent),
    by = "GEOID"
  ) %>%
  mutate(
    afford_cat = cut(
      pct_unaffordable,
      breaks = c(-Inf, 25, 50, 75, Inf),
      labels = c("< 25%", "25% - 49.9%", "50% - 74.9%", ">= 75%"),
      right = FALSE
    )
  )

library(ggspatial)

fig3 <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 11, alpha = 0.3) +
  geom_sf(data = sewer_boundary, fill = NA, color = "grey50",
          linewidth = 0.8) +
  geom_sf(data = map_data, aes(fill = afford_cat),
          color = "grey40", linewidth = 0.2, alpha = 0.75) +
  scale_fill_manual(
    values = afford_colors,
    name = "% Households with\nUnaffordable Access",
    na.value = "grey80"
  ) +
  labs(
    title = "Households with Unaffordable Sewer Access at Proposed FY 30/31 Rate ($875/yr)",
    subtitle = "Bakersfield sewer service area census tracts | 2.5% threshold",
    caption = paste0(
      "Data: ACS 2019-2023 (B19001) \n",
      "Grey boundary = Sewer service area"
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    legend.position = c(0.85, 0.25)
  )

ggsave("output/fig3_map_fy3031.png", fig3,
       width = 9, height = 8, dpi = 300, bg = "white")

library(leaflet)
library(htmlwidgets)

# Prepare data for FY 30/31
map_interactive <- tracts_spatial %>%
  left_join(
    results %>%
      filter(scenario == "FY 30/31") %>%
      select(GEOID, pct_unaffordable, unaffordable_hh, mhi_pct_spent),
    by = "GEOID"
  ) %>%
  mutate(
    pct_unaffordable = round(pct_unaffordable, 0),
    mhi_pct_spent = round(mhi_pct_spent, 2),
    mhi_dollars = scales::comma(round(mhi, 0)),
    popup_text = paste0(
      "<b>MHI:</b> $", mhi_dollars, "<br>",
      "<b>Unaffordable:</b> ", pct_unaffordable, "%<br>",
      "<b>Sewer as % of MHI:</b> ", mhi_pct_spent, "%"
    )
  ) %>%
  st_transform(4326)

# Color palette matching static map
pal <- colorBin(
  palette = c("#2166ac", "#fdb863", "#e66101", "#b2182b"),
  domain = map_interactive$pct_unaffordable,
  bins = c(0, 25, 50, 75, 100)
)

m <- leaflet(map_interactive) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(pct_unaffordable),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~popup_text
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~pct_unaffordable,
    title = "% HH Unaffordable",
    labFormat = labelFormat(suffix = "%")
  ) %>%
  addControl(
    html = paste0(
      "<div style='font-size:11px; color:grey; padding:4px;'>",
      "<b>Click a tract for details.</b> MHI = Median Household Income. ",
      "Unaffordable = % of households spending &gt;2.5% of income on sewer. ",
      "Sewer as % of MHI = $875 / tract MHI.</div>"
    ),
    position = "bottomleft"
  )
saveWidget(m, "output/map_fy3031_interactive.html", selfcontained = TRUE)
## --- Figure 4: Map - % MHI spent on sewer at FY 30/31 (traditional method) ---

map_data_mhi <- map_data %>%
  mutate(
    mhi_cat = cut(
      mhi_pct_spent,
      breaks = c(-Inf, 1.0, 2.0, 2.5, Inf),
      labels = c("< 1.00%", "1.00% - 1.99%", "2.00% - 2.49%", ">= 2.50%"),
      right = FALSE
    )
  )

fig4 <- ggplot() +
  geom_sf(data = sewer_boundary, fill = "grey95", color = "grey50",
          linewidth = 0.8) +
  geom_sf(data = map_data_mhi, aes(fill = mhi_cat),
          color = "grey40", linewidth = 0.2) +
  scale_fill_manual(
    values = mhi_colors,
    name = "% of MHI Spent\non Sewer",
    na.value = "grey80"
  ) +
  labs(
    title = "Sewer Expenses as Percentage of Median Household Income at FY 30/31 Rate",
    subtitle = "City of Bakersfield census tracts | Traditional MHI-based assessment",
    caption = paste0(
      "Data: ACS 2019-2023 (B19013) | ",
      "Threshold: 2.5% of MHI (USEPA)\n",
      "Compare with income bracket-based analysis (Fig. 3) ",
      "to see masking effect of MHI"
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    legend.position = c(0.15, 0.25)
  )

ggsave("output/fig4_map_mhi_fy3031.png", fig4,
       width = 9, height = 8, dpi = 300, bg = "white")

## --- Figure 5: Sewer burden as % of income by bracket (FY 30/31) -------------

income_brackets_summary <- readRDS("data/income_brackets_long.rds") %>%
  group_by(bracket_order, lower_bound, upper_bound) %>%
  summarise(
    total_hh = sum(hh_count_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    midpoint = ifelse(is.na(upper_bound), 250000,
                      (lower_bound + upper_bound) / 2),
    sewer_pct_875 = 875 / midpoint * 100,
    sewer_pct_247 = 247.13 / midpoint * 100,
    bracket_label = case_when(
      is.na(upper_bound) ~ "$200k+",
      lower_bound >= 100000 ~ paste0("$", lower_bound / 1000, "k-$",
                                      upper_bound / 1000, "k"),
      TRUE ~ paste0("$", lower_bound / 1000, "k-$",
                    (upper_bound + 1) / 1000, "k")
    ),
    bracket_label = fct_reorder(bracket_label, bracket_order)
  )

fig5 <- ggplot(income_brackets_summary,
               aes(x = bracket_label, y = sewer_pct_875)) +
  geom_col(aes(fill = sewer_pct_875 >= 2.5), width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "red",
             linewidth = 0.8) +
  annotate("text", x = 14, y = 2.8, label = "USEPA 2.5% threshold",
           color = "red", size = 3, hjust = 0) +
  geom_point(aes(y = sewer_pct_247), color = "grey40", size = 2, shape = 4) +
  scale_fill_manual(
    values = c("FALSE" = "#4393c3", "TRUE" = "#d73027"),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Sewer Rate as Percentage of Income by Household Income Bracket",
    subtitle = paste0(
      "Bars = proposed FY 30/31 rate ($875/yr) | ",
      "X markers = current rate ($247/yr)"
    ),
    x = "Annual Household Income Bracket",
    y = "Annual Sewer Rate as % of Income (bracket midpoint)",
    caption = paste0(
      "Red bars exceed USEPA 2.5% affordability threshold | ",
      "Data: ACS 2019-2023 (B19001)"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("output/fig5_burden_by_bracket.png", fig5,
       width = 10, height = 5.5, dpi = 300, bg = "white")

## --- Summary table for the letter/report -------------------------------------

cat("\n=== Final Summary for Report ===\n\n")
summary_table %>%
  mutate(
    scenario = factor(scenario, levels = c("Current", "FY 26/27", "FY 27/28",
                                            "FY 28/29", "FY 29/30", "FY 30/31"))
  ) %>%
  arrange(scenario) %>%
  transmute(
    Scenario = scenario,
    `Annual Rate ($)` = annual_rate,
    `Income Threshold ($)` = comma(income_threshold),
    `Total HH` = comma(round(total_hh)),
    `Unaffordable HH` = comma(round(unaffordable_hh)),
    `% Unaffordable` = paste0(round(pct_unaffordable, 1), "%"),
    `Tracts MHI > 2.5%` = tracts_mhi_unaffordable
  ) %>%
  print(n = Inf)

cat("\nAll figures saved to output/ directory.\n")

## --- Figure 2b: MHI-based only -----------------------------------------------

## --- Figure 2b: MHI-based only (same format as Fig 1) ------------------------

fig2b_data <- summary_table %>%
  mutate(
    scenario = factor(scenario, levels = c("Current", "FY 26/27", "FY 27/28",
                                           "FY 28/29", "FY 29/30", "FY 30/31")),
    mhi_based_pct = hh_in_mhi_unaffordable_tracts / total_hh * 100,
    mhi_based_hh  = hh_in_mhi_unaffordable_tracts,
    label = paste0(round(mhi_based_pct, 1), "%\n(",
                   scales::comma(round(mhi_based_hh)), " HH)")
  )

fig2b <- ggplot(fig2b_data, aes(x = scenario, y = mhi_based_pct)) +
  geom_col(fill = "#4393c3", alpha = 0.85, width = 0.65) +
  geom_text(aes(label = label), vjust = -0.5, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_y_continuous(
    limits = c(0, 30),
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Estimated Households with Unaffordable Sewer Access",
    subtitle = "City of Bakersfield | MHI-based method (traditional)",
    x = "Proposed Rate Schedule",
    y = "Households Exceeding Affordability Threshold (%)",
    caption = paste0(
      "Data: ACS 2019-2023 5-Year Estimates (Table B19013) | ",
      "Threshold: rate >= 2.5% of tract MHI\n",
      "Rate schedule from City of Bakersfield Prop 218 notice (Feb 2026)"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("output/fig2b_mhi_only.png", fig2b,
       width = 8, height = 5, dpi = 300, bg = "white")

fig5b <- ggplot(income_brackets_summary,
                aes(x = bracket_label, y = total_hh,
                    fill = sewer_pct_875 >= 2.5)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(aes(label = scales::comma(round(total_hh))),
            vjust = -0.3, size = 2.5, color = "grey30") +
  scale_fill_manual(
    values = c("TRUE" = "#d73027", "FALSE" = "#4393c3"),
    labels = c("TRUE" = "Above 2.5% threshold",
               "FALSE" = "Below 2.5% threshold"),
    name = NULL
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Households by Income Bracket in Bakersfield Sewer Service Area",
    subtitle = "Red = sewer rate ($875/yr) exceeds 2.5% of bracket midpoint income",
    x = "Annual Household Income Bracket",
    y = "Number of Households",
    caption = "Data: ACS 2019-2023 (B19001) | FY 30/31 rate ($875/yr)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )
ggsave("output/fig5b_Households by Income Bracket.png", fig5b,
       width = 8, height = 5, dpi = 300, bg = "white")
