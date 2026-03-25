## =============================================================================
## 04_rate_comparison.R
## Recreate the City of Bakersfield "How do we compare?" chart, adding
## income-normalized sewer burden (rate as % of MHI) alongside absolute rates.
##
## The city compares absolute dollar rates to other CA cities. This script
## shows that normalizing by median household income reverses the picture.
## =============================================================================

library(tidyverse)
library(scales)

## --- Data from the city's comparison chart and ACS 2019-2023 5-yr MHI --------
## Sewer rates from city's website (FY 2025-26 comparison chart)
## MHI from Census Reporter / Data USA / ACS 2019-2023 5-year estimates
##
## Notes:
## - "Kern County - KSA" = Kern Sanitation Authority; using Kern County MHI
## - "NORSD" = North of River Sanitary District (serves Oildale area);
##   using Oildale CDP / 93308 zip MHI as proxy
## - Bakersfield proposed = FY 30/31 endpoint rate

comparison <- tribble(
  ~city,                  ~annual_rate, ~mhi,
  "Bakersfield (Current)",  247.13,      77397,
  "Bakersfield (FY 26/27)", 475.00,      77397,
  "Bakersfield (FY 27/28)", 575.00,      77397,
  "Bakersfield (FY 28/29)", 675.00,      77397,
  "Bakersfield (FY 29/30)", 775.00,      77397,
  "Bakersfield (FY 30/31)", 875.00,      77397,
  "Fresno",                 309.00,      66804,
  "City of Shafter",        412.92,      67989,
  "Anaheim",                469.68,      90583,
  "Oakland",                555.60,      97369,
  "San Jose",               605.04,     141565,
  "City of Arvin",          726.64,      49984,
  "San Diego",              761.18,     104321,
  "Sacramento",             832.20,      83753,
  "Los Angeles",            910.33,      80366,
  "San Francisco",         2000.00,     141446
) %>%
  mutate(sewer_pct_mhi = annual_rate / mhi * 100)

comparison <- comparison %>%
  mutate(
    sewer_pct_mhi = annual_rate / mhi * 100,
    city = factor(city, levels = city)  # preserve order
  )

cat("=== Rate Comparison with MHI Normalization ===\n\n")
print(comparison %>%
        select(city, annual_rate, mhi, sewer_pct_mhi) %>%
        mutate(sewer_pct_mhi = round(sewer_pct_mhi, 2)))

## --- Figure 6a: Absolute rate comparison (city's version) --------------------

fig6a <- ggplot(comparison, aes(x = city, y = annual_rate)) +
  geom_col(
    aes(fill = grepl("Bakersfield", city)),
    width = 0.7, alpha = 0.85
  ) +
  geom_text(aes(label = paste0("$", comma(annual_rate))),
            vjust = -0.4, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#7fbc41"),
                    guide = "none") +
  scale_y_continuous(
    labels = dollar_format(),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Annual Sewer Rate Comparison (Absolute Dollars)",
    subtitle = "Replicating City of Bakersfield FY 2025-26 chart + proposed FY 30/31 rate",
    x = NULL,
    y = "Annual Sewer Rate ($)",
    caption = "Source: City of Bakersfield sewer rate website; proposed rate from Prop 218 notice"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("output/fig6a_absolute_rate_comparison.png", fig6a,
       width = 11, height = 6, dpi = 300, bg = "white")

## --- Figure 6b: Income-normalized comparison (rate as % of MHI) --------------

fig6b <- ggplot(comparison, aes(x = city, y = sewer_pct_mhi)) +
  geom_col(
    aes(fill = grepl("Bakersfield", city)),
    width = 0.7, alpha = 0.85
  ) +
  geom_text(aes(label = paste0(round(sewer_pct_mhi, 2), "%")),
            vjust = -0.4, size = 3, fontface = "bold") +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "red",
             linewidth = 0.7) +
  annotate("text", x = 13, y = 2.6,
           label = "USEPA 2.5% threshold", color = "red",
           size = 3, hjust = 0.5, vjust = 0) +
  scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#7fbc41"),
                    guide = "none") +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Annual Sewer Rate as Percentage of Median Household Income",
    subtitle = "Same cities as city's comparison chart, normalized by ACS MHI",
    x = NULL,
    y = "Sewer Rate as % of MHI",
    caption = paste0(
      "Sewer rates from City of Bakersfield website (FY 2025-26) | ",
      "MHI from ACS 2019-2023 5-Year Estimates\n",
      "Red dashed line = USEPA 2.5% affordability threshold for wastewater"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("output/fig6b_mhi_normalized_comparison.png", fig6b,
       width = 11, height = 6, dpi = 300, bg = "white")

cat("\nFigures saved to output/\n")
