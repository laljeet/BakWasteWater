## --- Figure 6c: Bars for other cities + Bakersfield line ---------------------

library(tidyverse)
library(scales)

other_cities <- tribble(
  ~city,              ~annual_rate, ~mhi,
  "Fresno",            309.00,      66804,
  "City of Shafter",   412.92,      67989,
  "Anaheim",           469.68,      90583,
  "Oakland",           555.60,      97369,
  "San Jose",          605.04,     141565,
  "City of Arvin",     726.64,      49984,
  "San Diego",         761.18,     104321,
  "Sacramento",        832.20,      83753,
  "Los Angeles",       910.33,      80366,
  "San Francisco",    2000.00,     141446
) %>%
  mutate(
    sewer_pct_mhi = annual_rate / mhi * 100,
    city = fct_reorder(city, sewer_pct_mhi)
  )

# Bakersfield line
bkfd_line <- tibble(
  year = c("Current", "FY 26/27", "FY 27/28", "FY 28/29", "FY 29/30", "FY 30/31"),
  rate = c(247.13, 475, 575, 675, 775, 875),
  mhi  = 77397,
  sewer_pct_mhi = rate / mhi * 100
) %>%
  mutate(y_pos = seq(0.5, nrow(other_cities) + 0.5, length.out = 6))

fig6c <- ggplot() +
  # Bars for other cities
  geom_col(data = other_cities,
           aes(x = sewer_pct_mhi, y = city),
           fill = "#7fbc41", alpha = 0.7, width = 0.6) +
  geom_text(data = other_cities,
            aes(x = sewer_pct_mhi, y = city,
                label = paste0(round(sewer_pct_mhi, 2), "%")),
            hjust = -0.2, size = 3, color = "grey30") +
  # Bakersfield line overlaid
  geom_vline(data = bkfd_line,
             aes(xintercept = sewer_pct_mhi),
             color = "#d73027", linewidth = 0.9, alpha = 0.7) +
  geom_label(data = bkfd_line,
             aes(x = sewer_pct_mhi, y = nrow(other_cities) / 2,
                 label = year),
             size = 2.5, color = "#d73027", fill = "white",
             label.padding = unit(0.15, "lines"),
             angle = 90) +
  scale_x_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Sewer Rate as Percentage of Median Household Income",
    subtitle = "Green bars = other cities (FY 2025-26 rates) | Red lines = Bakersfield sewer service area proposed rate steps",
    x = "Annual Sewer Rate as % of MHI",
    y = NULL,
    caption = paste0(
      "Sewer rates from City of Bakersfield website | ",
      "MHI from ACS 2019-2023 5-Year Estimates\n",
      "Other cities' rates will also change over this period"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 9),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("output/fig6c_bars_and_line.png", fig6c,
       width = 10, height = 6, dpi = 300, bg = "white")
