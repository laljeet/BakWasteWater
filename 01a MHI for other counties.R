library(tidycensus)

# Pull MHI for all comparison cities from ACS 2019-2023 5-year
city_fips <- tribble(
  ~city,            ~state, ~place_fips,
  "Bakersfield",    "CA",   "03526",
  "Fresno",         "CA",   "27000",
  "Anaheim",        "CA",   "02000",
  "Oakland",        "CA",   "53000",
  "San Jose",       "CA",   "68000",
  "San Diego",      "CA",   "66000",
  "Sacramento",     "CA",   "64000",
  "Los Angeles",    "CA",   "44000",
  "San Francisco",  "CA",   "67000",
  "Shafter",        "CA",   "71106",
  "Arvin",          "CA",   "02924"
)

mhi_comparison <- get_acs(
  geography = "place",
  variables = "B19013_001",
  state     = "CA",
  year      = 2023,
  survey    = "acs5"
) %>%
  filter(GEOID %in% paste0("06", city_fips$place_fips))

print(mhi_comparison %>% select(NAME, estimate, moe))
