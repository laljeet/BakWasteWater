## =============================================================================
## 01_data_acquisition.R
## Pull ACS tract-level income bracket data and city boundary geometry
## for the City of Bakersfield sewer service area.
## =============================================================================

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

## --- Census API key -----------------------------------------------------------
## Uncomment and set your key on first use:
census_api_key("ADD Your Key", overwrite = TRUE)

## --- Parameters ---------------------------------------------------------------

state_fips  <- "CA"
county_fips <- "029"        # Kern County
acs_year    <- 2023         # Most recent 5-year ACS (2019-2023)
bkfd_place  <- "03526"     # Bakersfield city place FIPS (within CA = 06)

## --- 1. Pull tract-level income bracket data (Table B19001) ------------------
## B19001 provides household counts in 16 income brackets plus total.
## This is the core table used by Maxcy-Brown et al. (2024).

cat("Pulling ACS B19001 (income brackets) for Kern County tracts...\n")

income_brackets_raw <- get_acs(
  geography  = "tract",
  table      = "B19001",
  state      = state_fips,
  county     = county_fips,
  year       = acs_year,
  survey     = "acs5",
  geometry   = TRUE,
  output     = "wide",
  cache_table = TRUE
)

## --- 2. Pull tract-level median household income (B19013) --------------------

cat("Pulling ACS B19013 (median household income) for Kern County tracts...\n")

mhi_raw <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state     = state_fips,
  county    = county_fips,
  year      = acs_year,
  survey    = "acs5",
  output    = "wide"
)

## --- 3. Load sewer service area boundary -------------------------------------
cat("Loading sewer service area boundary...\n")

sewer_boundary <- st_read("./Sewer_Service_Area/Sewer_Service_Area.shp") %>%
  filter(grepl("SEWER SERVICE AREA|SHARED PLANT BORDER", SSA_NAME))  # point to your .shp
sewer_boundary_by_plant <- sewer_boundary  # keep original
sewer_boundary <- sewer_boundary %>% 
  st_union() %>% 
  st_sf()
## --- 4. Identify tracts within sewer service area ----------------------------
cat("Identifying tracts within sewer service area...\n")

income_brackets_raw <- st_transform(income_brackets_raw, st_crs(sewer_boundary))

income_brackets_raw$tract_area <- st_area(income_brackets_raw)

tracts_in_city <- st_intersection(income_brackets_raw, sewer_boundary)
tracts_in_city$overlap_area <- st_area(tracts_in_city)

tracts_in_city$overlap_frac <- as.numeric(
  tracts_in_city$overlap_area / tracts_in_city$tract_area
)

tracts_in_city <- tracts_in_city %>%
  filter(overlap_frac >= 0.10)

## --- 4. Identify tracts within Bakersfield service area ----------------------
## Use spatial intersection: tracts that overlap the city boundary.
## Weight by area overlap fraction to handle split tracts.

cat("Identifying tracts within Bakersfield...\n")

# Ensure consistent CRS
income_brackets_raw <- st_transform(income_brackets_raw, st_crs(sewer_boundary))

# Calculate area of each full tract
income_brackets_raw$tract_area <- st_area(income_brackets_raw)

# Intersect tracts with city boundary
tracts_in_city <- st_intersection(income_brackets_raw, sewer_boundary)
tracts_in_city$overlap_area <- st_area(tracts_in_city)

# Calculate overlap fraction (for weighting households in split tracts)
tracts_in_city$overlap_frac <- as.numeric(
  tracts_in_city$overlap_area / tracts_in_city$tract_area
)

# Keep tracts with at least 10% overlap
tracts_in_city <- tracts_in_city %>%
  filter(overlap_frac >= 0.10)

cat(sprintf("  Found %d tracts overlapping Bakersfield city boundary\n",
            nrow(tracts_in_city)))

## --- 5. Clean and structure income bracket data ------------------------------

## B19001 variable mapping (ACS 2023, inflation-adjusted dollars)
## _001 = Total
## _002 = Less than $10,000
## _003 = $10,000 to $14,999
## _004 = $15,000 to $19,999
## _005 = $20,000 to $24,999
## _006 = $25,000 to $29,999
## _007 = $30,000 to $34,999
## _008 = $35,000 to $39,999
## _009 = $40,000 to $44,999
## _010 = $45,000 to $49,999
## _011 = $50,000 to $59,999
## _012 = $60,000 to $74,999
## _013 = $75,000 to $99,999
## _014 = $100,000 to $124,999
## _015 = $125,000 to $149,999
## _016 = $150,000 to $199,999
## _017 = $200,000 or more

bracket_info <- tibble(
  variable = paste0("B19001_", sprintf("%03d", 2:17)),
  bracket_order = 1:16,
  lower_bound = c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000,
                   45000, 50000, 60000, 75000, 100000, 125000, 150000, 200000),
  upper_bound = c(9999, 14999, 19999, 24999, 29999, 34999, 39999, 44999,
                   49999, 59999, 74999, 99999, 124999, 149999, 199999, NA)
)

## Reshape income data to long format for analysis
## Build the column names for the 16 income brackets (002 through 017)
bracket_cols <- paste0("B19001_", sprintf("%03d", 2:17), "E")

income_long <- tracts_in_city %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, overlap_frac, B19001_001E, all_of(bracket_cols)) %>%
  rename(total_hh = B19001_001E) %>%
  pivot_longer(
    cols = all_of(bracket_cols),
    names_to = "variable",
    values_to = "hh_count"
  ) %>%
  mutate(
    variable = str_remove(variable, "E$"),
    # Weight household counts by overlap fraction
    hh_count_weighted = hh_count * overlap_frac,
    total_hh_weighted = total_hh * overlap_frac
  ) %>%
  left_join(bracket_info, by = "variable")

## --- 6. Join MHI data --------------------------------------------------------

mhi_clean <- mhi_raw %>%
  select(GEOID, mhi = B19013_001E, mhi_moe = B19013_001M)

income_long <- income_long %>%
  left_join(mhi_clean, by = "GEOID")

## --- 7. Save processed data --------------------------------------------------

cat("Saving processed data...\n")

# Save the spatial data (tracts with geometry for mapping)
tracts_spatial <- tracts_in_city %>%
  select(GEOID, NAME, overlap_frac, B19001_001E) %>%
  rename(total_hh = B19001_001E) %>%
  mutate(total_hh_weighted = total_hh * overlap_frac) %>%
  left_join(mhi_clean, by = "GEOID")

saveRDS(income_long,    "data/income_brackets_long.rds")
saveRDS(tracts_spatial, "data/tracts_spatial.rds")
saveRDS(sewer_boundary,  "data/bakersfield_boundary.rds")

# Also save as CSV for portability
income_long %>%
  select(-variable) %>%
  write_csv("data/income_brackets_long.csv")

cat("Done. Data saved to data/ directory.\n")
cat(sprintf("  Tracts: %d\n", n_distinct(income_long$GEOID)))
cat(sprintf("  Estimated total households in service area: %.0f\n",
            sum(unique(income_long[, c("GEOID", "total_hh_weighted")])$total_hh_weighted)))

