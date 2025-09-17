library(tigris)
library(tidyverse)
library(glue)
library(sf)
library(here)

options(tigris_use_cache = TRUE)


## ---------------------------------------------------------------------------=
# Get hot-dry mixed dry climate region ----
## ---------------------------------------------------------------------------=

clim_path <-
  "C:/Users/js5466/OneDrive - Drexel University/r_master/new_projects/climate_regions/raw_data"

clim_reg <-
  read_csv(
    glue(
      '{clim_path}/climate_zones.csv'
    )
  ) %>%
  janitor::clean_names()

## Narrow to hot humid mixed humid climate region
hd_md_reg <-
  clim_reg %>%
  mutate(
    climate_region_filt = if_else(
      ba_climate_zone %in% c('Hot-Dry', 'Mixed-Dry"'),
      'IN',
      'OUT'
    )
  ) %>%
  filter(climate_region_filt == 'IN')


## ---------------------------------------------------------------------------=
# Get counties for hot humid states ----
## ---------------------------------------------------------------------------=
hd_md_st <-
  hd_md_reg %>%
  distinct(state_fips)

## Didn't need to do this, just use a vector dummy
hd_sf <-
  map(hd_md_st, ~ counties(state = .x, year = "2010"))

hd_md_states <- hd_sf$state_fips

hd_md_county <-
  hd_md_reg %>%
  distinct(state_fips, county_fips) %>%
  mutate(
    GEOID10 = glue('{state_fips}{county_fips}'),
    GEOID10 = as.character(GEOID10)
  ) %>%
  select(GEOID10) %>%
  pull()

hd_md_sf_fin <-
  hd_md_sf %>%
  filter(GEOID10 %in% hd_md_county) %>%
  st_as_sf()


st_write(
  hd_md_sf_fin,
  here("data", "shapefiles", "hot_dry_mixed_dry_2010_counties.shp")
)
