### Author: JMZ
### Last Modified: 2024-10-15 by NBZ
### Purpose: Read in LI-COR JSON files and compute the fluxes.
### Converts the local time to UTC to avoid any time mismatches.

# After downloading a data file onto a computer, values for
# variables To, Po, Wo, V, and A can be conveniently obtained
# using SoilFluxPro Software. These variables are labeled as IV
# Tcham, IV Pressure, IV H2O, Vtotal, and Area. Tcham, IV Pressure, and IV H2O are in the group of Measured Variables.
# Vtotal and Area can also be exported into a text file for the
# flux calculation.

library(tidyverse)
library(jsonlite)

# Include all the script files we are sourcing
files_sources = list.files('R/functions',
                           full.names = TRUE)

sapply(files_sources,
       source)

### Link up timezone info

file_names <- list.files(path = 'data/raw',
                         full.names = TRUE,
                         recursive = TRUE,
                         pattern = '.json')

sites <- str_extract(
  file_names,
  pattern = '(?<=Summer2024/)[:alpha:]{4}|(?<=_NEON_)[:alpha:]{4}') |>
  unique()

# Link up Ameriflux and sites
site_ids <- read_tsv('data/raw/AmeriFlux-site-search-results-20240422535.tsv') |>
  select(`Site ID`, Name, `Latitude (degrees)`, `Longitude (degrees)`) |>
  rename(Ameriflux = `Site ID`,
         latitude = `Latitude (degrees)`,
         longitude = `Longitude (degrees)`) |>
  mutate(NEON = str_extract(Name, pattern = '(?<=\\()[:upper:]{4}'),
         tz = lutz::tz_lookup_coords(lat = latitude,
                                     lon = longitude,
                                     method = "accurate")) |>
  select(NEON, tz) |>
  filter(NEON %in% sites)

### Check to see if results changes if SRER is set to SJER timezone
### 5/29/24: correspondence with Naupaka via text suggested he may have not
### switched the timezone.

# Load up sampling sites
sampling_locations <- read_csv('data/raw/field_sampling_sites.csv')

site_locations <- str_extract(
  file_names,
  pattern = '(?<=Summer2024/)[:alpha:]{4}|(?<=_NEON_)[:alpha:]{4}')

# Map results to data frame
licor_flux_data <- map2_df(.x = file_names,
                           .y = site_locations,
                           .f = ~extract_licor_flux(.x, .y, site_ids)) |>
  mutate(instrument = "6800") |>
  filter(RepNum == 1) |>
  select(-RepNum)

### Read in the automated chamber measurements

automated_licor <- list.files(path = 'data/raw/Summer2024',
                              full.names = TRUE,
                              recursive = TRUE,
                              pattern = '.csv')

automated_licor_site <- str_extract(
  automated_licor,
  pattern = '(?<=Summer2024/)[:alpha:]{4}|(?<=_NEON_)[:alpha:]{4}')

# Map results to data frame
automated_flux_data <- map2_df(.x = automated_licor,
                               .y = automated_licor_site,
                               .f = ~extract_licor_automated_flux(.x,
                                                                  .y,
                                                                  site_ids)) |>
  mutate(instrument = "8250")

### Combine these two together, forcing things to be in UTC time

licor_all_data <- licor_flux_data |>
  rename(fluxes = flux_lin) |>
  select(-flux_exp) |>
  rbind(automated_flux_data) |>
  group_by(site) |>
  nest() |>
  inner_join(site_ids,
             by = c("site" = "NEON")) |>
  select(site, data) |>
  unnest(cols = c(data)) |>
  ungroup()

save(licor_all_data,
     file = 'data/derived/licor-all-data.Rda')

### figure out when we visited each site with the LICOR
### with the appropriate time zone - yay!

measurement_times <- licor_all_data |>
  group_by(site) |>
  summarize(start_time = floor_date(min(date), unit = "day"),
            end_time = ceiling_date(max(date), unit = "day")) |>
  inner_join(site_ids,
             by = c("site" = "NEON")) |>
  rename(curr_tz = tz) |>
  inner_join(sampling_locations,
             by = c("site" = "NEON")) |>
  rename(sampling_location = location)

save(measurement_times,
     file = 'data/derived/field-data-info.Rda')

