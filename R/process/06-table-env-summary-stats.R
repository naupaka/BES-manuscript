library(tidyverse)
# Get some basic stats of moisture and temperature data from when we were there - used in Table 1 (site description)

# (1) Load up flux data
load('data/derived/combined-field-data.Rda')

summary_env_data <- field_data_joined |>
  select(site,field_env) |>
  unnest(cols=c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = mean(VSWC),
    temp_data = mean(soilTemp),
    .groups="drop"
  )
