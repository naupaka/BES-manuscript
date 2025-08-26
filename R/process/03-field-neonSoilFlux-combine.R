### Purpose: Compare LICOR fluxes to model fluxes and synthesize into a single data table

library(tidyverse)
library(lubridate)


# Load up the flux results
load('data/derived/licor-all-data.Rda')

# Load up information about the sites
load('data/derived/field-data-info.Rda')

# Load up the combined outputs from `neonSoilFlux`:
load('data/derived/all-year-flux-results.Rda')



### Verify the times are correct
licor_rev_all <- licor_all_data |>
  rename(flux = fluxes,
         startDateTime = date) |>
  mutate(fluxExpUncert = NA,
         VSWCExpUncert = NA,
         soilTempExpUncert = NA,
         horizontalPosition = "LICOR")


licor_flux <- licor_rev_all |>
  select(-matches("VSWC|soilTemp|staPres")) |>
  group_by(site) |>
  nest() |>
  rename(field_flux = data)

licor_env <- licor_rev_all |>
  select(-matches("flux")) |>
  group_by(site) |>
  nest() |>
  rename(field_env = data)



# Combine model and revised data, only using the times that are here
model_sites_mq <- model_fluxes_mq |>
  group_by(site) |>
  nest() |>
  inner_join(measurement_times,by="site") |>
  mutate(model_data_mq = pmap(.l=list(data,start_time,end_time,sampling_location),
                           .f=~filter(..1,between(startDateTime,..2,..3),
                                      horizontalPosition == ..4))) |>
  select(site,model_data_mq,sampling_location)

model_sites_marshall <- model_fluxes_marshall |>
  group_by(site) |>
  nest() |>
  inner_join(measurement_times,by="site") |>
  mutate(model_data_marshall = pmap(.l=list(data,start_time,end_time,sampling_location),
                           .f=~filter(..1,between(startDateTime,..2,..3),
                                      horizontalPosition == ..4))) |>
  select(site,model_data_marshall,sampling_location)


env_sites <- env_values |>
  select(-monthly_mean) |>
  filter(measurement %in% c("VSWC","soilTemp")) |>
  rename(env_data_in = data) |>
  group_by(site,measurement) |>
  nest() |>
  mutate(data = map(.x = data,.f=~(.x |> unnest(cols=c(env_data_in)) |> ungroup()))) |> # Combine consecutive months together
  inner_join(measurement_times,by="site") |>  # Filter only during times we are at field
  mutate(env_data = pmap(.l=list(data,start_time,end_time,sampling_location),
                           .f=~(..1 |>
                                  ungroup() |>
                                  filter(horizontalPosition == ..4,
                                         str_detect(verticalPosition,pattern="[:digit:]{2}1"),
                                         between(startDateTime,..2,..3),
                                      )))) |>
  mutate(n_tot = map_int(env_data,nrow)) |>
  filter(n_tot>0) |>
  select(site,sampling_location,measurement,env_data) |>
  pivot_wider(names_from = "measurement",values_from = "env_data",names_prefix = "NEON_")






# Now join the licor data to the flux data.
field_data_joined <- model_sites_mq |>
  inner_join(model_sites_marshall,by=c("site","sampling_location")) |>
  inner_join(licor_flux,by=c("site")) |>
  inner_join(env_sites,by=c("site","sampling_location")) |>
  inner_join(licor_env,by="site") |>
  relocate(site,sampling_location) |>
  ungroup()


# Save the combined dataset
save(field_data_joined,file = 'data/derived/combined-field-data.Rda')

