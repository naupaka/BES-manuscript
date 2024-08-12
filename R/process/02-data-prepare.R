### Compare LICOR fluxes to model fluxes
library(tidyverse)
library(lubridate)
library(NEONSoils)


# Load up the flux results
load('data/derived/licor-all-data.Rda')

# Load up information about the sites
load('data/derived/field-data-info.Rda')

# Load up the flux and env files
flux_files <- list.files(path='data/raw/flux-data',pattern='out-flux-',full.names=TRUE)
env_files <- list.files(path='data/raw/flux-data',pattern='env-meas-',full.names=TRUE)

model_fluxes <- vector(mode = "list", length = length(flux_files))
env_values <- vector(mode = "list", length = length(env_files))
soil_values <- vector(mode = "list", length = length(env_files))


for(i in seq_along(model_fluxes)) {

  load(flux_files[[i]])
  site_name <- str_extract(flux_files[[i]],
                           pattern="(?<=out-flux-)[:alpha:]{4}")
  model_fluxes[[i]] <- out_fluxes |> mutate(site=site_name)

}

### Do the same for the env values
### More graveyard for env data
for(i in seq_along(env_values)) {

  load(env_files[[i]])
  site_name <- str_extract(env_files[[i]],
                           pattern="(?<=env-meas-)[:alpha:]{4}")

  env_values[[i]] <- site_data |> mutate(site=site_name)


  # Ingest the megapit soil physical properties pit, horizon, and biogeo data
  mgp.pit <- site_megapit$mgp_permegapit
  mgp.hzon <- site_megapit$mgp_perhorizon
  mgp.bgeo <- site_megapit$mgp_perbiogeosample
  mgp.bden <- site_megapit$mgp_perbulksample


  # Merge the soil properties into a single data frame

  mgp.hzon.bgeo <- inner_join(mgp.hzon, mgp.bgeo, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName"))
  mgp.hzon.bgeo.bden <- inner_join(mgp.hzon.bgeo, mgp.bden, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName", "labProjID", "laboratoryName"))
  mgp <- inner_join(mgp.hzon.bgeo.bden, mgp.pit, by=c("pitID", "domainID", "siteID", "pitNamedLocation", "nrcsDescriptionID"))

  ###############################
  # Future development: Estimate particle density of <2 mm fraction based on Ruhlmann et al. 2006 Geoderma 130,
  # 272-283. Assumes C content of organic matter is 55%. Constants 1.127, 0.373, 2.684 come
  # from Ruhlman et al. 2006 (2.684 = particle density of the mineral fraction, "(1.127 +
  # 0.373*(dfBGChem$Estimated.organic.C..../55))" = particle density of organic matter).
  ###############################

  # Calculate 2-20 mm rock volume (cm3 cm-3). Assume 2.65 g cm-3 density.
  rockVol <- ((mgp$coarseFrag2To5 + mgp$coarseFrag5To20) / 1000) / 2.65

  # Calculate porosity of the <2 mm fraction (cm3 cm-3). Assume soil particle density of 2.65 g cm-3.
  porosSub2mm <- 1 - mgp$bulkDensExclCoarseFrag/2.65

  # Calculate porosity of the 0-20 mm fraction (cm3 cm-3). Assume no pores within rocks.
  mgp$porVol2To20 <- porosSub2mm * (1 - rockVol)


  soil_values[[i]] <- mgp |>
    filter(biogeoTopDepth == min(biogeoTopDepth,na.rm=TRUE)) |>
    select(porVol2To20) |>
    mutate(site = site_name)

}

# Bind everything together
model_fluxes <- bind_rows(model_fluxes)

env_values <- bind_rows(env_values)

soil_values <- bind_rows(soil_values) |>
  distinct() # Ok to do this since this is a common measurement



### FORCE THE TIME ZONE WHEN PROCESSING THE FLUXES


### Verify the times are correct with Naupaka
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
model_sites <- model_fluxes |>
  group_by(site) |>
  nest() |>
  inner_join(measurement_times,by="site") |>
  mutate(model_data = pmap(.l=list(data,start_time,end_time,sampling_location),
                           .f=~filter(..1,between(startDateTime,..2,..3),
                                      horizontalPosition == ..4))) |>
  select(site,model_data,sampling_location)


env_sites <- env_values |>
  select(-monthly_mean) |>
  filter(measurement %in% c("VSWC","soilTemp")) |>
  #group_by(site,measurement) |>
  #nest() |>
  inner_join(measurement_times,by="site") |>
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
field_data_joined <- model_sites |>
  inner_join(licor_flux,by=c("site")) |>
  inner_join(env_sites,by=c("site","sampling_location")) |>
  inner_join(soil_values,by="site") |>
  inner_join(licor_env,by="site")


### Ready to plot fluxes!

save(field_data_joined,file = 'data/derived/combined-field-data.Rda')

