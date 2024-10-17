### Author: JMZ
### Purpose: Use the NEON Env data to compute the diffusivity, co2 gradient, bulk density, and rock volume from the soil megapit measurements, narrowing it down to when Naupaka made field measurements.

library(tidyverse)

# Helper Function that extracts out the diffusivity (closest to the surface) at a given NEON site, gradient, rock volume, and bulk density information.


extract_gradient_diffusivity<- function(env_file_name) {

  load(env_file_name)

  ### Largely extracted from neonSoilFlux::compute_neon_flux
  input_site_env <- site_data
  input_site_megapit <- site_megapit



  # Ingest the megapit soil physical properties pit, horizon, and biogeo data
  mgp.pit <-  input_site_megapit$mgp_permegapit

  ### Information about the horizons
  mgp.hzon <- input_site_megapit$mgp_perhorizon |>
    select(horizonID,horizonTopDepth,horizonBottomDepth) |>
    arrange(horizonTopDepth)

  ###
  mgp.bgeo <- input_site_megapit$mgp_perbiogeosample
  mgp.bden <- input_site_megapit$mgp_perbulksample


  # Merge the soil properties into a single data frame.  We average by horizon

  ###############################
  # Future development: Estimate particle density of <2 mm fraction based on Ruhlmann et al. 2006 Geoderma 130,
  # 272-283. Assumes C content of organic matter is 55%. Constants 1.127, 0.373, 2.684 come
  # from Ruhlman et al. 2006 (2.684 = particle density of the mineral fraction, "(1.127 +
  # 0.373*(dfBGChem$Estimated.organic.C..../55))" = particle density of organic matter).
  ###############################

  # Calculate 2-20 mm rock volume (cm3 cm-3). Assume 2.65 g cm-3 density for each horizon.
  #rockVol <- ((mgp$coarseFrag2To5 + mgp$coarseFrag5To20) / 1000) / 2.65
  rockVol <- mgp.bgeo |>
    dplyr::mutate(rockVol = (coarseFrag2To5 + coarseFrag5To20) / 1000 / 2.65) |>
    dplyr::group_by(horizonID) |>
    dplyr::summarize(rockVol = mean(rockVol,na.rm=TRUE)) |>
    dplyr::ungroup()

  # Calculate porosity of the <2 mm fraction (cm3 cm-3). Assume soil particle density of 2.65 g cm-3. (done across each horizon)
  porosSub2mm <- mgp.bden |>
    dplyr::mutate(porosSub2mm = 1 - bulkDensExclCoarseFrag / 2.65) |>
    dplyr::group_by(horizonID) |>
    dplyr::summarize(porosSub2mm = mean(porosSub2mm,na.rm=TRUE),
                     bulkDens = mean(bulkDensExclCoarseFrag,na.rm=TRUE)) |>
    dplyr::ungroup()




  # Join these all up together in a megapit data frame, convert depths to m

  mgp <- mgp.hzon |>
    dplyr::inner_join(rockVol,by="horizonID") |>
    dplyr::inner_join(porosSub2mm,by="horizonID") |>
    dplyr::mutate(porVol2To20 = porosSub2mm * (1 - rockVol),  # Define the porosity
                  horizonTopDepth = horizonTopDepth/100,
                  horizonBottomDepth = horizonBottomDepth/100)  # convert to m


  # Tells us the depths at each site, adds in the porosity
  site_depths <- input_site_env[input_site_env$measurement == "soilCO2concentration",]$data[[1]] |>
    dplyr::group_by(horizontalPosition,verticalPosition) |>
    dplyr::distinct(zOffset) |>
    dplyr::mutate(porVol2To20 = purrr::map_dbl(.x=.data[["zOffset"]],
                                               .f=~mgp[abs(.x) > mgp$horizonTopDepth & abs(.x) <= mgp$horizonBottomDepth,"porVol2To20"])
    ) |>
    dplyr::select(horizontalPosition,verticalPosition,porVol2To20)

  ### Now we can join things together
  input_site_env[input_site_env$measurement == "soilCO2concentration",]$data[[1]]  <- input_site_env[input_site_env$measurement == "soilCO2concentration",]$data[[1]] |>
    dplyr::inner_join(site_depths,by=c("horizontalPosition","verticalPosition"))


  # Now correct
  corrected_data <- neonSoilFlux::correct_env_data(input_site_env)

  qf_flags <- corrected_data$all_flags
  all_measures <- corrected_data$site_filtered

  # Determine if any flags for VSWC, soilTemp, staPres where used
  diffus_qf <- qf_flags |>
    select(-soilCO2concentrationMeanQF) |>
    mutate(diffusMeanQF = if_any(ends_with("MeanQF"), ~ .x != 0),
           diffusMeanQF = as.numeric(diffusMeanQF)) |>
    select(startDateTime,horizontalPosition,diffusMeanQF)

  flux_out <- all_measures |> # first filter out any bad measurements
    dplyr::mutate(flux_intro = purrr::map2(.x = .data[["env_data"]], .y = .data[["press_data"]], .f = function(.x, .y) {
      c <- neonSoilFlux::co2_to_umol(
        .x$soilTempMean,
        .y$staPresMean,
        .x$soilCO2concentrationMean,
        .x$soilTempExpUncert,
        .y$staPresExpUncert,
        .x$soilCO2concentrationExpUncert,
        .x$zOffset
      )



      d <- neonSoilFlux::diffusivity(
        temperature = .x$soilTempMean,
        soil_water = .x$VSWCMean,
        pressure = .y$staPresMean,
        temperature_err = .x$soilTempExpUncert,
        soil_water_err = .x$VSWCExpUncert,
        pressure_err = .y$staPresExpUncert,
        zOffset = .x$zOffset,
        porVol2To20 = .x$porVol2To20
      )


      new_data <- dplyr::inner_join(c, d, by = "zOffset")


      return(new_data)
    }))

  # Now compute the gradient and the uncertainty at the levels closest to the surface
  # x2 - x1, so pd_x1 = -1, pd_x2 = 1, and the uncertainty
  flux_out2 <- flux_out |>
    mutate(co2gradientMeanQF = map_int(.x=env_data,.f= function(x) {
      test_values <- x |> slice_max(zOffset,n=2) |>
        select(contains("MeanQF")) |> pull()

      # Now see if it is flagged
      flagged <- 1 %in% test_values

      out_val <- if_else(flagged,1,0)
      return(out_val)
    } )
    ) |>
    inner_join(diffus_qf,by=c("horizontalPosition","startDateTime")) |>

    mutate(co2gradient = map(.x=flux_intro, .f=~(.x  |>
                                                   slice_max(zOffset,n=2) |>
                                                   summarize(gradient = diff(co2_umol)/diff(zOffset),
                                                             gradientExpUncert = neonSoilFlux::quadrature_error(c(-1,1),co2ExpUncert) ) ) ),
           diffusivity = map(.x=flux_intro,.f=~slice_max(.x,zOffset) |> select(diffusivity,diffusExpUncert))) |>
    select(horizontalPosition,startDateTime,co2gradient,diffusivity,co2gradientMeanQF,diffusMeanQF)

  rock_volume <- mgp.hzon |>
    dplyr::inner_join(rockVol,by="horizonID") |>
    slice_min(horizonBottomDepth,n=1) |> pull(rockVol)

  # Take all of the values and the rock volumes across the horizon - it'll be more work, but also more honest in the calculations / comparisons.

  bulk_density <- mgp

  return(list(flux_gradient = flux_out2,
              rock_volume = rock_volume,
              bulk_density = bulk_density
              ) )

}


# Load up the sites - time to start looping
env_files <- list.files(path='data/raw/flux-data',pattern='env-meas-',full.names=TRUE)

# Where we store the env values, bulk density, and the rock volume values
env_values <- vector(mode = "list", length = length(env_files))

rock_volume_values <- vector(mode = "numeric", length = length(env_files))

bulk_density_values <- vector(mode = "list", length = length(env_files))

site_names <- map_chr(.x=env_files,.f=~str_extract(.x,
                         pattern="(?<=env-meas-)[:alpha:]{4}") )

# Loop along here
for(i in seq_along(env_values)) {
  print(i)

  out_values <- extract_gradient_diffusivity(env_files[[i]])

  env_values[[i]] <- out_values$flux_gradient
  rock_volume_values[[i]] <- out_values$rock_volume
  bulk_density_values[[i]] <- out_values$bulk_density

}

### Add this all up, but first we need to join the different months together to a site.

out_sites <- tibble(site = site_names,
                    env_data = env_values) |>
  group_by(site) |>
  nest() |>
  mutate(data = map(data,.f=~bind_rows(.x$env_data) |> ungroup()))

out_rock_vol <- tibble(site = site_names,
                       rock_volume = rock_volume_values,
                       bulk_density = bulk_density_values) |>
  group_by(site) |>
  summarize(rock_volume = mean(rock_volume),
            bulk_density = mean(bulk_density)) |>
  ungroup()

# Now join up

out_flux_gradient <- out_sites |> inner_join(out_rock_vol,by="site")


### Load up when we were actually there:
# Load up information about the sites
load('data/derived/field-data-info.Rda')

flux_gradient_diffusivity <- out_flux_gradient |>
inner_join(measurement_times,by="site") |>
  mutate(model_data = pmap(.l=list(data,start_time,end_time,sampling_location),
                           .f=~filter(..1,between(startDateTime,..2,..3),
                                      horizontalPosition == ..4))) |>
  select(site,model_data,rock_volume,bulk_density,curr_tz)

# Now save everythin
save(flux_gradient_diffusivity,file = 'data/derived/diffusivity-gradient.Rda')

