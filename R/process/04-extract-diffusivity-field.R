### Purpose: Use the NEON Env data to compute the diffusivity, co2 gradient, bulk density, and rock volume from the soil megapit measurements, narrowing it down to when field measurements were collected - this allows us to back-calculate the diffusivity from the field data.

# Load up the associated libraries
library(tidyverse)

# Load up information about the sites
load("data/derived/field-data-info.Rda")
load("data/derived/combined-field-data.Rda")

# Helper Function that extracts out the diffusivity (closest to the surface) at a given NEON site, gradient, rock volume, and bulk density information.
extract_gradient_diffusivity <- function(env_file_name) {
  load(env_file_name)

  ### Largely extracted from neonSoilFlux::compute_neon_flux
  input_site_env <- site_data

  # Now correct
  corrected_data <- neonSoilFlux::correct_env_data(input_site_env)

  qf_flags <- corrected_data$all_flags
  all_measures <- corrected_data$site_filtered

  # Determine if any flags for VSWC, soilTemp, staPres where used
  diffus_qf <- qf_flags |>
    select(-soilCO2concentrationMeanQF) |>
    mutate(
      diffusMeanQF = if_any(ends_with("MeanQF"), ~ .x != 0),
      diffusMeanQF = as.numeric(diffusMeanQF)
    ) |>
    select(startDateTime, horizontalPosition, diffusMeanQF)

  flux_out <- all_measures |> # first filter out any bad measurements
    dplyr::mutate(flux_intro = purrr::map2(.x = .data[["env_data"]],
                                           .y = .data[["press_data"]],
                                           .f = function(.x, .y) {
      c <- neonSoilFlux::co2_to_umol(
        .x$soilTempMean,
        .y$staPresMean,
        .x$soilCO2concentrationMean,
        .x$soilTempExpUncert,
        .y$staPresExpUncert,
        .x$soilCO2concentrationExpUncert,
        .x$zOffset
      )

      return(c)
    }))

  # Now compute the gradient and the uncertainty at the levels closest to the surface
  # x2 - x1, so pd_x1 = -1, pd_x2 = 1, and the uncertainty
  flux_gradient <- flux_out |>
    mutate(co2gradientMeanQF = map_int(.x = env_data, .f = function(x) {
      test_values <- x |>
        slice_max(zOffset, n = 2) |>
        select(contains("MeanQF")) |>
        pull()

      # Now see if it is flagged
      flagged <- 1 %in% test_values

      out_val <- if_else(flagged, 1, 0)
      return(out_val)
    })) |>
    inner_join(diffus_qf, by = c("horizontalPosition", "startDateTime")) |>
    mutate(co2gradient = map(.x = flux_intro, .f = ~ (.x |>
      slice_max(zOffset, n = 2) |>
      summarize(
        gradient = diff(co2_umol) / diff(zOffset),
        gradientExpUncert = neonSoilFlux::quadrature_error(c(-1, 1), co2ExpUncert)
      )) |>
      mutate(zOffset = max(.x$zOffset)) |>
      relocate(zOffset))) |>
    select(horizontalPosition, startDateTime, co2gradient, co2gradientMeanQF)

  ## Note: when gradient is positive, then the co2 at the deeper depth is smaller.
  return(flux_gradient)
}

# Only take out files that happen while we are there
env_files <- list.files(path = "data/raw/flux-data",
                        pattern = "env-meas-",
                        full.names = TRUE) |>
  str_subset(pattern = "2022-05|2022-06|2024-05|2024-06")

# Where we store the env values, bulk density, and the rock volume values
env_values <- vector(mode = "list", length = length(env_files))

site_names <- map_chr(.x = env_files, .f = ~ str_extract(.x,
  pattern = "(?<=env-meas-)[:alpha:]{4}"
))

# Loop along here
for (i in seq_along(env_values)) {
  print(i)

  env_values[[i]] <- extract_gradient_diffusivity(env_files[[i]])
}

### Add this all up, but first we need to join the different months together to a site.
out_sites <- tibble(
  site = site_names,
  env_data = env_values
) |>
  group_by(site) |>
  nest() |>
  mutate(data = map(data, .f = ~ bind_rows(.x$env_data) |> ungroup()))

# Now compute the diffusivity from the flux gradient
flux_gradient_diffusivity <- out_sites |>
  inner_join(measurement_times, by = "site") |>
  mutate(model_data = pmap(
    .l = list(data, start_time, end_time, sampling_location),
    .f = ~ filter(
      ..1, between(startDateTime, ..2, ..3),
      horizontalPosition == ..4
    )
  )) |>
  select(site, model_data, curr_tz)

# Now save everything
save(flux_gradient_diffusivity, file = "data/derived/diffusivity-gradient.Rda")
