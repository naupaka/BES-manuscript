### Purpose: script file to acquire NEON data and compute soil fluxes using neonSoilFlux.
### NOTE: this code may take a while to run.

# Load libraries
library(neonSoilFlux)
library(tidyverse)
library(neonUtilities)
library(doParallel)
library(foreach)

# setup parallel
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# We collected data in 2022 and 2024, so we will structure a long vector to loop through and compute
# --> Define the name of the site:
site_name_2022 <- c("SRER", "SJER", "WREF")

# --> Create the dates vector
dates_2022 <- c(
  "2021-09", "2021-10", "2021-11",
  "2021-12", "2022-01", "2022-02",
  "2022-03", "2022-04", "2022-05",
  "2022-06", "2022-07", "2022-08")

places_2022 <- expand_grid(site_name_2022, dates_2022) |>
  rename(
    site_name = site_name_2022,
    dates = dates_2022)

# Now do 2024 sampling
site_name_2024 <- c("UNDE", "WOOD", "KONZ")

# --> Create the dates vector
dates_2024 <- c(
  "2023-09", "2023-10", "2023-11",
  "2023-12", "2024-01", "2024-02",
  "2024-03", "2024-04", "2024-05",
  "2024-06", "2024-07", "2024-08")

# Expand the dates vector
places_2024 <- expand_grid(site_name_2024, dates_2024) |>
  rename(
    site_name = site_name_2024,
    dates = dates_2024)

# Bind everything together - now ready to compute!
tot_places <- rbind(places_2022, places_2024)

# Now we go through and do the dirty work of saving and computing fluxes. Yay .... :)
# # Loop along here using a parallel foreach approach
# note that lots of troubleshooting output is not displayed to user when
# running in parallel mode. Can switch %dopar% to %do% to run in serial
# which will provide more output for troubleshooting
cat("Acquiring NEON data and computing fluxes. This may take a while...\n")
cat("Downloading and calculating fluxes for", nrow(tot_places), "site-months\n")
cat("You can monitor progress by looking at the files in data/raw/flux-data/\n")
results <- foreach(i = 1:nrow(tot_places),
                   .packages = c("tidyverse", "neonSoilFlux")) %dopar% {
                     curr_month <- tot_places$dates[[i]]
                     curr_site_name <- tot_places$site_name[[i]]
                     env_name <- paste0(
                       "data/raw/flux-data/env-meas-",
                       curr_site_name, "-", curr_month, ".Rda"
                     )
                     flux_name <- paste0(
                       "data/raw/flux-data/out-flux-",
                       curr_site_name, "-", curr_month, ".Rda"
                     )

                     # Record where the index is so we don't need to guess
                     cat("Current vector index:", i, "of", nrow(tot_places), "\n")
                     cat("Looking for existing files. Otherwise, will download and calculate.\n")

                     if (all(file.exists(c(env_name, flux_name)))) {
                       return(paste0("Found ", env_name, "\n",
                                    "Found ", flux_name, "\n",
                                    "NEON data and calculated flux files exist: already done!\n"))
                     }

                     # Process
                     # Put this as a try loop to do error handling
                     # NOTE: you may need to say y/n at several points here

                     if (!file.exists(env_name)) {
                       cat("Attempting to acquire NEON data for",
                           curr_site_name, curr_month, "\n")
                       tryCatch(
                         {
                           out_env_data <- acquire_neon_data(
                             site_name = curr_site_name,
                             download_date = curr_month,
                             provisional = TRUE
                           )

                           site_data <- out_env_data$site_data
                           site_megapit <- out_env_data$site_megapit

                           cat("Saving", env_name, "\n")
                           save(site_data, site_megapit, file = env_name)
                         },
                         error = function(e) {
                           message("Error downloading", env_name, ":", e$message)
                         }
                       )
                     } else {
                       cat("Found", env_name, "\n")
                       load(env_name)
                     }

                     cat("Calculating fluxes for", curr_site_name, curr_month, "\n")
                     out_fluxes <- compute_neon_flux(
                       input_site_env = site_data,
                       input_site_megapit = site_megapit)

                     cat("Saving", flux_name, "\n")
                     save(out_fluxes, file = flux_name)
                   }

stopCluster(cl)

for (msg in results) {
  if (is.character(msg)) {
    cat(msg)
  }
}

env_names <- paste0(
  "data/raw/flux-data/env-meas-",
  tot_places$site_name, "-", tot_places$dates, ".Rda")

flux_names <- paste0(
  "data/raw/flux-data/out-flux-",
  tot_places$site_name, "-", tot_places$dates, ".Rda")

# Stop if we weren't able to download all the required data files from NEON
# and/or process them to estimate fluxes
stopifnot(all(file.exists(c(env_names, flux_names))))

# Combine all the downloaded files into a single one:
# Load up the flux and env files
flux_files <- list.files(
  path = "data/raw/flux-data",
  pattern = "out-flux-", full.names = TRUE)

env_files <- list.files(
  path = "data/raw/flux-data",
  pattern = "env-meas-", full.names = TRUE)

model_fluxes_mq <- vector(mode = "list", length = length(flux_files))
model_fluxes_marshall <- vector(mode = "list", length = length(flux_files))
env_values <- vector(mode = "list", length = length(env_files))

for (i in seq_along(flux_files)) {
  load(flux_files[[i]])
  site_name <- str_extract(flux_files[[i]],
                           pattern = "(?<=out-flux-)[:alpha:]{4}")

  # We need to do some extraction / data wrangling to get the MQ and
  # Marshall fluxes as separate lists.  This is some crazy tidyverse wrangling.
  nested_flux_diffus <- out_fluxes |>
    mutate(joined_data = map2(.x = flux_compute,
                              .y = surface_diffusivity,
                              .f = ~ left_join(.x,
                                               .y,
                                               by = "diffus_method"))) |>
    select(-surface_diffusivity, -flux_compute) |>
    unnest(cols = c(joined_data)) |>
    group_by(diffus_method) |>
    nest() |>
    mutate(data = map(data, .f = ~ (.x |>
                                      select(-gradient, -gradient_err) |>
                                      group_by(startDateTime,
                                               horizontalPosition,
                                               soilCO2concentrationMeanQF,
                                               VSWCMeanQF, soilTempMeanQF,
                                               staPresMeanQF,
                                               diffusivity,
                                               diffusExpUncert,
                                               zOffset) |>
                                      nest() |>
                                      rename(flux_compute = data) |>
                                      ungroup() |>
                                      group_by(startDateTime,
                                               horizontalPosition,
                                               flux_compute,
                                               soilCO2concentrationMeanQF,
                                               VSWCMeanQF,
                                               soilTempMeanQF,
                                               staPresMeanQF) |>
                                      nest() |>
                                      rename(diffusivity = data) |>
                                      ungroup() |>
                                      relocate(startDateTime,
                                               horizontalPosition,
                                               flux_compute, diffusivity,
                                               soilCO2concentrationMeanQF,
                                               VSWCMeanQF,
                                               soilTempMeanQF,
                                               staPresMeanQF)
    )))

  # Assign each of the fluxes to a different data frame
  model_fluxes_mq[[i]] <- nested_flux_diffus |>
    filter(diffus_method == "Millington-Quirk") |>
    pull(data) |>
    pluck(1) |> # return df not list of length 1
    mutate(site = site_name)

  model_fluxes_marshall[[i]] <- nested_flux_diffus |>
    filter(diffus_method == "Marshall") |>
    pull(data) |>
    pluck(1) |> # return df not list of length 1
    mutate(site = site_name)
}

### Do the same for the env values
### More graveyard for env data
for (i in seq_along(env_values)) {
  load(env_files[[i]])
  site_name <- str_extract(env_files[[i]],
                           pattern = "(?<=env-meas-)[:alpha:]{4}")

  env_values[[i]] <- site_data |> mutate(site = site_name)
}

# Bind everything together
model_fluxes_mq <- bind_rows(model_fluxes_mq)
model_fluxes_marshall <- bind_rows(model_fluxes_marshall)
env_values <- bind_rows(env_values)

# For simplicity, we only want to take
save(model_fluxes_mq, model_fluxes_marshall, env_values,
     file = "data/derived/all-year-flux-results.Rda"
)
