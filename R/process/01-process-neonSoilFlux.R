# ### Purpose: script file to acquire NEON data and compute soil fluxes using neonSoilFlux.
# ### NOTE: this code is commented out because it may take a while to run.  All the pre-downloaded data files for review are located in data/raw/flux-data
#
#
# # Load libraries
# library(NEONSoils)
# library(tidyverse)
# library(neonUtilities)
#
#
#
# # We collected data in 2022 and 2024, so we will structure a long vector to loop through and computer
# # --> Define the name of the site:
# site_name_2022 <- c("SRER","SJER","WREF")
#
# # --> Create the dates vector
# dates_2022 <- c("2021-09","2021-10","2021-11",
#                 "2021-12","2022-01","2022-02",
#                 "2022-03","2022-04","2022-05",
#                 "2022-06","2022-07","2022-08")
#
# places_2022 <- expand_grid(site_name_2022,dates_2022) |>
#   rename(site_name = site_name_2022,
#          dates = dates_2022)
#
#
# # Now do 2024 sampling
# site_name_2024 <- c("UNDE","WOOD","KONZ")
#
#
# # --> Create the dates vector
# dates_2024 <- c("2023-09","2023-10","2023-11",
#                 "2023-12","2024-01","2024-02",
#                 "2024-03","2024-04","2024-05",
#                 "2024-06","2024-07","2024-08")
#
# # Expand the dates vector
# places_2024 <- expand_grid(site_name_2024,dates_2024)  |>
#   rename(site_name = site_name_2024,
#          dates = dates_2024)
#
# # Bind everything together - now ready to compute!
# tot_places <- rbind(places_2022,places_2024)
#
#
# # Now we go through and do the dirty work of saving and computing fluxes. Yay .... :)
# for(i in 1:nrow(tot_places)) {
#
#
#   # Name current month (you will need to adjust this on your computer)
#   curr_month <- tot_places$dates[[i]]
#   curr_site_name <- tot_places$site_name[[i]]
#   env_name <- paste0("data/raw/flux-data/env-meas-",curr_site_name,"-",curr_month,".Rda")
#   flux_name <- paste0("data/raw/flux-data/out-flux-",curr_site_name,"-",curr_month,'.Rda')
#
#   # Record where the index is so we don't need to guess
#   print(paste("Current vector index:",i))
#   print(env_name)
#   print(flux_name)
#
#
#
#
#   # Process
#   try(  # Put this as a try loop to do error handling
#     # NOTE: you will need to say y/n at several points here
#     {
#
#       out_env_data <- acquire_neon_data(
#         site_name = curr_site_name,
#         download_date = curr_download_date,
#         provisional = TRUE
#       )
#
#       site_data <- out_env_data$site_data
#       site_megapit <- out_env_data$site_megapit
#
#       save(site_data,site_megapit,file = env_name)
#
#       out_fluxes <- compute_neon_flux(input_site_env = out_env_data$site_data,
#                                       input_site_megapit = out_env_data$site_megapit)
#
#       save(out_fluxes,file = flux_name)
#
#     }
#
#   )
# }
#
#
