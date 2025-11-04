#' @title Reads in a LICOR file with JSON data and computes the associated flux.
#'
#' @param file_name Name of the JSON file to be read in
#' @param tz_ids Data frame of timezones for the different sites
#'
#' @return a data frame of licor fluxes
#' @export
#'
extract_licor_flux <- function(file_name, site_name, tz_ids) {
  # https://www.licor.com/env/support/LI-6800/topics/symbols.html#meas --> info about data

  ####
  in_data <- jsonlite::fromJSON(file_name)

  time_vals <- in_data$obslist$data$TIME # the last entry in each list component is what we need

  curr_tz <- tz_ids |>
    dplyr::filter(NEON == site_name) |>
    dplyr::pull(tz)

  # Take the last value from the sample - that is what gets measured
  rep_times <- purrr::map_dbl(time_vals, last) |>
    as.POSIXct(origin = "1970-01-01", tz = curr_tz) |>
    lubridate::with_tz(tzone = "UTC")

  # compute the system dates for each sampling measurement
  test_times <- tibble::tibble(
    site = site_name,
    date = rep_times
  )

  # Wrangle the fluxes
  Vtotal <- in_data$obslist$const$TotalVolume
  Area <- in_data$obslist$const$SoilArea

  reps <- in_data$obslist$comp2$RepNum
  swc <- in_data$obslist$comp2$Ms_avg
  Tsoil <- in_data$obslist$comp2$Ts_avg
  P_o <- in_data$obslist$comp2$P_o

  R <- 8.314 # Ideal gas constant
  measured_values <- in_data$obslist$stats$IV |>
    tibble::as_tibble() |>
    dplyr::select(Pressure, H2O, Tchamber) |>
    dplyr::mutate(dplyr::across(.cols = tidyr::everything(), .fns = ~ as.double(.x))) |>
    cbind(Vtotal, Area) |>
    dplyr::mutate(flux_mult = 10 * Vtotal * Pressure * (1 - H2O / 1000) / (R * Area * (Tchamber + 273.15)))

  # derivative is (C0 - Cx)*exp(-a*(t-t0))*-a
  # at t = t0, then we have (C0-Cx)*-a

  ## For the linear fit, it is just a
  F_CO2_lin <- in_data$obslist$linfit$slope
  F_CO2_exp <- (in_data$obslist$expfit$fit_Co - in_data$obslist$expfit$fit_Cx) * -in_data$obslist$expfit$fit_a

  meas_flux <- measured_values |>
    dplyr::mutate(
      RepNum = reps,
      VSWC = swc,
      soilTemp = Tsoil,
      staPres = P_o,
      flux_lin = flux_mult * F_CO2_lin,
      flux_exp = flux_mult * F_CO2_exp
    ) |>
    dplyr::select(RepNum, staPres, VSWC, soilTemp, flux_lin, flux_exp)

  out_flux <- cbind(test_times, meas_flux)

  return(out_flux)
}
