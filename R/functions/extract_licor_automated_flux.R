#' @title Reads in an automated LICOR file with csv data and computes the associated flux.
#'
#' @param file_name Name of the JSON file to be read in
#' @param tz_ids Data frame of timezones for the different sites
#'
#' @return a data frame of licor fluxes
#' @export
#'
extract_licor_automated_flux <- function(file_name, site_name, tz_ids) {
  # https://www.licor.com/env/support/LI-6800/topics/symbols.html#meas --> info about data

  in_data <- read_csv(file_name, skip = 1) |>
    slice_tail(n = -1)

  fluxes <- in_data$FCO2 |> as.numeric()
  soilTemp <- in_data$TS_1 |> as.numeric()
  VSWC <- in_data$SWC_1 |> as.numeric()
  staPres <- in_data$PA |> as.numeric()

  curr_tz <- tz_ids |>
    dplyr::filter(NEON == site_name) |>
    dplyr::pull(tz)

  # sst <- lubridate::ymd_hms(in_data$`DATE_TIME initial_value`)
  times <- map_chr(in_data$TIME, .f = ~ (paste0(
    substr(.x, 1, 2), ":",
    substr(.x, 3, 4), ":",
    substr(.x, 5, 6)
  )))

  sst <- paste0(in_data$DATE, " ", times)

  test_times <- tibble::tibble(
    site = site_name,
    date = lubridate::ymd_hms(sst, tz = curr_tz) |>
      lubridate::with_tz(tzone = "UTC")
  )

  out_flux <- cbind(test_times, staPres, VSWC, soilTemp, fluxes) |>
    dplyr::arrange(date)

  return(out_flux)
}
