### Figure 7: Use the measured LICOR data and flux gradient data to back-compute
# diffusivity, ultimately comparing the two approaches to each other

# Load up the associated libraries:
library(tidyverse)
library(lubridate)

# All the env data are contained in the combined data frame
# field_data_joined
load("data/derived/combined-field-data.Rda")

# co2 gradient from neonSoilFlux (computed in 03-diffusivity-prepare.R)
load("data/derived/diffusivity-gradient.Rda")
## Note: when gradient is positive, then the co2 at the deeper depth is smaller.

# Compute some summary stats.  Organize by the temperature
# this is how we make sure each site is ordered in our plots.
summary_env_data <- field_data_joined |>
  select(site, field_env) |>
  unnest(cols = c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = mean(VSWC),
    temp_data = mean(soilTemp),
    .groups = "drop"
  ) |>
  arrange(temp_data)

combined_data <- field_data_joined |>
  inner_join(flux_gradient_diffusivity, by = "site") |>
  rename(neon_diffusivity_gradient = model_data) |>
  mutate(combined_field = map2(.x = field_flux,
                               .y = field_env,
                               .f = ~ inner_join(.x, .y,
                                                 by = c("startDateTime",
                                                        "instrument",
                                                        "horizontalPosition")))) |>
  select(site, sampling_location, combined_field, model_data_mq,
         model_data_marshall, neon_diffusivity_gradient, curr_tz)

### Helper function to standardize the timestapes between field and measured data
standardize_timestamps <- function(input_model_data, input_field_data) {
  # lag_time is the number of minutes we subtract from field data to comapre with NEON
  # Create a tibble of intervals for NEON
  start_times <- input_model_data$startDateTime
  end_times <- input_model_data$startDateTime + 30 * 60 # Add 30 minutes
  intervals <- interval(start_times, end_times)
  dates <- tibble(start_times, intervals)

  # Create a vector of LICOR reported times - we shift it by a the lag to see
  measurement_times <- input_field_data$startDateTime

  # Figure out which NEON intervals the LICOR measured, adding it to the field data
  my_intervals <- input_field_data |>
    mutate(ival = intervals[map_int(measurement_times, ~ which(.x %within% intervals))])

  # Join the intervals to the NEON dates so we have a NEON timestamp for each LICOR
  NEON_field_timestamp <- my_intervals |>
    group_by(ival) |>
    nest() |>
    inner_join(dates, by = c("ival" = "intervals")) |>
    rename(field_flux = data)

  # Join these up now
  joined_data <- input_model_data |>
    inner_join(NEON_field_timestamp, by = c("startDateTime" = "start_times")) |>
    select(startDateTime, co2gradient, field_flux)

  # Prepare a data frame of timestamped NEON and field data for analysis
  ### When the gradient negative, that means fluxes are coming from the soil

  out_data <- joined_data |>
    mutate(
      field_flux = map(field_flux, ~ select(.x, -startDateTime)),
      gradient = map_dbl(co2gradient, .f = ~ pull(.x, gradient)), # Get the gradient
      diffusivity_field = map2_dbl(.x = gradient, .y = field_flux, .f = ~ (-mean(.y$flux / .x, na.rm = TRUE))),
      diffusivity_fieldExpUncert = map2_dbl(.x = gradient, .y = field_flux, .f = ~ (sd(.y$flux / .x, na.rm = TRUE)))
    ) |>
    select(startDateTime, diffusivity_field, diffusivity_fieldExpUncert) |>
    ungroup()

  return(out_data)
}

# Collect all of the field and computed flux data within the same time interval
field_stats_data <- combined_data |>
  mutate(
    diffusivity_mq = map(.x = model_data_mq, .f = ~ (.x |> select(startDateTime, diffusivity) |> unnest(cols = c("diffusivity")) |> ungroup() |>
      mutate(method = "Millington-\nQuirk") |> select(diffusivity, method))),
    diffusivity_marshall = map(.x = model_data_marshall, .f = ~ (.x |> select(startDateTime, diffusivity) |> unnest(cols = c("diffusivity")) |> ungroup() |>
      mutate(method = "Marshall") |> select(diffusivity, method))),
    diffusivity_field = map2(.x = neon_diffusivity_gradient, .y = combined_field, .f = ~ standardize_timestamps(.x, .y) |>
      filter(diffusivity_field > 0) |>
      mutate(method = "Field\nestimated") |>
      select(diffusivity_field, method) |>
      rename(diffusivity = diffusivity_field))
  ) |>
  select(site, sampling_location, diffusivity_mq, diffusivity_marshall, diffusivity_field)

# Now we can be ready to generate the boxplot!
diffusivity_plot <- field_stats_data |>
  pivot_longer(cols = c("diffusivity_mq", "diffusivity_marshall", "diffusivity_field")) |>
  unnest(cols = c("value")) |>
  inner_join(summary_env_data, by = "site") |>
  ungroup() |>
  mutate(site = fct_reorder(site, temp_data)) |>
  ggplot(aes(x = method, y = diffusivity / 1e-6)) +
  geom_boxplot() +
  facet_grid(. ~ site, scales = "free") +
  #ylim(c(0, 10)) +
  labs( # y = "Diffusivity (*1e6)",
    y = bquote(~ D[a] ~ "(" ~ 10^6 ~ m^-2 ~ s^-1 * ~")"),
    x = bquote(~ D[a] ~ "calculation method")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 10, angle = 0),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  )

# Save the diffusivity plot to the associated file
ggsave(filename = "figures/diffusivity-plot.png",
       plot = diffusivity_plot, width = 16, height = 4)
