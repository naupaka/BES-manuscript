### Figure 6: Derive fit values for the different model
# results compared to measured values, compare against daily averaged values.

### Load up the associated libraries
library(tidyverse)
library(lubridate)
library(broom)

# (1) Load up data and measured fluxes for each site
load("data/derived/combined-field-data.Rda")

# Prevent plotting to pdf if not in interactive mode
if (!interactive()) grDevices::pdf(NULL)

# Compute some summary stats, used primarily for plot presentation
summary_env_data <- field_data_joined |>
  select(site, field_env) |>
  unnest(cols = c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = mean(VSWC),
    temp_data = mean(soilTemp),
    .groups = "drop"
  )

# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window
# Helper function to co-locate field obs and neonSoilFlux obs in same half-hourly window.  The lag determines how far back in time the field data are compared to the computed flux. (lag_time = 30 means we compare field data to the computed flux a half hour previously.)
standardize_timestamps <- function(input_model_data,
                                   input_field_data,
                                   lag_time = 0) {
  # lag_time is the number of minutes we subtract from field data to comapre with NEON
  # Create a tibble of intervals for NEON
  start_times <- input_model_data$startDateTime
  end_times <- input_model_data$startDateTime + 30 * 60 # Add 30 minutes
  intervals <- interval(start_times, end_times)
  dates <- tibble(start_times, intervals)

  # Create a binary variable indicating if any gap-filled environmental data were used to calculate flux
  model_dataset <- input_model_data |>
    mutate(fluxMeanQF = if_any(ends_with("MeanQF"), ~ .x == 1))

  # Create a vector of LICOR reported times - we shift it by a the lag to see
  measurement_times <- input_field_data$startDateTime - lubridate::minutes(lag_time)

  # Figure out which NEON intervals the LICOR measured, adding it to the field data
  my_intervals <- input_field_data |>
    mutate(ival = intervals[map_int(
      measurement_times,
      ~ which(.x %within% intervals)
    )])

  # Join the intervals to the NEON dates so we have a NEON timestamp for each LICOR
  NEON_field_timestamp <- my_intervals |>
    group_by(ival) |>
    nest() |>
    inner_join(dates, by = c("ival" = "intervals")) |>
    rename(field_flux = data)

  # Join these up now
  joined_data <- model_dataset |>
    inner_join(NEON_field_timestamp,
      by = c("startDateTime" = "start_times")
    ) |>
    select(startDateTime, flux_compute, field_flux, fluxMeanQF)

  # Prepare a data frame of timestamped NEON and field data for analysis
  out_data <- joined_data |>
    mutate(field_flux = map(
      field_flux,
      ~ select(.x, -startDateTime)
    )) |>
    # select(flux_compute,field_flux,fluxMeanQF) |>
    unnest(cols = c("field_flux")) |>
    rename(flux_field = flux) |>
    unnest(cols = c("flux_compute")) |>
    select(-horizontalPosition) |>
    ungroup()

  return(out_data)
}

# (3) Compute R2 and other regression stats.
extract_stats <- function(harmonized_data) {
  harmonized_data |>
    group_by(method, name) |>
    nest() |>
    mutate(
      nrmse = map_dbl(
        .x = data,
        .f = ~ sqrt(sum((.x$flux - .x$flux_field)^2) /
          nrow(.x)) /
          mean(.x$flux_field)
      ), # computed nrmse - we divide by mean of field obs for standardization
      lm = map(
        .x = data,
        .f = ~ lm(flux ~ flux_field, data = .x)
      ),
      coeff = map(lm, broom::tidy),
      vals = map(lm, broom::glance),
      r.squared = map_dbl(.x = vals, .f = ~ pull(.x, r.squared)),
      intercept = map_dbl(.x = coeff, .f = ~ pull(.x, estimate)[[1]]),
      slope = map_dbl(.x = coeff, .f = ~ pull(.x, estimate)[[2]]),
      p.value = map_dbl(.x = coeff, .f = ~ pull(.x, p.value)[[2]]),
    ) |>
    select(method, nrmse, r.squared, intercept, slope, p.value) |>
    ungroup()
}

### OK, let's get cooking!
field_stats_data_0 <- field_data_joined |>
  pivot_longer(cols = c("model_data_mq", "model_data_marshall")) |>
  mutate(harmonized_data = map2(
    .x = value,
    .y = field_flux,
    .f = ~ standardize_timestamps(.x,
      .y,
      lag_time = 0
    )
  )) |>
  mutate(new_data = map(
    .x = harmonized_data,
    .f = ~ (.x |>
      filter(instrument == "6800") |>
      mutate(day = day(startDateTime)) |>
      group_by(day, method) |>
      mutate(
        flux = if_else(flux < 0,
          NA,
          flux
        ),
        flux_field = if_else(flux_field < 0,
          NA,
          flux_field
        )
      ) |>
      summarize(
        flux = mean(flux, na.rm = TRUE),
        flux_field = mean(flux_field,
          na.rm = TRUE
        )
      ))
  )) |>
  select(site, name, new_data) |>
  unnest(cols = c(new_data)) |>
  ungroup() |>
  mutate(name = if_else(name == "model_data_mq", "Millington-Quirk", "Marshall")) |>
  inner_join(summary_env_data, by = "site") |>
  mutate(site = fct_reorder(site, temp_data))

# Now fix the labels
field_stats_data_0$method <- as.character(field_stats_data_0$method)
field_stats_data_0$method <- gsub(
  "([0-9]+)", "F[\\\"\\1\\\"]",
  field_stats_data_0$method
)

# Compute the summary stats:
regression_stats <- extract_stats(field_stats_data_0) |>
  mutate(
    stars = case_when(
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),

    # deals with aligning stats properly including with negative intercepts
    slope_str = if_else(slope >= 0,
      paste0(" ", sprintf("%.2f", slope)),
      sprintf("%.2f", slope)
    ),
    intercept_str = if_else(intercept >= 0,
      paste0(" ", sprintf("%.2f", intercept)),
      sprintf("%.2f", intercept)
    ),
    r2_str = if_else(r.squared >= 0,
      paste0(" ", sprintf("%.2f", r.squared)),
      sprintf("%.2f", r.squared)
    ),
    label = paste0(
      "R²=", r2_str, stars, "\n",
      "m =", slope_str, "\n",
      "b =", intercept_str
    ),
    x = 12, # position on x-axis, manually tweaked ot match plot size below
    y = 1.5,
    eq = paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x"),
    xeq = 4.5,
    yeq = 13
  )

# CVD friend palette:
okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00" # vermillion
)

# Now let's get plotting!
r2_plot <- field_stats_data_0 |>
  ggplot(aes(x = flux_field, y = flux)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(color = site, shape = site, fill = site), size = 6) +
  ylim(c(0, 18)) +
  xlim(c(0, 18)) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 18),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    strip.text = element_text(size = 18)
  ) +
  geom_text(
    data = regression_stats,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    hjust = 0,
    vjust = 0.2,
    family = "mono", # for aligning stats
    fontface = "bold"
  ) +
  facet_grid(method ~ name,
    labeller = label_parsed
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 8)) +
  scale_fill_manual(values = okabe_ito) +
  scale_color_manual(values = okabe_ito) +
  labs(
    x = bquote(~LICOR ~ F[S] ~ "(" ~ mu * mol ~ m^-2 ~ s^-1 * ~")"),
    y = bquote(~neonSoilFlux ~ F[S] ~ "(" ~ mu * mol ~ m^-2 ~ s^-1 * ~")"),
    color = "Site:",
    shape = "Site:",
    fill = "Site:"
  )

# Save the table to a file for use in the manuscript
ggsave("figures/r2-plot-test.png",
       plot = r2_plot, width = 10, height = 14)
