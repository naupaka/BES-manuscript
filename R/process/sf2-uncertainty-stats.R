### Supplemental figure 2: Plot Signal to Noise ratios and a within bound plot for different soil flux outputs

# Load up associated libraries
library(tidyverse)
library(lubridate)
library(broom)
library(gridExtra)

### Script objectives:
# (1) Load up data and measured fluxes for each site
# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window

# (1) Load up flux data
load("data/derived/combined-field-data.Rda")

# Compute some summary stats.  Organized by the mean temperature and mean SWC
summary_env_data <- field_data_joined |>
  select(site, field_env) |>
  unnest(cols = c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = mean(VSWC),
    temp_data = mean(soilTemp),
    .groups = "drop"
  )

### First plot: histogram of signal to noise ratios
# Compute the signal to noise ratio for the different computed fluxes
snr_data <- field_data_joined |>
  select(site, model_data_mq, model_data_marshall) |>
  pivot_longer(cols = c("model_data_mq", "model_data_marshall")) |>
  mutate(value = map(.x = value, .f = ~ (select(.x, startDateTime, flux_compute)) |>
    unnest(cols = c(flux_compute)) |>
    mutate(snr = abs(flux / flux_err)))) |>
  unnest(cols = c(value))


# Signal to noise ratio plot
snr_plot_mq <- snr_data |>
  inner_join(summary_env_data, by = "site") |>
  filter(name == "model_data_mq") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  ggplot(aes(x = snr, fill = method)) +
  geom_histogram(alpha = 0.6, binwidth = 0.2, position = "identity") +
  facet_grid(. ~ site) + # xlim(c(0,10)) +
  labs(x = "SNR", y = "Count", fill = "Flux method:") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  scale_fill_discrete(labels = c(
    "000" = bquote(~ F["000"]),
    "011" = bquote(~ F["011"]),
    "101" = bquote(~ F["101"]),
    "110" = bquote(~ F["110"])
  )) +
  ggtitle(bquote(~ D[a] ~ "calculation: Millington-Quirk"))


snr_plot_marshall <- snr_data |>
  inner_join(summary_env_data, by = "site") |>
  filter(name == "model_data_marshall") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  ggplot(aes(x = snr, fill = method)) +
  geom_histogram(alpha = 0.6, binwidth = 0.2, position = "identity") +
  facet_grid(. ~ site) + # xlim(c(0,10)) +
  labs(x = "SNR", y = NULL, fill = "Flux method:") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  scale_fill_discrete(labels = c(
    "000" = bquote(~ F["000"]),
    "011" = bquote(~ F["011"]),
    "101" = bquote(~ F["101"]),
    "110" = bquote(~ F["110"])
  )) +
  # guides(fill = "none") +
  ggtitle(bquote(~ D[a] ~ "calculation: Marshall"))


### Second plot: are observed field fluxes within the bounds of the data?

# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window.  The lag determines how far back in time the field data are compared to the computed flux. (lag_time = 30 means we compare field data to the computed flux a half hour previously.)

# Helper Function to standardize timestamps with a lag
standardize_timestamps <- function(input_model_data, input_field_data, lag_time = 0) {
  # lag_time is the number of minutes we subtract from field data to compare with NEON
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
    mutate(ival = intervals[map_int(measurement_times, ~ which(.x %within% intervals))])

  # Join the intervals to the NEON dates so we have a NEON timestamp for each LICOR
  NEON_field_timestamp <- my_intervals |>
    group_by(ival) |>
    nest() |>
    inner_join(dates, by = c("ival" = "intervals")) |>
    rename(field_flux = data)

  # Join these up now
  joined_data <- model_dataset |>
    inner_join(NEON_field_timestamp, by = c("startDateTime" = "start_times")) |>
    select(startDateTime, flux_compute, field_flux, fluxMeanQF)

  # Prepare a data frame of timestamped NEON and field data for analysis

  out_data <- joined_data |>
    mutate(field_flux = map(field_flux, ~ select(.x, -startDateTime))) |>
    # select(flux_compute,field_flux,fluxMeanQF) |>
    unnest(cols = c("field_flux")) |>
    rename(flux_field = flux) |>
    unnest(cols = c("flux_compute")) |>
    select(-horizontalPosition) |>
    ungroup()

  return(out_data)
}

# Collect all of the field and computed flux data within the same time interval
field_stats_data <- field_data_joined |>
  pivot_longer(cols = c("model_data_mq",
                        "model_data_marshall")) |>
  mutate(harmonized_data = map2(.x = value,
                                .y = field_flux,
                                .f = ~ standardize_timestamps(.x,
                                                              .y,
                                                              lag_time = 0)))

# Determine if the bounds are within a window of data
within_bounds <- function(data) {
  reduce_factor <- seq(0.1, 1, by = 0.1)

  tibble(
    reduction = reduce_factor,
    values = map(.x = reduction, .f = ~ (data |>
      mutate(in_bounds = between(flux_field,
                                 flux - .x * flux_err,
                                 flux + .x * flux_err)) |>
      group_by(method) |>
      summarize(tot_prop = sum(in_bounds) / n()) |>
      ungroup()))
  ) |>
    unnest(cols = c(values))
}

# Now start to combine the different sites together
bounds_data <- field_stats_data |>
  mutate(values = map(harmonized_data, within_bounds)) |>
  select(site, name, values) |>
  unnest(cols = c(values))

bounds_plot_mq <- bounds_data |>
  filter(name == "model_data_mq") |>
  inner_join(summary_env_data, by = "site") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  ggplot(aes(x = 1 - reduction, y = tot_prop, color = method)) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ site) +
  labs(x = bquote(epsilon),
       y = "Proportion within range",
       color = "Flux method:") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  scale_color_discrete(labels = c(
    "000" = bquote(~ F["000"]),
    "011" = bquote(~ F["011"]),
    "101" = bquote(~ F["101"]),
    "110" = bquote(~ F["110"])
  )) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    minor_breaks = seq(0.1, 0.9, by = 0.2)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    minor_breaks = seq(0.1, 0.9, by = 0.2)
  )

bounds_plot_marshall <- bounds_data |>
  filter(name == "model_data_marshall") |>
  inner_join(summary_env_data, by = "site") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  ggplot(aes(x = 1 - reduction, y = tot_prop, color = method)) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ site) +
  labs(x = bquote(epsilon), y = NULL, color = "Flux method:") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  scale_color_discrete(labels = c(
    "000" = bquote(~ F["000"]),
    "011" = bquote(~ F["011"]),
    "101" = bquote(~ F["101"]),
    "110" = bquote(~ F["110"])
  )) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    minor_breaks = seq(0.1, 0.9, by = 0.2)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    minor_breaks = seq(0.1, 0.9, by = 0.2)
  )

# Now put the two plots together, lining them up correctly
# this will be a two by two plot
g1a <- ggplotGrob(snr_plot_mq + guides(fill = "none"))
g2a <- ggplotGrob(snr_plot_marshall + guides(fill = "none"))

shared_legend_snr <- lemon::g_legend(snr_plot_mq)
g_snr <- grid.arrange(g1a, g2a,
                      nrow = 1,
                      bottom = shared_legend_snr$grobs[[1]])

g1b <- ggplotGrob(bounds_plot_mq + guides(color = "none"))
g2b <- ggplotGrob(bounds_plot_marshall + guides(color = "none"))

shared_legend_bounds <- lemon::g_legend(bounds_plot_mq)
g_bounds <- grid.arrange(g1b, g2b,
                         nrow = 1,
                         bottom = shared_legend_bounds$grobs[[1]])

g_all <- grid.arrange(g_snr, g_bounds, ncol = 1)

png("figures/uncertainty-stats.png",
    width = 18,
    height = 6,
    units = "in",
    res = 300)
grid::grid.draw(g_all)
dev.off()
