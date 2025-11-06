# Figure 4: Make a plot of the direct comparison between field measured data
# and neonSoilFlux outputs at each validation site

# Load in the associated libraries
library(tidyverse)
library(lubridate)
library(broom)
library(grid)
library(gridExtra)

# (1) Load up flux data (field and neonSoilFlux outputs)
load("data/derived/combined-field-data.Rda")
load("data/derived/field-data-info.Rda")

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

# Join the field data neonSoilFlux data to match when field sites were visited.
field_data_joined_test <- field_data_joined |>
  inner_join(select(measurement_times, site, curr_tz), by = "site") |>
  mutate(
    model_data_mq = map2(
      .x = model_data_mq,
      .y = curr_tz,
      .f = ~ (.x |>
        mutate(
          startDateTime = lubridate::with_tz(startDateTime, tzone = .y)
        ))
    ),
    model_data_marshall = map2(
      .x = model_data_marshall,
      .y = curr_tz,
      .f = ~ (.x |>
        mutate(
          startDateTime = lubridate::with_tz(startDateTime, tzone = .y)
        ))
    ),
    field_flux = map2(
      .x = field_flux,
      .y = curr_tz,
      .f = ~ (.x |>
        mutate(
          startDateTime = lubridate::with_tz(startDateTime, tzone = .y)
        ))
    )
  ) |>
  rename(millington_quirk = model_data_mq, marshall = model_data_marshall) |>
  pivot_longer(cols = c("millington_quirk", "marshall")) |>
  rename(tortuosity = name) |>
  select(-NEON_VSWC, -NEON_soilTemp, -field_env) |>
  mutate(value = map2(
    .x = value,
    .y = tortuosity,
    .f = ~ (
      .x |> select(startDateTime, flux_compute) |> mutate(tort = .y) |> unnest(
        cols =
          c(flux_compute)
      )
    )
  )) |>
  select(-tortuosity)

## All of this is run from field_data_joined_test
days_at_site <- field_data_joined_test |>
  mutate(days = map_dbl(
    .x = field_flux,
    .f = ~ (.x$startDateTime |>
      as_date() |> unique() |> length())
  )) |>
  select(site, days) |>
  distinct(site, .keep_all = TRUE)

base_plot_level <- function(input_site,
                            input_method,
                            plot_title,
                            y_limits,
                            x_axis = TRUE,
                            y_axis = TRUE) {
  model_results <- field_data_joined_test |>
    filter(site == input_site) |>
    select(value) |>
    unnest(cols = c("value")) |>
    ungroup() |>
    filter(method == input_method) |>
    mutate(
      flux_min = flux - flux_err,
      flux_max = flux + flux_err
    )

  field_results <- field_data_joined_test |>
    filter(site == input_site) |>
    select(field_flux) |>
    unnest(cols = c("field_flux")) |>
    ungroup() |>
    mutate(horizontalPosition = fct_relevel(horizontalPosition, "LICOR"))

  start_date <- field_results |>
    pull(startDateTime) |>
    min() |>
    floor_date(unit = "day")

  end_date <- field_results |>
    pull(startDateTime) |>
    max() |>
    floor_date(unit = "day")

  # subtract a day
  start_date <- start_date - as.difftime(1, units = "days")
  end_date <- end_date + as.difftime(1, units = "days")

  out_plot <- model_results |> # Make sure errors aren't negative
    mutate(tort = if_else(tort == "millington_quirk", "Millington-Quirk", "Marshall")) |>
    ggplot(aes(x = startDateTime, y = flux)) +
    geom_line(aes(color = tort), linewidth = 1.5) +
    #    geom_ribbon(aes(ymin=flux_min,ymax=flux_max,fill=tort),alpha=0.25) +
    geom_point(
      data = field_results,
      aes(x = startDateTime, y = flux, color = "LICOR"),
      size = 1.5,
      inherit.aes = FALSE,
      alpha = 0.6
    ) +
    theme(axis.text.x = element_text(angle = -90)) +
    scale_x_datetime(
      "Date",
      breaks = scales::date_breaks("1 day"),
      minor_breaks = scales::date_breaks("6 hours"),
      date_labels = "%m-%d",
      limits = c(start_date, end_date)
    ) +
    # scale_y_continuous(limits=input_limits) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = -90),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      strip.text = element_text(size = 14)
    ) +
    labs(y = bquote(~ F[.(input_method)] ~ "(" ~ mu * mol ~ m^-2 ~
      s^-1 * ~")")) +
    scale_color_manual(values = c(
      "LICOR" = "#E69F00",
      # Orange for Field Data
      "Marshall" = "#009E73",
      # Teal for Method 1
      "Millington-Quirk" = "#CC79A7"
    )) +
    coord_cartesian(ylim = y_limits) +
    labs(color = "Flux: ") # Points are kept, just cropped from view


  if (plot_title) {
    out_plot <- out_plot + ggtitle(input_site)
  }


  if (!x_axis) {
    out_plot <- out_plot + theme(
      axis.title.x = element_blank(), axis.text.x =
        element_blank()
    )
  }

  if (!y_axis) {
    out_plot <- out_plot + theme(axis.title.y = element_blank())
  }

  return(out_plot)
}


plot_level <- function(input_site,
                       input_method,
                       plot_title,
                       y_limits,
                       x_axis = TRUE,
                       y_axis = TRUE) {
  model_results <- field_data_joined_test |>
    filter(site == input_site) |>
    select(value) |>
    unnest(cols = c("value")) |>
    ungroup() |>
    filter(method == input_method) |>
    mutate(
      flux_min = flux - flux_err,
      flux_max = flux + flux_err
    )

  field_results <- field_data_joined_test |>
    filter(site == input_site) |>
    select(field_flux) |>
    unnest(cols = c("field_flux")) |>
    ungroup() |>
    mutate(horizontalPosition = fct_relevel(horizontalPosition, "LICOR"))

  start_date <- field_results |>
    pull(startDateTime) |>
    min() |>
    floor_date(unit = "day")

  end_date <- field_results |>
    pull(startDateTime) |>
    max() |>
    floor_date(unit = "day")

  # subtract a day
  start_date <- start_date - as.difftime(0.5, units = "days")
  end_date <- end_date + as.difftime(0.5, units = "days")

  out_plot <- model_results |> # Make sure errors aren't negative
    mutate(tort = if_else(tort == "millington_quirk", "Millington-Quirk", "Marshall")) |>
    ggplot(aes(x = startDateTime, y = flux)) +
    geom_line(aes(color = tort), linewidth = 1.5) +
    geom_ribbon(aes(
      ymin = flux_min,
      ymax = flux_max,
      fill = tort
    ), alpha = 0.25) +
    geom_point(
      data = field_results,
      aes(x = startDateTime, y = flux, color = "LICOR"),
      size = 1.5,
      inherit.aes = FALSE,
      alpha = 0.6
    ) +
    theme(axis.text.x = element_text(angle = -90)) +
    scale_x_datetime(
      "Date",
      breaks = scales::date_breaks("1 day"),
      minor_breaks = scales::date_breaks("6 hours"),
      date_labels = "%m-%d",
      limits = c(start_date, end_date)
    ) +
    # scale_y_continuous(limits=input_limits) +
    theme_bw() +
    theme(
      legend.position = "none",
      legend.text = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = -90, vjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      strip.text = element_text(size = 14)
    ) +
    labs(y = bquote(~ F[.(input_method)] ~ "(" ~ mu * mol ~ m^-2 ~
      s^-1 * ~")")) +
    scale_color_manual(values = c(
      "LICOR" = "#E69F00",
      # Orange for Field Data
      "Marshall" = "#009E73",
      # Teal for Method 1
      "Millington-Quirk" = "#CC79A7"
    )) +
    scale_fill_manual(values = c(
      "LICOR" = "#E69F00",
      # Orange for Field Data
      "Marshall" = "#009E73",
      # Teal for Method 1
      "Millington-Quirk" = "#CC79A7"
    )) +
    coord_cartesian(ylim = y_limits) # Points are kept, just cropped from view

  if (plot_title) {
    out_plot <- out_plot + ggtitle(input_site)
  }

  if (!x_axis) {
    out_plot <- out_plot + theme(
      axis.title.x = element_blank(), axis.text.x =
        element_blank()
    )
  }

  if (!y_axis) {
    out_plot <- out_plot + theme(axis.title.y = element_blank())
  }

  return(out_plot)
}

# Define a base plot for the legend
base_plot <- base_plot_level("UNDE",
  "000",
  TRUE,
  y_limits = c(0, 6),
  x_axis = FALSE
)
shared_legend <- lemon::g_legend(base_plot)

# double check min and max flux values to enable limit setting
# set to TRUE to run this
if (FALSE) {
  field_data_joined_test |>
    unnest(cols = c("value")) |>
    group_by(site) |>
    summarise(
      min_flux = min(flux, na.rm = TRUE),
      max_flux = max(flux, na.rm = TRUE),
      .groups = "drop")
}

y_limits <- tibble(
  site = c("KONZ", "UNDE", "WOOD",
           "SRER", "SJER", "WREF"),
  limits = list(c(-10, 22), c(-0.5, 4.5), c(-3.5, 11),
                c(0, 2), c(0, 2), c(-0.5, 6.5))
)

grid_plots <- summary_env_data |>
  inner_join(y_limits, by = "site") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  arrange(site) |>
  mutate(
    y_axis = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    first_row = pmap(
      .l = list(site, y_axis, limits),
      .f = ~ plot_level(..1, "000", TRUE, ..3, FALSE, ..2) |> ggplotGrob()
    ),
    second_row = pmap(
      .l = list(site, y_axis, limits),
      .f = ~ plot_level(..1, "101", FALSE, ..3, FALSE, ..2) |> ggplotGrob()
    ),
    third_row = pmap(
      .l = list(site, y_axis, limits),
      .f = ~ plot_level(..1, "110", FALSE, ..3, FALSE, ..2) |> ggplotGrob()
    ),
    fourth_row = pmap(
      .l = list(site, y_axis, limits),
      .f = ~ plot_level(..1, "011", FALSE, ..3, TRUE, ..2) |> ggplotGrob()
    ),
  )


grid_plots_rev <- grid_plots |>
  inner_join(days_at_site, by = "site")

# now start putting this all together
g1 <- rbind(
  grid_plots_rev$first_row[[1]],
  grid_plots_rev$second_row[[1]],
  grid_plots_rev$third_row[[1]],
  grid_plots_rev$fourth_row[[1]],
  size = "first"
) |>
  arrangeGrob(widths = unit(grid_plots_rev$days[[1]], "cm"))

g2 <- rbind(
  grid_plots_rev$first_row[[2]],
  grid_plots_rev$second_row[[2]],
  grid_plots_rev$third_row[[2]],
  grid_plots_rev$fourth_row[[2]],
  size = "first"
) |>
  arrangeGrob(widths = unit(grid_plots_rev$days[[2]], "cm"))

# WREF
g3 <- rbind(
  grid_plots_rev$first_row[[3]],
  grid_plots_rev$second_row[[3]],
  grid_plots_rev$third_row[[3]],
  grid_plots_rev$fourth_row[[3]],
  size = "first"
) |>
  arrangeGrob(widths = unit(grid_plots_rev$days[[3]], "cm"))

g4 <- rbind(
  grid_plots_rev$first_row[[4]],
  grid_plots_rev$second_row[[4]],
  grid_plots_rev$third_row[[4]],
  grid_plots_rev$fourth_row[[4]],
  size = "first"
) |>
  arrangeGrob(widths = unit(grid_plots_rev$days[[4]], "cm"))

g5 <- rbind(
  grid_plots_rev$first_row[[5]],
  grid_plots_rev$second_row[[5]],
  grid_plots_rev$third_row[[5]],
  grid_plots_rev$fourth_row[[5]],
  size = "first"
) |>
  arrangeGrob(widths = unit(grid_plots_rev$days[[5]], "cm"))

g6 <- rbind(
  grid_plots_rev$first_row[[6]],
  grid_plots_rev$second_row[[6]],
  grid_plots_rev$third_row[[6]],
  grid_plots_rev$fourth_row[[6]],
  size = "first"
) |>
  arrangeGrob(widths = unit(grid_plots_rev$days[[6]], "cm"))

out_big <- arrangeGrob(
  g1,
  g2,
  g3,
  g4,
  g5,
  g6,
  nrow = 1,
  widths = c(1.8, 1.5, 0.9, 1.2, 1.2, 1.2),
  bottom = shared_legend$grobs[[1]],
  vp = viewport(
    width = 0.75,
    height = 1,
    clip = TRUE
  )
)

ggsave(
  "figures/flux-results-test.png",
  plot = out_big,
  width = 14,
  height = 8
)
