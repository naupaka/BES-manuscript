# Figure 5: Make a plot of the different methods and the fluxes across a whole year.

# Load up associated libraries
library(tidyverse)
library(lubridate)
library(broom)
library(gridExtra)

# Load up the data used for plot generation
load("data/derived/all-year-flux-results.Rda")
load("data/derived/combined-field-data.Rda")

# For each site, only filter on the position where we computed
marshall_fluxes_small <- model_fluxes_marshall |>
  semi_join(field_data_joined,
            by = c("site", "horizontalPosition" = "sampling_location"))

mq_fluxes_small <- model_fluxes_mq |>
  semi_join(field_data_joined,
            by = c("site", "horizontalPosition" = "sampling_location"))

# Helper function to standardize plotting the flux across each site
plot_daily_flux <- function(site_name, plot_legend, x_axis, y_axis) {
  mq_plot_data <- mq_fluxes_small |>
    filter(site == site_name) |>
    unnest(cols = c("flux_compute")) |>
    mutate(day = lubridate::floor_date(startDateTime, unit = "day")) |>
    group_by(day) |>
    summarize(m_flux = mean(flux, na.rm = TRUE)) |>
    mutate(method = "Millington-Quirk")

  marshall_plot_data <- marshall_fluxes_small |>
    filter(site == site_name) |>
    unnest(cols = c("flux_compute")) |>
    mutate(day = lubridate::floor_date(startDateTime, unit = "day")) |>
    group_by(day) |>
    summarize(m_flux = mean(flux, na.rm = TRUE)) |>
    mutate(method = "Marshall")

  field_data <- field_data_joined |>
    filter(site == site_name) |>
    unnest(cols = c("field_flux")) |>
    mutate(day = lubridate::floor_date(startDateTime, unit = "day")) |>
    group_by(day) |>
    summarize(m_flux = mean(flux, na.rm = TRUE))

  # now plot these all together!
  base_plot <- ggplot() +
    geom_line(
      data = rbind(marshall_plot_data, mq_plot_data),
      aes(x = day, y = m_flux, color = method, linetype = method), linewidth = 1
    ) +
    geom_point(
      data = field_data,
      aes(x = day, y = m_flux, color = "LICOR"), # Map color to a legend label
      size = 3, alpha = 0.6
    ) +
    theme(axis.text.x = element_text(angle = -90)) +
    scale_x_datetime("Date",
      breaks = scales::date_breaks("3 months"),
      minor_breaks = scales::date_breaks("1 month"),
      date_labels = "%Y-%m"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      strip.text = element_text(size = 14)
    ) +
    ggtitle(site_name) +
    ylim(c(0, 20)) +
    scale_color_manual(values = c(
      "LICOR" = "#E69F00", # Orange for Field Data
      "Marshall" = "#009E73", # Teal for Method 1
      "Millington-Quirk" = "#CC79A7"
    )) +
    guides(linetype = "none")

  if (plot_legend) {
    base_plot <- base_plot +
      labs(
        y = bquote(~ F[S] ~ "(" ~ mu * mol ~ m^-2 ~ s^-1 * ~")"),
        color = "Flux:"
      )
  } else {
    base_plot <- base_plot +
      labs(y = bquote(~ F[S] ~ "(" ~ mu * mol ~ m^-2 ~ s^-1 * ~")")) +
      guides(
        color = "none",
        linetype = "none"
      )
  }

  if (!x_axis) {
    base_plot <- base_plot + theme(axis.title.x = element_blank())
  }

  if (!y_axis) {
    base_plot <- base_plot + theme(axis.title.y = element_blank())
  }

  return(base_plot)
}

# Now we start assembling things together
# SJER SRER WREF  UNDE KONZ WOOD

out_plots <- tibble(
  site = c("SJER", "SRER", "WREF", "UNDE", "KONZ", "WOOD"),
  x_axis = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
  y_axis = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
) |>
  mutate(plot = pmap(.l = list(site, x_axis, y_axis), .f = ~ plot_daily_flux(..1, FALSE, ..2, ..3)))

# Now we need to arrange a select number of these

# We will have 3 plots here
g1 <- ggplotGrob(out_plots$plot[[1]])
g2 <- ggplotGrob(out_plots$plot[[2]])
g3 <- ggplotGrob(out_plots$plot[[3]])

g4 <- ggplotGrob(out_plots$plot[[4]])
g5 <- ggplotGrob(out_plots$plot[[5]])
g6 <- ggplotGrob(out_plots$plot[[6]])

shared_legend <- lemon::g_legend(plot_daily_flux("KONZ", TRUE, FALSE, FALSE))

out_big <- grid.arrange(g1, g2, g3, g4, g5, g6,
  layout_matrix = rbind(c(1, 4), c(2, 5), c(3, 6)),
  bottom = shared_legend$grobs[[1]]
)

### Now let's get these all plotted!
ggsave("figures/flux-results-year.png", plot = out_big, width = 7, height = 6)
