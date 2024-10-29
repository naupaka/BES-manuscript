# Plot of the different levels for a given flux value

# Choose 001, 010, 100, add on the top layer

library(tidyverse)
library(lubridate)
library(broom)
library(grid)
library(gridExtra)
### Goals:
# (1) Load up data and measured fluxes for each site
# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window

### Somehow the stats don't seem to be working, but perhaps show every 3 hours on a plot
### Convert to local time?


# (1) Load up flux data
load('data/derived/combined-field-data.Rda')

# Compute some summary stats.  Organize by the temperature
summary_env_data <- field_data_joined |>
  select(site,field_env) |>
  unnest(cols=c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = mean(VSWC),
    temp_data = mean(soilTemp),
    .groups="drop"
  )


# Get up the function that we need
load('data/derived/field-data-info.Rda')
field_data_joined_test <- field_data_joined |>
  inner_join(select(measurement_times,site,curr_tz),by="site") |>
  mutate(model_data = map2(.x=model_data,
                           .y=curr_tz,
                           .f=~(.x |>
                                  mutate(startDateTime = lubridate::with_tz(startDateTime,tzone = .y)))),
         field_flux = map2(.x=field_flux,
                           .y=curr_tz,
                           .f=~(.x |>
                                  mutate(startDateTime = lubridate::with_tz(startDateTime,tzone = .y)))) )


# Source the plotting functions for the data
files_sources = list.files('R/functions',full.names = TRUE,pattern="_plot")
sapply(files_sources, source)




plot_level <- function(input_data,field_data_in,input_levels,plot_surface,x_axis=TRUE,plot_title) {

  licor_data <- field_data_in |>
    mutate(horizontalPosition = fct_relevel(horizontalPosition,"LICOR") )

  start_date <- licor_data |>
    pull(startDateTime) |>
    min() |> floor_date(unit="day")

  end_date <- licor_data |>
    pull(startDateTime) |>
    max() |> ceiling_date(unit="day")



  if(input_levels =="top") {
    curr_level <- plot_surface
    plot_licor <- TRUE
  } else {
    curr_level <- input_levels
    plot_licor <- TRUE
  }

  if(plot_licor) {

    out_plot <- input_data |>
      filter(method %in% curr_level) |>
      mutate(flux_min = flux-flux_err,
             flux_max = flux+flux_err) |> # Make sure errors aren't negative
      ggplot(aes(x=startDateTime,y=flux)) + geom_line(aes(color=method),linewidth = 1.5) +
      geom_ribbon(aes(ymin=flux_min,ymax=flux_max,fill=method),alpha=0.25) +
      geom_point(data=licor_data,aes(x=startDateTime,y=flux,shape=instrument,color=instrument),size=2) +
      theme(axis.text.x=element_text(angle=-90)) +
      scale_x_datetime('Date (UTC)',
                       breaks = scales::date_breaks("6 hours"),
                       minor_breaks=scales::date_breaks("3 hours"),
                       date_labels = "%m-%d %H:%M",
                       limits = c(start_date,end_date)
      ) +
      # scale_y_continuous(limits=input_limits) +
      theme_bw() +
      theme(
        legend.position = "none",
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle=-90),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 14)
      )


  } else {
    out_plot <- input_data |>
      filter(method %in% curr_level) |>
      mutate(flux_min = flux-flux_err,
             flux_max = flux+flux_err) |> # Make sure errors aren't negative
      ggplot(aes(x=startDateTime,y=flux)) + geom_line(linewidth = 1.5,color='#c994c7') +
      geom_ribbon(aes(ymin=flux_min,ymax=flux_max),alpha=0.25,fill='#c994c7') +
      # geom_point(data=licor_data,aes(x=startDateTime,y=flux,shape=instrument,color=instrument),size=2) +
      theme(axis.text.x=element_text(angle=-90)) +
      scale_x_datetime('Date',
                       breaks = scales::date_breaks("6 hours"),
                       minor_breaks=scales::date_breaks("3 hours"),
                       date_labels = "%m-%d %H:%M",
                       limits = c(start_date,end_date)
      ) +
      # scale_y_continuous(limits=input_limits) +
      theme_bw() +
      theme(
        legend.position = "none",
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle=-90),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 14)
      )

  }

  if(!x_axis) {
    out_plot <- out_plot + theme(axis.title.x=element_blank(),
                                 axis.text.x=element_blank() )
  }

  if(input_levels =="top") {
    out_plot <- out_plot + labs(y=bquote(~F[.(input_levels)]~'('~mu*mol~m^-2~s^-1*~')'),
                                title = plot_title)
    #out_plot <- out_plot + labs(y=bquote(~F[S]~'('~mu*mol~m^-2~s^-1*~')'),
     #                           title = plot_title)
  } else {
    out_plot <- out_plot + labs(y=bquote(~F[.(input_levels)]~'('~mu*mol~m^-2~s^-1*~')'))
  }


  return(out_plot)

}

#plot_levels <- c("top", "100", "010", "001")
#plot_surface <- c("111", "000")
#plot_axes <- c(FALSE, FALSE, FALSE, TRUE)

plot_levels <- c("top", "010", "001")
plot_surface <- c("011", "000")
plot_axes <- c(FALSE, FALSE, TRUE)

make_joined_plot <- function(model_data_in,licor_data,plot_name) {

  # Unnest the data
  model_data <- model_data_in |>
    select(startDateTime, flux_compute) |>
    unnest(cols = c(flux_compute))

  # Define the separate levels we will use to display on the top - we combine two for ease of plotting

  plot_levels <- c("top", "101", "110", "011")
  plot_surface <- c("000")
  plot_axes <- c(FALSE, FALSE, FALSE, TRUE)

  #plot_levels <- c("top", "010", "001")
  #plot_surface <- c("011", "000")
  #plot_axes <- c(FALSE, FALSE, TRUE)

  # Define a data frame that makes the plot work
  plot_nest <- tibble(plot_levels,
                 plot_axes,
                 plots = map2(
                   .x = plot_levels,
                   .y = plot_axes,
                   .f = ~ plot_level(model_data, licor_data,
                                     input_levels = .x,
                                     plot_surface = plot_surface,
                                     x_axis = .y,
                                     plot_title = plot_name
                   )
                 )
  )



  # We will have 4 plots here
#  g2 <- ggplotGrob(plot_nest$plots[[1]])
#  g3 <- ggplotGrob(plot_nest$plots[[2]])
#  g4 <- ggplotGrob(plot_nest$plots[[3]])
#  g5 <- ggplotGrob(plot_nest$plots[[4]])
#  g <- rbind(g2, g3, g4, g5, size = "first")
#  g$widths <- unit.pmax(g2$widths, g3$widths, g4$widths, g5$widths)

  # We will have 4 plots here
  g2 <- ggplotGrob(plot_nest$plots[[1]])
  g3 <- ggplotGrob(plot_nest$plots[[2]])
  g4 <- ggplotGrob(plot_nest$plots[[3]])
  g5 <- ggplotGrob(plot_nest$plots[[4]])
  g <- rbind(g2, g3, g4, g5, size = "first")
  g$widths <- grid::unit.pmax(g2$widths, g3$widths, g4$widths)


  return(g)



}

# Now we map these all together, arranging by temperature
flux_sites_plot <- field_data_joined_test |>
  mutate(plots = pmap(.l=list(x=model_data,y=field_flux,z=site),.f=~make_joined_plot(..1,..2,..3))) |>
  select(site,plots) |>
  inner_join(summary_env_data,by="site") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  arrange(site)

out_big <- gridExtra::grid.arrange(grobs = flux_sites_plot$plots, nrow = 1)


### Extract the legend
model_data_test <- field_data_joined_test$model_data[[1]] |>
  select(startDateTime, flux_compute) |>
  unnest(cols = c(flux_compute))

legend_plot_top <- plot_level(input_data = model_data_test,
                              field_data_in = field_data_joined_test$field_flux[[1]],
                              input_levels = "top",
                              plot_surface =  c("011", "000"),
                              x_axis = FALSE,
                              plot_title = "plot_name"
) + theme(legend.position = "bottom")

legend = gtable::gtable_filter(ggplot_gtable(ggplot_build(legend_plot_top)), "guide-box")
lheight <- sum(legend$height)
lwidth <- sum(legend$width)

### Now arrange them all together
combined <- arrangeGrob(
  do.call(arrangeGrob, out_big),
  legend,
  ncol = 1,
  heights = unit.c(unit(1, "npc") - lheight, lheight)
)

### Now let's get these all plotted!
png('figures/flux-results.png', width = 25, height = 9, units = 'in', res = 300); plot(combined); dev.off()


