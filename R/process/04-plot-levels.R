# Plot of the different levels for a given flux value

# Choose 001, 010, 100, add on the top layer

library(tidyverse)
library(lubridate)
library(broom)
### Goals:
# (1) Load up data and measured fluxes for each site
# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window
# (3) Compute R2 and RMSE.  For RMSE we also normalize it by dividing by the mean

# (1) Load up flux data
load('data/derived/combined-field-data.Rda')

# Source the plotting functions for the data
files_sources = list.files('R/functions',full.names = TRUE,pattern="_plot")
sapply(files_sources, source)



model_data_in <- field_data_joined$model_data[[6]]

model_data <- model_data_in |>
  select(startDateTime,flux_compute) |>
  unnest(cols=c(flux_compute))

licor_data <- field_data_joined$field_flux[[6]]

plot_levels <- c("top","100","010","001")
plot_surface <- c("111","000")

yoop <- tibble(plot_levels,
               plots = map(plot_levels,.f=~plot_level(model_data,licor_data,.x,plot_surface)))

library(gridExtra)

grid.arrange(grobs=yoop$plots,ncol=1,nrow=4)


plot_level <- function(input_data,field_data_in,input_levels,plot_surface) {

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
    plot_licor <- FALSE
  }

  if(plot_licor) {

    input_data |>
      filter(method %in% curr_level) |>
      mutate(flux_min = flux-flux_err,
             flux_max = flux+flux_err) |> # Make sure errors aren't negative
      ggplot(aes(x=startDateTime,y=flux)) + geom_line(aes(color=method),linewidth = 1.5) +
      geom_ribbon(aes(ymin=flux_min,ymax=flux_max,fill=method),alpha=0.25) +
      geom_point(data=licor_data,aes(x=startDateTime,y=flux,shape=instrument,color=instrument),size=2) +
      theme(axis.text.x=element_text(angle=-90)) +
      scale_x_datetime('Date (UTC)',
                       breaks = scales::date_breaks("6 hours"),
                       minor_breaks=scales::date_breaks("30 mins"),
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
    input_data |>
      filter(method %in% curr_level) |>
      mutate(flux_min = flux-flux_err,
             flux_max = flux+flux_err) |> # Make sure errors aren't negative
      ggplot(aes(x=startDateTime,y=flux)) + geom_line(aes(color=method),linewidth = 1.5) +
      geom_ribbon(aes(ymin=flux_min,ymax=flux_max,fill=method),alpha=0.25) +
     # geom_point(data=licor_data,aes(x=startDateTime,y=flux,shape=instrument,color=instrument),size=2) +
      theme(axis.text.x=element_text(angle=-90)) +
      scale_x_datetime('Date (UTC)',
                       breaks = scales::date_breaks("6 hours"),
                       minor_breaks=scales::date_breaks("30 mins"),
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



}


plot_level(model_data,licor_data,"100",plot_surface = c("111","000"))
plot_level(model_data,licor_data,"top",plot_surface = c("111","000"))
# Make plots for each of the surface

 #+
  #ggtitle(site_name) +
  #ylab(bquote('Flux ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))

flux_plot(field_data_joined$model_data[[6]],
          field_data_joined$field_flux[[6]],
          field_data_joined$site[[6]],
          c(-1,7))



field_data_joined$model_data[[1]] |> glimpse()
