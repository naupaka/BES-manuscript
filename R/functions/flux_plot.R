#' Make a plot of measured or computed fluxes at a NEON site
#'
#' @param model_data_in Required. Input modeled flux data to plot.
#' @param field_data_in Required. Input measured LICOR data to plot.
#' @param site_name Required. 4 digit code to refer to NEON site.
#' @param input_limits Required. Range to show on y axis
#' @param licor_only  Optional. Do we want to just plot the LICOR data?
#'
#' @return a ggplot of the output data
#' @export
#'
#' @examples
#' # TBD
#'
flux_plot <- function(model_data_in,field_data_in,site_name,input_limits,licor_only=FALSE) {



  licor_data <- field_data_in |>
    mutate(horizontalPosition = fct_relevel(horizontalPosition,"LICOR") )

  start_date <- licor_data |>
    pull(startDateTime) |>
    min() |> floor_date(unit="day")

  end_date <- licor_data |>
    pull(startDateTime) |>
    max() |> ceiling_date(unit="day")

  if(!licor_only) {
    model_data <- model_data_in |>
      select(startDateTime,flux_compute) |>
      unnest(cols=c(flux_compute))


    model_data |>
      mutate(flux_min = pmax(flux-flux_err,0),
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
      scale_y_continuous(limits=input_limits) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle=-90),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 14)
      ) +
      ggtitle(site_name) +
      ylab(bquote('Flux ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))

  } else {
    licor_data |>
      ggplot(aes(x=startDateTime,y=flux,shape=instrument,color=instrument)) +
      geom_point(size=2) +
      scale_colour_manual(values = c("#7777DD","#CC6666")) +
      theme(axis.text.x=element_text(angle=-90)) +
      scale_x_datetime('Date (UTC)',
                       breaks = scales::date_breaks("6 hours"),
                       minor_breaks=scales::date_breaks("30 mins"),
                       date_labels = "%m-%d %H:%M",
                       limits = c(start_date,end_date)
      ) +
      theme_bw() +
      scale_y_continuous(limits=input_limits) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle=-90),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 14)
      ) +
      ggtitle(site_name) +
      ylab(bquote('Flux ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))  +
      labs(color = "LICOR Instrument:",shape = "LICOR Instrument:")

  }


}
