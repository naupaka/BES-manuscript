#' Make a plot of environmental data (LICOR and NEON) at a site
#'
#' @param env_neon_in Required. Data frame of NEON environmental measurements.
#' @param env_licor_in Required. Data frame of LICOR measurements
#' @param site_name Required. 4 digit code to refer to NEON site.
#' @param measurement  Required. Type of measurement (e.g. VSWC or soilTemp)
#' @param input_limits Required. Limits to show on the y axis
#' @param y_label Optional. Label on y axis. Defaults to NULL
#' @param in_color Optional. Color of LICOR data. Defaults to red.
#' @param licor_only Optional. Do you want to plot LICOR data only?
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' #TBD
#'
env_plot <- function(env_neon_in,env_licor_in,site_name,measurement,input_limits,y_label=NULL,in_color='red',licor_only = FALSE) {
  ### Function that plots the data for a given site

  lookup_licor <- c(value = measurement)
  lookup_neon <- c(meas = str_c(measurement,"Mean"), uncert = str_c(measurement,"ExpUncert"))


  licor_data <- env_licor_in |>
    select(startDateTime,instrument,horizontalPosition,matches(measurement)) |>
    mutate(horizontalPosition = fct_relevel(horizontalPosition,"LICOR") ) |>
    rename(all_of(lookup_licor))

  neon_data <- env_neon_in |>
    select(matches("startDateTime|(?<!StdEr)Mean$|ExpUncert|FinalQF",perl=TRUE)) |>
    rename(all_of(lookup_neon))





  start_date <- licor_data |>
    pull(startDateTime) |>
    min() |> floor_date(unit="day")

  end_date <- licor_data |>
    pull(startDateTime) |>
    max() |> ceiling_date(unit="day")




  if(!licor_only) {

    neon_data |>
      mutate(min_meas = pmax(meas-uncert,input_limits[1]),
             max_meas = pmin(meas+uncert,input_limits[2])) |>
      ggplot(aes(x=startDateTime,y=meas)) +
      geom_line(linewidth = 1.5) +
      geom_ribbon(aes(ymin=min_meas,ymax=max_meas),alpha=0.25) +
      geom_point(data=licor_data,aes(x=startDateTime,y=value,shape=instrument),color=in_color,size=3) +
      theme(axis.text.x=element_text(angle=-90)) +
      scale_x_datetime('Date',
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
      ylab(y_label)


  } else{
    licor_data |>
      ggplot(aes(x=startDateTime,y=value,shape=instrument)) +
      geom_point(color=in_color,size=3) +
      theme(axis.text.x=element_text(angle=-90)) +
      scale_x_datetime('Date',
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
      ylab(y_label)


  }




}
