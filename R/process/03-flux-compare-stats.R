### Derive RMSE and R2 values for the different model results compared to measured values
### Work in progress
# TO DO: Compare lagged from the measurements (+/half hour?)
# Don't add lines
library(tidyverse)
library(lubridate)
library(broom)
library(gtable)
library(gt)
### Goals:
# (1) Load up data and measured fluxes for each site
# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window
# (3) Compute R2 and RMSE.  For RMSE we also normalize it by dividing by the mean

# (1) Load up flux data
load('data/derived/combined-field-data.Rda')

# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window.  The lag determines how far back in time the field data are compared to the computed flux. (lag_time = 30 means we compare field data to the computed flux a half hour previously.)

standardize_timestamps <- function(input_model_data,input_field_data,lag_time = 0) {
# lag_time is the number of minutes we subtract from field data to comapre with NEON
  # Create a tibble of intervals for NEON
  start_times <- input_model_data$startDateTime
  end_times <- input_model_data$startDateTime + 30*60 # Add 30 minutes
  intervals <- interval(start_times,end_times)
  dates <- tibble(start_times,intervals)

  # Create a binary variable indicating if any gap-filled environmental data were used to calculate flux
  model_dataset <- input_model_data |>
    mutate(fluxMeanQF = if_any(ends_with("MeanQF"), ~ .x == 1))

  # Create a vector of LICOR reported times - we shift it by a the lag to see
  measurement_times <- input_field_data$startDateTime - lubridate::minutes(lag_time)


  # Figure out which NEON intervals the LICOR measured, adding it to the field data
  my_intervals <- input_field_data |>
    mutate(ival = intervals[map_int(measurement_times, ~ which(.x %within% intervals))] )


  # Join the intervals to the NEON dates so we have a NEON timestamp for each LICOR
  NEON_field_timestamp <- my_intervals |>
    group_by(ival) |>
    nest() |>
    inner_join(dates,by=c("ival"="intervals")) |>
    rename(field_flux = data)

  # Join these up now
  joined_data <- model_dataset |>
    inner_join(NEON_field_timestamp,by=c("startDateTime" = "start_times")) |>
    select(startDateTime,flux_compute,field_flux,fluxMeanQF)

  # Prepare a data frame of timestamped NEON and field data for analysis

  out_data <- joined_data |>
    mutate(field_flux = map(field_flux,~select(.x,-startDateTime))) |>
    #select(flux_compute,field_flux,fluxMeanQF) |>
    unnest(cols=c("field_flux")) |>
    rename(flux_field = flux) |>
    unnest(cols=c("flux_compute")) |>
    select(-horizontalPosition) |>
    ungroup()


  return(out_data)



  }


# (3) Compute R2 and RMSE and other regression stats.  For RMSE we also normalize it by dividing by the mean of field obs

extract_stats <- function(harmonized_data) {

  harmonized_data |>
    group_by(method) |>
    nest() |>
    mutate( nrmse = map_dbl(.x=data, .f = ~sqrt(sum((.x$flux-.x$flux_field)^2)/nrow(.x))/mean(.x$flux_field) ),  # computed nrmse - we divide by mean of field obs for standardization
            lm = map(.x=data,.f=~lm(flux~flux_field,data=.x)),
            coeff = map(lm,broom::tidy),
            vals = map(lm,broom::glance),
            r.squared = map_dbl(.x=vals,.f=~pull(.x,r.squared)),
            slope = map_dbl(.x=coeff,.f=~pull(.x,estimate)[[2]]),
            p.value = map_dbl(.x=coeff,.f=~pull(.x,p.value)[[2]]),) |>
    select(method,nrmse,r.squared,slope,p.value) |>
    ungroup()


}


### OK, let's get cooking!

field_stats_data_0 <- field_data_joined |>
  pivot_longer(cols=c("model_data_mq","model_data_marshall")) |>
  mutate(harmonized_data = map2(.x=value,.y=field_flux,.f=~standardize_timestamps(.x,.y,lag_time = 0)) ,
         comparison_stats = map(harmonized_data,extract_stats)) |>
  select(site,name,comparison_stats) |>
  unnest(cols=c(comparison_stats)) |>
  mutate(lag = "0")

# Plot
# Combine everything together.
all_stats <- field_stats_data_0

# Compute some summary stats.  Organize by
summary_env_data <- field_data_joined |>
  select(site,field_env) |>
  unnest(cols=c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = mean(VSWC),
    temp_data = mean(soilTemp),
    .groups="drop"
  )

# Now plot the stats.  Do some relabeling and factor ordering for the plot.
## Add in the F111 and F000 separately, or just plot it at the end.  A second factor plot?


gt_tbl <- all_stats |>
  mutate(across(.cols=c("nrmse","r.squared","slope","p.value"),.fns=~round(.x,2))) |>
  mutate(sig = if_else((p.value < 0.01),"**",""),
         sig = if_else(between(p.value,0.01,0.05),"*",sig)
  ) |>
  select(-lag,-p.value,-sig,-slope) |>
  group_by(method,name) |>
  nest() |>
  pivot_wider(values_from = "data") |>
  unnest(cols=c("model_data_mq","model_data_marshall"),names_repair ="unique") |>
  select(-site...2) |>
  group_by(site...5) |>
  mutate(method = factor(method,
                       levels = c("000",
                                  "101",
                                  "110",
                                  "011"
                       ),
                       labels = c(html("<em>F<sub>000</sub></em>"),
                                  html("<em>F<sub>101</sub></em>"),
                                  html("<em>F<sub>110</sub></em>"),
                                  html("<em>F<sub>011</sub></em>")  ) ) ) |>
  gt(rowname_col = "method")  |>
  fmt(
    columns = method,
    rows = everything(),
    fns = function(x) {
      map(.x=x,.f=~html(paste0(.x)) )
    }
  ) |>
  tab_spanner(
    label = "Millington-Quirk",
    columns = c(nrmse...3, r.squared...4)
  ) |>
  tab_spanner(
    label = "Marshall",
    columns = c(nrmse...6, r.squared...7)
  ) |>
  cols_label(
    nrmse...6 = "NRMSE",
    r.squared...7 = "R2",
    nrmse...3 = "NRMSE",
    r.squared...4 = "R2"
  )

# Show the gt Table
gt_tbl

gtsave(gt_tbl,filename='figures/r2-plot.png',vwidth = 3000)

