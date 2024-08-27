### Derive RMSE and R2 values for the different model results compared to measured values
### Work in progress
# TO DO: Compare lagged from the measurements (+/half hour?)
library(tidyverse)
library(lubridate)
library(broom)
### Goals:
# (1) Load up data and measured fluxes for each site
# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window
# (3) Compute R2 and RMSE.  For RMSE we also normalize it by dividing by the mean

# (1) Load up flux data
load('data/derived/combined-field-data.Rda')

# (2) Co-locate field obs and neonSoilFlux obs in same half-hourly window

standardize_timestamps <- function(input_model_data,input_field_data) {

  # Create a tibble of intervals for NEON
  start_times <- input_model_data$startDateTime
  end_times <- input_model_data$startDateTime + 30*60 # Add 30 minutes
  intervals <- interval(start_times,end_times)
  dates <- tibble(start_times,intervals)

  # Create a binary variable indicating if any gap-filled environmental data were used to calculate flux
  model_dataset <- input_model_data |>
    mutate(fluxMeanQF = if_any(ends_with("MeanQF"), ~ .x == 1))

  # Create a vector of LICOR reported times
  measurement_times <- input_field_data$startDateTime


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
            slope = map_dbl(.x=coeff,.f=~pull(.x,estimate)[[2]])) |>
    select(method,nrmse,r.squared,slope) |>
    ungroup()


}


### OK, let's get cooking!

field_stats_data <- field_data_joined |>
  mutate(harmonized_data = map2(.x=model_data,.y=field_flux,.f=~standardize_timestamps(.x,.y)),
         comparison_stats = map(harmonized_data,extract_stats)) |>
  select(site,comparison_stats) |>
  unnest(cols=c(comparison_stats))

# Plot!


field_stats_data |> glimpse()

field_stats_data |>
  pivot_longer(cols=c("nrmse","r.squared","slope")) |>
  ggplot(aes(x=site,y=value,color=method,group=method)) +
    geom_point() + geom_line() + facet_grid(name~.,scales="free_y")
