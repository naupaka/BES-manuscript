### Derive RMSE and R2 values for the different model results compared to measured values
### Work in progress
# TO DO: Compare lagged from the measurements (+/half hour?)
# Don't add lines
library(tidyverse)
library(lubridate)
library(broom)
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

yoop <- field_stats_data_0$harmonized_data[[1]] |> glimpse()

yee <- yoop |> group_by(method) |> nest()

yee$data[[4]]
### OK, let's get cooking!

field_stats_data_0 <- field_data_joined |>
  mutate(harmonized_data = map2(.x=model_data,.y=field_flux,.f=~standardize_timestamps(.x,.y,lag_time = 0)) ,
         comparison_stats = map(harmonized_data,extract_stats)) |>
  select(site,comparison_stats) |>
  unnest(cols=c(comparison_stats)) |>
  mutate(lag = "0")

# 30 minute lag
field_stats_data_30 <- field_data_joined |>
  mutate(harmonized_data = map2(.x=model_data,.y=field_flux,.f=~standardize_timestamps(.x,.y,lag_time = 30)),
         comparison_stats = map(harmonized_data,extract_stats)) |>
  select(site,comparison_stats) |>
  unnest(cols=c(comparison_stats)) |>
  mutate(lag = "30")

# 60 minute lag
field_stats_data_60 <- field_data_joined |>
  mutate(harmonized_data = map2(.x=model_data,.y=field_flux,.f=~standardize_timestamps(.x,.y,lag_time = 60)),
         comparison_stats = map(harmonized_data,extract_stats)) |>
  select(site,comparison_stats) |>
  unnest(cols=c(comparison_stats)) |>
  mutate(lag = "60")
# Plot!

# Combine everything together.
all_stats <- rbind(field_stats_data_0,field_stats_data_30,field_stats_data_60)

# Compute some summary stats.  Organize by
summary_env_data <- field_data_joined |>
  select(site,field_env) |>
  unnest(cols=c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = median(VSWC),
    temp_data = median(soilTemp),
    .groups="drop"
  )

# Now plot the stats.  Do some relabeling and factor ordering for the plot.
## Add in the F111 and F000 separately, or just plot it at the end.  A second factor plot?


#p_stats <-
  all_stats |>
  filter(method %in% c("000","111","010","100","001")) |>
  inner_join(summary_env_data,by="site") |>
  pivot_longer(cols=c("nrmse","r.squared","slope")) |>
    filter(name == "r.squared") |>
  mutate( #
    name = case_match(name, "nrmse" ~ "Normalized~RMSE", "r.squared" ~ "R^{2}", "slope" ~ "m"),
         method = case_match(method,"000" ~ "F['000']","001" ~"F['001']","010" ~"F['010']","100" ~"F['100']","111"~"F['111']"),
    name = factor(name,levels=c('m',"R^{2}","Normalized~RMSE")),
    method = factor(method,levels=c("F['111']","F['000']","F['100']","F['010']","F['001']")),
        )|>
  ggplot(aes(x=fct_reorder(site, temp_data),y=value,fill=lag)) +
    geom_col(position="dodge") + #geom_line() +
  facet_grid(method~name,labeller = label_parsed) +
  labs(x = "Site", y = "Value", fill = "Lag") +
  theme(axis.text.x=element_text(angle=-90)) +
  # scale_y_continuous(limits=input_limits) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10,angle=-90),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 14)
  )


ggsave(filename = 'figures/stats-plot.png',plot = p_stats,width = 13,height=7)
