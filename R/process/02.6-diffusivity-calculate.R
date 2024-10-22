### Author: JMZ
### Purpose: Use the measured LICOR data and flux gradient data to back-compute diffusivity and bulk density, ultimately comparing the two



# All the env data are contained in the combined data frame

load('data/derived/combined-field-data.Rda')  # field_data_joined

load('data/derived/diffusivity-gradient.Rda')  # diffusivity, co2 gradient, bulk density, and rock volume from neonSoilFlux

# Compute some summary stats.  Organize by the temperature - this is how we make sure each site is ordered in our plots.
summary_env_data <- field_data_joined |>
  select(site,field_env) |>
  unnest(cols=c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = median(VSWC),
    temp_data = median(soilTemp),
    .groups="drop"
  )



combined_data <- field_data_joined |>
  inner_join(flux_gradient_diffusivity,by="site") |>
  rename(neon_flux = model_data.x,
         neon_diffusivity_gradient = model_data.y) |>
  mutate(combined_field = map2(.x=field_flux,.y=field_env,.f=~inner_join(.x,.y,by=c("startDateTime","instrument","horizontalPosition")))) |>
  select(site,sampling_location,combined_field,neon_diffusivity_gradient,rock_volume,curr_tz)

### Now standardize



standardize_timestamps <- function(input_model_data,input_field_data) {
  # lag_time is the number of minutes we subtract from field data to comapre with NEON
  # Create a tibble of intervals for NEON
  start_times <- input_model_data$startDateTime
  end_times <- input_model_data$startDateTime + 30*60 # Add 30 minutes
  intervals <- interval(start_times,end_times)
  dates <- tibble(start_times,intervals)



  # Create a vector of LICOR reported times - we shift it by a the lag to see
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
  joined_data <- input_model_data |>
    inner_join(NEON_field_timestamp,by=c("startDateTime" = "start_times")) |>
    select(startDateTime,co2gradient,diffusivity,field_flux)

  # Prepare a data frame of timestamped NEON and field data for analysis
  ### When the gradient negative, that means fluxes are coming from the soil, so we take the negative according to Fick's Law

  out_data <- joined_data |>
    mutate(field_flux = map(field_flux,~select(.x,-startDateTime)),
           env_summary = map(field_flux,.f=~summarize(.x,across(.cols=staPres:soilTemp,.fns=list(mean=mean,sd=sd)))), # Compute the average env measurements
           gradient = map_dbl(co2gradient,.f=~pull(.x,gradient)), # Get the gradient
           diffusivity_field = map2_dbl(.x=gradient,.y=field_flux,.f=~( -mean(.y$flux / .x,na.rm=TRUE ))),
           diffusivity_fieldExpUncert = map2_dbl(.x=gradient,.y=field_flux,.f=~( sd(.y$flux / .x,na.rm=TRUE )))) |>
    select(-co2gradient,-gradient,-field_flux) |>
    unnest(cols=c("diffusivity","env_summary"))

  return(out_data)



}



# Collect all of the field and computed flux data within the same time interval
field_stats_data <- combined_data |>
  mutate(harmonized_data = map2(.x=neon_diffusivity_gradient,.y=combined_field,.f=~standardize_timestamps(.x,.y))) |>
  mutate(harmonized_data = map2(.x=harmonized_data,
                                .y=curr_tz,
                                .f=~(.x |>
                                       mutate(startDateTime = lubridate::with_tz(startDateTime,tzone = .y))) ) ) |>
  select(site,harmonized_data,rock_volume) |>
  unnest(cols=c("harmonized_data"))

### We need to test out if the flux is within the given half hour of a measurement
# Group_by the startDate Time for the diffusivity_field to get out the uncertainty



p_diffus <- field_stats_data |>
  inner_join(summary_env_data,by="site") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  pivot_longer(cols=c("diffusivity","diffusivity_field")) |>
  #filter(site == "SJER") |>
  ggplot(aes(x=startDateTime,y=value/1e-6,color=name)) +
  geom_point() +
  facet_grid(.~site,scales="free") + ylim(c(0,10)) +
  labs(#y = "Diffusivity (*1e6)",
       y=bquote(~Diffusivity~'('~10^6~m^-2~s^-1*~')'),
       color='Diffusivity Calculation:',
       x='Date') +
  scale_color_discrete(labels = c('diffusivity' = 'neonSoilFlux',
                                 'diffusivity_field' = 'LICOR')) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 10,angle=-90),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  scale_x_datetime(breaks = scales::date_breaks("6 hours"),
                   minor_breaks=scales::date_breaks("3 hours"),
                   date_labels = "%m-%d %H:%M")



ggsave(filename = 'figures/diffusivity-plot.png',plot = p_diffus,width = 12,height=5)


### Bulk density calculation time - this is a series of back calculations from
### diffusivity.R (neonSoilFlux)
### compute_neon_flux.R (neonSoilFlux)

# ### Forward calculations that we work backwards

# porosSub2mm = 1 - bulkDensExclCoarseFrag / 2.65
# porVol2To20 = porosSub2mm * (1 - rockVol)

# # Calculate the air filled porosity - if the soil water is larger, then set it to 0
# porVol2To20AirFilled <- pmax(porVol2To20 - soil_water, 0)
#
# # Calculate gas tortuosity factor based on Millington and Quirk 1961
# tort <- (porVol2To20AirFilled^(10 / 3)) / (porVol2To20^2)
#
# # Calculate CO2 diffusivity in free air (m2 s-1).
# diffuFreeAir <- 0.0000147 * ((temperature - absZero) / (20 - absZero))^1.75 * (pressure / 101.3)
#
# # Calculate CO2 diffusivity (m2 s-1).
# diffusivity <- tort * diffuFreeAir



# Assign values to constants
R <- 0.008314472 # Ideal gas constant = 0.008314472 m3 kPa °K-1 mol-1
absZero <- -273.15 # Absolute zero (-273.15 °C; 0 °K)

field_stats_data2 <- field_stats_data |>
  mutate(diffuFreeAir = 0.0000147 * ((soilTemp_mean - absZero) / (20 - absZero))^1.75 * (staPres_mean / 101.3), # Calculate CO2 diffusivity in free air (m2 s-1).  # Known from LICOR
         tortuosity = diffusivity/diffuFreeAir # Calculate tortuosity diffusivity <- tort * diffuFreeAir
         )

# Since the tortuosity is a nonlinear equation, we need to find roots
### Tortuosity root finding function Millington and Quirk 1961:
tortu <- function(porVol2To20, VSWC) {

  porVol2To20AirFilled <- pmax(porVol2To20 - VSWC, 0)
  porVol2To20AirFilled_power <- pracma::nthroot(porVol2To20AirFilled,3)^10
  tort <- porVol2To20AirFilled_power / (porVol2To20^2)

  return(tort)


}
uniroot(froot, u=0.4185890, VSWC=0.211000, interval= c(0.3, 1))

froot(0.9646048,VSWC=0.211000,0.4185890)
# Define the root finding function
froot <- function(porVol2To20, VSWC,u) tortu(porVol2To20, VSWC) - u

# Use purrr::safely so any error messages get dispersed
froot_safe <- safely(uniroot)

uniroot(froot, u=field_stats_data2$tortuosity[[2]], VSWC=field_stats_data2$VSWC_mean[[2]], interval= c(0, 1))

# There can be multiple solutions here when solving this equation.  GRRR.

field_stats_data2 |> filter(site == "WREF") |> glimpse()
# Let's get ready to map!
field_stats_data3 <- field_stats_data2 |>
  mutate(porVol2To20 = map2(.x=tortuosity,.y=VSWC_mean,.f=~froot_safe(froot, u=.x, VSWC=.y, interval= c(.3, 1))$result) ) |>
  mutate(porVol2To20 = map_dbl(.x=porVol2To20,.f=~(ifelse(is.null(.x),NA,.x$root)) ) ) |>
  mutate(porosSub2mm = porVol2To20,
         bulkDens_field = 2.65*(1-porosSub2mm)) #|>
  #select(site,startDateTime,diffusivity,diffusivity_field,bulkDens_field)

# Now we can  make a plot
field_stats_data3 |> filter(site == "WREF") |> glimpse()
flux_gradient_diffusivity |> glimpse()

field_stats_data3 |>
  ggplot(aes(x=site)) +
  #geom_boxplot(aes(y=porVol2To20)) + ylim(c(0,1)) +
  geom_point(data = flux_gradient_diffusivity,aes(y=bulk_density),color='red',inherit.aes = TRUE,size=3) + ylim(c(0,3))

### We would expect that phi is on the higher end of things:
flux_gradient_diffusivity |>
  mutate(phi_test = 1-bulk_density*(1-rock_volume)/2.65) |>
  select(site,rock_volume,bulk_density,phi_test)
