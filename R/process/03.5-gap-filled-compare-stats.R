# Make a plot of gap-filled measurements by site

load('data/derived/combined-field-data.Rda')  # field_data_joined

library(tidyverse)
library(grid)
library(gridExtra)

# Compute some summary stats.  Organize by the temperature - this is how we make sure each site is ordered in our plots.
summary_env_data <- field_data_joined |>
  select(site,field_env) |>
  unnest(cols=c(field_env)) |>
  group_by(site) |>
  summarise(
    vswc_data = mean(VSWC),
    temp_data = mean(soilTemp),
    .groups="drop"
  )

# Determine when we were at the sites
field_times <- field_data_joined |>
  mutate(start_date = map(.x=field_env,.f=~(.x |> pull(startDateTime) |>
                                              min() |>
                                              floor_date(unit="30 minutes"))),
         end_date = map(.x=field_env,.f=~(.x |> pull(startDateTime) |>
                                            max() |>
                                            ceiling_date(unit="30 minutes")))) |>
  select(site,sampling_location,start_date,end_date) |>
  unnest(cols=c("start_date","end_date")) |>
  ungroup()

# Pull in the env files
env_files <- list.files(path='data/raw/flux-data',pattern='env-meas-',full.names=TRUE) |>
  str_subset(pattern="05|06")

env_values <- vector(mode = "list", length = length(env_files))



### Do the same for the env values
### More graveyard for env data
for(i in seq_along(env_values)) {

  load(env_files[[i]])
  site_name <- str_extract(env_files[[i]],
                           pattern="(?<=env-meas-)[:alpha:]{4}")

  env_values[[i]] <- site_data |> mutate(site=site_name)


}

env_values <- bind_rows(env_values)



# Next filter the env data when we were at the sites
gap_fill_numbers <- env_values |>
  inner_join(field_times,by="site") |>
  mutate(new_data = pmap(.l=list(data,sampling_location,start_date,end_date,measurement),
                         .f=function(x1,x2,x3,x4,name) {

                           if(name != "staPres") {
                             out <- x1 |>
                               filter(horizontalPosition == x2,
                                      between(startDateTime,x3,x4)
                               )
                           } else {
                             out <- x1 |>
                               filter(between(startDateTime,x3,x4))
                           }

                           return(out)

                            }


                         )
  )|>
  select(site,measurement,new_data) |>
  group_by(site,measurement) |>
  nest() |>
  mutate(data = map(.x=data,.f=~unnest(.x,cols=c("new_data"))),
        rows = map_int(data,nrow)) |>
  filter(rows > 0)



# Now we can count the proportion of each measurement at the sites
compute_bad_vals <- function(input_site_data) {
  input_site_data |>
    group_by(horizontalPosition,startDateTime) |>
    nest() |>
    mutate(check = map(.x=data,.f=~(.x |> select(ends_with("FinalQF")) |>
                                      rename(vals = 1) |>
                                      summarize(bad_vals = sum(vals != 0),
                                                tot_vals = n(),
                                                prop = bad_vals/tot_vals)) ) ) |>
    unnest(cols=c("check")) |> ungroup() |>
    select(startDateTime,bad_vals,tot_vals,prop)

}










# Next: what is the distribution of the 4 measurements that are available at each site?

 prop_bad <- gap_fill_numbers |> mutate(prop = map(data,compute_bad_vals)) |>
   select(site,measurement,prop) |>
   unnest(cols=c(prop)) |>
   ungroup()


 meas_dist <- prop_bad |>
  group_by(site, startDateTime) |>
  nest() |>
  mutate(tot = map_int(.x = data, .f = ~ (sum(.x$prop >= 0.3)))) |>
  select(site, tot) |>
  ungroup() |>
  group_by(site) |>
  count(tot) |>
  ungroup() |>
  right_join(expand_grid(site = unique(prop_bad$site), tot = 0:4), by = c("site", "tot"), ) |>
  mutate(n = if_else(is.na(n), 0, n))


 # Make the plots

 p1 <- prop_bad |>
   group_by(site,measurement) |>
   summarize(tot_bad = sum(prop>=0.3)/n()) |>
   ungroup() |>
   inner_join(summary_env_data,by="site") |>
   mutate(site = fct_reorder(site, temp_data)) |>
   ggplot(aes(x=site,y=tot_bad,fill=measurement,group=measurement)) + geom_col(position="dodge") +
   labs(y="Proportion of gap-filled measurements",
        fill='Measurement:',
        x='Site') +
   theme_bw() +
   theme(
     legend.position = "bottom",
     legend.text = element_text(size = 12),
     axis.title.x = element_text(size = 14),
     axis.text = element_text(size = 12),
     axis.title.y = element_text(size = 14),
     strip.text = element_text(size = 12)
   ) + annotate("text", x = 0.8, y = 0.9, label = "a)",size=8) +
   scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Set2"),
                     labels = c('soilCO2concentration' = bquote(~Soil~CO['2']~'( '*mu*mol~m^-3*~')'),
                                'VSWC' = bquote('SWC ('~m^3~m^-3*')'),
                                'soilTemp' = bquote(~T[S]~'('^o*C*')'),
                                'staPres' = 'P (kPa)'))

p2 <- meas_dist |>
   ungroup() |>
   inner_join(summary_env_data,by="site") |>
   arrange(desc(tot)) |>
   mutate(site = fct_reorder(site, temp_data)) |>
   ggplot(aes(x=site,y=n,fill=as.factor(tot))) +
   geom_bar(position =position_fill(reverse = TRUE),stat = "identity") +
   labs(y="Proportion",
        fill='Number of gap-filled measurements:',
        x='Site') +
   theme_bw() +
   theme(
     legend.position = "bottom",
     legend.text = element_text(size = 12),
     axis.title.x = element_text(size = 14),
     axis.text = element_text(size = 12),
     axis.title.y = element_text(size = 14),
     strip.text = element_text(size = 12)
   ) +
   scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Set2")) +
   annotate("text", x = 0.8, y = 0.9, label = "b)",size=8)




# Now put the two plots together, lining them up correctly
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

g <- cbind(g1,g2, size = "first")
#g$widths <- unit.pmax(g1$widths, g2$widths)
#g$heights <- unit.pmax(g1$heights, g2$heights)


png("figures/gap-filled-stats.png",width = 18, height = 7, units = 'in',res = 300); plot(g); dev.off()


