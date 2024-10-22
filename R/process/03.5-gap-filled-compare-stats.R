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
    vswc_data = median(VSWC),
    temp_data = median(soilTemp),
    .groups="drop"
  )


p1 <- field_data_joined |>
  mutate(QF_prop = map(.x=model_data,.f=~summarise(.x,across(contains("MeanQF"),~sum(.x==1)/n())))) |>
  select(site,QF_prop) |>
  unnest(cols=c("QF_prop")) |>
  pivot_longer(cols=-"site") |>
  inner_join(summary_env_data,by="site") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  ggplot(aes(x=site,y=value,fill=name,group=name)) + geom_col(position="dodge") +
  labs(y="Proportion of measurements",
       fill='Measurement:',
       x='Site') +
  scale_fill_discrete(labels = c('soilCO2concentrationMeanQF' = 'Soil CO2',
                                'VSWCMeanQF' = 'Soil water',
                                'soilTempMeanQF' = 'Soil temp',
                                'staPresMeanQF' = 'Atm. Pressure')) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  )



# 4 square with each area filled up with the values (from 0 to 1 )

# Gap filled - determine how many gap filled measuresments
test_data <- field_data_joined |>
  mutate(total_vals = map(.x=model_data, .f=~(.x |>
  mutate(across(contains("MeanQF"),~.x==0)) |>
  rowwise() |>
  mutate(total_sum = sum(c_across((contains("MeanQF")))) ) |>
  count(total_sum) |> ungroup() |>
    right_join(tibble(total_sum=0:4,n_out=0),by="total_sum") |>
    arrange(total_sum) |>
    mutate(n = if_else(!is.na(n),n,0)) |>
    select(-n_out) |>
  mutate(prop = n/sum(n),
         at_least=cumsum(prop)))) )

p2 <- test_data |>
  select(site,total_vals) |>
  unnest(cols=c(total_vals)) |>
  inner_join(summary_env_data,by="site") |>
  mutate(site = fct_reorder(site, temp_data)) |>
  ggplot(aes(x=total_sum,y=prop,color=site)) + geom_line(linewidth=1) + geom_point(size=3) +
  labs(y=NULL,
       x ="Number of gap-filled measurements",
       color = "Site:") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  )


# Now put the two plots together, lining them up correctly
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

g <- cbind(g1,g2, size = "first")
#g$widths <- unit.pmax(g1$widths, g2$widths)
#g$heights <- unit.pmax(g1$heights, g2$heights)


png("figures/gap-filled-stats.png",width = 18, height = 7, units = 'in',res = 300); plot(g); dev.off()


