library(tidyverse)
library(sf)
library(scales)
library(colorspace)
library(cowplot)
library(mgcv)
library(tidymv)
library(ggrepel)
source("R/functions.R")
theme_set(theme_bw())


# Loading the datasets ----------------------------------------------------

map_dpt <- read_sf("data/departamentos/DEPARTAMENTOS.shp") %>% 
  rename(dpt_cdc = DEPARTAMEN)

death_count_day <- read_csv("data/death_count_day.csv") 

poblacion_dpt <- read_csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(dpt_cdc = DEPARTAMENTO) %>% 
  summarize(pob = sum(POBLACION))

death_count_ntl <- death_count_day %>% 
  group_by(fecha_fallecimiento) %>% 
  summarise(n = sum(n)) %>% 
  mutate(
    day = as.numeric(fecha_fallecimiento) - 18324
  )

death_count_dpt <- death_count_day %>% 
  group_by(fecha_fallecimiento, dpt_cdc) %>% 
  summarise(n = sum(n)) %>% 
  left_join(poblacion_dpt) %>% 
  mutate(
    mort = n / pob * 10^6,
    day = as.numeric(fecha_fallecimiento) - 18324
  )

death_count_dpt_list <- split(death_count_dpt, f = death_count_dpt$dpt_cdc)


# General national COVID-19 deaths ----------------------------------------

death_count_ntl %>% 
  ggplot(aes(x = fecha_fallecimiento, y = n)) +
  geom_line() + 
  ylab("Total deaths") + xlab("Date")
  
# Plotting different departments ------------------------------------------

death_count_dpt %>% 
  ggplot(aes(x = fecha_fallecimiento, y = n)) +
  geom_line() +
  facet_wrap(. ~ dpt_cdc) +
  ylab("Total deaths") + xlab("Date")

# Plotting different departments per 1 million inhabitants-----------------

death_count_dpt %>% 
  ggplot(aes(x = fecha_fallecimiento, y = mort)) +
  geom_line() +
  facet_wrap(. ~ dpt_cdc) +
  ylab("Deaths per 1 million") + xlab("Date")

# GAM models --------------------------------------------------------------

# Setting national GAM model
mod_lm <- gam(n ~ s(day, k = 20), data = death_count_ntl)
peak_ntl <- death_count_ntl$day[which(get_peak(mod_lm$fitted.values))]
labels <- tibble(
  x = peak_ntl,
  y = mod_lm$fitted.values[peak_ntl],
  label = as.character(death_count_ntl$fecha_fallecimiento[peak_ntl])
)

# Plotting national GAM model
ggplot(aes(x = day, y = n), data = death_count_ntl) +
  geom_covid_gam(
    k = 20,
    label_df = labels,
    title = "National COVID-19 related deaths",
    nudge_y = 150,
    x = "Days",
    y = "Number of deaths per day"
    )

# Setting departmental GAM models
mod_list <- death_count_dpt_list %>% 
  map(~gam(mort ~ s(day, k = 40), data = .))

# Creating a peak list of data frames per department 
peak_list <- mod_list %>% 
  map2(
    death_count_dpt_list,
    ~tibble(
      x = .y$day[which(get_peak(.x$fitted.values))],
      index = which(get_peak(.x$fitted.values))
      )
    ) %>% 
  map2(
    mod_list,
    ~ .x %>% 
      mutate(
        y = .y$fitted.values[.x$index]
      )
  ) %>%
  map2(
    death_count_dpt_list,
    ~ .x %>% 
      mutate(
        label = as.character(.y$fecha_fallecimiento[.$y])
      )
  )

for (i in 1:25) {
  g <- ggplot(aes(x = day, y = mort), data = death_count_dpt_list[[i]]) +
    geom_covid_gam(
      k = 40,
      x = "Days",
      y = "Number of deaths per day",
      title = death_count_dpt_list[[i]][1,2],
      label_df = peak_list[[i]],
      nudge_y = 10
    )
  ggsave(
    paste0("plots/Plot_",i,"_", death_count_dpt_list[[i]][1,2],".png"), 
    scale = 1.5,
    g)
}

# Mapping deaths per department -------------------------------------------

# Create first and number of peaks data frame from peak list
dpt_peaks <- names(peak_list) %>% 
  map2(
    peak_list,
    ~ tibble(
      dpt_cdc = .x,
      first_peak = min(.y$x),
      n_peaks = nrow(.y)
    )
  ) %>% 
  bind_rows()

dpt_peaks %>% 
  left_join(map_dpt) %>% 
  ggplot() +
  geom_sf(
    aes(fill = first_peak, geometry = geometry), 
    size = 0.05, color = "grey40"
    ) +
  scale_fill_continuous_sequential(
    palette = "BurgYl",
    name = "Time to peak"
  )

dpt_peaks %>% 
  left_join(map_dpt) %>% 
  ggplot() +
  geom_sf(
    aes(fill = n_peaks, geometry = geometry), 
    size = 0.05, color = "grey40"
  ) +
  scale_fill_continuous_sequential(
    palette = "BurgYl",
    name = "Number of peaks"
  )


# steps better peak estimators OK
# Number of peaks
# Initial ramp up (ways, common trick)
#  if population is well mixed identify firs#
  
  
  # Improve Peak Finder
  # Plot on a map the timing of peak
  # Identify categories of trayectories (either gam or eyeball)
  # Are there differences in peaks? general question
  # Doing the window of first infection, plot growth
  #
  #
  #
