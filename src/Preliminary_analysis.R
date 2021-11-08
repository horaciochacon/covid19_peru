library(tidyverse)
library(sf)
library(scales)
library(colorspace)
library(cowplot)
library(pracma)
library(mgcv)
library(EMD)
library(tidymv)
source("R/functions.R")
theme_set(theme_bw())


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


# Identifying time of peaks of infection ----------------------------------
# WORK WITH GAM 
# Better peak finder
  get_peaks(death_count_ntl$n)
  
  map(death_count_dpt_list,.f = ~get_peaks(.$n))
  
  first_peak <- map(death_count_dpt_list,.f = ~get_peaks_first(.$n)) %>% 
    map2_dfr(death_count_dpt_list, .f = ~.y$fecha_fallecimiento[.x]) 
  
  first_peak <- tibble(dpt_cdc = names(first_peak), peak = t(first_peak))

# Mapping deaths per department -------------------------------------------

death_count_dpt %>% 
  group_by(dpt_cdc) %>% 
  summarise(n = sum(n) / max(pob)) %>% 
  left_join()
  ggplot() +
  geom_sf(aes(fill = mort, geometry = geometry), size = 0.01, color = "grey40") +
  scale_fill_continuous_sequential(
    palette = "BurgYl",
    name = "Mortalidad"
  )


# Gam models --------------------------------------------------------------

mod_lm <- gam(n ~ s(day, k = 20), data = death_count_ntl)
peak_ntl <- death_count_ntl$day[which(get_peak(mod_lm$fitted.values))]

ggplot(aes(x = day, y = n), data = death_count_ntl) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 20)) +
  geom_vline(xintercept = peak_ntl)

mod_list <- death_count_dpt_list %>% 
  map(
    ~gam(n ~ s(day, k = 20), data = .)
  )

peak_list <- mod_list %>% 
  map(
    ~death_count_ntl$day[which(get_peak(.$fitted.values))]
  )

death_count_dpt %>% 
  bind_rows() %>% 
  ggplot(aes(x = day, y = mort)) +
  geom_point(size = 0.1, alpha = 0.1) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 20)) +
  facet_wrap(. ~ dpt_cdc, ncol = 5)

  


# steps better peak estimators
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
