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

# Provincial (Adm2) maps
map_prov <- read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA)

# Death count per day dataset
death_count_day <- read_csv("data/death_count_prov_day.csv") 

# Province level population
poblacion_prov <- read_csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(dpt_cdc = DEPARTAMENTO,prov_cdc = PROVINCIA) %>% 
  summarize(pob = sum(POBLACION))

# Department level population
poblacion_dpt <- read_csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(dpt_cdc = DEPARTAMENTO) %>% 
  summarize(pob = sum(POBLACION))

# National Death count
death_count_ntl <- death_count_day %>% 
  group_by(fecha_fallecimiento) %>% 
  summarise(n = sum(n)) %>% 
  mutate(
    day = as.numeric(fecha_fallecimiento) - 18324
  )

# Department level death counts
death_count_dpt <- death_count_day %>% 
  group_by(fecha_fallecimiento, dpt_cdc) %>% 
  summarise(n = sum(n)) %>% 
  left_join(poblacion_dpt,by = "dpt_cdc") %>% 
  mutate(
    mort = n / pob * 10^6,
    day = as.numeric(fecha_fallecimiento) - 18324
  )

death_count_dpt_list <- split(death_count_dpt, f = death_count_dpt$dpt_cdc)

# Province level death counts
death_count_prov <- death_count_day %>% 
  group_by(fecha_fallecimiento,dpt_cdc, prov_cdc) %>% 
  summarise(n = sum(n)) %>% 
  left_join(poblacion_prov) %>% 
  mutate(
    mort = n / pob * 10^6,
    day = as.numeric(fecha_fallecimiento) - 18324
  ) %>% 
  na.omit()

death_count_prov_list <- split(death_count_prov, f = death_count_prov$prov_cdc)

# Setting Provincial model ------------------------------------------------

# Creating 196 data frames with province and department level mortality rates
model_data <- death_count_prov_list %>%
  map(
    .%>%
      select(fecha_fallecimiento, day, death_rate_prov = mort) %>%
      left_join(death_count_dpt, by = c("fecha_fallecimiento", "dpt_cdc")) %>%
      select(
        dpt_cdc,
        fecha_fallecimiento,
        day = day.x,
        death_rate_prov,
        death_rate_dpto = mort
      ) %>%
      pivot_longer(death_rate_prov:death_rate_dpto) %>%
      mutate(weight = ifelse(name == "death_rate_prov", 1, 0.6))
  )


  
