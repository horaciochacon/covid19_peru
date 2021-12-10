library(tidyverse)
library(sf)
library(scales)
library(colorspace)
library(cowplot)
library(mgcv)
library(lubridate)
library(viridis)
library(tidymv)
library(ggrepel)
source("R/functions.R")
theme_set(theme_bw())

# Loading the datasets ----------------------------------------------------

# Provincial (Adm2) maps
map_prov <- read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "ANTONIO RAYMONDI", "ANTONIO RAIMONDI", prov_cdc)
    ) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "NASCA", "NAZCA", prov_cdc)
  )

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
  mutate(
    fecha_fallecimiento = floor_date(
      fecha_fallecimiento, "weeks", week_start = 1
      )
    ) %>% 
  group_by(fecha_fallecimiento) %>% 
  summarise(n = sum(n)) %>% 
  mutate(
    week = (as.numeric(fecha_fallecimiento) - 18323) / 7
  )

# Department level death counts
death_count_dpt <- death_count_day %>% 
  mutate(
    fecha_fallecimiento = floor_date(
      fecha_fallecimiento, "weeks", week_start = 1
    )
  ) %>% 
  group_by(fecha_fallecimiento, dpt_cdc) %>% 
  summarise(n = sum(n)) %>% 
  left_join(poblacion_dpt,by = "dpt_cdc") %>% 
  mutate(
    mort = n / pob * 10^6,
    week = (as.numeric(fecha_fallecimiento) - 18323) / 7
  )

death_count_dpt_list <- split(death_count_dpt, f = death_count_dpt$dpt_cdc)

# Province level death counts
death_count_prov <- death_count_day %>% 
  mutate(
    fecha_fallecimiento = floor_date(
      fecha_fallecimiento, "weeks", week_start = 1
    )
  ) %>% 
  group_by(fecha_fallecimiento,dpt_cdc, prov_cdc) %>% 
  summarise(n = sum(n)) %>% 
  left_join(poblacion_prov) %>% 
  mutate(
    mort = n / pob * 10^6,
    week = (as.numeric(fecha_fallecimiento) - 18323) / 7
  ) %>%
  na.omit()

death_count_prov_list <- split(death_count_prov, f = death_count_prov$prov_cdc)

# Setting Provincial model ------------------------------------------------

# Creating 196 data frames with province and department level mortality rates
# Using join 1:1
# model_data <- death_count_prov_list %>%
#   map(
#     .%>%
#       select(fecha_fallecimiento, week, death_rate_prov = mort, prov_cdc) %>%
#       left_join(death_count_dpt, by = c("fecha_fallecimiento", "dpt_cdc")) %>%
#       select(
#         prov_cdc,
#         dpt_cdc,
#         fecha_fallecimiento,
#         week = week.x,
#         death_rate_prov,
#         death_rate_dpto = mort
#       ) %>%
#       pivot_longer(death_rate_prov:death_rate_dpto) %>%
#       mutate(weight = ifelse(name == "death_rate_prov", 1, 5)) %>% 
#       select(dpt_cdc, prov_cdc, fecha_fallecimiento, week, mort = value, weight)
#   )

#Appending, more entries from department
model_data <- death_count_prov_list %>%
  map(
    .%>%
      select(fecha_fallecimiento, week, mort, prov_cdc) %>% 
      rbind(
        death_count_dpt[
          death_count_dpt$dpt_cdc == .$dpt_cdc[1],
        ]
      ) %>% 
      select(dpt_cdc, prov_cdc, fecha_fallecimiento, week, mort) %>% 
      arrange(week) %>% 
      ungroup() %>% 
      mutate(
        weight = ifelse(is.na(prov_cdc), 0.1, 1),
      ) %>% 
      fill(prov_cdc, .direction = "updown")
  )

# Setting provincial GAM models
mod_list <- model_data %>% 
  map(
    ~gam(
      mort ~ s(week),
      family = quasipoisson(),
      weights = weight,
      data = .
      )
    )

# for (i in 1:195) {
#   g <- ggplot() +
#     geom_point(
#       aes(x = week, y = mort),
#       data = death_count_prov_list[[i]], 
#       col = "purple"
#     ) +
#     geom_point(
#       aes(x = week, y = mort),
#       data = model_data[[i]][model_data[[i]]$weight == 0.1,], 
#       col = "red"
#     ) +
#     geom_smooth(
#       aes(x = week, y = mort, weight = weight),
#       method = "gam", 
#       formula = y ~ s(x),
#       method.args = list(family = "quasipoisson"),
#       data = model_data[[i]]
#     ) +
#     labs(title = paste(
#       model_data[[i]]$dpt_cdc[1],
#       model_data[[i]]$prov_cdc[1],
#       sep = " - "
#     )
#     )
#   print(g)
  # ggsave(
  #   filename = paste0(
  #     "plots/prov/",
  #     tolower(model_data[[i]]$dpt_cdc[1]),
  #     "_",
  #     tolower(model_data[[i]]$prov_cdc[1]),
  #     ".pdf"),
  #   device = "pdf",
  #   plot = g
  # )
# }

# Peak Detection ----------------------------------------------------------

# Creating a peak list of data frames per province
peak_list <- mod_list %>% 
  map2(
    model_data,
    ~tibble(
      x = .y$week[
        which(
          get_peak_province(.x$fitted.values)
        )
      ],
      index = which(get_peak_province(.x$fitted.values))
    )
  ) %>% 
  map2(
    mod_list,
    ~ .x %>% 
      mutate(
        y = .y$fitted.values[.x$index]
      )
  ) 

# Plotting peaks
for (i in c(1:82, 84:141, 143:195)) {
  g <- ggplot() +
    geom_point(
      aes(x = week, y = mort),
      data = death_count_prov_list[[i]], 
      col = "purple"
    ) +
    geom_point(
      aes(x = week, y = mort),
      data = model_data[[i]][model_data[[i]]$weight == 0.1,], 
      col = "red"
    ) +
    geom_smooth(
      aes(x = week, y = mort, weight = weight),
      method = "gam", 
      formula = y ~ s(x),
      method.args = list(family = "quasipoisson"),
      data = model_data[[i]]
    ) +
    geom_label_repel(
      aes(x = x, y = y, label = "peak"),
      nudge_y = 100,
      data = peak_list[[i]],
      arrow = arrow(length = unit(0.015, "npc"))
    ) +
    labs(
      title = paste(
        model_data[[i]]$dpt_cdc[1],
        model_data[[i]]$prov_cdc[1],
        sep = " - "
        ),
      x = "Weeks",
      y = "Mortality rate"
    )
  # print(g)
  ggsave(
    filename = paste0(
      "plots/prov/",
      tolower(model_data[[i]]$dpt_cdc[1]),
      "_",
      tolower(model_data[[i]]$prov_cdc[1]),
      ".pdf"),
    device = "pdf",
    plot = g
  )
}


# Mapping Peaks -----------------------------------------------------------
# Generate dataset of first peak and number of peaks per province
prov_peaks <- names(peak_list) %>% 
  map2(
    peak_list,
    ~ tibble(
      prov_cdc = .x,
      first_peak = min(.y$x),
      n_peaks = nrow(.y)
    )
  ) %>% 
  bind_rows() %>% 
  mutate(
    first_peak = ifelse(is.infinite(first_peak), NA, first_peak)
    ) %>% 
  mutate(
    first_peak = ifelse(
      is.na(first_peak), max(first_peak,na.rm = T), first_peak
      )
    )

# Mapping first peak per province
map_first_peak <- prov_peaks %>% 
  left_join(map_prov) %>% 
  ggplot() +
  geom_sf(
    aes(fill = first_peak, geometry = geometry), 
    size = 0.005, color = "grey40", alpha = 1
  ) +
  scale_fill_distiller(
    direction = -1
    )

# Saving the file
ggsave(
  "plots/maps/map_first_peak.pdf", 
  plot = map_first_peak, 
  scale = 2,
  dpi = 500,
  device = "pdf"
  )

# Mapping number of peaks per province
map_n_peaks <- prov_peaks %>% 
  left_join(map_prov) %>% 
  ggplot() +
  geom_sf(
    aes(fill = n_peaks, geometry = geometry), 
    size = 0.005, color = "grey40", alpha = 1
  ) +
  scale_fill_distiller(
    direction = 1
  )

# Saving the file
ggsave(
  "plots/maps/map_n_peaks.pdf", 
  plot = map_n_peaks, 
  scale = 2,
  dpi = 500,
  device = "pdf"
)

# Scatterplot -------------------------------------------------------------
# Time between peak 1 and 2
# Correlation deaths first year vs second year

scatter_first_peak_mortality <- death_count_prov %>%
  filter(week <= 57) %>% 
  group_by(prov_cdc) %>% 
  summarise(mort = sum(mort)) %>% 
  left_join(prov_peaks) %>% 
  # filter(first_peak < 40) %>%
  ggplot(aes(x = first_peak, y = mort)) +
  geom_point() +
  geom_smooth(method = "lm")
 
# Saving the file
ggsave(
  "plots/misc/scatter_first_peak_mortality.pdf", 
  plot = scatter_first_peak_mortality, 
  scale = 2,
  dpi = 500,
  device = "pdf"
)


