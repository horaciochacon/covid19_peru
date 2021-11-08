library(tidyverse)
library(janitor)
library(lubridate)
library(tidyquant)
library(AMR)


# Read + Merge mortality datasets -----------------------------------------

sinadef <- read_csv("data/TB_SINADEF.csv", locale = locale("es")) %>% 
  clean_names()

covid_deaths <- read_csv(
  "data/TB_FALLECIDO_HOSP_VAC.csv", locale = locale(encoding = "UTF-8")
  ) %>% 
  clean_names()

covid_deaths <- covid_deaths %>% 
  left_join(sinadef) %>% 
  select(id_persona:flag_vacuna, estado_civil, nivel_de_instruccion) %>% 
  filter(edad >= 0) %>% 
  mutate(
    fecha_fallecimiento = dmy(fecha_fallecimiento),
    nivel_de_instruccion = case_when(
      nivel_de_instruccion == "IGNORADO" ~ "Ignored",
      nivel_de_instruccion == "INICIAL / PRE-ESCOLAR" ~ "No School",
      nivel_de_instruccion == "NINGUN NIVEL / ILETRADO" ~ "No School",
      str_starts(nivel_de_instruccion, "PRIMARIA") ~ "Primary",
      str_starts(nivel_de_instruccion, "SECUNDARIA") ~ "Secondary",
      str_starts(nivel_de_instruccion, "SUPERIOR") ~ "Superior"
    ),
    agrp = age_groups(edad, split_at = c(18, 45, 65))
  )


# Creates death count data frame ------------------------------------------

death_count_day <- covid_deaths %>% 
  group_by(fecha_fallecimiento, sexo, agrp, dpt_cdc) %>% 
  count()

write_excel_csv(death_count_day, "data/death_count_day.csv")

death_count_week <- death_count_day %>%
  mutate(
    deaths_7d = rollmean(n, 7)
  ) %>%
  group_by(
    year_week = floor_date(fecha_fallecimiento, "1 week"), sexo, agrp, dpt_cdc
    ) %>% 
  count()

write_excel_csv(death_count_week, "data/death_count_week.csv")

# Plot results ------------------------------------------------------------

theme_set(theme_bw())

death_count %>% 
  ggplot(aes(x = fecha_fallecimiento, y = n)) +
  geom_ma(n = 90)

death_count %>% 
  select(fecha_fallecimiento, sexo, n) %>% 
  na.omit() %>% 
  ggplot(aes(x = fecha_fallecimiento, y = n, col = sexo)) +
  geom_ma(n = 90)

death_count %>% 
  select(fecha_fallecimiento, nivel_de_instruccion, n) %>% 
  na.omit() %>% 
  ggplot(aes(x = fecha_fallecimiento, y = n, col = nivel_de_instruccion)) +
  geom_ma(n = 30, size = 1)

death_count %>% 
  select(fecha_fallecimiento, agrp, n) %>% 
  na.omit() %>% 
  ggplot(aes(x = fecha_fallecimiento, y = n, col = agrp)) +
  geom_line(n = 90, size = 1) +
  facet_grid(dpt_cdc~ .)

# When did transmission start
# When did it exceed some threshold per department
# Look in maps
# Use connectivity, how similar is the start for different places
# Temporal coherence, who seems to be similar.
# Are there similar patterns in transmission across different departments
# Peak, when did they hit the peak
# Do we see different peaks for different departments
# What are the patterns of transmission in Peru, if not what differences?
# Total deaths by day, by department on the same plot per capita.
# Analyze trends by department, then replicate framework
# Weekly analysis
# Plot adm3
# When did the first peak occur




