library(tidyverse)
library(lubridate)
library(sf)
library(ggplot2)
library(cartogram)
library(leaflet)
library(readxl)
library(plotly)
library(viridis)

vic_cases <- read_csv("data/NCOV_COVID_Cases_by_LGA_20211016.csv")
vic_cases_source <- read_csv("data/NCOV_COVID_Cases_by_LGA_Source_20211015.csv")

# Filter for dates

vic_cases <- vic_cases %>%
  filter(diagnosis_date >= "2020-05-01" & diagnosis_date <= "2020-09-30")


vic_cases_aggregate <- vic_cases %>%
  mutate(Localgovernmentarea = sapply(strsplit(vic_cases$Localgovernmentarea,
                                               split=' ',
                                               fixed=TRUE),
                                      function(x) (x[1]))) %>%
  group_by(Localgovernmentarea) %>%
  tally() %>%
  mutate(NAME = toupper(Localgovernmentarea))

vic_map <- st_read(dsn = "data/Order_T8YRCY/ll_gda2020/esrishape/whole_of_dataset/victoria/VMADMIN/AD_LGA_AREA_POLYGON.shp")

vic_map <- vic_map %>%
  left_join(vic_cases_aggregate) %>%
  filter(NAME !=  "UNINCORPORATED")

pop <- read_excel(path = "data/32180ds0002_2017-18.xls",
                  sheet = "Table 2",
                  skip = 7)

pop <- pop %>%
  select(`Local Government Area`, no....4) %>%
  na.omit()

pop <- pop %>%
  mutate(NAME = sapply(strsplit(pop$`Local Government Area`,
                                split=' ',
                                fixed=TRUE),
                       function(x) (x[1])),
         pop = no....4) %>%
  select(NAME, pop) %>%
  mutate(NAME = toupper(NAME))

vic_map <- vic_map %>%
  rename("Number of cases" = "n") %>%
  left_join(pop)

vic_3857 <- st_transform(vic_map, 3857)


vic_3857_data_cartog_cont <-
  cartogram_cont(vic_3857,
                 weight = "pop")

carto <- ggplot(vic_3857_data_cartog_cont) +
  scale_fill_viridis() +
  geom_sf(aes(fill = `Number of cases`)) +
  geom_sf_text(aes(label = NAME), size = 1.5, color = "black") +
  theme_void()


ggsave('carto.png', carto, dpi = 600)
