##################################################################
# Introduction to R and Rstudio
# 11th. May 2023
# Yuzuru Utsunomiya, Ph. D.
# 
# NOTE
# When you would like to find information regarding R functions, try
# the following options.
# 1. Google
# 2. add ? and implement
# e.g.) ?dplyr::mutate
# 

# ----- read.library -----
library(tidyverse)
library(viridis)
library(khroma)
library(estatapi)
library(sf)
# 
# ----- line.population -----
# read data and make it tidy
total_population <- 
  readxl::read_excel(
    "total_population_01.xlsx",
    sheet = "Data"
  ) %>% 
  tidyr::pivot_longer(
    cols = c(-year),
    names_to = "gender",
    values_to = "population"
  ) %>% 
  dplyr::mutate(
    year = lubridate::ymd(paste0(year, "/01/01")) %>% year(.),
    gender = factor(gender)
  )
# draw a line plot
line_total_population_mf_01 <- 
  total_population %>% 
  dplyr::filter(gender != "total") %>% 
  ggplot2::ggplot(
    aes(
      x = year,
      y = population,
      color = gender,
      group = gender,
      shape = gender
    )
  ) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year (1950-2100)",
    y = "Population (Unit: K persons)",
    caption = "your name"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
# save the line plot
ggsave(
  "line_total_population_mf_01.pdf",
  plot = line_total_population_mf_01,
  width = 150,
  height = 150,
  units = "mm"
)
# 
# separate the figure by gender
line_total_population_mf_02 <- 
  line_total_population_mf_01 + 
  facet_wrap(~ gender)
# 
# use viridis color palette
line_total_population_mf_02_viridis <- 
  line_total_population_mf_02 +
  scale_color_viridis(
    discrete = TRUE,
    direction = -1
    )
# apply okabeito colorpallete
line_total_population_mf_02_okabeito <- 
  line_total_population_mf_02 +
  scale_color_okabeito()
# 
# ----- gdp.growth -----
# read.data
gdp_growth <- 
  readxl::read_excel(
    "gdp_growth.xlsx",
     sheet = "Data"
  ) %>% 
  na.omit() %>% 
  tidyr::pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "growth_rate"
  ) %>% 
  dplyr::mutate(
    country = factor(country),
    year = lubridate::ymd(paste0(year, "/01/01"))
  ) 
# 
# select target countries
# NOTE 
# When you would like to refer to a list of country,
# run the following code.
# levels(gdp_growth$country)
# 
gdp_growth_selected <- 
  gdp_growth %>% 
  dplyr::filter(
    country %in% c("Japan","Thailand")
    )
# 
# draw a line plot
line_gdp_growth_selected <- 
  gdp_growth_selected %>% 
  ggplot2::ggplot(
    aes(
      x = year,
      y = growth_rate,
      color = country,
      group = country,
      shape = country
    )
  ) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year (1950-2100)",
    y = "GDP growth rate (Unit: %)",
    caption = "your name"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
# 
# 
# save the line plot
ggsave(
  "line_gdp_growth_selected.pdf",
  plot = line_gdp_growth_selected,
  width = 150,
  height = 150,
  units = "mm"
)


gdp_growth_selected <- 
  gdp_growth %>% 
  dplyr::filter(
    country %in% c("Japan","Thailand","Togo", "Brazil", "Norway")
  )
line_gdp_growth_selected_5 <- 
  gdp_growth_selected %>% 
  ggplot2::ggplot(
    aes(
      x = year,
      y = growth_rate,
      color = country,
      group = country,
      shape = country
    )
  ) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year (1950-2100)",
    y = "GDP growth rate (Unit: %)",
    caption = "your name"
  ) +
  scale_color_viridis(discrete = TRUE, option = "plasma") +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
# save the line plot
ggsave(
  "line_gdp_growth_selected_5.pdf",
  plot = line_gdp_growth_selected_5,
  width = 150,
  height = 150,
  units = "mm"
)

# ----- poll.map -----
map <- 
  sf::read_sf(
    "../JPN_adm/JPN_adm1.shp",
    crs = "+proj=longlat +datum=WGS84"
    )
poll <- readxl::read_excel("../poll.xlsx")
poll_map <- 
  poll %>% 
  dplyr::left_join(map, by = c("prefecture" = "NAME_1")
  )

poll_rate <- 
  poll_map %>% 
  ggplot(aes(fill = female)) + 
  geom_sf(aes(geometry = geometry)) + 
  labs(fill = "Rate (%)") +
  theme_void() +
  theme(
    legend.position = "bottom"
  )

# ----- scatter.plot -----

scatter_iris <- 
  iris %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point()

scatter_iris_revised <- 
  scatter_iris +
  scale_color_okabeito() +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
