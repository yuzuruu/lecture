##################################################################
# Introduction to R and Rstudio
# 11th. May 2023
# Yuzuru Utsunomiya, Ph. D.
# 
# ----- read.libraries -----
library(tidyverse)
library(viridis)
library(khroma)
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
  scale_color_viridis(
    discrete = TRUE,
    direction = -1
  ) +
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
