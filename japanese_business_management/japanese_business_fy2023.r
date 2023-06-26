##################################################################
# Introduction to R and Rstudio for "Japanese Business"
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
##################################################################
# 
# ----- read.library -----
# LOAD the libraries whenever you start R.
library(tidyverse)
library(viridis)
library(khroma)
library(GGally)
# library(estatapi)
# library(sf)
# library(ggrepel)

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

# # ----- poll.map -----
# map <- 
#   sf::read_sf(
#     "../JPN_adm/JPN_adm1.shp",
#     crs = "+proj=longlat +datum=WGS84"
#     )
# poll <- readxl::read_excel("../poll.xlsx")
# poll_map <- 
#   poll %>% 
#   dplyr::left_join(map, by = c("prefecture" = "NAME_1")
#   )
# 
# poll_rate <- 
#   poll_map %>% 
#   ggplot(aes(fill = female)) + 
#   geom_sf(aes(geometry = geometry)) + 
#   labs(fill = "Rate (%)") +
#   theme_void() +
#   theme(
#     legend.position = "bottom"
#   )

# ----- scatter.plot -----
# 
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

# when you have no idea all about R, please refer to the following website.
# https://r-graphics.org/
# read data
# We have upload the MSExcel file. 
# Before use, download the data from MANABA and read it.
ssdse <- 
  readxl::read_excel(
    "ssdse.xlsx",
    sheet = "ssdse"
  ) 
# draw a scatter plot
scatter_ssdse <- 
  # pass the data to ggplot
  ssdse %>% 
  ggplot2::ggplot(
    aes(
      x = log(total_population),
      y = log(n_of_enterprise)
    )
  ) +
  geom_point() + 
  labs(
    x = "Total population (Unit: persons, log Trans.)",
    y = "N. of enterprise (log Trans.)"
  ) +
  # fix range of axes for convenience
  # adjust the range in accordance with data
  xlim(13,17) +
  ylim(10,14) +
  # add labels by prefecture
  geom_text_repel(aes(label = prefecture), size = 3) +
  theme_classic()
# 
# save the line plot
ggsave(
  "scatter_ssdse.pdf",
  plot = scatter_ssdse,
  width = 150,
  height = 150,
  units = "mm"
)


scatter_assignment <- 
  # pass the data to ggplot
  ssdse %>% 
  ggplot2::ggplot(
    aes(
      x = log(income),
      y = log(tfr)
    )
  ) +
  geom_point(color = "blue") + 
  labs(
    x = "Income (Unit: JPY, log Trans.)",
    y = "N. of Birth (Unit: persons, log Trans.)",
    caption = "INSERT YOUR NAME HERE."
  ) +
  # fix range of axes for convenience
  # adjust the range in accordance with data
  # xlim(13,17) +
  # ylim(10,14) +
  # add labels by prefecture
  geom_text_repel(aes(label = prefecture), size = 3) +
  theme_classic()

ggsave(
  "scatter_assignment.pdf",
  plot = scatter_assignment,
  width = 150,
  height = 150,
  units = "mm"
)
# 
# ----- doing.business -----
# read data
doing_business <- 
  readxl::read_excel(
    "doing_business_2015_2020.xlsx",
    sheet = "selected"
  ) %>% 
  # transform character data to factor
  dplyr::mutate(
    dplyr::across(
      where(is.character), as.factor
    )
  )
# display columns' name for reference
colnames(doing_business)
# draw a scatter plot
# https://stackoverflow.com/questions/27668266/dplyr-change-many-data-types
doing_business_scatter <- 
  doing_business %>% 
  # add a variable to emphasize Japan
  dplyr::mutate(
    japanornot = ifelse(economy == "Japan", "Japan", "Others")) %>% 
  # draw a figure
  ggplot2::ggplot(
    aes(
      # set variables
      x = Enforcing_contracts, 
      y = Protecting_minority_investors
      )
  ) +
  geom_point(
    aes(
      color = japanornot == "Japan", 
      size = japanornot == "Japan",
      shape = factor(year),
      alpha = 0.2
      )) +
  scale_color_manual(values = c("grey","blue")) +
  scale_size_manual(values = c(1,3)) +
  # set limits of axes
  # The indicators evidently distribute between 0 and 100.
  xlim(0,100) +
  ylim(0,100) + 
  labs(
    title = "INSERT APPROPRIATE TITLE", 
    subtitle = "Shapes of points indicate year (2016-2020). \n Source: Doing business by World Bank", 
    caption = "Your name"
    ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )
# save the figure
ggsave(
  "doing_business_scatter.pdf",
  plot = doing_business_scatter,
  width = 150,
  height = 150,
  units = "mm"
)  
# 
# ----- doing.business.and.gdp -----
# read data and combine
doing_business_longer <- 
  doing_business %>% 
  tidyr::pivot_longer(
    cols = c(-economy, -region, -income_group, -year),
    names_to = "perspective",
    values_to = "score"
  ) %>% 
  dplyr::left_join(
    readxl::read_excel("gdp.xlsx", sheet = "gdp"),
    by = c("economy", "year")
  ) %>% 
  dplyr::mutate(
    dplyr::across(
      where(is.character), as.factor
    )
  )
# make a summary table
doing_business_longer_summary <- 
  doing_business_longer %>% 
  dplyr::group_by(perspective, year) %>% 
  dplyr::summarise(
    n = n(),
    Min. = min(score),
    Mean = mean(score),
    Median = median(score),
    Max. = max(score),
    SD = sd(score)
  )
# draw scatter plots systematically
doing_business_longer_scatter <- 
  doing_business_longer %>% 
  group_by(perspective) %>% 
  nest() %>% 
  dplyr::mutate(
    scatter_plot = purrr::map(
      data,
      ~
        ggplot2::ggplot(
          data = ., 
          aes(
            x = score,
            y = log(GDP),
            shape = factor(year),
            color = factor(year)
          ) 
        ) +
        geom_point() +
        # geom_smooth(method = "lm") +
        scale_color_okabeito() +
        labs(
          x = perspective, 
          title = perspective
          ) +
        theme_classic() +
        theme(
          legend.position = "none"
        )
    )
  )
