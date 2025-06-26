#########################################################################
# Where are employees there? An essay on spatial and industrial distribution
# of employees in Japan
# Original: 11th. June 2025 
# Revised:
# by Yuzuru Utsunomiya, Ph. D.
#########################################################################
# 
# ----- read.library -----
library(tidyverse)
library(sf)
library(spdep)
library(stringdist)
library(cmdstanr)
library(khroma)
library(estatapi)
# 
# ----- load.data -----
# API for estatapi
source("appID.r")
# read Economic census data
enterprise_prefecture_2021_2nd_layer <- 
  read_csv("enterprise_prefecture_2021_2nd_layer.csv") %>% 
  dplyr::mutate(across(where(is.character), factor)) %>%
  dplyr::select(-unit) %>%
  tidyr::pivot_wider(names_from = attribute, values_from = value) %>%
  data.table::setnames(c("industry", "size", "capital", "prefecture_kanji", "n_enterprise", "n_employees_male", "n_employees_female")) %>%
  dplyr::mutate(
    size = factor(size, levels = c(
      "0～4人", "5～9人", "10～19人", "20～29人", "30～49人",
      "50～99人", "100～299人", "300～999人", "1000～1999人",
      "2000～4999人", "5000人以上"
    )),
    capital = factor(capital, levels = c(
      "300万円未満", "300～500万円未満", "500～1000万円未満",
      "1000～3000万円未満", "3000～5000万円未満",
      "5000万～1億円未満", "1～3億円未満", "3～10億円未満",
      "10～50億円未満", "50億円以上"
    ))
  )
# shape files
jpn_shape <- sf::read_sf("./JPN_adm/JPN_adm1.shp")
nb <- spdep::poly2nb(jpn_shape)
nb_regions <- attr(nb, "region.id")
# --- Build ICAR edge list
nb <- spdep::poly2nb(jpn_shape)  # already created
node1 <- rep(1:length(nb), sapply(nb, length))
node2 <- unlist(nb)
# Ensure valid edges (remove NAs or zeroes, though poly2nb normally avoids 0s)
valid <- !is.na(node2) & node2 > 0
node1 <- node1[valid]
node2 <- node2[valid]
# obtain prefectures' names list from shape files
shape_names <- 
  sf::read_sf("./JPN_adm/JPN_adm1.shp") %>%
  dplyr::select(ID_1, NAME_1, NL_NAME_1) %>%
  sf::st_drop_geometry() %>% 
  dplyr::transmute(
    prefecture_romaji = NAME_1,
    prefecture_kanji = NL_NAME_1,
    prefecture_id = ID_1
  )  # Match to your data column
# 
# ----- refine.combine.data -----
# merge the Economic census data and shape files
enterprise_prefecture_2021_2nd_layer_mv <- 
  enterprise_prefecture_2021_2nd_layer %>%
  # omit observations with 0 persons or 0 enterprise
  dplyr::filter(
    n_employees_male > 0, n_employees_female > 0, n_enterprise > 0
    ) %>%
  # add scaled variables
  dplyr::mutate(
    log_male = log(n_employees_male),
    log_female = log(n_employees_female),
    log_enterprise = scale(log(n_enterprise))[, 1],
    size = factor(size, ordered = TRUE, levels = levels(size)),
    capital = factor(capital, ordered = TRUE, levels = levels(capital))
  ) %>% 
  # merge Japanese names and English names
  dplyr::left_join(shape_names, by = "prefecture_kanji") %>%
  dplyr::mutate(
    prefecture_std = prefecture_romaji,
    prefecture = prefecture_id
  )
# save
readr::write_rds(
  enterprise_prefecture_2021_2nd_layer_mv, 
  "enterprise_prefecture_2021_2nd_layer_mv.rds"
  )
# 
# ----- kick.stan -----
# make a data list
stan_data <- list(
  N = nrow(enterprise_prefecture_2021_2nd_layer_mv),
  K = 2,  # Two outcomes: male and female employees
  
  y = as.matrix(enterprise_prefecture_2021_2nd_layer_mv[, c("log_male", "log_female")]),
  
  log_enterprise = as.vector(enterprise_prefecture_2021_2nd_layer_mv$log_enterprise),
  
  size = as.array(as.integer(enterprise_prefecture_2021_2nd_layer_mv$size)),
  J_size = nlevels(enterprise_prefecture_2021_2nd_layer_mv$size),
  
  capital = as.array(as.integer(enterprise_prefecture_2021_2nd_layer_mv$capital)),
  J_capital = nlevels(enterprise_prefecture_2021_2nd_layer_mv$capital),
  
  industry = as.array(as.integer(factor(enterprise_prefecture_2021_2nd_layer_mv$industry))),
  J_industry = nlevels(factor(enterprise_prefecture_2021_2nd_layer_mv$industry)),
  
  prefecture = as.array(enterprise_prefecture_2021_2nd_layer_mv$prefecture),
  J_pref = length(unique(enterprise_prefecture_2021_2nd_layer_mv$prefecture)),
  
  N_edges = length(node1),
  node1 = as.array(node1),
  node2 = as.array(node2)
)

# Set file path to Stan file
stan_file <- "economic_census_japan.stan"
# Compile stan code
mod <- cmdstan_model(stan_file)
# Fit the model
fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  seed = 123,
  refresh = 100,
  adapt_delta = 0.99,
  max_treedepth = 15
)
# Save computation results
fit$save_object("economic_census_japan.rds")
fit_summary <- fit$summary()
print(fit_summary)
readr::write_excel_csv(fit_summary, "fit_summary.csv")
# 
# ----- draw.figures -----
# load data
# shape file
shape <- sf::read_sf("./JPN_adm/JPN_adm1.shp")   # Make sure .shp, .dbf, etc., are in the same directory
# read original observation
enterprise_prefecture_2021_2nd_layer_mv <- 
  readr::read_rds(
    "enterprise_prefecture_2021_2nd_layer_mv.rds"
    )
# list of target industry both in English and Japanese
# list from original data
industry_levels <- 
  levels(
    factor(
      enterprise_prefecture_2021_2nd_layer_mv$industry
    )
  )
# refine the list to merge some data sets below
industry_lookup <- 
  dplyr::tibble(
    industry_id = seq_along(industry_levels),
    industry_name = industry_levels
  )
# obtain English names of industries and 
# combine the names with target industries in Japanese
industry_table <- 
  readr::read_csv("industry_table.csv", col_types = cols(code = col_character())) %>%
  mutate(code_trimmed = str_remove(code, "^0+")) %>%
  filter(str_detect(code_trimmed, "^[1-9][0-9]?$")) %>%
  dplyr::mutate(code = as.numeric(code)) %>% 
  dplyr::arrange(code) %>% 
  dplyr::inner_join(industry_lookup, by = c("industry_japanese" = "industry_name")) %>% 
  dplyr::select(code, industry_japanese, industry_english)
# read estimated results
fit_summary <- 
  readr::read_csv("fit_summary.csv")
# 
# Draw figures
# 1. phi
# (spatial random effect by province)
# Load phi data
# magic words to move and enlarge Okinawa prefecture
source("shift_okinawa.r")
# shape files
shape <- sf::read_sf("./JPN_adm/JPN_adm1.shp")
phi <- 
  readr::read_csv("fit_summary.csv") %>% 
  dplyr::filter(
    stringr::str_detect(
      variable, "phi_raw"
      )
    ) %>% 
  dplyr::mutate(
    gender_id = as.integer(str_match(variable, "phi_raw\\[(\\d+),")[,2]),
    prefecture_id = as.integer(str_match(variable, ",(\\d+)\\]")[,2])
  ) %>% 
  dplyr::select(variable, mean, q5, q95, gender_id, prefecture_id)
# merge phi data and shape files
phi_shape <- 
  shape %>%
  dplyr::left_join(
    phi, 
    by = c("ID_1" = "prefecture_id")
    ) %>% 
  dplyr::mutate(
    gender = dplyr::case_when(
      gender_id == "1" ~ "Male",
      gender_id == "2" ~ "Female",
      TRUE ~ "hoge"
    )
  ) %>% 
  dplyr::select(mean, q5, q95, gender, NAME_1)
# Plot spatial effect by province
fig_phi_shape <- 
  phi_shape %>% 
  shift_okinawa() %>% 
  ggplot() +
  geom_sf(aes(fill = mean), color = "white") +
  khroma::scale_fill_BuRd() +
  facet_wrap(~ gender) +
  labs(fill = "Scaled \n spatial impact") +
  annotate("segment",
           x = c(122, 132.5, 138),
           xend = c(132.5, 138, 138),
           y = c(38, 38, 42),
           yend = c(38, 42, 46),
           linewidth = .pt / 15
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(10, 'mm'), #change legend key size
    legend.key.height = unit(2.5, 'mm'), #change legend key height
    legend.key.width = unit(20, 'mm') #change legend key width
  )
# save
ggsave(
  "fig_phi_shape.pdf",
  plot = fig_phi_shape,
  width = 300,
  height = 200,
  units = "mm"
)
# 
# alpha
# (Effect of industry by gender)
alpha_industry <- 
  fit_summary %>%
  dplyr::filter(
    stringr::str_detect(
      variable, 
      # variables starting from (^) alpha_industry[
      "^alpha_industry\\["
      )
    ) %>%
  dplyr::mutate(
    gender_id = as.integer(stringr::str_match(variable, "\\[(\\d+),")[,2]),
    industry_id = as.integer(stringr::str_match(variable, ",(\\d+)\\]")[,2]),
    gender = dplyr::if_else(gender_id == 1, "Male", "Female")
  ) %>%
  # merge names of target industries in Japanese
  dplyr::left_join(industry_lookup, by = "industry_id") %>% 
  # merge names of target industries in English
  dplyr::left_join(industry_table, by = c("industry_name" = "industry_japanese"))
# Plot
fig_alpha_industry <-
  alpha_industry %>% 
  ggplot2::ggplot(
    aes(x = mean, y = forcats::fct_reorder(industry_english, mean), fill = gender)
    ) +
  # horizontal bar chart
  geom_col(position = "dodge") +
  labs(x = "Posterior Mean (log count)", y = "Industry", title = "Industry Intercepts by Gender") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  khroma::scale_fill_okabeito() +
  facet_wrap(~ gender) +
  theme_minimal()
# save
ggsave(
  "fig_alpha_industry.pdf",
  plot = fig_alpha_industry,
  width = 1200, 
  height = 400,
  units = "mm"
)
# beta_size
# (coefficient of size)
# size labels
size_levels <- 
  c(
    "0～4人", "5～9人", "10～19人", "20～29人", "30～49人", "50～99人", 
    "100～299人", "300～999人", "1000～1999人","2000～4999人", "5000人以上"
  )
# merge data
beta_size <- 
  fit_summary %>%
  dplyr::filter(stringr::str_detect(variable, "^beta_size\\[")) %>%
  dplyr::mutate(
    gender_id = as.integer(stringr::str_match(variable, "\\[(\\d+),")[,2]),
    size_id = as.integer(stringr::str_match(variable, ",(\\d+)\\]")[,2]),
    gender = dplyr::if_else(
      gender_id == 1, 
      "Male", "Female"
      )
  ) %>% 
  dplyr::mutate(size = factor(size_levels[size_id], levels = size_levels))
# draw
fig_beta_size <- 
  beta_size %>% 
  ggplot2::ggplot(
    aes(x = size, y = mean, color = gender, group = gender)) +
  geom_line() + 
  geom_point() +
  khroma::scale_color_okabeito() +
  labs(
    x = "Firm Size", 
    y = "Coefficient (beta_alpha)", 
    title = "Effect of Firm Size by Gender"
    ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()
# save
ggsave(
  "fig_beta_size.pdf",
  plot = fig_beta_size,
  width = 250,
  height = 200,
  units = "mm",
  device = cairo_pdf
)
# beta_capital
# set capital levels
capital_levels <- c(
  "300万円未満", "300～500万円未満", "500～1000万円未満",
  "1000～3000万円未満", "3000～5000万円未満", "5000万～1億円未満",
  "1～3億円未満", "3～10億円未満", "10～50億円未満", "50億円以上"
)
beta_capital <- 
  fit_summary %>%
  dplyr::filter(
    stringr::str_detect(
      variable, "^beta_capital\\[")
    ) %>%
  dplyr::mutate(
    gender_id = as.integer(str_match(variable, "\\[(\\d+),")[,2]),
    capital_id = as.integer(str_match(variable, ",(\\d+)\\]")[,2]),
    gender = if_else(gender_id == 1, "Male", "Female")
  ) %>%
  dplyr::mutate(
    capital = factor(
      capital_levels[capital_id], 
      levels = capital_levels)
    )
# draw
fig_beta_capital <- 
  beta_capital %>% 
  ggplot2::ggplot(
    aes(x = capital, y = mean, color = gender, group = gender)) +
  geom_line() + 
  geom_point() +
  khroma::scale_color_okabeito() +
  labs(
    x = "Capital Class", 
    y = "Coefficient (beta_capital)", 
    title = "Effect of Capital Class by Gender"
    ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# save
ggsave(
  "fig_beta_capital.pdf",
  plot = fig_beta_capital,
  width = 200,
  height = 200,
  units = "mm",
  device = cairo_pdf
)
# beta_enterprise
beta_enterprise <- 
  fit_summary %>%
  dplyr::filter(
    stringr::str_detect(
      variable, "^beta_enterprise\\["
      )
    ) %>%
  dplyr::mutate(
    gender_id = as.integer(str_match(variable, "\\[(\\d+)\\]")[,2]),
    gender = if_else(gender_id == 1, "Male", "Female")
  )
# 
fig_beta_enterprise <- 
  beta_enterprise %>% 
  ggplot2::ggplot(
    aes(x = gender, y = mean, fill = gender)) +
  geom_col(width = 0.4) +
  scale_fill_okabeito() +
  labs(y = "Effect per log enterprise", title = "Effect of Number of Enterprises (Standardized)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()
# save
ggsave(
  "fig_beta_enterprise.pdf",
  plot = fig_beta_enterprise,
  width = 200,
  height = 200,
  units = "mm",
  device = cairo_pdf
)
# ----- Japan.in.brief -----
# map

shp_jpn_01 <- sf::read_sf("./gadm41_JPN_shp/gadm41_JPN_0.shp")
shp_chn_01 <- sf::read_sf("./gadm41_CHN_shp/gadm41_CHN_0.shp")
shp_twn_01 <- sf::read_sf("./gadm41_TWN_shp/gadm41_TWN_0.shp")
shp_rus_01 <- sf::read_sf("./gadm41_RUS_shp/gadm41_RUS_0.shp")
shp_kor_01 <- sf::read_sf("./gadm41_KOR_shp/gadm41_KOR_0.shp")
shp_prk_01 <- sf::read_sf("./gadm41_PRK_shp/gadm41_PRK_0.shp")

jpn_surroundings <- 
  shp_jpn_01 %>% 
  dplyr::bind_rows(shp_chn_01) %>% 
  dplyr::bind_rows(shp_twn_01) %>% 
  dplyr::bind_rows(shp_rus_01) %>% 
  dplyr::bind_rows(shp_kor_01) %>% 
  dplyr::bind_rows(shp_prk_01) %>% 
  dplyr::filter(!GID_0 %in% c("Z02","Z03","Z08"))

hoge <- 
  jpn_surroundings %>% 
  ggplot2::ggplot() +
  geom_sf() +
  lims(
    x = c(120,150),
    y = c(20, 50)
  )


# LPR gap by gender


