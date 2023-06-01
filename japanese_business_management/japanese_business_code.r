# Introduction to R and Rstudio
# 20th. March 2018
# Yuzuru Utsunomiya, Ph. D.


# To refer to data code, visit the following website.
# https://data.worldbank.org/indicator/


# ---- load.libraries ----
library(classInt)
library(ggrepel)
library(gghighlight)
library(khroma)
library(patchwork)
library(rgdal)
library(tidyverse)
library(ggmosaic)
library(sf)
library(maptools)
library(maps)
library(RColorBrewer)
library(sf)
library(viridis)
library(viridisLite)
library(wbstats)
library(WDI)

country.list <- c("Brunei Darussalam", "Cambodia", "China", "Indonesia", "Japan", "Korea, Rep.", "Lao PDR"   , "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam", "United States")

# ---- Japan.map ----
japanmap.data.01 <- readr::read_rds("./map/gadm36_JPN_1_sf.rds")
japanmap.data.02 <- readr::read_rds("./map/gadm36_JPN_2_sf.rds")

prefecture <- 
  cbind(japanmap.data.01, 
        st_coordinates(
          st_centroid(
            japanmap.data.01
            )
          )
        )
prefecture$GID_1 <- c(1:47)

# Map of Japan by prefecture
japanmap.01 <- ggplot() +
  geom_sf(data = japanmap.data.02, fill = "transparent", colour = "grey60") +
  geom_sf(data = japanmap.data.01, fill = "transparent", colour = "steelblue", size = 1) +
  theme_minimal() +
  geom_text_repel(
    data = prefecture,
    aes(X, Y, label = NAME_1),
    colour = "black",
    segment.size = 0.8,
    size = 4
  )

# Pacific coastal belt
pacific.belt <- 
  japanmap.01 + 
  ylim(33.5,36.5) + 
  xlim(128,143)

# ---- land.and.water----

world.land <- 
  WDI(indicator = "ag.lnd.totl.k2") %>% 
  as_tibble()
country.list <- c("Brunei Darussalam", "Cambodia", "China", "Indonesia", "Japan", "Korea, Rep.", "Lao PDR"   , "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam", "United States")

## Land area
world.land.sub <-
  world.land %>%
  dplyr::filter(country %in% country.list) %>%
  dplyr::filter(year == 2011)

world.land.sub.bar <-
  world.land.sub %>%
  ggplot(aes(x = iso2c, y = ag.lnd.totl.k2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Country", y = "Total population (Unit: 1,000 persons)")

# water surface area
world.surface <- WDI(indicator = "ag.lnd.totl.k2") %>% as_tibble()
world.surface.sub <-
  world.surface %>%
  dplyr::filter(country %in% country.list) %>%
  dplyr::filter(year == 2011)

world.surface.sub.bar <-
  world.surface.sub %>%
  ggplot(aes(x = iso2c, y = ag.lnd.totl.k2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Country", y = "Total population (Unit: 1,000 persons)")


## safe water
world.water <- WDI(indicator = "SH.H2O.SMDW.ZS") %>% as_tibble()
world.water.sub <-
  world.water %>%
  dplyr::filter(country %in% country.list) %>%
  dplyr::filter(year == 2011)

world.water.sub.bar <-
  world.water.sub %>%
  ggplot(aes(x = iso2c, y = SH.H2O.SMDW.ZS)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Country", y = "Access to safe water (Unit: %)")

#
# ----- END

# plot(dnorm(seq(-3,3, 20), mean = 0, sd = 1))
# country.list <- c("Brunei Darussalam", "Cambodia", "China", "Indonesia", "Japan", "Korea, Rep.", "Lao PDR"   , "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam", "United States")



# ---- macroeconomy ----
## GDP (USD)
world.gdp <- WDI(indicator = "NY.GDP.MKTP.CD", start = 1960, end = 2017) %>% as_tibble()
world.gdp.sub <-
  world.gdp %>%
  dplyr::filter(country == "Japan")
world.gdp.sub.line <-
  world.gdp.sub %>%
  ggplot(aes(x = year, y = NY.GDP.MKTP.CD)) +
  geom_line(size = 1) +
  geom_point(shape = 21) +
  theme_minimal() +
  labs(x = "year", y = "GDP (Unit: USD)")
## GDP + socioeconomic events
japan.gdp.history <-
  world.gdp.sub.line +
  geom_vline(xintercept = 1964) +
  geom_vline(xintercept = 1973) +
  geom_vline(xintercept = 1985) +
  geom_vline(xintercept = 1991) +
  geom_vline(xintercept = 1997) +
  geom_vline(xintercept = 2008) +
  geom_vline(xintercept = 2011) +
  annotate("text", x = 1963, y = 4e+12, angle = 90, size = 5, label = "Tokyo Olympics (1964)") + # Tokyo olympic in 1964
  annotate("text", x = 1972, y = 4e+12, angle = 90, size = 5, label = "Oil crisis (1973)") + # oil crisis in 1973
  annotate("text", x = 1984, y = 4e+12, angle = 90, size = 5, label = "The plaza accord (1985)") + # The plaza accord in 1985
  annotate("text", x = 1990, y = 2e+12, angle = 90, size = 5, label = "End of bubble economy (1991)") + # End of bubble economy in 1991
  annotate("text", x = 1996, y = 2e+12, angle = 90, size = 5, label = "Asian financial crisis (1997)") + # Asian financial crisis in 1997
  annotate("text", x = 2007, y = 2e+12, angle = 90, size = 5, label = "Lheman shock (2007)")  + # Lheman shock in 2007
  annotate("text", x = 2010, y = 2e+12, angle = 90, size = 5, label = "Earthquake (2011)")  # Lheman shock in 2007

## GDP per capita (USD / pax)
world.gdppercapita <- WDI(indicator = "NY.GDP.PCAP.CD", start = 1960, end = 2017) %>% as_tibble()
world.gdppercapita.sub <-
  world.gdppercapita %>%
  dplyr::filter(country == "Japan")
world.gdppercapita.sub.line <-
  world.gdppercapita.sub %>%
  ggplot(aes(x = year, y = NY.GDP.PCAP.CD)) +
  geom_line() +
  geom_point(shape = 21) +
  theme_minimal() +
  labs(x = "year", y = "GDP per capita (Unit: USD/Pax.)")

## GDP growth rate (%)
world.gdpgrowth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG", start = 1960, end = 2017) %>% as_tibble()
world.gdpgrowth.sub <-
  world.gdpgrowth %>%
  dplyr::filter(country == "Japan")
world.growth.sub.line <-
  world.gdpgrowth.sub %>%
  ggplot(aes(x = year, y = NY.GDP.MKTP.KD.ZG)) +
  geom_line() +
  geom_point(shape = 21) +
  theme_minimal() +
  labs(x = "year", y = "GDP growth rate (Unit: %)")

jpn.gdp.growth <- world.growth.sub.line + geom_hline(yintercept =0)


## Gross savings (% of GDP)
## NY.GNS.ICTR.ZS
world.grosssavings <- WDI(indicator = "NY.GNS.ICTR.ZS", start = 1995, end = 2017) %>% as_tibble()
world.grosssavings.sub <-
  world.grosssavings %>%
  dplyr::filter(country == "Japan")
world.grosssavings.sub.line <-
  world.grosssavings.sub %>%
  ggplot(aes(x = year, y = NY.GNS.ICTR.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "GDP savings (Unit: % of GDP)")


## Expense (% of GDP)
## GC.XPN.TOTL.GD.ZS
world.expense <- WDI(indicator = "GC.XPN.TOTL.GD.ZS", start = 1970, end = 2017) %>% as_tibble()
world.expense.sub <-
  world.expense %>%
  dplyr::filter(country == "Japan")
world.expense.sub.line <-
  world.expense.sub %>%
  ggplot(aes(x = year, y = GC.XPN.TOTL.GD.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Expense (Unit: % of GDP)")


## Exports of goods and services (% of GDP)
## NE.EXP.GNFS.ZS
world.exports <- WDI(indicator = "NE.EXP.GNFS.ZS", start = 1960, end = 2017) %>% as_tibble()
world.exports.sub <-
  world.exports %>%
  dplyr::filter(country == "Japan")
world.exports.sub.line <-
  world.exports.sub %>%
  ggplot(aes(x = year, y = NE.EXP.GNFS.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Exports (Unit: % of GDP)")

## Imports of goods and services (% of GDP)
## NE.IMP.GNFS.ZS
world.imports <- WDI(indicator = "NE.IMP.GNFS.ZS", start = 1960, end = 2017) %>% as_tibble()
world.imports.sub <-
  world.imports %>%
  dplyr::filter(country == "Japan")
world.imports.sub.line <-
  world.imports.sub %>%
  ggplot(aes(x = year, y = NE.IMP.GNFS.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  labs(x = "year", y = "Imports (Unit: % of GDP)")

## FDI (USD)
## BX.KLT.DINV.CD.WD
world.fdi <- WDI(indicator = "BX.KLT.DINV.CD.WD", start = 1975, end = 2017) %>% as_tibble()
world.fdi.sub <-
  world.fdi %>%
  dplyr::filter(country == "Japan")
world.fdi.sub.line <-
  world.fdi.sub %>%
  ggplot(aes(x = year, y = BX.KLT.DINV.CD.WD)) +
  geom_line() +
  geom_point(shape = 21) +
  theme_minimal() +
  labs(x = "year", y = "FDI (Unit: USD)")

## Gross capital formation (% of GDP)
## NE.GDI.TOTL.ZS
world.capitalformation <- WDI(indicator = "NE.GDI.TOTL.ZS", start = 1960, end = 2017) %>% as_tibble()
world.capitalformation.sub <-
  world.capitalformation %>%
  dplyr::filter(country == "Japan")
world.capitalformation.sub.line <-
  world.capitalformation.sub %>%
  ggplot(aes(x = year, y = NE.GDI.TOTL.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  theme_minimal() +
  labs(x = "year", y = "Capital Formation (Unit: % of GDP)")

## Inflation, consumer prices (annual %)
## FP.CPI.TOTL.ZG
world.inflation <- WDI(indicator = "FP.CPI.TOTL.ZG", start = 1960, end = 2017) %>% as_tibble()
world.inflation.sub <-
  world.inflation %>%
  dplyr::filter(country == "Japan")
world.inflation.sub.line <-
  world.inflation.sub %>%
  ggplot(aes(x = year, y = FP.CPI.TOTL.ZG)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Annual inflation rate (Unit: %)")

#
# ----- END


# ---- population ----

## Total population (Unit: Billion)
## SP.POP.TOTL
japan.population <- WDI(country = "JP", indicator = "SP.POP.TOTL", start = 1960, end = 2017) %>%
  as_tibble()
japan.population.sub <-
  japan.population %>%
  dplyr::filter(country == "Japan")
japan.population.sub.line <-
  japan.population.sub %>%
  ggplot(aes(x = year, y = SP.POP.TOTL)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Total population (Unit: Billion")


# Population growth
# SP.POP.GROW
japan.populationgrowth <- WDI(country = "JP", indicator = "SP.POP.GROW", start = 1960, end = 2017) %>%
  as_tibble()
japan.populationgrowth.sub <-
  japan.populationgrowth %>%
  dplyr::filter(country == "Japan")
japan.populationgrowth.sub.line <-
  japan.populationgrowth.sub %>%
  ggplot(aes(x = year, y = SP.POP.GROW)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Population growth rate (Unit: %)")

# Population ages 0-14 (Unit: %)
# SP.POP.0014.TO.ZS
japan.population0014 <- WDI(country = "JP", indicator = "SP.POP.0014.TO.ZS", start = 1960, end = 2017) %>%
  as_tibble()
japan.population0014.sub <-
  japan.population0014 %>%
  dplyr::filter(country == "Japan")
japan.population0014.sub.line <-
  japan.population0014.sub %>%
  ggplot(aes(x = year, y = SP.POP.0014.TO.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Population ages 0-14 (Unit: %)")

# Population ages 15-64 (Unit: %)
# SP.POP.1564.TO.ZS
japan.population1564 <- WDI(country = "JP", indicator = "SP.POP.1564.TO.ZS", start = 1960, end = 2017) %>%
  as_tibble()
japan.population1564.sub <-
  japan.population1564 %>%
  dplyr::filter(country == "Japan")
japan.population1564.sub.line <-
  japan.population1564.sub %>%
  ggplot(aes(x = year, y = SP.POP.1564.TO.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Population ages 15-64 (Unit: %)")

# Aging rate (Unit: %)
# SP.POP.65UP.TO.ZS
japan.population65 <- WDI(country = "JP", indicator = "SP.POP.65UP.TO.ZS", start = 1960, end = 2017) %>%
  as_tibble()
japan.population65.sub <-
  japan.population65 %>%
  dplyr::filter(country == "Japan")
japan.population65.sub.line <-
  japan.population65.sub %>%
  ggplot(aes(x = year, y = SP.POP.65UP.TO.ZS)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Population ages 65 and over (Unit: %)")

# Total fertility rate
# SP.DYN.TFRT.IN
japan.tfr <- WDI(country = "JP", indicator = "SP.DYN.TFRT.IN", start = 1960, end = 2017) %>%
  as_tibble()
japan.tfr.sub <-
  japan.tfr %>%
  dplyr::filter(country == "Japan")
japan.tfr.sub.line <-
  japan.tfr.sub %>%
  ggplot(aes(x = year, y = SP.DYN.TFRT.IN)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Total fertility rate")

# Population density (Unit: persons per km^2)
# EN.POP.DNST
japan.popdensity <- WDI(country = "JP", indicator = "EN.POP.DNST", start = 1960, end = 2017) %>%
  as_tibble()
japan.popdensity.sub <-
  japan.popdensity %>%
  dplyr::filter(country == "Japan")
japan.popdensity.sub.line <-
  japan.popdensity.sub %>%
  ggplot(aes(x = year, y = EN.POP.DNST)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "year", y = "Pupulatoin density (Unit: km^2/person")




# ---- industry.line ----
lfs.industry <-
  readxl::read_excel("lfs_lt_industry.xls",
                     sheet = "lfs.lt.industry",
                     range = "A1:S18",
                     col_names = TRUE
                     ) %>%
  dplyr::mutate(year = c(2002:2018)) %>%
  tidyr::gather(key = industry, value = number, -year)

selected.industry <- c("Accommodations, eating and drinking services", "Construction","Manufacturing", "Medical, health care and welfare","Transport and postal activities", "Wholesale and retail trade")

lfs.industry.line <-
  lfs.industry %>%
  dplyr::filter(industry %in% selected.industry) %>%
  ggplot(aes(x = year,
             y = number,
             colour = industry,
             shape = industry
             )
         ) +
  geom_line(size = 1) +
  geom_point() +
  theme_classic()

lfs.industry.line


lfs.status.industry <-
  readxl::read_excel("lfs_lt_industry.xls",
                     sheet = "lfs.status.industry",
                     range = "A1:E37",
                     col_names = TRUE
  ) %>%
  tidyr::gather(key = status, value = number, self.employed, family.worker, employee) %>%
  mutate(year = as.numeric(year))

lfs.status.industry.bar.01 <-
  lfs.status.industry %>%
  dplyr::filter(year == 2018) %>%
  ggplot(aes(x = status, y = number, fill = industry)) +
  geom_bar(stat = "identity") +
  theme_classic()

lfs.status.industry.bar.02 <-
  lfs.status.industry %>%
  dplyr::filter(year == 2018) %>%
  ggplot(aes(x = industry, y = number, fill = status)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(label=c("Accommo",
                           "Construction",
                           "Manufacturing",
                           "Health",
                           "Transportation",
                           "Sales"
                           )
                   ) +
  theme_classic()




# Aging rate (Unit: %)
# SP.POP.65UP.TO.ZS
industry.gdp.comparison <-
  WDI(country = "all",
      indicator = "NV.IND.MANF.ZS",
      start = 1960,
      end = 2017
      ) %>%
  as_tibble() %>%
  dplyr::filter(country %in% country.list)

industry.gdp.comparison.line.01 <-
  industry.gdp.comparison %>%
  ggplot(aes(x = year, y = NV.IND.MANF.ZS, colour = country)) +
  geom_line(size = 1) +
  # gghighlight(country == "Japan",
  #             label_key = country
  #             ) +
  geom_text_repel(
    data = subset(industry.gdp.comparison, year == max(year)),
    aes(label = country),
    nudge_x = 50,
    segment.alpha = 0.5,
    size = 5
    ) +
  theme(legend.position = "none") +
  # theme_classic() +
  labs(x = "Year", y = "GDP by manufacturing (Unit: %)")
industry.gdp.comparison.line.01

industry.gdp.comparison.line.02 <-
  industry.gdp.comparison %>%
  ggplot(aes(x = year, y = NV.IND.MANF.ZS, colour = country)) +
  geom_line(size = 2) +
  gghighlight(country == "Japan",
              label_key = country
              ) +
  theme_classic() +
  labs(x = "Year", y = "GDP by manufacturing (Unit: %)")
industry.gdp.comparison.line.02

#
### ----- END -----###

---- wage.census ----
census.ts.grads.male <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "ts.grads",
                     range = "A2:D45",
                     col_names = TRUE
  ) %>%
  dplyr::mutate(year = c(1976:2018)) %>%
  dplyr::mutate(gender = "male") %>%
  tidyr::gather(key = school, value = amount, -year, -gender)

census.ts.grads.female <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "ts.grads",
                     range = "E2:G45",
                     col_names = TRUE
  ) %>%
  dplyr::mutate(year = c(1976:2018)) %>%
  dplyr::mutate(gender = "female") %>%
  tidyr::gather(key = school, value = amount, -year, -gender)
census.prefecture.name <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "prefecture.mf",
                     range = "d14:d60",
                     col_names = FALSE
  ) %>%
  magrittr::set_colnames(x =., value = c("prefecture"))
census.prefecture.male <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "prefecture.mf",
                     range = "i14:k60",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("base.payment","regular.payment","dividend")) %>%
  bind_cols(census.prefecture.name) %>%
  dplyr::mutate(gender = "male") %>%
  tidyr::gather(key = type, value = amount, -prefecture, -gender)
census.prefecture.female <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "prefecture.mf",
                     range = "q14:s60",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("base.payment","regular.payment","dividend")) %>%
  bind_cols(census.prefecture.name) %>%
  dplyr::mutate(gender = "female") %>%
  tidyr::gather(key = type, value = amount, -prefecture, -gender)

census.ts.grads <- dplyr::bind_rows(census.ts.grads.male, census.ts.grads.female)
census.prefecture <- bind_rows(census.prefecture.male, census.prefecture.female)

jpn.map <- readRDS("gadm36_JPN_1_sf.rds")
jpn.map.amount <-
  jpn.map %>%
  left_join(., census.prefecture, by = c("NAME_1"= "prefecture"))




# base
census.ageclass <- data.frame(census.ageclass = c("under 19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69", "70 and over"))
census.seniority.base.total <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "base",
                     range = "d5:L16",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "total") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.base.jh <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "base",
                     range = "d18:L29",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "jh") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.base.high <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "base",
                     range = "d31:L42",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "high") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.base.college <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "base",
                     range = "d44:L55",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "college") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.base.univ <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "base",
                     range = "d57:L68",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "univ") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))


census.seniority.base <-
  dplyr::bind_rows(census.seniority.base.jh,
                   census.seniority.base.high,
                   census.seniority.base.college,
                   census.seniority.base.univ
                   )

census.seniority.base <- transform(census.seniority.base, service= factor(service, levels = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over"))) %>%
  as_tibble()

## dividend
## dividend
census.seniority.dividend.total <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "dividend",
                     range = "d5:L16",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "total") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.dividend.jh <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "dividend",
                     range = "d18:L29",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "jh") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.dividend.high <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "dividend",
                     range = "d31:L42",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "high") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.dividend.college <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "dividend",
                     range = "d44:L55",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "college") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))

census.seniority.dividend.univ <-
  readxl::read_excel("wage_census.xlsx",
                     sheet = "dividend",
                     range = "d57:L68",
                     col_names = FALSE
  ) %>%
  magrittr::set_names(x = ., value = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over")) %>%
  bind_cols(census.ageclass) %>%
  dplyr::mutate(gender = "total") %>%
  dplyr::mutate(school = "univ") %>%
  tidyr::gather(key = service, value = amount, -census.ageclass, -gender, -school) %>%
  dplyr::mutate(amount = as.numeric(amount))


census.seniority.dividend <-
  dplyr::bind_rows(census.seniority.dividend.jh,
                   census.seniority.dividend.high,
                   census.seniority.dividend.college,
                   census.seniority.dividend.univ
  )

census.seniority.dividend <- transform(census.seniority.dividend, service= factor(service, levels = c("0","1-2","3-4","5-9","10-14","15-19","20-24","25-29","30 and over"))) %>%
  as_tibble()

###
jpn.wage.ts <- census.ts.grads %>%
  ggplot(aes(x = year, y = amount)) +
  geom_line(size = 1) +
  facet_wrap(~ gender + school) +
  theme_classic()

jpn.wage.map <- jpn.map.amount %>%
  ggplot2::ggplot(aes(fill = amount)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
                       ) +
  facet_grid(gender ~ type) +
  theme_classic()

jpn.wage.seniority.base <-
  census.seniority.base %>%
  dplyr::filter(service != "total") %>%
  ggplot2::ggplot(aes(x = service, y = amount, colour = census.ageclass, group = census.ageclass)) +
  geom_point()+
  geom_line(size = 1)+
  facet_wrap(~ school) +
  labs(x = "Length of service", y = "Amount (Unit: 1,000JPY, CY2018, regular payment)", colour = "Age class") +
  theme_classic()

jpn.wage.seniority.dividend <-
  census.seniority.dividend %>%
  dplyr::filter(service != "total") %>%
  ggplot2::ggplot(aes(x = service, y = amount, colour = census.ageclass, group = census.ageclass)) +
  geom_point()+
  geom_line(size = 1)+
  facet_wrap(~ school) +
  labs(x = "Length of service", y = "Amount (Unit: 1,000JPY, CY2018, dividend)", colour = "Age class") +
  theme_classic()


jpn.wage.ts
ggsave("jpn.wage.ts.pdf")
jpn.wage.map
ggsave("jpn.wage.map.pdf")
jpn.wage.seniority.base
ggsave("jpn.wage.seniority.base.pdf")
jpn.wage.seniority.dividend
ggsave("jpn.wage.seniority.dividend.pdf")


# ---- labor.union ----
# Definition of is_blank function
# by
# https://id.fnshr.info/2017/08/14/r-blank-row-col/
is_blank <- function(x) {is.na(x) | x == ""}

# read data
labor.union.unit <-
  readxl::read_excel("labor_union.xlsx",
                     sheet = "union.ts",
                     range = "c12:d84",
                     col_names = FALSE
  ) %>%
  dplyr::filter(
    rowSums(is_blank(.)) != ncol(.)
  ) %>% 
  magrittr::set_colnames(c("N.of.union","N.of.member")) %>% 
  dplyr::mutate(year = c(1953:2018)) %>%
  dplyr::mutate(type = c("unit")) %>% 
  tidyr::gather(key = trait, value = number, -year, -type)
labor.union.single <-
  readxl::read_excel("labor_union.xlsx",
                     sheet = "union.ts",
                     range = "e12:f84",
                     col_names = FALSE
  ) %>%
  dplyr::filter(
    rowSums(is_blank(.)) != ncol(.)
  ) %>% 
  magrittr::set_colnames(c("N.of.union","N.of.member")) %>% 
  dplyr::mutate(year = c(1953:2018)) %>%
  dplyr::mutate(type = c("single")) %>% 
  tidyr::gather(key = trait, value = number, -year, -type)
labor.union.01 <- bind_rows(labor.union.unit, labor.union.single)





labor.union.member <-
  readxl::read_excel("labor_union.xlsx",
                     sheet = "union.prefecture",
                     range = "b3:FI52",
                     col_names = FALSE
  )

varname<-colnames(labor.union.member)
hoge <-
  labor.union.member %>%
  dplyr::filter_(paste(varname, "==", "北"))

dplyr::filter(str_detect(., "北"))

dplyr::filter(
  rowSums(is_blank(.)) != ncol(.)
) %>%
  magrittr::set_colnames(c("N.of.union","N.of.member")) %>%
  dplyr::mutate(year = c(1953:2018)) %>%
  dplyr::mutate(type = c("unit")) %>%
  tidyr::gather(key = trait, value = number, -year, -type)



# line plot by type and trait
labor.union.01.line <- 
  labor.union.01 %>% 
  ggplot2::ggplot(aes(x = year, y = number)) +
  geom_point() +
  geom_line ()+
  facet_wrap(~ type + trait, scale = "free_y") +
  labs(x = "Year (1953-2018)", y = "N. of mamber & N. of Labor Union ") + 
  theme_classic()

#
# ----- END




# ---- japan.liquor ----
japanmap.data.01 <- 
  readr::read_rds("./map/gadm36_JPN_1_sf.rds") %>% 
  dplyr::filter(NAME_1 != "Okinawa") 

liquor.map <-
  readxl::read_excel("liquor.xlsx",
                     sheet = "liquor",
                     range = "A1:P47",
                     col_names = TRUE
  ) %>% 
  dplyr::left_join(japanmap.data.01, ., by = c("NAME_1" = "province"))



##
jpn.liquor.map.sake <- 
  liquor.map %>%
  ggplot2::ggplot(aes(fill = sake/population)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
                       ) +
  xlim(125,150) +
  labs(x = "Longitude", y = "Latitude", fill = "Sake (Unit: KL/year*person)") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))

##
jpn.liquor.map.shochu <- 
  liquor.map %>%
  ggplot2::ggplot(aes(fill = shochu/population)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
  ) +
  xlim(125,150) +
  labs(x = "Longitude", y = "Latitude", fill = "Shochu (Unit: KL/year*person)") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))

##
jpn.liquor.map.mirin <- 
  liquor.map %>%
  ggplot2::ggplot(aes(fill = mirin/population)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
  ) +
  xlim(125,150) +
  labs(x = "Longitude", y = "Latitude", fill = "Mirin (Unit: KL/year*person)") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))

##
jpn.liquor.map.beer <- 
  liquor.map %>%
  ggplot2::ggplot(aes(fill = beer/population)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
  ) +
  xlim(125,150) +
  labs(x = "Longitude", y = "Latitude", fill = "Beer (Unit: KL/year*person)") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))

##
jpn.liquor.map.wine <- 
  liquor.map %>%
  ggplot2::ggplot(aes(fill = wine/population)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
  ) +
  xlim(125,150) +
  labs(x = "Longitude", y = "Latitude", fill = "Wine (Unit: KL/year*person)") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))

##
jpn.liquor.map.whisky <- 
  liquor.map %>%
  ggplot2::ggplot(aes(fill = whisky/population)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
  ) +
  xlim(125,150) +
  labs(x = "Longitude", y = "Latitude", fill = "Whisky (Unit: KL/year*person)") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))

##
jpn.liquor.map.happoshu <- 
  liquor.map %>%
  ggplot2::ggplot(aes(fill = happoshu/population)) +
  geom_sf() +
  scale_fill_viridis_c(begin =1,
                       end = 0,
                       space = "Lab"
  ) +
  xlim(125,150) +
  labs(x = "Longitude", y = "Latitude", fill = "Happoshu (Unit: KL/year*person)") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))

# pdf("jpn.liquor.map.pdf")
# jpn.liquor.map.sake
# jpn.liquor.map.shochu
# jpn.liquor.map.mirin
# jpn.liquor.map.beer
# jpn.liquor.map.wine
# jpn.liquor.map.whisky
# jpn.liquor.map.happoshu
# dev.off()
#
# ----- END





# # results of special topics of global economy FY2019
# evaluation.2019 <- readxl::read_excel("final_report_evaluation.xlsx",
#                                       range = "B1:DY12",
#                                       col_names = TRUE
#                                       )
# 
# results.2019 <- 
#   evaluation.2019 %>% 
#   tidyr::gather(key = trait,
#                 value = grade, 
#                 -email, -Name
#                 ) %>% 
#   dplyr::mutate(score = str_replace_all(grade,
#                                         pattern = c(
#                                           "Excellent" = "5",
#                                           "Very good" = "4",
#                                           "Good" = "3",
#                                           "Acceptable" = "2",
#                                           "Poor" = "1",
#                                           "NONE" = "NA"
#                                           )
#                                         )
#                 ) %>%
#   mutate(score = as.numeric(as.character(score))) %>% 
#   dplyr::mutate(name_short = str_replace_all(trait,
#                                         pattern = c(
#                                           "Clear focus" = "",
#                                           "Accomplishment" = "",
#                                           "Conclusion" = "",
#                                           "Organization" = "",
#                                           "Creativity" = "",
#                                           "Visualization" = ""
#                                         )
#                                         )
#                 ) %>% 
#   group_by(name_short) %>% 
#   summarize(Mean = 20*mean(score, na.rm = TRUE)+5)
# 
# # write.csv(results.2019,"results.2019.csv")




# ---- exchange.rate ----

exchange_rate <- 
  readxl::read_excel("exchange_rate_73_22.xlsx") %>% 
  dplyr::mutate(hundred_hundredfifty = factor(if_else(price > 100 & price < 150, "in", "out")))

exchange_rate_line <- 
  exchange_rate %>% 
  ggplot2::ggplot(aes(x = month_year, y = price)) +
  geom_line() +
  geom_point(aes(color = hundred_hundredfifty)) +
  labs(x = "Year (Jan. 1973 - Sep. 2022)", y = "Exchange rate between JPY and USD (Unit: JPY)") +
  scale_color_okabeito() +
  theme_classic() + 
  theme(
    legend.position = "none"
  )

