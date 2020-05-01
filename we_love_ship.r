# we love ships!!
# Made: 1st. May 2020
# Revised: 
# by Yuzuru Utsunomiya

# ---- load.library ----
library(ggmap)
library(ggthemes)
library(magrittr)
library(Rilostat)
library(tidyverse)
library(wppExplorer)
library(sf)



nagasaki.depth <- 
  readxl::read_excel("depth.nagasaki.xlsx",
                     sheet = "depth", 
                     range = "a1:b932"
                     )

# # WARNING
# # THIS PROCESS NEEDS COMPUTATION PERIOD.
# # COMMENT OUT WHEN NOT IN USE.
# # Make maps to detect farmers' / fishermen's using areas
# # By having them draw an area (circle, rectangle), we are able to 
# # obtain exact information on their land use.
# # read Google API
# # change the API code as you obtain from Google
# ---- area.map ----
source("../../r_project/map.key.r")
# #
# ## --- END ---


# function to make a boundary box
# Original code is below.
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
# EPSG by google can be obtained from below.
# https://colauttilab.github.io/EcologyTutorials/mapping.html
# ---- ggmap.bbox.fun ----
ggmap.bbox.fun <- function(sat.map) {
  if (!inherits(sat.map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(sat.map,
                                   "bb"
  )
  ),
  c("ymin",
    "xmin",
    "ymax",
    "xmax"
  )
  )
  # Convert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  # st_as_sfc requires CRS. Google maps obtained from get_map() has no CRS info.
  # We need to set the temporal CRS first. Then we transform the temporal CRS
  # into real one (3857).
  bbox.3857 <-
    map_bbox %>%
    st_bbox(crs = 4326) %>%
    st_as_sfc %>%
    st_transform(crs = 3857) %>%
    st_bbox()
  # Overwrite the bbox of the ggmap object with the transformed coordinates
  # Names below can be obtained using str(map) function
  attr(sat.map, "bb")$ll.lat <- bbox.3857["ymin"]
  attr(sat.map, "bb")$ll.lon <- bbox.3857["xmin"]
  attr(sat.map, "bb")$ur.lat <- bbox.3857["ymax"]
  attr(sat.map, "bb")$ur.lon <- bbox.3857["xmax"]
  sat.map
}
#
## --- END ---

# obtain a map from Google
# Internet connection and Google API are necessary.
# ---- sat.grid.fun ----

sat.map <-
  ggmap::get_map(
    location = c(
      lon = 129.860988,
      lat = 32.734841
    ),
    maptype = "satellite",
    zoom = 15
  )
# make grids
#
# https://tsukubar.github.io/r-spatial-guide/simple-feature-for-r.html
# https://gis.stackexchange.com/questions/88830/overlaying-spatial-polygon-with-grid-and-checking-in-which-grid-element-specific

map.grid <-
  tibble::data_frame(
    id = seq(1,2),
    lon = as.numeric(unlist(attr(sat.map, which = "bb"))[c(2,4)]),
    lat = as.numeric(unlist(attr(sat.map, which = "bb"))[c(1,3)])
  ) %>%
  sf::st_as_sf(
    coords = c("lon","lat"),
    crs = 4326
  ) %>%
  st_transform(crs = 3857) %>%
  st_bbox() %>%
  sf::st_make_grid(.,
                   cellsize = 100 # size of grid. Unit is metre (m)
  ) %>% 
  st_sf(id = 1:length(.)) %>% 
  dplyr::left_join(., nagasaki.depth, by = "id") %>% 
  dplyr::mutate(depth = as.numeric(depth))


map.label <-
  map.grid %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.)) 

# make a ggplot-object map from the obtained map and
# overlay the grid on the map
sat.map <- ggmap.bbox.fun(sat.map)

ggmap(sat.map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = map.grid,
          aes(fill = depth),
          colour = "white",
          alpha = 0.5,
          lwd = 0.01,
          inherit.aes = FALSE
  ) +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(
    data = map.label,
    aes(x = X, y = Y, label = id),
    colour = "white",
    size = 2
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       subtitle = "Size of square is 100m*100m"
  ) +
  theme_minimal()
#
## --- END ---


# ggsave("port.map.grid.sat.pdf")



# # save the maps as a pdf file
# # comment out when not in use.
# # ---- target.landuse.map ----
# lon.lat.id %>%
#   pmap(sat.grid.fun)
# dev.off()
# # #
# # #
# # ## --- END ---



