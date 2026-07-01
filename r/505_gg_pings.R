library(basemaps)


map_tiler_key <- read.delim("maptiler")

boundary <- st_read(choose.files()) %>% st_transform(crs = 3857)
#bbox <- st_read(choose.files()) %>% st_transform(crs = 3857)
#land <- st_read(choose.files()) %>% st_transform(crs = 3857)
#inverse <- st_read(choose.files()) %>% st_transform(crs = 3857)

boundary_buffer <- st_buffer(boundary, 2000)

inverse <- st_difference(st_bbox(boundary_buffer) %>% st_as_sfc(), boundary)

# view all available maps
get_maptypes()

# set defaults for the basemap
#set_defaults(map_service = "carto", map_type = "light")  # don't need a key for carto maps
set_defaults(map_service = "maptiler", map_type = "dataviz")

# load and return basemap map as class of choice, e.g. as image using magick:
#basemap_magick(boundary)
library(ggplot2)
weca_basemap <- 
  ggplot() + 
  basemap_gglayer(boundary_buffer, map_token = map_tiler_key) +
  scale_fill_identity() + 
  coord_sf() +
  theme_void()

weca_basemap

bod_eg_weca <- bod_eg %>% 
  st_transform(crs = 3857) %>% 
  st_intersection(boundary)

one_ping <- bod_eg %>% sample_n(3)
one_hour_pings <- bod_eg %>% filter(hour(time) == 8)

weca_basemap +
  ggplot2::geom_sf(data = one_ping %>% st_transform(3857)) +
  ggplot2::geom_sf(data = inverse) +
  # geom_sf_label(data = one_ping, aes(label = time), 
  #               #vjust = 1.5, 
  #               size = 3, 
  #               position = "nudge" ) +
  ggrepel::geom_label_repel(
    data = one_ping,
    aes(label = as.character(time), geometry = geometry),
    size = 2,
    stat = "sf_coordinates",
    min.segment.length = 10
  )
  #ggplot2::geom_sf(data = boundary) +
 # xlim(c(st_bbox(boundary_buffer)[1],st_bbox(boundary_buffer)[3])) +
 # ylim(c(st_bbox(boundary_buffer)[2],st_bbox(boundary_buffer)[4])) +
  theme(
    plot.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) 

  
  
  weca_basemap +
    ggplot2::geom_sf(data = one_hour_pings %>% st_transform(3857),
                     size = 0.5) +
    # geom_sf_label(data = one_ping, aes(label = time), 
    #               #vjust = 1.5, 
    #               size = 3, 
    #               position = "nudge" ) +
    # ggrepel::geom_label_repel(
    #   data = one_ping,
    #   aes(label = as.character(time), geometry = geometry),
    #   size = 2,
    #   stat = "sf_coordinates",
    #   min.segment.length = 10
    # ) +
    ggplot2::geom_sf(data = inverse) 
  