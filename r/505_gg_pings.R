library(basemaps)


map_tiler_key <- read.delim("maptiler")
map_tiler_key <- names(map_tiler_key)

boundary <- st_read(choose.files()) %>% st_transform(crs = 3857)
#bbox <- st_read(choose.files()) %>% st_transform(crs = 3857)
#land <- st_read(choose.files()) %>% st_transform(crs = 3857)
#inverse <- st_read(choose.files()) %>% st_transform(crs = 3857)

boundary_buffer <- st_buffer(boundary, 5000)

inverse <- st_difference(st_bbox(boundary_buffer) %>% st_as_sfc(), boundary)
inverse_boundary_buffer <- st_difference(st_bbox(boundary_buffer) %>% st_as_sfc(),
                                         boundary_buffer)

# view all available maps
get_maptypes()

# set defaults for the basemap
#set_defaults(map_service = "carto", map_type = "light")  # don't need a key for carto maps
set_defaults(map_service = "maptiler", map_type = "dataviz")

# load and return basemap map as class of choice, e.g. as image using magick:
#basemap_magick(boundary)
library(ggplot2)
### basemap 
weca_basemap <- 
  ggplot() + 
  basemap_gglayer(boundary_buffer, map_token = map_tiler_key) +
  scale_fill_identity() + 
  coord_sf() +
  theme_void()
### basemap with inverse filters applied
weca_basemap <- 
  weca_basemap +
  ggplot2::geom_sf(data = inverse, fill = "#fff",
                  # colour = "#666", 
                   alpha = 0.5,
                   linewidth = NA) +
  ggplot2::geom_sf(data = inverse_boundary_buffer, 
                   fill = "#fff",
                   #colour = "#666", 
                   alpha = 0.9,
                   linewidth = NA) +
  geom_sf(data = boundary, 
          alpha = 0,
          linewidth = 0.5)

#weca_basemap2

bod_eg_weca <- bod_eg %>% 
  sample_n(1000) %>% 
  st_transform(crs = 3857) %>% 
  st_intersection(boundary)

one_ping <- bod_eg %>% sample_n(3)

ping_sample_x_mins <- function(pings, hour_of_day = c(8), mins = c(1:5)){
  
min_pings <- pings %>% 
  filter(hour(time) %in% hour_of_day) %>%
  filter(minute(time) %in% mins) %>% 
  #sample_n(10000) %>% 
  st_transform(crs = 3857) %>% 
  st_intersection(boundary)
 
return(min_pings) 
}

ping_sample_one_route <- function(route_number = "1"){
  
  pings_unq_route <- bod_eg %>%
    filter(ticketMachineServiceCode %in% route_number)
  return(pings_unq_route)
}

pings_unq_route <- ping_sample_one_route(route_number = "1")
pings_unq_route <- pings_unq_route %>% ping_sample_x_mins(hour_of_day = c(8), mins = c(0:30))

minutes <- 30

pings_in_min <- ping_sample_x_mins(pings = bod_eg, hour_of_day = c(8), mins = c(1:minutes))

weca_basemap +
  ggplot2::geom_sf(data = pings_in_min %>% st_transform(3857),
                   colour = pal[2],
                   linewidth = NA,
                   size = 0.1,
                   alpha = 0.1)

ggsave(filename = glue::glue("qmd/pings_in_{minutes}mins.jpeg"),
       plot = get_last_plot(),
       dpi = 320,
       width = 12,
       height = 8,
       units = "cm")



weca_basemap +
  ggplot2::geom_sf(data = one_ping %>% st_transform(3857),
                   colour = pal[2],
                   size = 1) +
  ggrepel::geom_label_repel(
    data = one_ping,
    aes(label = as.character(time), geometry = geometry),
    size = 2,
    stat = "sf_coordinates"
   # min.segment.length = 10
  )

  
  weca_basemap +
    ggplot2::geom_sf(data = pings_1min %>% st_transform(3857),
                     colour = pal[2],
                     size = 0.8,
                     alpha = 0.2)
  
  ggsave(filename = glue::glue("qmd/pings_random.jpeg"),
         plot = get_last_plot(),
         dpi = 320,
         width = 12,
         height = 8,
         units = "cm")
  
  
  ### ggplot: one route
  
  weca_basemap +
    ggplot2::geom_sf(data = pings_unq_route %>% st_transform(3857),
                     colour = pal[2],
                     linewidth = NA,
                     size = 0.1,
                     alpha = 0.1)
  
  ggsave(filename = glue::glue("qmd/pings_unq_route.jpeg"),
         plot = get_last_plot(),
         dpi = 320,
         width = 12,
         height = 8,
         units = "cm")
  