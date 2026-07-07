

library(basemaps)
library(gganimate)


map_tiler_key <- read.delim("maptiler")
map_tiler_key <- names(map_tiler_key)

# buffed route polygon
route_buffer <- dc_routes %>% 
  summarise() %>% 
  st_buffer(1200) %>% 
  st_transform(crs = 3857)




inverse <- st_difference(st_bbox(route_buffer) %>% st_as_sfc(), route_buffer)
inverse_plus <- st_difference(st_bbox(route_buffer) %>% st_as_sfc(), st_buffer(route_buffer, -200))

pings_unq_trip <- ping_sample_one_route(pings = pings_in_min, route_number = "1")
pings_unq_trip_id <- pings_unq_trip %>% 
  #filter(directionRef == "inbound") %>% 
  filter(hour(time) == 8) %>% 
  pull(journeyCodeUnq) %>% 
  unique() #%>% 
  #sample(2)
pings_unq_trip <- pings_unq_trip %>%
  #filter(journeyCodeUnq %in% pings_unq_trip_id) %>% 
  group_by(journeyCodeUnq) %>% 
  filter(n() > 60) %>% 
  st_transform(crs = 3857) # %>% 
  #ungroup()
  





# view all available maps
get_maptypes()

# set defaults for the basemap
#set_defaults(map_service = "carto", map_type = "light")  # don't need a key for carto maps
set_defaults(map_service = "maptiler", map_type = "dataviz")

# load and return basemap map as class of choice, e.g. as image using magick:
#basemap_magick(boundary)
library(ggplot2)
### basemap 
route_basemap <- 
  ggplot() + 
  basemap_gglayer(route_buffer, map_token = map_tiler_key) +
  scale_fill_identity() + 
  coord_sf() +
  theme_void()


route_basemap <- 
  route_basemap +
  ggplot2::geom_sf(data = inverse_plus, fill = "#fff",
                 # colour = "#666", 
                 alpha = 0.4,
                 linewidth = NA) +
  ggplot2::geom_sf(data = inverse, fill = "#fff",
                   # colour = "#666", 
                   alpha = 0.4,
                   linewidth = NA)

route_basemap +
  ggplot2::geom_sf(data = pings_unq_route %>% st_transform(3857),
                   colour = pal[2],
                   linewidth = NA,
                   size = 0.5,
                   alpha = 0.4)

ggsave(filename = glue::glue("qmd/pings_unq_route_focus.jpeg"),
       plot = get_last_plot(),
       dpi = 320,
       width = 12,
       height = 8,
       units = "cm")


### map - one trip only
route_basemap +
  ggplot2::geom_sf(data = pings_unq_trip %>% st_transform(3857),
                   colour = pal[2],
                   linewidth = NA,
                   size = 1,
                   alpha = 0.8)

ggsave(filename = glue::glue("qmd/pings_unq_route_focus.jpeg"),
       plot = get_last_plot(),
       dpi = 320,
       width = 12,
       height = 8,
       units = "cm")

## animate pings 

pings_anim <- 
  route_basemap +
  ggplot2::geom_sf(data = pings_unq_trip %>% st_transform(3857),
                   aes(color = journeyCodeUnq),
                   #colour = pal[2],
                   linewidth = NA,
                   size = 4,
                   alpha = 0.8) +
  scale_colour_manual(values=pal)

pings_anim <- pings_anim +
  transition_states(time, transition_length = 1, state_length = 1) +
  shadow_wake(0.9, size = 1, alpha = 0.2) +
  labs( subtitle = "Time: {frame_along}" ) 
  
  #transition_time(time = time) +
  #shadow_mark(past = TRUE)

animate( pings_anim, fps = 10, nframes = 30, duration = 5)
  
anim_save(file = "gif/pings_anim_multi.gif",
          plot = get_last_plot(),
          dpi = 320,
          width = 12,
          height = 8,
          units = "cm")



