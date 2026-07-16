

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

#pings_unq_trip <- ping_sample_one_route(pings = pings_in_min, route_number = "1")
pings_unq_trip_id <- pings %>% 
  #filter(directionRef == "inbound") %>% 
  filter(hour(time) == 8) %>% 
  pull(journeyCodeUnq) %>% 
  unique() %>% 
  sample(2)

pings_unq_trip <- pings %>% 
  filter(journeyCodeUnq %in% pings_unq_trip_id)

pings_unq_trip <- pings_unq_trip %>%
  #filter(journeyCodeUnq %in% pings_unq_trip_id) %>% 
  group_by(journeyCodeUnq) %>% 
  filter(n() > 100) %>% 
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
                   colour = "#fff", 
                   alpha = 1,
                   linewidth = 1)

route_basemap +
  ggplot2::geom_sf(data = pings_unq_trip %>% st_transform(3857),
                   colour = pal_bright[2],
                   linewidth = NA,
                   size = 0.5,
                   alpha = 0.4)

route_basemap +
ggplot2::geom_sf(data = pings_unq_trip %>% st_transform(3857),# %>% filter(journeyCode == "0823"),
                 aes(color = journeyCodeUnq),
                 #colour = pal[2],
                 linewidth = NA,
                 size = 1,
                 alpha = 0.6) +
  scale_colour_manual(values=rep(pal_bright[1:6],10)) +
  facet_grid(rows = ~journeyCodeUnq)

ggsave(filename = glue::glue("qmd/pings_unq_route_focus.jpeg"),
       plot = get_last_plot(),
       dpi = 320,
       width = 12,
       height = 8,
       units = "cm")


### map - one trip only
route_basemap +
  ggplot2::geom_sf(data = pings_unq_trip %>% st_transform(3857),
                   colour = pal_bright[2],
                   linewidth = NA,
                   size = 1,
                   alpha = 0.8) +
  labs(title = "pings on X39 route", caption = "08:00") +
  theme(plot.title = element_text(hjust = 1, vjust = -10),
        plot.caption = element_text(hjust = 0, vjust = 15, size = 20, face = "bold", colour = "#888"))

ggsave(filename = glue::glue("qmd/pings_unq_route_focus.jpeg"),
       plot = get_last_plot(),
       dpi = 320,
       width = 12,
       height = 8,
       units = "cm")

## animate pings 

# pings_anim <- 
#   route_basemap +
#   ggplot2::geom_sf(data = pings_unq_trip %>% st_transform(3857),# %>%  filter(journeyCode == "0751"),
#                    aes(#group = journeyCodeUnq, 
#                        # color = dist_m_rank),
#                       color = journeyCodeUnq),
#                    #colour = pal[2],
#                    linewidth = NA,
#                    size = 4,
#                    alpha = 0.8) +
#   scale_colour_manual(values=rep(pal_bright[5:6],10)) #+
  #facet_grid(rows = ~journeyCodeUnq)
  # scale_color_binned(palette = pal_rainbow[2:7])


# use geom_point instead of geom_sf
pings_unq_trip_pnt <- ping_speed(pings = pings_unq_trip) 

pings_unq_trip_pnt <- pings_unq_trip_pnt %>% ungroup() %>% 
  mutate(x = st_coordinates(pings_unq_trip)[,1]) %>% 
  mutate(y = st_coordinates(pings_unq_trip)[,2]) %>% 
  st_drop_geometry()

pings_anim <- 
  route_basemap +
  ggplot2::geom_point(data = pings_unq_trip_pnt,# %>%  filter(journeyCode == "0751"),
                   aes(#group = journeyCodeUnq, 
                     # color = dist_m_rank),
                     x = x,
                     y = y,
                     color = ping_speed),
                   #colour = pal[2],
                   #linewidth = NA,
                   size = 2,
                   alpha = 0.8) +
#  scale_colour_manual(values=rep(pal_bright[1:6],10)) #+
facet_grid(rows = ~journeyCodeUnq) +
# scale_color_binned(palette = pal_rainbow[2:7])
#scale_color_gradientn(colours = pal_rainbow[2:7], limits = c(0,18))
scale_color_gradient(limits = c(0,18), low = "red", high = "green")



pings_anim <- pings_anim +
  #transition_states(lubridate::as_datetime(time), transition_length = 1, state_length = 1) +
  transition_components(lubridate::as_datetime(time) ) +
  #transition_manual(lubridate::as_datetime(time) ) +
  #transition_time(lubridate::as_datetime(time)) +
  shadow_wake(0.03, size = 1, alpha = 0.4) +
  labs(#title = "pings on 1 route", 
       caption = "Time: {format(frame_time, '%H:%M')}") +
  theme(#plot.title = element_text(hjust = 1, vjust = -10),
        plot.caption = element_text(hjust = 0, vjust = 15, size = 20, face = "bold", colour = "#888"))

  # 
  # labs(title = "Time: {format(frame_time, '%H:%M')}") +
  # theme(plot.title = element_text(hjust = 1, vjust = -10))
  
  #transition_time(time = time) +
  #shadow_mark(past = TRUE)

animate( pings_anim, 
         fps = 20, 
        # nframes = 30, 
         duration = 10)
  
anim_save(file = "gif/pings_anim_multi4.gif",
          plot = get_last_plot(),
          dpi = 320,
          width = 12,
          height = 8,
          units = "cm")



