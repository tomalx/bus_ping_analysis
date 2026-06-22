# Create LINESTRINGs from each point to its nearest location on the line
#nearest_lines <- st_nearest_points(bod_eg1, remix_routes) # if using remix routes

# to generalise this code, could iterate through a list of bus routes
# performing the 'st_nearest_points' function in a loop where the raw BOD points
# are only snapped to their corresponding gtfs route shape.

### need to run 6a_stop_seq first
## TO DO snap bod_eg points to correct route geometry

in_out_lookup <- stop_seq %>%     # BOD pings use inbound/outbound, whereas
  ungroup() %>%                   # GTFS uses 1/0, 
  filter(stop_sequence == 1) %>% 
  dplyr::select(stop_code, direction_id)

bod_eg <- bod_eg %>% 
  left_join(in_out_lookup, by = c("originRef" = "stop_code"))
bod_eg_am <- bod_eg_am %>% 
  left_join(in_out_lookup, by = c("originRef" = "stop_code"))

# filter out pings that are x metres from routes
  # buffered routes
dc_routes_buffered <- dc_routes %>% 
  st_buffer(100) %>%
  st_union() %>% 
  st_make_valid() %>% 
  st_transform(4326) 


bod_eg <- bod_eg %>% st_intersection(dc_routes_buffered)
bod_eg_am <- bod_eg_am %>% st_intersection(dc_routes_buffered)

nearest_lines_0 <- st_union(dc_routes %>% filter(direction_id == 0)) %>% 
  st_nearest_points(bod_eg %>% filter(direction_id == 0))
nearest_lines_1 <- st_union(dc_routes %>% filter(direction_id == 1)) %>% 
  st_nearest_points(bod_eg %>% filter(direction_id == 1))

nearest_lines_0_am <- st_union(dc_routes %>% filter(direction_id == 0)) %>% 
  st_nearest_points(bod_eg_am %>% filter(direction_id == 0))
nearest_lines_1_am <- st_union(dc_routes %>% filter(direction_id == 1)) %>% 
  st_nearest_points(bod_eg_am %>% filter(direction_id == 1))

## check using the correct route shape: e.g. visualise lines with
# mapview::mapview(nearest_lines)

# Extract the second point from each LINESTRING (i.e., snapped point)
snapped_points_0 <- st_cast(nearest_lines_0, "POINT")[seq(1, length(nearest_lines_0)*2, by = 2)]
snapped_points_1 <- st_cast(nearest_lines_1, "POINT")[seq(1, length(nearest_lines_1)*2, by = 2)]

snapped_points_0_am <- st_cast(nearest_lines_0_am, "POINT")[seq(1, length(nearest_lines_0_am)*2, by = 2)]
snapped_points_1_am <- st_cast(nearest_lines_1_am, "POINT")[seq(1, length(nearest_lines_1_am)*2, by = 2)]


# Create a new sf object with original attributes and the snapped geometry
bod_snap_0 <- st_sf(
  st_drop_geometry(bod_eg %>% filter(direction_id == 0)),   # keeps original attributes
  geometry = snapped_points_0    # uses snapped points as geometry
)
bod_snap_1 <- st_sf(
  st_drop_geometry(bod_eg %>% filter(direction_id == 1)),   # keeps original attributes
  geometry = snapped_points_1    # uses snapped points as geometry
)

bod_snap_0_am <- st_sf(
  st_drop_geometry(bod_eg_am %>% filter(direction_id == 0)),   # keeps original attributes
  geometry = snapped_points_0_am    # uses snapped points as geometry
)
bod_snap_1_am <- st_sf(
  st_drop_geometry(bod_eg_am %>% filter(direction_id == 1)),   # keeps original attributes
  geometry = snapped_points_1_am    # uses snapped points as geometry
)

# remove pings that are 


#### leaflet map ####



# map <-  leaflet::leaflet() %>%
#   addTiles(group = "OSM") %>% 
#   leaflet::addProviderTiles("CartoDB.Positron", group = "carto")
# 
# 
# for(i in 1:nrow(dc_routes_shape)){
#   map <- map %>% 
#     leaflet::addPolylines(data = dc_routes_shape[i,],
#                           color = brew_color_pal[i], 
#                           stroke = TRUE,
#                           weight = 10,
#                           popup = paste0("direction: ",dc_routes_shape$direction_id[i], " <br>", 
#                                          dc_routes_shape$shape_id[i]),
#                           group = as.character(i),
#                           fillOpacity = 0.6,
#                           opacity = 0.6)
#   
# }
# 
# map <- map %>% addPolylines(data = nearest_lines,
#                      weight = 1,
#                      color = "#444444",
#                      opacity = 0.8,
#                      #dashArray = "2,2",
#                      group = "nearest lines")
# 
# map <- map %>% addCircles(data = bod_eg1,
#                          radius = 0.5,
#                          weight = 0.3,
#                          color = "black",
#                          fillOpacity = 0,
#                          group = "original points")
# 
# map  %>% addLayersControl(baseGroups = c("OSM", "carto"),
#                           overlayGroups = c(as.character(1:nrow(dc_routes_shape)),"nearest lines","original points"),
#                           options = layersControlOptions(collapsed = FALSE))
