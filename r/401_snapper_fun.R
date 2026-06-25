

# make in/out direction lookup dataframe from stop_sequence data frame
in_out_lookup <- stop_seq %>%     # BOD pings use inbound/outbound, whereas
  ungroup() %>%                   # GTFS uses 1/0, 
  filter(stop_sequence == 1) %>% 
  dplyr::select(stop_code, direction_id)



# snap points function

ping_snapper <- function(pings, dir_lookup, route_shape, dir = 1, buff_dist = 100){
  
  pings_2 <- pings %>% 
    left_join(dir_lookup, by = c("originRef" = "stop_code"))
  
  route_shape_buffered <- route_shape %>% 
    st_buffer(buff_dist) %>%
    st_union() %>% 
    st_make_valid() %>% 
    st_transform(4326) 
  
  pings_2 <- pings_2 %>% st_intersection(route_shape_buffered)
  
  # nearest_lines_0 <- st_union(route_shape %>% filter(direction_id == 0)) %>% 
  #   st_nearest_points(pings_2 %>% filter(direction_id == 0))
  nearest_lines_1 <- st_union(route_shape %>% filter(direction_id == dir)) %>% 
    st_nearest_points(pings_2 %>% filter(direction_id == dir))
  
  # snapped_points_0 <- st_cast(nearest_lines_0, "POINT")[seq(1, length(nearest_lines_0)*2, by = 2)]
  snapped_points_1 <- st_cast(nearest_lines_1, "POINT")[seq(1, length(nearest_lines_1)*2, by = 2)]
  
  # pings_0 <- st_sf(
  #   st_drop_geometry(pings_2 %>% filter(direction_id == 0)),   # keeps original attributes
  #   geometry = snapped_points_0    # uses snapped points as geometry
  # )
  pings_1 <- st_sf(
    st_drop_geometry(pings_2 %>% filter(direction_id == dir)),   # keeps original attributes
    geometry = snapped_points_1    # uses snapped points as geometry
  )
  
  return(pings_1)
  
}

ping_sample_n <- 10000

pings_day_1 <- 
  ping_snapper(pings = bod_eg, #%>% slice_sample(n = ping_sample_n), 
                          dir_lookup = in_out_lookup, 
                          route_shape = dc_routes,
                           dir = 1) # %>%
  # slice_sample(n = ping_sample_n)

pings_day_0 <- 
  ping_snapper(pings = bod_eg, #%>% slice_sample(n = ping_sample_n), 
               dir_lookup = in_out_lookup, 
               route_shape = dc_routes,
               dir = 0)  
    

# pings_am <- ping_snapper(pings = bod_eg_am , 
#                          dir_lookup = in_out_lookup, 
#                          route_shape = dc_routes,
#                          dir = 1) %>% 
#   slice_sample(n = ping_sample_n)

# pings_pm <- 
#   ping_snapper(pings = bod_eg_pm, 
#                dir_lookup = in_out_lookup, 
#                route_shape = dc_routes,
#                dir = 1) %>% 
#   slice_sample(n = ping_sample_n)



# bod_eg <- bod_eg %>% 
#   left_join(in_out_lookup, by = c("originRef" = "stop_code"))
# bod_eg_am <- bod_eg_am %>% 
#   left_join(in_out_lookup, by = c("originRef" = "stop_code"))
# 
# # filter out pings that are x metres from routes
# # buffered routes
# dc_routes_buffered <- dc_routes %>% 
#   st_buffer(100) %>%
#   st_union() %>% 
#   st_make_valid() %>% 
#   st_transform(4326) 
# 
# 
# bod_eg <- bod_eg %>% st_intersection(dc_routes_buffered)
# bod_eg_am <- bod_eg_am %>% st_intersection(dc_routes_buffered)
# 
# nearest_lines_0 <- st_union(dc_routes %>% filter(direction_id == 0)) %>% 
#   st_nearest_points(bod_eg %>% filter(direction_id == 0))
# nearest_lines_1 <- st_union(dc_routes %>% filter(direction_id == 1)) %>% 
#   st_nearest_points(bod_eg %>% filter(direction_id == 1))
# 
# nearest_lines_0_am <- st_union(dc_routes %>% filter(direction_id == 0)) %>% 
#   st_nearest_points(bod_eg_am %>% filter(direction_id == 0))
# nearest_lines_1_am <- st_union(dc_routes %>% filter(direction_id == 1)) %>% 
#   st_nearest_points(bod_eg_am %>% filter(direction_id == 1))
# 
# ## check using the correct route shape: e.g. visualise lines with
# # mapview::mapview(nearest_lines)
# 
# # Extract the second point from each LINESTRING (i.e., snapped point)
# snapped_points_0 <- st_cast(nearest_lines_0, "POINT")[seq(1, length(nearest_lines_0)*2, by = 2)]
# snapped_points_1 <- st_cast(nearest_lines_1, "POINT")[seq(1, length(nearest_lines_1)*2, by = 2)]
# 
# snapped_points_0_am <- st_cast(nearest_lines_0_am, "POINT")[seq(1, length(nearest_lines_0_am)*2, by = 2)]
# snapped_points_1_am <- st_cast(nearest_lines_1_am, "POINT")[seq(1, length(nearest_lines_1_am)*2, by = 2)]
# 
# 
# # Create a new sf object with original attributes and the snapped geometry
# bod_snap_0 <- st_sf(
#   st_drop_geometry(bod_eg %>% filter(direction_id == 0)),   # keeps original attributes
#   geometry = snapped_points_0    # uses snapped points as geometry
# )
# bod_snap_1 <- st_sf(
#   st_drop_geometry(bod_eg %>% filter(direction_id == 1)),   # keeps original attributes
#   geometry = snapped_points_1    # uses snapped points as geometry
# )
# 
# bod_snap_0_am <- st_sf(
#   st_drop_geometry(bod_eg_am %>% filter(direction_id == 0)),   # keeps original attributes
#   geometry = snapped_points_0_am    # uses snapped points as geometry
# )
# bod_snap_1_am <- st_sf(
#   st_drop_geometry(bod_eg_am %>% filter(direction_id == 1)),   # keeps original attributes
#   geometry = snapped_points_1_am    # uses snapped points as geometry
# )
# 
# 



 