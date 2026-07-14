
###
# might need to run this first to get dist_m var
# stops_0 <- stop_seq %>% 
#   ungroup() %>% 
#   st_as_sf() %>% 
#   st_transform(4326) %>%
#   filter(direction_id == 0) %>% 
#   #group_by(journeyCode, day, month) %>% 
#   mutate(dist_m = route_distance_calc(., routes = dc_routes, longest_stop_seq = longest_stop_seq, density = 0.5))


split_at_stop <- function(stop_seq = stop_seq , routes,  
                          longest_stop_seq = longest_stop_seq) {
  # Transform to projected CRS for accurate distance (e.g. British National Grid)
  dir <- stop_seq %>% pull(direction_id) %>% unique()
  stop_seq <- filter(stop_seq, direction_id %in% dir) %>% st_as_sf()
  points_sf <- st_transform(stop_seq, 27700)
  #dir <- points_sf$direction_id %>% unique()
  longest_shape <- longest_stop_seq %>% filter(direction_id %in% dir) %>% pull(shape_id)
  line_sf <- routes %>% filter(shape_id %in% longest_shape)
  
  line_sf <- st_transform(line_sf, 27700)
  
  # Get line geometry
  # set crs to WGS84
  route <- st_geometry(line_sf)[[1]] #%>% stplanr::line_segment1(segment_length = c(1000,5000,2000))
  
  #pts <- route %>% st_line_sample(sample = c(0.1,0.2,0.3,0.9))  
    #st_cast("POINT")
  
  #split_route <- st_collection_extract(lwgeom::st_split(route, st_union(pts)),
  #                                  "LINESTRING")
  
  
  breaks <- c(0,0.1,0.2,0.9,1)
  
  # create segments
  segments <- st_sfc(
    lapply(seq_len(length(breaks) - 1), function(i) {
      lwgeom::st_linesubstring(route, breaks[i], breaks[i + 1])
    }),
    crs = st_crs(route)
  )
  
 # length(segments)
  
  # Sample points densely along the line to serve as reference path
 # sampled_points <- st_line_sample(route, density = density) %>% st_cast("POINT")
  
  # Snap each point to the nearest sampled point on the line
#  nearest_index <- st_nearest_feature(points_sf, sampled_points)
  
  # Calculate cumulative distance along the line for sampled points
#  dist_along_line <- c(0, cumsum(st_distance(sampled_points[-length(sampled_points)],
#                                             sampled_points[-1], by_element = TRUE)))
  
  # Assign distance based on nearest sampled point
#  distance_along <- dist_along_line[nearest_index]
  
  return(segments)
}

my_route <- split_at_stop(stop_seq = stops_0,
                          routes = dc_routes,
                          longest_stop_seq = longest_stop_seq
                          )
my_route <- st_cast(my_route, "LINESTRING")

leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(sf::st_as_sf(my_route, crs = 27700) %>% st_transform(4326))

plot(my_route)
plot(sf::st_geometry(my_route), col = c(1,2,5), lwd = 3)

#plot(sf::st_geometry(segments), col = c(1,2,5), lwd = 3)

