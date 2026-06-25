# stop sequence snapper

# aim:
#   - group by trip_id
#   - for each trip, make sure pings are in order of distance from origin
#   - if out of sequence - remove erroneous pings or...
#   - delete the pings for the entire trip.
#   - could also delete pings if entire trip isn't represented.
#   - ALSO rebase trip start time to 0 seconds - to account for pings sent before 
#   bus has started the trip (e.g. layover between trips) - 

# assume the 401 snapper function has already been run on the ping data

# select unique origin - destination trips

unique_trip_OD <- function(pings){
  
  pings_2 <- pings %>% mutate(od_name = paste0(originName,"_to_",destinationName))
  return(pings_2)
}

route_distance_calc <- function(pings , routes,  longest_stop_seq = longest_stop_seq, density = 0.5) {
  # Transform to projected CRS for accurate distance (e.g. British National Grid)
  points_sf <- st_transform(pings, 27700)
  dir <- points_sf$direction_id %>% unique()
  longest_shape <- longest_stop_seq %>% filter(direction_id == dir) %>% pull(shape_id)
  line_sf <- routes %>% filter(shape_id == longest_shape)
  
  line_sf <- st_transform(line_sf, 27700)
  
  # Get line geometry
  # set crs to WGS84
  route <- st_geometry(line_sf) 
  
  # Sample points densely along the line to serve as reference path
  sampled_points <- st_line_sample(route, density = density) %>% st_cast("POINT")
  
  # Snap each point to the nearest sampled point on the line
  nearest_index <- st_nearest_feature(points_sf, sampled_points)
  
  # Calculate cumulative distance along the line for sampled points
  dist_along_line <- c(0, cumsum(st_distance(sampled_points[-length(sampled_points)],
                                             sampled_points[-1], by_element = TRUE)))
  
  # Assign distance based on nearest sampled point
  distance_along <- dist_along_line[nearest_index]
  
  return(as.numeric(distance_along))
}


test_pings <- pings_day_0 %>% unique_trip_OD()
#glimpse(test_pings)

test_pings <- test_pings %>%
  #group_by(journeyCode, day, month) %>% 
  mutate(dist_m = route_distance_calc(., routes = dc_routes, longest_stop_seq = longest_stop_seq, density = 0.5))

test_pings <- test_pings %>% 
  group_by(journeyCode, day, month) %>% 
  # normalise time to start of journey
  mutate(time_trip = time - min(time)) %>% 
  arrange(dist_m) %>% 
  mutate(dist_m_rank = row_number()) %>% 
  arrange(time_trip) %>% 
  mutate(time_trip_rank = row_number())

unq_od_names <- test_pings$od_name %>% unique()
unq_od_names
#test_pings %>% st_drop_geometry() %>% count(od_name)
#test_pings$directionRef %>% unique()
# select od_name
unq_od_names <- unq_od_names[1]
test_pings <- test_pings %>% filter(od_name %in% unq_od_names)

test_pings <- test_pings %>% 
  mutate(journeyCodeUnq = paste0(journeyCode,"-",vehicleId))



st_length(route_eg_1$geometry %>% st_transform(27700))

#### split route into segments
distance = units::set_units(1, degrees)
u = units::set_units(seq(0, 10000, 500), metres)
points <- st_line_interpolate(route_eg_1$geometry %>% st_transform(27700) , u, normalized = FALSE)
points <- points %>% st_transform(4326)

leaflet() %>% 
  #addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(data = points, color = "#882255") %>% 
  addPolylines(data = route_eg_1, color = "#66aaaa")
