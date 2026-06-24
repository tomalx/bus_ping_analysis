# stop sequence snapper

# aim:
#   - group by trip_id
#   - for each trip, make sure pings are in order of distance from origin
#   - if out of sequence - remove erroneous pings or...
#   - delete the pings for the entire trip.
#   - could also detelt pings if entire trip isn't represented.
#   - ALSO rebase trip start time to 0 seconds - to account for pings sent before 
#   bus has started the trip (e.g. layover between trips) - 

# assume the 401 snapper function has already been run on the ping data

# select unique origin - destination trips

unique_trip_OD <- function(pings){
  
  pings_2 <- pings %>% mutate(od_name = paste0(originName,"_to_",destinationName))
  return(pings_2)
}

test_pings <- pings_day %>% unique_trip_OD()
#glimpse(test_pings)

test_pings <- test_pings %>%
  #group_by(journeyCode, day, month) %>% 
  mutate(dist_m = route_distance_calc(., route_eg_1))

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
