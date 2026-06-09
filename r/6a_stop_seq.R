# stop seq

routes_select <- c("1")

routes_filtered <- gtfs_sf$routes %>% filter(route_short_name %in% routes_select)

calendar_filtered <- gtfs_sf$calendar %>% filter(  tuesday == 1 &     # filter for mon-fri service pattern
                                                     wednesday == 1 &   # tue,wed,thu accounts for some
                                                     thursday == 1) %>% # unique fri service_ids
  pull(service_id)

# filter trips - extract only weekday services and selected routes
trips_filtered <- gtfs_sf$trips %>% 
  filter(route_id %in% routes_filtered$route_id) %>% 
  filter(service_id %in% calendar_filtered)
rm(calendar_filtered)

bus_stats <- gtfs_sf$stop_times %>% inner_join(trips_filtered, by = "trip_id") #%>% # req trips_filtered obj from filter_gtfs.R
rm(trips_filtered)

stop_seq <- bus_stats %>% 
  group_by(shape_id, 
           route_id, 
           stop_sequence,
           direction_id,
           stop_id) %>% 
  summarise()
# get the stop code for each stop id
stop_seq <- stop_seq %>% left_join(gtfs_sf$stops %>% 
                                     select(-wheelchair_boarding, 
                                            -location_type,
                                            -parent_station,
                                            -platform_code,
                                            -zone_id))
stop_seq <- stop_seq %>% left_join(gtfs_sf$routes %>% 
                                     select(route_id, 
                                            route_short_name, 
                                            route_long_name))


###

unq_long_name <- stop_seq_trip_lookup_all_stops$route_long_name %>% unique
unq_long_name
unq_long_name <- unq_long_name[2]

stop_seq_trip_lookup_all_stops <- stop_seq_trip_lookup_all_stops %>% 
  filter(route_long_name == unq_long_name)
unq_route_id <- stop_seq_trip_lookup_all_stops$route_id %>% unique()
bus_stats <- bus_stats %>% filter(route_id %in% unq_route_id)
