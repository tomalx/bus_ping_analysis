#import_basemap_routes

# **ALTERNATIVE** to import_remix_routes.R

# choose basemap gtfs file

## this gets a 'snap shot' of routes as they are at one point in time
## for one quarter of a year
## doesn't take account of routes that are diverted or have changed
## during the quarter.

gtfs <- tidytransit::read_gtfs(choose.files())
gtfs <- gtfstools::filter_by_route_type(gtfs, route_type = 3)
gtfs_sf <- gtfs %>% tidytransit::gtfs_as_sf(crs = 27700)
gtfs_sf <- gtfs_sf %>% gtfstools::convert_time_to_seconds()
rm(gtfs)

# get gtfs shape(s) for specified route (and direction)
# dc = datacutter

dc_routes <- gtfs_sf$routes %>% filter(route_short_name %in% c(route_number))

cat("select specific route(s): \n")
for( n in 1:nrow(dc_routes)){
  cat(n," ---->    ",dc_routes$route_short_name[n]," ", dc_routes$route_long_name[n],"\n")
}

dc_routes <- dc_routes[1]  ## WARNING ## need to specify row(s) in square brackets

dc_routes <- dc_routes %>% 
  left_join(gtfs_sf$trips, by = "route_id") %>% 
  dplyr::select(route_id, route_short_name, route_long_name, shape_id, direction_id) %>% 
  distinct() %>% 
  left_join(gtfs_sf$shape, by = c("shape_id")) 

dc_routes <- dc_routes %>% #filter(direction_id == 1) %>%
  st_as_sf(crs = 27700) %>% 
  st_transform(4326)

