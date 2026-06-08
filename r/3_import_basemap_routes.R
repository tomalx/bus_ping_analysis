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

dc_routes <- gtfs_sf$routes %>% filter(route_short_name %in% c("1"))

cat("select specific route(s): \n")
for( n in 1:nrow(dc_routes)){
  cat(n," ---- ",dc_routes$route_short_name[n]," ", dc_routes$route_long_name[n],"\n")
}

dc_routes <- dc_routes[2]  ## WARNING ## need to specify row(s) in square brackets

dc_routes <- dc_routes %>% 
  left_join(gtfs_sf$trips, by = "route_id") %>% 
  select(route_id, route_short_name, route_long_name, shape_id, direction_id) %>% 
  distinct() %>% 
  left_join(gtfs_sf$shape, by = c("shape_id")) 

dc_routes_shape <- dc_routes %>% #filter(direction_id == 1) %>%
  st_as_sf(crs = 27700) %>% 
  st_transform(4326)


# library(leaflet)
#   ## each row of data is separate layer on leaflet map
# map <- leaflet::leaflet() %>% 
#   leaflet::addProviderTiles("CartoDB.Positron")
#  
#  brew_color_pal <- RColorBrewer::brewer.pal(8,"Set1")
#    
#    
#   for(i in 1:nrow(dc_routes_shape)){
#   map <- map %>% 
#   leaflet::addPolylines(data = dc_routes_shape[i,],
#                         color = brew_color_pal[i], 
#                         stroke = FALSE,
#                         weight = 10,
#                         popup = paste0("direction: ",dc_routes_shape$direction_id[i], " <br>", 
#                                        dc_routes_shape$shape_id[i]),
#                         group = as.character(i),
#                         fillOpacity = 0.1,
#                         opacity = 0.1)
#     
#   }
#     
#     
#   
# map  %>% addLayersControl(baseGroups = as.character(1:nrow(dc_routes_shape)),
#                           options = layersControlOptions(collapsed = FALSE))
# 
# #dc_routes_selected <- dc_routes_shape %>% filter(direction_id == 1)
