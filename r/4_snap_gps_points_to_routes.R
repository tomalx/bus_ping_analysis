# Create LINESTRINGs from each point to its nearest location on the line
#nearest_lines <- st_nearest_points(bod_eg1, remix_routes) # if using remix routes

# to generalise this code, could iterate through a list of bus routes
# performing the 'st_nearest_points' function in a loop where the raw BOD points
# are only snapped to their corresponding gtfs route shape.


nearest_lines <- st_union(dc_routes_shape) %>% st_nearest_points(bod_eg1)

## check using the correct route shape: e.g. visualise lines with
# mapview::mapview(nearest_lines)

# Extract the second point from each LINESTRING (i.e., snapped point)
snapped_points <- st_cast(nearest_lines, "POINT")[seq(2, length(nearest_lines)*2, by = 2)]

# Create a new sf object with original attributes and the snapped geometry
bod_snap <- st_sf(
  st_drop_geometry(bod_eg1),   # keeps original attributes
  geometry = snapped_points    # uses snapped points as geometry
)



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
