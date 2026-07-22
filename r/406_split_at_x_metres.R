

# function that splits route every x metres

split_every_x_metres <- function(routes,
                                 dir = c(0,1),
                                 dist = 250,
                                 longest_stop_seq = longest_stop_seq){
  

  
  longest_shape <- longest_stop_seq %>% filter(direction_id %in% dir) %>% pull(shape_id)
  line_sf <- routes %>% filter(shape_id %in% longest_shape)
  
  line_sf <- st_transform(line_sf, 27700)
  # Get line geometry
  route <- st_geometry(line_sf)[[1]]
  
  num_breaks <- st_length(route) %/% dist# %/% integer divison operator
  
  breaks <- c(0:num_breaks * dist ,  st_length(route) )
  breaks_norm <- breaks/st_length(route)
  
  # create segments
  segments <- st_sfc(
    lapply(seq_len(length(breaks_norm) - 1), function(i) {
      lwgeom::st_linesubstring(route, breaks_norm[i], breaks_norm[i + 1])
    }),
    crs = st_crs(route)
  )
  
  df <- data.frame(start_seg = head(breaks, -1),
                   end_seg = breaks[-1] )
  df <- df %>% 
    mutate(seg_name = paste0(start_seg,
                             " - ",
                             round(end_seg,0)
    )) 
  
  sf_obj <- st_sf(df, geom = segments)
  
  return(sf_obj)
  
}




my_route <- split_every_x_metres(
                          routes = dc_routes,
                          longest_stop_seq = longest_stop_seq
)  

my_route <- st_cast(my_route, "LINESTRING")

leaflet::leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>% 
  leaflet::addPolylines(data = my_route %>% st_set_crs(27700) %>% st_transform(4326))

plot(my_route)
plot(sf::st_geometry(my_route), col = c(1,2,5), lwd = 3)
