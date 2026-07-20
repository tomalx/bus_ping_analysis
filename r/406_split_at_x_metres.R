

# function that splits route every x metres

split_every_x_metres <- function(routes,
                                 dist = 250,
                                 longest_stop_seq = longest_stop_seq){
  

  
  longest_shape <- longest_stop_seq %>% filter(direction_id %in% dir) %>% pull(shape_id)
  line_sf <- routes %>% filter(shape_id %in% longest_shape)
  
  line_sf <- st_transform(line_sf, 27700)
  # Get line geometry
  route <- st_geometry(line_sf)[[1]]
  
  num_breaks <- st_length(route) %/% dist# %/% integer divison operator
  
  breaks <- c(0:num_breaks * dist ,  st_length(route) )
  
  # create segments
  segments <- st_sfc(
    lapply(seq_len(length(breaks) - 1), function(i) {
      lwgeom::st_linesubstring(route, breaks[i], breaks[i + 1])
    }),
    crs = st_crs(route)
  )
  
  df <- data.frame(start_seg = c(0,breaks, ),
                   end_stop = stop_seq$stop_name[2:nrow(stop_seq)],
                   dist_m_start = stop_seq$dist_m[1:nrow(stop_seq)-1],
                   dist_m_end = stop_seq$dist_m[2:nrow(stop_seq)])
  df <- df %>% 
    mutate(seg_name = paste0(word(start_stop, start = 1, sep = ","),
                             " - ",
                             word(end_stop, start = 1, sep = ",")
    )) %>% 
    mutate(seg_length = dist_m_end - dist_m_start)
  
  return(segments)
  
}


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
  route <- st_geometry(line_sf)[[1]] #%>% stplanr::line_segment1(segment_length = c(1000,5000,2000))
  
  breaks <- stop_seq$dist_m/max(stop_seq$dist_m)
  
  # create segments
  segments <- st_sfc(
    lapply(seq_len(length(breaks) - 1), function(i) {
      lwgeom::st_linesubstring(route, breaks[i], breaks[i + 1])
    }),
    crs = st_crs(route)
  )
  
  df <- data.frame(start_stop = stop_seq$stop_name[1:nrow(stop_seq)-1],
                   end_stop = stop_seq$stop_name[2:nrow(stop_seq)],
                   dist_m_start = stop_seq$dist_m[1:nrow(stop_seq)-1],
                   dist_m_end = stop_seq$dist_m[2:nrow(stop_seq)])
  df <- df %>% 
    mutate(seg_name = paste0(word(start_stop, start = 1, sep = ","),
                             " - ",
                             word(end_stop, start = 1, sep = ",")
    )) %>% 
    mutate(seg_length = dist_m_end - dist_m_start)
  
  sf_obj <- st_sf(df, geom = segments)
  
  return(sf_obj)
}

my_route <- split_at_stop(stop_seq = stops_0,
                          routes = dc_routes,
                          longest_stop_seq = longest_stop_seq
)  