

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
  dist = 200,
                          routes = dc_routes,
                          longest_stop_seq = longest_stop_seq
)  

my_route <- st_cast(my_route, "LINESTRING")

leaflet::leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>% 
  leaflet::addPolylines(data = my_route %>% st_set_crs(27700) %>% st_transform(4326))

plot(my_route)

my_route <- my_route %>%
  mutate(alt = factor(row_number() %% 2)) %>% 
  mutate(rand = sample(c(1,1,1,1,1,2,3,3,4,4,4), nrow(my_route), replace = TRUE)) %>% 
  mutate(
    rand = case_when(
      rand == 1 ~ "Traffic",
      rand == 2 ~ "Passenger Boarding",
      rand == 3 ~ "Road Width",
      rand == 4 ~ "Junction"
    )
  )

#speed palette
bright <- khroma::color("bright")
bright_pal <- bright(6)[1:6]

p1 <- ggplot(my_route) +
  geom_sf(aes(colour = factor(rand)), linewidth = 4) +
  scale_colour_manual(
    values = c("Traffic" = bright_pal[5], 
               "Passenger Boarding" = bright_pal[2],
               "Road Width" = bright_pal[3], 
               "Junction" = bright_pal[4]
               )
   # guide = "none"
  ) +
  theme_void() +
  ggtitle("cause of delay")

p1 +
guides(colour = guide_legend(nrow = 1)) +
theme(legend.position = "top",
      legend.title = element_blank())

p2 <- ggplot(my_route) +
  geom_sf(aes(colour = alt), linewidth = 2) +
  scale_colour_manual(
    values = c("0" = "grey30", "1" = "orange"),
    guide = "none"
  ) +
  theme_void() +
  ggtitle("250m segments")

p3 <- ggplot(my_route) +
  geom_sf(aes(colour = alt), linewidth = 2) +
  scale_colour_manual(
    values = c("0" = "grey30", "1" = "orange"),
    guide = "none"
  ) +
  theme_void() +
  ggtitle("500m segments")


library(patchwork)

p <- p1 / p2 / p3
p3
