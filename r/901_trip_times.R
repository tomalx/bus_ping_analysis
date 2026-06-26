# plot time of day versus journey time

ggplot_time_of_day_jny_time <- function(pings){
  
  max_dist <- pings$dist_m %>% max()
  
   plot <- pings %>% #####
   st_drop_geometry() %>% 
   group_by(journeyCodeUnq) %>% 
   filter(any(dist_m < 5)) %>%  # filter any trips that don't start at origin
   filter(any(dist_m > max_dist - 20)) %>%
    filter(!dist_m > max_dist - 20) %>% 
     summarise(time_trip = max(time_trip),
               departure_time = first(originAimedDepatureTime)) %>% 
     filter(!time_trip > 3*60*60)
   
   plot <- plot %>% 
     mutate(departure_time = lubridate::ymd_hms(departure_time)) %>% 
     mutate(departure_time = hms::as_hms(departure_time))
   
   
  # mutate(journeyCodeUnq = paste0(journeyCode,"-",vehicleId
  # )) %>% 
  #   #  filter(journeyCode %in% c(#"0802","0818","0832",
  #   #                            "0850"
  #   # #                           # "0906","0921","0922","0938","0952","0957"
  #   #                            )) %>%
  #   # filter(day == 9) %>% 
  #   filter(dist_m > 134) %>% 
  #   #filter(journeyCode %in% c("0630")) %>% 
  #   # mutate(time = ymd_hms(time)) %>% 
  #   group_by(journeyCode, day, month) %>% 
  #   # normalise time to start of journey
  #   mutate(time_trip = time - min(time))
  
  plot <- ggplot(plot, aes(y = time_trip, x = departure_time #, color = journeyCodeUnq
  )) +
    geom_point(# aes(group = journeyCodeUnq),
               color = pal[2], #linewidth = 0.1
               ) +
    # geom_line(data = bod_plot %>% filter(hour(time) %in% highlight), 
    #           aes(group = journeyCodeUnq), 
    #           color = "#bb44bb",
    # ) +
    scale_x_time(labels = label_timespan(unit = "hours")) +
    #scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " km")) +
    labs(title = "trip times",
         y = "Time (s)",
         x = "origin start time",
         color = "Journey Code") +
    theme_minimal() +
    theme(legend.position="none")
  
  return(plot)
  
}

ggplot_time_of_day_jny_time(pings)
