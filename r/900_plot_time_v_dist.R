
## ggplot line plot of bod_snap, grouped by journeyCode. time on x axis, distance on y axis
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)



# function to calculate distance along route
# route_distance_calc <- function(points_sf , line_sf, density = 0.5) {
#   # Transform to projected CRS for accurate distance (e.g. British National Grid)
#   points_sf <- st_transform(points_sf, 27700)
#   line_sf <- st_transform(line_sf, 27700)
#   
#   # Get line geometry
#   # set crs to WGS84
#   route <- st_geometry(line_sf) 
#   
#   # Sample points densely along the line to serve as reference path
#   sampled_points <- st_line_sample(route, density = density) %>% st_cast("POINT")
#   
#   # Snap each point to the nearest sampled point on the line
#   nearest_index <- st_nearest_feature(points_sf, sampled_points)
#   
#   # Calculate cumulative distance along the line for sampled points
#   dist_along_line <- c(0, cumsum(st_distance(sampled_points[-length(sampled_points)],
#                                              sampled_points[-1], by_element = TRUE)))
#   
#   # Assign distance based on nearest sampled point
#   distance_along <- dist_along_line[nearest_index]
#   
#   return(as.numeric(distance_along))
# }

# route_1 <- longest_stop_seq %>% filter(direction_id == 1) %>% pull(shape_id)
# route_eg_1 <- dc_routes %>% filter(shape_id == route_1)



# bod_snap_1 <- pings_day %>%
#   mutate(dist_m = route_distance_calc(., route_eg_1))


# bod_plot <- bod_snap_1 %>% 
#   mutate(journeyCodeUnq = paste0(journeyCode,"-",vehicleId
#                                  )) %>% 
#   #  filter(journeyCode %in% c(#"0802","0818","0832",
#   #                            "0850"
#   # #                           # "0906","0921","0922","0938","0952","0957"
#   #                            )) %>%
#  # filter(day == 9) %>% 
#   filter(dist_m > 134) %>% 
#   #filter(journeyCode %in% c("0630")) %>% 
#   # mutate(time = ymd_hms(time)) %>% 
#   group_by(journeyCode, day, month) %>% 
#   # normalise time to start of journey
#   mutate(time_trip = time - min(time))
# 
# bod_plot <- bod_plot %>% filter_out(time_trip > 7200) %>% 
#   filter(direction_id == 1)
# 
# # line plot of time in secs by distance travelled, colour by journeyCode
# ggplot(bod_plot, aes(y = time_trip, x = dist_m, color = journeyCodeUnq)) +
#   geom_line() +
#   scale_y_time(labels = label_timespan(unit = "mins")) +
#   scale_x_continuous(labels = label_number(scale = 1e-3, suffix = " km")) +
#   labs(title = "Distance travelled by time",
#        y = "Time (s)",
#        x = "Distance (km)",
#        color = "Journey Code") +
#   theme_minimal() +
#   theme(legend.position="none")


ggplot_time_dist <- function(pings, highlight = c(16,17)){
  
  bod_plot <- pings %>% #####
  mutate(journeyCodeUnq = paste0(journeyCode,"-",vehicleId
  )) %>% 
    #  filter(journeyCode %in% c(#"0802","0818","0832",
    #                            "0850"
    # #                           # "0906","0921","0922","0938","0952","0957"
    #                            )) %>%
    # filter(day == 9) %>% 
    filter(dist_m > 134) %>% 
    #filter(journeyCode %in% c("0630")) %>% 
    # mutate(time = ymd_hms(time)) %>% 
    group_by(journeyCode, day, month) %>% 
    # normalise time to start of journey
    mutate(time_trip = time - min(time))
  
  plot <- ggplot(bod_plot, aes(y = time_trip, x = dist_m #, color = journeyCodeUnq
                               )) +
    geom_line(aes(group = journeyCodeUnq),color = "#dddddd", linewidth = 0.1) +
    geom_line(data = bod_plot %>% filter(hour(time) %in% highlight), 
              aes(group = journeyCodeUnq), 
              color = "#bb44bb",
              ) +
    scale_y_time(labels = label_timespan(unit = "mins")) +
    scale_x_continuous(labels = label_number(scale = 1e-3, suffix = " km")) +
    labs(title = "Distance travelled by time",
         y = "Time (s)",
         x = "Distance (km)",
         color = "Journey Code") +
    theme_minimal() +
    theme(legend.position="none")
  
  return(plot)
}


#test_ping

#route length
### put function in here to work out length of the route

dir <- pings$direction_id %>% unique()
longest_shape <- longest_stop_seq %>% filter(direction_id == dir) %>% pull(shape_id)
line_sf <- dc_routes %>% filter(shape_id == longest_shape)
route_length <- st_length(line_sf$geometry %>% st_transform(27700)) %>% as.numeric()

# pings_plot <- ggplot_time_dist(pings = test_pings)
# pings_plot

pings_2 <- pings %>%
  mutate(rank_diff = time_trip_rank - dist_m_rank) %>% 
  group_by(journeyCodeUnq) %>%
  #count()
  filter(!any(time_trip > 120*60)) %>% 
  filter(!any(rank_diff > 100)) %>% 
  filter(!any(rank_diff < -100)) %>% 
  filter_out(dist_m > (route_length - 50))

pings_plot <- ggplot_time_dist(pings = pings_2, highlight = c(7,8,9))
pings_plot


