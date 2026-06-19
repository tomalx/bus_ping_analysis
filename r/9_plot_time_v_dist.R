
## ggplot line plot of bod_snap, grouped by journeyCode. time on x axis, distance on y axis
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)


# function to calculate distance along route
route_distance_calc <- function(points_sf , line_sf, density = 0.5) {
  # Transform to projected CRS for accurate distance (e.g. British National Grid)
  points_sf <- st_transform(points_sf, 27700)
  line_sf <- st_transform(line_sf, 27700)
  
  # Get line geometry
  # set crs to WGS84
  route <- st_geometry(line_sf) 
  
  # Sample points densely along the line to serve as reference path
  sampled_points <- st_line_sample(route, density = density) %>% st_cast("POINT")
  
  # Snap each point to the nearest sampled point on the line
  nearest_index <- st_nearest_feature(points_sf, sampled_points)
  
  # Calculate cumulative distance along the line for sampled points
  dist_along_line <- c(0, cumsum(st_distance(sampled_points[-length(sampled_points)],
                                             sampled_points[-1], by_element = TRUE)))
  
  # Assign distance based on nearest sampled point
  distance_along <- dist_along_line[nearest_index]
  
  return(as.numeric(distance_along))
}

route_0 <- longest_stop_seq %>% filter(direction_id == 0) %>% pull(shape_id)
route_eg_0 <- dc_routes %>% filter(shape_id == route_0)

bod_snap_0 <- bod_snap_0 %>%
  mutate(dist_m = route_distance_calc(., route_eg_0))


bod_plot <- bod_snap_0 %>%
  mutate(journeyCodeUnq = paste0(journeyCode,"-",vehicleId
                                 )) %>% 
  # filter(journeyCode %in% c(#"0802","0818","0832",
  #                           "0850"
  #                           # "0906","0921","0922","0938","0952","0957"
  #                           )) %>%
  filter(day == 10) %>% 
  filter(dist_m > 1) %>% 
  #filter(journeyCode %in% c("0630")) %>% 
  # mutate(time = ymd_hms(time)) %>% 
  # group_by(journeyCode) %>% 
  # normalise time to start of journey
  #mutate(time_trip = time - min(time)) %>% 
  ungroup()

# line plot of time in secs by distance travelled, colour by journeyCode
ggplot(bod_plot, aes(y = time_trip, x = dist_m, color = journeyCodeUnq)) +
  geom_line() +
  scale_y_time(labels = label_timespan(unit = "mins")) +
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = " km")) +
  labs(title = "Distance travelled by time",
       y = "Time (s)",
       x = "Distance (km)",
       color = "Journey Code") +
  theme_minimal()
