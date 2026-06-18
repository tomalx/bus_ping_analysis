
## ggplot line plot of bod_snap, grouped by journeyCode. time on x axis, distance on y axis
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

bod_eg1 <- bod_eg1 %>%
  filter(journeyCode %in% c("0817","0847","0902","0918", "0933")) %>% 
  #filter(journeyCode %in% c("0630")) %>% 
  mutate(time = ymd_hms(time)) %>% 
  group_by(journeyCode) %>% 
  # normalise time to start of journey
  mutate(time_trip = time - min(time)) %>% 
  ungroup()

# line plot of time in secs by distance travelled, colour by journeyCode
ggplot(bod_eg1, aes(x = time_trip, y = dist_m, color = journeyCode)) +
  geom_line() +
  scale_x_time(labels = label_timespan(unit = "secs")) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " km")) +
  labs(title = "Distance travelled by time",
       x = "Time (s)",
       y = "Distance (km)",
       color = "Journey Code") +
  theme_minimal()
