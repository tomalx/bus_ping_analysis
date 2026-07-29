## plot ping speed

# stops with dist_m
stops_0 <- stop_seq %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  st_transform(4326) %>%
  filter(direction_id == 0) %>% 
  #group_by(journeyCode, day, month) %>% 
  mutate(dist_m = route_distance_calc(., routes = dc_routes, longest_stop_seq = longest_stop_seq, density = 0.5))

stops_1 <- stop_seq %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  st_transform(4326) %>%
  filter(direction_id == 1) %>% 
  #group_by(journeyCode, day, month) %>% 
  mutate(dist_m = route_distance_calc(., routes = dc_routes, longest_stop_seq = longest_stop_seq, density = 0.5))

route_split_1 <- split_at_stop(stop_seq = stops_1,
                          routes = dc_routes,
                          longest_stop_seq = longest_stop_seq
)

route_split_0 <- split_at_stop(stop_seq = stops_0,
                               routes = dc_routes,
                               longest_stop_seq = longest_stop_seq
)

# breaks by a set distance
seg_size <- 100 ## size in metres

route_split_1 <- split_every_x_metres(
  dir = 1,
  routes = dc_routes,
  dist = seg_size,
  longest_stop_seq = longest_stop_seq
)  


# breaks by stop to stop distance - USE WITH SPLIT AT STOP
seg_break_0 <- stops_0$dist_m
seg_name_0 <- route_split_0$seg_name
seg_break_1 <- stops_1$dist_m
seg_name_1 <- route_split_1$seg_name

# breaks by stop to stop distance - USE WITH SPLIT AT X METRES
seg_break_1 <- c(route_split_1$start_seg, max(route_split_1$end_seg))
seg_name_1 <- route_split_1$seg_name
seg_break_0 <- c(route_split_0$start_seg, max(route_split_0$end_seg))
seg_name_0 <- route_split_0$seg_name


hours_of_day <- c(7:9)

## outbound
pings_filtered_0 <- pings %>% 
  ping_filter(direction = 0, 
              hr_of_day = c(8,9) #, 
             # sample_jnycode = 25
             ) %>%
  group_by(journeyCodeUnq,day) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 80) %>% 

 # filter(stringr::str_starts(journeyCode, pattern = "08")) %>% 
  ping_speed() %>% 
  # mutate(dist_m_bin =  cut(dist_m, breaks = 2)) 
  mutate(seg_name = cut(dist_m,
                          breaks = seg_break_0,
                          labels = seg_name_0
                          #breaks = c(seq(0, max(dist_m), dist_m_bin_size)), 
                          #labels = c(seq(dist_m_bin_size, max(dist_m), dist_m_bin_size)) 
                          )) %>% 
  #remove rows with NA values
  filter(!is.na(seg_name)) %>% 
  filter(prev_ping_dist < 600) %>%  # remove pings that are too far (distance) apart
  filter(prev_ping_time > 5) %>% # remove pings that are too close together (time)
  group_by(seg_name, journeyCodeUnq, day) %>% 
  mutate(seg_ping_count = n()) %>% 
  filter_out(seg_ping_count > 20 & ping_speed <1) %>% 
  ungroup()
  
# inbound
pings_filtered_1 <- pings %>% 
  ping_filter(direction = 1, 
              hr_of_day = c(8,9) #, 
              # sample_jnycode = 25
  ) %>%
  group_by(journeyCodeUnq,day) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 80) %>% 
  
  # filter(stringr::str_starts(journeyCode, pattern = "08")) %>% 
  ping_speed() %>% 
  # mutate(dist_m_bin =  cut(dist_m, breaks = 2)) 
  mutate(seg_name = cut(dist_m,
                        breaks = seg_break_1,
                        labels = seg_name_1
                        #breaks = c(seq(0, max(dist_m), dist_m_bin_size)), 
                        #labels = c(seq(dist_m_bin_size, max(dist_m), dist_m_bin_size)) 
  )) %>% 
  #remove rows with NA values
  filter(!is.na(seg_name)) %>% 
  filter(prev_ping_dist < 600) %>%  # remove pings that are too far apart
  filter(prev_ping_time > 5) %>% 
  group_by(seg_name, journeyCodeUnq, day) %>% 
  mutate(seg_ping_count = n()) %>% 
  filter_out(seg_ping_count > 20 & ping_speed <1) %>% 
  ungroup()



####
# join pings filtered to geometry of route_stop_split - join by seg_name = dist_m_bin
# outbound
pings_seg_speed_0 <- pings_filtered_0 %>%
  st_drop_geometry() %>%
  filter(!is.na(ping_speed)) %>% 
  group_by(seg_name) %>% 
  summarise(speed_50 = mean(ping_speed),
            speed_iqr = IQR(ping_speed),
            speed_sd = sd(ping_speed)) %>% 
  left_join(route_split_0, by = c("seg_name" = "seg_name")) %>%
  mutate(direction_id = 0) %>% 
  st_as_sf(crs = 27700) %>% 
  st_transform(4326)

# inbound
pings_seg_speed_1 <- pings_filtered_1 %>%
  st_drop_geometry() %>%
  filter(!is.na(ping_speed)) %>% 
  group_by(seg_name) %>% 
  summarise(speed_50 = mean(ping_speed),
            speed_iqr = IQR(ping_speed),
            speed_sd = sd(ping_speed)) %>% 
  left_join(route_split_1, by = c("seg_name" = "seg_name")) %>% 
  mutate(direction_id = 1) %>% 
  st_as_sf(crs = 27700) %>% 
  st_transform(4326)

# join inbound and outbound segs
pings_seg_both_dir <- rbind(pings_seg_speed_0, pings_seg_speed_1)

##########################

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(data = pings_seg_speed_1, color = ~pal_speed(speed_50), opacity = 1 ) #%>% 
  #addPolylines(data = pings_seg_speed, color = ~pal_speed(speed_50), opacity = 1 ) 



pings_plot <- pings_filtered %>% 
  group_by(journeyCodeUnq, seg_name) %>% 
  mutate(sample_size = n()) %>%
  mutate(bin_speed = sum(ping_speed)/n()) %>% 
  ungroup()


pings_plot %>% ggplot(
  aes(x = seg_name, y = bin_speed) #, group = journeyCodeUnq)
) +
  geom_line(alpha = 0.1, stroke = NA, size = 1) +
  theme_minimal()


## x binned
boxplot <- pings_plot %>% ggplot(
  aes(x = seg_name, y = ping_speed)
) +
  geom_boxplot() +
  ylim(c(0,25)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
 # scale_x_reverse() +
  coord_flip() +
  theme_void()
  #scale_x_binned()

boxplot

sample_journeys <-  pings_plot %>% 
  pull(journeyCodeUnq) %>% 
  unique() %>% 
  sample(5)

pings_plot %>%
  filter(journeyCodeUnq %in% sample_journeys) %>% 
  ggplot(
  aes(x = dist_m, 
      y = as.numeric(time_trip) , 
      group = journeyCodeUnq
      )
) +
  geom_line(alpha = 1, size = 0.1, color = "#444444") +
  theme_classic() +
  geom_vline(xintercept = stops_1$dist_m, alpha = 0.5, size = 0.5, color = "#ababab")


# ideas
# make a bin_route function which splits the route line dc_route shape into
# segments which are the same size as the dist_m bins
# this can then be mapped chloropleth rout line map broken every 50m - 
# coloured by avg speed, speed variance, iqr



