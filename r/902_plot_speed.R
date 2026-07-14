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

route_stop_split <- split_at_stop(stop_seq = stops_0,
                          routes = dc_routes,
                          longest_stop_seq = longest_stop_seq
)

pings

# breaks by a set distance
dist_m_bin_size <- 400 ## size in metres

# breaks by stop to stop distance
seg_breaks <- route_stop_split$dist_m_start
seg_names <- stops_0$stop_name[2:length(stops_0$stop_name)]


pings_plot <- pings %>% 
  ping_filter(direction = 0, 
              hr_of_day = c(0:23) , 
              sample_jnycode = 25) %>%
  group_by(journeyCodeUnq) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 200) %>% 

 # filter(stringr::str_starts(journeyCode, pattern = "08")) %>% 
  ping_speed() %>% 
  # mutate(dist_m_bin =  cut(dist_m, breaks = 2)) 
  mutate(dist_m_bin = cut(dist_m,
                          breaks = seg_breaks,
                          labels = seg_names
                          #breaks = c(seq(0, max(dist_m), dist_m_bin_size)),   ### Breaks by equal size (option)
                          #labels = c(seq(dist_m_bin_size, max(dist_m), dist_m_bin_size)) 
                          )) %>% 
  #remove rows with NA values
  filter(!is.na(dist_m_bin))

pings_plot <- pings_plot %>% 
  group_by(journeyCodeUnq, dist_m_bin) %>% 
  mutate(sample_size = n()) %>%
  mutate(bin_speed = sum(ping_speed)/n()) %>% 
  ungroup()


pings_plot %>% ggplot(
  aes(x = dist_m_bin, y = bin_speed) #, group = journeyCodeUnq)
) +
  geom_line(alpha = 0.1, stroke = NA, size = 1) +
  theme_minimal()


## x binned
pings_plot %>% ggplot(
  aes(x = dist_m_bin, y = ping_speed)
) +
  geom_boxplot() +
  ylim(c(0,25)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_x_binned()

pings_plot %>% ggplot(
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



