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



pings

dist_m_bin_size <- 50 ## size in metres


pings_plot <- pings %>% 
  ping_filter(direction = 1, sample_jnycode = 10) %>% 

 # filter(stringr::str_starts(journeyCode, pattern = "08")) %>% 
  ping_speed() %>% 
  # mutate(dist_m_bin =  cut(dist_m, breaks = 2)) 
  mutate(dist_m_bin = cut(dist_m, 
                          breaks = c(seq(0, max(dist_m), dist_m_bin_size)), 
                          labels = c(seq(dist_m_bin_size, max(dist_m), dist_m_bin_size)) )) 

pings_plot <- pings_plot %>% 
  group_by(journeyCodeUnq, dist_m_bin) %>% 
  mutate(sample_size = n()) %>%
  mutate(bin_speed = sum(ping_speed)/n()) %>% 
  ungroup()


pings_plot %>% ggplot(
  aes(x = dist_m_bin, y = bin_speed) #, group = journeyCodeUnq)
) +
  geom_col(alpha = 0.1, stroke = NA, size = 1) +
  theme_minimal()


## x binned
pings_plot %>% ggplot(
  aes(x = dist_m_bin, y = ping_speed)
) +
  geom_boxplot()# +
  #scale_x_binned()

pings_plot %>% ggplot(
  aes(x = dist_m, 
      y = as.numeric(time_trip) , 
      group = journeyCodeUnq
      )
) +
  geom_line(alpha = 0.5, stroke = NA, size = 0.1, color = "#ababab") +
  theme_minimal() +
  geom_vline(xintercept = stops_1$dist_m)


# ideas
# make a bin_route function which splits the route line dc_route shape into
# segments which are the same size as the dist_m bins
# this can then be mapped chloropleth rout line map broken every 50m - 
# coloured by avg speed, speed variance, iqr



