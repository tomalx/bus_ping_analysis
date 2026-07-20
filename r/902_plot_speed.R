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

route_stop_split <- split_at_stop(stop_seq = stops_1,
                          routes = dc_routes,
                          longest_stop_seq = longest_stop_seq
)

pings

# breaks by a set distance
dist_m_bin_size <- 400 ## size in metres

# breaks by stop to stop distance
seg_breaks <- stops_1$dist_m
seg_names <- route_stop_split$seg_name


pings_filtered <- pings %>% 
  ping_filter(direction = 1, 
              hr_of_day = c(0:23) #, 
             # sample_jnycode = 25
             ) %>%
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
                          #breaks = c(seq(0, max(dist_m), dist_m_bin_size)), 
                          #labels = c(seq(dist_m_bin_size, max(dist_m), dist_m_bin_size)) 
                          )) %>% 
  #remove rows with NA values
  filter(!is.na(dist_m_bin))


#speed palette
incandescent <- khroma::color("incandescent")
incandescent(6)[6:1]
pal_speed <- colorNumeric(palette = incandescent(6)[6:1], domain = 0:12)
pal_iqr <- colorNumeric(palette = incandescent(6)[1:6], domain = 0:8)
pal_sd <- colorNumeric(palette = incandescent(6)[1:6], domain = 0:4)

####
# join pings filtered to geometry of route_stop_split - join by seg_name = dist_m_bin
pings_seg_speed <- pings_filtered %>%
  st_drop_geometry() %>%
  filter(!is.na(ping_speed)) %>% 
  group_by(dist_m_bin) %>% 
  summarise(speed_50 = mean(ping_speed),
            speed_iqr = IQR(ping_speed),
            speed_sd = sd(ping_speed)) %>% 
  left_join(route_stop_split, by = c("dist_m_bin" = "seg_name")) %>% 
  st_as_sf(crs = 27700) %>% 
  st_transform(4326)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(data = pings_seg_speed, color = ~pal_speed(speed_50), opacity = 1 )



pings_plot <- pings_filtered %>% 
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



