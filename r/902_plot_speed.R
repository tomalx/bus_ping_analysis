## plot ping speed

pings_plot <- pings %>% 
  ping_filter() %>% 
  filter(direction_id == 0) %>% 
 # filter(stringr::str_starts(journeyCode, pattern = "08")) %>% 
  ping_speed() %>% 
  # mutate(dist_m_bin =  cut(dist_m, breaks = 2)) 
  mutate(dist_m_bin = ntile(dist_m, n = 200))

pings_plot$dist_m_bin <- format(pings_plot$dist_m_bin, scientific = FALSE)

pings_plot %>% ggplot(
  aes(x = dist_m_bin, y = ping_speed, group = journeyCodeUnq)
) +
  geom_point(alpha = 0.1, stroke = NA, size = 1)


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
  geom_vline(xintercept = c(1000))

