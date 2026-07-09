## plot ping speed

pings_plot <- pings %>% 
  filter(direction_id == 0) %>% 
 # filter(stringr::str_starts(journeyCode, pattern = "08")) %>% 
  ping_speed() %>% 
  # mutate(dist_m_bin =  cut(dist_m, breaks = 2)) 
  mutate(dist_m_bin = ntile(dist_m, n = 20))

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
