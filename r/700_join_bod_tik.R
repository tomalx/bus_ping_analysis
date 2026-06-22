# join bod and tik

bod_bind <- bod_eg1 %>% select(datetime = time, geometry) %>% 
  mutate(source = "bod")

first_ping <- bod_bind$datetime %>% min()
last_ping <- bod_bind$datetime %>% max()

pax_bind <- pax %>% select(datetime, geometry) %>%
  filter(datetime >= first_ping & datetime <= last_ping) %>% 
  mutate(source = "tik") %>% 
  st_as_sf() %>% 
  st_transform(4326)

bod_tik <- bind_rows(bod_bind, pax_bind)

