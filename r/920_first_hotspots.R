hotspots <- st_read(choose.files())

#speed palette
incandescent <- khroma::color("incandescent")
incandescent(6)[6:1]
pal_delay <- colorNumeric(palette = incandescent(6)[6:1], domain = 0:350)

names(hotspots)[2:9] <- word(names(hotspots)[2:9], sep = "_", 2)
names(hotspots)

hotspots <- st_as_sf(hotspots, crs = 4326)
hotspots <- hotspots %>%
  mutate(propnDelay = as.numeric(str_remove(propnDelay, pattern = "%")))


# Source - https://stackoverflow.com/a/73170772
# Posted by ulfelder
# Retrieved 2026-08-05, License - CC BY-SA 4.0

hotspots$marker <- with(hotspots, sprintf("%s </br> %s", names(hotspots)[1], names(hotspots)[2]))

leaflet(df) %>%
  addTiles() %>%
  addMarkers(~Long, ~Lat,
             popup = ~marker)


  
map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") 

map <- map %>% addArrowhead(data = hotspots,
                            color = ~pal_delay(propnDelay),
                            label = ~lapply(paste(
                                                  paste0("name: ",name),
                                                  paste0("am peak (sec): ",Morning.Peak.Seconds),
                                                  paste0("eve (sec): ", Evening.Seconds),
                                                  paste0("absDelay: ",absDelay),
                                                  paste0("propnDelay: ",propnDelay),
                                                  sep = "<br>"), HTML )
                            )

# map <- map %>% addPolylines(data = hotspots, 
#                             label = ~lapply(paste(prop, sep = "<br>"), HTML )
# )

map <- map %>% addPolygons(data = schemes_all, color = "#555555", weight = NA, popup = ~name, group = "schemes")
map <- map %>% addPolylines(data = schemes %>% filter(intervention_type == "route"),  color = "#555555", popup = ~name, group = "schemes")


map
