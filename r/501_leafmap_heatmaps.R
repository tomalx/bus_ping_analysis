#### leaflet map ####
library(leaflet)
library(RColorBrewer)

palette_fun <- khroma::color("bright")
pal <- palette_fun(7)
pal[1:7]

heatmap_max <- 4
heatmap_radius <- 20
heatmap_blur <- 25

stops <- stop_seq %>% ungroup() %>% 
  filter(direction_id == 1) %>% 
  st_as_sf() %>% 
  st_transform(4326)

map <-  leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron", group = "carto") #%>% 
  #addTiles(group = "OSM")  

map <- map %>% leaflet.extras::addHeatmap(data = pings_day ,
                                          max = heatmap_max,  # default 1.0
                                          radius = heatmap_radius, #default 25
                                          blur =  heatmap_blur, # default 15 (1=no blur)
                                          group = "day")

map <- map %>% leaflet.extras::addHeatmap(data = pings_am ,
                                          max = heatmap_max,  # default 1.0
                                          radius = heatmap_radius, #default 25
                                          blur =  heatmap_blur, # default 15 (1=no blur)
                                          group = "am")

map <- map %>% leaflet.extras::addHeatmap(data = pings_pm ,
                                          max = heatmap_max,  # default 1.0
                                          radius = heatmap_radius, #default 25
                                          blur =  heatmap_blur, # default 15 (1=no blur)
                                          group = "pm")

map <- map %>% addCircles(data = stops,
                                  color = "#444444", label = ~stop_name,
                          group = "stops")

map <- map  %>% addLayersControl(#baseGroups = c("OSM", "carto"),
                                 baseGroups = c("am",
                                 "pm",
                                 "day"),
                                 overlayGroups = "stops",
                                 options = layersControlOptions(collapsed = FALSE))

map %>% hideGroup("OSM")
# for(i in 1:nrow(dc_routes)){
#   map <- map %>% 
#     leaflet::addPolylines(data = dc_routes[i,],
#                           color = pal[i], 
#                           stroke = TRUE,
#                           weight = 10,
#                           popup = paste0("direction: ",dc_routes$direction_id[i], " <br>", 
#                                          dc_routes$shape_id[i]),
#                           group = as.character(i),
#                           fillOpacity = 0.6,
#                           opacity = 0.6)
#   
# }

# map <- map %>% addPolylines(data = nearest_lines_0,
#                             weight = 0.6,
#                             color = "#444444",
#                             opacity = 0.5,
#                             #dashArray = "2,2",
#                             group = "nearest lines out")
# 
# map <- map %>% addPolylines(data = nearest_lines_1,
#                             weight = 0.6,
#                             color = "#444444",
#                             opacity = 0.5,
#                             #dashArray = "2,2",
#                             group = "nearest lines in")

# map <- map %>% addCircles(data = bod_eg %>% filter(direction_id == 0),
#                           radius = 0.5,
#                           weight = 0.3,
#                           color = pal[3],
#                           fillOpacity = 0.5,
#                           popup = ~paste0(time, "<br>", 
#                                           directionRef, "<br>"
#                                           ),
#                           group = "original points out")

# map <- map %>% addCircles(data = bod_eg %>% filter(direction_id == 1),
#                           radius = 0.5,
#                           weight = 0.3,
#                           color = pal[4],
#                           fillOpacity = 0.5,
#                           popup = ~paste0(time, "<br>", 
#                                           directionRef, "<br>"
#                           ),
#                           group = "original points in")
# 
# map <- map %>% addCircles(data = bod_snap_0,
#                           radius = 0.8,
#                           weight = 0.5,
#                           color = pal[3],
#                           fillOpacity = 0,
#                           popup = ~paste0(time, "<br>", 
#                                           directionRef, "<br>"
#                           ),
#                           group = "snapped points out")
# 
# map <- map %>% addCircles(data = bod_snap_1,
#                           radius = 0.8,
#                           weight = 0.5,
#                           color = pal[4],
#                           fillOpacity = 0,
#                           popup = ~paste0(time, "<br>", 
#                                           directionRef, "<br>"
#                           ),
#                           group = "snapped points in")

map <- map %>% leaflet.extras::addHeatmap(data = bod_snap_0 ,
                                          max = 0.8,  # default 1.0
                                          radius = 10, #default 25
                                          blur =  20, # default 15 (1=no blur)
                                          group = "heatmap out")

map <- map %>% leaflet.extras::addHeatmap(data = bod_snap_1 ,
                                          max = 0.8,  # default 1.0
                                          radius = 10, #default 25
                                          blur =  20, # default 15 (1=no blur)
                                          group = "heatmap in")

map <- map %>% leaflet.extras::addHeatmap(data = bod_snap_0_am ,
                                          max = 0.8,  # default 1.0
                                          radius = 10, #default 25
                                          blur =  20, # default 15 (1=no blur)
                                          group = "heatmap out am")

map <- map %>% leaflet.extras::addHeatmap(data = bod_snap_1_am ,
                                          max = 0.8,  # default 1.0
                                          radius = 10, #default 25
                                          blur =  20, # default 15 (1=no blur)
                                          group = "heatmap in am")

map <- map  %>% addLayersControl(baseGroups = c("OSM", "carto"),
                          overlayGroups = c(as.character(1:nrow(dc_routes)),
                                            "nearest lines in",
                                            "nearest lines out",
                                            "original points in",
                                            "original points out",
                                            "snapped points in",
                                            "snapped points out",
                                            "heatmap in",
                                            "heatmap out",
                                            "heatmap in am",
                                            "heatmap out am"
                                            
                                            ),
                          options = layersControlOptions(collapsed = FALSE))

map %>% hideGroup(c(as.character(1:nrow(dc_routes))))
