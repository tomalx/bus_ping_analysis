#### leaflet map ####
library(leaflet)
library(RColorBrewer)

palette_fun <- khroma::color("bright")
pal <- palette_fun(7)
pal[1:7]

map <-  leaflet::leaflet() %>%
  addTiles(group = "OSM") %>% 
  leaflet::addProviderTiles("CartoDB.Positron", group = "carto")


for(i in 1:nrow(dc_routes)){
  map <- map %>% 
    leaflet::addPolylines(data = dc_routes[i,],
                          color = pal[i], 
                          stroke = TRUE,
                          weight = 10,
                          popup = paste0("direction: ",dc_routes$direction_id[i], " <br>", 
                                         dc_routes$shape_id[i]),
                          group = as.character(i),
                          fillOpacity = 0.6,
                          opacity = 0.6)
  
}

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
#                           color = pal[1],
#                           fillOpacity = 0.5,
#                           popup = ~paste0(time, "<br>", 
#                                           directionRef, "<br>"
#                                           ),
#                           group = "original points out")

# map <- map %>% addCircles(data = bod_eg %>% filter(direction_id == 1),
#                           radius = 0.5,
#                           weight = 0.3,
#                           color = pal[2],
#                           fillOpacity = 0.5,
#                           popup = ~paste0(time, "<br>", 
#                                           directionRef, "<br>"
#                           ),
#                           group = "original points in")

map <- map %>% addCircles(data = pings, # %>% filter(direction_id == 0),
                          radius = 0.8,
                          weight = 0.5,
                          color = pal[2],
                          fillOpacity = 0,
                          popup = ~paste0(time, "<br>", 
                                          directionRef, "<br>"
                          ),
                          group = "snapped points")

# map <- map %>% addCircles(data = pings_day_1, #%>% filter(direction_id == 1),
#                           radius = 0.8,
#                           weight = 0.5,
#                           color = pal[1],
#                           fillOpacity = 0,
#                           popup = ~paste0(time, "<br>", 
#                                           directionRef, "<br>"
#                           ),
#                           group = "snapped points in")

map <- map %>% leaflet.extras::addHeatmap(data = pings, #%>% filter(direction_id == 0) ,
                                          max = 0.8,  # default 1.0
                                          radius = 10, #default 25
                                          blur =  20, # default 15 (1=no blur)
                                          group = "heatmap")

map <- map %>% leaflet.extras::addHeatmap(data = pings_day %>% filter(direction_id == 1) ,
                                          max = 0.8,  # default 1.0
                                          radius = 10, #default 25
                                          blur =  20, # default 15 (1=no blur)
                                          group = "heatmap in")

# map <- map %>% leaflet.extras::addHeatmap(data = bod_snap_0_am ,
#                                           max = 0.8,  # default 1.0
#                                           radius = 10, #default 25
#                                           blur =  20, # default 15 (1=no blur)
#                                           group = "heatmap out am")
# 
# map <- map %>% leaflet.extras::addHeatmap(data = bod_snap_1_am ,
#                                           max = 0.8,  # default 1.0
#                                           radius = 10, #default 25
#                                           blur =  20, # default 15 (1=no blur)
#                                           group = "heatmap in am")

map <- map  %>% addLayersControl(baseGroups = c("OSM", "carto"),
                          overlayGroups = c(as.character(1:nrow(dc_routes)),
                                           # "nearest lines in",
                                           # "nearest lines out",
                                           # "original points in",
                                           # "original points out",
                                           # "snapped points in",
                                            "snapped points",
                                           # "heatmap in",
                                            "heatmap"
                                           # "heatmap in am",
                                           # "heatmap out am"
                                            
                                            ),
                          options = layersControlOptions(collapsed = FALSE))

map %>% hideGroup(c(as.character(1:nrow(dc_routes))))
