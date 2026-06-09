#### leaflet map ####
library(leaflet)


map <-  leaflet::leaflet() %>%
  addTiles(group = "OSM") %>% 
  leaflet::addProviderTiles("CartoDB.Positron", group = "carto")


for(i in 1:nrow(dc_routes_shape)){
  map <- map %>% 
    leaflet::addPolylines(data = dc_routes_shape[i,],
                          color = brew_color_pal[i], 
                          stroke = TRUE,
                          weight = 10,
                          popup = paste0("direction: ",dc_routes_shape$direction_id[i], " <br>", 
                                         dc_routes_shape$shape_id[i]),
                          group = as.character(i),
                          fillOpacity = 0.6,
                          opacity = 0.6)
  
}

map <- map %>% addPolylines(data = nearest_lines,
                            weight = 1,
                            color = "#444444",
                            opacity = 0.8,
                            #dashArray = "2,2",
                            group = "nearest lines")

map <- map %>% addCircles(data = bod_eg1,
                          radius = 0.8,
                          weight = 0.3,
                          color = "black",
                          fillOpacity = 0,
                          popup = ~paste0(time, "<br>", 
                                          directionRef, "<br>",
                                          ),
                          group = "original points")

map  %>% addLayersControl(baseGroups = c("OSM", "carto"),
                          overlayGroups = c(as.character(1:nrow(dc_routes_shape)),"nearest lines","original points"),
                          options = layersControlOptions(collapsed = FALSE))
