#### leaflet map ####
library(leaflet)
library(RColorBrewer)

palette_fun <- khroma::color("bright")
pal <- palette_fun(7)
pal[1:7]

map <-  leaflet::leaflet() %>%
  addTiles(group = "OSM") %>% 
  leaflet::addProviderTiles("CartoDB.Positron", group = "carto")


for(i in 1:nrow(dc_routes_shape)){
  map <- map %>% 
    leaflet::addPolylines(data = dc_routes_shape[i,],
                          color = pal[i], 
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
                                          directionRef, "<br>"
                                          ),
                          group = "original points")

map <- map %>% leaflet.extras::addHeatmap(data = bod_eg1,
                                          max = 0.8,  # default 1.0
                                          radius = 10, #default 25
                                          blur =  20, # default 15 (1=no blur)
                                          group = "heatmap")

map  %>% addLayersControl(baseGroups = c("OSM", "carto"),
                          overlayGroups = c(as.character(1:nrow(dc_routes_shape)),"nearest lines","original points", "heatmap"),
                          options = layersControlOptions(collapsed = FALSE))
