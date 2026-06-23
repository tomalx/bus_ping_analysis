library(raster)
library(KernSmooth)
library(sf)
library(MASS)
library(tidyverse)

library("leaflet")
library("data.table")
#library("sp")
#library("rgdal")
# library("maptools")
#library("KernSmooth")
#library("raster")

### TRY MASS::kde2d

#MASS::kde2d()

pings <- bod_snap_0  %>% # filter(directionRef == "outbound") %>% 
  filter(hour(time) == 8)

pings <- pings_am

end_stops <- stop_seq %>% filter(stop_sequence == 1) # get first and last stops
end_stops <- gtfs_sf$stops %>% filter(stop_code %in% end_stops$stop_code) # find coordinates
end_stops <- end_stops %>% st_buffer(90) %>% st_transform(4326)

other_stops <- stop_seq %>% group_by(direction_id) %>% 
  filter(stop_sequence != min(stop_sequence))
  
other_stops <- gtfs_sf$stops %>% filter(stop_code %in% other_stops$stop_code)
other_stops <- other_stops %>% st_transform(4326) #%>% st_buffer(20)

pings <- pings %>% st_difference(st_union(end_stops))

pings$lng <- st_coordinates(pings)[,1]
pings$lat <- st_coordinates(pings)[,2]
pings <- data.table::data.table(pings)


#setnames(dat, tolower(colnames(dat)))
#setnames(dat, gsub(" ", "_", colnames(dat)))
pings <- pings[!is.na(lng)]
#dat[ , date := as.IDate(date, "%m/%d/%Y")]

lngbw <- 0.001
latbw <- 0.001

kde2 <- kde2d(pings$lng, pings$lat, 
              h = c(lngbw,latbw), # h = bandwidth
              n = 2500) # n = grid size n x n

kde_raster <- raster(list(x=kde2$x ,y=kde2$y ,z = kde2$z))

## Create kernel density output
#kde <- bkde2D(pings[ , list(lng, lat)],
             # bandwidth=c(.0008, .0008),
              # bandwidth=c(lngbw, latbw),
              # range.x = list(c(min(pings$lng)-(2*lngbw),max(pings$lng)+(2*lngbw)), 
              #                c(min(pings$lat)-(2*lngbw),max(pings$lat)+(2*latbw))),
              # gridsize = c(2500,2500))
# Create Raster from Kernel Density output
# KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

range(kde2$z)

#set low density cells as NA so we can make them transparent with the colorNumeric function
#KernelDensityRaster@data@values[which(KernelDensityRaster@data@values <= 1)] <- NA

kde_raster@data@values[which(kde_raster@data@values <= 1)] <- NA

#create pal function for coloring the raster
#palRaster <- colorQuantile("inferno", n = 6,domain = 0:max(kde2$z), na.color = "transparent", reverse = TRUE)

max_z <- 100000

palRaster <- colorNumeric("inferno",domain = kde2$z, na.color = "transparent", reverse = TRUE)

## Leaflet map with raster
leaflet() %>% addProviderTiles("CartoDB.Positron", group = "carto") %>%
  addTiles(group = "OSM") %>% 
  # addPolygons(data = end_stops %>% st_transform(4326)) %>% 
  addCircles(data = pings,
                 radius = 1,
                 weight = 0.0,
                 color = "black",
                 fillOpacity = 0.3,
                 popup = ~paste0(time, "<br>",
                                 directionRef, "<br>"
                 ),
                 group = "snapped pings") %>%
  addMarkers(data = other_stops,
             group = "stops - outbound") %>% 
  # addRasterImage(kde_raster,
  #                colors = palRaster,
  #                opacity = .6,
  #                group = "heatmap 2") %>%
  leaflet.extras::addHeatmap(data = pings,
                                 max = 0.8,  # default 1.0
                                 radius = 5, #default 25
                                 blur =  10, # default 15 (1=no blur)
                                 group = "heatmap 1") %>% 
  # addLegend(pal = palRaster, 
  #           values = kde2$z, 
  #           title = "Kernel Density of Points",
  #           group = "") %>% 
  addLayersControl(baseGroups = c("OSM", "carto"),
                            overlayGroups = c(as.character(1:nrow(dc_routes_shape)),"nearest lines","original points","stops - outbound", "heatmap 1","heatmap 2"),
                            options = layersControlOptions(collapsed = FALSE))




## Leaflet map with raster
leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
  addRasterImage(kde_raster,
                  #KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = 1:18500, 
            title = "Kernel Density of Points") 
