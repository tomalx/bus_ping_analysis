library(raster)
library(KernSmooth)
library(sf)

library("leaflet")
library("data.table")
#library("sp")
#library("rgdal")
# library("maptools")
#library("KernSmooth")
#library("raster")

### TRY MASS::kde2d

#MASS::kde2d()

pings <- bod_eg1 %>% filter(directionRef == "inbound") %>% sample_n(12000)
pings$lng <- st_coordinates(pings)[,1]
pings$lat <- st_coordinates(pings)[,2]
pings <- data.table::data.table(pings)

#setnames(dat, tolower(colnames(dat)))
#setnames(dat, gsub(" ", "_", colnames(dat)))
pings <- pings[!is.na(lng)]
#dat[ , date := as.IDate(date, "%m/%d/%Y")]

lngbw <- .001
latbw <- .001

## Create kernel density output
kde <- bkde2D(pings[ , list(lng, lat)],
             # bandwidth=c(.0008, .0008),
              bandwidth=c(lngbw, latbw),
              range.x = list(c(min(pings$lng)-(2*lngbw),max(pings$lng)+(2*lngbw)), 
                             c(min(pings$lat)-(2*lngbw),max(pings$lat)+(2*latbw))),
              gridsize = c(2500,2500))
# Create Raster from Kernel Density output
KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

range(kde$fhat)

#set low density cells as NA so we can make them transparent with the colorNumeric function
KernelDensityRaster@data@values[which(KernelDensityRaster@data@values <= 1)] <- NA


#create pal function for coloring the raster
palRaster <- colorQuantile("inferno", n = 6,domain = 1:18500, na.color = "transparent", reverse = TRUE)


## Leaflet map with raster
leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = 1:18500, 
            title = "Kernel Density of Points")
