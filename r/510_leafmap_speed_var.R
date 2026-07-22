#### leaflet map ####
library(leaflet)
library(htmltools)
library(RColorBrewer)

palette_fun <- khroma::color("bright")
pal_bright <- palette_fun(7)
pal_bright[1:7]

palette_fun <- khroma::color("smoothrainbow")
pal_rainbow <- palette_fun(7)
pal_rainbow[1:7]
khroma::plot_scheme_colorblind(pal_rainbow)

map <-  leaflet::leaflet() %>%
  # addTiles(group = "OSM") %>% 
  leaflet::addProviderTiles("CartoDB.Positron", group = "carto")


map <- map %>% addPolylines(data = pings_seg_speed, 
                            color = ~pal_speed(speed_50),
                            popup = ~seg_name,
                            opacity = 1,
                            group = "speed")

map <- map %>% addPolylines(data = pings_seg_speed, 
                            color = ~pal_iqr(speed_iqr), 
                            opacity = 1,
                            group = "iqr")

map <- map %>% addPolylines(data = pings_seg_speed, 
                            color = ~pal_sd(speed_sd), 
                            opacity = 1,
                            group = "sd")

map <- map %>% addLegend("bottomright", pal = pal_speed, values = 0:15 , #title = "Average Speed (m/sec)", 
                         opacity = 1,
                         className = "legend-speed",
                         
                         group = "speed")
map <- map %>% addLegend("bottomright",
                         pal = pal_sd,
                         values = c(0, 5),
                         opacity = 1,
                         className = "legend-sd",
                         group = "sd"
)
map <- map %>% addLegend("bottomright", pal = pal_iqr, values = 0:8 , # title = "inter quartile range (m/sec)", 
                         opacity = 1,
                         className = "legend-iqr",
                         group = "iqr")

map <- map %>% addCircles(data = stops_1,
                          label = ~htmlEscape(stop_name))

map <- map  %>% addLayersControl(
  baseGroups = 
    # c("OSM", "carto"),
    # overlayGroups =
    c(
      #as.character(1:nrow(dc_routes)),
      # "nearest lines in",
      # "nearest lines out",
      # "original points in",
      # "original points out",
      # "snapped points in",
      # "snapped points",
      # "heatmap in",
      # "heatmap out",
      "speed",
      "iqr",
      "sd"
      # "heatmap in am",
      # "heatmap out am"
      
    ),
  options = layersControlOptions(collapsed = FALSE))



map %>% htmlwidgets::onRender("
function(el, x) {

  function hideAllLegends() {
    document.querySelectorAll(
      '.legend-speed, .legend-sd, .legend-iqr'
    ).forEach(function(l) {
      l.style.display = 'none';
    });
  }

  function showLegend(group) {

    hideAllLegends();

    var legend = document.querySelector('.legend-' + group);

    if (legend) {
      legend.style.display = 'block';
    }
  }

  // Show speed legend when map loads
  showLegend('speed');

  this.on('baselayerchange', function(e) {
    console.log('Changed to:', e.name);
    showLegend(e.name);
  });

}
")




