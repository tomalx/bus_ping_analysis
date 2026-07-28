# requires pings_seg_both_dir
library(leaflet.extras2) # for arrowheads function
library(htmltools)

seg_avg_speed <- pings_seg_both_dir %>%
  slice_min(speed_50,n = 20)

seg_sd_speed <- pings_seg_both_dir %>%
  slice_max(speed_sd,n = 20)


map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(data = seg_avg_speed, color = ~pal_speed(speed_50), opacity = 1,
               group = "speed") %>% 
  addArrowhead(data = seg_avg_speed, color = ~pal_speed(speed_50), opacity = 1,
               options = arrowheadOptions(
                 yawn = 75,
                 size = "10px",
                 frequency = '50px',
                 fill = FALSE,
                 offsets = list('start' = '50px', 'end' = '50px'),
                 perArrowheadOptions = NULL),
               group = "speed") %>% 
  addPolylines(data = seg_sd_speed, color = ~pal_sd(speed_sd), opacity = 1,
               group = "variation") %>% 
  addArrowhead(data = seg_sd_speed, color = ~pal_sd(speed_sd), opacity = 1,
               options = arrowheadOptions(
                 yawn = 75,
                 size = "10px",
                 frequency = '50px',
                 fill = FALSE,
                 offsets = list('start' = '50px', 'end' = '50px'),
                 perArrowheadOptions = NULL),
               group = "variation")


map <- map %>% addLegend("bottomright", pal = pal_speed, values = 0:15 , #title = "Average Speed (m/sec)", 
                         opacity = 1,
                         className = "legend-speed",
                         
                         group = "speed")

map <- map %>% addLegend("bottomright",
                         pal = pal_sd,
                         values = 0:5,
                         opacity = 1,
                         className = "legend-variation",
                         group = "variation")
# map <- map %>% addLegend("bottomright", pal = pal_iqr, values = 0:8 , # title = "inter quartile range (m/sec)", 
#                          opacity = 1,
#                          className = "legend-iqr",
#                          group = "iqr")

map <- map %>% addCircles(data = stops_0,
                          label = ~htmlEscape(stop_name),
                          radius = 2,
                          fill = NA,
                          opacity = 1,
                          color = "#444")

map <- map %>% addCircles(data = stops_1,
                          label = ~htmlEscape(stop_name),
                          color = "#444")

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
      #"iqr",
      "variation"
      # "heatmap in am",
      # "heatmap out am"
      
    ),
  options = layersControlOptions(collapsed = FALSE))



map %>% htmlwidgets::onRender("
function(el, x) {

  function hideAllLegends() {
    document.querySelectorAll(
      '.legend-speed, .legend-variation'
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





