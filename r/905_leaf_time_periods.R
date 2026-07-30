# requires pings_seg_both_dir
library(leaflet.extras) # for grouped layers control
library(leaflet.extras2) # for arrowheads function
library(htmltools)

schemes <- st_read(choose.files())

### To Do : -> -> -> -> 
###        segment by length
###        variance and speed on same map
###        maybe use thickness of line for variance???
###        speed by time of day

###        more than one route
###        both top 10 variance and lowest 10 speed

# seg_avg_speed <- pings_seg_both_dir %>%
#   slice_min(speed_50,n = 20)

# seg_sd_speed <- pings_seg_both_dir %>%
#   slice_max(speed_sd,n = 20)

top_n <- 20

#speed palette
incandescent <- khroma::color("incandescent")
incandescent(6)[6:1]

burg <- unname(
  as.character(
    paletteer::paletteer_d("rcartocolor::Burg", 7)
  )
)

pal_speed <- colorNumeric(palette = incandescent(6)[6:1], domain = 0:12)
#pal_iqr <- colorNumeric(palette = burg, domain = 0:8)
#pal_sd <- colorNumeric(palette = burg, domain = 0:5)



pings_seg_both_dir <- pings_seg_both_dir %>% 
  mutate(speed_sd_scaled = rescale(speed_sd, to = c(2,10)))
  
  #mutate(speed_50_scaled = rescale(speed_50, to = c(0,15)))


map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") 
  
  # southbound am peak
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                 # slice_min(speed_50, n = 20), 
                 filter(direction_id == 0),  
               color = ~pal_speed(speed_am_peak), opacity = 1,
               weight = ~speed_sd_scaled,
               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                               "am peak average speed: ", htmlEscape(round(speed_am_peak,2)),"m/s","<br>",
                               "Speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
               options = arrowheadOptions(
                 yawn = 60,
                 size = "20px",
                 frequency = '100px',
                 fill = TRUE,
                 offsets = list('start' = '50px', 'end' = '50px'),
                 perArrowheadOptions = NULL),
               group = "southbound am peak") 
  
  # southbound inter peak
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                 # slice_min(speed_50, n = 20), 
                 filter(direction_id == 0),  
               color = ~pal_speed(speed_inter_peak), opacity = 1,
               weight = ~speed_sd_scaled,
               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                               "inter peak average speed: ", htmlEscape(round(speed_inter_peak,2)),"m/s","<br>",
                               "Speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
               options = arrowheadOptions(
                 yawn = 60,
                 size = "20px",
                 frequency = '100px',
                 fill = TRUE,
                 offsets = list('start' = '50px', 'end' = '50px'),
                 perArrowheadOptions = NULL),
               group = "southbound inter peak") 
  
  # southbound pm peak
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                 # slice_min(speed_50, n = 20), 
                 filter(direction_id == 0),  
               color = ~pal_speed(speed_pm_peak), opacity = 1,
               weight = ~speed_sd_scaled,
               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                               "pm peak average speed: ", htmlEscape(round(speed_pm_peak,2)),"m/s","<br>",
                               "speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
               options = arrowheadOptions(
                 yawn = 60,
                 size = "20px",
                 frequency = '100px',
                 fill = TRUE,
                 offsets = list('start' = '50px', 'end' = '50px'),
                 perArrowheadOptions = NULL),
               group = "southbound pm peak") 
  
  # southbound early/late
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                 # slice_min(speed_50, n = 20), 
                 filter(direction_id == 0),  
               color = ~pal_speed(speed_early_late), opacity = 1,
               weight = ~speed_sd_scaled,
               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                               "early/late average speed: ", htmlEscape(round(speed_early_late,2)),"m/s","<br>",
                               "Speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
               options = arrowheadOptions(
                 yawn = 60,
                 size = "20px",
                 frequency = '100px',
                 fill = TRUE,
                 offsets = list('start' = '50px', 'end' = '50px'),
                 perArrowheadOptions = NULL),
               group = "southbound early/late") 
  
  # northbound am peak
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                                 # slice_min(speed_50, n = 20), 
                                 filter(direction_id == 1),  
                               color = ~pal_speed(speed_am_peak), opacity = 1,
                               weight = ~speed_sd_scaled,
                               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                                               "am peak average speed: ", htmlEscape(round(speed_am_peak,2)),"m/s","<br>",
                                               "Speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
                               options = arrowheadOptions(
                                 yawn = 60,
                                 size = "20px",
                                 frequency = '100px',
                                 fill = TRUE,
                                 offsets = list('start' = '50px', 'end' = '50px'),
                                 perArrowheadOptions = NULL),
                               group = "northbound am peak") 
  
  # northbound inter peak
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                                 # slice_min(speed_50, n = 20), 
                                 filter(direction_id == 1),  
                               color = ~pal_speed(speed_inter_peak), opacity = 1,
                               weight = ~speed_sd_scaled,
                               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                                               "inter peak average speed: ", htmlEscape(round(speed_inter_peak,2)),"m/s","<br>",
                                               "Speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
                               options = arrowheadOptions(
                                 yawn = 60,
                                 size = "20px",
                                 frequency = '100px',
                                 fill = TRUE,
                                 offsets = list('start' = '50px', 'end' = '50px'),
                                 perArrowheadOptions = NULL),
                               group = "northbound inter peak") 
  
  # northbound pm peak
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                                 # slice_min(speed_50, n = 20), 
                                 filter(direction_id == 1),  
                               color = ~pal_speed(speed_pm_peak), opacity = 1,
                               weight = ~speed_sd_scaled,
                               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                                               "pm peak average speed: ", htmlEscape(round(speed_pm_peak,2)),"m/s","<br>",
                                               "speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
                               options = arrowheadOptions(
                                 yawn = 60,
                                 size = "20px",
                                 frequency = '100px',
                                 fill = TRUE,
                                 offsets = list('start' = '50px', 'end' = '50px'),
                                 perArrowheadOptions = NULL),
                               group = "northbound pm peak") 
  
  # northbound early/late
  map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
                                 # slice_min(speed_50, n = 20), 
                                 filter(direction_id == 1),  
                               color = ~pal_speed(speed_early_late), opacity = 1,
                               weight = ~speed_sd_scaled,
                               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                                               "early/late average speed: ", htmlEscape(round(speed_early_late,2)),"m/s","<br>",
                                               "Speed standard deviation: ", htmlEscape(round(speed_sd,2)),"m/s"),
                               options = arrowheadOptions(
                                 yawn = 60,
                                 size = "20px",
                                 frequency = '100px',
                                 fill = TRUE,
                                 offsets = list('start' = '50px', 'end' = '50px'),
                                 perArrowheadOptions = NULL),
                               group = "northbound early/late") 

map <- map %>% addLegend("bottomright", 
                         title = "Average Speed <br>(m/s)",
                         labFormat = labelFormat(suffix = " m/s"),
                         pal = pal_speed, values = 0:12 , #title = "Average Speed (m/sec)", 
                         opacity = 1,
                         className = "legend-speed"#,
                         #group = "speed"
                         )

# map <- map %>% addLegend("bottomright",
#                          title = "Variation in speeds <br>(standard deviation - m/s)",
#                          labFormat = labelFormat(suffix = " m/s"),
#                          pal = pal_sd,
#                          values = 0:5,
#                          opacity = 1,
#                          className = "legend-variation",
#                          group = "variation")
# map <- map %>% addLegend("bottomright", pal = pal_iqr, values = 0:8 , # title = "inter quartile range (m/sec)", 
#                          opacity = 1,
#                          className = "legend-iqr",
#                          group = "iqr")

map <- map %>% addCircles(data = stops_0,
                          label = ~htmlEscape(stop_name),
                          radius = 2,
                          fill = NA,
                          opacity = 1,
                          color = "#444",
                          group = "southbound stops")

map <- map %>% addCircles(data = stops_1,
                          label = ~htmlEscape(stop_name),
                          radius = 2,
                          fill = NA,
                          opacity = 1,
                          color = "#444",
                          group = "northbound stops")

map <- map %>% addPolygons(data = schemes %>% filter(intervention_type == "area"), color = "#555555", weight = NA, popup = ~name, group = "schemes")
map <- map %>% addPolylines(data = schemes %>% filter(intervention_type == "route"),  color = "#555555", popup = ~name, group = "schemes")

map <- map  %>% addGroupedLayersControl(
      overlayGroups = list(
        "stops & schemes" = 
          c("southbound stops", "northbound stops","schemes"),
        "average speed" = 
          c("southbound am peak", "southbound inter peak", "southbound pm peak", "southbound early/late",
            "northbound am peak", "northbound inter peak", "northbound pm peak", "northbound early/late"
          )#,
       # "speed variation" = c("variation", "peak")
      ),
      options = groupedLayersControlOptions(
        groupCheckboxes = TRUE,
        collapsed = FALSE,
        groupsCollapsable = FALSE,
        sortLayers = FALSE,
        sortGroups = FALSE,
        sortBaseLayers = FALSE,
        exclusiveGroups = c("average speed")
      )
    )

map %>% hideGroup(c("schemes", "northbound stops", "southbound stops"))

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





