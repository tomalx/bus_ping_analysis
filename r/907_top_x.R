# requires pings_seg_both_dir
library(leaflet.extras) # for grouped layers control
library(leaflet.extras2) # for arrowheads function
library(htmltools)
library(purrr)

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

#speed palette
bright <- khroma::color("bright")
bright_pal <- bright(6)[1:6]

#speed palette
prgn <- khroma::color("PRGn")
prgn(6)[6:1]
khroma::plot_scheme(prgn(6))
khroma::plot_scheme_colourblind(prgn(6))

# burg <- unname(
#   as.character(
#     paletteer::paletteer_d("rcartocolor::Burg", 7)
#   )
# )

pal_speed <- colorNumeric(palette = incandescent(6)[6:1], domain = 0:12)
#pal_iqr <- colorNumeric(palette = burg, domain = 0:8)
#pal_sd <- colorNumeric(palette = burg, domain = 0:5)
pal_speed_div <- colorNumeric(palette = prgn(6)[6:1], domain = -2:2)



# pings_seg_both_dir <- pings_seg_both_dir %>% 
#   mutate(speed_sd_scaled = rescale(speed_sd, to = c(2,10))) %>% 
#   mutate(speed_am_v_qck = speed_early_late - speed_am_peak) %>% 
#   mutate(speed_pm_v_qck = speed_early_late - speed_pm_peak)


# library(sf)
# library(dplyr)
# library(purrr)

pings_seg_both_dir_all <- list.files(
  path = "rds",
  pattern = "^pings_seg.*\\.rds$",
  full.names = TRUE
) #|>
  # map(readRDS) |>
  # list_rbind()

# Print files
for (i in seq_along(pings_seg_both_dir_all)) {
  cat(sprintf("[%i] %s\n", i, basename(pings_seg_both_dir_all[i])))
}

# Specify files to exclude
exclude <- c( 6)

# Remove them
pings_seg_both_dir_all <- pings_seg_both_dir_all[-exclude]

# Check what's left
print(basename(pings_seg_both_dir_all))

# Read and combine
pings_seg_both_dir_all <- pings_seg_both_dir_all %>% 
  map(readRDS) %>% 
  list_rbind()

# convert to sf
pings_seg_both_dir_all <- pings_seg_both_dir_all %>%
  st_as_sf(crs = 4326)
  


# pings_seg_both_dir_all <- rbind(pings_seg_both_dir_all, pings_seg_both_dir)

pings_seg_both_dir_all <- pings_seg_both_dir_all %>% 
  group_by(seg_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  #mutate(speed_am_off_diff = speed_early_late - speed_am_peak) %>% 
  mutate(speed_am_v_qck = speed_early_late - speed_am_peak) %>%
  mutate(speed_pm_v_qck = speed_early_late - speed_pm_peak) %>% 
  filter(dist_m_start > 10)
  
  #mutate(speed_50_scaled = rescale(speed_50, to = c(0,15)))


map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") 
  
  # slowest
  map <- map %>%  addArrowhead(data = pings_seg_both_dir_all %>% 
                  slice_min(speed_50, n = 20), 
                # filter(direction_id == 0),  
               color = bright_pal[1], opacity = 1,
               weight = 5,
               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                               "am peak average speed: ", htmlEscape(round(speed_am_peak,2)),"m/s","<br>",
                               "early/late average speed: ", htmlEscape(round(speed_early_late,2)),"m/s","<br>",
                               "difference between AM peak and early/late: ", htmlEscape(round(speed_am_v_qck,2)),"m/s","<br>",
                               "difference between PM peak and early/late: ", htmlEscape(round(speed_pm_v_qck,2)),"m/s"),
               options = arrowheadOptions(
                 yawn = 40,
                 size = "20px",
                 frequency = '100px',
                 fill = TRUE,
                 offsets = list('start' = '30px', 'end' = '30px'),
                 perArrowheadOptions = NULL),
               highlightOptions = highlightOptions(
                 color = bright_pal[1],
                 weight = 10,
                 opacity = 0.5,
                 bringToFront = TRUE
               ),
               group = "slowest")
  
  # variance
  map <- map %>%  addArrowhead(data = pings_seg_both_dir_all %>% 
                                 slice_max(speed_sd, n = 20), 
                               # filter(direction_id == 0),  
                               color = bright_pal[2], opacity = 1,
                               weight = 5,
                              
                               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                                               "am peak average speed: ", htmlEscape(round(speed_am_peak,2)),"m/s","<br>",
                                               "early/late average speed: ", htmlEscape(round(speed_early_late,2)),"m/s","<br>",
                                               "difference between AM peak and early/late: ", htmlEscape(round(speed_am_v_qck,2)),"m/s","<br>",
                                               "difference between PM peak and early/late: ", htmlEscape(round(speed_pm_v_qck,2)),"m/s"),
                               options = arrowheadOptions(
                                 yawn = 40,
                                 size = "20px",
                                 frequency = '100px',
                                 fill = TRUE,
                                 offsets = list('start' = '50px', 'end' = '50px'),
                                 perArrowheadOptions = NULL),
                               highlightOptions = highlightOptions(
                                 color = bright_pal[2],
                                 weight = 10,
                                 opacity = 0.5,
                                 bringToFront = TRUE
                               ),
                              #options = list(offset = 3),
                               
                               group = "most variable") 
  
  
  # peak - off peak difference
  map <- map %>%  addArrowhead(data = pings_seg_both_dir_all %>% 
                                 slice_max(speed_am_v_qck, n = 20), 
                               # filter(direction_id == 0),  
                               color = bright_pal[3], opacity = 1,
                               weight = 5,
                               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                                               "am peak average speed: ", htmlEscape(round(speed_am_peak,2)),"m/s","<br>",
                                               "early/late average speed: ", htmlEscape(round(speed_early_late,2)),"m/s","<br>",
                                               "difference between AM peak and early/late: ", htmlEscape(round(speed_am_v_qck,2)),"m/s","<br>",
                                               "difference between PM peak and early/late: ", htmlEscape(round(speed_pm_v_qck,2)),"m/s"),
                               options = arrowheadOptions(
                                 yawn = 40,
                                 size = "20px",
                                 frequency = '100px',
                                 fill = TRUE,
                                 offsets = list('start' = '70px', 'end' = '70px'),
                                 perArrowheadOptions = NULL),
                               highlightOptions = highlightOptions(
                                 color = bright_pal[3],
                                 weight = 10,
                                 opacity = 0.5,
                                 bringToFront = TRUE
                               ),
                               group = "AM peak (difference v early/late)") 
  
  
  # peak - off peak difference
  map <- map %>%  addArrowhead(data = pings_seg_both_dir_all %>% 
                                 slice_max(speed_pm_v_qck, n = 20), 
                               # filter(direction_id == 0),  
                               color = bright_pal[4], opacity = 1,
                               weight = 5,
                               popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
                                               "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
                                               "am peak average speed: ", htmlEscape(round(speed_am_peak,2)),"m/s","<br>",
                                               "early/late average speed: ", htmlEscape(round(speed_early_late,2)),"m/s","<br>",
                                               "difference between AM peak and early/late: ", htmlEscape(round(speed_am_v_qck,2)),"m/s","<br>",
                                               "difference between PM peak and early/late: ", htmlEscape(round(speed_pm_v_qck,2)),"m/s"),
                               options = arrowheadOptions(
                                 yawn = 40,
                                 size = "20px",
                                 frequency = '100px',
                                 fill = TRUE,
                                 offsets = list('start' = '25px', 'end' = '35px'),
                                 perArrowheadOptions = NULL),
                               highlightOptions = highlightOptions(
                                 color = bright_pal[4],
                                 weight = 10,
                                 opacity = 0.5,
                                 bringToFront = TRUE
                                 ),
                               group = "PM peak (difference v early/late)") 
  
  # southbound am-off difference
  # map <- map %>%  addArrowhead(data = pings_seg_both_dir %>% 
  #                                # slice_min(speed_50, n = 20), 
  #                                filter(direction_id == 1),  
  #                              color = ~pal_speed_div(speed_am_off_diff), opacity = 1,
  #                              weight = 5,
  #                              popup = ~paste0("<b>",htmlEscape(seg_name),"</b>","<br>",
  #                                              "average speed (all day): ", htmlEscape(round(speed_50,2)),"m/s","<br>",
  #                                              "am peak average speed: ", htmlEscape(round(speed_am_peak,2)),"m/s","<br>",
  #                                              "early/late average speed: ", htmlEscape(round(speed_early_late,2)),"m/s","<br>",
  #                                              "difference (early/late minus am peak): ", htmlEscape(round(speed_am_off_diff,2)),"m/s"),
  #                              options = arrowheadOptions(
  #                                yawn = 60,
  #                                size = "20px",
  #                                frequency = '100px',
  #                                fill = TRUE,
  #                                offsets = list('start' = '50px', 'end' = '50px'),
  #                                perArrowheadOptions = NULL),
  #                              group = "northbound") 
  # 
  
  

# map <- map %>% addLegend("bottomright", 
#                          title = "Average Speed Difference (m/s) <br>
#                          am peak v early/late",
#                          labFormat = labelFormat(suffix = " m/s"),
#                          pal = pal_speed_div, values = -2:2 , #title = "Average Speed (m/sec)", 
#                          opacity = 1,
#                          className = "legend-speed"#,
#                          #group = "speed"
#                          )
#  

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

# map <- map %>% addCircles(data = stops_0,
#                           label = ~htmlEscape(stop_name),
#                           radius = 2,
#                           fill = NA,
#                           opacity = 1,
#                           color = "#444",
#                           group = "southbound stops")
# 
# map <- map %>% addCircles(data = stops_1,
#                           label = ~htmlEscape(stop_name),
#                           radius = 2,
#                           fill = NA,
#                           opacity = 1,
#                           color = "#444",
#                           group = "northbound stops")

map <- map %>% addPolygons(data = schemes %>% filter(intervention_type == "area"), color = "#555555", weight = NA, popup = ~name, group = "schemes")
map <- map %>% addPolylines(data = schemes %>% filter(intervention_type == "route"),  color = "#555555", popup = ~name, group = "schemes")

map <- map  %>% addGroupedLayersControl(
      overlayGroups = list(
        "stops & schemes" =
          c(#"southbound stops", 
            #"northbound stops",
            "schemes"),
        "top/bottom ranked stop to stop segments" = 
          c("slowest","most variable",
            "AM peak (difference v early/late)",
            "PM peak (difference v early/late)"
          )#,
       # "speed variation" = c("variation", "peak")
      ),
      options = groupedLayersControlOptions(
        groupCheckboxes = TRUE,
        collapsed = FALSE,
        groupsCollapsable = FALSE,
        sortLayers = FALSE,
        sortGroups = FALSE,
        sortBaseLayers = FALSE#,
        #exclusiveGroups = c("top/bottom ranked stop to stop segments")
      )
    )



legend_html <- sprintf("
<div style='background:white;
            padding:10px;
            border-radius:5px;'>

<div style='display:flex; align-items:center; margin-bottom:4px;'>
  <svg width='80' height='20'>
   <rect x='0' y='6.5'
          width='55' height='5'
          rx='2' ry='2'
          fill='%s'
          fill-opacity='1'/>
<path d='
    M20 2
    Q21 1 22 2
    L36 9
    L22 16
    Q21 17 20 16
    Z'
      fill='%s'
      stroke='%s'
      stroke-width='1'/>
  <span>slowest</span>
</div>

<div style='display:flex; align-items:center; margin-bottom:4px;'>
  <svg width='80' height='20'>
   <rect x='0' y='6.5'
          width='55' height='5'
          rx='2' ry='2'
          fill='%s'
          fill-opacity='1'/>
<path d='
    M20 2
    Q21 1 22 2
    L36 9
    L22 16
    Q21 17 20 16
    Z'
      fill='%s'
      stroke='%s'
      stroke-width='1'/>
  <span>most variable</span>
</div>

<div style='display:flex; align-items:center; margin-bottom:4px;'>
  <svg width='80' height='20'>
   <rect x='0' y='6.5'
          width='55' height='5'
          rx='2' ry='2'
          fill='%s'
          fill-opacity='1'/>
<path d='
    M20 2
    Q21 1 22 2
    L36 9
    L22 16
    Q21 17 20 16
    Z'
      fill='%s'
      stroke='%s'
      stroke-width='1'/>
  <span>AM peak (difference v early/late)</span>
</div>

<div style='display:flex; align-items:center; margin-bottom:4px;'>
  <svg width='80' height='20'>
   <rect x='0' y='6.5'
          width='55' height='5'
          rx='2' ry='2'
          fill='%s'
          fill-opacity='1'/>
<path d='
    M20 2
    Q21 1 22 2
    L36 9
    L22 16
    Q21 17 20 16
    Z'
      fill='%s'
      stroke='%s'
      stroke-width='1'/>
  <span>PM peak (difference v early/late)</span>
</div>


</div>
",
bright_pal[1], bright_pal[1], bright_pal[1],
bright_pal[2], bright_pal[2],  bright_pal[2],
bright_pal[3], bright_pal[3], bright_pal[3],
bright_pal[4], bright_pal[4], bright_pal[4]
)



map <- map |>
  addControl(
    html = legend_html,
    position = "bottomright"
  )



map %>% hideGroup(c("schemes"))
map
# map %>% htmlwidgets::onRender("
# function(el, x) {
# 
#   function hideAllLegends() {
#     document.querySelectorAll(
#       '.legend-speed, .legend-variation'
#     ).forEach(function(l) {
#       l.style.display = 'none';
#     });
#   }
# 
#   function showLegend(group) {
# 
#     hideAllLegends();
# 
#     var legend = document.querySelector('.legend-' + group);
# 
#     if (legend) {
#       legend.style.display = 'block';
#     }
#   }
# 
#   // Show speed legend when map loads
#   showLegend('speed');
# 
#   this.on('baselayerchange', function(e) {
#     console.log('Changed to:', e.name);
#     showLegend(e.name);
#   });
# 
# }
# ")





