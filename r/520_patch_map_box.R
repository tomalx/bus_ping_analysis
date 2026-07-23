# ggplot map


map <- ggplot() +
  geom_sf_interactive(data = pings_seg_speed, 
                      mapping = aes(colour = speed_50, 
                                    #tooltip = round(speed_50,2), 
                                    data_id = seg_name), 
                      size = 4) +
 # scale_color_brewer() +
  geom_sf_interactive(data = stops_1, aes(tooltip = stop_name)) +
  theme_void() +
  theme(legend.position = "bottom") +
  
  
  scale_colour_stepsn(
    colours = viridisLite::plasma(7),
    breaks = 0:6,
    labels = c(0:5,"6+"),
    name = "Average Speed (m/s)",
    guide = guide_coloursteps(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      direction = "horizontal",
      barwidth = unit(5, "cm"),
      barheight = unit(5, "mm"),
      even.steps = TRUE
    )
  )

 
map
  
  
 

## gg boxplot

boxplot <- pings_plot %>%
  group_by(seg_name) %>% 
  mutate(
    ping_speed_median = median(ping_speed, na.rm = TRUE),
    ping_speed_mean   = mean(ping_speed, na.rm = TRUE),
    ping_speed_iqr    = IQR(ping_speed, na.rm = TRUE),
    ping_speed_sd     = sd(ping_speed, na.rm = TRUE),
    ping_count        = n(),
    ping_count_lt1    = sum(ping_speed < 1, na.rm = TRUE),
    tooltip_text = paste0(
      "<b>", seg_name, "</b><br/>",
      "Median: ", round(ping_speed_median, 2), " m/s<br/>",
      "Mean: ", round(ping_speed_mean, 2), " m/s<br/>",
      "IQR: ", round(ping_speed_iqr, 2), "<br/>",
      "SD: ", round(ping_speed_sd, 2), "<br/>",
      "Pings: ", ping_count, "<br/>",
      "&lt;1 m/s: ", ping_count_lt1
    )
    ) %>% 
  ungroup() %>% 
  ggplot(
  aes(x = forcats::fct_rev(seg_name), y = ping_speed, group = seg_name, fill = ping_speed_mean)
) +
  geom_boxplot_interactive(aes(data_id = as.character(seg_name), tooltip = tooltip_text), outliers = FALSE) +
  ylim(c(0,25)) +
  
  #scale_x_reverse() +
  coord_flip() +
  theme_void() +
  theme(# axis.text.y = element_text(size = 7,vjust = 0.5, hjust=1),
        legend.position = "bottom") 
#scale_x_binned()

boxplot

## -------- render ggiraph --------- ##

girafe(ggobj = map | boxplot,
       width_svg = 18,
       height_svg = 12,
       options = list(
         opts_hover(css = ''),
         opts_hover_inv(css = 'opacity:0.1;'),
         opts_tooltip(
           offx = 20,
           offy = -50,
           css = 'background:white; border:1px solid grey;'
         )
       ))

# render with fixed position tooltip:
girafe(
  ggobj = map | boxplot,
  width_svg = 18,
  height_svg = 12,
  options = list(
    opts_hover(css = ''),
    opts_hover_inv(css = 'opacity:0.1;'),
    opts_tooltip(
      use_fill = TRUE,
      offx = 0,
      offy = 0,
      css = "
    background:white;
    border:1px solid grey;
    padding:5px;
  "
    )
  )
)



# ---------------------------------------- #

library(patchwork)
library(paletteer)
library(ggiraph)

layout <- "
AAA#
AAA#
BBBB
"
patch <- map + boxplot + plot_layout(design = layout)
patch 
