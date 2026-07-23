# ggplot map


map <- ggplot() +
  geom_sf_interactive(data = pings_seg_speed, mapping = aes(colour = speed_50, tooltip = round(speed_50,2), data_id = seg_name), size = 2) +
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

  
  scale_colour_paletteer_c(
    "viridis::plasma",
   # breaks = rev(brks_scale),
   # labels = labels_scale,
    guide = guide_colourbar(
      title.position = 'top',
      direction = "horizontal"
    )
  ) 
map
  
  
  scale_colour_paletteer_c("ggthemes::Green-Gold",
                                )

  # scale_fill_paletteer_c("MoMAColors::Flash",
  #   
  #   breaks = rev(brks_scale),
  #   name = "Average age",
  #   drop = FALSE,
  #   labels = labels_scale,
  #   guide = guide_legend(
  #     direction = "horizontal",
  #     keyheight = unit(2, units = "mm"), 
  #     keywidth = unit(70/length(labels), units = "mm"),
  #     title.position = 'top',
  #     title.hjust = 0.5,
  #     label.hjust = 1,
  #     nrow = 1,
  #     byrow = TRUE,
  #     reverse = TRUE,
  #     label.position = "bottom"
  #   )
  # )

map

## gg boxplot

boxplot <- pings_plot %>%
  group_by(seg_name) %>% 
  mutate(ping_speed_median = median(ping_speed)) %>% 
  ungroup() %>% 
  ggplot(
  aes(x = forcats::fct_rev(seg_name), y = ping_speed, group = seg_name, fill = ping_speed_median)
) +
  geom_boxplot_interactive(aes(data_id = as.character(seg_name), tooltip = seg_name), outliers = FALSE) +
  ylim(c(0,25)) +
  
  #scale_x_reverse() +
  coord_flip() +
  theme_void() +
  theme(axis.text.y = element_text(size = 7, 
                                   vjust = 0.5, hjust=1),
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
           css = 'background:white; border:1px solid grey;'
         )
       ))



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
