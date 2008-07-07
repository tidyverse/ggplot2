
# Should be function with params:
#   fontsize
#   background colour
#   foreground colour  (and interpolate between them? prob won't work)
theme_gray <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size / 1.2 , lineheight = 0.9, colour = "grey50", vjust = 1),
    axis.text.y =       theme_text(size = base_size / 1.2, lineheight = 0.9, colour = "grey50", hjust = 1),
    axis.ticks.x =      theme_segment(colour = "grey50"),
    axis.ticks.y =      theme_segment(colour = "grey50"),
    axis.title.x =      theme_text(size = base_size),
    axis.title.y =      theme_text(size = base_size, angle = 90),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),

    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(fill = "grey95", colour = "grey80"),
    legend.text =       theme_text(size = base_size),
    legend.title =      theme_text(size = base_size, face = "bold", hjust = 0),

    panel.background =  theme_rect(fill = "grey90", colour = NA), 
    panel.border =      theme_rect(fill = NA, colour="white", size=1), 
    panel.grid.major =  theme_line(colour = "white"),
    panel.grid.minor =  theme_line(colour = "grey95", size = 0.25),
    panel.empty =       theme_rect(fill = "white", colour = NA),

    strip.background =  theme_rect(fill = "grey80"), 
    strip.label =       function(variable, value) value, 
    strip.text.x =     theme_text(size = base_size / 1.2),
    strip.text.y =     theme_text(size = base_size / 1.2, angle = -90),

    plot.box =          theme_rect(colour = NA),
    plot.title =        theme_text(size = base_size * 1.4)    
  ), class = "options")
}


