theme_gray <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
    axis.ticks =        theme_segment(colour = "grey50"),
    axis.title.x =      theme_text(size = base_size),
    axis.title.y =      theme_text(size = base_size, angle = 90),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(fill = "grey95", colour = "grey80"),
    legend.text =       theme_text(size = base_size),
    legend.title =      theme_text(size = base_size, face = "bold", hjust = 0),

    panel.background =  theme_rect(fill = "grey90", colour = NA), 
    panel.border =      theme_rect(fill = NA, colour="white", size=2), 
    panel.grid.major =  theme_line(colour = "white"),
    panel.grid.minor =  theme_line(colour = "grey95", size = 0.25),
    panel.empty =       theme_rect(fill = "white", colour = NA),

    strip.background =  theme_rect(fill = "grey80"), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(size = base_size * 1.2)
  ), class = "options")
}


theme_bw <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        theme_segment(colour = "grey80", size = 1.5),
    axis.title.x =      theme_text(size = base_size),
    axis.title.y =      theme_text(size = base_size, angle = 90),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),

    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(colour = "grey80"),
    legend.text =       theme_text(size = base_size),
    legend.title =      theme_text(size = base_size, face = "bold", hjust = 0),

    panel.background =  theme_rect(fill = "white", colour = NA), 
    panel.border =      theme_rect(fill = NA, colour="black", size=1.5), 
    panel.grid.major =  theme_line(colour = "grey80"),
    panel.grid.minor =  theme_line(colour = "grey95", size = 0.2),
    panel.empty =       theme_rect(fill = "white", colour = NA),

    strip.background =  theme_rect(fill = "grey80", colour = "black"), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(size = base_size * 1.2)    
  ), class = "options")
}

