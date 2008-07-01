
# Should be function with params:
#   fontsize
#   background colour
#   foreground colour  (and interpolate between them? prob won't work)
default_theme <- list(
  # axis.box =         theme_blank(), 
  axis.line =         theme_line(),
  axis.text.x =       theme_text(size = 10, lineheight = 0.9, colour = "grey50"),
  axis.text.y =       theme_text(size = 10, lineheight = 0.9, colour = "grey50"),
  axis.ticks =        theme_segment(colour = "grey50"),
  axis.title.x =      theme_text(),
  axis.title.y =      theme_text(angle = 90),
                      
  legend.background = theme_rect(colour=NA), 
  legend.key =        theme_rect(fill = "grey95", colour = "grey80"),
  legend.text =       theme_text(),
  legend.title =      theme_text(face = "bold", hjust = 0),
                      
  panel.background =  theme_rect(fill = "grey90", colour = NA), 
  panel.border =      theme_rect(fill = NA, colour="white", size=1), 
  panel.grid.major =  theme_line(colour = "white"),
  panel.grid.minor =  theme_line(colour = "grey95", size = 0.25),
                      
  strip.background =  theme_rect(fill = "grey80"), 
  strip.label =       function(variable, value) value, 
  strip.title.x =     theme_text(),
  strip.title.y =     theme_text(angle = -90),
                      
  plot.box =          theme_rect(colour = NA),
  plot.title =        theme_text(size = 14)
)

