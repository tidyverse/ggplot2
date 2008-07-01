
# Should be function with params:
#   fontsize
#   background colour
#   foreground colour
default_theme <- list(
  # axis.box =        theme_blank(), 
  axis.line =        theme_line(),
  axis.text.x =      theme_text(size = 10, lineheight=0.9, colour = "grey50"),
  axis.text.y =      theme_text(size = 10, lineheight=0.9, colour = "grey50"),
  axis.ticks =       theme_segment(colour = "grey50"), # height = unit(0.4, "lines"),
  axis.title.x =     theme_text(),
  axis.title.y =     theme_text(angle = 90),
                     
  legend.box =       theme_box(), 
  legend.key =       theme_text(),
  legend.title =     theme_text(face = "bold"),
                     
  panel.background = theme_box(fill = "grey90", colour = NA), 
  panel.border =     theme_box(fill = NA, colour="white", size=1), 
  panel.strip =      theme_box(fill = "grey80"), 
  panel.title.x =    theme_text(),
  panel.title.y =    theme_text(angle = 90),
  panel.grid.major = theme_line(colour = "white"),
  panel.grid.minor = theme_line(colour = "grey95", size = 0.25),

  plot.box =         theme_box(colour = NA),
  plot.title =       theme_text(size = 14)
)
