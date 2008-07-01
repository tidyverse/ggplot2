
# Should be function with params:
#   fontsize
#   background colour
#   foreground colour
default_theme <- list(
  # axis.box =        theme_blank(), 
  axis.line =        theme_line(),
  axis.ticks =       theme_segment(colour = "grey50"), # height = unit(0.4, "lines"),
  axis.x.text =      theme_text(size = 10, lineheight=0.9, colour = "grey50"),
  axis.x.title =     theme_text(),
  axis.y.text =      theme_text(size = 10, lineheight=0.9, colour = "grey50"),
  axis.y.title =     theme_text(angle = 90),
                     
  legend.box =       theme_box(), 
  legend.key =       theme_text(),
  legend.title =     theme_text(face = "bold"),
                     
  panel.box =        theme_box(fill = "grey90", colour="white", size=1), 
  panel.strip =      theme_box(fill = "grey80"), 
  panel.x.title =    theme_text(),
  panel.y.title =    theme_text(angle = 90),
  panel.grid.major = theme_segment(colour = "white"),
  panel.grid.minor = theme_segment(colour = "grey95", size = 0.25),

  plot.box =         theme_box(colour = NA),
  plot.title =       theme_text(size = 14)
)
