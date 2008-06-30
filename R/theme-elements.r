theme_blank <- function() {
  structure(
    function(...) nullGrob(),
    class = "theme",
    type = "any",
    call = match.call()
  )  
}

# box
#   fill
#   (border) colour
#   (border) size
#   (border) linetype
theme_box <- function(fill = NA, colour = "black", size = 0.5, linetype = 1) {
  structure(
    function(x = 0.5, y = 0.5, width = 1, height = 1, ...) {
      rectGrob(
        x, y, width, height, ...,
        gp=gpar(size=unit(size, "mm"), col=colour, fill=fill, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

# text
#   family
#   face
#   size
#   colour
#   line height
#   angle
#   vjust
#   hjust
theme_text <- function(family = "", face = "plain", colour = "black", size = 10, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1.3) {
  structure(
    function(label, x = 0.5, y = 0.5, ...) {
      textGrob(
        label, x, y, hjust = hjust, vjust = vjust, ...,
        gp = gpar(
          fontsize = size, col = colour, 
          fontfamily = family, fontface = face, 
          lineheight = lineheight
        ),
        rot = angle
      )
    },
    class = "theme",
    type = "text",
    call = match.call()
  )
}

default_theme <- list(
  axis.box =        theme_blank(), 
  axis.x.title =    theme_text(),
  axis.y.title =    theme_text(angle = 90),
                    
  legend.box =      theme_box(), 
  legend.key =      theme_text(),
  legend.title =    theme_text(face = "bold"),
                    
  panel.box =       theme_box(fill = "grey90", colour="white", size=1), 
  panel.strip =     theme_box(fill = "grey80"), 
  panel.title =     theme_text(),
  # panel.axis = ?,
  # panel.grid.major = theme_segment(colour = "white"),
  # panel.grid.minor = theme_segment(colour = "grey95"),

  plot.box =         theme_box(colour = NA),
  plot.title =       theme_text(size = 16)

)


# plot.legend

# axis.ticks

# legend.keys

# panel.grid


