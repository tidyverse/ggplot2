theme_blank <- function() {
  structure(
    function(...) {},
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
    function(x, y, width, height, ..., vp) {
      rectGrob(
        x, y, width, height, ..., vp = vp,
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
theme_text <- function(family = "", face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle, lineheight = 1.3) {
  structure(
    function(text, x, y) {
      textGrob(
        text, x, y, hjust = hjust, vjust = vjust, ..., vp = vp,
        gp = gpar(
          size = size, col = colour, 
          fontfamily = family, fontfact = face, 
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
  axis.box =          theme_blank(), 
  axis.text =         theme_text(size = 10, colour="grey50"),
  axis.title.box =    theme_blank(), 
  axis.title.text =   theme_text(), 

  legend.box =        theme_box(), 
  legend.text =       theme_text(),
  legend.title.box =  theme_blank(), 
  legend.title.text = theme_text(face = "bold"),

  panel.box =         theme_box(fill = "grey90", colour="white", size=1), 
  panel.title.box  =  theme_box(fill = "grey80"), 
  panel.title.text =  theme_text(),
  # panel.axis = ?,
  # panel.grid.major = theme_segment(colour = "white"),
  # panel.grid.minor = theme_segment(colour = "grey95"),

  plot.box =          theme_box(colour = NA),
  plot.title.box =    theme_blank(),
  plot.title.text =   theme_text(size = 14)

)


# plot.legend

# axis.ticks

# legend.keys

# panel.grid


