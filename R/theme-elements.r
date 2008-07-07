# Email Paul:  absolute vs relative grobs
# Exact grob heights
# Computing max and min at creation where possible

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
theme_rect <- function(fill = NA, colour = "black", size = 0.5, linetype = 1) {
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

theme_line <- function(colour = "black", size = 0.5, linetype = 1) {
  structure(
    function(x = 0:1, y = 0:1, ..., default.units = "npc") {
      polylineGrob(
        x, y, ..., default.units = default.units,
        gp=gpar(size=unit(size, "mm"), col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "line",
    call = match.call()
  )
}

theme_segment <- function(colour = "black", size = 0.5, linetype = 1) {
  structure(
    function(x0 = 0, y0 = 0, x1 = 1, y1 = 1, ...) {
      segmentsGrob(
        x0, y0, x1, y1, ..., default.units = "npc",
        gp=gpar(size=unit(size, "mm"), col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "segment",
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
  vj <- vjust
  hj <- hjust
  
  structure(
    function(label, x = 0.5, y = 0.5, ..., vjust = vj, hjust = hj, default.units = "npc") {
      textGrob(
        label, x, y, hjust = hjust, vjust = vjust, ...,
        default.units = default.units,
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


theme_axis_ticks <- function(length = unit(0.4, "lines"), margin = unit(0.2, "lines")) {
  
  function(at, position = "top") {
    at <- unit(at, "native")

    grob <- switch(position,
      top =    ,
      bottom = segmentsGrob(at, margin, at, margin + length),
      left =   ,
      right =  segmentsGrob(margin, at, margin + length, at),
    )

    vp <- switch(position,
      top =    ,
      bottom = viewport(height = margin + length),
      left =   ,
      right =  viewport(width = margin + length),
    )
    grobTree(grob, vp = vp)
  }
}

fixed_width <- function(grob) {
}

# plot.legend
# axis.ticks
# legend.keys


# grid function to take grob and add:
#   * padding
#   * background 
#   * border
#   * margins


