# Email Paul:  absolute vs relative grobs
# Exact grob heights
# Computing max and min at creation where possible

# Theme element: blank
# This theme element draws nothing, and assigns no space
# 
# @keyword dplot
theme_blank <- function() {
  structure(
    function(...) zeroGrob(),
    class = "theme",
    type = "any",
    call = match.call()
  )  
}

# Theme element: rectangle
# This element draws a rectangular box
# 
# This is most often used for backgrounds and borders
# 
# @seealso \code{\link{rectGrob}} for underlying grid function
# @arguments fill colour
# @arguments border color
# @arguments border size
# @arguments border linetype
# @keyword dplot
theme_rect <- function(fill = NA, colour = "black", size = 0.5, linetype = 1) {
  structure(
    function(x = 0.5, y = 0.5, width = 1, height = 1, ...) {
      rectGrob(
        x, y, width, height, ...,
        gp=gpar(lwd=size * .pt, col=colour, fill=fill, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

# Theme element: line
# This element draws a line between two (or more) points
# 
# @seealso \code{\link{polylineGrob}} for underlying grid function, \code{link{theme_segment}}
# @arguments line color
# @arguments line size
# @arguments line type
# @keyword dplot
theme_line <- function(colour = "black", size = 0.5, linetype = 1) {
  structure(
    function(x = 0:1, y = 0:1, ..., default.units = "npc") {
      polylineGrob(
        x, y, ..., default.units = default.units,
        gp=gpar(lwd=size * .pt, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "line",
    call = match.call()
  )
}

# Theme element: segments
# This element draws segments between a set of points
# 
# @seealso \code{\link{segmentsGrob}} for underlying grid function, \code{link{theme_line}}
# @arguments line color
# @arguments line size
# @arguments line type
# @keyword dplot
theme_segment <- function(colour = "black", size = 0.5, linetype = 1) {
  structure(
    function(x0 = 0, y0 = 0, x1 = 1, y1 = 1, ...) {
      segmentsGrob(
        x0, y0, x1, y1, ..., default.units = "npc",
        gp=gpar(col=colour, lty=linetype, lwd = size * .pt),
      )
    },
    class = "theme",
    type = "segment",
    call = match.call()
  )
}


# Theme element: text
# This element adds text
# 
# @seealso \code{\link{textGrob}} for underlying grid function
# @arguments font family
# @arguments font face ("plain", "italic", "bold")
# @arguments text colour
# @arguments text size (in pts)
# @arguments horizontal justification (in [0, 1])
# @arguments vertical justification (in [0, 1])
# @arguments angle (in [0, 360])
# @arguments line height
# @keyword dplot
theme_text <- function(family = "", face = "plain", colour = "black", size = 10, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1.1) {

  vj <- vjust
  hj <- hjust
  angle <- angle %% 360
  
  if (angle == 90) {
    xp <- vj
    yp <- hj
  } else if (angle == 180) {
    xp <- 1 - hj
    yp <- vj
  } else if (angle == 270) {
    xp <- vj
    yp <- 1 - hj
  }else {
    xp <- hj
    yp <- vj
  }

  structure(
    function(label, x = xp, y = yp, ..., vjust = vj, hjust = hj, default.units = "npc") {

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
