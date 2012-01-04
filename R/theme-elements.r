#' Theme element: blank.
#' This theme element draws nothing, and assigns no space
#' 
#' @keywords dplot
#' @export
theme_blank <- function() {
  structure(
    function(...) zeroGrob(),
    class = "theme",
    type = "any",
    call = match.call()
  )  
}

#' Theme element: rectangle.
#' 
#' Most often used for backgrounds and borders.
#' 
#' @seealso \code{\link{rectGrob}} for underlying grid function
#' @param fill fill colour
#' @param colour border color
#' @param size border size
#' @param linetype border linetype
#' @keywords dplot
#' @export
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

#' Theme element: line.
#'
#' This element draws a line between two (or more) points
#' 
#' @seealso \code{\link{polylineGrob}} for underlying grid function,
#'  \code{link{theme_segment}}
#' @param colour line color
#' @param size line size
#' @param linetype line type
#' @keywords dplot
#' @export
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

#' Theme element: segments.
#'
#' This element draws segments between a set of points
#' 
#' @seealso \code{\link{segmentsGrob}} for underlying grid function,
#'   \code{link{theme_line}}
#' @param colour line color
#' @param size line size
#' @param linetype line type
#' @keywords dplot
#' @export
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


#' Theme element: text.
#' 
#' @seealso \code{\link{textGrob}} for underlying grid function
#' @param family font family
#' @param face font face ("plain", "italic", "bold")
#' @param colour text colour
#' @param size text size (in pts)
#' @param hjust horizontal justification (in [0, 1])
#' @param vjust vertical justification (in [0, 1])
#' @param angle angle (in [0, 360])
#' @param lineheight line height
#' @keywords dplot
#' @export
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
