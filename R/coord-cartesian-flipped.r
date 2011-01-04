#' Flipped cartesian coordinates.
#' 
#' Flipped cartesian coordinates so that horizontal becomes vertical, and
#' vertical, horizontal. This is primarily useful for converting geoms and
#' statistics which display y conditional on x, to x conditional on y.
#'
#' @name coord_flip
#' @export
#' @examples
#' # Very useful for creating boxplots, and other interval
#' # geoms in the horizontal instead of vertical position.
#' qplot(cut, price, data=diamonds, geom="boxplot")
#' last_plot() + coord_flip()
#'
#' qplot(cut, data=diamonds, geom="bar")
#' last_plot() + coord_flip()
#' 
#' qplot(carat, data=diamonds, geom="histogram")
#' last_plot() + coord_flip()
#'
#' # You can also use it to flip lines and area plots:
#' qplot(1:5, (1:5)^2, geom="line")
#' last_plot() + coord_flip()
CoordFlip <- proto(CoordCartesian, expr={
  objname <- "flip"

  
  transform <- function(., data, details) {
    rescale_x <- function(data) .$rescale_var(data, details$x.range)
    rescale_y <- function(data) .$rescale_var(data, details$y.range)
    
    data <- transform_position(data, rescale_y, rescale_x)
    data <- transform_position(data, trim_infinite_01, trim_infinite_01)
    
    rename(data, c(
      x = "y",       y = "x", 
      xend = "yend", yend = "xend", 
      xmin = "ymin", ymin = "xmin",
      xmax = "ymax", ymax = "xmax")
    )
  }

  compute_ranges <- function(., scales) {
    details <- .super$compute_ranges(., scales)
    with(details, list(
      x.range = y.range, y.range = x.range, 
      x.major = y.major, x.minor = y.minor, x.labels = y.labels,
      y.major = x.major, y.minor = x.minor, y.labels = x.labels
    ))
  }
  
  labels <- function(., scales) {
    list(
      x = scales$y,
      y = scales$x
    )
  }
  

  icon <- function(.) {
    angles <- seq(0, pi/2, length=20)[-c(1, 20)]
    gTree(children=gList(
      segmentsGrob(0, 0, 0, 1),
      segmentsGrob(0, 0, 1, 0),
      linesGrob(0.9 * sin(angles), 0.9 * cos(angles), arrow=arrow(length=unit(0.05, "npc"))),
      linesGrob(0.5 * sin(angles), 0.5 * cos(angles), arrow=arrow(end="first", length= unit(0.05, "npc")))
    ))
  }
})
