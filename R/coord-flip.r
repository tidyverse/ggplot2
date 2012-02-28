#' Flipped cartesian coordinates.
#' 
#' Flipped cartesian coordinates so that horizontal becomes vertical, and
#' vertical, horizontal. This is primarily useful for converting geoms and
#' statistics which display y conditional on x, to x conditional on y.
#'
#' @export
#' @param ... Other arguments passed onto \code{\link{coord_cartesian}}
#' @examples
#' \donttest{
#' # Very useful for creating boxplots, and other interval
#' # geoms in the horizontal instead of vertical position.
#' qplot(cut, price, data=diamonds, geom="boxplot")
#' last_plot() + coord_flip()
#'
#' qplot(cut, data=diamonds, geom="bar")
#' last_plot() + coord_flip()
#' 
#' h <- qplot(carat, data=diamonds, geom="histogram")
#' h
#' h + coord_flip()
#' h + coord_flip() + scale_x_reverse()
#'
#' # You can also use it to flip lines and area plots:
#' qplot(1:5, (1:5)^2, geom="area")
#' last_plot() + coord_flip()
#' }
coord_flip <- function(...) {
  coord <- coord_cartesian(...)
  structure(coord, class = c("flip", class(coord)))
}

flip_labels <- function(x) {
  old_names <- names(x)

  new_names <- old_names
  new_names <- gsub("^x", "z", new_names)
  new_names <- gsub("^y", "x", new_names)
  new_names <- gsub("^z", "y", new_names)
  
  setNames(x, new_names)
}

#' @S3method is.linear flip
is.linear.flip <- function(coord) TRUE

#' @S3method coord_transform flip
coord_transform.flip <- function(coord, data, details) {
  data <- flip_labels(data)  
  NextMethod()
}

#' @S3method coord_range flip
coord_range.flip <- function(coord, scales) {
  return(list(x = scales$y.range, y = scales$x.range))
}

#' @S3method coord_train flip
coord_train.flip <- function(coord, scales) {
  flip_labels(NextMethod())
}

#' @S3method coord_labels flip
coord_labels.flip <- function(coord, scales) {
  flip_labels(NextMethod())
}

icon.flip <- function(.) {
  angles <- seq(0, pi/2, length=20)[-c(1, 20)]
  gTree(children=gList(
    segmentsGrob(0, 0, 0, 1),
    segmentsGrob(0, 0, 1, 0),
    linesGrob(0.9 * sin(angles), 0.9 * cos(angles), arrow=arrow(length=unit(0.05, "npc"))),
    linesGrob(0.5 * sin(angles), 0.5 * cos(angles), arrow=arrow(ends="first", length= unit(0.05, "npc")))
  ))
}
