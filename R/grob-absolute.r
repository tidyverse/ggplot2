#' Absolute grob
#'
#' This grob has fixed dimesions and position.
#' 
#' It's still experimental
#' 
#' @S3method grobHeight absoluteGrob
#' @S3method grobWidth absoluteGrob
#' @S3method grobX absoluteGrob
#' @S3method grobY absoluteGrob
#' @S3method grid.draw absoluteGrob
#' @keywords internal
absoluteGrob <- function(grob, width = NULL, height = NULL,
  xmin = NULL, ymin = NULL, vp = NULL) {
  gTree(
    children = grob, 
    width = width, height = height, 
    xmin = xmin, ymin = ymin,
    vp = vp, cl="absoluteGrob"
  )
}

grobHeight.absoluteGrob <- function(x) {
  x$height %||% grobHeight(x$children)
}
grobWidth.absoluteGrob <- function(x) {
  x$width %||%  grobWidth(x$children)
}

grobX.absoluteGrob <- function(x, theta) {
  if (!is.null(x$xmin) && theta == "west") return(x$xmin)
  grobX(x$children, theta)
}
grobY.absoluteGrob <- function(x, theta) {
  if (!is.null(x$ymin) && theta == "south") return(x$ymin)
  grobY(x$children, theta)
}

grid.draw.absoluteGrob <- function(x, recording = TRUE) {
  grid:::drawGTree(x)
}
