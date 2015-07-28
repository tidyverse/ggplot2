#' Absolute grob
#'
#' This grob has fixed dimensions and position.
#'
#' It's still experimental
#'
#' @keywords internal
absoluteGrob <- function(grob, width = NULL, height = NULL,
  xmin = NULL, ymin = NULL, vp = NULL) {

  gTree(
    children = grob,
    width = width, height = height,
    xmin = xmin, ymin = ymin,
    vp = vp, cl = "absoluteGrob"
  )
}

#' @export
#' @method grobHeight absoluteGrob
grobHeight.absoluteGrob <- function(x) {
  x$height %||% grobHeight(x$children)
}
#' @export
#' @method grobWidth absoluteGrob
grobWidth.absoluteGrob <- function(x) {
  x$width %||%  grobWidth(x$children)
}

#' @export
#' @method grobX absoluteGrob
grobX.absoluteGrob <- function(x, theta) {
  if (!is.null(x$xmin) && theta == "west") return(x$xmin)
  grobX(x$children, theta)
}
#' @export
#' @method grobY absoluteGrob
grobY.absoluteGrob <- function(x, theta) {
  if (!is.null(x$ymin) && theta == "south") return(x$ymin)
  grobY(x$children, theta)
}

#' @export
#' @method grid.draw absoluteGrob
grid.draw.absoluteGrob <- function(x, recording = TRUE) {
  NextMethod()
}
