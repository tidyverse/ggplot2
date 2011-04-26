#' Absolute grob
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
absoluteGrob <- function(grob, width = NULL, height = NULL, xmin = NULL, ymin = NULL) {
  gTree(
    children = grob, 
    width = width, height = height, 
    xmin = xmin, ymin = ymin,
    cl="absoluteGrob"
  )
}

grobHeight.absoluteGrob <- function(x) {
  nulldefault(x$height, grobHeight(x$children))
}
grobWidth.absoluteGrob <- function(x) {
  nulldefault(x$width, grobWidth(x$children))
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


## gTree with absolute size specification
##
## this should be removed in future.
sizedGTree <- function(...) {
  gTree(..., cl=c("sizedGTree"))
}
grobHeight.sizedGTree <- function(x) {
  nulldefault(x$height, nulldefault(x$vp$height, nulldefault(sum(x$vp$layout$heights), grobHeight(x$children))))
}
grobWidth.sizedGTree <- function(x) {
  nulldefault(x$width, nulldefault(x$vp$width, nulldefault(sum(x$vp$layout$widths), grobWidth(x$children))))
}

if(0){
grobX.sizedGTree <- function(x, theta) {
  if (!is.null(x$xmin) && theta == "west") return(x$xmin)
  grobX(x$children, theta)
}
grobY.sizedGTree <- function(x, theta) {
  if (!is.null(x$ymin) && theta == "south") return(x$ymin)
  grobY(x$children, theta)
}
}
grid.draw.sizedGTree <- function(x, recording = TRUE) {
  grid:::drawGTree(x)
}
