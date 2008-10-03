# Absolute grob
# This grob has fixed dimesions and position.
# 
# It's still experimental
# 
# @alias grobHeight.absoluteGrob
# @alias grobWidth.absoluteGrob
# @alias grobX.absoluteGrob
# @alias grobY.absoluteGrob
# @alias grid.draw.absoluteGrob
# @keywords internal
absoluteGrob <- function(grob, width = NULL, height = NULL, xmin = NULL, ymin = NULL) {
  grob(
    child = grob, 
    width = width, height = height, 
    xmin = xmin, ymin = ymin,
    cl="absoluteGrob"
  )
}

grobHeight.absoluteGrob <- function(x) {
  nulldefault(x$height, grobHeight(x$child))
}
grobWidth.absoluteGrob <- function(x) {
  nulldefault(x$width, grobWidth(x$child))
}

grobX.absoluteGrob <- function(x, theta) {
  if (!is.null(x$xmin) && theta == "west") return(x$xmin)
  grobX(x$child, theta)
}
grobY.absoluteGrob <- function(x, theta) {
  if (!is.null(x$ymin) && theta == "south") return(x$ymin)
  grobY(x$child, theta)
}

grid.draw.absoluteGrob <- function(x, recording = TRUE) {
  x$child$vp <- x$vp
  grid.draw(x$child)
}