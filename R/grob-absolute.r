absoluteGrob <- function(grob, width = NULL, height = NULL) {
  grob(child = grob, width = width, height = height, cl="absoluteGrob")
}

grobHeight.absoluteGrob <- function(x) {
  nulldefault(x$height, grobHeight(x$child))
}
grobWidth.absoluteGrob <- function(x) {
  nulldefault(x$width, grobWidth(x$child))
}

grid.draw.absoluteGrob <- function(x, recording = TRUE) {
  x$child$vp <- x$vp
  grid.draw(x$child)
}