#' Use values without scaling.
#'
#' @examples
#' colour <- c("red", "green", "blue", "yellow")
#' qplot(1:4, 1:4, fill = colour, geom = "tile")
#' qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()
#' 
#' # To get a legend, you also need to supply the labels to
#' # be used on the legend
#' qplot(1:4, 1:4, fill = colour, geom = "tile") +
#'   scale_fill_identity("trt", labels = letters[1:4], breaks = colour)
#' 
#' # cyl scaled to appropriate size
#' qplot(mpg, wt, data = mtcars, size = cyl)
#' 
#' # cyl used as point size
#' qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()
scale_colour_identity <- function(...) {
  discrete_scale("colour", "identity", identity_pal())
}
scale_fill_identity <- function(...) {
  discrete_scale("fill", "identity", identity_pal())
}
scale_shape_identity <- function(...) {
  discrete_scale("shape", "identity", identity_pal())
}
scale_linetype_identity <- function(...) {
  discrete_scale("linetype", "identity", identity_pal())
}

scale_alpha_identity <- function(...) {
  continuous_scale("alpha", "identity", identity_pal())
}
scale_size_identity <- function(...) {
  continuous_scale("size", "identity", identity_pal())
}

icon.identity <- function() textGrob("f(x) = x", gp=gpar(cex=1.2))
