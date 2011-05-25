#' Use values without scaling.
#'
#' @export scale_colour_identity scale_fill_identity scale_shape_identity
#'   scale_linetype_identity scale_alpha_identity scale_size_identity
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
  identity_scale(discrete_scale("colour", "identity", identity_pal()), ...)
}
scale_fill_identity <- function(...) {
  identity_scale(discrete_scale("fill", "identity", identity_pal()), ...)
}
scale_shape_identity <- function(...) {
  identity_scale(discrete_scale("shape", "identity", identity_pal()), ...)
}
scale_linetype_identity <- function(...) {
  identity_scale(discrete_scale("linetype", "identity", identity_pal()), ...)
}

scale_alpha_identity <- function(...) {
  identity_scale(continuous_scale("alpha", "identity", identity_pal()), ...)
}
scale_size_identity <- function(...) {
  identity_scale(continuous_scale("size", "identity", identity_pal()), ...)
}

identity_scale <- function(x) {
  structure(x, class = c("identity", class(x)))
}

#' @S3method scale_map identity
scale_map.identity <- function(scale, x) {
  if (is.factor(x)) {
    as.character(x)
  } else {
    x
  }
}

icon.identity <- function() textGrob("f(x) = x", gp=gpar(cex=1.2))
