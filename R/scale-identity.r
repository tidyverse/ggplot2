#' Use values without scaling.
#'
#' @export scale_colour_identity scale_fill_identity scale_shape_identity
#'   scale_linetype_identity scale_alpha_identity scale_size_identity
#' @param ... Other arguments passed on to \code{\link{discrete_scale}}
#' @param legend Should this scale have a legend?  Defaults to \code{FALSE}
#' @examples
#' colour <- c("red", "green", "blue", "yellow")
#' qplot(1:4, 1:4, fill = colour, geom = "tile")
#' qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()
#' 
#' # To get a legend, you also need to supply labels, and specify legend = T
#' qplot(1:4, 1:4, fill = colour, geom = "tile") +
#'   scale_fill_identity("trt", labels = letters[1:4], breaks = colour, 
#'    legend = TRUE)
#' 
#' # cyl scaled to appropriate size
#' qplot(mpg, wt, data = mtcars, size = cyl)
#' 
#' # cyl used as point size
#' qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()
scale_colour_identity <- function(..., legend = FALSE) {
  identity_scale(discrete_scale("colour", "identity", identity_pal(), ...,
    legend = legend))
}
scale_fill_identity <- function(..., legend = FALSE) {
  identity_scale(discrete_scale("fill", "identity", identity_pal(), ...,
    legend = legend))
}
scale_shape_identity <- function(..., legend = FALSE) {
  identity_scale(discrete_scale("shape", "identity", identity_pal(), ...,
    legend = legend))
}
scale_linetype_identity <- function(..., legend = FALSE) {
  identity_scale(discrete_scale("linetype", "identity", identity_pal(), ...,
    legend = legend))
}

scale_alpha_identity <- function(..., legend = FALSE) {
  identity_scale(continuous_scale("alpha", "identity", identity_pal(), ...,
    legend = legend))
}
scale_size_identity <- function(..., legend = FALSE) {
  identity_scale(continuous_scale("size", "identity", identity_pal(), ...,
    legend = legend))
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
