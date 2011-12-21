#' Smooth colour gradient between n colours
#' 
#' @inheritParams scales::gradient_n_pal
#' @inheritParams scale_colour_hue
#' @family colour scales
#' @rdname scale_gradientn
#' @export
#' @examples
#' # scale_colour_gradient make it easy to use existing colour palettes
#' 
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
#' (d <- qplot(x, y, data=dsub, colour=diff))
#' 
#' d + scale_colour_gradientn(colours = rainbow(7))
#' breaks <- c(-0.5, 0, 0.5)
#' d + scale_colour_gradientn(colours = rainbow(7), 
#'   breaks = breaks, labels = format(breaks))
#' 
#' d + scale_colour_gradientn(colours = topo.colors(10))
#' d + scale_colour_gradientn(colours = terrain.colors(10))
#' 
#' # You can force them to be symmetric by supplying a vector of 
#' # values, and turning rescaling off
#' max_val <- max(abs(dsub$diff))
#' values <- seq(-max_val, max_val, length = 11)
#' 
#' d + scale_colour_gradientn(colours = topo.colors(10), 
#'   values = values, rescale = function(x, ...) x, oob = identity)
#' d + scale_colour_gradientn(colours = terrain.colors(10), 
#'   values = values, rescale = function(x, ...) x, oob = identity)
scale_colour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50") {
  continuous_scale("colour", "gradientn", 
    gradient_n_pal(colours, values, space), na.value = na.value, ...)
}
#' @rdname scale_gradientn
#' @export
scale_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50") {
  continuous_scale("fill", "gradientn",
    gradient_n_pal(colours, values, space), na.value = na.value, ...)
}

icon.gradientn <- function(.) {
  g <- scale_fill_gradientn(colours = rainbow(7))
  g$train(1:5)
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
    gp=gpar(fill = g$map(1:5), col=NA)
  )
}
