#' Smooth gradient between two colours
#'
#' Default colours are generated with \pkg{munsell} and
#' \code{mnsl(c("2.5PB 2/4", "2.5PB 7/10")}. Generally, for continuous
#' colour scales you want to keep hue constant, but vary chroma and
#' luminance. The \pkg{munsell} package makes this easy to do using the
#' Munsell colour system.
#'
#' @inheritParams scale_colour_hue
#' @inheritParams scales::seq_gradient_pal
#' @param guide Type of legend. Use \code{"colourbar"} for continuous
#'   colour bar, or \code{"legend"} for discrete colour legend.
#' @seealso \code{\link[scales]{seq_gradient_pal}} for details on underlying
#'   palette
#' @rdname scale_gradient
#' @family colour scales
#' @export
#' @examples
#' \donttest{
#' # It's hard to see, but look for the bright yellow dot
#' # in the bottom right hand corner
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' (d <- qplot(x, y, data=dsub, colour=z))
#' # That one point throws our entire scale off.  We could
#' # remove it, or manually tweak the limits of the scale
#'
#' # Tweak scale limits.  Any points outside these limits will not be
#' # plotted, and will not affect the calculation of statistics, etc
#' d + scale_colour_gradient(limits=c(3, 10))
#' d + scale_colour_gradient(limits=c(3, 4))
#' # Setting the limits manually is also useful when producing
#' # multiple plots that need to be comparable
#'
#' # Alternatively we could try transforming the scale:
#' d + scale_colour_gradient(trans = "log")
#' d + scale_colour_gradient(trans = "sqrt")
#'
#' # Other more trivial manipulations, including changing the name
#' # of the scale and the colours.
#'
#' d + scale_colour_gradient("Depth")
#' d + scale_colour_gradient(expression(Depth[mm]))
#'
#' d + scale_colour_gradient(limits=c(3, 4), low="red")
#' d + scale_colour_gradient(limits=c(3, 4), low="red", high="white")
#' # Much slower
#' d + scale_colour_gradient(limits=c(3, 4), low="red", high="white", space="Lab")
#' d + scale_colour_gradient(limits=c(3, 4), space="Lab")
#'
#' # scale_fill_continuous works similarly, but for fill colours
#' (h <- qplot(x - y, data=dsub, geom="histogram", binwidth=0.01, fill=..count..))
#' h + scale_fill_continuous(low="black", high="pink", limits=c(0,3100))
#'
#' # Colour of missing values is controlled with na.value:
#' miss <- sample(c(NA, 1:5), nrow(mtcars), rep = TRUE)
#' qplot(mpg, wt, data = mtcars, colour = miss)
#' qplot(mpg, wt, data = mtcars, colour = miss) +
#'   scale_colour_gradient(na.value = "black")
#' }
scale_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") {
  continuous_scale("colour", "gradient", seq_gradient_pal(low, high, space),
    na.value = na.value, guide = guide, ...)
}

#' @rdname scale_gradient
#' @export
scale_fill_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") {
  continuous_scale("fill", "gradient", seq_gradient_pal(low, high, space),
    na.value = na.value, guide = guide, ...)
}
