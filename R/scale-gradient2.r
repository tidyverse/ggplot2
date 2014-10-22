#' Diverging colour gradient
#'
#' @inheritParams scale_colour_hue
#' @inheritParams scales::div_gradient_pal
#' @param midpoint The midpoint (in data value) of the diverging scale.
#'   Defaults to 0.
#' @param guide Type of legend. Use \code{"colourbar"} for continuous
#'   colour bar, or \code{"legend"} for discrete colour legend.
#' @family colour scales
#' @rdname scale_gradient2
#' @export
#' @examples
#' \donttest{
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
#' (d <- qplot(x, y, data=dsub, colour=diff))
#'
#' d + scale_colour_gradient2()
#' # Change scale name
#' d + scale_colour_gradient2(expression(sqrt(abs(x - y))))
#' d + scale_colour_gradient2("Difference\nbetween\nwidth and\nheight")
#'
#' # Change limits and colours
#' d + scale_colour_gradient2(limits=c(-0.2, 0.2))
#'
#' # Using "muted" colours makes for pleasant graphics
#' # (and they have better perceptual properties too)
#' library(scales) # for muted
#' d + scale_colour_gradient2(low="red", high="blue")
#' d + scale_colour_gradient2(low=muted("red"), high=muted("blue"))
#'
#' # Using the Lab colour space also improves perceptual properties
#' # at the price of slightly slower operation
#' d + scale_colour_gradient2(space="Lab")
#'
#' # About 5% of males are red-green colour blind, so it's a good
#' # idea to avoid that combination
#' d + scale_colour_gradient2(high=muted("green"))
#'
#' # We can also make the middle stand out
#' d + scale_colour_gradient2(mid=muted("green"), high="white", low="white")
#'
#' # or use a non zero mid point
#' (d <- qplot(carat, price, data=diamonds, colour=price/carat))
#' d + scale_colour_gradient2(midpoint=mean(diamonds$price / diamonds$carat))
#'
#' # Fill gradients work much the same way
#' p <- qplot(letters[1:5], 1:5, fill= c(-3, 3, 5, 2, -2), geom = "bar",
#'   stat = "identity")
#' p + scale_fill_gradient2("fill")
#' # Note how positive and negative values of the same magnitude
#' # have similar intensity
#' }
scale_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar") {
  continuous_scale("colour", "gradient2",
    div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
    rescaler = mid_rescaler(mid = midpoint))
}

#' @rdname scale_gradient2
#' @export
scale_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar") {
  continuous_scale("fill", "gradient2",
    div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
    rescaler = mid_rescaler(mid = midpoint))
}

mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    rescale_mid(x, to, from, mid)
  }
}
