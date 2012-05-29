#' Sequential grey colour scale.
#'
#' Based on \code{\link{gray.colors}}
#'
#' @inheritParams scales::grey_pal
#' @inheritParams scale_colour_hue
#' @family colour scales
#' @rdname scale_grey
#' @export
#' @examples
#' p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
#' p + scale_colour_grey()
#' p + scale_colour_grey(end = 0)
#' 
#' # You may want to turn off the pale grey background with this scale
#' p + scale_colour_grey() + theme_bw()
#'
#' # Colour of missing values is controlled with na.value:
#' miss <- factor(sample(c(NA, 1:5), nrow(mtcars), rep = TRUE))
#' qplot(mpg, wt, data = mtcars, colour = miss) + scale_colour_grey()
#' qplot(mpg, wt, data = mtcars, colour = miss) + 
#'   scale_colour_grey(na.value = "green")
scale_colour_grey <- function(..., start = 0.2, end = 0.8, na.value = "red") {
  discrete_scale("colour", "grey", grey_pal(start, end), 
    na.value = na.value, ...)
}

#' @rdname scale_grey
#' @export
scale_fill_grey <- function(..., start = 0.2, end = 0.8, na.value = "grey50") {
  discrete_scale("fill", "grey", grey_pal(start, end), 
    na.value = na.value, ...)
}
