#' Create your own discrete scale.
#' 
#' @rdname scale_manual
#' @inheritParams scale_x_discrete
#' @param values a set of aesthetic values to map data values to.  If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical, but
#'   could be modified with the limits argument to the scale).  Any data
#'   values that don't match, will be given \code{na.value}.
#' @export
#' @examples
#' \donttest{
#' p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))
#' 
#' p + scale_colour_manual(values = c("red","blue", "green"))
#' p + scale_colour_manual(
#'   values = c("8" = "red","4" = "blue","6" = "green"))
#' # With rgb hex values
#' p + scale_colour_manual(values = c("#FF0000", "#0000FF", "#00FF00"))
#' 
#' # As with other scales you can use breaks to control the appearance
#' # of the legend
#' cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
#' p + scale_colour_manual(values = cols)
#' p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
#' p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
#' p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
#'   labels = c("four", "six", "eight"))
#' 
#' # And limits to control the possible values of the scale
#' p + scale_colour_manual(values = cols, limits = c("4", "8"))
#' p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))
#' }
scale_colour_manual <- function(..., values) {
  manual_scale("colour", values, ...)
}

#' @rdname scale_manual
#' @export
scale_fill_manual <- function(..., values) {
  manual_scale("fill", values, ...)
}

#' @rdname scale_manual
#' @export
scale_size_manual <- function(..., values) {
  manual_scale("size", values, ...)
}

#' @rdname scale_manual
#' @export
scale_shape_manual <- function(..., values) {
  manual_scale("shape", values, ...)
}

#' @rdname scale_manual
#' @export
scale_linetype_manual <- function(..., values) {
  manual_scale("linetype", values, ...)
}

#' @rdname scale_manual
#' @export
scale_alpha_manual <- function(..., values) {
  manual_scale("alpha", values, ...)
}

icon.manual <- function() textGrob("DIY", gp=gpar(cex=1.2))

manual_scale <- function(aesthetic, values, ...) {
  pal <- function(n) values
  discrete_scale(aesthetic, "manual", pal, ...)
}

