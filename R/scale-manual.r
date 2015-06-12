#' Create your own discrete scale.
#'
#' @name scale_manual
#' @inheritParams scale_x_discrete
#' @param values a set of aesthetic values to map data values to.  If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale.  Any data values that don't match will be
#'   given \code{na.value}.
#' @examples
#' \donttest{
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl)))
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
#'
#' # Notice that the values are matched with limits, and not breaks
#' p + scale_colour_manual(limits = c(6, 8, 4), breaks = c(8, 4, 6),
#'   values = c("grey50", "grey80", "black"))
#' }
NULL

#' @rdname scale_manual
#' @export
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


manual_scale <- function(aesthetic, values, ...) {
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n, " needed but only ",
        length(values), " provided.", call. = FALSE)
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, ...)
}
