#' Create your own discrete scale
#'
#' This allows you to specify you own set of mappings from levels in the
#' data to aesthetic values.
#'
#' @inheritParams scale_x_discrete
#' @inheritDotParams discrete_scale -expand -position
#' @param values a set of aesthetic values to map data values to. If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale. Any data values that don't match will be
#'   given `na.value`.
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl)))
#' p + scale_colour_manual(values = c("red", "blue", "green"))
#'
#' # It's recommended to use a named vector
#' cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
#' p + scale_colour_manual(values = cols)
#'
#' # As with other scales you can use breaks to control the appearance
#' # of the legend.
#' p + scale_colour_manual(values = cols)
#' p + scale_colour_manual(
#'   values = cols,
#'   breaks = c("4", "6", "8"),
#'   labels = c("four", "six", "eight")
#' )
#'
#' # And limits to control the possible values of the scale
#' p + scale_colour_manual(values = cols, limits = c("4", "8"))
#' p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))
#' @name scale_manual
#' @aliases NULL
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
