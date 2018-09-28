#' Create your own discrete scale
#'
#' These functions allow you to specify your own set of mappings from levels in the
#' data to aesthetic values.
#'
#' The functions `scale_colour_manual()`, `scale_fill_manual()`, `scale_size_manual()`,
#' etc. work on the aesthetics specified in the scale name: `colour`, `fill`, `size`,
#' etc. However, the functions `scale_colour_manual()` and `scale_fill_manual()` also
#' have an optional `aesthetics` argument that can be used to define both `colour` and
#' `fill` aesthetic mappings via a single function call (see examples). The function
#' `scale_discrete_manual()` is a generic scale that can work with any aesthetic or set
#' of aesthetics provided via the `aesthetics` argument.
#'
#' @inheritParams scale_x_discrete
#' @inheritDotParams discrete_scale -expand -position -aesthetics
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
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
#' # You can set color and fill aesthetics at the same time
#' ggplot(
#'   mtcars,
#'   aes(mpg, wt, colour = factor(cyl), fill = factor(cyl))
#' ) +
#'   geom_point(shape = 21, alpha = 0.5, size = 2) +
#'   scale_colour_manual(
#'     values = cols,
#'     aesthetics = c("colour", "fill")
#'   )
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
scale_colour_manual <- function(..., values, aesthetics = "colour") {
  manual_scale(aesthetics, values, ...)
}

#' @rdname scale_manual
#' @export
scale_fill_manual <- function(..., values, aesthetics = "fill") {
  manual_scale(aesthetics, values, ...)
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

#' @rdname scale_manual
#' @export
scale_discrete_manual <- function(aesthetics, ..., values) {
  manual_scale(aesthetics, values, ...)
}


manual_scale <- function(aesthetic, values = NULL, ...) {
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (rlang::is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n, " needed but only ",
        length(values), " provided.", call. = FALSE)
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, ...)
}
