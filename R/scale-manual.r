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
#' @param values a set of aesthetic values to map data values to. The values
#'   will be matched in order (usually alphabetical) with the limits of the
#'   scale, or with `breaks` if provided. If this is a named vector, then the
#'   values will be matched based on the names instead. Data values that don't
#'   match will be given `na.value`.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks (the scale limits)
#'   - A character vector of breaks
#'   - A function that takes the limits as input and returns breaks
#'     as output
#' @section Color Blindness:
#' Many color palettes derived from RGB combinations (like the "rainbow" color
#' palette) are not suitable to support all viewers, especially those with
#' color vision deficiencies. Using `viridis` type, which is perceptually
#' uniform in both colour and black-and-white display is an easy option to
#' ensure good perceptive properties of your visulizations.
#' The colorspace package offers functionalities
#' - to generate color palettes with good perceptive properties,
#' - to analyse a given color palette, like emulating color blindness,
#' - and to modify a given color palette for better perceptivity.
#'
#' For more information on color vision deficiencies and suitable color choices
#' see the [paper on the colorspace package](https://arxiv.org/abs/1903.06490)
#' and references therein.
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
scale_colour_manual <- function(..., values, aesthetics = "colour", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}

#' @rdname scale_manual
#' @export
scale_fill_manual <- function(..., values, aesthetics = "fill", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}

#' @rdname scale_manual
#' @export
scale_size_manual <- function(..., values, breaks = waiver()) {
  manual_scale("size", values, breaks, ...)
}

#' @rdname scale_manual
#' @export
scale_shape_manual <- function(..., values, breaks = waiver()) {
  manual_scale("shape", values, breaks, ...)
}

#' @rdname scale_manual
#' @export
scale_linetype_manual <- function(..., values, breaks = waiver()) {
  manual_scale("linetype", values, breaks, ...)
}

#' @rdname scale_manual
#' @export
scale_alpha_manual <- function(..., values, breaks = waiver()) {
  manual_scale("alpha", values, breaks, ...)
}

#' @rdname scale_manual
#' @export
scale_discrete_manual <- function(aesthetics, ..., values, breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}


manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), ...) {
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  # order values according to breaks
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[1:length(values)]
    }
  }

  pal <- function(n) {
    if (n > length(values)) {
      abort(glue("Insufficient values in manual scale. {n} needed but only {length(values)} provided."))
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, breaks = breaks, ...)
}
