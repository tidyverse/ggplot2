#' Continuous and binned colour scales
#'
#' Colour scales for continuous data default to the values of the
#' `ggplot2.continuous.colour` and `ggplot2.continuous.fill` options. These
#' [options()] default to `"gradient"` (i.e., [scale_colour_gradient()] and
#' [scale_fill_gradient()])
#'
#' @param ... Additional parameters passed on to the scale type
#' @param type One of the following:
#'   * "gradient" (the default)
#'   * "viridis"
#'   * A function that returns a continuous colour scale.
#' @seealso [scale_colour_gradient()], [scale_colour_viridis_c()],
#'   [scale_colour_steps()], [scale_colour_viridis_b()], [scale_fill_gradient()],
#'   [scale_fill_viridis_c()], [scale_fill_steps()], and [scale_fill_viridis_b()]
#' @export
#' @rdname scale_colour_continuous
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
#' v <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#' geom_tile()
#' v
#'
#' v + scale_fill_continuous(type = "gradient")
#' v + scale_fill_continuous(type = "viridis")
#'
#' # The above are equivalent to
#' v + scale_fill_gradient()
#' v + scale_fill_viridis_c()
scale_colour_continuous <- function(...,
                                    type = getOption("ggplot2.continuous.colour", default = "gradient")) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_colour_gradient(...)
  } else if (identical(type, "viridis")) {
    scale_colour_viridis_c(...)
  } else {
    abort("Unknown scale type")
  }
}

#' @rdname scale_colour_continuous
#' @export
scale_fill_continuous <- function(...,
                                  type = getOption("ggplot2.continuous.fill", default = "gradient")) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_fill_gradient(...)
  } else if (identical(type, "viridis")) {
    scale_fill_viridis_c(...)
  } else {
    abort("Unknown scale type")
  }
}

#' @export
#' @rdname scale_colour_continuous
#' @usage NULL
scale_colour_binned <- function(...,
                                type = getOption("ggplot2.binned.colour", default = getOption("ggplot2.continuous.colour", default = "gradient"))) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_colour_steps(...)
  } else if (identical(type, "viridis")) {
    scale_colour_viridis_b(...)
  } else {
    abort("Unknown scale type")
  }
}

#' @export
#' @rdname scale_colour_continuous
#' @usage NULL
scale_fill_binned <- function(...,
                              type = getOption("ggplot2.binned.fill", default = getOption("ggplot2.continuous.fill", default = "gradient"))) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_fill_steps(...)
  } else if (identical(type, "viridis")) {
    scale_fill_viridis_b(...)
  } else {
    abort("Unknown scale type")
  }
}
