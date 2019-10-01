#' Continuous and binned colour scales
#'
#' Colour scales for continuous data default to the values of the
#' `ggplot2.continuous.colour` and `ggplot2.continuous.fill` options. If these
#' options are not present, `"gradient"` will be used. See [options()] for more
#' information.
#'
#' @param ... Additional parameters passed on to the scale type
#' @param type One of "gradient" (the default) or "viridis" indicating the
#'   colour scale to use
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
  switch(
    type,
    gradient = scale_colour_gradient(...),
    viridis = scale_colour_viridis_c(...),
    stop("Unknown scale type", call. = FALSE)
  )
}

#' @rdname scale_colour_continuous
#' @export
scale_fill_continuous <- function(...,
                                  type = getOption("ggplot2.continuous.fill", default = "gradient")) {
  switch(
    type,
    gradient = scale_fill_gradient(...),
    viridis = scale_fill_viridis_c(...),
    stop("Unknown scale type", call. = FALSE)
  )
}

#' @export
#' @rdname scale_colour_continuous
#' @usage NULL
scale_colour_binned <- function(...,
                                type = getOption("ggplot2.continuous.colour", default = "gradient")) {
  switch(
    type,
    gradient = scale_colour_steps(...),
    viridis = scale_colour_viridis_b(...),
    stop("Unknown scale type", call. = FALSE)
  )
}

#' @export
#' @rdname scale_colour_continuous
#' @usage NULL
scale_fill_binned <- function(...,
                              type = getOption("ggplot2.continuous.colour", default = "gradient")) {
  switch(
    type,
    gradient = scale_fill_steps(...),
    viridis = scale_fill_viridis_b(...),
    stop("Unknown scale type", call. = FALSE)
  )
}
