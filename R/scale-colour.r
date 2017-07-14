#' Continuous colour scales
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
#'   [scale_fill_gradient()], and [scale_fill_viridis_c()]
#' @export
#' @rdname scale_colour_continuous
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
