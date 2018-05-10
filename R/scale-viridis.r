#' Viridis colour scales from viridisLite
#'
#' The `viridis` scales provide color maps that are perceptually uniform in both
#' color and black-and-white. They are also designed to be perceived by viewers
#' with common forms of color blindness. See also
#' <https://bids.github.io/colormap/>.
#'
#' @inheritParams viridisLite::viridis
#' @inheritParams scales::gradient_n_pal
#' @inheritParams continuous_scale
#' @param ... Other arguments passed on to [discrete_scale()] or
#' [continuous_scale()] to control name, limits, breaks, labels and so forth.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @family colour scales
#' @rdname scale_viridis
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'   geom_point(aes(colour = clarity)))
#' d + scale_colour_viridis_d()
#'
#' # Change scale label
#' d + scale_colour_viridis_d("Diamond\nclarity")
#'
#' # Select palette to use, see ?scales::viridis_pal for more details
#' d + scale_colour_viridis_d(option = "plasma")
#' d + scale_colour_viridis_d(option = "inferno")
#'
#' \donttest{
#' # scale_fill_viridis_d works just the same as
#' # scale_colour_viridis_d but for fill colours
#' p <- ggplot(diamonds, aes(x = price, fill = cut)) +
#'   geom_histogram(position = "dodge", binwidth = 1000)
#' p + scale_fill_viridis_d()
#' # the order of colour can be reversed
#' p + scale_fill_viridis_d(direction = -1)
#' }
#'
#' # Use viridis_c with continous data
#' v <- ggplot(faithfuld) +
#'   geom_tile(aes(waiting, eruptions, fill = density))
#' v
#' v + scale_fill_viridis_c()
#' v + scale_fill_viridis_c(option = "plasma")
scale_colour_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", aesthetics = "colour") {
  discrete_scale(
    aesthetics,
    "viridis_d",
    viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}

#' @export
#' @rdname scale_viridis
scale_fill_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                 direction = 1, option = "D", aesthetics = "fill") {
  discrete_scale(
    aesthetics,
    "viridis_d",
    viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}

#' @export
#' @rdname scale_viridis
scale_colour_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = "colourbar", aesthetics = "colour") {
  continuous_scale(
    aesthetics,
    "viridis_c",
    gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_viridis
scale_fill_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                 direction = 1, option = "D", values = NULL,
                                 space = "Lab", na.value = "grey50",
                                 guide = "colourbar", aesthetics = "fill") {
  continuous_scale(
    aesthetics,
    "viridis_c",
    gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}
