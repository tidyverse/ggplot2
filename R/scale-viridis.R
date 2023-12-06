#' Viridis colour scales from viridisLite
#'
#' The `viridis` scales provide colour maps that are perceptually uniform in both
#' colour and black-and-white. They are also designed to be perceived by viewers
#' with common forms of colour blindness. See also
#' <https://bids.github.io/colormap/>.
#'
#' @inheritParams scales::pal_viridis
#' @inheritParams scales::pal_gradient_n
#' @inheritParams continuous_scale
#' @param ... Other arguments passed on to [discrete_scale()],
#' [continuous_scale()], or [binned_scale()] to control name, limits, breaks,
#'   labels and so forth.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @family colour scales
#' @seealso
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#' @rdname scale_viridis
#' @export
#' @examples
#' # viridis is the default colour/fill scale for ordered factors
#' set.seed(596)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' ggplot(dsamp, aes(carat, price)) +
#'   geom_point(aes(colour = clarity))
#'
#' # Use viridis_d with discrete data
#' txsamp <- subset(txhousing, city %in%
#'   c("Houston", "Fort Worth", "San Antonio", "Dallas", "Austin"))
#' (d <- ggplot(data = txsamp, aes(x = sales, y = median)) +
#'    geom_point(aes(colour = city)))
#' d + scale_colour_viridis_d()
#'
#' # Change scale label
#' d + scale_colour_viridis_d("City\nCenter")
#'
#' # Select palette to use, see ?scales::pal_viridis for more details
#' d + scale_colour_viridis_d(option = "plasma")
#' d + scale_colour_viridis_d(option = "inferno")
#'
#' # scale_fill_viridis_d works just the same as
#' # scale_colour_viridis_d but for fill colours
#' p <- ggplot(txsamp, aes(x = median, fill = city)) +
#'   geom_histogram(position = "dodge", binwidth = 15000)
#' p + scale_fill_viridis_d()
#' # the order of colour can be reversed
#' p + scale_fill_viridis_d(direction = -1)
#'
#' # Use viridis_c with continuous data
#' (v <- ggplot(faithfuld) +
#'   geom_tile(aes(waiting, eruptions, fill = density)))
#' v + scale_fill_viridis_c()
#' v + scale_fill_viridis_c(option = "plasma")
#'
#' # Use viridis_b to bin continuous data before mapping
#' v + scale_fill_viridis_b()
#'
scale_colour_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", aesthetics = "colour") {
  discrete_scale(
    aesthetics,
    palette = pal_viridis(alpha, begin, end, direction, option),
    ...
  )
}

#' @export
#' @rdname scale_viridis
scale_fill_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                 direction = 1, option = "D", aesthetics = "fill") {
  discrete_scale(
    aesthetics,
    palette = pal_viridis(alpha, begin, end, direction, option),
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
    palette = pal_gradient_n(
      pal_viridis(alpha, begin, end, direction, option)(6),
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
    palette = pal_gradient_n(
      pal_viridis(alpha, begin, end, direction, option)(6),
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
scale_colour_viridis_b <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = "coloursteps", aesthetics = "colour") {
  pal <-  pal_binned(
    pal_viridis(alpha, begin, end, direction, option)
  )

  binned_scale(
    aesthetics,
    palette = pal,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_viridis
scale_fill_viridis_b <- function(..., alpha = 1, begin = 0, end = 1,
                                 direction = 1, option = "D", values = NULL,
                                 space = "Lab", na.value = "grey50",
                                 guide = "coloursteps", aesthetics = "fill") {
  pal <-  pal_binned(
    pal_viridis(alpha, begin, end, direction, option)
  )

  binned_scale(
    aesthetics,
    palette = pal,
    na.value = na.value,
    guide = guide,
    ...
  )
}
