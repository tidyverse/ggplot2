#' Viridis colour scales from viridisLite
#'
#' @description
#' The `viridis` scales provide color maps that are perceptually uniform in both
#' color and black-and-white. They are also designed to be perceived by viewers
#' with common forms of color blindness. See also
#' \url{https://bids.github.io/colormap/}.
#' 
#' @inheritParams viridisLite::viridis
#' @inheritParams scales::gradient_n_pal
#' @param ... Other arguments passed on to [discrete_scale()] or
#' [continuous_scale()] to control name, limits, breaks, labels and so forth.
#' @family colour scales
#' @rdname scale_viridis
#' @export
scale_colour_viridis_discrete <- function(..., alpha = 1, begin = 0, end = 1,
                                          direction = 1, option = "D") {
  discrete_scale("colour", "viridis_discrete",
                 viridis_pal(alpha, begin, end, direction, option),
                 ...)
}

#' @export
#' @rdname scale_viridis
scale_fill_viridis_discrete <- function(..., alpha = 1, begin = 0, end = 1,
                                        direction = 1, option = "D") {
  discrete_scale("fill", "viridis_discrete",
                 viridis_pal(alpha, begin, end, direction, option),
                 ...)
}

#' @export
#' @rdname scale_viridis
scale_colour_viridis_continuous <- function(..., alpha = 1, begin = 0, end = 1,
                                            direction = 1, option = "D",
                                            values = NULL, space = "Lab",
                                            na.value = "grey50",
                                            guide = "colourbar") {
  continuous_scale("colour", "viridis_continuous",
                   gradient_n_pal(viridis_pal(alpha, begin, end, direction,
                                              option)(6), values, space),
                   na.value = na.value,
                   guide = guide, ...)
}

#' @export
#' @rdname scale_viridis
scale_fill_viridis_continuous <- function(..., alpha = 1, begin = 0, end = 1,
                                          direction = 1, option = "D",
                                          values = NULL, space = "Lab",
                                          na.value = "grey50",
                                          guide = "colourbar") {
  continuous_scale("fill", "viridis_continuous",
                   gradient_n_pal(viridis_pal(alpha, begin, end, direction,
                                              option)(6), values, space),
                   na.value = na.value,
                   guide = guide, ...)
}

#' @inheritParams viridisLite::viridis
#' @export
#' @rdname scale_viridis
viridis_pal <- function(alpha = 1, begin = 0, end = 1, direction = 1, option= "D") {
  function(n) {
    viridisLite::viridis(n, alpha, begin, end, direction, option)
  }
}
