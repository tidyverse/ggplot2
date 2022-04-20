#' Line segments parameterised by location, direction and distance
#'
#' This is a polar parameterisation of [geom_segment()]. It is
#' useful when you have variables that describe direction and distance.
#' The angles start from east and increase counterclockwise.
#'
#' @eval rd_aesthetics("geom", "spoke")
#' @inheritParams layer
#' @inheritParams geom_segment
#' @export
#' @examples
#' df <- expand.grid(x = 1:10, y=1:10)
#' df$angle <- runif(100, 0, 2*pi)
#' df$speed <- runif(100, 0, sqrt(0.1 * df$x))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_spoke(aes(angle = angle), radius = 0.5)
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_spoke(aes(angle = angle, radius = speed))
geom_spoke <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomSpoke,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_spoke
#' @usage NULL
stat_spoke <- function(...) {
  lifecycle::deprecate_warn("2.0.0", "stat_spoke()", "geom_spoke()")
  geom_spoke(...)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSpoke <- ggproto("GeomSpoke", GeomSegment,
  setup_data = function(data, params) {
    data$radius <- data$radius %||% params$radius
    data$angle <- data$angle %||% params$angle

    transform(data,
      xend = x + cos(angle) * radius,
      yend = y + sin(angle) * radius
    )
  },
  required_aes = c("x", "y", "angle", "radius")
)
