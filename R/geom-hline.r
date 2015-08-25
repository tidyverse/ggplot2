#' @include stat-.r
NULL

#' @export
#' @rdname geom_abline
geom_hline <- function(mapping = NULL, data = NULL, show.legend = NA,
                       yintercept, ...) {

  # Act like an annotation
  if (!missing(yintercept)) {
    data <- data.frame(yintercept = yintercept)
    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomHline,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHline <- ggproto("GeomHline", Geom,
  draw_panel = function(data, scales, coordinates, ...) {
    ranges <- coordinates$range(scales)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- data$yintercept
    data$yend <- data$yintercept

    GeomSegment$draw_panel(unique(data), scales, coordinates)
  },

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  required_aes = "yintercept",

  draw_key = draw_key_path
)
