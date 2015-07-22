#' @include stat-.r
NULL

#' @export
#' @rdname geom_abline
geom_vline <- function(mapping = NULL, data = NULL, show_guide = FALSE,
                       xintercept, ...) {

  # Act like an annotation
  if (!missing(xintercept)) {
    data <- data.frame(xintercept = xintercept)
    mapping <- aes(xintercept = xintercept)
    show_guide <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomVline,
    position = PositionIdentity,
    show_guide = show_guide,
    inherit.aes = FALSE,
    params = list(...)
  )
}

GeomVline <- proto2("GeomVline", Geom,
  draw = function(self, data, scales, coordinates, ...) {
    ranges <- coord_range(coordinates, scales)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomSegment$draw(unique(data), scales, coordinates)
  },

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  required_aes = "xintercept",

  guide_geom = legend_vline
)
