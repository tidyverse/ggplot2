#' @include stat-.r
NULL

#' @export
#' @rdname geom_abline
geom_hline <- function(mapping = NULL, data = NULL, show_guide = NA,
                       yintercept, ...) {

  # Act like an annotation
  if (!missing(yintercept)) {
    data <- data.frame(yintercept = yintercept)
    mapping <- aes(yintercept = yintercept)
    show_guide <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomHline,
    position = PositionIdentity,
    show_guide = show_guide,
    inherit.aes = FALSE,
    params = list(...)
  )
}

GeomHline <- proto2("GeomHline", Geom,
  draw = function(self, data, scales, coordinates, ...) {
    ranges <- coord_range(coordinates, scales)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- data$yintercept
    data$yend <- data$yintercept

    GeomSegment$draw(unique(data), scales, coordinates)
  },

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  required_aes = "yintercept",

  guide_geom = legend_path
)
