#' @inheritParams grid::curveGrob
#' @export
#' @rdname geom_segment
geom_curve <- function(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", curvature = 0.5, angle = 90, ncp = 5, arrow = NULL,
  lineend = "butt", na.rm = FALSE, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCurve,
    position = position,
    inherit.aes = inherit.aes,
    geom_params = list(
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      lineend = lineend,
      na.rm = na.rm
    ),
    params = list(...)
  )
}

GeomCurve <- proto2("GeomCurve", Geom,
  draw = function(self, data, scales, coordinates, curvature = 0.5, angle = 90,
                  ncp = 5, arrow = NULL, lineend = "butt", na.rm = FALSE, ...) {

    data <- remove_missing(data, na.rm = na.rm,
                           c("x", "y", "xend", "yend", "linetype", "size", "shape"),
                           name = "geom_curve")

    if (empty(data)) return(zeroGrob())

    if (!is.linear(coordinates)) {
      warning("geom_curve is not implemented for non-linear coordinates",
        call. = FALSE)
    }
    trans <- coord_transform(coordinates, data, scales)
    curveGrob(
      trans$x, trans$y, trans$xend, trans$yend,
      default.units = "native",
      curvature = curvature, angle = angle, ncp = ncp,
      square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
      gp = gpar(
        col = alpha(trans$colour, trans$alpha),
        lwd = trans$size * .pt,
        lty = trans$linetype,
        lineend = trans$lineend),
      arrow = arrow
    )
  },

  required_aes = c("x", "y", "xend", "yend"),

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  guide_geom = function(self) "path"
)
