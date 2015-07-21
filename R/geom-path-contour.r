#' Display contours of a 3d surface in 2d.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "contour")}
#'
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @seealso \code{\link{geom_density2d}}: 2d density contours
#' @export
#' @examples
#' # See stat_contour for examples
geom_contour <- function (mapping = NULL, data = NULL, stat = "contour",
  position = "identity", lineend = "butt", linejoin = "round", linemitre = 1,
  na.rm = FALSE, show_guide = NA, inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomContour,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    geom_params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm
    ),
    params = list(...)
  )
}

GeomContour <- proto2(
  class = "GeomContour",
  inherit = GeomPath,
  members = list(
    objname = "contour",

    default_aes = function(self) {
      aes(weight = 1, colour = "#3366FF", size = 0.5, linetype = 1, alpha = NA)
    }
  )
)
