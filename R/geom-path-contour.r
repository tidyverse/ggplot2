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
geom_contour <- function (mapping = NULL, data = NULL, stat = "contour", position = "identity",
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) {
  GeomContour$new(mapping = mapping, data = data, stat = stat, position = position,
  lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}

GeomContour <- proto(GeomPath, {
  objname <- "contour"

  default_aes <- function(.) aes(weight=1, colour="#3366FF", size = 0.5, linetype = 1, alpha = NA)

  default_stat <- function(.) StatContour
})

