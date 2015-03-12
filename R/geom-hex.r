#' Hexagon bining.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "hex")}
#'
#' @export
#' @inheritParams geom_point
#' @examples
#' # See ?stat_binhex for examples
geom_hex <- function (mapping = NULL, data = NULL, stat = "binhex", position = "identity", ...) {
  GeomHex$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomHex <- proto(Geom, {
  objname <- "hex"

  draw <- function(., data, scales, coordinates, ...) {
    with(coord_transform(coordinates, data, scales),
      ggname(.$my_name(), hexGrob(x, y, col=colour,
        fill = alpha(fill, alpha)))
    )
  }

  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour=NA, fill = "grey50", size=0.5, alpha = NA)
  default_stat <- function(.) StatBinhex
  guide_geom <- function(.) "polygon"

})


# Draw hexagon grob
# Modified from code by Nicholas Lewin-Koh and Martin Maechler
#
# @param x positions of hex centres
# @param y positions
# @param vector of hex sizes
# @param border colour
# @param fill colour
# @keyword internal
hexGrob <- function(x, y, size = rep(1, length(x)), colour = "grey50", fill = "grey90") {
  stopifnot(length(y) == length(x))

  dx <- resolution(x, FALSE)
  dy <- resolution(y, FALSE) / sqrt(3) / 2 * 1.15

  hexC <- hexbin::hexcoords(dx, dy, n = 1)

  n <- length(x)

  polygonGrob(
    x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(x, each = 6),
    y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(y, each = 6),
    default.units = "native",
    id.lengths = rep(6, n), gp = gpar(col = colour, fill = fill)
  )
}
