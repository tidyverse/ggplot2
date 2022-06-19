#' Hexagonal heatmap of 2d bin counts
#'
#' Divides the plane into regular hexagons, counts the number of cases in
#' each hexagon, and then (by default) maps the number of cases to the hexagon
#' fill.  Hexagon bins avoid the visual artefacts sometimes generated by
#' the very regular alignment of [geom_bin2d()].
#'
#' @eval rd_aesthetics("geom", "hex")
#' @seealso [stat_bin2d()] for rectangular binning
#' @param geom,stat Override the default connection between `geom_hex()` and
#'   `stat_binhex()`.
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, price))
#' d + geom_hex()
#'
#' \donttest{
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + geom_hex(bins = 10)
#' d + geom_hex(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + geom_hex(binwidth = c(1, 1000))
#' d + geom_hex(binwidth = c(.1, 500))
#' }
geom_hex <- function(mapping = NULL, data = NULL,
                     stat = "binhex", position = "identity",
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHex,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHex <- ggproto("GeomHex", Geom,
  draw_group = function(data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", linemitre = 10) {
    if (empty(data)) {
      return(zeroGrob())
    }

    # Get hex sizes
    if (!is.null(data$width)) {
      dx <- data$width[1] / 2
    } else {
      dx <- resolution(data$x, FALSE)
    }
    # Adjust for difference in width and height of regular hexagon. 1.15 adjusts
    # for the effect of the overlapping range in y-direction on the resolution
    # calculation
    if (!is.null(data$height)) {
      dy <- data$height[1] /  sqrt(3) / 2
    } else {
      dy <- resolution(data$y, FALSE) / sqrt(3) / 2 * 1.15
    }

    hexC <- hexbin::hexcoords(dx, dy, n = 1)

    n <- nrow(data)

    data <- data[rep(seq_len(n), each = 6), ]
    data$x <- rep.int(hexC$x, n) + data$x
    data$y <- rep.int(hexC$y, n) + data$y

    coords <- coord$transform(data, panel_params)

    ggname("geom_hex", polygonGrob(
      coords$x, coords$y,
      gp = gpar(
        col = coords$colour,
        fill = alpha(coords$fill, coords$alpha),
        lwd = coords$linewidth * .pt,
        lty = coords$linetype,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      ),
      default.units = "native",
      id.lengths = rep.int(6, n)
    ))
  },

  required_aes = c("x", "y"),

  default_aes = aes(
    colour = NA,
    fill = "grey50",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),

  draw_key = draw_key_polygon,

  rename_size = TRUE
)


# Draw hexagon grob
# Modified from code by Nicholas Lewin-Koh and Martin Maechler
#
# @param x positions of hex centres
# @param y positions
# @param size vector of hex sizes
# @param gp graphical parameters
# @keyword internal
#
# THIS IS NO LONGER USED BUT LEFT IF CODE SOMEWHERE ELSE RELIES ON IT
hexGrob <- function(x, y, size = rep(1, length(x)), gp = gpar()) {
  if (length(y) != length(x)) {
    cli::cli_abort("{.arg x} and {.arg y} must have the same length")
  }

  dx <- resolution(x, FALSE)
  dy <- resolution(y, FALSE) / sqrt(3) / 2 * 1.15

  hexC <- hexbin::hexcoords(dx, dy, n = 1)

  n <- length(x)

  polygonGrob(
    x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(x, each = 6),
    y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(y, each = 6),
    default.units = "native",
    id.lengths = rep(6, n), gp = gp
  )
}
