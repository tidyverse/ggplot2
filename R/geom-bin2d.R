#' @include geom-tile.R
NULL

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBin2d <- ggproto("GeomBin2d", GeomTile)

#' Heatmap of 2d bin counts
#'
#' Divides the plane into rectangles, counts the number of cases in
#' each rectangle, and then (by default) maps the number of cases to the
#' rectangle's fill. This is a useful alternative to [geom_point()]
#' in the presence of overplotting.
#'
#' @eval rd_aesthetics("stat", "bin_2d")
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   `geom_bin_2d()` and `stat_bin_2d()`. For more information about overriding
#'   these connections, see how the [stat][layer_stats] and [geom][layer_geoms]
#'   arguments work.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @seealso [stat_bin_hex()] for hexagonal binning
#' @examples
#' d <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10)
#' d + geom_bin_2d()
#'
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + geom_bin_2d(bins = 10)
#' d + geom_bin_2d(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + geom_bin_2d(binwidth = c(0.1, 0.1))
geom_bin_2d <- make_constructor(GeomBin2d, stat = "bin2d")

#' @export
#' @rdname geom_bin_2d
#' @usage NULL
geom_bin2d <- geom_bin_2d
