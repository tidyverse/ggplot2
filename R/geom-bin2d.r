#' Add heatmap of 2d bin counts.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "bin2d")}
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_bin2d} and \code{stat_bin2d}.
#' @seealso \code{\link{stat_binhex}} for hexagonal binning
#' @examples
#' d <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10)
#' d + geom_bin2d()
#'
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + geom_bin2d(bins = 10)
#' d + geom_bin2d(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + geom_bin2d(binwidth = c(0.1, 0.1))
geom_bin2d <- function(mapping = NULL, data = NULL,
                       stat = "bin2d", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
