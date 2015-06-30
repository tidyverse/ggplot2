#' Frequency polygon.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "freqpoly")}
#'
#' @seealso \code{\link{geom_histogram}}: histograms
#' @inheritParams geom_point
#' @export
#' @examples
#' m <- ggplot(diamonds, aes(carat))
#' m + geom_freqpoly(binwidth = 0.1)
#' m + geom_freqpoly(binwidth = 0.01)
#'
#' p <- ggplot(diamonds, aes(price))
#' p + geom_freqpoly(binwidth = 1000)
#' p + geom_freqpoly(aes(colour = color), binwidth = 1000)
#' p + geom_freqpoly(aes(y = ..density.., colour = color),
#'                   binwidth = 1000)
geom_freqpoly <- function(mapping = NULL, data = NULL, stat = "bin",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFreqpoly,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}


GeomFreqpoly <- proto2(
  inherit = Geom,
  members = list(
    objname = "freqpoly",

    default_aes = function(self) GeomPath$default_aes(),

    default_stat = function(self) StatBin,

    draw = function(self, ...) GeomPath$draw(...),

    guide_geom = function(self) "path"
  )
)
