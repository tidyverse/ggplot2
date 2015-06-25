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
  position = "identity", show_guide = NA, ...)
{
  LayerR6$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFreqpoly,
    position = position,
    params = list(...)
  )
}


GeomFreqpoly <- R6::R6Class("GeomFreqpoly", inherit = GeomR6,
  public = list(
    objname = "freqpoly",

    # R6 TODO: Avoid instantiation
    default_aes = function() GeomPath$new()$default_aes(),

    default_stat = function() StatBin,

    # R6 TODO: Avoid instantiation
    draw = function(...) GeomPath$new()$draw(...),

    guide_geom = function() "path"
  )
)
