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
                          position = "identity", show_guide = NA, ...) {
  GeomFreqpoly$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide,...)
}

GeomFreqpoly <- proto(Geom, {
  objname <- "freqpoly"

  default_aes <- function(.) GeomPath$default_aes()
  default_stat <- function(.) StatBin
  draw <- function(., ...) GeomPath$draw(...)
  guide_geom <- function(.) "path"
})
