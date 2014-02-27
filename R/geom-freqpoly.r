#' Frequency polygon.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "freqpoly")}
#'
#' @seealso \code{\link{geom_histogram}}: histograms
#' @inheritParams geom_point
#' @export
#' @examples
#' qplot(carat, data = diamonds, geom = "freqpoly")
#' qplot(carat, data = diamonds, geom = "freqpoly", binwidth = 0.1)
#' qplot(carat, data = diamonds, geom = "freqpoly", binwidth = 0.01)
#'
#' qplot(price, data = diamonds, geom = "freqpoly", binwidth = 1000)
#' qplot(price, data = diamonds, geom = "freqpoly", binwidth = 1000,
#'   colour = color)
#' qplot(price, ..density.., data = diamonds, geom = "freqpoly",
#'   binwidth = 1000, colour = color)
geom_freqpoly <- function (mapping = NULL, data = NULL, stat = "bin", position = "identity", ...) {
  GeomFreqpoly$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomFreqpoly <- proto(Geom, {
  objname <- "freqpoly"

  default_aes <- function(.) GeomPath$default_aes()
  default_stat <- function(.) StatBin
  draw <- function(., ...) GeomPath$draw(...)
  guide_geom <- function(.) "path"
})
