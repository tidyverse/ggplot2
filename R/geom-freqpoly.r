#' Frequency polygon.
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

  icon <- function(.) {
    y <- c(0.2, 0.3, 0.5, 0.6,0.2, 0.8, 0.5, 0.3)
    linesGrob(seq(0.1, 0.9, by=0.1), y, gp=gpar(col="grey20"))
  }
  
  default_aes <- function(.) GeomPath$default_aes()
  default_stat <- function(.) StatBin
  draw <- function(., ...) GeomPath$draw(...)
  guide_geom <- function(.) "path"
})
