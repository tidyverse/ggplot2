#' Blank, draws nothing.
#' 
#' The blank geom draws nothing, but can be a useful way of ensuring common
#' scales between different plots.
#'
#' @export
#' @inheritParams geom_point
#' @examples
#' qplot(length, rating, data=movies, geom="blank")
#' # Nothing to see here!
geom_blank <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) { 
  GeomBlank$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomBlank <- proto(Geom, {
  objname <- "blank"

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes()

  draw_legend <- function(., data, ...) {
    zeroGrob()
  }
  
})
